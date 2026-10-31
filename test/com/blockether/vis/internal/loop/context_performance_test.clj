(ns com.blockether.vis.internal.loop.context-performance-test
  "Context preparation regressions and an opt-in local JFR benchmark. Use
   `profile-context!` from a test REPL. Recordings contain only synthetic text."
  (:require [clojure.java.io :as io]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.channel.form :as form]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.management ThreadMXBean)
           (java.lang.management ManagementFactory)
           (java.lang.ref WeakReference)
           (jdk.jfr Configuration Recording)))

(def ^:private model "gpt-6.1-sol")

(def ^:private target {:provider "openai-codex" :model model})

(defn- history
  [n]
  (let [body (str "HEAD\n" (apply str (repeat 6000 "source path: \"quoted\"\n")) "TAIL")]
    (mapv (fn [i]
            (let [id (str "call_" i)
                  call {:id id :name "python_execution" :input {:code "print(result)"}}]

              [i
               {:scope (str "t1/i" i)
                :llm-model model
                :tool-calls [call]
                :assistant-message {:role "assistant" :content [(assoc call :type "tool_use")]}
                :forms-vec [{:scope (str "t1/i" i)
                             :svar/tool-call-id id
                             :llm-model model
                             :stdout body
                             :code "print(result)"}]}]))
          (range 1 (inc n)))))

(defn- prepare
  [entries]
  (let [context (atom {})]
    (transcript/stamp-iter-universe! context entries)
    (transcript/stamp-iter-universe! context entries entries {:model model :replay-target target})
    ((prompt/request-token-counter) model (transcript/conversation-suffix entries target))))

(defn- sample
  [f]
  (let [^ThreadMXBean bean
        (ManagementFactory/getThreadMXBean)

        id
        (.threadId (Thread/currentThread))

        allocated
        (.getThreadAllocatedBytes bean id)

        cpu
        (.getCurrentThreadCpuTime bean)

        started
        (System/nanoTime)]

    (f)
    {:wall-ms (/ (- (System/nanoTime) started) 1e6)
     :cpu-ms (/ (- (.getCurrentThreadCpuTime bean) cpu) 1e6)
     :allocated-bytes (- (.getThreadAllocatedBytes bean id) allocated)}))

(defn- measure
  [f]
  (dotimes [_ 3]
    (f))
  (let [samples (repeatedly 7 #(sample f))]
    (into {}
          (for [key [:wall-ms :cpu-ms :allocated-bytes]]
            [key (nth (vec (sort (map key samples))) 3)]))))

(defn profile-context!
  "Record warmed context preparation and append-only history growth into a local JFR file."
  [destination]
  (let [file (io/file destination)]
    (io/make-parents file)
    (with-open [recording (Recording. (Configuration/getConfiguration "profile"))]
      (.setName recording "vis-context-benchmark")
      (.setMaxSize recording (* 128 1024 1024))
      (doseq [event ["jdk.InitialEnvironmentVariable" "jdk.InitialSystemProperty"
                     "jdk.SystemProcess"]]
        (.disable recording ^String event))
      (.start recording)
      (try (let [rows (mapv (fn [n]
                              (let [entries (history n)]
                                (assoc (measure #(prepare entries)) :forms n)))
                            [1 10 30])
                 entries (history 30)
                 growth (sample #(doseq [n (range 1 31)] (prepare (subvec entries 0 n))))]

             {:rows rows :growth growth :recording (.getAbsolutePath file)})
           (finally (.stop recording) (.dump recording (.toPath file)))))))

(defdescribe
  context-preparation-regression-test
  (it "renders each native tool output once instead of building an unused fallback"
      ;; Regression: local JFR investigation found repeated historical stdout clipping.
      (let [calls
            (atom 0)

            clip
            form/clip-to-wire

            record
            (second (first (history 1)))]

        (with-redefs [form/clip-to-wire (fn [& args]
                                          (swap! calls inc)
                                          (apply clip args))]
          (#'transcript/iteration-results-message record))
        (expect (= 1 @calls))))
  (it "reuses an unchanged stdout projection without another token search"
      (let [record
            (first (:forms-vec (second (first (history 1)))))

            calls
            (atom 0)

            count-tokens
            svar/count-tokens]

        (with-redefs [svar/count-tokens (fn [& args]
                                          (swap! calls inc)
                                          (apply count-tokens args))]
          (let [first-output (form/clip-to-wire (:stdout record) record)
                first-count @calls
                second-output (form/clip-to-wire (:stdout record) record)]

            (expect (= first-output second-output))
            (expect (= first-count @calls))))))
  (it "prices unchanged visible history without tokenizing it again"
      ;; Regression: a 30-turn native benchmark spent about 150 ms on each step
      ;; while `stamp-iter-universe!` tokenized the whole visible history again.
      (let [entries
            (assoc-in (history 4) [3 1 :forms-vec 0 :stdout] (str "fresh " (random-uuid)))

            changed
            (assoc-in entries [3 1 :forms-vec 0 :stdout] "small")

            pricing
            {:model model :replay-target target}

            context
            (atom {})

            calls
            (atom 0)

            count-messages
            svar/count-messages]

        (with-redefs [svar/count-messages (fn [& args]
                                            (swap! calls inc)
                                            (apply count-messages args))]
          (transcript/stamp-iter-universe! context (pop entries) (pop entries) pricing)
          (reset! calls 0)
          (transcript/stamp-iter-universe! context entries entries pricing)
          ;; One new group: its own count and the empty-array baseline.
          (expect (= 2 @calls))
          (let [before (get-in @context ["engine_iter_weights" "t1/i4"])]
            (transcript/stamp-iter-universe! context changed changed pricing)
            (expect (< (get-in @context ["engine_iter_weights" "t1/i4"]) before))))))
  (it "counts a long request again without tokenizing its unchanged messages"
      ;; Regression: the request counter kept 512 messages, so a longer history evicted
      ;; each message before its reuse and the whole request was tokenized again.
      (let [messages
            (vec (for [i (range 600)]
                   {:role (if (even? i) "assistant" "user")
                    :content (str "message " i " " (random-uuid))}))

            counter
            (prompt/request-token-counter)

            calls
            (atom 0)

            count-messages
            svar/count-messages]

        (with-redefs [svar/count-messages (fn [& args]
                                            (swap! calls inc)
                                            (apply count-messages args))]
          (let [first-count (counter model messages)]
            (reset! calls 0)
            (expect (= first-count (counter model messages)))
            (expect (zero? @calls)))))))

(defdescribe
  context-cache-invalidation-test
  (it "matches uncached clipping after changes to content, model, scope, call or ceiling"
      (let [record
            (first (:forms-vec (second (first (history 1)))))

            body
            (:stdout record)]

        (doseq [[text f]
                [[body record] [(str "CHANGED" body) record]
                 [body (assoc record :llm-model "gpt-4")] [body (assoc record :scope "t9/i8")]
                 [body (assoc record :svar/tool-call-id "call_changed")]]

                ceiling
                [4096 1024]]

          (with-redefs [form/MAX_FORM_OUTPUT_TOKENS ceiling]
            (let [expected (#'form/clip-stdout
                            text
                            (:llm-model f)
                            (:scope f)
                            (:svar/tool-call-id f)
                            ceiling
                            32768)
                  actual (form/clip-to-wire text f)]

              (expect (= expected actual))
              (expect (<= (svar/count-tokens (:llm-model f) actual) ceiling)))))
        (expect (= body (:stdout record)))))
  (it "separates tokenizer options, changed content and image geometry across counters"
      (let [messages [{:role "user" :content "Zażółć 中 with code = 42"}
                      {:role "user"
                       :content [{:type "image_url"
                                  :image_url {:url "data:image/png;base64,AAAA" :detail "low"}}]}]]
        (doseq [opts [{} {:tokenizer "cl100k_base"} {:tokenizer "o200k_base"}]
                model ["gpt-4" "gpt-4o"]
                selected [messages (assoc-in messages [0 :content] "Changed request")
                          (assoc-in messages [1 :content 0 :image_url :detail] "high")]]

          (expect (= (svar/count-messages model selected opts)
                     ((prompt/request-token-counter opts) model selected))))))
  (it "prices changed and folded history without stale raw weights"
      (let [entries
            (history 1)

            changed
            (assoc-in entries [0 1 :forms-vec 0 :stdout] "small changed output")

            folded
            (assoc-in entries [0 1 :collapsed?] true)

            context
            (atom {})]

        (transcript/stamp-iter-universe! context entries)
        (let [before (get-in @context ["engine_iter_weights" "t1/i1"])]
          (transcript/stamp-iter-universe! context changed)
          (expect (< (get-in @context ["engine_iter_weights" "t1/i1"]) before))
          (transcript/stamp-iter-universe! context folded)
          (expect (zero? (get-in @context ["engine_iter_weights" "t1/i1"])))))))

(defdescribe
  weak-stdout-cache-test
  (it "uses exact string equality and keeps hashes stable after a reference is cleared"
      (let [args
            ["Aa" model "t23/i101" "call_weak" 4096 32768]

            a
            (#'form/stdout-key args)

            equal-a
            (#'form/stdout-key (assoc args 0 (String. ^String (first args))))

            b
            (#'form/stdout-key (assoc args 0 "BB"))

            ^com.blockether.vis.internal.channel.form.WeakStdout leaf
            (first a)

            before
            (hash leaf)]

        (expect (= (hash a) (hash b)))
        (expect (= a equal-a))
        (expect (not= a b))
        (.clear ^WeakReference (.-source leaf))
        (expect (= before (hash leaf)))
        (expect (= leaf leaf))
        (expect (not= a equal-a))))
  (it
    "treats cleared sources as misses and can evict their entries"
    (let [keys
          (atom [])

          calls
          (atom 0)

          cached
          (util/bounded-memoize (fn [s & _]
                                  (swap! calls inc)
                                  (count s))
                                2
                                4096
                                (fn [_ _]
                                  1)
                                (fn [args]
                                  (let [key (#'form/stdout-key args)]
                                    (swap! keys conj key)
                                    key)))

          args
          ["source" model nil nil 4096 32768]]

      (expect (= 6 (apply cached args)))
      (expect (= 6 (apply cached args)))
      (expect (= 1 @calls))
      (let [^com.blockether.vis.internal.channel.form.WeakStdout leaf (ffirst @keys)]
        (.clear ^WeakReference (.-source leaf)))
      (expect (= 6 (apply cached args)))
      (expect (= 2 @calls))
      (expect (= 5 (apply cached (assoc args 0 "other"))))
      (expect (= 5 (apply cached (assoc args 0 "third"))))
      (expect (= 6 (apply cached args)))
      (expect (= 5 @calls))))
  (it "separates colliding large inputs and preserves Unicode head and tail output"
      (let [prefix
            (str "HEAD 😀 中 " (apply str (repeat 17000 "x")))

            a
            (str prefix "Aa😀TAIL\n")

            b
            (str prefix "BB😀TAIL\n")

            record
            {:llm-model model :scope "t23/i102" :svar/tool-call-id "call_collision"}

            calls
            (atom 0)]

        (expect (= (.hashCode ^String a) (.hashCode ^String b)))
        (with-redefs [svar/count-tokens (fn [_ text]
                                          (swap! calls inc)
                                          (count text))]
          (let [expected-a (#'form/clip-stdout a model "t23/i102" "call_collision" 4096 32768)
                expected-b (#'form/clip-stdout b model "t23/i102" "call_collision" 4096 32768)
                actual-a (form/clip-to-wire a record)
                actual-b (form/clip-to-wire b record)
                after-misses @calls]

            (expect (= expected-a actual-a))
            (expect (= expected-b actual-b))
            (expect (not= actual-a actual-b))
            (expect (= actual-a (form/clip-to-wire a record)))
            (expect (= after-misses @calls))
            (expect (.startsWith ^String actual-a "HEAD 😀 中 "))
            (expect (.endsWith ^String actual-a "Aa😀TAIL")))))))
