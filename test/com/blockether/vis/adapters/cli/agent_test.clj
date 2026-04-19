(ns com.blockether.vis.adapters.cli.agent-test
  (:require
   [babashka.fs :as fs]
   [charred.api :as json]
   [lazytest.core :refer [defdescribe describe expect it]]
   [com.blockether.vis.config :as config]
   [com.blockether.vis.loop.conversations.core :as conversations]
   [com.blockether.vis.adapters.cli.agent :as sut]))

(defn- parse-json
  [s]
  (json/read-json s))

(defn- expect-keys-subset
  [m ks]
  (doseq [k ks]
    (expect (contains? m k))))

(defn- with-temp-db*
  [f]
  (let [original-path config/db-path
        temp-dir (str (fs/create-temp-dir {:prefix "vis-agent-test-"}))]
    (try
      (alter-var-root #'config/db-path (constantly temp-dir))
      (conversations/close-all!)
      (f)
      (finally
        (conversations/close-all!)
        (alter-var-root #'config/db-path (constantly original-path))
        (fs/delete-tree temp-dir)))))

(defmacro ^:private with-temp-db [& body]
  `(with-temp-db* (fn [] ~@body)))

(defdescribe result-json-integration-test
  (describe "CLI agent JSON integration"
    (it "run! -> result->json contains full envelope keys"
      (with-temp-db
        (let [agent (sut/agent {:name "itest-json"})
              result (sut/run! agent "hello" {:max-iterations 0})
              json-str (sut/result->json result)
              parsed (parse-json json-str)]
          (expect (string? json-str))
          (expect (map? parsed))
          ;; Full JSON envelope verification
          (expect-keys-subset parsed ["conv-id" "iterations" "duration-ms" "tokens" "cost" "trace" "status" "answer"])
          (expect (string? (get parsed "conv-id")))
          ;; With :max-iterations 0 the loop hits the cap on the first
          ;; check. The loop builds a \"⚠️ Iteration limit reached\"
          ;; markdown fallback (surfaced to the UI so the bubble isn't
          ;; blank). Test that it came through the envelope.
          (expect (string? (get parsed "answer")))
          (expect (clojure.string/includes? (get parsed "answer") "Iteration limit reached"))
          (expect (false? (contains? parsed "confidence")))
          (expect (false? (contains? parsed "learn")))
          (expect (false? (contains? parsed "error")))
          (expect (false? (contains? parsed "exception")))
          (expect (number? (get parsed "iterations")))
          (expect (>= (get parsed "iterations") 0))
          (expect (number? (get parsed "duration-ms")))
          (expect (>= (get parsed "duration-ms") 0))
          (expect (map? (get parsed "tokens")))
          (expect (map? (get parsed "cost")))
          (expect (vector? (get parsed "trace")))
          (expect (contains? parsed "status"))
          (expect (string? (get parsed "status")))
          ;; nested checks
          (expect-keys-subset (get parsed "tokens") ["input" "output" "reasoning" "cached" "total"])
          (expect (number? (get-in parsed ["tokens" "input"])))
          (expect (>= (get-in parsed ["tokens" "input"]) 0))
          (expect (number? (get-in parsed ["tokens" "output"])))
          (expect (>= (get-in parsed ["tokens" "output"]) 0))
          (expect (number? (get-in parsed ["tokens" "reasoning"])))
          (expect (>= (get-in parsed ["tokens" "reasoning"]) 0))
          (expect (number? (get-in parsed ["tokens" "cached"])))
          (expect (>= (get-in parsed ["tokens" "cached"]) 0))
          (expect (number? (get-in parsed ["tokens" "total"])))
          (expect (>= (get-in parsed ["tokens" "total"]) 0))
          (expect-keys-subset (get parsed "cost") ["input-cost" "output-cost" "total-cost" "model" "pricing"])
          (expect (number? (get-in parsed ["cost" "input-cost"])))
          (expect (>= (get-in parsed ["cost" "input-cost"]) 0))
          (expect (number? (get-in parsed ["cost" "output-cost"])))
          (expect (>= (get-in parsed ["cost" "output-cost"]) 0))
          (expect (number? (get-in parsed ["cost" "total-cost"])))
          (expect (>= (get-in parsed ["cost" "total-cost"]) 0))
          (expect (string? (get-in parsed ["cost" "model"])))
          (expect (map? (get-in parsed ["cost" "pricing"])))
          (expect-keys-subset (get-in parsed ["cost" "pricing"]) ["input" "output"])
          (expect (number? (get-in parsed ["cost" "pricing" "input"])))
          (expect (number? (get-in parsed ["cost" "pricing" "output"])))))))

  (it "result->json stringifies keyword/symbol keys and drops nil values"
    (let [result {:status :ok
                  :tokens {:input 1 :output 2}
                  :meta {:k :v 'sym 42}
                  :nil-field nil}
          parsed (parse-json (sut/result->json result))]
      (expect (= "ok" (get parsed "status")))
      (expect (= 1 (get-in parsed ["tokens" "input"])))
      (expect (= 2 (get-in parsed ["tokens" "output"])))
      (expect (= "v" (get-in parsed ["meta" "k"])))
      (expect (= 42 (get-in parsed ["meta" "sym"])))
      (expect (nil? (get parsed "nil-field"))))))
