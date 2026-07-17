(ns com.blockether.vis.ext.channel-web.live-key-test
  "Web-channel live-key guard: the web reads RAW canonical wire, so every
   turn payload it touches must be snake_case STRING-keyed end-to-end, and
   the SHARED formatters must consume those wire maps DIRECTLY — no
   `wire->kw`-style re-keying layer anywhere between wire and pixels."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-web.core :as web]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private engine-turn
  "An engine-shaped completed-turn patch as `gateway.state` builds it:
   keyword top-level fields, canonical STRING-keyed metric maps."
  {:status "completed"
   :content [{"id" "b1" "type" "prose" "markdown" "done"}]
   :is_needs_input false
   :iteration_count 3
   :duration_ms 4900
   :tokens {"input" 11461 "output" 35 "cached" 4096}
   :cost {"total_cost" 0.006954 "model" "gpt-4o" "provider" "openai"}
   :utilization {"last_request_tokens" 11461 "saturation" 6 "headroom_tokens" 90000}})

(defn- all-map-keys
  [x]
  (let [acc (atom [])]
    (walk/postwalk (fn [v]
                     (when (map? v) (swap! acc into (keys v)))
                     v)
                   x)
    @acc))

(defdescribe web-wire-keys-test
             (it "a turn event serialized for the web carries ONLY snake_case string keys"
                 (let [ks (all-map-keys (wire/parse-json (wire/json-str engine-turn)))]
                   (expect (seq ks))
                   (expect (every? string? ks))
                   (expect (not-any? #(str/includes? % "-") ks))
                   (expect (not-any? #(str/ends-with? % "?") ks))))
             (it "the metric maps survive the wire byte-identical (already canonical at source)"
                 (let [t (wire/parse-json (wire/json-str engine-turn))]
                   (expect (= (:tokens engine-turn) (get t "tokens")))
                   (expect (= (:cost engine-turn) (get t "cost")))
                   (expect (= (:utilization engine-turn) (get t "utilization"))))))

(defdescribe web-formatters-read-wire-directly-test
             (it "meta-summary-line renders straight off the wire turn — no re-keying layer"
                 (let [t
                       (wire/parse-json (wire/json-str engine-turn))

                       line
                       (vis/meta-summary-line {:tokens (get t "tokens")
                                               :cost (get t "cost")
                                               :duration-ms (get t "duration_ms")})]

                   (expect (str/includes? line "11.5k→35"))
                   (expect (str/includes? line "(cached 4.1k)"))
                   (expect (str/includes? line "~$0.0070"))
                   (expect (str/includes? line "openai/gpt-4o"))
                   (expect (str/includes? line "4.9s")))))

(defdescribe lazy-trace-handler-test
             (it "passes canonical wire keys to the trace renderer"
                 (with-redefs [vis/gateway-turn-trace (constantly [{"thinking" "history visible"
                                                                    "forms" []}])]
                   (let [handler @#'web/turn-trace-handler
                         response (handler {:path-params {:sid "session-1" :tid "turn-1"}})]

                     (expect (= 200 (:status response)))
                     (expect (str/includes? (:body response) "history visible"))))))

(defdescribe
  live-stream-placement-test
  (it "streams an answer delta into the single Vis shell's answer preview"
      (let [shell
            (pr-str (@#'web/live-vis-shell {"turn_id" "turn-1"}))

            [frame]
            (@#'web/event->frames
             "session-1"
             {"type" "content.block.delta"
              "turn_id" "turn-1"
              "iteration" 1
              "block_id" "turn-1:content:1"
              "field" "markdown"
              "text" "answer in progress"})]

        (expect (str/includes? shell ":div.role.role-vis"))
        (expect (str/includes? shell ":sse-swap \"trace-message\""))
        (expect (str/includes? shell ":sse-swap \"content\""))
        (expect (= "content" (:event frame)))
        (expect (str/includes? (:html frame) "answer in progress"))))
  (it "streams a reasoning delta into the transient thinking ticker"
      (let [[frame] (@#'web/event->frames
                     "session-1"
                     {"type" "content.block.delta"
                      "turn_id" "turn-1"
                      "iteration" 1
                      "block_id" "turn-1:reasoning:1"
                      "field" "text"
                      "text" "thinking hard"})]
        (expect (= "thinking" (:event frame)))
        (expect (str/includes? (:html frame) "thinking hard"))))
  (it
    "clears the streaming answer preview when it pins the iteration prose (no double answer)"
    (let [frames
          (@#'web/event->frames
           "session-1"
           {"type" "iteration.completed" "turn_id" "turn-1" "iteration" 1 "assistant_prose" "Hej!"})

          prose
          (filter #(= "trace-message" (:event %)) frames)

          clears
          (filter #(and (= "content" (:event %)) (str/blank? (str (:html %)))) frames)]

      ;; the prose pins into the durable trace ...
      (expect (some #(str/includes? (:html %) "Hej!") prose))
      ;; ... AND the transient `.live-vis-answer` streaming copy is
      ;; wiped, so the answer never renders twice in the live bubble.
      (expect (seq clears))))
  (it "settles streamed work and the final answer into the same Vis bubble"
      (with-redefs [vis/gateway-turn-trace (constantly [{"thinking" "durable work" "forms" []}])]
        (let [bubble (pr-str (@#'web/settled-vis-bubble
                              {"session_id" "session-1"
                               "turn_id" "turn-1"
                               "status" "completed"
                               "content" [{"id" "b1" "type" "prose" "markdown" "final answer"}]}))]
          (expect (str/includes? bubble "durable work"))
          (expect (str/includes? bubble "final answer"))
          (expect (< (.indexOf bubble "durable work") (.indexOf bubble "final answer")))))))
