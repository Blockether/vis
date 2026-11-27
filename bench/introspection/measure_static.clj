(ns measure-static
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.introspection :as inspect]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.session.agents :as agents]))

(defn- measure [text]
  {:chars (count text) :utf8_bytes (count (.getBytes (str text) "UTF-8"))})

(defn- council-input [db a b]
  (let [gid (str (:id (ps/db-create-project! db {:name "Benchmark"})))
        _ (doseq [sid [a b]] (ps/db-set-session-project! db sid gid))
        state (atom {})
        active {:activation-id "benchmark-b" :group-id gid :state "running" :input-state state}
        fleet {a {:activation-id "benchmark-a" :group-id gid :state "running"} b active}
        _ (council/publish! db (constantly fleet)
                            {:session-id a :activation-id "benchmark-a" :source "host"}
                            {:content "Send the benchmark evidence." :ping [b] :reply_required true})]
    (mapv (fn [step]
            (let [key ["benchmark-turn" step]
                  batch (council/prepare-input! db b (:activation-id active) gid state key 8192)
                  text (:content (council/input-message batch))]
              (council/acknowledge-input! db b active key)
              (when (empty? (council/pending-replies db b gid state))
                (throw (ex-info "The benchmark reply was not due" {:step step})))
              (measure text)))
          (range 1 21))))

(defn- measure-handoff
  "The Svar estimate of the prefix that a child inherits from a synthetic parent history.
   An older checkout has `handoff-checkpoint`: its default mode gives the baseline."
  [request]
  (let [agents 'com.blockether.vis.internal.session.agents
        estimate (ns-resolve agents 'handoff-estimates)
        old (ns-resolve agents 'handoff-checkpoint)
        handoff (ns-resolve agents 'handoff)
        mark #(with-meta {:role %2 :content %3} {:vis.handoff/kind %1})
        ;; A deterministic past conversation, independent of the live trial and its answer.
        history (mapcat (fn [n]
                          [(mark :request "user"
                                 (str ";; -- USER-REQUEST --\n"
                                      "Review earlier release " n ". Compare its test results and keep the workspace unchanged."))
                           (mark :answer "assistant"
                                 (str/join "\n" (for [i (range 15)]
                                                  (str "Release " n " check " i
                                                       " completed. Read the report for the exact result."))))])
                        (range 1 31))
        messages (into [{:role "system" :content ";; -- SYSTEM-PROMPT --\nUse the Council tools."}
                        {:role "system" :content ";; -- PROJECT-INSTRUCTIONS --\nDo not edit the fixtures."}
                        {:role "system" :content (str ";; -- TURN-SYSTEM-CONTEXT --\n;; -- EXTENSIONS --\n"
                                                     ";; -- EXTENSION graphify --\nRead the code graph.\n")}]
                       (concat history
                               [(mark :gist "user" "# ⋯ folded t1/* · Keep the release evidence separate.")
                                (mark :request "user" (str ";; -- USER-REQUEST --\n" request))]))]
    {:handoff (cond old (let [row (estimate (old messages "full" "gpt-4o"))]
                          (-> row (dissoc :mode :full :bounded) (assoc :tokens (:full row))))
                    (and estimate handoff) (estimate (handoff messages "gpt-4o")))}))

(foundation/register!)

(let [db (ps/db-create-connection! (System/getenv "VIS_BENCH_DB"))
      a "10200000-0000-4000-8000-000000000001"
      b "10200000-0000-4000-8000-000000000002"
      env {:db-info db :session-id a}
      mode (System/getenv "VIS_BENCH_MEASURE")]
  (try
    (with-redefs-fn {#'toggles/enabled? #(contains? #{"council" "subagents" "introspection"} %)
                      #'council/lookup-timeout-ms 10000}
      (fn []
      (let [result (case mode
                     "handoff" (measure-handoff (System/getenv "VIS_BENCH_REQUEST"))
                     "session" (:result (extension/stamp-public-result-op (inspect/read-session env)))
                     "prompts" {:introspection (measure (inspect/prompt env))
                                :agents (measure (agents/prompt env))
                                :council (measure (council/prompt env))
                                :council_input (council-input db a b)})]
        (when-not (seq result) (throw (ex-info "No benchmark result" {:mode mode})))
        (println "VIS_BENCH_STATIC" (wire/json-str (wire/->wire result))))))
    (finally (ps/db-dispose-connection! db))))
(shutdown-agents)
