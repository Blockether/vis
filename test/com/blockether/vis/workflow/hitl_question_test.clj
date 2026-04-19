(ns com.blockether.vis.workflow.hitl-question-test
  "Integration test for 'Do you support HITL?' — the real-world query that
   surfaced two bugs in the `9a41b4e2-309b-4d96-bd6d-ac21dcb8d116` conversation:

   1. Agent thrash — 21 iterations of hand-rolled regex searching because there
      was no `grep` tool; the LLM kept cycling through broken approaches.
   2. Late `ClassCastException: String cannot be cast to Pattern` — `str/split`
      with a string delimiter failed only when a lazy `keep` was realized during
      mustache rendering, crashing the turn after all iterations had “succeeded”.

   This test scripts the LLM via `with-redefs` on `llm/ask!` and exercises the
   **real** `list-dir`, `read-file`, and `grep` tools against a temp workspace.
   It pins the public contract:

     * grep is registered and callable from the SCI sandbox.
     * `str/split` now tolerates string delimiters without throwing
       (previously burned whole turns through the lazy-seq path).
     * An LLM can answer the HITL question in a small, fixed iteration budget."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.conversations.core :as conversations]
            [com.blockether.vis.loop.conversations.shared :as shared]
            [com.blockether.vis.core :as rlm]
            [com.blockether.vis.loop.core :as rlm-core]))

;;; ── Helpers ─────────────────────────────────────────────────────────────

(defn- zero-usage []
  {:result {}
   :tokens {:input 0 :output 0 :total 0 :reasoning 0 :cached 0}
   :cost   {:input-cost 0 :output-cost 0 :total-cost 0}
   :duration-ms 0})

(defn- scripted-llm
  "Build a stub for `llm/ask!` that returns pre-canned iteration responses in
   order. Throws loudly if the rlm outruns the script."
  [responses]
  (let [queue (atom (vec responses))]
    (fn [_router _opts]
      (let [[head & rest] @queue]
        (when-not head
          (throw (ex-info "scripted-llm exhausted" {})))
        (reset! queue (vec rest))
        (merge (zero-usage) {:result head})))))

(defn- with-temp-db*
  "Pin `config/db-path` to a fresh temp directory, reset the conversations
   cache, clean up after the body."
  [f]
  (let [original-path config/db-path
        temp-dir (str (fs/create-temp-dir {:prefix "vis-hitl-test-"}))]
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

(defn- seed-hitl-workspace!
  "Write a miniature project tree with HITL-ish content so the real file tools
   have something to find. Returns the workspace path as a string, canonicalized
   so it matches the paths list-dir/grep report (macOS /var → /private/var)."
  []
  (let [ws (str (fs/create-temp-dir {:prefix "vis-hitl-ws-"}))]
    (fs/create-dirs (fs/path ws "src/guard"))
    (fs/create-dirs (fs/path ws "src/core"))
    (spit (str (fs/path ws "src/guard/approval.clj"))
      (str "(ns guard.approval)\n"
        "\n"
        ";; HITL gate for dangerous operations.\n"
        "(defn require-human-approval [action]\n"
        "  (println \"[HITL] confirm?\" action))\n"))
    (spit (str (fs/path ws "src/core/executor.clj"))
      (str "(ns core.executor\n"
        "  \"No HITL in this module — auto-executes every tool call.\")\n"
        "\n"
        "(defn run-tool [t] (t))\n"))
    (spit (str (fs/path ws "README.md"))
      (str "# demo project\n\n"
        "Supports human-in-the-loop (HITL) via guard/approval.\n"))
    (.getCanonicalPath (java.io.File. ws))))

(defn- sandbox-value
  "Return the live value of `sym` in the conversation's SCI sandbox, or nil
   if unbound. Mirrors the helper used in forget_restore_test."
  [env sym]
  (let [v (get-in @(:env (:sci-ctx env)) [:namespaces 'sandbox sym])]
    (cond
      (nil? v) nil
      (instance? clojure.lang.IDeref v) @v
      :else v)))

;;; ── Tool registration smoke test ────────────────────────────────────────

(defdescribe grep-tool-registration-test
  (describe "grep tool is part of the base tool set"
    (it "is included in shared/base-tools"
      (let [syms (set (map :sym shared/base-tools))]
        (expect (contains? syms 'grep))
        (expect (contains? syms 'list-dir))
        (expect (contains? syms 'read-file))))

    (it "is bound in the SCI sandbox of a fresh conversation"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "grep binding"})
              env     (conversations/env-for id)
              sandbox (get-in @(:env (:sci-ctx env)) [:namespaces 'sandbox])]
          (expect (contains? sandbox 'grep))
          (expect (contains? sandbox 'list-dir))
          (expect (contains? sandbox 'read-file))
          (conversations/delete! id))))))

;;; ── grep fires for real against real files ─────────────────────────────

(defdescribe grep-finds-hitl-content-test
  (describe "grep invoked from SCI returns real matches"
    (it "finds HITL references across seeded files"
      (with-temp-db
        (let [ws          (seed-hitl-workspace!)
              {id :id}    (conversations/create! :cli {:title "grep hitl"})
              env         (conversations/env-for id)
              ;; No :glob — default walk covers every file in the tree,
              ;; including root-level README.md.
              result      (#'rlm-core/execute-code env
                            (str "(grep \"HITL\" \"" ws "\""
                              " {:case-insensitive? true})"))
              grep-output (:result result)]
          (expect (nil? (:error result)))
          (expect (map? grep-output))
          (expect (vector? (:matches grep-output)))
          (expect (pos? (count (:matches grep-output))))
          (let [paths (set (map :path (:matches grep-output)))]
            (expect (some #(str/includes? % "approval.clj") paths))
            (expect (some #(str/includes? % "README.md") paths))
            (expect (some #(str/includes? % "executor.clj") paths)))
          (fs/delete-tree ws)
          (conversations/delete! id))))

    (it "respects :glob to scope the search to a subtree"
      (with-temp-db
        (let [ws       (seed-hitl-workspace!)
              {id :id} (conversations/create! :cli {:title "grep scoped"})
              env      (conversations/env-for id)
              result   (#'rlm-core/execute-code env
                         (str "(grep \"HITL\" \"" ws "\""
                           " {:glob \"**/guard/*.clj\" :case-insensitive? true})"))
              paths    (->> result :result :matches (map :path) set)]
          (expect (nil? (:error result)))
          (expect (pos? (count paths)))
          (expect (every? #(str/includes? % "guard/") paths))
          (fs/delete-tree ws)
          (conversations/delete! id))))

    (it "is ReDoS-safe — catastrophic patterns terminate quickly under re2j"
      ;; Adversarial pattern `(a+)+$` against `aaaa...aaa!` triggers exponential
      ;; backtracking in `java.util.regex`. Under re2j it runs in linear time.
      ;; This guard keeps grep immune to prompt-injected ReDoS attacks.
      (with-temp-db
        (let [ws       (str (fs/create-temp-dir {:prefix "vis-redos-"}))
              _        (spit (str (fs/path ws "bomb.txt"))
                         (str (apply str (repeat 40 "a")) "!"))
              {id :id} (conversations/create! :cli {:title "grep ReDoS guard"})
              env      (conversations/env-for id)
              start    (System/currentTimeMillis)
              result   (#'rlm-core/execute-code env
                         (str "(grep \"(a+)+$\" \"" ws "\" {:max-matches 5})"))
              elapsed  (- (System/currentTimeMillis) start)]
          (expect (nil? (:error result)))
          (expect (map? (:result result)))
          ;; Linear-time guarantee: even ~40 a's + ! under `(a+)+$` must
          ;; finish in well under a second with re2j. `java.util.regex` would
          ;; take exponentially longer on the same input.
          (expect (< elapsed 1000))
          (fs/delete-tree ws)
          (conversations/delete! id))))))

;;; ── str/split no longer throws on a string delimiter ────────────────────

(defdescribe str-split-string-delimiter-test
  (describe "str/split accepts a string delimiter in the SCI sandbox"
    (it "returns the split vector instead of throwing"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "str/split string"})
              env     (conversations/env-for id)
              result  (#'rlm-core/execute-code env
                        "(str/split \"a,b,c\" \",\")")]
          (expect (nil? (:error result)))
          (expect (= ["a" "b" "c"] (:result result)))
          (conversations/delete! id))))

    (it "still accepts a regex Pattern unchanged"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "str/split regex"})
              env     (conversations/env-for id)
              result  (#'rlm-core/execute-code env
                        "(str/split \"a  b   c\" #\"\\s+\")")]
          (expect (nil? (:error result)))
          (expect (= ["a" "b" "c"] (:result result)))
          (conversations/delete! id))))

    (it "survives a lazy keep that only realizes string-delim splits downstream"
      ;; Reproducer for the `9a41b4e2` crash: LLM put `(str/split s \"\\n\")`
      ;; inside a `(keep ...)`. The lazy seq hid the cast until mustache
      ;; rendering forced realization, blowing up the whole turn.
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "lazy split"})
              env     (conversations/env-for id)
              result  (#'rlm-core/execute-code env
                        (str "(doall (keep (fn [x]"
                          " (when (pos? (count x))"
                          " (count (str/split x \"\\n\"))))"
                          " [\"a\\nb\" \"c\"]))"))]
          (expect (nil? (:error result)))
          (expect (= [2 1] (:result result)))
          (conversations/delete! id))))))

;;; ── End-to-end scripted HITL conversation ──────────────────────────────

(defdescribe hitl-question-scripted-conversation-test
  (describe "LLM can answer 'Do you support HITL?' using list-dir + read-file + grep"
    (it "completes the turn in a small, fixed iteration budget"
      (with-temp-db
        (let [ws       (seed-hitl-workspace!)
              {id :id} (conversations/create! :cli {:title "HITL question"})
              listing-expr
              (str "(def listing (list-dir \"" ws "\""
                " {:glob \"**/*\" :depth 5 :limit 200}))")
              grep-expr
              (str "(def hits (grep \"HITL|approval|human-in-the-loop\" \""
                ws "\" {:case-insensitive? true}))")
              read-expr
              (str "(def approval-src (read-file \""
                ws "/src/guard/approval.clj\" 1 20))")
              script
              [{:code [{:expr listing-expr :time-ms 500}] :next-optimize "intelligence"}
               {:code [{:expr grep-expr :time-ms 500}] :next-optimize "intelligence"}
               {:code [{:expr read-expr :time-ms 500}] :next-optimize "intelligence"}
               {:answer "vis supports HITL — see guard/approval.clj: require-human-approval."
                :answer-type "mustache-text"
                :confidence "high"}]]
          (with-redefs [llm/ask! (scripted-llm script)]
            (let [result (conversations/send! id "Do you support HITL?"
                           {:max-iterations 5})
                  env (conversations/env-for id)]
              (expect (string? (:answer result)))
              (expect (str/includes? (:answer result) "HITL"))
              (expect (= 4 (:iterations result)))

              ;; Real tool outputs landed in the sandbox.
              (let [listing-v (sandbox-value env 'listing)]
                (expect (map? listing-v))
                (expect (vector? (:entries listing-v)))
                (expect (pos? (count (:entries listing-v)))))

              (let [hits-v (sandbox-value env 'hits)]
                (expect (map? hits-v))
                (expect (vector? (:matches hits-v)))
                (expect (pos? (count (:matches hits-v))))
                ;; At least one match must reference the approval file we seeded.
                (expect (boolean
                          (some #(str/includes? (:path %) "approval.clj")
                            (:matches hits-v)))))

              (let [src (sandbox-value env 'approval-src)]
                (expect (string? src))
                (expect (str/includes? src "require-human-approval")))))
          (fs/delete-tree ws)
          (conversations/delete! id))))

    (it "does NOT thrash: a 3-iteration budget is enough when grep exists"
      ;; Regression guard. `9a41b4e2` burned 21 iterations searching for HITL.
      ;; With a real grep the same task fits in three iterations comfortably.
      (with-temp-db
        (let [ws       (seed-hitl-workspace!)
              {id :id} (conversations/create! :cli {:title "HITL fast path"})
              script
              [{:code [{:expr (str "(def hits (grep \"HITL\" \"" ws "\""
                                " {:case-insensitive? true}))")
                        :time-ms 500}]}
               {:code [{:expr (str "(def src (read-file \""
                                ws "/src/guard/approval.clj\" 1 10))")
                        :time-ms 500}]}
               {:answer "Yes — HITL gate lives in guard/approval.clj."
                :answer-type "mustache-text"
                :confidence "high"}]]
          (with-redefs [llm/ask! (scripted-llm script)]
            (let [result (conversations/send! id "Do you support HITL?"
                           {:max-iterations 3})]
              (expect (= 3 (:iterations result)))
              (expect (string? (:answer result)))
              (expect (str/includes? (:answer result) "HITL"))))
          (fs/delete-tree ws)
          (conversations/delete! id))))))

;;; ── Live integration (opt-in via OPENAI_API_KEY) ───────────────────────

(def ^:private integration-providers
  [{:id :openai
    :api-key (System/getenv "OPENAI_API_KEY")
    :base-url (or (System/getenv "OPENAI_BASE_URL")
                "https://api.openai.com/v1")
    :models [{:name "gpt-4o-mini"} {:name "gpt-4o"}]}])

(defn- live-integration-enabled?
  []
  (some? (:api-key (first integration-providers))))

(defdescribe hitl-question-live-test
  (describe "live model can answer the HITL question using the real tools"
    (it "a real model finishes in <= 8 iterations when grep is available"
      (when (live-integration-enabled?)
        (with-temp-db
          (let [ws     (seed-hitl-workspace!)
                router (llm/make-router integration-providers)
                env    (rlm/create-env router {:db config/db-path})]
            (try
              (shared/register-base-tools! env)
              (let [result (rlm/query-env! env
                             [(llm/user
                                (str "Look inside " ws " and tell me whether this project "
                                  "supports HITL (human-in-the-loop). Use list-dir, read-file, "
                                  "and grep. Answer yes/no with the file that proves it."))]
                             {:max-iterations 8 :refine? false
                              :system-prompt
                              (str "You are a code investigator. You have read-file, list-dir, "
                                "and grep available. Prefer grep for content searches. Keep "
                                "answers short.")})]
                (expect (string? (:answer result)))
                (expect (str/includes? (str/lower-case (:answer result)) "hitl"))
                (expect (<= (:iterations result) 8)))
              (finally
                (fs/delete-tree ws)
                (rlm/dispose-env! env)))))))))
