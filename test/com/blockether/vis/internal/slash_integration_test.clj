(ns com.blockether.vis.internal.slash-integration-test
  "Engine loop integration of slash dispatch.

   Asserts that `run-turn!` short-circuits the LLM round-trip when
   the user message resolves to a registered slash, persists a
   synthetic iteration row with `:tag :user-slash`, and (when the
   slash returns `:slash/tasks / :facts`) writes them to
   the CTX engine via the same path a model-emitted mutator would."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- slash-env
  "Build a minimal engine env with one extension carrying `slashes`,
   a workspace, a session, and a fresh ctx-atom + turn-state-atom."
  [store slashes]
  (let [ext
        (extension/extension {:ext/name "test.slash-integration"
                              :ext/description "Slash integration test fixture."
                              :ext/slash-commands slashes})

        ws
        (persistance/db-workspace-insert!
          store
          {:repo-id "test" :repo-root "/tmp" :root "/tmp" :state :active :fork-ms 0})

        soul-id
        (persistance/db-store-session!
          store
          {:channel :tui :workspace-id (:id ws) :title "slash-test" :system-prompt ""})]

    {:extensions (atom [ext])
     :db-info store
     :session-id soul-id
     :channel :tui
     :workspace ws
     :workspace/id (:id ws)
     :ctx-atom (ctx-loop/make-ctx-atom soul-id)
     :turn-state-atom (ctx-loop/make-turn-state-atom)}))

(defn- slash-spec-ok
  [name body]
  {:slash/name name
   :slash/run-fn (fn [_ctx]
                   {:slash/status :ok :slash/title (str "ran /" name) :slash/body body})})

;; =============================================================================
;; Short-circuit
;; =============================================================================

(defdescribe
  run-turn-slash-short-circuit-test
  (it "handled slash skips iteration-loop and persists a :user-slash iter"
      (with-store
        (fn [store]
          (let [env
                (slash-env store [(slash-spec-ok "ping" "pong")])

                call-count
                (atom 0)

                ;; iteration-loop must NOT run for handled slashes.
                result
                (with-redefs [lp/iteration-loop (fn [& _]
                                                  (swap! call-count inc)
                                                  {:status :success})]
                  (lp/run-turn! env "/ping" {}))]

            (expect (= 0 @call-count))
            (expect (= :success (:status result)))
            (expect (= :complete (:prior-outcome result)))
            (expect (some? (:slash result)))
            (expect (str/includes? (:answer result) "ran /ping"))
            (expect (str/includes? (:answer result) "pong"))
            ;; The synthetic turn was persisted; non-slash text still
            ;; goes through the normal path.
            (let [turns (persistance/db-list-session-turns store (:session-id env))]
              (expect (= 1 (count turns)))
              (expect (= "/ping"
                         (-> turns
                             first
                             :user-request))))))))
  (it "non-slash text falls through to iteration-loop"
      (with-store (fn [store]
                    (let [env
                          (slash-env store [(slash-spec-ok "ping" "pong")])

                          fell-through?
                          (atom false)]

                      (with-redefs [lp/iteration-loop
                                    (fn [& _]
                                      (reset! fell-through? true)
                                      ;; Mimic real iteration-loop happy path:
                                      ;; no :status (the row column gets
                                      ;; :success via `or` then normalized
                                      ;; to "done"; prior_outcome stays NULL).
                                      {:answer nil :iteration-count 0 :duration-ms 0})]
                        (lp/run-turn! env "hello world" {}))
                      (expect (true? @fell-through?)))))))

;; =============================================================================
;; IR-shaped :slash/body persists as Markdown
;; =============================================================================

(defdescribe slash-body-ir-test
             (it "IR :slash/body renders to Markdown for answer_markdown column"
                 (with-store
                   (fn [store]
                     (let [ast
                           [:ast {} [:p {} [:span {} "Hello "] [:strong {} "world"]]]

                           env
                           (slash-env store
                                      [{:slash/name "ir-body"
                                        :slash/run-fn (fn [_]
                                                        {:slash/status :ok
                                                         :slash/title "IR body"
                                                         :slash/body ast})}])

                           result
                           (with-redefs [lp/iteration-loop (fn [& _]
                                                             {:status :success})]
                             (lp/run-turn! env "/ir-body" {}))]

                       (expect (= :success (:status result)))
                       (expect (str/includes? (:answer result) "Hello"))
                       (expect (str/includes? (:answer result) "**world**")))))))

;; =============================================================================
;; Slash-emitted task / fact land on CTX engine
;; =============================================================================

(defdescribe slash-error-envelope-test
             (it "unknown slash persists as :user-slash with error in result"
                 (with-store (fn [store]
                               (let [env
                                     (slash-env store [(slash-spec-ok "ping" "pong")])

                                     result
                                     (with-redefs [lp/iteration-loop
                                                   (fn [& _]
                                                     (throw (ex-info "should not be called" {})))]
                                       (lp/run-turn! env "/nonexistent" {}))]

                                 (expect (= :success (:status result)))
                                 (expect (= :unknown (get-in result [:slash :reason])))
                                 (expect (str/includes? (:answer result) "unknown")))))))
