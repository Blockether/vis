(ns com.blockether.vis.internal.ctx-loop-test
  "Tests for the loop integration adapter — scope synthesis, ctx atom
   swapping, mutator binding wiring (tasks + facts model)."
  (:require
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-loop :as cl]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.persistance]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env []
  ;; Single ctx-atom carries engine state; single turn-state-atom
  ;; carries cursor + DB-id fields. Seed turn-state to a non-trivial
  ;; coordinate so synthesize-scope tests get a stable t2/i3/f5 cursor.
  {:ctx-atom (cl/make-ctx-atom "test-session")
   :turn-state-atom (atom {:turn-position 2 :iteration 3 :form-idx 4})})

(defn- warnings-of
  "Helper: read accumulated `:engine/warnings` from the ctx atom on env."
  [env]
  (-> env :ctx-atom deref :engine/warnings (or [])))

(defdescribe synthesize-scope-test
  (describe "synthesize-scope"
    (it "builds tN/iM/fK from the loop counters (form is 1-based)"
      (expect (= "t2/i3/f5" (cl/synthesize-scope (mk-env)))))

    (it ":iteration field accepts a {:position N} map (positional shape)"
      (let [env (assoc (mk-env)
                  :turn-state-atom (atom {:turn-position 2
                                          :iteration     {:position 7 :other :stuff}
                                          :form-idx      4}))]
        (expect (= "t2/i7/f5" (cl/synthesize-scope env)))))

    (it "defaults to t1/i1/f1 when atoms unset"
      (expect (= "t1/i1/f1" (cl/synthesize-scope {}))))))

(defdescribe cursor-snapshot-test
  (describe "cursor-snapshot"
    (it "produces engine-cursor shape {:turn :iter :next-form}"
      (expect (= {:turn 2 :iter 3 :next-form 5}
                (cl/cursor-snapshot (mk-env)))))

    (it "defaults safely on empty env"
      (expect (= {:turn 1 :iter 1 :next-form 1}
                (cl/cursor-snapshot {}))))))

(defdescribe build-sci-bindings-test
  (describe "SCI bindings build"
    (let [env (mk-env)
          bindings (cl/build-sci-bindings env)]
      (it "exposes exactly the surviving engine mutators"
        (expect (= #{'task-set! 'fact-set!
                     'task-depends! 'fact-depends!
                     'fact-contradicts! 'fact-contradicts-remove!}
                  (set (keys bindings)))))

      (it "each binding is a callable function"
        (expect (every? fn? (vals bindings)))))))

(defdescribe task-mutator-roundtrip-test
  (describe "task-set! through the binding mutates the ctx atom"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-sci-bindings env)
          ret (task-set :auth {:title "switch to bcrypt" :status :todo})]
      (it "returns :vis/silent (no echo)"
        (expect (= :vis/silent ret)))

      (it "ctx atom carries the new task"
        (expect (= "switch to bcrypt"
                  (get-in @(:ctx-atom env) [:session/tasks :auth :title]))))

      (it "the :born scope was stamped from the loop cursor"
        (expect (= "t2/i3/f5"
                  (get-in @(:ctx-atom env) [:session/tasks :auth :born])))))))

(defdescribe fact-mutator-roundtrip-test
  (describe "fact-set! through the binding mutates the ctx atom"
    (let [env (mk-env)
          {fact-set 'fact-set!} (cl/build-sci-bindings env)
          _ (fact-set :race {:content "non-atomic trim+conj"})]
      (it "ctx atom carries the new fact, :active by default"
        (expect (= "non-atomic trim+conj"
                  (get-in @(:ctx-atom env) [:session/facts :race :content]))))

      (it "the :born scope was stamped from the loop cursor"
        (expect (= "t2/i3/f5"
                  (get-in @(:ctx-atom env) [:session/facts :race :born])))))))

(defdescribe done-self-asserted-test
  (describe "task-set! :status :done is accepted and stamped (self-asserted)"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-sci-bindings env)
          _ (task-set :ship {:title "ship it" :status :todo})
          _ (task-set :ship {:status :done})]
      (it "status is :done"
        (expect (= :done (get-in @(:ctx-atom env) [:session/tasks :ship :status]))))

      (it ":done-born stamped by the engine"
        (expect (some? (get-in @(:ctx-atom env) [:session/tasks :ship :done-born])))))))

(defdescribe fact-contradicts-binding-test
  (describe "fact-contradicts! through the binding writes symmetric link"
    (let [env (mk-env)
          {fact-set 'fact-set! fc 'fact-contradicts!} (cl/build-sci-bindings env)
          _ (fact-set :a {:content "bcrypt"})
          _ (fact-set :b {:content "argon2"})
          _ (fc :a :b)]
      (it "both facts carry the symmetric :contradicts link"
        (expect (contains? (get-in @(:ctx-atom env) [:session/facts :a :contradicts]) :b))
        (expect (contains? (get-in @(:ctx-atom env) [:session/facts :b :contradicts]) :a))))))

(defdescribe cycle-hard-reject-test
  (describe "task-set! :depends-on cycle is hard-rejected via the binding"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-sci-bindings env)
          _ (task-set :a {:title "a" :status :todo})
          _ (task-set :b {:title "b" :status :todo :depends-on [:a]})
          _ (task-set :a {:depends-on [:b]})]
      (it ":a has no :depends-on (write refused)"
        (expect (not (contains? (get-in @(:ctx-atom env) [:session/tasks :a])
                       :depends-on))))

      (it ":depends-on-cycle warning captured"
        (expect (some #(= :depends-on-cycle (:code %))
                  (warnings-of env)))))))

(defdescribe drain-warnings-test
  (describe "drain-warnings! returns + clears"
    (let [env (mk-env)
          {fc 'fact-contradicts!} (cl/build-sci-bindings env)
          ;; fact-contradicts! on a missing fact emits a warning onto the atom
          _ (fc :missing-a :missing-b)]
      (it "drain yields the recorded warnings"
        (let [ws (cl/drain-warnings! env)]
          (expect (>= (count ws) 1))))

      (it "second drain returns empty (atom cleared)"
        (expect (empty? (cl/drain-warnings! env)))))))

(defdescribe render-block-extension-ctx-test
  (it "merges top-level extension ctx, including :session/workspace"
    (let [env (mk-env)
          rendered (with-redefs [extension/ctx-contributions
                                 (constantly {:session/workspace
                                              {:workspace/root "/repo"
                                               :workspace/sandbox? false
                                               :vcs/kind :git
                                               :vcs/ref "main"
                                               :vcs/mainline "main"}})]
                     (cl/render-block! env (fn [{:keys [ctx]}] ctx)))]
      (expect (= :git (get-in rendered [:session/workspace :vcs/kind])))
      (expect (= "main" (get-in rendered [:session/workspace :vcs/ref]))))))

(defdescribe render-block-payload-test
  (describe "render-block! passes {:ctx :warnings} and surfaces :session/warnings"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-sci-bindings env)
          ;; produce a structural warning: task :done with a non-terminal dep
          _ (task-set :prereq {:title "first" :status :todo})
          _ (task-set :follow {:title "second" :depends-on [:prereq] :status :done})
          captured (atom nil)
          rendered (cl/render-block! env
                     (fn [payload] (reset! captured payload) (:ctx payload)))]
      (it "renderer receives a map with :ctx and :warnings keys"
        (expect (contains? @captured :ctx))
        (expect (contains? @captured :warnings)))

      (it ":warnings is a vec of short strings (not maps)"
        (expect (vector? (:warnings @captured)))
        (expect (every? string? (:warnings @captured)))
        (expect (seq (:warnings @captured))))

      (it "rendered ctx carries :session/warnings as the same string vec"
        (expect (= (:warnings @captured) (:session/warnings rendered)))
        (expect (every? string? (:session/warnings rendered))))

      (it "rendered ctx carries NO :session/stages"
        (expect (not (contains? rendered :session/stages)))))))

(defdescribe current-ctx-test
  (describe "current-ctx stamps the cursor at render time"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-sci-bindings env)
          _ (task-set :s {:title "x" :status :todo})
          c (cl/current-ctx env)]
      (it "ctx carries the task"
        (expect (= "x" (get-in c [:session/tasks :s :title]))))

      (it ":session/scope reflects current cursor"
        (expect (= {:turn 2 :iter 3 :next-form 5}
                  (:session/scope c))))

      (it "ctx is engine-valid against ::cs/ctx after live mutations"
        ;; engine consumers (renderer, derive-warnings) need a valid ctx
        (expect (some? (:session/id c)))
        (expect (some? (:session/turn c)))
        (expect (some? (:session/scope c)))
        (expect (= "test-session" (:session/id c)))))))

(defdescribe end-to-end-mini-scenario-test
  (describe "small scenario via SCI bindings: facts → tasks → deps → done"
    (let [env (mk-env)
          {tk 'task-set! ft 'fact-set! td 'task-depends!} (cl/build-sci-bindings env)
          _ (ft :rl-bug {:content "race"})
          _ (tk :swap {:title "CAS" :status :doing})
          _ (tk :test {:title "property test" :status :todo})
          _ (td :test [:swap])
          _ (tk :swap {:status :done})
          ctx (cl/current-ctx env)]
      (it "ctx has the fact and both tasks"
        (expect (= 1 (count (:session/facts ctx))))
        (expect (= 2 (count (:session/tasks ctx)))))

      (it ":test depends on :swap (via task-depends!)"
        (expect (= [:swap] (get-in ctx [:session/tasks :test :depends-on]))))

      (it ":swap is :done with :done-born stamped"
        (expect (= :done (get-in ctx [:session/tasks :swap :status])))
        (expect (some? (get-in ctx [:session/tasks :swap :done-born])))))))

;; =============================================================================
;; Introspect verb bindings
;; =============================================================================

(defdescribe introspect-bindings-test
  (describe "build-introspect-bindings"
    (let [env (mk-env)
          {tk 'task-set!} (cl/build-sci-bindings env)
          _   (tk :swap {:title "CAS rewrite" :status :doing})
          ;; History loader returns a vec of [turn ctx] pairs as if loaded
          ;; from persistance. We fake one prior turn snapshot.
          prior-ctx (-> (eng/empty-ctx "test-session")
                      (assoc :session/turn 1)
                      (assoc-in [:session/facts :rl-bug]
                        {:content "old race fact" :born "t1/i1/f1"}))
          history-loader (constantly [[1 prior-ctx]])
          {itask 'introspect-task
           ifact 'introspect-fact
           iarch 'introspect-archived
           ictx  'introspect-ctx-at}
          (cl/build-introspect-bindings env history-loader)]

      (it "introspect-task finds task in LIVE ctx"
        (let [r (itask :swap)]
          (expect (= "CAS rewrite" (:title r)))))

      (it "introspect-fact finds fact from PRIOR turn (history)"
        (let [r (ifact :rl-bug)]
          (expect (= "old race fact" (:content r)))))

      (it "introspect-archived enumerates entries missing from latest"
        ;; the LIVE ctx (treated as latest turn 2 from env atoms) has no
        ;; :rl-bug; turn 1 had it → archived
        (let [arch (iarch :facts)]
          (expect (some #(= :rl-bug (:key %)) arch))))

      (it "introspect-ctx-at \"t1\" returns the turn 1 snapshot"
        (let [snap (ictx "t1")]
          (expect (= "old race fact"
                    (get-in snap [:session/facts :rl-bug :content])))))

      (it "introspect-ctx-at \"t99\" → nil for unknown turn"
        (expect (nil? (ictx "t99"))))

      (it "introspect-ctx-at malformed string → nil"
        (expect (nil? (ictx "bogus"))))

      (it "introspect-ctx-at accepts integer too"
        (expect (= "old race fact"
                  (get-in (ictx 1) [:session/facts :rl-bug :content])))))))

(defdescribe db-backed-introspect-diagnostics-test
  (describe "introspect-form / introspect-iter diagnostics"
    (let [env (assoc (mk-env) :db-info ::db :session-id "S")
          turns [{:id "soul-1" :position 1}
                 {:id "soul-2" :position 2}]
          iters-by-soul {"soul-1" [{:id "it-1" :position 1 :status "done"
                                    :code "(+ 1 2)"
                                    :forms [{:scope "t1/i1/f1"
                                             :tag :observation
                                             :src "(+ 1 2)"
                                             :result 3}]}]
                         ;; current cursor is t2/i3 from mk-env; no row exists
                         ;; until the whole fence finishes.
                         "soul-2" []}
          bindings (cl/build-introspect-bindings env (constantly []))
          introspect-form (get bindings 'introspect-form)
          introspect-iter (get bindings 'introspect-iter)
          with-db (fn [f]
                    (with-redefs [com.blockether.vis.internal.persistance/db-list-session-turns
                                  (constantly turns)
                                  com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                                  (fn [_db soul-id] (get iters-by-soul soul-id))]
                      (f)))]
      (it "returns completed prior form payloads"
        (with-db
          #(expect (= 3 (:result (introspect-form "t1/i1/f1"))))))

      (it "current-iteration introspect returns explicit diagnostic, not nil"
        (with-db
          #(let [r (introspect-form "t2/i3/f1")]
             (expect (= :introspect-scope-unavailable (:vis/error r)))
             (expect (= :current-iteration-not-persisted (:reason r))))))

      (it "malformed iter scope returns explicit diagnostic, not nil"
        (with-db
          #(let [r (introspect-iter "bogus")]
             (expect (= :introspect-scope-unavailable (:vis/error r)))
             (expect (= :malformed-scope (:reason r)))))))))

;; =============================================================================
;; trailer→form-results projection
;; =============================================================================

(defdescribe trailer-form-results-test
  (describe "trailer->form-results"
    (let [trailer [{:scope "t1/i1"
                    :forms [{:scope "t1/i1/f1" :tag :observation :src "(read)"
                             :result "ok"}
                            {:scope "t1/i1/f2" :tag :mutation :src "(write!)"
                             :result :ok}]}
                   {:scope "t1/i2"
                    :forms [{:scope "t1/i2/f1" :tag :observation :src "(/ 1 0)"
                             :error {:message "boom"}}]}
                   ;; summary entries get skipped (no :forms)
                   {:scope-start "t0/i1" :scope-end "t0/i5"
                    :summary "older summarized" :born "t1/i1/f1"}]
          fr (cl/trailer->form-results trailer)]

      (it "keys by scope-form string"
        (expect (contains? fr "t1/i1/f1"))
        (expect (contains? fr "t1/i1/f2"))
        (expect (contains? fr "t1/i2/f1")))

      (it "skips summary entries"
        (expect (= 3 (count fr))))

      (it "preserves :result for ok forms"
        (expect (= "ok" (:result (get fr "t1/i1/f1")))))

      (it "preserves :error for errored forms"
        (expect (= "boom" (:message (:error (get fr "t1/i2/f1")))))))))

;; =============================================================================
;; Phase F: trailer-find — FTS5 query → ranked iteration scopes.
;;
;; Composition test only — the actual SQLite FTS5 semantics live in the
;; persistance suite. Here we wire mocked db-search + db-list-* and assert
;; that trailer-find joins the hits with turn positions and applies the
;; declared filters.
;; =============================================================================

(defdescribe trailer-find-test
  (describe "trailer-find composes db-search hits into iter scopes"
    (let [history-loader (constantly [])
          bindings (cl/build-introspect-bindings
                     {:db-info ::db :session-id "S"}
                     history-loader)
          trailer-find (get bindings 'trailer-find)]
      (it "is exposed under the 'trailer-find binding"
        (expect (some? trailer-find)))

      (it "returns nil/empty when :src-matches is blank or missing"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (constantly [])]
          (expect (nil? (trailer-find nil)))
          (expect (nil? (trailer-find {:src-matches ""}))))))

    (let [;; mock 2 turns × 2 iters; an FTS5 hit on iter-2 of turn-1
          turns [{:id "soul-1" :position 1}
                 {:id "soul-2" :position 2}]
          iters-by-soul {"soul-1" [{:id "it-1a" :position 1}
                                   {:id "it-1b" :position 2}]
                         "soul-2" [{:id "it-2a" :position 1}]}
          bindings (cl/build-introspect-bindings
                     {:db-info ::db :session-id "S"}
                     (constantly []))
          trailer-find (get bindings 'trailer-find)]

      (it "joins owner-id to a turn+iter position and emits the tN/iM scope"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (fn [_db q _opts]
                        (when (= q "v/rg")
                          [{:owner-table "session_turn_iteration"
                            :owner-id    "it-1b"
                            :field       "code"
                            :snippet     "(v/rg […])"
                            :rank        -1.234}]))
                      com.blockether.vis.internal.persistance/db-list-session-turns
                      (constantly turns)
                      com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                      (fn [_db soul-id] (get iters-by-soul soul-id))]
          (let [hits (trailer-find {:src-matches "v/rg" :limit 20})]
            (expect (= 1 (count hits)))
            (expect (= "t1/i2" (:scope (first hits))))
            (expect (= "(v/rg […])" (:preview (first hits)))))))

      (it ":scope-after filters out hits at or before the cursor"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (constantly
                        [{:owner-table "session_turn_iteration"
                          :owner-id    "it-1a" :field "code"
                          :snippet "old hit" :rank -2.0}
                         {:owner-table "session_turn_iteration"
                          :owner-id    "it-2a" :field "code"
                          :snippet "new hit" :rank -3.0}])
                      com.blockether.vis.internal.persistance/db-list-session-turns
                      (constantly turns)
                      com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                      (fn [_db soul-id] (get iters-by-soul soul-id))]
          (let [hits (trailer-find {:src-matches "x" :scope-after "t1/i1"})]
            ;; t1/i1 itself filtered; t2/i1 kept (turn>cursor.turn)
            (expect (= ["t2/i1"] (mapv :scope hits)))))))))
