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

(defdescribe build-engine-bindings-test
  (describe "engine bindings build"
    (let [env (mk-env)
          bindings (cl/build-engine-bindings env)]
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
          {task-set 'task-set!} (cl/build-engine-bindings env)
          ret (task-set :auth {:title "switch to bcrypt" :status :todo})]
      (it "returns the vis_silent sentinel (no echo)"
        (expect (= "vis_silent" ret)))

      (it "ctx atom carries the new task"
        (expect (= "switch to bcrypt"
                  (get-in @(:ctx-atom env) [:session/tasks :auth :title]))))

      (it "the :born scope was stamped from the loop cursor"
        (expect (= "t2/i3/f5"
                  (get-in @(:ctx-atom env) [:session/tasks :auth :born])))))))

(defdescribe fact-mutator-roundtrip-test
  (describe "fact-set! through the binding mutates the ctx atom"
    (let [env (mk-env)
          {fact-set 'fact-set!} (cl/build-engine-bindings env)
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
          {task-set 'task-set!} (cl/build-engine-bindings env)
          _ (task-set :ship {:title "ship it" :status :todo})
          _ (task-set :ship {:status :done})]
      (it "status is :done"
        (expect (= :done (get-in @(:ctx-atom env) [:session/tasks :ship :status]))))

      (it ":done-born stamped by the engine"
        (expect (some? (get-in @(:ctx-atom env) [:session/tasks :ship :done-born])))))))

(defdescribe fact-contradicts-binding-test
  (describe "fact-contradicts! through the binding writes symmetric link"
    (let [env (mk-env)
          {fact-set 'fact-set! fc 'fact-contradicts!} (cl/build-engine-bindings env)
          _ (fact-set :a {:content "bcrypt"})
          _ (fact-set :b {:content "argon2"})
          _ (fc :a :b)]
      (it "both facts carry the symmetric :contradicts link"
        (expect (contains? (get-in @(:ctx-atom env) [:session/facts :a :contradicts]) :b))
        (expect (contains? (get-in @(:ctx-atom env) [:session/facts :b :contradicts]) :a))))))

(defdescribe cycle-hard-reject-test
  (describe "task-set! :depends_on cycle is hard-rejected via the binding"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-engine-bindings env)
          _ (task-set :a {:title "a" :status :todo})
          _ (task-set :b {:title "b" :status :todo :depends_on [:a]})
          _ (task-set :a {:depends_on [:b]})]
      (it ":a has no :depends_on (write refused)"
        (expect (not (contains? (get-in @(:ctx-atom env) [:session/tasks :a])
                       :depends_on))))

      (it ":depends_on_cycle warning captured"
        (expect (some #(= :depends_on_cycle (:code %))
                  (warnings-of env)))))))

(defdescribe drain-warnings-test
  (describe "drain-warnings! returns + clears"
    (let [env (mk-env)
          {fc 'fact-contradicts!} (cl/build-engine-bindings env)
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
  (describe "render-block! passes {:ctx :warnings} and surfaces :session/hints"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-engine-bindings env)
          ;; produce a structural warning: task :done with a non-terminal dep
          _ (task-set :prereq {:title "first" :status :todo})
          _ (task-set :follow {:title "second" :depends_on [:prereq] :status :done})
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

      (it "session-view conjoins the structural warns into :session/hints maps"
        (let [hints (:session/hints (eng/session-view (:ctx @captured) (:warnings @captured)))]
          (expect (seq hints))
          (expect (every? map? hints))
          (expect (every? #(= :engine (:source %)) hints))
          (expect (= (set (:warnings @captured)) (set (map :content hints))))))

      (it "rendered ctx carries NO :session/stages"
        (expect (not (contains? rendered :session/stages)))))))

(defdescribe current-ctx-test
  (describe "current-ctx stamps the cursor at render time"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-engine-bindings env)
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
  (describe "small scenario via engine bindings: facts → tasks → deps → done"
    (let [env (mk-env)
          {tk 'task-set! ft 'fact-set! td 'task-depends!} (cl/build-engine-bindings env)
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
        (expect (= [:swap] (get-in ctx [:session/tasks :test :depends_on]))))

      (it ":swap is :done with :done-born stamped"
        (expect (= :done (get-in ctx [:session/tasks :swap :status])))
        (expect (some? (get-in ctx [:session/tasks :swap :done-born])))))))

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
;; recall — the single recovery verb (window by address / search by content)
;; =============================================================================

(defdescribe recall-window-entity-test
  (describe "recall by entity address windows a live value"
    (let [env (mk-env)
          {tk 'task-set!} (cl/build-engine-bindings env)
          _ (tk :swap {:title "CAS rewrite" :status :doing})
          {recall 'recall} (cl/build-introspect-bindings env (constantly []))]

      (it "recall :K windows a stored entity's value"
        (let [r (recall :swap)]
          (expect (= ":swap" (:vis/recall r)))
          (expect (string? (:view r)))))

      (it "recall unknown key → :recall-target-not-found"
        (expect (= :recall-target-not-found (:vis/error (recall :nope))))))))

(defdescribe recall-restore-test
  (describe "recall {:ids … :why …} restores entities to live (why required)"
    (let [env (mk-env)
          {recall 'recall} (cl/build-introspect-bindings env (constantly []))]

      (it ":why is REQUIRED to restore"
        (expect (= :recall-requires-why (:vis/error (recall {:ids [:t3/auth]})))))

      (it "restores an archived entity by stable :id, stamping :recalled"
        (swap! (:ctx-atom env) assoc-in [:session/facts :auth]
          {:content "JWT" :status :archived :id :t3/auth :born "t3/i1/f1"})
        (let [r (recall {:ids [:t3/auth] :why "need auth decision"})
              live @(:ctx-atom env)]
          (expect (= :fact (:restored (first (get-in r [:recalled :ids])))))
          (expect (= :active (get-in live [:session/facts :auth :status])))
          (expect (= "need auth decision" (get-in live [:session/facts :auth :recalled :why])))))

      (it "unknown :id → :not-found in the report"
        (let [r (recall {:ids [:t9/ghost] :why "x"})]
          (expect (= :not-found (:vis/error (first (get-in r [:recalled :ids]))))))))))

(defdescribe recall-restore-from-archived-test
  (describe "recall {:ids …} restores a GC'd entity from :session/archived (no DB)"
    (let [env (mk-env)
          {recall 'recall} (cl/build-introspect-bindings env (constantly []))]
      ;; entity is no longer in live facts/tasks — it was captured into
      ;; :session/archived at gc-pass time, final state intact.
      (swap! (:ctx-atom env) assoc :session/archived
        {:t3/auth {:id :t3/auth :content "JWT 15min"
                   :vis/kind :fact :vis/key :auth}})
      (it "re-inserts the captured final state into live, in-memory"
        (let [r (recall {:ids [:t3/auth] :why "need final auth"})
              live @(:ctx-atom env)]
          (expect (= :fact (:restored (first (get-in r [:recalled :ids])))))
          (expect (= :active (get-in live [:session/facts :auth :status])))
          (expect (= "JWT 15min" (get-in live [:session/facts :auth :content])))
          (expect (= "need final auth" (get-in live [:session/facts :auth :recalled :why])))
          (expect (not (contains? (:session/archived live) :t3/auth))))))))

(defdescribe recall-window-form-test
  (describe "recall by form scope windows a DB-backed result"
    (let [env  (assoc (mk-env) :db-info ::db :session-id "S")
          big  (apply str (repeat 4000 "y"))
          turns [{:id "soul-1" :position 1} {:id "soul-2" :position 2}]
          iters {"soul-1" [{:id "it-1" :position 1 :status "done" :code "(+ 1 2)"
                            :forms [{:scope "t1/i1/f1" :tag :observation :src "(+ 1 2)" :result big}]}]
                 "soul-2" []}
          {recall 'recall} (cl/build-introspect-bindings env (constantly []))
          with-db (fn [f]
                    (with-redefs [com.blockether.vis.internal.persistance/db-list-session-turns
                                  (constantly turns)
                                  com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                                  (fn [_db sid] (get iters sid))]
                      (f)))]
      (it "recall \"tN/iM/fK\" windows a big form result with a token-budget cursor"
        (with-db
          (fn []
            (let [r (recall "t1/i1/f1" {:offset 0 :limit 200})]
              (expect (= 0 (first (:vis/window r))))    ; window starts at the offset
              (expect (pos? (second (:vis/window r))))  ; advanced forward by a token budget
              (expect (< (second (:vis/window r)) (:vis/size r)))
              (expect (integer? (:vis/size r)))         ; chars int, not a map
              (expect (some? (:vis/next r)))            ; scroll continue-call
              (expect (nil? (:vis/rewind r)))           ; rewind is GONE
              (expect (pos? (count (:view r))))         ; non-empty slice
              (expect (< (count (:view r)) (:vis/size r)))))))
      (it "recall unknown scope → :recall-target-not-found"
        (with-db
          (fn []
            (expect (= :recall-target-not-found
                      (:vis/error (recall "t9/i9/f9"))))))))))

(defdescribe recall-search-test
  (describe "recall {:match …} searches the raw iteration trace (summarized stays searchable)"
    (let [{recall 'recall} (cl/build-introspect-bindings
                             {:db-info ::db :session-id "S" :ctx-atom (atom {:session/trailer []})}
                             (constantly []))]
      (it ":match is REQUIRED in search mode"
        (expect (= :recall-requires-match (:vis/error (recall {:match ""}))))))

    (let [turns [{:id "soul-1" :position 1} {:id "soul-2" :position 2}]
          iters {"soul-1" [{:id "it-1a" :position 1} {:id "it-1b" :position 2}]
                 "soul-2" [{:id "it-2a" :position 1}]}
          mk-recall (fn [trailer]
                      (get (cl/build-introspect-bindings
                             {:db-info ::db :session-id "S"
                              :ctx-atom (atom {:session/trailer trailer})}
                             (constantly [])) 'recall))]
      (it "joins a hit to its tN/iM scope (default limit 10)"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (fn [_db q opts]
                        (when (and (= q "rg") (= :literal-text (:query-mode opts)))
                          [{:owner-table "session_turn_iteration" :owner-id "it-1b"
                            :field "code" :snippet "(rg …)" :rank -1.2}]))
                      com.blockether.vis.internal.persistance/db-list-session-turns (constantly turns)
                      com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                      (fn [_db sid] (get iters sid))]
          (let [hits ((mk-recall []) {:match "rg"})]
            (expect (= ["t1/i2"] (mapv :scope hits))))))

      (it "passes plain-text search through for punctuation-heavy literals"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (fn [_db q opts]
                        (expect (= "located in /vis" q))
                        (expect (= :literal-text (:query-mode opts)))
                        [{:owner-table "session_turn_iteration" :owner-id "it-1b"
                          :field "code" :snippet "located in [/vis]" :rank -1.2}])
                      com.blockether.vis.internal.persistance/db-list-session-turns (constantly turns)
                      com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                      (fn [_db sid] (get iters sid))]
          (let [hits ((mk-recall []) {:match "located in /vis"})]
            (expect (= ["t1/i2"] (mapv :scope hits))))))

      (it "returns a structured error when backend search fails"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (fn [_db _q _opts]
                        (throw (ex-info "fts blew up" {})))]
          (let [r ((mk-recall []) {:match "located in /vis"})]
            (expect (= :recall-search-failed (:vis/error r)))
            (expect (= "located in /vis" (:query r)))
            (expect (re-find #"plain-text history search" (:hint r))))))

      (it "INCLUDES hits inside a summarized range (summarized stays searchable)"
        (with-redefs [com.blockether.vis.internal.persistance/db-search
                      (constantly [{:owner-table "session_turn_iteration" :owner-id "it-1b"
                                    :field "code" :snippet "x" :rank -1.0}])
                      com.blockether.vis.internal.persistance/db-list-session-turns (constantly turns)
                      com.blockether.vis.internal.persistance/db-list-session-turn-iterations
                      (fn [_db sid] (get iters sid))]
          ;; t1/i2 sits inside the summary stub t1/i1..t1/i3, but search runs
          ;; over the raw session_turn_iteration rows (NOT the stub), so the
          ;; observation stays findable — summarizing compresses the prompt
          ;; view, it never buries the evidence. The model can then
          ;; (recall {:scopes ["t1/i2"]}) to re-materialise the raw forms.
          (let [hits ((mk-recall [{:scope-start "t1/i1" :scope-end "t1/i3" :summary "s"}]) {:match "x"})]
            (expect (= ["t1/i2"] (mapv :scope hits)))))))))

(defdescribe mid-turn-summarize-test
  (describe "summarize is callable mid-turn (not only at done)"
    (let [env (mk-env)
          {summarize 'summarize} (cl/build-introspect-bindings env (constantly []))]
      (it "collapses N facts into one summary fact + archives originals NOW"
        (swap! (:ctx-atom env) update :session/facts merge
          {:a {:content "alpha" :status :active :born "t1/i1/f1"}
           :b {:content "beta" :status :active :born "t1/i2/f1"}})
        (let [r (summarize {:facts [{:keys [:a :b] :into :ab :summary "settled"}]})
              c @(:ctx-atom env)]
          (expect (= {:facts [{:keys [:a :b] :into :ab :summary "settled"}]} (:summarized r)))
          (expect (= "settled" (get-in c [:session/facts :ab :content])))
          (expect (= :archived (get-in c [:session/facts :a :status]))))))))
