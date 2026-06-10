(ns com.blockether.vis.internal.ctx-persistence-integration-test
  "End-to-end integration: build a real env subset against an in-memory
   SQLite DB, mutate the CTX through the loop's SCI bindings, snapshot via
   the run-turn! path, drop the env atoms (simulating a Vis kill), rebuild
   env on the same session-id, and verify the CTX engine state survives.

   No real LLM call — we drive the SCI bindings directly. The point is to
   prove the **persistence + restore** layer works end-to-end against the
   real schema + Nippy serialization. If this test passes, kill-vis-and-
   restart preserves tasks/facts/trailer/cursor correctly."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- via [env sym & args]
  (let [{f sym} (ctx-loop/build-engine-bindings env)]
    (apply f args)))

(defn- simulate-turn!
  "Drive one full turn against a (real) DB: invoke the caller's mutations
   on the SCI-bound env, then stamp the cursor + run gc-pass + persist the
   ctx-atom to session_turn_state.ctx the same way run-turn! does in prod.
   Returns the persisted (post-gc) ctx for assertions."
  [db-info session-turn-id env mutate-fn]
  (mutate-fn env)
  (let [stamped (ctx-loop/stamp-cursor env @(:ctx-atom env))
        gced    (eng/gc-pass stamped)
        snap    (dissoc gced :session/scope)]
    (reset! (:ctx-atom env) snap)
    (persistance/db-update-session-turn! db-info session-turn-id
      {:status :done :iteration-count 1 :duration-ms 1 :ctx snap})
    snap))

(defn- mk-env
  "Loop env subset that ctx-loop bindings + helpers actually touch.
   Single ctx-atom; single turn-state-atom."
  [db-info session-id turn-pos]
  {:db-info         db-info
   :session-id      session-id
   :ctx-atom        (ctx-loop/make-ctx-atom session-id)
   :turn-state-atom (atom {:turn-position turn-pos :iteration 1 :form-idx 0})})

(defdescribe end-to-end-persist-restart-test
  (describe "engine state survives DB round-trip"
    (it "tasks / facts persist across env rebuild on the same session"
      (let [db-info (vis/db-create-connection! :memory)]
        (try
          (let [ws (vis/db-workspace-insert! db-info
                       {:repo-id "test" :repo-root "/tmp" :root "/tmp"
                        :state :active :fork-ms 0})
                session-id (vis/db-store-session!
                             db-info {:channel :tui :title "integration"
                                      :workspace-id (:id ws)})

                ;; ── Turn 1: define fact + task
                turn1-id (vis/db-store-session-turn!
                           db-info {:parent-session-id session-id
                                    :user-request "Verify rate limiter"
                                    :status :running})
                env1 (mk-env db-info session-id 1)
                ;; "CAS rewrite" slugs to the snake_case plan-step key
                ;; "cas_rewrite"; a lone :doing step stays :doing.
                ctx1 (simulate-turn! db-info turn1-id env1
                       (fn [e]
                         (via e 'fact-set! :rl-bug {:content "swap! race observed"})
                         (via e 'update-plan! [{:title "CAS rewrite"
                                                :status "doing"}])))]
            (expect (= "swap! race observed"
                      (get-in ctx1 [:session/facts :rl-bug :content])))
            (expect (= "CAS rewrite" (get-in ctx1 [:session/tasks "cas_rewrite" :title])))
            (expect (= :doing (get-in ctx1 [:session/tasks "cas_rewrite" :status])))

            ;; ── Verify db-load-latest-ctx returns the same shape
            (let [persisted (persistance/db-load-latest-ctx db-info session-id)]
              (expect (= "swap! race observed"
                        (get-in persisted [:session/facts :rl-bug :content])))
              (expect (= "CAS rewrite" (get-in persisted [:session/tasks "cas_rewrite" :title]))))

            ;; ── Restart: drop env1, rebuild env2 fresh; load ctx from DB
            (let [env2 (mk-env db-info session-id 2)
                  loaded (persistance/db-load-latest-ctx db-info session-id)]
              (when loaded
                (reset! (:ctx-atom env2) (assoc loaded :session/id session-id)))

              ;; env2 sees turn 1's task immediately
              (expect (= "CAS rewrite"
                        (get-in @(:ctx-atom env2) [:session/tasks "cas_rewrite" :title])))

              ;; ── Turn 2: self-assert the task done; engine stamps :done-born
              (let [turn2-id (vis/db-store-session-turn!
                               db-info {:parent-session-id session-id
                                        :user-request "Close it out"
                                        :status :running})
                    ctx2 (simulate-turn! db-info turn2-id env2
                           (fn [e]
                             (via e 'plan-step! "cas_rewrite" {:status "done"})))]
                (expect (= "swap! race observed"
                          (get-in ctx2 [:session/facts :rl-bug :content])))
                (expect (= :done (get-in ctx2 [:session/tasks "cas_rewrite" :status])))
                (expect (some? (get-in ctx2 [:session/tasks "cas_rewrite" :done-born])))

                ;; Second restart sees the done status too
                (let [reloaded (persistance/db-load-latest-ctx db-info session-id)]
                  (expect (= :done (get-in reloaded [:session/tasks "cas_rewrite" :status])))))

              ;; ── History loader sees BOTH turns
              (let [history (persistance/db-load-ctx-history db-info session-id)
                    as-of-1 (get (into {} history) 1)
                    as-of-2 (get (into {} history) 2)]
                (expect (= 2 (count history)))
                (expect (some? as-of-1))
                (expect (some? as-of-2))
                ;; turn 1 had the task :doing; turn 2 flipped it :done
                (expect (= :doing (get-in as-of-1 [:session/tasks "cas_rewrite" :status])))
                (expect (= :done (get-in as-of-2 [:session/tasks "cas_rewrite" :status]))))))
          (finally (vis/db-dispose-connection! db-info)))))))

(defdescribe ttl-gc-survives-restart-test
  (describe "gc-pass + persistence: archived from live, kept in history"
    (it "task aged past TTL is dropped from live ctx but reachable via history"
      (let [db-info (vis/db-create-connection! :memory)]
        (try
          (let [ws (vis/db-workspace-insert! db-info
                       {:repo-id "test" :repo-root "/tmp" :root "/tmp"
                        :state :active :fork-ms 0})
                session-id (vis/db-store-session!
                             db-info {:channel :tui :title "ttl"
                                      :workspace-id (:id ws)})

                ;; Turn 1: write an :old-task with :done-born = "t1/i1/f1"
                turn1-id (vis/db-store-session-turn!
                           db-info {:parent-session-id session-id
                                    :user-request "old turn"
                                    :status :running})
                env1 (mk-env db-info session-id 1)
                ;; title "x" slugs to plan-step key "x"; status done.
                _ (simulate-turn! db-info turn1-id env1
                    (fn [e]
                      (via e 'update-plan! [{:title "x" :status "done"}])
                      (swap! (:ctx-atom e)
                        assoc-in [:session/tasks "x" :done-born] "t1/i1/f1")))

                ;; Advance loop to turn 8 (= 7 turns past the :done-born.turn=1,
                ;; exceeding the task-done TTL of 6). gc-pass at turn-8 snapshot
                ;; should archive :old-task.
                turn8-id (vis/db-store-session-turn!
                           db-info {:parent-session-id session-id
                                    :user-request "current turn"
                                    :status :running})
                env8 (mk-env db-info session-id 8)
                loaded (persistance/db-load-latest-ctx db-info session-id)
                _ (when loaded
                    (reset! (:ctx-atom env8) (assoc loaded :session/id session-id)))
                ;; plan-step! APPENDS without wiping the prior plan, so the
                ;; aged-out "x" stays in live ctx and is archived by gc-pass
                ;; (TTL), not by the whole-plan replace.
                ctx8 (simulate-turn! db-info turn8-id env8
                       (fn [e]
                         (via e 'plan-step! "current" {:title "current" :status "todo"})))]
            ;; live ctx after turn 8: "x" gone (aged out), "current" alive
            (expect (nil? (get-in ctx8 [:session/tasks "x"])))
            (expect (= "current" (get-in ctx8 [:session/tasks "current" :title])))

            ;; History: turn 1 snapshot still carries the aged-out "x" step
            (let [history (persistance/db-load-ctx-history db-info session-id)
                  t1-snap (get (into {} history) 1)]
              (expect (some? (get-in t1-snap [:session/tasks "x"])))))
          (finally (vis/db-dispose-connection! db-info)))))))

;; =============================================================================
;; The TASK TREE rides the SAME V1 ctx blob — no schema change needed.
;; Proves :parent / :composite / :decorators / :evidence on tasks AND the fact
;; :depends_on / :contradicts relations all survive the real DB round-trip, and
;; that derived-outcome rolls up correctly on the RESTORED data.
;; =============================================================================

(defdescribe task-tree-survives-db-round-trip-test
  (describe "the full tree + fact relations round-trip the V1 ctx blob (no new tables)"
    (it "persists + restores parent/composite/decorators/evidence + fact depends_on/contradicts"
      (let [db-info (vis/db-create-connection! :memory)]
        (try
          (let [ws (vis/db-workspace-insert! db-info
                     {:repo-id "test" :repo-root "/tmp" :root "/tmp"
                      :state :active :fork-ms 0})
                session-id (vis/db-store-session! db-info
                             {:channel :tui :title "tree-persist" :workspace-id (:id ws)})
                turn1-id (vis/db-store-session-turn! db-info
                           {:parent-session-id session-id
                            :user-request "build auth tree" :status :running})
                env1 (mk-env db-info session-id 1)
                ctx1 (simulate-turn! db-info turn1-id env1
                       (fn [e]
                         ;; selector parent + two children; one child carries a
                         ;; retry decorator, the other a done+evidence close.
                         (via e 'update-plan!
                           [{:key "auth"   :title "auth" :composite "selector"}
                            {:key "oauth"  :title "oauth" :parent "auth"
                             :decorators [{:type "retry" :n 3}]}
                            {:key "apikey" :title "apikey" :parent "auth"
                             :status "done" :acceptance "key works"
                             :evidence "ran probe -> 200"}])
                         ;; two facts wired by DECLARATIVE relations on fact_set
                         (via e 'fact-set! :ev-a {:content "oauth needs secret"})
                         (via e 'fact-set! :ev-b {:content "apikey simpler"
                                                  :depends_on [[:fact :ev-a]]
                                                  :contradicts [:ev-a]})))]
            ;; live ctx carries the tree
            (expect (= :selector (get-in ctx1 [:session/tasks "auth" :composite])))
            (expect (= [{:type :retry :n 3}] (get-in ctx1 [:session/tasks "oauth" :decorators])))

            ;; ── reload from the REAL DB blob: the whole tree + relations survive
            (let [loaded (persistance/db-load-latest-ctx db-info session-id)]
              (expect (= :selector (get-in loaded [:session/tasks "auth" :composite])))
              (expect (= "auth" (get-in loaded [:session/tasks "oauth" :parent])))
              (expect (= "auth" (get-in loaded [:session/tasks "apikey" :parent])))
              (expect (= [{:type :retry :n 3}] (get-in loaded [:session/tasks "oauth" :decorators])))
              (expect (= "ran probe -> 200" (get-in loaded [:session/tasks "apikey" :evidence])))
              ;; fact relations survive (depends_on vec + symmetric contradicts)
              (expect (= [[:fact :ev-a]] (get-in loaded [:session/facts :ev-b :depends_on])))
              (expect (contains? (get-in loaded [:session/facts :ev-b :contradicts]) :ev-a))
              (expect (contains? (get-in loaded [:session/facts :ev-a :contradicts]) :ev-b))
              ;; ROLLUP works on RESTORED data: a selector parent with a done
              ;; child rolls up to :success even though a sibling is still pending.
              (expect (= :success (eng/derived-outcome loaded "auth"))))

            ;; ── DEDICATED STORES (slice 2): the write-through also landed real
            ;; task/fact ROWS (not just the blob), keyed by session_state.
            (let [ss-id (persistance/db-latest-session-state-id db-info session-id)
                  tasks (persistance/db-list-tasks db-info ss-id)
                  facts (persistance/db-list-facts db-info ss-id)]
              (expect (= #{"auth" "oauth" "apikey"} (set (keys tasks))))
              ;; thawed entity carries the full shape, incl. the `order` column path
              (expect (= :selector (:composite (get tasks "auth"))))
              (expect (= "auth" (:parent (get tasks "oauth"))))
              (expect (= [{:type :retry :n 3}] (:decorators (get tasks "oauth"))))
              (expect (= 1 (:order (get tasks "auth"))))   ; the renamed `order` column round-trips
              ;; facts as rows too (keys stored as strings), relations intact
              (expect (= #{"ev-a" "ev-b"} (set (keys facts))))
              (expect (= [[:fact :ev-a]] (:depends_on (get facts "ev-b"))))))
          (finally (vis/db-dispose-connection! db-info)))))))
