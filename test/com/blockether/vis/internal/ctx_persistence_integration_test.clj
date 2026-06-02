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
  (let [{f sym} (ctx-loop/build-sci-bindings env)]
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
          (let [ws (vis/workspace-ensure-trunk! db-info {})
                session-id (vis/db-store-session!
                             db-info {:channel :tui :title "integration"
                                      :workspace-id (:id ws)})

                ;; ── Turn 1: define fact + task
                turn1-id (vis/db-store-session-turn!
                           db-info {:parent-session-id session-id
                                    :user-request "Verify rate limiter"
                                    :status :running})
                env1 (mk-env db-info session-id 1)
                ctx1 (simulate-turn! db-info turn1-id env1
                       (fn [e]
                         (via e 'fact-set! :rl-bug {:content "swap! race observed"})
                         (via e 'task-set! :swap {:title "CAS rewrite"
                                                  :status :doing})))]
            (expect (= "swap! race observed"
                      (get-in ctx1 [:session/facts :rl-bug :content])))
            (expect (= "CAS rewrite" (get-in ctx1 [:session/tasks :swap :title])))
            (expect (= :doing (get-in ctx1 [:session/tasks :swap :status])))

            ;; ── Verify db-load-latest-ctx returns the same shape
            (let [persisted (persistance/db-load-latest-ctx db-info session-id)]
              (expect (= "swap! race observed"
                        (get-in persisted [:session/facts :rl-bug :content])))
              (expect (= "CAS rewrite" (get-in persisted [:session/tasks :swap :title]))))

            ;; ── Restart: drop env1, rebuild env2 fresh; load ctx from DB
            (let [env2 (mk-env db-info session-id 2)
                  loaded (persistance/db-load-latest-ctx db-info session-id)]
              (when loaded
                (reset! (:ctx-atom env2) (assoc loaded :session/id session-id)))

              ;; env2 sees turn 1's task immediately
              (expect (= "CAS rewrite"
                        (get-in @(:ctx-atom env2) [:session/tasks :swap :title])))

              ;; ── Turn 2: self-assert the task done; engine stamps :done-born
              (let [turn2-id (vis/db-store-session-turn!
                               db-info {:parent-session-id session-id
                                        :user-request "Close it out"
                                        :status :running})
                    ctx2 (simulate-turn! db-info turn2-id env2
                           (fn [e]
                             (via e 'task-set! :swap {:status :done})))]
                (expect (= "swap! race observed"
                          (get-in ctx2 [:session/facts :rl-bug :content])))
                (expect (= :done (get-in ctx2 [:session/tasks :swap :status])))
                (expect (some? (get-in ctx2 [:session/tasks :swap :done-born])))

                ;; Second restart sees the done status too
                (let [reloaded (persistance/db-load-latest-ctx db-info session-id)]
                  (expect (= :done (get-in reloaded [:session/tasks :swap :status])))))

              ;; ── History loader sees BOTH turns
              (let [history (persistance/db-load-ctx-history db-info session-id)
                    as-of-1 (get (into {} history) 1)
                    as-of-2 (get (into {} history) 2)]
                (expect (= 2 (count history)))
                (expect (some? as-of-1))
                (expect (some? as-of-2))
                ;; turn 1 had the task :doing; turn 2 flipped it :done
                (expect (= :doing (get-in as-of-1 [:session/tasks :swap :status])))
                (expect (= :done (get-in as-of-2 [:session/tasks :swap :status]))))))
          (finally (vis/db-dispose-connection! db-info)))))))

(defdescribe ttl-gc-survives-restart-test
  (describe "gc-pass + persistence: archived from live, kept in history"
    (it "task aged past TTL is dropped from live ctx but reachable via history"
      (let [db-info (vis/db-create-connection! :memory)]
        (try
          (let [ws (vis/workspace-ensure-trunk! db-info {})
                session-id (vis/db-store-session!
                             db-info {:channel :tui :title "ttl"
                                      :workspace-id (:id ws)})

                ;; Turn 1: write an :old-task with :done-born = "t1/i1/f1"
                turn1-id (vis/db-store-session-turn!
                           db-info {:parent-session-id session-id
                                    :user-request "old turn"
                                    :status :running})
                env1 (mk-env db-info session-id 1)
                _ (simulate-turn! db-info turn1-id env1
                    (fn [e]
                      (via e 'task-set! :old-task {:title "x" :status :done})
                      (swap! (:ctx-atom e)
                        assoc-in [:session/tasks :old-task :done-born] "t1/i1/f1")))

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
                ctx8 (simulate-turn! db-info turn8-id env8
                       (fn [e]
                         (via e 'task-set! :new-task
                           {:title "current" :status :todo})))]
            ;; live ctx after turn 8: :old-task gone, :new-task alive
            (expect (nil? (get-in ctx8 [:session/tasks :old-task])))
            (expect (= "current" (get-in ctx8 [:session/tasks :new-task :title])))

            ;; History: turn 1 snapshot still carries :old-task
            (let [history (persistance/db-load-ctx-history db-info session-id)
                  t1-snap (get (into {} history) 1)]
              (expect (some? (get-in t1-snap [:session/tasks :old-task])))))
          (finally (vis/db-dispose-connection! db-info)))))))
