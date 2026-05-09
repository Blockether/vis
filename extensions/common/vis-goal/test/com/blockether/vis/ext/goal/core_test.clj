(ns com.blockether.vis.ext.goal.core-test
  "End-to-end tests against the goal extension. Uses the in-memory
   SQLite store from `test-helpers` (vis-persistance-sqlite) so every
   test gets an isolated DB and the full extension_aggregate write
   path is exercised."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.goal.core :as goal]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
   [com.blockether.vis.internal.extension :as ext]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it throws? throws-with-msg?]]))

(h/use-mem-store!)

;; =============================================================================
;; Read-only / no-goal
;; =============================================================================

(defdescribe goal-read-empty-test
  (it "returns nil when no goal is set"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (expect (nil? (goal/get-goal s cid)))))

  (it "returns nil for missing args"
    (expect (nil? (goal/get-goal nil "anything")))
    (expect (nil? (goal/get-goal :fake-store nil)))))

;; =============================================================================
;; Set
;; =============================================================================

(defdescribe goal-set-test
  (it "set-then-get round-trips with paused-aware elapsed snapshot"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          set (goal/set-goal! s cid {:objective "Finish migration" :set-by :user})
          got (goal/get-goal s cid)]
      (expect (= "Finish migration" (:objective set)))
      (expect (= :active (:status set)))
      (expect (= :user   (:set-by set)))
      (expect (nil?      (:done-reason set)))
      (expect (zero?     (:total-paused-ms set)))
      (expect (some?     (:started-at-ms set)))
      (expect (= (:objective set) (:objective got)))
      (expect (= (:status    set) (:status    got)))
      (expect (>= (long (:elapsed-ms got)) 0))))

  (it "rejects blank objective with :vis/goal-objective-blank"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (expect (throws-with-msg? clojure.lang.ExceptionInfo #"non-blank"
                (fn [] (goal/set-goal! s cid {:objective "" :set-by :user}))))
      (expect (throws-with-msg? clojure.lang.ExceptionInfo #"non-blank"
                (fn [] (goal/set-goal! s cid {:objective "   " :set-by :user}))))))

  (it "rejects 4001-char objective"
    (let [s    (h/store)
          cid  (vis/db-store-conversation! s {:channel :tui})
          huge (apply str (repeat 4001 \X))]
      (expect (throws? clojure.lang.ExceptionInfo
                (fn [] (goal/set-goal! s cid {:objective huge :set-by :user}))))))

  (it "rejects unknown :set-by"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (expect (throws-with-msg? clojure.lang.ExceptionInfo #":set-by"
                (fn [] (goal/set-goal! s cid {:objective "x" :set-by :rogue})))))))

;; =============================================================================
;; Pause / Resume
;; =============================================================================

(defdescribe goal-pause-resume-test
  (it "pause moves :active -> :paused and stamps paused-at-ms"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          out (goal/pause-goal! s cid)]
      (expect (= :paused (:status out)))
      (expect (some?     (:paused-at-ms out)))))

  (it "resume accumulates total-paused-ms across two pause cycles"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          _   (goal/pause-goal! s cid)
          _   (Thread/sleep 30)
          mid (goal/resume-goal! s cid)
          mid-paused (long (:total-paused-ms mid))
          _   (goal/pause-goal! s cid)
          _   (Thread/sleep 30)
          end (goal/resume-goal! s cid)
          end-paused (long (:total-paused-ms end))]
      (expect (>= mid-paused 25))
      (expect (>= end-paused (+ mid-paused 25)))
      (expect (= :active (:status end)))
      (expect (nil?      (:paused-at-ms end)))))

  (it "double-pause is a no-op (idempotent)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          a   (goal/pause-goal! s cid)
          _   (Thread/sleep 5)
          b   (goal/pause-goal! s cid)]
      (expect (= :paused (:status a)))
      (expect (= :paused (:status b)))
      ;; paused-at frozen on first pause; second pause does NOT
      ;; reset the clock.
      (expect (= (:paused-at-ms a) (:paused-at-ms b)))))

  (it "double-resume is a no-op"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          a   (goal/resume-goal! s cid)
          b   (goal/resume-goal! s cid)]
      (expect (= :active (:status a)))
      (expect (= :active (:status b)))))

  (it "pause when no goal exists throws :vis/goal-illegal-transition"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (expect (throws? clojure.lang.ExceptionInfo
                (fn [] (goal/pause-goal! s cid)))))))

;; =============================================================================
;; Mark done
;; =============================================================================

(defdescribe goal-mark-done-test
  (it "active -> done(:achieved) freezes elapsed and records reason"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          _   (Thread/sleep 30)
          out (goal/mark-goal-done! s cid :achieved)]
      (expect (= :done     (:status out)))
      (expect (= :achieved (:done-reason out)))
      (expect (>= (long (:elapsed-ms out)) 25))))

  (it "paused -> done folds final pause window into total-paused-ms"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal!   s cid {:objective "x" :set-by :user})
          _   (goal/pause-goal! s cid)
          _   (Thread/sleep 30)
          out (goal/mark-goal-done! s cid :unmet)]
      (expect (= :done  (:status out)))
      (expect (= :unmet (:done-reason out)))
      (expect (nil?     (:paused-at-ms out)))
      (expect (>= (long (:total-paused-ms out)) 25))))

  (it "done -> mark-done again throws :vis/goal-illegal-transition"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal!         s cid {:objective "x" :set-by :user})
          _   (goal/mark-goal-done!  s cid :achieved)]
      (expect (throws? clojure.lang.ExceptionInfo
                (fn [] (goal/mark-goal-done! s cid :achieved))))))

  (it "rejects unknown reason"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})]
      (expect (throws-with-msg? clojure.lang.ExceptionInfo #"reason"
                (fn [] (goal/mark-goal-done! s cid :nope)))))))

;; =============================================================================
;; Clear
;; =============================================================================

(defdescribe goal-clear-test
  (it "clear wipes objective and stamps reason=cleared"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal!  s cid {:objective "x" :set-by :user})
          out (goal/clear-goal! s cid)]
      (expect (= :done    (:status out)))
      (expect (= :cleared (:done-reason out)))
      (expect (nil?       (:objective out)))))

  (it "clear is idempotent on an already-cleared goal"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal!  s cid {:objective "x" :set-by :user})
          _   (goal/clear-goal! s cid)
          out (goal/clear-goal! s cid)]
      (expect (= :done    (:status out)))
      (expect (= :cleared (:done-reason out)))))

  (it "clear when no goal exists is a tolerant no-op (returns nil)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (expect (nil? (goal/clear-goal! s cid))))))

;; =============================================================================
;; Replace + soul-level scope
;; =============================================================================

(defdescribe goal-replace-test
  (it "set-after-done starts a fresh active goal (timer reset)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal!         s cid {:objective "first" :set-by :user})
          _   (goal/mark-goal-done!  s cid :achieved)
          out (goal/set-goal!         s cid {:objective "second" :set-by :user})]
      (expect (= "second" (:objective out)))
      (expect (= :active  (:status out)))
      (expect (zero?      (:total-paused-ms out)))
      (expect (nil?       (:done-reason out))))))

(defdescribe goal-soul-level-scope-test
  ;; Goal must survive state forks (Q1: soul-level). After forking
  ;; the conversation_state into a new version, looking up the goal
  ;; by conversation-id (which IS the soul id) must still see the
  ;; same row \u2014 because the extension_aggregate row is keyed on
  ;; conversation-soul-id, not state-id.
  (it "goal stays attached to the soul across a state fork"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})
          _   (goal/set-goal! s cid {:objective "soul-level" :set-by :user})
          _   (vis/db-fork-conversation! s cid {})
          out (goal/get-goal s cid)]
      (expect (= "soul-level" (:objective out)))
      (expect (= :active      (:status out))))))

;; =============================================================================
;; Persistence shape \u2014 row scoped to the soul, kind/key correct
;; =============================================================================

;; =============================================================================
;; Extension self-hook — :ext/prompt fragment lands inside <extensions>
;; =============================================================================

(defdescribe goal-ext-prompt-hook-test
  (it "emits no <conversation_goal> when there is no goal"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          env {:db-info s :conversation-id cid}
          out (prompt/assemble-system-prompt env
                {:active-extensions [goal/vis-extension]
                 :system-prompt nil})]
      (expect (false? (str/includes? out "<conversation_goal")))))

  (it "emits the <conversation_goal> block (inside <extensions>) when goal is :active"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          env {:db-info s :conversation-id cid}
          _   (goal/set-goal! s cid {:objective "Ship goal feature" :set-by :user})
          out (prompt/assemble-system-prompt env
                {:active-extensions [goal/vis-extension]
                 :system-prompt nil})]
      (expect (str/includes? out "<conversation_goal"))
      (expect (str/includes? out "Ship goal feature"))
      (expect (str/includes? out "<extensions>"))))

  (it "omits the block when the goal is :done (no live instruction to inject)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          env {:db-info s :conversation-id cid}
          _   (goal/set-goal!         s cid {:objective "x" :set-by :user})
          _   (goal/mark-goal-done!  s cid :achieved)
          out (prompt/assemble-system-prompt env
                {:active-extensions [goal/vis-extension]
                 :system-prompt nil})]
      (expect (false? (str/includes? out "<conversation_goal"))))))

;; =============================================================================
;; Sandbox surface — model-facing `goal/` symbols
;; =============================================================================

(defdescribe goal-sandbox-symbols-test
  (it "exposes 6 symbols (status / set / pause / resume / clear / mark) under goal/"
    (let [syms (:ext/symbols goal/vis-extension)]
      (expect (= #{'status 'set 'pause 'resume 'clear 'mark}
                (set (map :ext.symbol/sym syms))))))

  (it "declares a `goal/` ns-alias"
    (expect (= 'goal (-> goal/vis-extension :ext/ns-alias :alias))))

  (it "strips `env` from the model-facing arglists (so the rendered
      prompt shows `(goal/set objective)` not `(goal/set env objective)`)"
    (let [out (ext/render-prompt {:heading "Goal"
                                  :ext/ns-alias (:ext/ns-alias goal/vis-extension)
                                  :ext/symbols  (:ext/symbols goal/vis-extension)})]
      (expect (str/includes? out "(goal/status)"))
      (expect (str/includes? out "(goal/set objective)"))
      (expect (str/includes? out "(goal/pause)"))
      (expect (str/includes? out "(goal/resume)"))
      (expect (str/includes? out "(goal/clear)"))
      (expect (str/includes? out "(goal/mark reason)"))
      ;; Defensive: no `env` should leak into any rendered call form.
      (expect (false? (str/includes? out "(goal/status env)")))
      (expect (false? (str/includes? out "(goal/set env")))))

  (it "impl fns return the same shape get-goal returns when called
      with a synthetic env (defends against future drift between the
      sandbox path and the underlying public Clojure API)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          env {:db-info s :conversation-id cid}]
      (expect (nil? (#'goal/status-impl env)))
      (let [set-out (#'goal/set-impl env "x")]
        (expect (= :active (:status set-out)))
        (expect (= :model  (:set-by set-out))))
      (let [pause-out (#'goal/pause-impl env)]
        (expect (= :paused (:status pause-out))))
      (let [resume-out (#'goal/resume-impl env)]
        (expect (= :active (:status resume-out))))
      (let [mark-out (#'goal/mark-impl env :achieved)]
        (expect (= :done     (:status mark-out)))
        (expect (= :achieved (:done-reason mark-out)))))))

(defdescribe goal-row-shape-test
  (it "writes exactly one extension_aggregate row per soul"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})]
      (expect (= 1 (h/raw-count s :extension_aggregate)))
      ;; Idempotent UPSERT: pause/resume/mark do NOT create new rows.
      (goal/pause-goal! s cid)
      (goal/resume-goal! s cid)
      (goal/mark-goal-done! s cid :achieved)
      (expect (= 1 (h/raw-count s :extension_aggregate)))))

  (it "row is scoped to the conversation soul, not state"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          row (first (h/raw-query s {:select [:extension_id :kind :scope_key
                                              :conversation_soul_id]
                                     :from   :extension_aggregate}))]
      (expect (= "com.blockether.vis.ext.goal.core" (:extension_id row)))
      ;; kind is stored as edn-text by the persistence layer
      (expect (= "\"goal-state\"" (:kind row)))
      (expect (clojure.string/starts-with? (:scope_key row) "conversation-soul:"))
      (expect (some? (:conversation_soul_id row))))))
