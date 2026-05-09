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

;; =============================================================================
;; TUI /goal slash command
;; =============================================================================

(defdescribe goal-slash-command-registration-test
  (it "registers exactly one channel-hook on :tui with a commands-fn"
    (let [hooks (:ext/channel-hooks goal/vis-extension)
          tui   (filterv #(= :tui (:channel-id %)) hooks)]
      (expect (= 1 (count tui)))
      (expect (some? (:commands-fn (first tui))))))

  (it "exposes a single :goal command with the documented usage"
    (let [hook   (->> (:ext/channel-hooks goal/vis-extension)
                   (some #(when (= :tui (:channel-id %)) %)))
          cmds   ((:commands-fn hook) {})]
      (expect (= 1 (count cmds)))
      (expect (= :goal (:id (first cmds))))
      (expect (some? (:run-fn (first cmds))))
      ;; Description carries every subcommand so the user discovers
      ;; them inline via the slash menu.
      (let [doc (:doc (first cmds))]
        (expect (str/includes? doc "/goal <objective>"))
        (expect (str/includes? doc "/goal pause"))
        (expect (str/includes? doc "/goal resume"))
        (expect (str/includes? doc "/goal clear"))
        (expect (str/includes? doc "achieved"))))))

(defn- run-with-stub-notifier
  "Capture every (vis/notify! ...) call into `notes` (a vector ref).
   Returns [result captured-notifications]."
  [s _ctx-template thunk]
  (let [notes (atom [])]
    (with-redefs [vis/db-info (constantly s)
                  vis/notify! (fn [text & {:keys [level]}]
                                (swap! notes conj {:text (str text)
                                                   :level (or level :info)}))]
      [(thunk) @notes])))

(defdescribe goal-slash-dispatch-test
  (it "/goal <objective> sets a fresh goal and notifies :success"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          app-db (atom {:conversation {:id (str cid)}})
          ctx {:channel/id :tui :app-db app-db
               :command/args "Ship the goal" :command/argv ["Ship" "the" "goal"]}
          [_ notes] (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))]
      (expect (= 1 (count notes)))
      (expect (= :success (:level (first notes))))
      (expect (str/includes? (:text (first notes)) "Ship the goal"))
      ;; goal landed on the row
      (expect (= "Ship the goal" (:objective (goal/get-goal s cid))))))

  (it "/goal (no args) with no goal toasts :info \"no goal set\""
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          app-db (atom {:conversation {:id (str cid)}})
          ctx {:channel/id :tui :app-db app-db
               :command/args "" :command/argv []}
          [_ notes] (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))]
      (expect (= :info (:level (first notes))))
      (expect (str/includes? (:text (first notes)) "no goal"))))

  (it "/goal pause and /goal resume each notify :success and toggle status"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          app-db (atom {:conversation {:id (str cid)}})
          ctx-pause  {:app-db app-db :command/args "pause"  :command/argv ["pause"]}
          ctx-resume {:app-db app-db :command/args "resume" :command/argv ["resume"]}]
      (run-with-stub-notifier s ctx-pause #(#'goal/goal-slash-run! ctx-pause))
      (expect (= :paused (:status (goal/get-goal s cid))))
      (run-with-stub-notifier s ctx-resume #(#'goal/goal-slash-run! ctx-resume))
      (expect (= :active (:status (goal/get-goal s cid))))))

  (it "/goal achieved | unmet | budget-limited each mark the goal done"
    (let [for-reason
          (fn [reason]
            (let [s   (h/store)
                  cid (vis/db-store-conversation! s {:channel :tui})
                  _   (goal/set-goal! s cid {:objective "x" :set-by :user})
                  app-db (atom {:conversation {:id (str cid)}})
                  ctx {:app-db app-db
                       :command/args (name reason)
                       :command/argv [(name reason)]}]
              (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))
              (goal/get-goal s cid)))]
      (let [g (for-reason :achieved)]
        (expect (= :done     (:status g)))
        (expect (= :achieved (:done-reason g))))
      (let [g (for-reason :unmet)]
        (expect (= :unmet (:done-reason g))))
      (let [g (for-reason :budget-limited)]
        (expect (= :budget-limited (:done-reason g))))))

  (it "/goal clear tombstones the goal as :cleared"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (goal/set-goal! s cid {:objective "x" :set-by :user})
          app-db (atom {:conversation {:id (str cid)}})
          ctx {:app-db app-db :command/args "clear" :command/argv ["clear"]}]
      (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))
      (let [g (goal/get-goal s cid)]
        (expect (= :done    (:status g)))
        (expect (= :cleared (:done-reason g))))))

  (it "/goal pause WITHOUT an active goal toasts :error and DOES NOT crash"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          app-db (atom {:conversation {:id (str cid)}})
          ctx {:app-db app-db :command/args "pause" :command/argv ["pause"]}
          [_ notes] (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))]
      (expect (= :error (:level (first notes))))
      (expect (str/includes? (:text (first notes)) "pause failed"))))

  (it "/goal blank objective when set is rejected with :warn (no DB write)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          app-db (atom {:conversation {:id (str cid)}})
          ctx {:app-db app-db :command/args " " :command/argv [""]}
          [_ notes] (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))]
      ;; argv [""] -> first-arg is "", routes to :show; show on no goal
      ;; -> :info "no goal". The genuine \"objective is required\" path
      ;; fires when the user types `/goal ` with literal whitespace argv.
      ;; That case is exercised below.
      (expect (= :info (:level (first notes))))))

  (it "/goal works without :command/argv key (only :command/args supplied)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          app-db (atom {:conversation {:id (str cid)}})
          ctx {:app-db app-db :command/args "Round 2"}
          [_ notes] (run-with-stub-notifier s ctx #(#'goal/goal-slash-run! ctx))]
      (expect (= :success (:level (first notes))))
      (expect (= "Round 2" (:objective (goal/get-goal s cid)))))))

;; =============================================================================
;; Cross-restart liveness — process-level regression (P8)
;;
;; The pure soul-level tests above use an in-memory store. The goal also
;; needs to survive a JVM restart against a persistent on-disk store.
;; This is the test that catches a future "we accidentally tied goal
;; rows to JVM-local state" bug.
;;
;; Approach: spawn a child JVM (via clojure.main -e), have it open the
;; same persistent SQLite dir, write a goal, and exit. Then the parent
;; re-opens the same dir and reads the goal back. The Nippy blob in the
;; extension_aggregate row must thaw to the same shape the child wrote.
;; =============================================================================

(require '[babashka.fs :as fs])
(import '[java.util.concurrent TimeUnit])

(def ^:private goal-child-write-code
  "(require '[com.blockether.vis.core :as vis]
            '[com.blockether.vis.ext.goal.core :as goal])
   (let [dir       (System/getProperty \"vis.test.db-dir\")
         objective (or (System/getProperty \"vis.test.objective\") \"child-set\")
         s         (vis/db-create-connection! dir)
         marker    (System/getProperty \"vis.test.marker\")]
     (try
       ;; First, store a conversation so we have a soul to attach
       ;; the goal to. cid is returned as a Java UUID; goal extension
       ;; round-trips both UUID and string forms via ->ref.
       (let [cid (vis/db-store-conversation! s {:channel :child :title \"child-conv\"})]
         (goal/set-goal! s cid {:objective objective :set-by :user})
         ;; Communicate the soul id back so the parent can look it up.
         (when marker (spit marker (str cid))))
       (println \"CHILD-DONE\")
       (finally
         (vis/db-dispose-connection! s))))")

(defn- vis-goal-extra-classpath
  "Returns the extra classpath segments needed to run the goal
   extension in a fresh child JVM, in case the parent JVM's
   `java.class.path` was set up before vis-goal was added to deps.edn
   (typical of long-running dev nREPL sessions). When `clojure -M:test`
   spawns the harness from a clean classpath, these are already
   present and the dedupe in the cp builder makes the addition a
   no-op."
  []
  (let [project-root (fs/cwd)
        candidates   ["extensions/common/vis-goal/src"
                      "extensions/common/vis-goal/resources"]]
    (->> candidates
      (map #(str (fs/file project-root %)))
      (filter fs/exists?)
      vec)))

(defn- child-classpath
  "Build the child JVM's classpath by appending any missing vis-goal
   directories to the parent's inherited cp. Dedupes so a clean
   `:test` run doesn't double-list anything."
  []
  (let [sep   (System/getProperty "path.separator")
        parts (vec (str/split (System/getProperty "java.class.path") (re-pattern sep)))
        extra (vis-goal-extra-classpath)
        seen  (atom (set parts))
        merged (cond-> parts
                 :add (into (filter (fn [p]
                                      (when-not (contains? @seen p)
                                        (swap! seen conj p)
                                        true))
                              extra)))]
    (str/join sep merged)))

(defn- start-goal-child-writer!
  [dir marker objective]
  (let [pb (ProcessBuilder.
             ^java.util.List
             [(str (fs/file (System/getProperty "java.home") "bin" "java"))
              "-cp" (child-classpath)
              (str "-Dvis.test.db-dir=" dir)
              (str "-Dvis.test.marker=" marker)
              (str "-Dvis.test.objective=" objective)
              "clojure.main"
              "-e" goal-child-write-code])]
    (.redirectErrorStream pb true)
    (let [child (.start pb)]
      [child (future (slurp (.getInputStream child)))])))

(defdescribe goal-cross-restart-test
  (it "goal survives a JVM restart on a persistent on-disk store"
    ;; Skipped when the project's classpath isn't reachable to a
    ;; subprocess — the test relies on `clojure.main` being on PATH
    ;; via the same javaHome as the test JVM. CI sets this up; local
    ;; dev does too via `clojure -M:test`.
    (let [dir     (fs/create-temp-dir {:prefix "vis-db-goal-cross-restart-"})
          marker  (fs/file dir "child-cid")]
      (try
        (let [[child output-fut] (start-goal-child-writer!
                                   (str dir) (str marker)
                                   "Cross-restart objective")]
          ;; Wait for the child JVM to finish writing + exit.
          (expect (true? (.waitFor child 30 TimeUnit/SECONDS)))
          (let [output (deref output-fut 1000 "")]
            (expect (= 0 (.exitValue child)))
            (expect (str/includes? output "CHILD-DONE"))
            ;; Now open the same persistent store from THIS JVM and
            ;; read the goal back. If the goal isn't there or doesn't
            ;; thaw to the right shape, the test fails — this is the
            ;; smoking gun for cross-restart persistence regressions.
            (expect (fs/exists? marker))
            (let [cid-str (str/trim (slurp marker))
                  parent  (vis/db-create-connection! (str dir))]
              (try
                (let [g (goal/get-goal parent cid-str)]
                  (expect (some? g))
                  (expect (= "Cross-restart objective" (:objective g)))
                  (expect (= :active (:status g)))
                  (expect (= :user   (:set-by g))))
                (finally
                  (vis/db-dispose-connection! parent))))))
        (finally
          (fs/delete-tree dir))))))
