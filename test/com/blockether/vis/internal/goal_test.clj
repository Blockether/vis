(ns com.blockether.vis.internal.goal-test
  (:require
   [com.blockether.vis.internal.goal :as goal]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe expect it]]))

;; =============================================================================
;; Validators
;; =============================================================================

(defdescribe valid-objective-test
  (it "accepts non-blank strings up to MAX_OBJECTIVE_CHARS"
    (expect (true?  (goal/valid-objective? "do the thing")))
    (expect (true?  (goal/valid-objective? "x")))
    (expect (true?  (goal/valid-objective? (apply str (repeat goal/MAX_OBJECTIVE_CHARS \X))))))

  (it "rejects blank, nil, non-strings, and overflow"
    (expect (false? (goal/valid-objective? "")))
    (expect (false? (goal/valid-objective? "   ")))
    (expect (false? (goal/valid-objective? nil)))
    (expect (false? (goal/valid-objective? 42)))
    (expect (false? (goal/valid-objective?
                      (apply str (repeat (inc goal/MAX_OBJECTIVE_CHARS) \X)))))))

(defdescribe valid-status-and-friends-test
  (it "status closed set is exactly #{:active :paused :done}"
    (expect (true?  (goal/valid-status? :active)))
    (expect (true?  (goal/valid-status? :paused)))
    (expect (true?  (goal/valid-status? :done)))
    (expect (false? (goal/valid-status? :pursuing)))
    (expect (false? (goal/valid-status? nil))))

  (it "done-reason set covers Codex parity + :cleared"
    (expect (true?  (goal/valid-done-reason? :achieved)))
    (expect (true?  (goal/valid-done-reason? :unmet)))
    (expect (true?  (goal/valid-done-reason? :budget-limited)))
    (expect (true?  (goal/valid-done-reason? :cleared)))
    (expect (false? (goal/valid-done-reason? :paused)))
    (expect (false? (goal/valid-done-reason? nil))))

  (it "set-by closed set is #{:user :model}"
    (expect (true?  (goal/valid-set-by? :user)))
    (expect (true?  (goal/valid-set-by? :model)))
    (expect (false? (goal/valid-set-by? :rogue)))
    (expect (false? (goal/valid-set-by? nil)))))

;; =============================================================================
;; State machine
;; =============================================================================

(defdescribe legal-action-test
  (it "no-goal -> :set is the only legal action"
    (expect (true?  (goal/legal-action? nil :set)))
    (expect (false? (goal/legal-action? nil :pause)))
    (expect (false? (goal/legal-action? nil :resume)))
    (expect (false? (goal/legal-action? nil :clear)))
    (expect (false? (goal/legal-action? nil :mark-done))))

  (it "from :active legal actions are #{:set :pause :clear :mark-done}"
    (expect (true?  (goal/legal-action? :active :set)))
    (expect (true?  (goal/legal-action? :active :pause)))
    (expect (true?  (goal/legal-action? :active :clear)))
    (expect (true?  (goal/legal-action? :active :mark-done)))
    (expect (false? (goal/legal-action? :active :resume))))

  (it "from :paused legal actions are #{:set :resume :clear :mark-done}"
    (expect (true?  (goal/legal-action? :paused :set)))
    (expect (true?  (goal/legal-action? :paused :resume)))
    (expect (true?  (goal/legal-action? :paused :clear)))
    (expect (true?  (goal/legal-action? :paused :mark-done)))
    (expect (false? (goal/legal-action? :paused :pause))))

  (it "from :done only :set is legal (replace with fresh goal)"
    (expect (true?  (goal/legal-action? :done :set)))
    (expect (false? (goal/legal-action? :done :pause)))
    (expect (false? (goal/legal-action? :done :resume)))
    (expect (false? (goal/legal-action? :done :clear)))
    (expect (false? (goal/legal-action? :done :mark-done))))

  (it "transition table covers every status × action with no gaps"
    ;; Defensive snapshot — catches a future change to the action set
    ;; that forgets to update some of the statuses.
    (let [actions   #{:set :pause :resume :clear :mark-done}
          all-keys  (set (keys goal/TRANSITIONS))]
      (expect (= #{nil :active :paused :done} all-keys))
      (doseq [s all-keys]
        (let [legal (get goal/TRANSITIONS s)]
          (expect (every? #(contains? actions %) legal)))))))

;; =============================================================================
;; Paused-aware elapsed math
;; =============================================================================

(defdescribe effective-elapsed-ms-test
  (it "active goal ticking from started-at"
    (let [g {:status :active :started-at-ms 1000 :total-paused-ms 0}]
      (expect (= 0    (goal/effective-elapsed-ms g 1000)))
      (expect (= 5000 (goal/effective-elapsed-ms g 6000)))))

  (it "paused goal subtracts the LIVE pause window"
    (let [g {:status :paused :started-at-ms 0 :paused-at-ms 1000 :total-paused-ms 0}]
      ;; t=2000: total - paused-window = 2000 - 1000 = 1000
      (expect (= 1000 (goal/effective-elapsed-ms g 2000)))
      ;; t=10000: 10000 - 9000 = 1000 still (paused = frozen)
      (expect (= 1000 (goal/effective-elapsed-ms g 10000)))))

  (it "active-after-resume subtracts accumulated total-paused-ms"
    (let [g {:status :active :started-at-ms 0 :paused-at-ms nil
             :total-paused-ms 4000}]
      ;; 10000 - 4000 = 6000
      (expect (= 6000 (goal/effective-elapsed-ms g 10000)))))

  (it "missing started-at returns 0 (no NPE)"
    (expect (= 0 (goal/effective-elapsed-ms {} 1000)))
    (expect (= 0 (goal/effective-elapsed-ms nil 1000)))
    (expect (= 0 (goal/effective-elapsed-ms {:started-at-ms 100} nil))))

  (it "clock-walked-back returns 0 (never negative)"
    (let [g {:status :active :started-at-ms 5000 :total-paused-ms 0}]
      (expect (= 0 (goal/effective-elapsed-ms g 4000)))))

  (it "negative net (bigger paused than elapsed) clamps to 0"
    (let [g {:status :active :started-at-ms 1000
             :total-paused-ms 999999}]
      (expect (= 0 (goal/effective-elapsed-ms g 5000))))))

(defdescribe format-elapsed-test
  (it "renders units in the expected vocabulary"
    (expect (= "0s"      (goal/format-elapsed 0)))
    (expect (= "0s"      (goal/format-elapsed 999)))
    (expect (= "1s"      (goal/format-elapsed 1000)))
    (expect (= "59s"     (goal/format-elapsed 59000)))
    (expect (= "1m 00s"  (goal/format-elapsed 60000)))
    (expect (= "12m 03s" (goal/format-elapsed (+ (* 12 60000) 3000))))
    (expect (= "59m 59s" (goal/format-elapsed (+ (* 59 60000) 59000))))
    (expect (= "1h 00m"  (goal/format-elapsed (* 60 60000))))
    (expect (= "1h 04m"  (goal/format-elapsed (+ (* 60 60000) (* 4 60000))))))

  (it "tolerates nil, negative, non-numbers"
    (expect (= "0s" (goal/format-elapsed nil)))
    (expect (= "0s" (goal/format-elapsed -5000)))))

;; =============================================================================
;; Renderers
;; =============================================================================

(defdescribe format-goal-summary-test
  (it "returns nil when no goal"
    (expect (nil? (goal/format-goal-summary nil 0)))
    (expect (nil? (goal/format-goal-summary {} 0))))

  (it "active goal includes status + elapsed + objective"
    (let [g {:status :active :started-at-ms 0 :total-paused-ms 0
             :objective "Finish migration" :set-by :user}
          s (goal/format-goal-summary g 60000)]
      (expect (str/includes? s "active"))
      (expect (str/includes? s "1m 00s"))
      (expect (str/includes? s "Finish migration"))))

  (it "paused goal shows paused tag and frozen elapsed"
    (let [g {:status :paused :started-at-ms 0 :paused-at-ms 30000
             :total-paused-ms 0 :objective "x" :set-by :user}]
      (expect (str/includes? (goal/format-goal-summary g 999999) "paused"))))

  (it "done(:achieved) includes reason in parentheses + objective"
    (let [g {:status :done :done-reason :achieved
             :started-at-ms 0 :total-paused-ms 0
             :elapsed-ms 60000 :objective "ship it" :set-by :user}
          s (goal/format-goal-summary g 60000)]
      (expect (str/includes? s "done (achieved)"))
      (expect (str/includes? s "ship it"))))

  (it "done(:cleared) hides the (now-blank) objective trail"
    ;; cleared blanks the objective in the DB so the row carries
    ;; nil/blank objective; the rendered summary should not show
    ;; an empty trailing \"  · \".
    (let [g {:status :done :done-reason :cleared
             :started-at-ms 0 :total-paused-ms 0
             :elapsed-ms 1000 :objective nil :set-by :user}
          s (goal/format-goal-summary g 1000)]
      (expect (str/includes? s "done (cleared)"))
      (expect (false? (str/ends-with? s " · "))))))

(defdescribe goal-system-prompt-block-test
  (it "returns nil when no goal or goal is :done"
    (expect (nil? (goal/goal-system-prompt-block nil 0)))
    (expect (nil? (goal/goal-system-prompt-block {} 0)))
    (expect (nil? (goal/goal-system-prompt-block
                    {:status :done :done-reason :achieved
                     :objective "x" :set-by :user :started-at-ms 0
                     :total-paused-ms 0}
                    1000))))

  (it "active goal renders an XML-tagged block with elapsed attr"
    (let [g {:status :active :started-at-ms 0 :total-paused-ms 0
             :objective "Finish migration" :set-by :user}
          s (goal/goal-system-prompt-block g 60000)]
      (expect (str/starts-with? s "<conversation_goal "))
      (expect (str/ends-with?   s "</conversation_goal>"))
      (expect (str/includes? s "status=\"active\""))
      (expect (str/includes? s "set-by=\"user\""))
      (expect (str/includes? s "elapsed=\"1m 00s\""))
      (expect (str/includes? s "Finish migration"))))

  (it "paused goal still renders the block (live context for next turn)"
    (let [g {:status :paused :started-at-ms 0 :paused-at-ms 1000
             :total-paused-ms 0 :objective "x" :set-by :user}
          s (goal/goal-system-prompt-block g 2000)]
      (expect (str/includes? s "status=\"paused\""))))

  (it "blank objective yields no block (defensive)"
    (let [g {:status :active :started-at-ms 0 :total-paused-ms 0
             :objective "   " :set-by :user}]
      (expect (nil? (goal/goal-system-prompt-block g 0))))))

;; =============================================================================
;; Truncation
;; =============================================================================

(defdescribe truncate-objective-test
  (it "passes through short strings unchanged (modulo whitespace flatten)"
    (expect (= "hello"     (goal/truncate-objective "hello")))
    (expect (= "a b c"     (goal/truncate-objective "a  b\nc"))))

  (it "ellipsis-truncates long strings to the configured cap"
    (let [s   (apply str (repeat 100 \X))
          out (goal/truncate-objective s 10)]
      (expect (= 10 (count out)))
      (expect (str/ends-with? out "…"))))

  (it "tolerates nil and non-strings"
    (expect (nil? (goal/truncate-objective nil)))
    (expect (nil? (goal/truncate-objective 42)))))
