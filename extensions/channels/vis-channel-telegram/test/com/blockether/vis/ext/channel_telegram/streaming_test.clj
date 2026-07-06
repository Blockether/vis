(ns com.blockether.vis.ext.channel-telegram.streaming-test
  "Pure-function tests for the live-bubble streaming machinery in bot.clj.

   No Telegram HTTP. Just reaches into the private helpers via
   `ns-resolve` and asserts behaviour around throttling, HTML
   composition, and bubble-state transitions."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-telegram.bot]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private bot-ns (find-ns 'com.blockether.vis.ext.channel-telegram.bot))

(defn- p
  [sym]
  (or (some-> (ns-resolve bot-ns sym)
              deref)
      (throw (ex-info (str "private symbol not found: " sym) {:symbol sym}))))

;; ---------------------------------------------------------------------------
;; thinking-html
;; ---------------------------------------------------------------------------

(defdescribe thinking-html-test
             (it "escapes HTML specials in reasoning text"
                 (let [thinking-html (p 'thinking-html)]
                   (expect (= "<blockquote expandable>a &lt;b&gt; &amp; c</blockquote>"
                              (thinking-html "a <b> & c")))))
             (it "windows long content with ellipsis prefix"
                 (let [thinking-html
                       (p 'thinking-html)

                       long-text
                       (apply str (repeat 5000 "x"))

                       out
                       (thinking-html long-text)]

                   (expect (str/starts-with? out "<blockquote expandable>…"))
                   (expect (str/ends-with? out "</blockquote>"))
                   (expect (<= (count out) 3700))))
             (it "returns nil for nil/blank input"
                 (let [thinking-html (p 'thinking-html)]
                   (expect (nil? (thinking-html nil)))
                   (expect (nil? (thinking-html "")))
                   (expect (nil? (thinking-html "   "))))))

;; ---------------------------------------------------------------------------
;; live-bubble-html composition
;; ---------------------------------------------------------------------------

(defdescribe live-bubble-html-test
             (it "empty state shows only lit lamp"
                 (let [live-bubble-html (p 'live-bubble-html)]
                   (expect (= "💭 <b>Thinking…</b>" (live-bubble-html {})))))
             (it "thinking-only state adds the blockquote"
                 (let [live-bubble-html
                       (p 'live-bubble-html)

                       out
                       (live-bubble-html {:thinking-acc "reasoning…"})]

                   (expect (str/includes? out "💭 <b>Thinking…</b>"))
                   (expect (str/includes? out "<blockquote expandable>reasoning…</blockquote>"))))
             (it "status line renders as italic"
                 (let [live-bubble-html
                       (p 'live-bubble-html)

                       out
                       (live-bubble-html {:thinking-acc "x" :status-line "⏳ Form 1"})]

                   (expect (str/includes? out "<i>⏳ Form 1</i>"))
                   (expect (str/includes? out "<blockquote expandable>x</blockquote>"))))
             (it "renders the running step feed (⏳ running, ✅ done)"
                 (let [live-bubble-html
                       (p 'live-bubble-html)

                       out
                       (live-bubble-html
                         {:steps [{:label "(cat \"a.clj\")" :code "(cat \"a.clj\")" :status :ok}
                                  {:label "(rg)" :code "(rg)" :status :running}]})]

                   (expect (str/includes? out "✅ <i>ok</i>"))
                   (expect (str/includes? out "⏳ <i>running</i>"))
                   (expect (str/includes? out "<pre>(cat \"a.clj\")</pre>"))
                   (expect (str/includes? out "<pre>(rg)</pre>"))))
             (it "escapes HTML specials in step labels"
                 (let [live-bubble-html
                       (p 'live-bubble-html)

                       out
                       (live-bubble-html {:steps [{:label "(< a b)" :status :running}]})]

                   (expect (str/includes? out "⏳ <i>running</i> (&lt; a b)")))))

;; ---------------------------------------------------------------------------
;; should-edit? throttle
;; ---------------------------------------------------------------------------

(defdescribe
  throttle-test
  (it "flush bypasses every check"
      (let [should-edit? (p 'should-edit?)]
        (expect (true? (should-edit?
                         {:last-edit-ms (System/currentTimeMillis) :backoff-ms 0 :pending-html "x"}
                         "x"
                         true)))))
  (it "identical text never triggers an edit"
      (let [should-edit? (p 'should-edit?)]
        (expect (false?
                  (should-edit? {:last-edit-ms 0 :backoff-ms 0 :pending-html "x"} "x" false)))))
  (it "within throttle window: small delta skipped"
      (let [should-edit?
            (p 'should-edit?)

            now
            (System/currentTimeMillis)]

        (expect (false? (should-edit? {:last-edit-ms now :backoff-ms 0 :pending-html "abc"}
                                      "abcd"
                                      false)))))
  (it "within throttle window: ≥40-char delta triggers edit"
      (let [should-edit?
            (p 'should-edit?)

            now
            (System/currentTimeMillis)]

        (expect (true? (should-edit? {:last-edit-ms now :backoff-ms 0 :pending-html "abc"}
                                     (str "abc" (apply str (repeat 60 "x")))
                                     false)))))
  (it "within throttle window: trailing newline triggers edit"
      (let [should-edit?
            (p 'should-edit?)

            now
            (System/currentTimeMillis)]

        (expect (true? (should-edit? {:last-edit-ms now :backoff-ms 0 :pending-html "abc"}
                                     "abcd\n"
                                     false)))))
  (it "after throttle window: edits on any change"
      (let [should-edit?
            (p 'should-edit?)

            old
            (- (System/currentTimeMillis) 5000)]

        (expect (true? (should-edit? {:last-edit-ms old :backoff-ms 0 :pending-html "abc"}
                                     "abcd"
                                     false)))))
  (it "honors backoff (locks past default throttle)"
      (let [should-edit?
            (p 'should-edit?)

            now
            (System/currentTimeMillis)]

        (expect (false? (should-edit?
                          {:last-edit-ms (- now 1500) :backoff-ms 5000 :pending-html "abc"}
                          "abcd"
                          false))))))

;; ---------------------------------------------------------------------------
;; cancel-keyboard
;; ---------------------------------------------------------------------------

(defdescribe cancel-keyboard-test
             (it "produces correct callback_data and label"
                 (let [cancel-keyboard
                       (p 'cancel-keyboard)

                       kb
                       (cancel-keyboard 12345)]

                   (expect (= [[{:text "⊘ Cancel" :callback_data "cancel:12345"}]]
                              (:inline_keyboard kb))))))
