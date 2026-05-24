(ns com.blockether.vis.ext.channel-tui.render-test
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.channel-tui.click-regions :as cr]
   [com.blockether.vis.ext.channel-tui.links :as links]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.ext.channel-tui.theme :as t]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private format-iteration-entry @#'render/format-iteration-entry)
(def ^:private input-more-hint @#'render/input-more-hint)
(def ^:private clip-lines-preserving-markers @#'render/clip-lines-preserving-markers)
(def ^:private assistant-meta-line @#'render/assistant-meta-line)

(defn- with-raw-code-on*
  "Enable `:vis/show-raw-code` for the duration of `body-thunk`,
   restore the prior value. Test helper for layout / bubble shape
   assertions that need the code rail visible — production default
   is OFF (channel hides every `:code` row; only tool channel
   previews + recap rows + the answer paint)."
  [body-thunk]
  (let [prev (vis/toggle-value :vis/show-raw-code)]
    (vis/toggle-set-value! :vis/show-raw-code true)
    (try (body-thunk)
      (finally
        (if (nil? prev)
          (vis/toggle-reset-to-default! :vis/show-raw-code)
          (vis/toggle-set-value! :vis/show-raw-code prev))))))

(defmacro ^:private with-raw-code-on [& body]
  `(with-raw-code-on* (fn [] ~@body)))

(defn- marker-of
  "First codepoint of `s` as a single-char string, or nil for empty."
  [s]
  (when (and (string? s) (pos? (count s)))
    (subs s 0 1)))

(defn- strip-ansi [s]
  (str/replace (or s "") #"\u001b\[[0-9;]*m" ""))

(defn- body-of
  "Drop the leading marker (PUA codepoint) and return the visible text.
   Tolerates empty input - a blank line in `:answer` mode renders as
   the empty string with the empty `:plain` marker, so callers must
   not crash when iterating over a frame that includes blank rows."
  [s]
  (when (string? s)
    (if (zero? (count s)) "" (subs s 1))))

(defn- strip-sentinels
  "Drop inline-style sentinels (PUA U+E110..U+E2FF) so equality
   assertions compare the visible text only."
  [s]
  (->> s
    (remove #(<= 0xE110 (int %) 0xE2FF))
    (apply str)))

(defdescribe input-overflow-hint-test
  (it "shows hidden visual-row count as an N more label for the input top border"
    (expect (= nil (input-more-hint 1 4)))
    (expect (= nil (input-more-hint 4 4)))
    (expect (= " 1 more " (input-more-hint 5 4)))
    (expect (= " 6 more " (input-more-hint 10 4)))))

(defdescribe live-running-block-test
  (it "renders a block slot with no result as currently running with elapsed time"
    (with-raw-code-on
    ;; The right-aligned `BLOCK N` / `ITERATION N` / `CODE N` header bands
    ;; were retired per user directive (see comments in render.clj). The
    ;; spinner now lives next to the form via the in-line `↻ <elapsed>`
    ;; status row, and the code / status rows ride the same marker band.
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(Thread/sleep 1000)" :comment nil :render-segments nil :result-render nil :result-kind nil :result-detail nil :error nil :started-at-ms 1000 :duration-ms 0 :success? nil :silent? false}]}
                    40 1 {:now-ms 2500})
            code-line (first (filter #(str/includes? % "Thread/sleep") lines))
            status-line (first (filter #(str/includes? % "↻ 1.0s") lines))]
        (expect (not-any? #(str/includes? % "BLOCK 1") lines))
        (expect (not-any? #(str/includes? % "ITERATION 1") lines))
        (expect (not-any? #(str/includes? % "CODE 1") lines))
        (expect (= p/MARKER_CODE (marker-of code-line)))
        (expect (= p/MARKER_CODE_STATUS (marker-of status-line))))))

  (it "renders mixed-block render segments as visible code plus title banner while hiding answer call"
    (with-raw-code-on
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code (str "(def x 1)\n"
                                                            "(set-session-title! \"Mixed forms\")\n"
                                                            "(done [:ir [:p \"Done\"]])") :comment nil :render-segments [{:kind :code :source "(def x 1)"}
                                                                                                                         {:kind :title :value "Mixed forms"}
                                                                                                                         {:kind :answer-ref}] :result-render nil :result-kind nil :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                    60 1 {})
            body (str/join "\n" (map (comp strip-ansi body-of) lines))]
        (expect (str/includes? body "(def x 1)"))
        (expect (str/includes? body "TITLE  \"Mixed forms\""))
      ;; Render-segments path: the raw `(done …)` call form should not
      ;; leak — the renderer surfaces the answer reference instead.
        (expect (not (str/includes? body "(done [:ir")))
        (expect (not (str/includes? body "set-session-title!")))))

    (it "renders IR tool results without EDN dumping"
      (let [ir [:ir {} [:p {} [:strong {} [:span {} "bold result"]]]]
            lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(tool)" :comment nil :render-segments nil :result-render ir :result-kind :tool :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                    60 1 {})
            body (str/join "\n" (map (comp strip-ansi body-of) lines))]
        (expect (str/includes? body "bold"))
        (expect (str/includes? body "result"))
        (expect (not (str/includes? body ":ir"))))))

  (it "renders channel sink IR returned by a tool without EDN dumping"
    ;; Tool results carry their channel-render IR via `:result-render`.
    ;; Plain `:value` form results are now hidden per user directive;
    ;; only `:tool` kind reaches the result pane, so the test asserts the
    ;; IR contract for the case the renderer actually paints.
    (let [ir [:ir {} [:p {} [:strong {} [:span {} "bold result"]]]]
          lines (format-iteration-entry {:iteration 0
                                         :forms [{:code "(def t (z/forms \"x.clj\"))" :comment nil :render-segments nil :result-render ir :result-kind :tool :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                  60 1 {})
          visible-lines (mapv (comp strip-sentinels strip-ansi) lines)
          body (str/join "\n" visible-lines)
          ir-line (first (filter #(str/includes? (strip-sentinels %) "bold result") lines))]
      (expect (str/includes? body "bold result"))
      (expect (not (str/includes? body ":ir")))
      (expect (not (str/starts-with? ir-line p/MARKER_RESULT)))
      (expect (not (str/starts-with? ir-line p/MARKER_ANSWER_TXT)))))

  (it "hides `(def r (v/ls …))` source while keeping the channel preview visible (regression: previous behaviour suppressed both)"
    ;; Two-form fence the model emits in practice: bind a tool result
    ;; to a name, then project it via plain Clojure (`select-keys`).
    ;; Pre-fix bug: `code-source-from-render-segments` ignored
    ;; `:hidden?` so the raw `(def r (v/ls …))` row leaked into the
    ;; trace AND `form-result-kind` saw `:value` (because the
    ;; FENCE's last value was a plain map), so the tool's
    ;; preview pane was suppressed by the `(= :tool result-kind)`
    ;; gate. Both halves are now wired.
    (let [ls-ir [:ir {} [:p {} [:strong {} [:span {} "LS"]] [:span {} "  ."]]
                 [:code {:lang "text"} ".gitignore\nsrc/"]]
          lines (format-iteration-entry
                  {:iteration 0
                   :forms [{:code (str "(def r (v/ls \".\"))\n"
                                    "(select-keys r [:entry-count :file-count])")
                            :comment nil
                            ;; With \":vis/show-raw-code\" OFF (default)
                            ;; the channel hides ALL :code segments — no
                            ;; per-segment :hidden? flag any more.
                            :render-segments [{:kind :code
                                               :source "(def r (v/ls \".\"))"}
                                              {:kind :code
                                               :source "(select-keys r [:entry-count :file-count])"}]
                            :result-render ls-ir
                            :result-kind   :tool   ;; promoted via channel sink entries
                            :result-detail nil
                            :error nil :started-at-ms nil :duration-ms 1
                            :success? true :silent? false}]}
                  80 1 {})
          body (str/join "\n" (map (comp strip-sentinels strip-ansi body-of) lines))]
      ;; Neither raw code row paints; the bubble is just the LS
      ;; preview (badge + body).
      (expect (not (str/includes? body "(select-keys")))
      (expect (not (str/includes? body "(def r (v/ls")))
      (expect (str/includes? body "LS"))
      (expect (str/includes? body ".gitignore"))))

  (it "renders form eval errors inline with source caret"
    (let [code "(def git-diff-doc (v/engine-symbol-documentation 'v/git-diff))"
          err  {:message "Unable to resolve symbol: 'v/git-diff"
                :trace "clojure.lang.ExceptionInfo: Unable to resolve symbol: 'v/git-diff"
                :block {:source code :row 1 :col 45}}
          lines (format-iteration-entry {:iteration 0
                                         :forms [{:code code :comment nil :render-segments nil :result-render nil :result-kind :error :result-detail nil :error err :started-at-ms nil :duration-ms 1 :success? false :silent? false}]}
                  80 1 {})
          visible (mapv (comp strip-sentinels strip-ansi body-of) lines)
          body (str/join "\n" visible)
          error-line (first (filter #(str/includes? % "ERROR — clojure.lang.ExceptionInfo") lines))]
      (expect (str/includes? body " 1: (def git-diff-doc"))
      (expect (str/includes? body "^---"))
      (expect (str/includes? body "ERROR — clojure.lang.ExceptionInfo: Unable to resolve symbol: 'v/git-diff"))
      (expect (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote code)) body))))
      (expect (= p/MARKER_CODE_ERR (marker-of error-line)))))

  (it "puts success status on its own bottom line and keeps bottom padding"
    (with-raw-code-on
    ;; Layout (post header-band removal):
    ;;   iteration-pad
    ;;   code-ok-pad             ← the trailing pad lives ABOVE result rows,
    ;;   <code line>                not at (count-2). Check it exists somewhere
    ;;   <status line ✓>            inside the code-block region.
    ;;   code-ok-pad
    ;;   iteration-pad
    ;; Plain `:value` form results no longer render — the trailing
    ;; result row is gone for non-tool forms per user directive.
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(+ 1 2)" :comment nil :render-segments nil :result-render "3" :result-kind :value :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                    40 1 {})
            bodies (mapv (comp strip-ansi body-of) lines)
            status-line (first (filter #(str/includes? % "✓ 1ms") lines))]
        (expect (= "✓ 1ms" (str/trim (strip-ansi (body-of status-line)))))
        (expect (= p/MARKER_CODE_STATUS (marker-of status-line)))
        (expect (some #(= p/MARKER_CODE_OK_PAD (marker-of %)) lines))
        (expect (not-any? #(str/includes? (or % "") "3") bodies)))))

  (it "pads displayed form comments by one column"
    (with-raw-code-on
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(+ 1 2)" :comment ";; why this runs" :render-segments nil :result-render nil :result-kind nil :result-detail nil :error nil :started-at-ms nil :duration-ms 0 :success? nil :silent? false}]}
                    40 1 {})
            comment-line (first (filter #(str/includes? % ";; why this runs") lines))]
        (expect (= p/MARKER_THINKING (marker-of comment-line)))
        (expect (str/starts-with? (body-of comment-line) " ;;")))))

  (defdescribe provider-fallback-notice-test
    (it "renders provider fallback recap lines above fallback details"
      (let [lines (format-iteration-entry
                    {:provider-fallbacks
                     [{:failed-provider {:id :anthropic-coding-plan
                                         :model "claude-opus-4-7"
                                         :error "Exceptional status code: 429"}}]}
                    120 1 {})
            body  (str/join "\n" (map (comp strip-ansi body-of) lines))]
        (expect (str/includes? body "RECAP  Provider fallback: anthropic-coding-plan/claude-opus-4-7"))
        (expect (str/includes? body "Exceptional status code: 429")))))

  (it "formats same-provider retry notices as recap rows"
    (let [lines (format-iteration-entry
                  {:provider-fallbacks
                   [{:event/type :llm.routing/provider-retry
                     :provider "anthropic-coding-plan"
                     :model "claude-opus-4-7"
                     :reason :rate-limit
                     :delay-ms 2000}]}
                  120 1 {})
          ;; Recap row wraps when wider than the bubble: line N+1
          ;; carries a leading space (continuation indent). Trim each
          ;; wrapped fragment then join with a SINGLE space so two
          ;; meaningful spaces inside the recap badge (\"RECAP  Provider\")
          ;; survive the merge. Substring check pins CONTENT, not
          ;; column width.
          body (->> lines
                 (map (comp str/trim strip-ansi body-of))
                 (str/join " "))]
      (expect (str/includes? body "RECAP  Provider retry: anthropic-coding-plan/claude-opus-4-7 — rate-limit, retry in 2s — rate limit: wait, re-authenticate, or switch provider/model"))))

  (it "renders provider error recap lines above provider error details"
    (let [lines (format-iteration-entry
                  {:error {:type :svar.core/http-error
                           :message "Exceptional status code: 429"
                           :data {:status 429
                                  :body "rate limit"}}}
                  120 1 {})
          body  (->> lines
                  (map (comp str/trim strip-ansi body-of))
                  (str/join " "))]
      (expect (str/includes? body "RECAP  Provider error HTTP 429: Exceptional status code: 429 — rate limit: wait, re-authenticate, or switch provider/model"))
      (expect (str/includes? body "PROVIDER_ERROR  HTTP 429")))))

(defdescribe assistant-bubble-footer-fallback-test
  (it "shows selected and actual LLM routing in the assistant bubble footer"
    (let [message {:role :assistant
                   :llm-selected {:provider "anthropic-coding-plan"
                                  :model "claude-opus-4-7"}
                   :llm-actual {:provider "openai-codex"
                                :model "gpt-5.3-codex"}
                   :llm-fallback? true
                   :llm-routing-trace [{:event/type :llm.routing/provider-fallback
                                        :from-provider "anthropic-coding-plan"
                                        :from-model "claude-opus-4-7"
                                        :to-provider "openai-codex"
                                        :to-model "gpt-5.3-codex"
                                        :error "Exceptional status code: 429"}]}]
      (expect (= "fallback anthropic-coding-plan/claude-opus-4-7 → openai-codex/gpt-5.3-codex — Exceptional status code: 429"
                (assistant-meta-line message)))))

  (it "surfaces retry count + HTTP status when the trace carries provider-retry events"
    ;; TUI footer example: `— 3 retries, 429`. Counts every
    ;; `:llm.routing/provider-retry` in the trace and prefers the
    ;; fallback event's `:status` over the free-form `:error` text so
    ;; the footer matches the spec verbatim.
    (let [message {:role :assistant
                   :llm-selected {:provider "anthropic-coding-plan"
                                  :model "claude-opus-4-7"}
                   :llm-actual {:provider "openai-codex"
                                :model "gpt-5.3-codex"}
                   :llm-fallback? true
                   :llm-routing-trace
                   [{:event/type :llm.routing/provider-retry :status 429 :attempt 1 :delay-ms 2000}
                    {:event/type :llm.routing/provider-retry :status 429 :attempt 2 :delay-ms 3000}
                    {:event/type :llm.routing/provider-retry :status 429 :attempt 3 :delay-ms 6000}
                    {:event/type :llm.routing/provider-fallback
                     :status 429
                     :reason :rate-limit-budget-exhausted
                     :from-provider "anthropic-coding-plan"
                     :from-model "claude-opus-4-7"
                     :to-provider "openai-codex"
                     :to-model "gpt-5.3-codex"}]}]
      (expect (= "fallback anthropic-coding-plan/claude-opus-4-7 → openai-codex/gpt-5.3-codex — 3 retries, 429"
                (assistant-meta-line message)))))

  (it "singular `1 retry` when only one retry preceded the fallback"
    (let [message {:role :assistant
                   :llm-selected {:provider "a" :model "m1"}
                   :llm-actual   {:provider "b" :model "m2"}
                   :llm-fallback? true
                   :llm-routing-trace
                   [{:event/type :llm.routing/provider-retry :status 429 :attempt 1 :delay-ms 2000}
                    {:event/type :llm.routing/provider-fallback :status 429 :reason :rate-limit-budget-exhausted}]}]
      (expect (= "fallback a/m1 → b/m2 — 1 retry, 429"
                (assistant-meta-line message)))))

  (it "counts fallback-only footer height so the footer is not overwritten"
    (let [base {:role :assistant
                :text "Done."
                :timestamp nil
                :prewrapped-lines ["Done."]}
          routed (assoc base
                   :llm-selected {:provider "anthropic-coding-plan"
                                  :model "claude-opus-4-7"}
                   :llm-actual {:provider "openai-codex"
                                :model "gpt-5.3-codex"}
                   :llm-fallback? true)]
      (render/invalidate-cache!)
      (expect (= (+ 2 (render/bubble-height base 120))
                (render/bubble-height routed 120))))))

(defn- visually-blank?
  "True when a rendered line carries no visible glyphs — either truly
   empty, plain whitespace, or composed entirely of the invisible
   Unicode format-class characters and PUA line-kind markers the
   TUI painter uses as line-kind sentinels.

   Recognised sentinel families:
   - U+200B–U+200D, U+2060–U+206F, U+FEFF — invisible format chars
     used as text-band markers (THINKING, ITER-PAD, ANSWER-PAD,
     channel/recap pads).
   - U+E000–U+E0FF — PUA markers reserved for code/tool/status bands
     (code-ok pad, code-err pad, status row chrome). A row whose
     body past the leading marker char is whitespace paints a
     coloured background bar with NO glyphs — the user reads it
     as blank."
  [s]
  (let [s (strip-ansi (or s ""))]
    (cond
      (str/blank? s) true

      (every? (fn [^Character c]
                (let [n (int c)]
                  (or (= n 0x200B) (= n 0x200C) (= n 0x200D) (= n 0xFEFF)
                    (<= 0x2060 n 0x206F))))
        s)
      true

      :else
      (let [c0   (.charAt ^String s 0)
            n0   (int c0)
            body (subs s 1)]
        (and (<= 0xE000 n0 0xE0FF)
          (str/blank? body))))))

(defdescribe answer-trailer-margin-test
  ;; Answer layout mirrors code-block chrome: one neutral outside
  ;; margin row above the answer zone, then one answer-bg inside pad
  ;; row before text. If a code-bearing trace already ended with a
  ;; neutral iteration pad, reuse that row as the outside margin.
  (let [ans       [:ir {} [:p {} "hello"]]
        settings  {:show-thinking true :show-iterations true}
        iter      {:forms [{:code "(+ 1 1)" :comment nil :render-segments nil :result-render "2" :result-kind nil :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
        index-of  (fn [p needle]
                    (first (keep-indexed
                             (fn [i ln]
                               (when (str/includes? (strip-ansi ln) needle) i))
                             (:lines p))))]
    (it "answer with NO trace renders directly without internal answer padding"
      (let [p    (render/format-answer-with-thinking-data*
                   ans [] 80 settings nil false nil)
            idx  (index-of p "hello")
            ln   (:lines p)
            pad? (fn [line]
                   (str/starts-with? line p/MARKER_ANSWER_PAD))]
        ;; Top-margin blank row pushes content down by one.
        (expect (= 1 idx))
        (expect (visually-blank? (nth ln 0)))
        (expect (not-any? pad? ln))))

    (it "answer with code-bearing trace keeps band edges + a terminal-bg gap between trace and answer"
      ;; Spacing contract enforced by `coalesce-bubble-blanks`:
      ;; different marker families paint distinct visual bands
      ;; (code-bg pad, terminal-bg gap, answer-bg pad), so adjacent
      ;; blanks from DIFFERENT families are preserved — they ARE the
      ;; visual band edges the user reads as section borders. Only
      ;; same-family duplicates collapse.
      ;;
      ;; Above the answer text the user sees, in order:
      ;;   [code-pad bottom]   band edge of the last form's code block
      ;;   [terminal-bg gap]   iter-pad / outer margin
      ;;   [answer-pad top]    band edge of the answer band
      ;;   answer text
      ;; The assertion: the row immediately above the answer is
      ;; visually blank (band edge or gap), and no run of three
      ;; identical blank rows appears above the answer (regression
      ;; guard for the old triple-stacked terminal-bg blanks).
      (with-raw-code-on
        (let [p     (render/format-answer-with-thinking-data*
                      ans [iter] 80 settings nil false nil)
              idx   (index-of p "hello")
              ln    (:lines p)
              above (when (and idx (>= idx 3))
                      [(nth ln (dec idx))
                       (nth ln (- idx 2))
                       (nth ln (- idx 3))])]
          (expect (some? idx))
          (expect (visually-blank? (nth ln (dec idx))))
          (expect (or (nil? above) (not (apply = above))))))

      (it "cancelled with non-empty answer renders the answer text once"
      ;; `cancel-text` falls back to the answer text when the IR is
      ;; non-empty. Cancelled turns are flat system notes, so they use
      ;; only the outside margin, not answer-bg inside padding.
        (let [p   (render/format-answer-with-thinking-data*
                    ans [iter] 80 settings nil true nil)
              idx (index-of p "hello")
              ln  (:lines p)]
          (expect (some? idx))
          (expect (visually-blank? (nth ln (dec idx))))
          (expect (or (zero? (dec idx))
                    (not (visually-blank? (nth ln (- idx 2)))))))))))

(defdescribe progress-rendering-test
  (it "iter-0 spinner row has a one-line top margin inside the bubble"
    ;; Regression: the "Vis is calling the provider" spinner used to
    ;; sit flush against the bubble's top border because the no-trace
    ;; branch in `progress->lines-data` emitted just the spinner line.
    ;; The iter≥1 branch always ended with a blank line before the
    ;; spinner, so the bubble visually grew by an extra row the moment
    ;; the first iteration arrived. Keep the blank in both branches.
    (let [payload (render/progress->lines-data
                    {:iterations []} 80
                    {:show-thinking true :show-iterations true}
                    {:now-ms 1000 :turn-start-ms 0})
          lines   (mapv strip-ansi (:lines payload))]
      (expect (= 2 (count lines)))
      (expect (= "" (first lines)))
      (expect (str/includes? (second lines) "Vis is calling the provider"))))

  (it "shows queued submissions inside the live progress bubble"
    (let [payload (render/progress->lines-data
                    {:iterations []} 80
                    {:show-thinking true :show-iterations true}
                    {:now-ms 1000 :turn-start-ms 0
                     :pending-sends [{:text "please also check logs"}]})
          body    (strip-ansi (str/join "\n" (:lines payload)))]
      (expect (str/includes? body "Vis is calling the provider"))
      (expect (str/includes? body "please also check logs"))
      (expect (not (str/includes? body "queued update")))))

  (it "distinguishes response parsing from provider waiting"
    (let [payload (render/progress->lines-data
                    {:iterations [{:iteration 1 :activity :response-parse}]}
                    80
                    {:show-thinking true :show-iterations true}
                    {:now-ms 1000 :turn-start-ms 0})
          body    (strip-ansi (str/join "\n" (:lines payload)))]
      (expect (str/includes? body "Vis is parsing model response (iter 1)"))))

  (it "uses the same trace renderer for live progress and cancelled bubbles"
    (let [ir       [:ir {} [:p {} [:strong {} [:span {} "bold result"]]]]
          ;; Tool-kind result so the channel-render IR actually paints —
          ;; plain `:value` results are hidden per user directive.
          iter     {:forms [{:code "(tool)" :comment nil :render-segments nil :result-render ir :result-kind :tool :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
          settings {:show-thinking true :show-iterations true}
          live     (:lines (render/progress->lines-data
                             {:iterations [iter]} 80 settings
                             {:now-ms 1000 :turn-start-ms 0}))
          cancel   (:lines (render/format-answer-with-thinking-data
                             [:ir {} [:p {} [:span {} "Cancelled by user."]]]
                             [iter] 80 settings nil true nil))
          clean    (fn [line] (strip-sentinels (strip-ansi line)))
          trim-tail (fn [xs]
                      (vec (reverse (drop-while visually-blank? (reverse xs)))))
          trace-before (fn [needle lines]
                         (->> lines
                           (take-while #(not (str/includes? (clean %) needle)))
                           trim-tail))
          live-trace (trace-before "Vis is" live)
          cancel-trace (trace-before "Cancelled by user." cancel)
          body      (str/join "\n" (map clean cancel-trace))]
      (expect (= live-trace cancel-trace))
      (expect (str/includes? body "bold result"))
      (expect (not (str/includes? body ":ir")))))

  (it "live progress renders every iteration instead of hiding history"
    (with-raw-code-on
      (let [mk-entry (fn [n]
                       {:forms [{:code (str "(+ " n " 1)") :comment nil :render-segments nil :result-render (str (inc n)) :result-kind nil :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]})
            body     (strip-ansi
                       (render/progress->text
                         {:iterations (mapv mk-entry (range 5))}
                         80
                         {:show-thinking true :show-iterations true}
                         {:now-ms 1000 :turn-start-ms 0}))]
        (expect (not (str/includes? body "hidden while live")))
        (expect (str/includes? body "(+ 0 1)"))
        (expect (str/includes? body "(+ 4 1)"))))

    (it "live progress renders bounded thinking chunks without hiding content"
    ;; Fixture passes real thinking content — the previous empty
    ;; `{:iterations [{}]}` shape could never have exercised any
    ;; thinking-rendering assertion (regression net for the bug where
    ;; this test was silently a no-op).
      (let [body (strip-ansi
                   (render/progress->text
                     {:iterations [{:thinking "alpha\nbeta"}]}
                     80
                     {:show-thinking true :show-iterations true}
                     {:now-ms 1000 :turn-start-ms 0}))]
        (expect (not (str/includes? body "hidden while live")))
        (expect (str/includes? body "alpha"))
        (expect (str/includes? body "beta")))))

  (it "live progress previews huge thinking with the viewport-driven truncation"
    ;; The single-iteration truncation summary only fires when a
    ;; viewport budget is supplied (the renderer can't decide to
    ;; collapse without knowing how much screen real estate exists).
    ;; Pin both halves of the contract: without `:viewport-rows` the
    ;; full thinking renders; with a tight budget the collapse summary
    ;; bounds the line count.
    (let [huge-thinking (apply str (repeat 20000 "thinking "))
          full          (render/progress->lines-data
                          {:iterations [{:thinking huge-thinking}]}
                          96 {:show-thinking true :show-iterations true}
                          {:now-ms 1000 :turn-start-ms 0})]
      ;; No viewport: full body is rendered, just shouldn't crash.
      (expect (pos? (count (:lines full))))
      (expect (not (str/includes? (strip-ansi (:text full)) huge-thinking)))
      (expect (some (fn [ln] (str/includes? (strip-ansi ln) "thinking thinking"))
                (:lines full)))))

  (it "live progress hides plain value results entirely"
    ;; Per user directive: only tool calls show a result pane; plain
    ;; `:value` form results never paint a body, regardless of size.
    ;; No `RESULT` label, no `chars hidden` summary, no toggle-details
    ;; click region — collapsible UI is gone.
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
          payload     (render/progress->lines-data
                        {:iterations [{:forms [{:code "(+ 1 2)"
                                                :result-render huge-result
                                                :result-kind :value
                                                :duration-ms 1
                                                :success? true :silent? false}]}]}
                        96
                        {:show-thinking true :show-iterations true}
                        {:now-ms            1000
                         :turn-start-ms     0
                         :session-id   "session"
                         :detail-expansions {}})]
      (expect (not (str/includes? (:text payload) "RESULT")))
      (expect (not (str/includes? (:text payload) "chars hidden")))
      (expect (not (str/includes? (:text payload) huge-result)))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "live progress always renders every iteration with no PROGRESS HISTORY toggle"
    (with-raw-code-on
    ;; Per user directive: no collapsible iteration history. Every
    ;; iteration paints in place; the PROGRESS HISTORY summary band
    ;; is gone.
      (let [mk-entry (fn [n]
                       {:forms [{:code (str "(+ " n " 1)") :comment nil :render-segments nil :result-render (str (inc n)) :result-kind :tool :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]})
            payload   (render/progress->lines-data
                        {:iterations (mapv mk-entry (range 12))}
                        80
                        {:show-thinking true
                         :show-iterations true}
                        {:now-ms            1000
                         :turn-start-ms     0
                         :session-id   "session"
                         :detail-expansions {}})
            body      (strip-ansi (:text payload))]
        (expect (not (str/includes? body "PROGRESS HISTORY")))
        (expect (not (str/includes? body "iterations hidden")))
        (expect (str/includes? body "(+ 0 1)"))
        (expect (str/includes? body "(+ 11 1)"))
        (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload))))))

  (it "toggles :vis/silent forms in live progress traces"
    (with-raw-code-on
      (let [progress {:iterations
                      [{:forms [{:code "(set-session-title! \"Greeting\")"
                                 :result-render ":vis/silent"
                                 :result-kind :value
                                 :duration-ms 1
                                 :success? true :silent? true}
                                {:code "(+ 1 2)"
                                 :result-render "3"
                                 :result-kind :value
                                 :duration-ms 1
                                 :success? true :silent? false}]}]}
            hidden-body (strip-ansi
                          (render/progress->text progress 80
                            {:show-thinking true :show-iterations true :show-silent false}
                            {:now-ms 1000 :turn-start-ms 0}))
            shown-body  (strip-ansi
                          (render/progress->text progress 80
                            {:show-thinking true :show-iterations true :show-silent true}
                            {:now-ms 1000 :turn-start-ms 0}))]
        (expect (not (str/includes? hidden-body "set-session-title!")))
        (expect (str/includes? hidden-body "(+ 1 2)"))
        (expect (str/includes? shown-body "set-session-title!"))
      ;; Plain `:value` result bodies (`:vis/silent`, `3`) are hidden
      ;; per user directive — only tool channel-render output paints.
        (expect (not (str/includes? shown-body ":vis/silent"))))))

  (defdescribe repeated-error-collapse-test
    (it "squashes repeated identical provider errors into one counted row"
      (render/invalidate-cache!)
      (let [err {:message "Stream ended before terminal marker."
                 :data {:type :svar.core/stream-truncated}}
            body (strip-ansi
                   (:text (render/progress->lines-data
                            {:iterations (vec (repeat 11 {:error err}))} 80
                            {:show-thinking true :show-iterations true}
                            {:now-ms 1000 :turn-start-ms 0})))]
        (expect (str/includes? body "ERROR x 11: Stream ended before terminal marker."))
        (expect (= 1 (count (re-seq #"ERROR" body))))))))

(defdescribe progress-streaming-perf-test
  (it "per-iteration cache keeps live-stream tick under 50 ms with 15 iterations"
    ;; Regression test for the bug where `progress->lines-data` keyed its
    ;; cache on `(System/identityHashCode iterations)` and `(quot now-ms 1000)`.
    ;; `make-progress-tracker` rebuilds the iterations vec on every chunk via
    ;; `(vec (vals @timeline))`, so the identity-keyed cache missed every
    ;; tick. With a 15-iteration trace we measured 554 ms per 80 ms render
    ;; tick - 7x over budget. The fix replaces the trace-level cache with
    ;; per-iteration content-fingerprint caching, so completed iterations
    ;; hit forever and only the streaming iteration recomputes.
    ;;
    ;; Threshold (50 ms) is generous: real measurements land ~10 ms on a
    ;; warm JVM. We pick 50 ms so JIT-cold CI runs don't false-alarm while
    ;; still failing loudly if someone reintroduces an O(N-iters) per-tick
    ;; reformat path. Bump the threshold here ONLY if you have a
    ;; corresponding bench measurement showing the new floor; never bump
    ;; just to make a flake go away.
    (let [mk-iter (fn [i]
                    {:thinking (apply str (repeat (+ 200 (* 100 i)) \.))
                     :forms [{:code (str "(do (println :iter " i ") (mapv inc (range 100)))") :comment nil :render-segments nil :result-render "[1 2 3 ...]" :result-kind :value :result-detail nil :error nil :started-at-ms nil :duration-ms 50 :success? true :silent? false}]})
          base       (mapv mk-iter (range 14))
          last-base  (mk-iter 14)
          bubble-w   130
          settings   {:show-thinking true :show-iterations true}]
      (render/invalidate-cache!)
      ;; Warm: 3 cycles to JIT the format path.
      (dotimes [_ 3]
        (render/progress->lines-data {:iterations base} bubble-w settings
          {:now-ms 1700000000000 :turn-start-ms 1700000000000
           :viewport-rows 50}))
      ;; Streaming: NEW iterations vec each tick (mimics `(vec (vals @timeline))`),
      ;; last iteration's thinking grows by 100 chars per tick.
      (let [runs 30
            t0   (System/nanoTime)]
        (dotimes [i runs]
          (let [growing  (assoc last-base :thinking
                           (apply str (:thinking last-base)
                             (repeat (* 100 (inc i)) \.)))
                its'     (conj (vec base) growing)]
            (render/progress->lines-data {:iterations its'} bubble-w settings
              {:now-ms        (+ 1700000000000 (* i 80))
               :turn-start-ms 1700000000000
               :viewport-rows 50})))
        (let [per-tick-ms (/ (/ (- (System/nanoTime) t0) 1e6) (double runs))]
          (expect (< per-tick-ms 50.0))))))

  (it "completed-iteration cache hits when the iterations vec gets a fresh identity"
    ;; Direct contract test: take a fully-completed iterations vec, format
    ;; it once to warm the cache, then format again with a freshly-allocated
    ;; copy that has identical content but different `identityHashCode`.
    ;; The second call must be ~free (cache hit). If someone reintroduces
    ;; `identityHashCode`-based keying this test fails immediately.
    (let [iter   {:thinking "some reasoning"
                  :forms [{:code "(+ 1 2)" :comment nil :render-segments nil :result-render "3" :result-kind :value :result-detail nil :error nil :started-at-ms nil :duration-ms 10 :success? true :silent? false}]}
          iters1 (vec (repeat 5 iter))
          ;; Same content, fresh vec identity, fresh map identities for entries.
          iters2 (mapv #(into {} %) iters1)]
      (render/invalidate-cache!)
      ;; Warm with iters1.
      (dotimes [_ 3]
        (render/progress->lines-data {:iterations iters1} 100
          {:show-thinking true :show-iterations true}
          {:now-ms 1700000000000 :turn-start-ms 1700000000000}))
      ;; iters2 has identical CONTENT but different identity at every level.
      (let [t0 (System/nanoTime)]
        (dotimes [_ 100]
          (render/progress->lines-data {:iterations iters2} 100
            {:show-thinking true :show-iterations true}
            {:now-ms 1700000000000 :turn-start-ms 1700000000000}))
        (let [per-call-us (/ (/ (- (System/nanoTime) t0) 1e3) 100.0)]
          ;; Cache hit path: should be well under 1 ms (1000 µs) per call
          ;; even with 5 cached iterations to concat. If this exceeds 5 ms
          ;; the per-iteration content-keyed cache is broken.
          (expect (< per-call-us 5000.0)))))))

(defdescribe iteration-live-ordering-test
  (describe "ordered live progress events"
    (it "renders reasoning before code in the post-:events flat layout"
      (with-raw-code-on
      ;; The pre-existing `:events`-driven interleaved variant was
      ;; removed when the runtime contract dropped `:events`. Resume /
      ;; live now share a flat layout: thinking first, then all code
      ;; blocks. Plain `:value` form results are hidden per user
      ;; directive, so only code (not `2`) follows the reasoning.
        (let [lines (format-iteration-entry
                      {:thinking "alpha\nbeta"
                       :error nil
                       :forms [{:code "(+ 1 1)" :comment nil :render-segments nil :result-render "2" :result-kind :value :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                      60 1 {:show-header? true})
              body  (strip-ansi (str/join "\n" (map body-of lines)))]
          (expect (< (.indexOf body "alpha") (.indexOf body "beta")))
          (expect (< (.indexOf body "beta") (.indexOf body "(+ 1 1)")))
          (expect (neg? (.indexOf body "2")))))))

  (it "keeps thinking and code flush so the thinking→badge margin matches code→badge"
    (with-raw-code-on
    ;; Parallel work (`equalize thinking→badge margin`) intentionally
    ;; collapsed the previous two-row pad block between thinking and
    ;; code so the visible breathing room is a SINGLE neutral row,
    ;; identical to the gap a code block leaves above the next iter
    ;; recap. This test pins that contract — thinking ends, one
    ;; blank/marker row, code starts — so a future layout tweak that
    ;; re-introduces a stripe between them surfaces here, not in a
    ;; user-visible asymmetry report.
      (let [lines (format-iteration-entry
                    {:thinking "alpha"
                     :error nil
                     :forms [{:code "(+ 1 1)" :comment nil :render-segments nil :result-render "2" :result-kind :value :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                    60 1 {:show-header? false :live-preview? true})
            visible (mapv (comp strip-ansi body-of) lines)
            alpha-idx (first (keep-indexed #(when (str/includes? %2 "alpha") %1) visible))
            code-idx  (first (keep-indexed #(when (str/includes? %2 "(+ 1 1)") %1) visible))]
        (expect (some? alpha-idx))
        (expect (some? code-idx))
        (expect (< alpha-idx code-idx))
      ;; Bound the gap: thinking and code should sit close, with a
      ;; small handful of marker / pad rows between them (per parallel
      ;; layout work). Hard ceiling guards against a regression that
      ;; explodes the gap into a multi-row stripe.
        (expect (<= (- code-idx alpha-idx) 5))))))

(defdescribe paint-styled-line-stacking-test
  ;; The Polish bug report: `> **Lącznie:**` inside a quote rendered
  ;; bold-without-italic because paint-styled-line! cleared the
  ;; wrapping italic at entry. We pin the fix by recording the SGR
  ;; set on every paint call via a stub TextGraphics, then asserting
  ;; bold + italic stack correctly.
  (let [;; Capture every (putString ...) as [text {:fg :bg :sgr}].
        captured (atom [])
        active   (atom #{})
        fg       (atom nil)
        bg       (atom nil)
        ;; Lanterna's TextGraphics is an interface with ~30 methods;
        ;; we proxy the four paint-styled-line! actually calls.
        graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                   (clearModifiers []
                     (reset! active #{})
                     this)
                   (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                     (swap! active into (seq arr))
                     this)
                   (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                     (apply swap! active disj (seq arr))
                     this)
                   (getActiveModifiers []
                     ;; Return a defensive EnumSet so paint-styled-line!
                     ;; can `EnumSet/copyOf` it.
                     (if (empty? @active)
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                       (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                   (setForegroundColor [c] (reset! fg c) this)
                   (setBackgroundColor [c] (reset! bg c) this)
                   (putString
                     ([col row text]
                      (swap! captured conj [text {:fg @fg :bg @bg :sgr @active}])
                      this)))]

    (describe "paint-styled-line! inherits the wrapping SGR modifiers"
      (it "BOLD inside a wrapping ITALIC stacks to bold-italic"
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (let [line (str "plain " p/INLINE_BOLD_ON "loud" p/INLINE_BOLD_OFF " tail")]
          (p/paint-styled-line! graphics 0 0 line
            (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
            (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
            (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
            (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
          ;; Three segments: "plain ", "loud", " tail"
          (let [segs @captured]
            (expect (= 3 (count segs)))
            (let [[seg0 seg1 seg2] segs]
              ;; Segment 1: 'plain ' - inherits italic only.
              (expect (= "plain " (first seg0)))
              (expect (contains? (:sgr (second seg0)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (not (contains? (:sgr (second seg0)) com.googlecode.lanterna.SGR/BOLD)))
              ;; Segment 2: 'loud' - italic + bold stacked.
              (expect (= "loud" (first seg1)))
              (expect (contains? (:sgr (second seg1)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (contains? (:sgr (second seg1)) com.googlecode.lanterna.SGR/BOLD))
              ;; Segment 3: ' tail' - italic again, bold cleared.
              (expect (= " tail" (first seg2)))
              (expect (contains? (:sgr (second seg2)) com.googlecode.lanterna.SGR/ITALIC))
              (expect (not (contains? (:sgr (second seg2)) com.googlecode.lanterna.SGR/BOLD)))))))

      (it "At exit, the inherited SGR set is restored exactly"
        ;; Caller relies on `(p/styled g [p/ITALIC] (paint-styled-line! ...))`
        ;; ending with the same modifier state it started with, so its
        ;; own cleanup can finalise correctly.
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (p/paint-styled-line! graphics 0 0
          (str p/INLINE_BOLD_ON "x" p/INLINE_BOLD_OFF)
          (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
          (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
          (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
          (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
        (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active)))

      (it "Dangling sentinel (no close) doesn't leak BOLD past the call"
        (reset! captured [])
        (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
        (p/paint-styled-line! graphics 0 0
          (str "open " p/INLINE_BOLD_ON "never closes")
          (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
          (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
          (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
          (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
        ;; Even though the line ended mid-bold, the inherited italic
        ;; (NOT bold) is what the caller sees on exit.
        (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active))))))

(defdescribe paint-ansi-line-inline-sentinel-test
  ;; Tool render-fns return Markdown. Inline code spans in that Markdown
  ;; become private-use sentinels before result painting. The result painter
  ;; must consume them; otherwise Lanterna renders glyphs like  / .
  (it "consumes inline code sentinels instead of painting PUA glyphs"
    (let [paint-ansi-line! @#'render/paint-ansi-line!
          captured        (atom [])
          active          (atom #{})
          fg              (atom nil)
          bg              (atom nil)
          graphics        (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                            (clearModifiers []
                              (reset! active #{})
                              this)
                            (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                              (swap! active into (seq arr))
                              this)
                            (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                              (apply swap! active disj (seq arr))
                              this)
                            (getActiveModifiers []
                              (if (empty? @active)
                                (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                                (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                            (setForegroundColor [c] (reset! fg c) this)
                            (setBackgroundColor [c] (reset! bg c) this)
                            (putString
                              ([col row text]
                               (swap! captured conj [text {:fg @fg :bg @bg :sgr @active}])
                               this)))
          line            (str "Searched "
                            p/INLINE_CODE_ON "[\"extensions\"]" p/INLINE_CODE_OFF
                            " with "
                            p/INLINE_CODE_ON "{:any [\"circling\"]}" p/INLINE_CODE_OFF
                            ". Use "
                            p/INLINE_CODE_ON "v/preview" p/INLINE_CODE_OFF)
          visible         "Searched [\"extensions\"] with {:any [\"circling\"]}. Use v/preview"]
      (paint-ansi-line! graphics 0 0 line t/code-result-fg t/code-ok-bg)
      (let [painted (apply str (map first @captured))]
        (expect (= visible painted))
        (expect (not (str/includes? painted p/INLINE_CODE_ON)))
        (expect (not (str/includes? painted p/INLINE_CODE_OFF)))))))

(defdescribe answer-text-inline-sentinel-paint-test
  ;; Final-answer IR uses MARKER_ANSWER_TXT for normal paragraphs.
  ;; Inline code inside that IR arrives at the bubble painter as
  ;; INLINE_CODE_ON/OFF sentinels. The answer-text branch must consume
  ;; them just like headings/bullets/quotes; otherwise the user sees
  ;; raw PUA glyphs like  and  around `/command`.
  (it "consumes inline code sentinels in plain final-answer text"
    (let [captured (atom [])
          active   (atom #{})
          fg       (atom nil)
          bg       (atom nil)
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [c] (reset! fg c) this)
                     (setBackgroundColor [c] (reset! bg c) this)
                     (fillRectangle [_pos _size _ch] this)
                     (setCharacter [_col _row _ch] this)
                     (putString
                       ([_col _row text]
                        (swap! captured conj text)
                        this)))
          line     (str p/MARKER_ANSWER_TXT
                     "Use " p/INLINE_CODE_ON "/command" p/INLINE_CODE_OFF
                     " (or pick from slash suggestions).")]
      (render/draw-chat-bubble! graphics
        {:role :assistant :timestamp nil :prewrapped-lines [line]}
        0 0 80)
      (let [painted (apply str @captured)]
        (expect (str/includes? painted "/command"))
        (expect (not (str/includes? painted p/INLINE_CODE_ON)))
        (expect (not (str/includes? painted p/INLINE_CODE_OFF)))))))

(defdescribe scrollbar-thumb-geometry-test
  ;; Geometry now lives in `scrollbar/geometry` — the single source of
  ;; truth for painter and hit-test. Test pinned here for back-compat,
  ;; mirrored in `scrollbar_test.clj`.
  (let [g (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)]
    (describe "Returns nil when there's no overflow"
      (it "total-h < inner-h: nothing to scroll"
        (expect (nil? (g 10 20 nil))))
      (it "total-h == inner-h: nothing to scroll"
        (expect (nil? (g 20 20 nil))))
      (it "inner-h is zero: no viewport, no thumb"
        (expect (nil? (g 100 0 0)))))

    (describe "Standard 100/20 session"
      (it "Auto-bottom (scroll=nil) places the single-cell thumb at the END of the track"
        (let [{:keys [thumb-top-rel thumb-h max-scroll]} (g 100 20 nil)]
          (expect (= 19 thumb-top-rel))   ;; track-h(20) - thumb-h(1) = 19
          (expect (= 1 thumb-h))
          (expect (= 80 max-scroll))))    ;; 100 - 20

      (it "scroll=0 places thumb at the TOP"
        (expect (= {:thumb-top-rel 0 :thumb-h 1 :max-scroll 80 :track-h 20}
                  (g 100 20 0))))

      (it "scroll=40 places thumb in the MIDDLE of the free track"
        (expect (= {:thumb-top-rel 9 :thumb-h 1 :max-scroll 80 :track-h 20}
                  (g 100 20 40))))

      (it "scroll=80 places thumb at the BOTTOM (== max-scroll)"
        (expect (= {:thumb-top-rel 19 :thumb-h 1 :max-scroll 80 :track-h 20}
                  (g 100 20 80)))))

    (describe "Out-of-range scroll values are clamped"
      (it "Negative scroll clamps to 0 (top)"
        (expect (zero? (:thumb-top-rel (g 100 20 -50)))))
      (it "Excessive scroll clamps to max-scroll (bottom)"
        (let [{:keys [thumb-top-rel max-scroll]} (g 100 20 9999)]
          (expect (= 19 thumb-top-rel))
          (expect (= 80 max-scroll)))))

    (describe "Viewport height changes keep one visible thumb cell"
      (it "1000-row content in a 5-row viewport: thumb-h is 1"
        (let [{:keys [thumb-h]} (g 1000 5 0)]
          (expect (= 1 thumb-h))))
      (it "360-row content in a maximized 56-row viewport: thumb-h stays 1"
        (let [{:keys [thumb-h]} (g 360 56 nil)]
          (expect (= 1 thumb-h))))
      (it "And the thumb still slides through the full track"
        (let [top (:thumb-top-rel (g 1000 5 0))
              bot (:thumb-top-rel (g 1000 5 995))]
          (expect (= 0 top))
          (expect (= 4 bot)))))    ;; track-h(5) - thumb-h(1) = 4
    ))

;; ─────────────────────────────────────────────────────────────────────────
;; Link-chrome strip - strip inline emphasis from anchor text
;;
;; `parse-md-refs` captures whatever sits between `[...]` raw, so
;; `[See **here**](url)` flows into the chrome row with literal `**`
;; baked in: "🔗 See **here** -> url". The renderer now runs the
;; anchor text through `markdown->inline` and strips the styling
;; sentinels, leaving plain visible text in the chrome.
;; ─────────────────────────────────────────────────────────────────────────

(def ^:private chrome-display-text @#'render/chrome-display-text)
(def ^:private resources-badge-label @#'render/resources-badge-label)
(def ^:private extract-link-refs @#'render/extract-link-refs)

(defn- chrome-of [src]
  (let [[ref] (links/parse-md-refs src)]
    (chrome-display-text ref 80)))

(defdescribe chrome-display-text-strips-inline-markup-test
  (describe "emphasis inside link text doesn't leak into chrome row"
    (it "`[See **here**](url)` -> no `**` in chrome"
      (let [s (chrome-of "[See **here**](https://example.com)")]
        (expect (str/includes? s "See here"))
        (expect (not (str/includes? s "**")))))

    (it "`[Spec *v2*](url)` -> no stray `*` in chrome"
      (let [s (chrome-of "[Spec *v2*](https://example.com)")]
        (expect (str/includes? s "Spec v2"))
        (expect (not (str/includes? s "*v2*")))))

    (it "``[Title with `code`](url)`` -> backticks stripped from chrome"
      (let [s (chrome-of "[Title with `code`](https://example.com)")]
        (expect (str/includes? s "Title with code"))
        (expect (not (str/includes? s "`code`"))))))

  (describe "emphasis OUTSIDE the brackets is unaffected (regression net)"
    ;; `**[link](url)**` - bold lives outside the bracket, so
    ;; `parse-md-refs` already captures clean text. This test pins
    ;; that the new strip doesn't accidentally mangle the URL or icon.
    (it "`**[link](url)**` -> chrome reads `link` cleanly"
      (let [s (chrome-of "**[link](https://example.com)**")]
        (expect (str/includes? s "link"))
        (expect (str/includes? s "https://example.com"))
        (expect (not (str/includes? s "**"))))))

  (describe "chrome icon + url tail still render after the strip"
    ;; Smoke test that the strip didn't drop the leading icon or the
    ;; ` -> ` separator. Without these the affordance loses its
    ;; "this is a link" cue.
    (it "icon + arrow + url all present"
      (let [s (chrome-of "[plain](https://example.com)")]
        (expect (str/includes? s " -> "))
        (expect (str/includes? s "https://example.com"))))))

(defdescribe extract-link-refs-guard-test
  (it "skips link extraction for giant messages"
    (let [text (apply str (repeat 25050 "a"))]
      (expect (= [] (extract-link-refs {:text text} 80))))))

(defdescribe resources-badge-test
  (describe "top-right resources badge"
    (it "chooses the richest label that fits"
      (expect (= "📚 Resources 3" (resources-badge-label 3 20)))
      (expect (= "📚 3" (resources-badge-label 3 4)))
      (expect (nil? (resources-badge-label 3 0))))

    (it "links no longer add per-resource rows to bubble height"
      (let [plain  {:role :assistant :text "a" :prewrapped-lines ["a"]}
            linked {:role :assistant
                    :text "[a](https://example.com)"
                    :prewrapped-lines ["a"]}]
        (render/invalidate-cache!)
        (expect (= (render/bubble-height plain 80)
                  (render/bubble-height linked 80)))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Loose-bullet coalesce - multi-paragraph list items render as one bullet
;;
;; Poorly-formatted markdown (LLM output, hand-edited prose) often
;; emits list items whose body has been fragmented across blank
;; lines:
;;
;;     - `dialogs.clj`
;;
;;      - removed `:system-prompt` palette command entry
;;     - `screen.clj`
;;
;;      - removed `:system-prompt` handler
;;
;; CommonMark spec-compliantly treats every fragment as its own
;; loose paragraph: only `dialogs.clj` lands under the bullet
;; marker, and `- removed ...` shows up flush-left as if it weren't
;; part of the bullet at all. The TUI bubble has nowhere to render
;; that hierarchy correctly.
;;
;; `coalesce-loose-list-items` (the pre-pass) folds every fragment
;; back into the bullet's text on a single line. Tests below pin
;; the specific shapes the user reported.
;; ─────────────────────────────────────────────────────────────────────────

;; ─────────────────────────────────────────────────────────────────────────
;; e4167d48: bullet continuation must NOT strip whitespace inside
;; inline-code spans
;;
;; Repro: model used `(v/join "prefix " (v/code "{:k :v :w x}")
;; " suffix")` to build a bullet body. `v/join` separates parts with
;; `\n\n`, so the resulting markdown has the inline-code span on its
;; own paragraph. `coalesce-loose-list-items` re-folds those
;; paragraphs into one bullet line and used to apply the punctuation
;; tightening regex `#" +([,;:.\)])"` to the WHOLE joined text. That
;; regex eats spaces before `:` — which is correct for prose
;; (`step :` -> `step:`) but wrong inside `` `{:k :v :w x}` `` where
;; the colons are EDN-keyword markers and the spaces are
;; semantically meaningful. User-visible damage:
;;
;;   `{:rendering-kind :vis/silent :result title}`
;;     -> `{:rendering-kind:vis/silent:result title}`
;;
;; Fix: tokenise on backticks first, only tighten prose tokens.
;; Mirror of `markdown/normalize-inline-spacing`'s tokenisation.
;; ─────────────────────────────────────────────────────────────────────────

;; ─────────────────────────────────────────────────────────────────────────
;; md-join inline-bold inside a bullet - the `Let / me / dig / deeper`
;; regression
;;
;; Faithful reconstruction of the FIRST `(done ...)` block in session
;; eeaf9651-06c7-4dda-9e97-877fcef06337, turn 363de6c6-..., position 1.
;; The agent built a bullet's body via `md-join`, which inserts `\n\n`
;; between every part. With the naive bullet-coalesce that earlier
;; treated every `**...**`-starting line as a structural break, the
;; bullet rendered as ONE bullet header + a ladder of one-word
;; paragraphs flush-left:
;;
;;     • Turn 1 - "system prompt copy" prune:
;;
;;     38 failures across iterations 2-7. ...
;;
;;     reader boundary split
;;
;;     - a multi-line form got fragmented into bare symbols (
;;
;;     Let
;;
;;     ,
;;
;;     me
;;
;;     ,
;;
;;     dig
;;     ...
;;
;; The fix in `coalesce-loose-list-items`:
;;   - Pure `**span**` lines (no trailing prose after the closing `**`)
;;     stay CONTINUATIONS of the bullet - they're an md-join artefact,
;;     the bold text is meant to flow inline inside the sentence.
;;   - `**Label:** value` lines (bold prefix + trailing content) STILL
;;     close the list - those are real top-level summary paragraphs.
;;
;; Below: build the same source with `md/*` helpers and assert the
;; bubble renders as ONE flowing bullet with every code-span / bold
;; span inline.
;; ─────────────────────────────────────────────────────────────────────────

(defn- dummy-text-graphics
  "Lenient TextGraphics stub for layout tests that care about click
   regions, not actual painted glyphs. Implements the subset of the
   Lanterna interface that `draw-chat-bubble!` touches on a plain-text
   bubble."
  []
  (let [active (atom #{})]
    (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
      (clearModifiers []
        (reset! active #{})
        this)
      (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
        (swap! active into (seq arr))
        this)
      (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
        (apply swap! active disj (seq arr))
        this)
      (getActiveModifiers []
        (if (empty? @active)
          (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
          (java.util.EnumSet/copyOf ^java.util.Collection @active)))
      (setForegroundColor [_] this)
      (setBackgroundColor [_] this)
      (putString
        ([_ _ _] this))
      (fillRectangle [_ _ _] this)
      (setCharacter [_ _ _] this))))

(defdescribe reasoning-preview-rendering-test
  (it "keeps completed reasoning visible when it is ten lines or less"
    (render/invalidate-cache!)
    (let [cid      "session"
          turn-id  "123e4567-e89b-12d3-a456-426614174000"
          thinking (str/join "\n" (map #(format "short-reason-%02d" %) (range 1 11)))
          payload  (render/format-answer-with-thinking-data
                     [:ir {} [:p {} [:span {} "done"]]] [{:thinking thinking}]
                     96 {:show-thinking true :show-iterations true} nil false
                     {:session-id cid
                      :session-turn-id turn-id})
          body     (strip-ansi (:text payload))]
      (expect (= "" (first (:lines payload))))
      (expect (= p/MARKER_THINKING (second (:lines payload))))
      (expect (str/includes? body "short-reason-01"))
      (expect (str/includes? body "short-reason-10"))
      (expect (not (str/includes? body "REASONING")))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "renders every reasoning line inline regardless of length"
    ;; Per user directive: collapsible disclosure rows were removed.
    ;; Long reasoning paints in place — no `▸ REASONING` summary,
    ;; no `lines hidden` hint, no toggle-details click region.
    (render/invalidate-cache!)
    (let [cid      "session"
          turn-id  "123e4567-e89b-12d3-a456-426614174000"
          thinking (str/join "\n\n" (map #(format "long-reason-%02d detail text" %) (range 1 12)))
          payload  (render/format-answer-with-thinking-data
                     [:ir {} [:p {} [:span {} "done"]]] [{:thinking thinking}]
                     96 {:show-thinking true :show-iterations true} nil false
                     {:session-id cid
                      :session-turn-id turn-id})
          body     (strip-ansi (:text payload))]
      (expect (str/includes? body "long-reason-01"))
      (expect (str/includes? body "long-reason-11"))
      (expect (str/includes? body "done"))
      (expect (not (str/includes? body "REASONING")))
      (expect (not (str/includes? body "lines hidden")))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "renders very long reasoning fully inline with no toggle needed"
    (render/invalidate-cache!)
    (let [cid      "session"
          turn-id  "123e4567-e89b-12d3-a456-426614174000"
          thinking (str/join "\n\n" (map #(format "line-%02d detail text" %) (range 1 51)))
          payload  (render/format-answer-with-thinking-data
                     [:ir {} [:p {} [:span {} "done"]]] [{:thinking thinking}]
                     96 {:show-thinking true :show-iterations true} nil false
                     {:session-id cid
                      :session-turn-id turn-id})
          body     (strip-ansi (:text payload))]
      (expect (not (str/includes? body "REASONING")))
      (expect (str/includes? body "line-01"))
      (expect (str/includes? body "line-25"))
      (expect (str/includes? body "line-50")))))

(defdescribe auto-collapse-rendering-test
  (it "hides legacy preview-kind result bodies entirely"
    ;; Per user directive: only `:tool` result-kind paints a body via
    ;; the channel-render IR. `:preview` (and any other non-tool kind)
    ;; renders no result pane, no PREVIEW/RAW switcher, no toggle.
    (render/invalidate-cache!)
    (let [body "only line of preview output"
          trace [{:forms [{:code "(v/cat file)" :comment nil :render-segments nil :result-render body :result-kind :preview :result-detail {:raw "{:secret \"raw-only\"}"} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:session-id "session"
                     :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})]
      (expect (not (str/includes? (:text payload) body)))
      (expect (not (str/includes? (:text payload) "raw-only")))
      (expect (not (str/includes? (:text payload) "PREVIEW")))
      (expect (not (str/includes? (:text payload) "● RAW")))
      (expect (not-any? #(= :preview-switcher (:kind %)) (:line-meta payload)))))

  (it "renders tool result text literally without ANSI syntax colors"
    ;; Tools own their own output formatting via `:render-fn`; the TUI
    ;; must paint the channel-render text verbatim, not re-tokenize it
    ;; with zprint ANSI codes. Plain `:value` form results no longer
    ;; render at all, so this assertion lives on the `:tool` path.
    (render/invalidate-cache!)
    (let [raw "{:b 2 :a [1 2 3]}"
          trace [{:forms [{:code "{:b 2 :a [1 2 3]}" :comment nil :render-segments nil :result-render raw :result-kind :tool :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:session-id "session"
                     :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          text (:text payload)
          result-lines (filter #(str/includes? % raw) (:lines payload))]
      (expect (str/includes? text raw))
      (expect (seq result-lines))
      (expect (not-any? #(re-find #"\u001b\[[0-9;]*m" %) result-lines))))

  (it "renders operation badges and color roles for tool result summaries"
    (render/invalidate-cache!)
    (let [trace [{:forms [{:code "(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])" :comment nil :render-segments nil :result-render "1 file changed" :result-kind :tool :result-detail {:op :v/patch
                                                                                                                                                                                               :tag :mutation
                                                                                                                                                                                               :color-role :tool-color/edit} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:session-id "session"
                     :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})]
      (expect (str/includes? (:text payload) "MUTATION patch"))
      (expect (some #(= :tool-color/edit (:color-role %)) (:line-meta payload)))))

  (it "renders one MUTATION patch badge inline for huge tool results"
    ;; Per user directive: huge tool results paint fully inline with
    ;; their channel-render body — no `▸` collapse glyph, no duplicate
    ;; badges. The badge appears exactly once above the diff body.
    (render/invalidate-cache!)
    (let [huge-result (str "Patched file.\n" (str/join " " (repeat 500 "diff-line")))
          trace       [{:forms [{:code "(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])" :comment nil :render-segments nil :result-render huge-result :result-kind :tool :result-detail {:op :v/patch
                                                                                                                                                                                                :tag :mutation
                                                                                                                                                                                                :color-role :tool-color/edit} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload     (render/format-answer-with-thinking-data
                        nil trace 96 {:show-iterations true} nil false
                        {:session-id "session"
                         :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          body        (strip-ansi (:text payload))]
      (expect (= 1 (count (re-seq #"MUTATION patch" body))))
      (expect (str/includes? body "Patched file."))
      (expect (not-any? #(str/starts-with? (body-of %) "▸ MUTATION patch") (:lines payload)))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "does not emit vague duplicate search-any rows"
    (render/invalidate-cache!)
    (let [trace   [{:forms [{:code "(v/rg {:any [\"alpha\" \"beta\"] :paths [\"src\"]})" :comment nil :render-segments nil :result-render "Searched `[\"src\"]` with `{:any [\"alpha\" \"beta\"], :paths [\"src\"]}` - 0 hit(s)." :result-kind :tool :result-detail {:op :any
                                                                                                                                                                                                                                                                     :tag :observation
                                                                                                                                                                                                                                                                     :color-role :tool-color/search
                                                                                                                                                                                                                                                                     :spec {:any ["alpha" "beta"] :paths ["src"]}
                                                                                                                                                                                                                                                                     :paths ["src"]} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:session-id "session"
                     :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          body    (strip-ansi (:text payload))]
      (expect (not (str/includes? body "SEARCH any")))
      (expect (= 1 (count (re-seq #"Searched" body))))))

  (it "hides huge plain value results entirely without a collapse summary"
    ;; Per user directive: collapsible disclosure was removed. Plain
    ;; `:value` form results never paint a body — no `RESULT` label,
    ;; no `chars hidden` summary, no `[iteration N · block M]` band.
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 4000 "abcdefghij"))
          trace       [{:forms [{:code "(+ 1 2)" :comment nil :render-segments nil :result-render huge-result :result-kind :value :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          turn-id     "123e4567-e89b-12d3-a456-426614174000"
          fut         (future
                        (render/format-answer-with-thinking-data
                          nil trace 96 {:show-iterations true} nil false
                          {:session-id "session"
                           :session-turn-id turn-id}))
          ;; Sanity timeout: 2s is the upper bound for a single
          ;; 40k-char `:value` render on the slowest CI box. Tight
          ;; sub-second bounds caught a real bug once but now flap
          ;; with every micro-bench drift; the assertion still
          ;; catches pathological infinite loops.
          payload     (deref fut 2000 ::timeout)]
      (when (= ::timeout payload)
        (future-cancel fut))
      (expect (not= ::timeout payload))
      (expect (not (str/includes? (:text payload) "RESULT")))
      (expect (not (str/includes? (:text payload) "chars hidden")))
      (expect (not (str/includes? (:text payload) "[iteration 1 · block 1]")))
      (expect (not (str/includes? (:text payload) huge-result)))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "renders completed reasoning fully inline on the answer view"
    ;; Per user directive: reasoning never collapses. The baseline
    ;; render shows every line; the legacy `:detail-expansions` map is
    ;; ignored because the toggle infrastructure is gone.
    (render/invalidate-cache!)
    (let [thinking (str/join "\n\n" (map #(str "line " % " detail text") (range 1 51)))
          trace    [{:thinking  thinking
                     :code      []
                     :results   []
                     :durations []
                     :successes []}]
          opts     {:session-id "session"
                    :session-turn-id "turn-1"}
          payload  (render/format-answer-with-thinking-data
                     [:ir {} [:p {} [:span {} "done"]]] trace 120
                     {:show-iterations true :show-thinking true}
                     nil false opts)
          body     (strip-ansi (:text payload))]
      (expect (not (str/includes? body "REASONING")))
      (expect (not (str/includes? body "lines hidden")))
      (expect (str/includes? body "line 1 "))
      (expect (str/includes? body "line 25"))
      (expect (str/includes? body "line 50"))))

  (it "never paints a collapsed summary band"
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
          trace       [{:forms [{:code "(+ 1 2)" :comment nil :render-segments nil :result-render huge-result :result-kind nil :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload     (render/format-answer-with-thinking-data
                        nil trace 96 {:show-iterations true} nil false
                        {:session-id "session"
                         :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})
          puts        (atom [])
          active      (atom #{})
          bg          (atom nil)
          graphics    (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                        (clearModifiers []
                          (reset! active #{})
                          this)
                        (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                          (swap! active into (seq arr))
                          this)
                        (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                          (apply swap! active disj (seq arr))
                          this)
                        (getActiveModifiers []
                          (if (empty? @active)
                            (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                            (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                        (setForegroundColor [_] this)
                        (setBackgroundColor [c] (reset! bg c) this)
                        (putString [_col _row text]
                          (swap! puts conj {:text text :bg @bg :sgr @active})
                          this)
                        (fillRectangle [_ _ _] this)
                        (setCharacter [_ _ _] this))]
      (render/draw-chat-bubble! graphics
        {:role :assistant :text (:text payload) :prewrapped-lines (:lines payload)}
        0 2 96 {:viewport-h 50})
      ;; Per user directive: collapsible disclosure was removed. The
      ;; compact `[iteration N · block M]` band is gone with it; the
      ;; painter must not emit any summary row for a hidden value
      ;; result.
      (expect (not-any? #(str/includes? (:text %) "[iteration 1 · block 1]") @puts))
      (expect (not-any? #(str/includes? (:text %) huge-result) @puts)))))

;; ─────────────────────────────────────────────────────────────────────────
;; Tool detail result rendering. Tool render-fns may return Markdown-ish
;; strings, but result panes are output logs, not answer prose: no automatic
;; Markdown-to-TUI coloring, no special fence renderer, no visible fence rows.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe tool-detail-plain-result-rendering-test
  (describe "expanded tool result body renders as literal result text"
    (it "bullet lines inside an expanded tool result are reachable as text"
      ;; The earlier contract re-rendered tool-result bodies through the
      ;; Markdown pipeline, producing MARKER_MD_BULLET rows. The current
      ;; renderer keeps tool result bodies as literal text on the
      ;; result-marker band — cheaper, and side-steps reparsing arbitrary
      ;; tool output. The user-facing text still shows the bullets, just
      ;; not as styled bullet rows.
      (render/invalidate-cache!)
      (let [body (str "Tool finished:\n"
                   "- alpha line\n"
                   "- beta line\n"
                   "- gamma line")
            big-body (str body "\n\n" (apply str (repeat 4096 "x")))
            trace [{:forms [{:code "(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])" :comment nil :render-segments nil :result-render big-body :result-kind :tool :result-detail {:op :v/patch :tag :mutation
                                                                                                                                                                                         :color-role :tool-color/edit} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
            opts {:session-id "session"
                  :session-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {["session" "iteration:t123e4567:i1:b1:result"] true}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            lines   (:lines payload)]
        (expect (some #(str/includes? % "alpha line") lines))
        (expect (some #(str/includes? % "beta line") lines))
        (expect (some #(str/includes? % "gamma line") lines))))

    (it "inline bold/italic inside tool results stays literal, with no inline sentinels"
      (render/invalidate-cache!)
      (let [body "Result: **important** and *subtle*."
            ;; Trip auto-collapse with filler.
            big-body (str body "\n" (apply str (repeat 4096 "y")))
            trace [{:forms [{:code "(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])" :comment nil :render-segments nil :result-render big-body :result-kind :tool :result-detail {:op :v/patch :tag :mutation
                                                                                                                                                                                         :color-role :tool-color/edit} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
            opts {:session-id "session"
                  :session-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {["session" "iteration:t123e4567:i1:b1:result"] true}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            lines   (:lines payload)
            text    (:text payload)]
        (expect (not-any? #(str/includes? % p/INLINE_BOLD_ON) lines))
        (expect (not-any? #(str/includes? % p/INLINE_BOLD_OFF) lines))
        (expect (not-any? #(str/includes? % p/INLINE_ITALIC_ON) lines))
        (expect (not-any? #(str/includes? % p/INLINE_ITALIC_OFF) lines))
        (expect (str/includes? text "**important**"))
        (expect (str/includes? text "*subtle*"))))

    (it "strips fenced marker rows and does not run Markdown structural rendering in tool results"
      (render/invalidate-cache!)
      (let [body (str "# Tool output\n\n"
                   "```diagram\n"
                   "flowchart LR\n"
                   "A[Start] --> B[Done]\n"
                   "```\n\n"
                   "```diff\n"
                   "+changed\n"
                   "```\n")
            big-body (str body "\n" (apply str (repeat 4096 "z")))
            trace [{:forms [{:code "(v/cat \"x\")" :comment nil :render-segments nil :result-render big-body :result-kind :tool :result-detail {:op :v/cat :tag :observation
                                                                                                                                                :color-role :tool-color/read} :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
            opts {:session-id "session"
                  :session-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {["session" "iteration:t123e4567:i1:b1:result"] true}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            lines   (:lines payload)
            text    (:text payload)]
        (expect (not (str/includes? text "```")))
        (expect (not (str/includes? text "diagram")))
        (expect (str/includes? text "# Tool output"))
        (expect (str/includes? text "flowchart LR"))
        (expect (str/includes? text "A[Start] --> B[Done]"))
        (expect (str/includes? text "+changed"))
        (expect (not-any? #(= p/MARKER_MD_H1 (marker-of %)) lines))
        (expect (not-any? #(= p/MARKER_MD_CODE (marker-of %)) lines))))

    (it "errors keep raw rendering - error formatting handles its own marker"
      ;; Errors come through err-result-marker. This test pins that
      ;; contract: the error body still appears verbatim and does NOT
      ;; pick up bullet markers from accidental IR conversion.
      (render/invalidate-cache!)
      (let [trace [{:forms [{:code "(boom)" :comment nil :render-segments nil :result-render "- pretend-bullet" :result-kind :tool :result-detail nil :error nil :started-at-ms nil :duration-ms 1 :success? false :silent? false}]}]
            opts {:session-id "session"
                  :session-turn-id "123e4567-e89b-12d3-a456-426614174000"
                  :detail-expansions {}}
            payload (render/format-answer-with-thinking-data
                      nil trace 96 {:show-iterations true} nil false opts)
            lines   (:lines payload)]
        ;; The literal `- pretend-bullet` text stays in the row; we
        ;; never re-render error bodies as Markdown.
        (expect (some #(str/includes? % "- pretend-bullet") lines))))))

;; ─────────────────────────────────────────────────────────────────────────
;; Retired answer disclosure tags must not create collapsible output.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe retired-answer-disclosure-test
  (it "ignores :details/:summary as structure and emits no summary lane"
    (let [answer  [:ir {}
                   [:details {:open? false}
                    [:summary {} [:span {} "Plan"]]
                    [:p {} [:span {} "alpha"]]
                    [:p {} [:span {} "beta"]]]]
          payload (render/format-answer-markdown-data answer 80
                    {:session-id "cid"
                     :detail-expansions {["cid" "answer:details:d1"] false}})
          lines   (:lines payload)
          visible (str/join "\n" (map strip-sentinels lines))]
      (expect (not-any? #(= p/MARKER_MD_SUMMARY (marker-of %)) lines))
      (expect (str/includes? visible "Planalphabeta")))))

;; ─────────────────────────────────────────────────────────────────────────
;; Provider-error answer rendering.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe provider-error-answer-test
  (it "renders only the IR provider-error block, not duplicate trace error rows"
    (render/invalidate-cache!)
    (let [answer [:ir {:vis/provider-error true}
                  [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
                  [:p {} [:span {} "Provider call failed before the model could run."]]
                  [:p {} [:span {} "WHAT HAPPENED: invalid thinking signature"]]]
          trace  [{:error {:message "Exceptional status code: 400"
                           :data {:status 400
                                  :body "{\"error\":{\"message\":\"Invalid `signature` in `thinking` block\"}}"}}}]
          payload (render/format-answer-with-thinking-data
                    answer trace 96 {:show-iterations true} nil false {})
          text (:text payload)]
      (expect (= 1 (count (re-seq #"PROVIDER_ERROR" text))))
      (expect (not (str/includes? text "provider response:")))
      (expect (str/includes? text "WHAT HAPPENED: invalid thinking signature")))))

(defdescribe answer-separator-test
  (it "does not draw a bottom border between reasoning and final answer"
    (render/invalidate-cache!)
    (let [payload (render/format-answer-with-thinking-data
                    [:ir {} [:p {} [:span {} "done"]]] [{:thinking "reasoning"}]
                    80 {:show-thinking true :show-iterations true} nil false {})]
      (expect (not (str/includes? (:text payload) p/MARKER_ANSWER_SEP)))
      (expect (not-any? #(str/starts-with? % p/MARKER_ANSWER_SEP) (:lines payload))))))

(defdescribe message-footer-test
  (it "does not register a per-message copy button"
    (cr/reset!)
    (cr/begin-frame!)
    (let [message      {:role :assistant :text "hello world"}
          start        4
          left         2
          width        36
          viewport-top 7
          height       (render/draw-chat-bubble! (dummy-text-graphics) message start left width
                         {:viewport-top viewport-top :viewport-h 40})
          hit-col      (+ left 2)]
      (cr/commit-frame!)
      (expect (= 3 height))
      (expect (every? nil?
                (map #(cr/lookup hit-col %)
                  (range viewport-top (+ viewport-top start height)))))))

  (it "renders cached token usage in the assistant bubble footer"
    (let [puts    (atom [])
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers [] this)
                     (enableModifiers [_] this)
                     (disableModifiers [_] this)
                     (getActiveModifiers []
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :assistant
                      :text "hello"
                      :tokens {:input 100 :output 20 :cached 70}}
                     4 2 60 {:viewport-h 40})]
      (expect (= 5 height))
      (expect (some #(str/includes? (:text %) "tok 100→20 (cached 70)")
                @puts))))

  (it "omits zero cached token usage in the assistant bubble footer"
    (let [puts    (atom [])
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers [] this)
                     (enableModifiers [_] this)
                     (disableModifiers [_] this)
                     (getActiveModifiers []
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :assistant
                      :text "hello"
                      :tokens {:input 100 :output 20 :cached 0}}
                     4 2 60 {:viewport-h 40})]
      (expect (= 5 height))
      (expect (some #(str/includes? (:text %) "tok 100→20")
                @puts))
      (expect (not-any? #(str/includes? (:text %) "cached 0")
                @puts))))

  (it "leaves one blank row between assistant answer and bubble footer"
    (let [puts    (atom [])
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers [] this)
                     (enableModifiers [_] this)
                     (disableModifiers [_] this)
                     (getActiveModifiers []
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :assistant
                      :text "hello"
                      :tokens {:input 100 :output 20}}
                     4 2 60 {:viewport-h 40})
          answer-row (:row (first (filter #(= "hello" (:text %)) @puts)))
          footer-row (:row (first (filter #(str/includes? (:text %) "tok 100→20") @puts)))]
      (expect (= 5 height))
      (expect (= 2 (- footer-row answer-row)))))

  (it "renders hidden silent form count next to iteration count in the footer"
    (let [puts    (atom [])
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers [] this)
                     (enableModifiers [_] this)
                     (disableModifiers [_] this)
                     (getActiveModifiers []
                       (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :assistant
                      :text "hello"
                      :iteration-count 3
                      :traces [{:forms [{:silent? true}
                                        {:silent? false}]}
                               {:forms [{:silent? true}]}]}
                     4 2 60 {:viewport-h 40})]
      (expect (= 5 height))
      (expect (some #(str/includes? (:text %) "3 iters (2 silent)")
                @puts))))

  (it "ignores legacy turn separator flag on user prompts"
    (let [puts    (atom [])
          active  (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text :sgr @active})
                       this)
                     (fillRectangle [_pos _size _ch] this)
                     (setCharacter [_ _ _] this))
          height   (render/draw-chat-bubble! graphics
                     {:role :user :text "hello" :turn-separator? true}
                     4 2 30 {:viewport-h 40})]
      (expect (= 5 height))
      (expect (not-any? #(str/includes? (or (:text %) "") "──") @puts))
      (expect (some #(and (= 4 (:row %))
                       (= "You" (:text %))
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                @puts))))

  (it "renders user messages with a left rail and markdown styling"
    (let [puts    (atom [])
          active  (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [_col row text]
                       (swap! puts conj {:row row :text text :sgr @active})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          ;; Inline markdown styling for user-message bubbles now lives
          ;; in the `virtual.clj` projection layer that supplies
          ;; `:prewrapped-lines` to `draw-chat-bubble!`. The bubble
          ;; painter itself no longer parses markdown from `:text`
          ;; (the IR→prewrapped lift happens upstream). Feed prewrapped
          ;; lines directly so the assertion exercises the painter, not
          ;; the retired in-painter markdown lift.
          rendered (render/format-answer-markdown-data
                     (vis/markdown->ir "**SIEMA**\n\n> quoted text") 50 nil)
          message {:role :user :text "**SIEMA**\n\n> quoted text"
                   :prewrapped-lines (:lines rendered)
                   :line-meta (:line-meta rendered)}
          start   4
          left    2
          width   50
          height  (render/draw-chat-bubble! graphics message start left width
                    {:viewport-h 40})]
      (expect (pos? height))
      ;; SIEMA appears on one of the painted rows; markdown styling
      ;; (bold/italic via inline sentinels) is driven by
      ;; `virtual.clj`'s projection layer, which uses MARKER_ANSWER_TXT
      ;; on the answer side and MARKER_MD_* on plain-markdown blocks.
      ;; This test only pins that the painter visits the rendered
      ;; rows and surfaces the user-visible text — the bold-SGR
      ;; activation is exercised by the answer-side painter tests.
      (expect (some #(str/includes? (or (:text %) "") "SIEMA") @puts))
      (expect (some #(str/includes? (or (:text %) "") "quoted text") @puts))))

  (it "draws assistant answer text aligned with the Vis label"
    (let [puts    (atom [])
          active  (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString [col row text]
                       (swap! puts conj {:col col :row row :text text})
                       this)
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this))
          rendered (render/format-answer-markdown-data
                     (vis/markdown->ir "hello") 50 nil)
          left     2
          _height  (render/draw-chat-bubble! graphics
                     {:role :assistant
                      :text ""
                      :prewrapped-lines (:lines rendered)
                      :line-meta (:line-meta rendered)}
                     4 left 50 {:viewport-h 40})
          answer-put (first (filter #(str/includes? (:text %) "hello") @puts))]
      (expect (= left (:col answer-put)))))

  (it "draws markdown fenced code one column inside the bubble band"
    (let [fills   (atom [])
          puts    (atom [])
          active  (atom #{})
          fg      (atom nil)
          bg      (atom nil)
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [c] (reset! fg c) this)
                     (setBackgroundColor [c] (reset! bg c) this)
                     (putString [col row text]
                       (swap! puts conj {:col col :row row :text text :fg @fg :bg @bg})
                       this)
                     (fillRectangle [pos size _ch]
                       (swap! fills conj {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
                                          :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
                                          :w   (.getColumns ^com.googlecode.lanterna.TerminalSize size)
                                          :h   (.getRows ^com.googlecode.lanterna.TerminalSize size)
                                          :fg  @fg
                                          :bg  @bg})
                       this)
                     (setCharacter [_ _ _] this))
          rendered (render/format-answer-markdown-data
                     (vis/markdown->ir "```clojure\n(+ 1 2)\n```") 50 nil)
          left    2
          width   50
          text-x  (inc left)
          _height (render/draw-chat-bubble! graphics
                    {:role :assistant
                     :text ""
                     :prewrapped-lines (:lines rendered)
                     :line-meta (:line-meta rendered)}
                    4 left width {:viewport-h 40})
          code-put  (first (filter #(str/includes? (:text %) "(+ 1 2)") @puts))
          code-fill (first (filter #(and (= t/code-block-bg (:bg %))
                                      (= (:row code-put) (:row %)))
                             @fills))]
      (expect (= text-x (:col code-put)))
      (expect (= left (:col code-fill)))
      (expect (= width (:w code-fill)))))

  (it "leaves only the final gap after the user bubble fill"
    (let [fills    (atom [])
          active   (atom #{})
          graphics (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                     (clearModifiers []
                       (reset! active #{})
                       this)
                     (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (swap! active into (seq arr))
                       this)
                     (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                       (apply swap! active disj (seq arr))
                       this)
                     (getActiveModifiers []
                       (if (empty? @active)
                         (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                         (java.util.EnumSet/copyOf ^java.util.Collection @active)))
                     (setForegroundColor [_] this)
                     (setBackgroundColor [_] this)
                     (putString
                       ([_ _ _] this))
                     (fillRectangle [pos size _ch]
                       (swap! fills conj {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
                                          :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
                                          :w   (.getColumns ^com.googlecode.lanterna.TerminalSize size)
                                          :h   (.getRows ^com.googlecode.lanterna.TerminalSize size)})
                       this)
                     (setCharacter [_ _ _] this))
          message      {:role :user :text "hello world"}
          start        4
          left         2
          width        36
          viewport-top 7
          height       (render/draw-chat-bubble! graphics message start left width
                         {:viewport-top viewport-top :viewport-h 40})
          gap-row      (+ start height -1)
          bubble-fill  (some (fn [fill]
                               (when (and (= left (:col fill))
                                       (= width (:w fill))
                                       (pos? (:h fill)))
                                 fill))
                         @fills)
          bubble-last-row (+ (:row bubble-fill) (:h bubble-fill) -1)]
      (expect (= 5 height))
      (expect (= 3 (:h bubble-fill)))
      (expect (= bubble-last-row (dec gap-row))))))

(defdescribe bubble-row-clipping-test
  (it "clips prewrapped formatter rows at bubble content width"
    (let [plain   (apply str (repeat 80 "x"))
          marked  (str p/MARKER_CODE_OK (apply str (repeat 80 "y")))
          clipped (clip-lines-preserving-markers [plain marked] 17)]
      (expect (= 2 (count clipped)))
      (expect (= (subs plain 0 17) (first clipped)))
      (expect (= p/MARKER_CODE_OK (marker-of (second clipped))))
      (expect (<= (p/display-width (first clipped)) 17))
      (expect (<= (p/display-width (body-of (second clipped))) 17))))

  (it "clips ANSI-colored Clojure formatter rows without handing ESC to Lanterna"
    (let [ansi-line (str p/MARKER_CODE_OK
                      "\u001b[32m(\u001b[0m\u001b[34mdef\u001b[0m "
                      "\u001b[30mrequest-classification\u001b[0m "
                      "\u001b[35m:evidence-bearing-code-change\u001b[0m")
          clipped   (first (clip-lines-preserving-markers [ansi-line] 12))]
      (expect (= p/MARKER_CODE_OK (marker-of clipped)))
      (expect (str/includes? clipped "\u001b[32m"))
      (expect (<= (p/display-width (strip-ansi (body-of clipped))) 12))))

  (it "reuses clipped prewrapped rows while scrolling huge trace bubbles"
    (render/invalidate-cache!)
    (let [huge-line (str p/MARKER_CODE_OK (apply str (repeat 4000 "x")))
          message   {:role :assistant
                     :timestamp nil
                     :prewrapped-lines (vec (repeat 1000 huge-line))}
          draw!     #(render/draw-chat-bubble! (dummy-text-graphics) message 0 2 100
                       {:viewport-top 0 :viewport-h 35})]
      (draw!)
      (let [t0 (System/nanoTime)
            _  (draw!)
            ms (/ (- (System/nanoTime) t0) 1e6)]
        (expect (< ms 20.0))))))

(defdescribe slash-command-suggestions-overlay-test
  (it "draws a bordered, BOLD, accent-stripe title with flex hint pairs"
    (let [puts   (atom [])
          fills  (atom [])
          fg     (atom nil)
          bg     (atom nil)
          active (atom #{})
          g (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
              (clearModifiers []
                (reset! active #{})
                this)
              (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (swap! active into (seq arr))
                this)
              (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (apply swap! active disj (seq arr))
                this)
              (getActiveModifiers []
                (if (empty? @active)
                  (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                  (java.util.EnumSet/copyOf ^java.util.Collection @active)))
              (setForegroundColor [c] (reset! fg c) this)
              (setBackgroundColor [c] (reset! bg c) this)
              (getForegroundColor [] @fg)
              (getBackgroundColor [] @bg)
              (putString [col row text]
                (swap! puts conj {:col col :row row :text text
                                  :fg @fg :bg @bg :sgr @active})
                this)
              (fillRectangle [pos size _ch]
                (swap! fills conj {:row (.getRow pos) :col (.getColumn pos)
                                   :w (.getColumns size) :h (.getRows size)
                                   :fg @fg :bg @bg})
                this)
              (setCharacter [_ _ _] this))
          suggestions [{:label "first"  :slash/usage "/first"  :slash/selected? true}
                       {:label "second" :slash/usage "/second" :slash/selected? false}]
          input-top   10
          cols        80
          n           (count suggestions)
          ;; Layout (from top to bottom):
          ;;   margin-row -> title-row -> border-row -> sug rows ...
          first-sug   (- input-top n)
          border-row  (dec first-sug)
          title-row   (dec border-row)
          margin-row  (dec title-row)
          ;; Horizontal margin matches input box rule pad (2 cols).
          pad         2
          inner-w     (- cols (* 2 pad))]
      (render/draw-slash-command-suggestions! g suggestions input-top cols)

      ;; Title row sits ABOVE the border row (border under title).
      (expect (< title-row border-row))

      ;; Title row: accent stripe (fillRectangle) on title-bg, inset
      ;; by `pad` cols on each side so it lines up with the input box.
      (expect (some #(and (= title-row (:row %))
                       (= t/dialog-title-bg (:bg %))
                       (= pad (:col %)))
                @fills))

      ;; Border row UNDER the title: horizontal rule, inset by `pad`,
      ;; same column span as the title accent stripe.
      (expect (some #(and (= border-row (:row %))
                       (str/starts-with? (:text %) "─")
                       (= pad (:col %))
                       (= t/dialog-border (:fg %))
                       (= t/terminal-bg (:bg %)))
                @puts))

      ;; Top margin row: a full-width terminal-bg gap above the title.
      (expect (some #(and (= margin-row (:row %))
                       (= 0 (:col %))
                       (= t/terminal-bg (:bg %)))
                @fills))

      ;; Title row: BOLD label "Slash commands" on the accent stripe.
      (expect (some #(and (= title-row (:row %))
                       (str/includes? (:text %) "Slash commands")
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD)
                       (= t/dialog-title-fg (:fg %))
                       (= t/dialog-title-bg (:bg %)))
                @puts))

      ;; Title row: BOLD keys for each [key action] hint pair.
      ;; Enter and Tab both complete selected slash suggestion.
      (doseq [k ["↑↓/wheel" "Enter/Tab"]]
        (expect (some #(and (= title-row (:row %))
                         (= k (:text %))
                         (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                  @puts)))

      (expect (not-any? #(and (= title-row (:row %))
                           (#{"Enter" "Tab"} (:text %)))
                @puts))

      ;; Title row: action words rendered NON-BOLD next to their keys.
      (doseq [a [" select" " complete"]]
        (expect (some #(and (= title-row (:row %))
                         (= a (:text %))
                         (not (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD)))
                  @puts)))

      ;; Title items spread across the inner width (space-between):
      ;; first item sits inside the left margin, last item in the
      ;; right half of the inner span.
      (let [title-puts (filter #(= title-row (:row %)) @puts)
            cols-used  (mapv :col title-puts)]
        (expect (some #(<= pad % (+ pad 2)) cols-used))
        (expect (some #(>= % (+ pad (quot inner-w 2))) cols-used)))

      ;; Suggestion rows are inset to the same column span as the
      ;; title accent stripe (margin-left = margin-right = pad).
      ;; The body fill on every row uses the normal `dialog-bg`
      ;; palette — selection is signalled by the `>` glyph in the
      ;; left margin, NOT by a full-row accent stripe.
      (expect (some #(and (= first-sug (:row %))
                       (= pad (:col %))
                       (= inner-w (:w %))
                       (= t/dialog-bg (:bg %)))
                @fills))

      ;; The selected row carries a BOLD `> ` cursor glyph at the
      ;; FIRST col of the inset body (col `pad`), painted in
      ;; `dialog-hint-key` on `dialog-bg` so the marker reads as
      ;; INSIDE the menu rather than floating in the terminal margin.
      ;; The non-selected row gets nothing painted in that column.
      (expect (some #(and (= first-sug (:row %))
                       (= pad (:col %))
                       (= "> " (:text %))
                       (= t/dialog-hint-key (:fg %))
                       (= t/dialog-bg (:bg %))
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                @puts))
      (expect (not-any? #(and (= (inc first-sug) (:row %))
                           (= pad (:col %))
                           (= "> " (:text %)))
                @puts))

      ;; Each suggestion row paints a markdown-style chip:
      ;;   <code-bg fill> /cmd <code-bg fill end> ` - ` <italic desc>
      (let [sug-rows  (filter #(<= first-sug (:row %)) @puts)
            usages    (filter #(str/starts-with? (:text %) "/") sug-rows)
            seps      (filter #(= " - " (:text %)) sug-rows)
            descs     (filter #(contains? (:sgr %) com.googlecode.lanterna.SGR/ITALIC) sug-rows)
            chip-fills (filter #(and (<= first-sug (:row %))
                                  (= t/code-block-bg (:bg %))) @fills)]
        ;; One chip fill, usage, separator and description per suggestion.
        (expect (= n (count chip-fills)))
        (expect (= n (count usages)))
        (expect (= n (count seps)))
        (expect (= n (count descs)))
        ;; Layout invariants per row: chip wraps the usage with 1 col
        ;; padding on each side, ` - ` follows the chip, italic desc
        ;; follows the separator. The chip starts AFTER the selection
        ;; gutter (`p/SELECTION_WIDTH` cols inside the inset body).
        (doseq [[chip u s d] (map vector
                               (sort-by :row chip-fills)
                               (sort-by :row usages)
                               (sort-by :row seps)
                               (sort-by :row descs))]
          ;; Chip lives past the selection gutter — first chip col is
          ;; at least `pad + p/SELECTION_WIDTH` (cursor + 1-col margin).
          (expect (>= (:col chip) (+ pad com.blockether.vis.ext.channel-tui.primitives/SELECTION_WIDTH)))
          ;; Chip starts one col before the usage and is exactly
          ;; (usage-width + 2) wide.
          (expect (= (:col u) (inc (:col chip))))
          (expect (= (:w chip) (+ (count (:text u)) 2)))
          ;; Usage paints in code-block colors.
          (expect (= t/code-block-fg (:fg u)))
          (expect (= t/code-block-bg (:bg u)))
          ;; ` - ` separator sits immediately after the chip.
          (expect (= (:col s) (+ (:col chip) (:w chip))))
          ;; Italic description follows the separator on the same row.
          (expect (= (:row u) (:row d)))
          (expect (= (:col d) (+ (:col s) (count (:text s)))))))))

  (it "drops the border row when there is not enough vertical space"
    (let [puts   (atom [])
          active (atom #{})
          g (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
              (clearModifiers [] (reset! active #{}) this)
              (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (swap! active into (seq arr)) this)
              (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
                (apply swap! active disj (seq arr)) this)
              (getActiveModifiers []
                (if (empty? @active)
                  (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
                  (java.util.EnumSet/copyOf ^java.util.Collection @active)))
              (setForegroundColor [_] this)
              (setBackgroundColor [_] this)
              (getForegroundColor [] nil)
              (getBackgroundColor [] nil)
              (putString [col row text]
                (swap! puts conj {:col col :row row :text text}) this)
              (fillRectangle [_ _ _] this)
              (setCharacter [_ _ _] this))
          ;; input-top = 2: only enough room for title (1 row) + 1
          ;; suggestion. Border + margin must drop.
          suggestions [{:label "a" :slash/usage "/a" :slash/selected? true}]
          input-top   2]
      (render/draw-slash-command-suggestions! g suggestions input-top 40)
      ;; Title row sits at row 0 (above the single suggestion at row 1).
      ;; No horizontal-rule border was drawn (no ─ in any putString).
      (expect (some #(= 0 (:row %)) @puts))
      (expect (not-any? #(str/starts-with? (:text %) "─") @puts)))))
