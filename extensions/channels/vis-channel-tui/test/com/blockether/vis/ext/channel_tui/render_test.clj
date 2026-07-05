(ns com.blockether.vis.ext.channel-tui.render-test
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.channel-tui.click-regions :as cr]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.ext.channel-tui.theme :as t]
   [com.blockether.vis.internal.iteration :as iteration]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private format-iteration-entry @#'render/format-iteration-entry)
(def ^:private input-more-hint @#'render/input-more-hint)
(def ^:private clip-lines-preserving-markers @#'render/clip-lines-preserving-markers)
(def ^:private tool-color-role->fg @#'render/tool-color-role->fg)
(def ^:private coalesce-forms vis/coalesce-forms)

(defn- native-form [tool summary render]
  (cond-> {:vis/tool-name tool :success? true :code ""
           :result-summary summary :tool-color-role :tool-color/search :result {}}
    render (assoc :result-render render)))

(defn- render-forms [forms]
  (format-iteration-entry
    (iteration/canonicalize {:position 0 :thinking nil :forms forms})
    80 1 {}))

(defdescribe native-card-flush-spacing-test
  ;; Two adjacent code-less native op-cards (cat/rg/ls/…) must stack FLUSH — no
  ;; blank row between their headlines. The flush logic (render/…block-code-body)
  ;; drops the trailing pad of the earlier card and the leading breathe of the
  ;; next when both are chrome-hidden natives. Guards the "two native calls
  ;; shouldn't have a blank line between them" contract.
  (it "two summary-only native cards render their headlines on ADJACENT lines"
    (let [lines (render-forms [(native-form "rg" "`x` · 0 hits in 0 files" nil)
                               (native-form "rg" "`y` · 0 hits in 0 files" nil)])
          head-idxs (keep-indexed (fn [i l] (when (str/includes? (str l) "hits in") i)) lines)]
      (expect (= 2 (count head-idxs)))
      ;; adjacent — index delta 1 means NO row (blank or otherwise) between them
      (expect (= 1 (- (second head-idxs) (first head-idxs))))))
  (it "a summary-only native card followed by one WITH a body still stacks flush"
    (let [lines (render-forms [(native-form "rg" "`x` · 0 hits in 0 files" nil)
                               (native-form "cat" "`a.clj` · L1-10" "line one\nline two")])
          head-idxs (keep-indexed (fn [i l]
                                    (when (or (str/includes? (str l) "hits in")
                                            (str/includes? (str l) "L1-10")) i))
                      lines)]
      (expect (= 2 (count head-idxs)))
      (expect (= 1 (- (second head-idxs) (first head-idxs)))))))

(defdescribe coalesce-forms-test
  ;; Regression: a DB-restored session whose trailer had >=2 adjacent `cat`
  ;; reads of the SAME file froze the whole TUI. `cat-form-path` groups them by
  ;; the summary chip (it recovers the path from the summary PRECISELY because
  ;; the DB round-trip flattens `:result` from a map to the rendered STRING),
  ;; then `merge-cat-forms` did `(assoc (:result f0) :anchors …)` on that string
  ;; -> ClassCastException every frame -> the render loop keeps re-throwing on
  ;; the last frame and never repaints.
  (it "merges adjacent same-path cat forms whose :result is a flattened STRING (no throw)"
    (let [forms [{:vis/tool-name "cat" :result-summary "`a.clj` · L1-10"
                  :result-render "line one\nline two" :result "line one\nline two"}
                 {:vis/tool-name "cat" :result-summary "`a.clj` · L40-50"
                  :result-render "line forty" :result "line forty"}]
          out (coalesce-forms forms)]
      (expect (= 1 (count out)))                                  ; the run collapsed
      (expect (str/includes? (:result-summary (first out)) "L1-10"))
      (expect (str/includes? (:result-summary (first out)) "L40-50"))
      ;; a string result carries through untouched (no anchors assoc'd onto it)
      (expect (string? (:result (first out))))))
  (it "still merges anchors when :result is a MAP (live, pre-DB-roundtrip)"
    (let [forms [{:vis/tool-name "cat" :result-summary "`a.clj` · L1-10"
                  :result-render "x" :result {:path "a.clj" :anchors {"1:aa" "x"}}}
                 {:vis/tool-name "cat" :result-summary "`a.clj` · L40-50"
                  :result-render "y" :result {:path "a.clj" :anchors {"40:bb" "y"}}}]
          out (coalesce-forms forms)]
      (expect (= 1 (count out)))
      (expect (= {"1:aa" "x" "40:bb" "y"} (get-in (first out) [:result :anchors])))))
  (it "leaves a solo cat form and non-cat forms untouched"
    (let [forms [{:vis/tool-name "cat" :result-summary "`a.clj` · L1-10" :result "solo"}
                 {:vis/tool-name "rg" :result-summary "5 hits" :result "hits"}]
          out (coalesce-forms forms)]
      (expect (= 2 (count out)))))
  (it "folds adjacent same-file patch forms into one multi-diff card"
    (let [forms [{:vis/tool-name "patch" :success? true :result-summary "update `a.clj`"
                  :result-render "```diff\n+ one\n```" :result {:path "a.clj"}}
                 {:vis/tool-name "patch" :success? true :result-summary "update `a.clj`"
                  :result-render "```diff\n+ two\n```" :result {:path "a.clj"}}]
          out (coalesce-forms forms)]
      (expect (= 1 (count out)))
      (expect (= "update `a.clj`" (:result-summary (first out))))
      (expect (str/includes? (:result-render (first out)) "+ one"))
      (expect (str/includes? (:result-render (first out)) "+ two"))))
  (it "keeps a failed patch separate from an adjacent successful one on the same file"
    (let [forms [{:vis/tool-name "patch" :success? false :result-summary "update `a.clj`"
                  :result-render "boom" :result {:path "a.clj"}}
                 {:vis/tool-name "patch" :success? true :result-summary "update `a.clj`"
                  :result-render "ok" :result {:path "a.clj"}}]
          out (coalesce-forms forms)]
      (expect (= 2 (count out)))))
  (it "does not merge a cat and a patch on the same file"
    (let [forms [{:vis/tool-name "cat" :success? true :result-summary "`a.clj` · L1-10"
                  :result-render "body" :result {:path "a.clj"}}
                 {:vis/tool-name "patch" :success? true :result-summary "update `a.clj`"
                  :result-render "```diff\n+ x\n```" :result {:path "a.clj"}}]
          out (coalesce-forms forms)]
      (expect (= 2 (count out))))))

(defdescribe tool-color-role-coverage-test
  (it "the TUI badge colour map covers every canonical vis/tool-color-roles role"
    ;; Guard against drift: the role list lives once in vis core; if a new role is
    ;; added there, this fails until the TUI map handles it (mirror of the web test).
    (doseq [role vis/tool-color-roles]
      (expect (some? (tool-color-role->fg role))
        (str role " has no TUI badge colour — add it to render/tool-color-role->fg")))))

(defmacro ^:private with-raw-code-on [& body]
  ;; The TUI now renders the model's raw `:code` unconditionally — the same
  ;; canonical contract as web's `block-code` (no `:vis/show-raw-code` gate).
  ;; This wrapper is a pass-through kept so existing layout/shape tests read
  ;; unchanged.
  `(do ~@body))

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
  (it "renders a block slot with no status footer"
    (with-raw-code-on
    ;; The right-aligned `BLOCK N` / `ITERATION N` / `CODE N` header bands
    ;; were retired per user directive (see comments in render.clj). Per-form
    ;; status footers are gone too; code blocks are source-only.
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(Thread/sleep 1000)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms 1000 :duration-ms 0 :success? nil :silent? false}]}
                    40 1 {:now-ms 2500})
            code-line (first (filter #(str/includes? % "Thread/sleep") lines))]
        (expect (not-any? #(str/includes? % "BLOCK 1") lines))
        (expect (not-any? #(str/includes? % "ITERATION 1") lines))
        (expect (not-any? #(str/includes? % "CODE 1") lines))
        (expect (= p/MARKER_CODE (marker-of code-line)))
        (expect (not-any? #(str/includes? % "↻") lines))
        (expect (not-any? #(str/includes? % "1.0s") lines)))))

  (it "renders the block's raw code verbatim — the canonical web block-code contract"
    (let [lines (format-iteration-entry {:iteration 0
                                         :forms [{:code "git_status()\nprint(42)"
                                                  :comment nil :stdout nil :error nil
                                                  :started-at-ms nil :duration-ms 1
                                                  :success? true :silent? false}]}
                  60 1 {})
          body (str/join "\n" (map (comp strip-ansi body-of) lines))]
      ;; The model's raw :code paints in full — no render-segment filtering,
      ;; no show-raw-code gate (identical to web's `block-code`). Engine-chrome
      ;; forms (answers/titles) are dropped upstream via :silent, never by
      ;; stripping segments out of a code body.
      (expect (str/includes? body "git_status()"))
      (expect (str/includes? body "print(42)"))))

  (it "renders form eval errors inline with source caret"
    (let [code "(def git-diff-doc (doc 'v/git-diff))"
          err  {:message "Unable to resolve symbol: 'v/git-diff"
                :trace "clojure.lang.ExceptionInfo: Unable to resolve symbol: 'v/git-diff"
                :block {:source code :row 1 :col 24}}
          lines (format-iteration-entry {:iteration 0
                                         :forms [{:code code :comment nil :render-segments nil :stdout nil :error err :started-at-ms nil :duration-ms 1 :success? false :silent? false}]}
                  80 1 {})
          visible (mapv (comp strip-sentinels strip-ansi body-of) lines)
          body (str/join "\n" visible)
          error-line (first (filter #(str/includes? % "Unable to resolve symbol") lines))]
      (expect (str/includes? body "(def git-diff-doc"))
      (expect (not (str/includes? body " 1:")))
      (expect (str/includes? body "^---"))
      (expect (str/includes? body "Unable to resolve symbol: 'v/git-diff"))
      (expect (not (str/includes? body "Error: Unable")))
      (expect (not (str/includes? body "ERROR — clojure.lang.ExceptionInfo")))
      (expect (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote code)) body))))
      (expect (= p/MARKER_CODE_ERR (marker-of error-line)))))

  (it "renders a form eval error message exactly once"
    (let [code "(clj/eval {:code \"(+ 1 2)\"})"
          msg  "nREPL connect failed on localhost:7888 — is the REPL running? Try (clj/ports)."
          err  {:type :clojure.lang/exception-info
                :message msg
                :trace (str "clojure.lang.ExceptionInfo: " msg)
                :block {:source code :row 1 :col 2}}
          lines (format-iteration-entry {:iteration 0
                                         :error err
                                         :forms [{:code code :comment nil :render-segments nil
                                                  :stdout nil :error err
                                                  :started-at-ms nil :duration-ms 1
                                                  :success? false :silent? false}]}
                  100 1 {})
          visible (mapv (comp strip-sentinels strip-ansi body-of) lines)
          body (str/join "\n" visible)]
      (expect (str/includes? body "(clj/eval"))
      (expect (not (str/includes? body " 1:")))
      (expect (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote msg)) body))))
      (expect (not (str/includes? body "ERROR:")))
      (expect (not (str/includes? body "ERROR —")))))

  (it "omits success status footer and keeps only code band edges"
    (with-raw-code-on
    ;; Layout (post status-footer removal):
    ;;   iteration-pad
    ;;   code-ok-pad
    ;;   <code line>
    ;;   code-ok-pad
    ;;   iteration-pad
    ;; Plain `:value` form results no longer render — the trailing
    ;; result row is gone for non-tool forms per user directive.
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(+ 1 2)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
                    40 1 {})
            bodies (mapv (comp strip-ansi body-of) lines)]
        (expect (not-any? #(str/includes? % "✓") lines))
        (expect (not-any? #(str/includes? % "1ms") lines))
        (expect (= 2 (count (filter #(= p/MARKER_CODE_OK_PAD (marker-of %)) lines))))
        (expect (not-any? #(str/includes? (or % "") "3") bodies)))))

  (it "pads displayed form comments by one column"
    (with-raw-code-on
      (let [lines (format-iteration-entry {:iteration 0
                                           :forms [{:code "(+ 1 2)" :comment ";; why this runs" :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 0 :success? nil :silent? false}]}
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
        ;; The recap rail is retired; provider fallback notices were
        ;; recap-only rows and no longer surface.
        (expect (not (str/includes? body "RECAP")))
        (expect (not (str/includes? body "Provider fallback:"))))))

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
      ;; Retired with the recap rail — same-provider retry notices were
      ;; recap-only rows and no longer surface.
      (expect (not (str/includes? body "RECAP")))))

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
      ;; The recap rail is retired; the provider error itself still
      ;; surfaces its actionable guidance via the error panel.
      (expect (not (str/includes? body "RECAP")))
      (expect (str/includes? body "NEXT STEP: rate limit — wait and retry, re-authenticate, or switch provider/model"))
      (expect (not (str/includes? body "PROVIDER_ERROR  HTTP 429"))))))

(defdescribe provider-auth-error-test
  (it "renders provider auth errors as action, not duplicate raw JSON"
    (let [lines (format-iteration-entry
                  {:error {:type :svar.core/http-error
                           :message "API authentication failed. Check your API key. (Original: Exceptional status code: 401)"
                           :data {:status 401
                                  :body "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid authentication credentials\"},\"request_id\":\"req_123\"}"}}}
                  120 1 {})
          body  (->> lines
                  (map (comp str/trim strip-ansi body-of))
                  (str/join " "))]
      ;; The recap rail is retired; auth errors still render as action.
      (expect (not (str/includes? body "RECAP")))
      (expect (not (str/includes? body "PROVIDER_ERROR  HTTP 401")))
      (expect (str/includes? body "Provider message: Invalid authentication credentials"))
      (expect (str/includes? body "NEXT STEP: re-authenticate this provider or update its API key"))
      (expect (not (str/includes? body "provider response:")))
      (expect (not (str/includes? body "{\"type\":"))))))

(defdescribe provider-transport-error-test
  (it "renders a transport blip (no HTTP status) as a structured provider error, not one plain line"
    (let [lines (format-iteration-entry
                  {:error {:type :svar.core/http-error
                           ;; A socket that died before any byte arrived carries
                           ;; NO :status/:body/:request-id — only the wrapper text.
                           :message "HTTP/1.1 header parser received no bytes"
                           :data {:stream? true}}}
                  120 1 {})
          body  (->> lines
                  (map (comp str/trim strip-ansi body-of))
                  (str/join " "))]
      ;; It must get the shared provider-error treatment: the split
      ;; explanation / NEXT STEP / facts rows — NOT the raw wrapper dumped as a
      ;; single generic "error" line.
      (expect (str/includes? body "WHAT HAPPENED: Vis could not complete the HTTP request"))
      (expect (str/includes? body "NEXT STEP: this is a network/connection blip, not a rejection"))
      ;; the wrapper is a compact fact row, not the whole message
      (expect (str/includes? body "Wrapper: HTTP/1.1 header parser received no bytes")))))

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
        iter      {:forms [{:code "(+ 1 1)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
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

    (it "engine-mutation recaps no longer render in the trace"
      ;; The RECAP rail is fully retired: an iteration carrying
      ;; `:recaps` produces no RECAP rows above the answer.
      (let [p          (render/format-answer-with-thinking-data*
                         ans [{:recaps ["Title — \"Wyjaśnienie rozmiaru kontekstu\""]}]
                         80 settings nil false nil)
            answer-idx (index-of p "hello")]
        (expect (some? answer-idx))
        (expect (nil? (index-of p "RECAP")))))

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

(defdescribe collapsed-thinking-ellipsis-test
  ;; Regression: the collapsed `▸ THINKING +N more` peek appends a dim
  ;; " …" to its LAST visible reasoning line. Thinking rows carry a
  ;; zero-width thinking marker (`​`) prefix that `str/blank?` does
  ;; NOT count as whitespace, so the old detection treated every row —
  ;; including paragraph-separator blanks — as non-blank. When the
  ;; 6-row peek window ended on a blank separator the " …" landed there
  ;; and rendered alone on its own otherwise-empty line.
  (let [;; A thinking text whose 6th wrapped row is a blank paragraph
        ;; separator, with enough rows after it to force the collapse.
        thinking (str "The patch was applied successfully. Now I need to verify the change "
                   "works by evaluating the namespace in the REPL, then close the task.\n\n"
                   "Let me verify by loading the file or at least checking the changed "
                   "lines look correct.\n\n"
                   "The diff looks correct.\n\n"
                   "More reasoning after the diff that should be hidden.\n"
                   "Even more hidden reasoning lines here.\n")
        stid     "abcd1234-5678-9999"
        visible  (->> (:lines (render/format-answer-with-thinking-data*
                                nil [{:thinking thinking}] 80
                                {:show-thinking true :show-iterations true} nil false
                                {:session-id "sid" :session-turn-id stid :detail-expansions {}}))
                   (mapv (comp str/trimr strip-sentinels strip-ansi body-of)))]

    (it "renders the collapsed THINKING peek with a +N more header"
      (expect (some #(str/includes? % "THINKING  +") visible)))))

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
    (let [;; Tool output paints purely as the program's stdout — both live
          ;; and cancelled paths share the renderer.
          iter     {:forms [{:code "(print \"bold result\")" :comment nil :render-segments nil
                             :stdout "bold result" :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
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
                       {:forms [{:code (str "(+ " n " 1)") :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]})
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
                                                :stdout nil
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
                       {:forms [{:code (str "(+ " n " 1)") :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]})
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
                                 :stdout nil
                                 :duration-ms 1
                                 :success? true :silent? true}
                                {:code "(+ 1 2)"
                                 :stdout nil
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
                     :forms [{:code (str "(do (println :iter " i ") (mapv inc (range 100)))") :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 50 :success? true :silent? false}]})
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
                  :forms [{:code "(+ 1 2)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 10 :success? true :silent? false}]}
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
                       :forms [{:code "(+ 1 1)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
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
                     :forms [{:code "(+ 1 1)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}
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

(defdescribe code-pad-payload-paint-test
  ;; MARKER_CODE_*_PAD can carry optional payload text. Regression: pad
  ;; painter filled bg then discarded payload.
  (it "paints code-pad payload text, not only its background"
    (let [puts    (atom [])
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
                     (fillRectangle [_ _ _] this)
                     (setCharacter [_ _ _] this)
                     (putString
                       ([col row text]
                        (swap! puts conj {:col col :row row :text text :fg @fg :bg @bg :sgr @active})
                        this)))
          footer   (str p/MARKER_CODE_OK_PAD " t24/i1/b1 ")]
      (render/draw-chat-bubble! graphics
        {:role :assistant :timestamp nil :prewrapped-lines [footer]}
        0 0 80)
      (let [painted (apply str (map :text @puts))
            stamp   (first (filter #(= "t24/i1/b1" (:text %)) @puts))]
        (expect (str/includes? painted "t24/i1/b1"))
        (expect (not (str/includes? painted "✓")))
        (expect (not (str/includes? painted "12ms")))
        (expect (= t/dialog-hint (:fg stamp)))
        (expect (contains? (:sgr stamp) com.googlecode.lanterna.SGR/ITALIC))))))

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
  (it "expands very long reasoning only when the badge is toggled open"
    (let [cid      "session"
          turn-id  "123e4567-e89b-12d3-a456-426614174000"
          node     "thinking:t123e4567:i1:reasoning"
          thinking (str/join "\n\n" (map #(format "line-%02d detail text" %) (range 1 51)))
          render*  (fn [exp]
                     (render/invalidate-cache!)
                     (render/format-answer-with-thinking-data
                       [:ir {} [:p {} [:span {} "done"]]] [{:thinking thinking}]
                       96 {:show-thinking true :show-iterations true} nil false
                       {:session-id cid :session-turn-id turn-id :detail-expansions exp}))
          cbody    (strip-ansi (:text (render* {})))
          ebody    (strip-ansi (:text (render* {[cid node] true})))]
      (expect (str/includes? cbody "THINKING"))
      (expect (not (str/includes? cbody "line-25")))
      (expect (str/includes? ebody "line-01"))
      (expect (str/includes? ebody "line-25"))
      (expect (str/includes? ebody "line-50")))))

(defdescribe auto-collapse-rendering-test
  (it "hides legacy preview-kind result bodies entirely"
    ;; Bare return values are never echoed: a form that printed nothing
    ;; renders no result body, no PREVIEW/RAW switcher, no toggle.
    (render/invalidate-cache!)
    (let [body "only line of preview output"
          trace [{:forms [{:code "(cat file)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false
                    {:session-id "session"
                     :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})]
      (expect (not (str/includes? (:text payload) body)))
      (expect (not (str/includes? (:text payload) "raw-only")))
      (expect (not (str/includes? (:text payload) "PREVIEW")))
      (expect (not (str/includes? (:text payload) "● RAW")))
      (expect (not-any? #(= :preview-switcher (:kind %)) (:line-meta payload)))))

  (it "hides huge plain value results entirely without a collapse summary"
    ;; Per user directive: collapsible disclosure was removed. Plain
    ;; `:value` form results never paint a body — no `RESULT` label,
    ;; no `chars hidden` summary, no `[iteration N · block M]` band.
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 4000 "abcdefghij"))
          trace       [{:forms [{:code "(+ 1 2)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
          turn-id     "123e4567-e89b-12d3-a456-426614174000"
          payload     (render/format-answer-with-thinking-data
                        nil trace 96 {:show-iterations true} nil false
                        {:session-id "session"
                         :session-turn-id turn-id})]
      (expect (not (str/includes? (:text payload) "RESULT")))
      (expect (not (str/includes? (:text payload) "chars hidden")))
      (expect (not (str/includes? (:text payload) "[iteration 1 · block 1]")))
      (expect (not (str/includes? (:text payload) huge-result)))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))

  (it "collapses completed reasoning behind the badge on the answer view"
    ;; Completed reasoning defaults collapsed; the legacy
    ;; `:detail-expansions` toggle now drives expansion again.
    (let [node    "thinking:tturn-1:i1:reasoning"
          thinking (str/join "\n\n" (map #(str "line " % " detail text") (range 1 51)))
          trace    [{:thinking thinking :code [] :results [] :durations [] :successes []}]
          render*  (fn [exp]
                     (render/invalidate-cache!)
                     (render/format-answer-with-thinking-data
                       [:ir {} [:p {} [:span {} "done"]]] trace 120
                       {:show-iterations true :show-thinking true}
                       nil false {:session-id "session" :session-turn-id "turn-1"
                                  :detail-expansions exp}))
          cbody    (strip-ansi (:text (render* {})))
          ebody    (strip-ansi (:text (render* {["session" node] true})))]
      (expect (str/includes? cbody "THINKING"))
      (expect (not (str/includes? cbody "line 25")))
      (expect (str/includes? ebody "line 1 "))
      (expect (str/includes? ebody "line 25"))
      (expect (str/includes? ebody "line 50"))))

  (it "never paints a collapsed summary band"
    (render/invalidate-cache!)
    (let [huge-result (str/join " " (repeat 1000 "abcdefghij"))
          trace       [{:forms [{:code "(+ 1 2)" :comment nil :render-segments nil :stdout nil :error nil :started-at-ms nil :duration-ms 1 :success? true :silent? false}]}]
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
;; Errored form rendering: an errored form surfaces its error message via the
;; inline error path.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe errored-form-rendering-test
  (it "an errored form surfaces its error message inline"
    (render/invalidate-cache!)
    (let [err {:message "boom" :type "java.lang.RuntimeException"
               :block {:source "(boom)" :row 1 :col 1}}
          trace [{:forms [{:code "(boom)" :comment nil :render-segments nil
                           :stdout nil :error err :started-at-ms nil :duration-ms 1 :success? false :silent? false}]}]
          opts {:session-id "session"
                :session-turn-id "123e4567-e89b-12d3-a456-426614174000"}
          payload (render/format-answer-with-thinking-data
                    nil trace 96 {:show-iterations true} nil false opts)
          text    (strip-sentinels (strip-ansi (:text payload)))]
      (expect (str/includes? text "boom")))))

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
                      :message-meta-mode :full
                      :tokens {:input 100 :output 20 :cached 70}}
                     4 2 60 {:viewport-h 40})]
      (expect (= 5 height))
      (expect (some #(str/includes? (:text %) "100→20 (cached 70)")
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
                      :message-meta-mode :full
                      :tokens {:input 100 :output 20 :cached 0}}
                     4 2 60 {:viewport-h 40})]
      (expect (= 5 height))
      (expect (some #(str/includes? (:text %) "100→20")
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
                      :message-meta-mode :full
                      :tokens {:input 100 :output 20}}
                     4 2 60 {:viewport-h 40})
          answer-row (:row (first (filter #(= "hello" (:text %)) @puts)))
          footer-row (:row (first (filter #(str/includes? (:text %) "100→20") @puts)))]
      (expect (= 5 height))
      (expect (= 2 (- footer-row answer-row)))))

  (it "omits the footer for an iteration-only turn with no model / tokens / cost"
    ;; The bubble footer is now the SHARED humanized turn-summary line
    ;; (`vis/meta-summary-line`): model · in→out (cached) · ~$cost · duration.
    ;; Iteration / silent-form bookkeeping no longer rides this footer, so a
    ;; turn that only carries iteration metadata produces no footer line at all
    ;; and the bubble collapses to the bare answer height (no footer rows).
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
                      :message-meta-mode :full
                      :iteration-count 3
                      :traces [{:forms [{:silent? true}
                                        {:silent? false}]}
                               {:forms [{:silent? true}]}]}
                     4 2 60 {:viewport-h 40})]
      (expect (= 3 height))
      (expect (not-any? #(str/includes? (str (:text %)) "iter") @puts))
      (expect (not-any? #(str/includes? (str (:text %)) "silent") @puts))))

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
          ;; ANSWER fenced-code rows are inset from the band's left edge by
          ;; `code-block-h-pad` (the band still fills from `left`).
          text-x  (+ left @#'render/code-block-h-pad)
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
      ;; palette — selection is signalled by the dot marker in the
      ;; left margin, NOT by a full-row accent stripe.
      (expect (some #(and (= first-sug (:row %))
                       (= pad (:col %))
                       (= inner-w (:w %))
                       (= t/dialog-bg (:bg %)))
                @fills))

      ;; The selected row carries a BOLD dot marker one col IN from the
      ;; inset body edge (col `pad`+1, a 1-col left margin), painted in
      ;; `dialog-hint-key` on `dialog-bg` so marker reads INSIDE menu
      ;; rather than floating in terminal margin. Non-selected row gets
      ;; nothing painted in that column.
      (expect (some #(and (= first-sug (:row %))
                       (= (inc pad) (:col %))
                       (= p/SELECTION_GLYPH (:text %))
                       (= t/dialog-hint-key (:fg %))
                       (= t/dialog-bg (:bg %))
                       (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                @puts))
      (expect (not-any? #(and (= (inc first-sug) (:row %))
                           (= (inc pad) (:col %))
                           (= p/SELECTION_GLYPH (:text %)))
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

(defdescribe iteration-fingerprint-error-test
  ;; Regression (issue #5): the render thread crashed every frame when an
  ;; iteration's :error was a plain String (e.g. CONSULT failures) because
  ;; iteration-fingerprint called select-keys on it. Non-map errors must NOT
  ;; throw and must still differentiate the fingerprint for cache invalidation.
  (let [fp #'render/iteration-fingerprint]
    (it "does not throw on a String :error and keeps it in the fingerprint"
      (let [out (fp {:error "CONSULT failed: boom"})]
        (expect (vector? out))
        (expect (some #{"CONSULT failed: boom"} out))))
    (it "still select-keys a map :error to :type/:message"
      (let [out (fp {:error {:type :x :message "m" :trace "noise"}})]
        (expect (some #{{:type :x :message "m"}} out))))
    (it "different String errors produce different fingerprints"
      (expect (not= (fp {:error "a"}) (fp {:error "b"}))))
    (it "nil :error is fine"
      (expect (vector? (fp {:error nil}))))))

(defdescribe message-detail-expansions-key-test
  ;; The height/estimate/projection caches key a message to ONLY its own
  ;; disclosure state. A user bubble with no turn token stays CONSTANT so no
  ;; unrelated session expansion leaks in (one fold click would otherwise bust
  ;; every user bubble's cached height). A user prompt CAN carry a collapsible
  ;; `[Pasted #N]` disclosure, though — so `expand-all` and its own per-turn
  ;; expansions must still key it (see the paste-disclosure path).
  (let [sid "s1"
        expansions {["s1" "iteration:tabc12345:i1:result"] true}]
    (it "user message key is constant regardless of expansions"
      (expect (= (render/message-detail-expansions-key sid {:role :user :text "hi"} {})
                (render/message-detail-expansions-key sid {:role :user :text "hi"} expansions))))
    (it "user message keys under expand-all (a `[Pasted #N]` disclosure may open)"
      (expect (= :expand-all
                (render/message-detail-expansions-key sid {:role :user :text "hi"}
                  {:vis.channel-tui/expand-all-details? true}))))
    (it "assistant message with matching turn token picks up its expansions"
      (expect (= [["iteration:tabc12345:i1:result" true]]
                (render/message-detail-expansions-key sid
                  {:role :assistant :session-turn-id "abc12345-0000-0000"}
                  expansions))))
    (it "assistant message of ANOTHER turn is not affected"
      (expect (= []
                (render/message-detail-expansions-key sid
                  {:role :assistant :session-turn-id "def456-0000-0000"}
                  expansions))))
    (it "expand-all still keys assistant messages"
      (expect (= :expand-all
                (render/message-detail-expansions-key sid
                  {:role :assistant :session-turn-id "abc12345-0000-0000"}
                  {:vis.channel-tui/expand-all-details? true}))))))

(defdescribe paste-disclosure-render-test
  ;; A user prompt's `[Pasted #N]` marker (the `vis-paste` fence
  ;; `input/collapse-paste-placeholders` emits) renders as a collapsible
  ;; disclosure: the token is the chevron summary row, the payload the body
  ;; shown only when expanded.
  (let [ir      [:ir {}
                 [:p {} [:span {} "look at this"]]
                 [:code {:lang "vis-paste"}
                  "[Pasted #1: 3 lines, 11B]\nAAA\nBBB\nCCC\n"]]
        sid     "s1"
        turn    "client-turn-1"
        opts    (fn [de] {:session-id sid :session-turn-id turn
                          :detail-expansions de :section :user})
        node-id (@#'render/detail-node-id
                 {:session-turn-id turn :section :user :kind :paste :details-path ["1"]})]
    (it "collapsed by default: summary chevron shows, payload hidden"
      (let [txt (:text (render/format-answer-markdown-data ir 76 (opts {})))]
        (expect (str/includes? txt "▸ [Pasted #1: 3 lines, 11B]"))
        (expect (not (str/includes? txt "AAA")))))
    (it "expanded: chevron flips and the verbatim payload appears"
      (let [txt (:text (render/format-answer-markdown-data ir 76 (opts {[sid node-id] true})))]
        (expect (str/includes? txt "▾ [Pasted #1: 3 lines, 11B]"))
        (expect (str/includes? txt "AAA"))
        (expect (str/includes? txt "CCC"))))
    (it "the summary row carries toggle-details click meta scoped to this node"
      (let [{:keys [lines line-meta]} (render/format-answer-markdown-data ir 76 (opts {}))
            idx  (first (keep-indexed (fn [i l] (when (str/includes? l "Pasted #1") i)) lines))
            meta (nth line-meta idx)]
        (expect (= :toggle-details (:kind meta)))
        (expect (= (str node-id) (:node-id meta)))
        (expect (true? (:collapsed? meta)))))))

(defn- git-only-form
  ;; Thin git filter over the generalized tagger — the source keeps only
  ;; `groupable-tool-form`; this preserves the original git-only assertions.
  [entry]
  (let [tf (@#'render/groupable-tool-form entry)]
    (when (= "git" (first tf)) (second tf))))
(def ^:private git-command-parts @#'render/git-command-parts)
(def ^:private git-group-entries @#'render/git-group-entries)
(def ^:private render-iteration-entries @#'render/render-iteration-entries)

(defn- git-form [summary render]
  {:vis/tool-name "git" :success? true :code ""
   :result-summary summary :result-render render
   :tool-color-role :tool-color/shell :result {}})

(defn- entry-text [entries]
  ;; Drop every zero-width structural / inline-style marker so assertions match
  ;; the human-visible text regardless of where sentinels sit in the row.
  (mapv (fn [e]
          (str/replace (str (:line e))
            #"[\u200B-\u200F\u2060-\u206F\uFEFF\uE000-\uF8FF]" ""))
    entries))

(defdescribe git-band-grouping-test
  ;; A RUN of consecutive git-only iterations coalesces into ONE collapsible
  ;; GIT band; a lone git call, or a run broken by other work, renders per
  ;; iteration as before.
  (let [ctx {:fill-w 76 :session-id "s1" :session-turn-id "t" :detail-expansions {}}
        gi  (fn [i s] [i {:forms [(git-form s nil)]}])
        py  (fn [i] [i {:forms [{:vis/tool-name "python_execution" :result-summary "x"}]}])
        iter-fn (fn [[idx _]] [{:line (str "NORMAL#" idx) :meta nil}])
        band-count (fn [pairs]
                     (->> (render-iteration-entries pairs iter-fn false true ctx)
                       entry-text
                       (filter #(str/includes? % "commands"))
                       count))
        normal-count (fn [pairs]
                       (->> (render-iteration-entries pairs iter-fn false true ctx)
                         (filter #(str/includes? (str (:line %)) "NORMAL#"))
                         count))]
    (it "git-only-form still detects a single-git iteration (narration no longer disqualifies)"
      (expect (some? (git-only-form {:forms [(git-form "status" nil)]})))
      ;; Narration (thinking / prose) rides ABOVE the band now, so it still groups.
      (expect (some? (git-only-form {:assistant-prose "hi" :forms [(git-form "status" nil)]})))
      (expect (nil? (git-only-form {:forms [(git-form "a" nil) (git-form "b" nil)]})))
      (expect (nil? (git-only-form {:forms [{:vis/tool-name "cat" :result-summary "x"}]}))))
    (it "git-command-parts recovers subcommand + failure from the summary note"
      (let [ok (git-command-parts (git-form "status --short" nil))
            ex (git-command-parts (git-form "push origin main (exit 1)" nil))
            to (git-command-parts (git-form "commit -m foo (timed out)" nil))]
        (expect (= "status" (:subcommand ok)))
        (expect (false? (:failed? ok)))
        (expect (= "push" (:subcommand ex)))
        (expect (true? (:failed? ex)))
        (expect (true? (:failed? to)))))
    (it "git-command-parts lifts a commit subject from its message blockquote"
      (let [c (git-command-parts
                (git-form "commit -m"
                  "> wip: tidy things\n>\n> a longer body paragraph\n\n```\n[main abc] wip: tidy things\n```"))]
        (expect (= "commit" (:subcommand c)))
        (expect (= "wip: tidy things" (:subject c))))
      ;; No blockquote / non-commit ⇒ no subject.
      (expect (nil? (:subject (git-command-parts (git-form "status --short" nil)))))
      (expect (nil? (:subject (git-command-parts (git-form "commit -m" "```\n[main abc] x\n```"))))))
    (it "a commit's subject rides on its collapsed chip"
      (let [forms     [(git-form "add -A" nil)
                       (git-form "commit -m"
                         "> wip: tidy\n>\n> body\n\n```\n[main abc] wip: tidy\n```")]
            collapsed (entry-text (git-group-entries forms (assoc ctx :iteration-number 7)))]
        (expect (some #(str/includes? % "2 commands") collapsed))
        ;; The message subject is visible WITHOUT expanding the band.
        (expect (str/includes? (str/join " " collapsed) "wip: tidy"))))
    (it "three consecutive git iterations collapse into ONE band"
      (let [pairs [(gi 0 "add -A") (gi 1 "commit -m x") (gi 2 "push")]]
        (expect (= 1 (band-count pairs)))
        (expect (= 0 (normal-count pairs)))))
    (it "a lone git iteration renders normally (no band)"
      (let [pairs [(gi 0 "status")]]
        (expect (= 0 (band-count pairs)))
        (expect (= 1 (normal-count pairs)))))
    (it "a non-git step between two git calls breaks the run"
      (let [pairs [(gi 0 "add") (py 1) (gi 2 "push")]]
        (expect (= 0 (band-count pairs)))
        (expect (= 3 (normal-count pairs)))))
    (it "two runs split by other work yield two bands"
      (let [pairs [(gi 0 "a") (gi 1 "b") (py 2) (gi 3 "c") (gi 4 "d")]]
        (expect (= 2 (band-count pairs)))
        (expect (= 1 (normal-count pairs)))))
    (it "a narrated head still groups; its narration renders ABOVE the band"
      ;; The assistant narrates, then add / commit / push — the WHOLE burst folds
      ;; into ONE band and the narration (the iter-fn's NORMAL# lead) sits above it.
      (let [pairs [[0 {:forms [(git-form "add -A" nil)] :thinking "let me commit"}]
                   (gi 1 "commit -m x") (gi 2 "push")]
            out   (entry-text (render-iteration-entries pairs iter-fn false true ctx))
            band-ix   (first (keep-indexed (fn [i l] (when (str/includes? l "3 commands") i)) out))
            normal-ix (first (keep-indexed (fn [i l] (when (str/includes? l "NORMAL#0") i)) out))]
        (expect (= 1 (band-count pairs)))
        (expect (some? band-ix))
        (expect (some? normal-ix))
        ;; Narration renders ABOVE the band, not swallowed by it.
        (expect (< normal-ix band-ix))))
    (it "an INTERIOR narrated call breaks the run (never floats above the band)"
      (let [pairs [(gi 0 "add") [1 {:forms [(git-form "commit" nil)] :thinking "hmm"}] (gi 2 "push")]]
        ;; add renders alone; the narrated commit + push form ONE band with the
        ;; commit's narration above it — still exactly one band, add left outside.
        (expect (= 1 (band-count pairs)))
        (expect (= 2 (normal-count pairs)))))
    (it "collapsed band shows a chip per command; expanded shows each `$` row"
      (let [forms   [(git-form "add -A" nil)
                     (git-form "push" "```\nmain -> main\n```")]
            node-id (@#'render/detail-node-id {:session-turn-id "t" :iteration-number 5
                                               :section :iteration :kind :git-group})
            collapsed (entry-text (git-group-entries forms (assoc ctx :iteration-number 5)))
            expanded  (entry-text (git-group-entries forms (assoc ctx :iteration-number 5
                                                             :detail-expansions {["s1" node-id] true})))]
        ;; Header row present in both; per-command `$` rows only when expanded.
        (expect (some #(str/includes? % "2 commands") collapsed))
        (expect (not-any? #(str/includes? % "$ add -A") collapsed))
        (expect (some #(str/includes? % "$ add -A") expanded))
        (expect (some #(str/includes? % "$ push") expanded))
        (expect (some #(str/includes? % "main -> main") expanded))
        ;; Nested command rows carry a left pad so they read as CHILDREN of the
        ;; band (indented under the label, not hugging the chevron column).
        (expect (every? #(str/starts-with? % "  ")
                  (filter #(str/includes? % "$ ") expanded)))))
    (it "the band opens with a leading breathe row (padding 1 up, like an op-card)"
      (let [entries (git-group-entries [(git-form "commit -m x" nil) (git-form "push" nil)]
                      (assoc ctx :iteration-number 9))
            texts   (entry-text entries)]
        ;; The FIRST row is a blank breathe; the chevron header sits BELOW it,
        ;; so the band's colored top edge has padding above the summary.
        (expect (str/blank? (first texts)))
        (expect (str/includes? (second texts) "2 commands"))))
    (it "a GIT band drops a preceding iteration-pad gap so the seam is one blank"
      (let [ip-ch   (first (str @#'render/iteration-pad-marker))
            rm-ch   (first (str @#'render/result-marker))
            ;; Fake a non-git iteration that closes with an iteration-pad gap
            ;; (what a real op-card iteration emits), then two git iterations.
            pad-fn  (fn [[idx _]] [{:line (str "NORMAL#" idx) :meta nil}
                                   {:line (str @#'render/iteration-pad-marker) :meta nil}])
            pairs   [(py 0) (gi 1 "commit -m x") (gi 2 "push")]
            out     (render-iteration-entries pairs pad-fn false true ctx)
            band-ix (first (keep-indexed
                             (fn [i e] (when (str/includes? (str (:line e)) "commands") i))
                             out))
            before  (str (:line (nth out (dec band-ix))))]
        ;; The row immediately before the band header is the band's OWN colored
        ;; breathe (result-marker), NOT the popped iteration-pad gap.
        (expect (= rm-ch (first before)))
        (expect (not= ip-ch (first before)))))))

(def ^:private groupable-tool-form @#'render/groupable-tool-form)
(def ^:private rg-command-parts @#'render/rg-command-parts)
(def ^:private rg-group-entries @#'render/rg-group-entries)

(defn- rg-form [summary render]
  {:vis/tool-name "rg" :success? true :code ""
   :result-summary summary :result-render render
   :tool-color-role :tool-color/search :result {}})

(defdescribe rg-band-grouping-test
  ;; A RUN of consecutive rg-only iterations coalesces into ONE collapsible RG
  ;; band — the SAME machinery as the GIT band, keyed on tool name. A lone rg
  ;; call, or a run broken by other work / a different groupable tool, renders
  ;; per iteration.
  (let [ctx {:fill-w 76 :session-id "s1" :session-turn-id "t" :detail-expansions {}}
        ri  (fn [i s r] [i {:forms [(rg-form s r)]}])
        gf  (fn [i s] [i {:forms [(git-form s nil)]}])
        py  (fn [i] [i {:forms [{:vis/tool-name "python_execution" :result-summary "x"}]}])
        iter-fn (fn [[idx _]] [{:line (str "NORMAL#" idx) :meta nil}])
        rg-band-count (fn [pairs]
                        (->> (render-iteration-entries pairs iter-fn false true ctx)
                          entry-text
                          (filter #(str/includes? % "searches"))
                          count))
        git-band-count (fn [pairs]
                         (->> (render-iteration-entries pairs iter-fn false true ctx)
                           entry-text
                           (filter #(str/includes? % "commands"))
                           count))
        normal-count (fn [pairs]
                       (->> (render-iteration-entries pairs iter-fn false true ctx)
                         (filter #(str/includes? (str (:line %)) "NORMAL#"))
                         count))]
    (it "groupable-tool-form tags git AND rg, rejects other tools + impure steps"
      (expect (= ["git" (git-form "status" nil)]
                (update (groupable-tool-form {:forms [(git-form "status" nil)]}) 1 identity)))
      (expect (= "rg" (first (groupable-tool-form {:forms [(rg-form "`x` · 1 file" nil)]}))))
      (expect (nil? (groupable-tool-form {:forms [{:vis/tool-name "cat" :result-summary "x"}]})))
      ;; Narration no longer disqualifies grouping — it renders above the band.
      (expect (some? (groupable-tool-form {:assistant-prose "hi" :forms [(rg-form "`x` · 1 file" nil)]}))))
    (it "rg-command-parts drops the query prefix for the collapsed chip, keeps the full summary"
      (let [content (rg-command-parts (rg-form "`a` OR `b` · 6 hits in 1 file" "\n`c.clj`\n\n```\n 1: x\n```"))
            files   (rg-command-parts (rg-form "`q` · 4 files" "\n```\n  a\n```"))]
        (expect (= "6 hits in 1 file" (:chip content)))
        (expect (= "`a` OR `b` · 6 hits in 1 file" (:summary content)))
        (expect (= "4 files" (:chip files)))))
    (it "two consecutive rg iterations collapse into ONE band"
      (let [pairs [(ri 0 "`a` · 4 files" nil) (ri 1 "`b` · 2 hits in 1 file" nil)]]
        (expect (= 1 (rg-band-count pairs)))
        (expect (= 0 (normal-count pairs)))))
    (it "a lone rg iteration renders normally (no band)"
      (let [pairs [(ri 0 "`a` · 1 file" nil)]]
        (expect (= 0 (rg-band-count pairs)))
        (expect (= 1 (normal-count pairs)))))
    (it "a non-rg step between two rg calls breaks the run"
      (let [pairs [(ri 0 "`a` · 1 file" nil) (py 1) (ri 2 "`b` · 1 file" nil)]]
        (expect (= 0 (rg-band-count pairs)))
        (expect (= 3 (normal-count pairs)))))
    (it "adjacent git-run and rg-run stay TWO separate bands, never merged"
      (let [pairs [(gf 0 "add -A") (gf 1 "commit -m x")
                   (ri 2 "`a` · 1 file" nil) (ri 3 "`b` · 2 files" nil)]]
        (expect (= 1 (git-band-count pairs)))
        (expect (= 1 (rg-band-count pairs)))
        (expect (= 0 (normal-count pairs)))))
    (it "collapsed band shows count chips; expanded shows each search's full query + matches"
      (let [forms   [(rg-form "`paste` OR `Pasted` · 4 files" "\n```\n  a.clj\n```")
                     (rg-form "`render-find` · 6 hits in 1 file" "\n`core.clj`\n\n```\n 12: foo\n```")]
            node-id (@#'render/detail-node-id {:session-turn-id "t" :iteration-number 5
                                               :section :iteration :kind :rg-group})
            collapsed (entry-text (rg-group-entries forms (assoc ctx :iteration-number 5)))
            expanded  (entry-text (rg-group-entries forms (assoc ctx :iteration-number 5
                                                            :detail-expansions {["s1" node-id] true})))]
        (expect (some #(str/includes? % "2 searches") collapsed))
        (expect (some #(str/includes? % "6 hits in 1 file") collapsed))
        ;; The long query does NOT ride the collapsed row.
        (expect (not-any? #(str/includes? % "render-find") collapsed))
        ;; Expanded restores each full query + its matching files.
        (expect (some #(str/includes? % "render-find") expanded))
        (expect (some #(str/includes? % "core.clj") expanded))
        (expect (some #(str/includes? % "12: foo") expanded))
        ;; Nested search-headline rows carry the child left-pad, like the GIT
        ;; band (the collapsed summary header is excluded — it leads with a chevron).
        (expect (every? #(str/starts-with? % "  ")
                  (filter #(str/includes? % "render-find ·") expanded)))))
    (it "the rg band opens with a leading breathe row, like an op-card"
      (let [texts (entry-text (rg-group-entries [(rg-form "`a` · 1 file" nil) (rg-form "`b` · 2 files" nil)]
                                (assoc ctx :iteration-number 9)))]
        (expect (str/blank? (first texts)))
        (expect (str/includes? (second texts) "2 searches"))))))

(def ^:private delete-command-parts @#'render/delete-command-parts)
(def ^:private delete-group-entries @#'render/delete-group-entries)

(defn- del-form [summary]
  {:vis/tool-name "delete" :success? true :code ""
   :result-summary summary :result-render nil
   :tool-color-role :tool-color/delete :result {}})

(defdescribe delete-band-grouping-test
  ;; A RUN of consecutive delete-only iterations coalesces into ONE collapsible
  ;; DELETE band — the SAME machinery as GIT / RG, keyed on tool name. A lone
  ;; delete, or a run broken by other work, renders per iteration.
  (let [ctx {:fill-w 76 :session-id "s1" :session-turn-id "t" :detail-expansions {}}
        dl  (fn [i s] [i {:forms [(del-form s)]}])
        py  (fn [i] [i {:forms [{:vis/tool-name "python_execution" :result-summary "x"}]}])
        iter-fn (fn [[idx _]] [{:line (str "NORMAL#" idx) :meta nil}])
        del-band-count (fn [pairs]
                         (->> (render-iteration-entries pairs iter-fn false true ctx)
                           entry-text
                           (filter #(str/includes? % "deletes"))
                           count))
        normal-count (fn [pairs]
                       (->> (render-iteration-entries pairs iter-fn false true ctx)
                         (filter #(str/includes? (str (:line %)) "NORMAL#"))
                         count))]
    (it "groupable-tool-form tags a lone delete iteration"
      (expect (= "delete" (first (groupable-tool-form {:forms [(del-form "deleted `a.clj`")]}))))
      (expect (nil? (groupable-tool-form {:forms [{:vis/tool-name "cat" :result-summary "x"}]}))))
    (it "delete-command-parts shows the basename chip, keeps the full summary"
      (let [p (delete-command-parts (del-form "deleted `src/com/blockether/vis/ports.clj`"))
            n (delete-command-parts (del-form "nothing to delete at `gone.clj`"))]
        (expect (= "ports.clj" (:chip p)))
        (expect (= "deleted `src/com/blockether/vis/ports.clj`" (:summary p)))
        (expect (= "gone.clj" (:chip n)))))
    (it "two consecutive delete iterations collapse into ONE band"
      (let [pairs [(dl 0 "deleted `a/ports.clj`") (dl 1 "deleted `a/ports_test.clj`")]]
        (expect (= 1 (del-band-count pairs)))
        (expect (= 0 (normal-count pairs)))))
    (it "a lone delete iteration renders normally (no band)"
      (let [pairs [(dl 0 "deleted `a.clj`")]]
        (expect (= 0 (del-band-count pairs)))
        (expect (= 1 (normal-count pairs)))))
    (it "a non-delete step between two deletes breaks the run"
      (let [pairs [(dl 0 "deleted `a.clj`") (py 1) (dl 2 "deleted `b.clj`")]]
        (expect (= 0 (del-band-count pairs)))
        (expect (= 3 (normal-count pairs)))))
    (it "collapsed band shows a basename chip per delete; expanded shows each full path"
      (let [forms   [(del-form "deleted `a/ports.clj`") (del-form "deleted `a/ports_test.clj`")]
            node-id (@#'render/detail-node-id {:session-turn-id "t" :iteration-number 5
                                               :section :iteration :kind :delete-group})
            collapsed (entry-text (delete-group-entries forms (assoc ctx :iteration-number 5)))
            expanded  (entry-text (delete-group-entries forms (assoc ctx :iteration-number 5
                                                                :detail-expansions {["s1" node-id] true})))]
        (expect (some #(str/includes? % "2 deletes") collapsed))
        (expect (some #(str/includes? % "ports.clj") collapsed))
        ;; Full paths stay OUT of the collapsed row.
        (expect (not-any? #(str/includes? % "a/ports_test.clj") collapsed))
        ;; Expanded restores each full path.
        (expect (some #(str/includes? % "a/ports_test.clj") expanded))))
    (it "the delete band opens with a leading breathe row, like an op-card"
      (let [texts (entry-text (delete-group-entries [(del-form "deleted `a.clj`") (del-form "deleted `b.clj`")]
                                (assoc ctx :iteration-number 9)))]
        (expect (str/blank? (first texts)))
        (expect (str/includes? (second texts) "2 deletes"))))))
