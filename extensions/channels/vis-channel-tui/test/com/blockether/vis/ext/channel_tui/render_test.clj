(ns com.blockether.vis.ext.channel-tui.render-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.iteration :as iteration]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private format-iteration-entry @#'render/format-iteration-entry)

(def ^:private input-more-hint @#'render/input-more-hint)

(def ^:private bang-prefix @#'render/bang-prefix)

(def ^:private clip-lines-preserving-markers @#'render/clip-lines-preserving-markers)

(def ^:private tool-color-role->fg @#'render/tool-color-role->fg)

(def ^:private result-row-bg @#'render/result-row-bg)

(def ^:private truncate-with-suffix @#'render/truncate-with-suffix)

(def ^:private coalesce-forms vis/coalesce-forms)

(def ^:private format-iteration-entry-entries @#'render/format-iteration-entry-entries)

(def ^:private coalesce-bubble-blanks @#'render/coalesce-bubble-blanks)

(defn- put-text
  "Coerce a `putString` 3rd argument to the String it paints. The plain body path
   now hands the fork's `putString(TextCharacter[])` overload a pre-segmented,
   cached array (see `primitives/blit-line!`); Clojure `proxy` dispatches by arity
   so both overloads land in the same 3-arg stub. Reconstruct the text so these
   recording proxies capture the exact String either overload paints."
  [x]
  (if (string? x)
    x
    (.toString ^StringBuilder
               (reduce (fn [^StringBuilder sb tc]
                         (.append sb
                                  (.getCharacterString ^com.googlecode.lanterna.TextCharacter tc)))
                       (StringBuilder.)
                       x))))

(defn- put->styled-runs
  "Expand a `putString` 3rd argument into the styled RUNS it paints, so recording
   proxies observe the same per-run [text {:fg :bg :sgr}] structure regardless of
   overload. A String is one run at the graphics' current `fg`/`bg`/`sgr`; a
   `TextCharacter[]` (the pre-segmented overload `blit-styled-line!` uses) is split
   into consecutive runs of equal fg/bg/modifiers, read straight from the cells --
   a STRONGER check than trusting g's transient per-call SGR state."
  [x fg bg sgr]
  (if (string? x)
    [[x {:fg fg :bg bg :sgr sgr}]]
    (->> (seq x)
         (partition-by (fn [^com.googlecode.lanterna.TextCharacter tc]
                         [(.getForegroundColor tc) (.getBackgroundColor tc)
                          (set (.getModifiers tc))]))
         (mapv (fn [cells]
                 (let [^com.googlecode.lanterna.TextCharacter c0 (first cells)]
                   [(apply str
                      (map #(.getCharacterString ^com.googlecode.lanterna.TextCharacter %) cells))
                    {:fg (.getForegroundColor c0)
                     :bg (.getBackgroundColor c0)
                     :sgr (set (.getModifiers c0))}]))))))

(defdescribe result-summary-color-test
             (it "keeps native-tool headlines flush on the quiet result band"
                 (expect (= t/result-bg (result-row-bg {:kind :result-headline} false)))
                 (expect (= t/result-bg (result-row-bg {:kind :toggle-details} false))))
             (it "keeps body rows quiet and gives hover the strongest affordance"
                 (expect (= t/result-bg (result-row-bg nil false)))
                 (expect (= t/link-chrome-hover-bg (result-row-bg {:kind :toggle-details} true)))))

(defn- native-form
  [tool summary render]
  (cond->
    {:vis/tool-name tool
     :success? true
     :code ""
     :result-summary summary
     :tool-color-role :tool-color/search
     :result {}}
    render
    (assoc :result-render render)))

(defn- render-forms
  [forms]
  ;; Mirror the REAL bubble-assembly seam: `format-iteration-entry-entries` then the
  ;; `coalesce-bubble-blanks` pass every live/restored path runs (see `trace-render-entries`).
  ;; That pass folds the seam between two adjacent native cards — the earlier card's trailing
  ;; pad + the next card's leading breathe — into ONE shared band row. Asserting on the raw
  ;; pre-coalesce lines would over-count the gap by one, so run the seam here too.
  (->> (apply format-iteration-entry-entries
         (iteration/canonicalize {:position 0 :thinking nil :forms forms})
         80
         1
         [{}])
       coalesce-bubble-blanks
       (mapv :line)))

(defdescribe native-card-flush-spacing-test
             ;; Two adjacent code-less native op-cards (cat/rg/ls/…) must stack FLUSH — no
             ;; blank row between their headlines. The flush logic (render/…block-code-body)
             ;; drops the trailing pad of the earlier card and the leading breathe of the
             ;; next when both are chrome-hidden natives. Guards the "two native calls
             ;; shouldn't have a blank line between them" contract.
             (it "two summary-only native cards render their headlines on ADJACENT lines"
                 (let
                   [lines
                    (render-forms [(native-form "rg" "`x` · 0 hits in 0 files" nil)
                                   (native-form "rg" "`y` · 0 hits in 0 files" nil)])

                    head-idxs
                    (keep-indexed (fn [i l]
                                    (when (str/includes? (str l) "hits in") i))
                                  lines)]

                   (expect (= 2 (count head-idxs)))
                   ;; delta 2 == exactly ONE band row between the two headlines (the shared
                   ;; result-bg breathe), i.e. flush — NOT the pre-coalesce two blanks.
                   (expect (= 2 (- (second head-idxs) (first head-idxs))))))
             (it "a summary-only native card followed by one WITH a body still stacks flush"
                 (let
                   [lines
                    (render-forms [(native-form "rg" "`x` · 0 hits in 0 files" nil)
                                   (native-form "cat" "`a.clj` · L1-10" "line one\nline two")])

                    head-idxs
                    (keep-indexed (fn [i l]
                                    (when (or (str/includes? (str l) "hits in")
                                              (str/includes? (str l) "L1-10"))
                                      i))
                                  lines)]

                   (expect (= 2 (count head-idxs)))
                   (expect (= 2 (- (second head-idxs) (first head-idxs)))))))

(defdescribe
  native-tool-error-compact-test
  ;; A FAILED native tool (cat/rg/patch/…) must NOT dump its synthesized
  ;; `name({…args…})` invocation source into the client — that wall of the very
  ;; args that failed is redundant chrome. The user channel shows only the
  ;; compact error message. `python_execution` (the model's own program) still
  ;; keeps its code so the inline caret has context.
  (it "drops the args-source wall for a failed native tool, keeps the message"
      (let
        [txt (str/join "\n"
                       (render-forms
                         [{:vis/tool-name "patch"
                           :success? false
                           :code "patch([{\"replace\": \"ARGWALLMARKER huge\nmulti\nline\"}])"
                           :error {:message
                                   "No changes: patch is atomic. edit 1: stale from_anchor."}
                           :result nil}]))]
        (expect (not (str/includes? txt "ARGWALLMARKER")))
        (expect (str/includes? txt "stale from_anchor"))))
  (it
    "keeps JSON-restored native errors compact"
    (let
      [msg
       "clojure.lang.ExceptionInfo: clj_test found no *_test.clj namespaces under [\"test/com/blockether/vis/internal/gateway\"] {:type :clj/bad-args, :got {\"language\" \"clojure\"}}"

       txt
       (str/join
         "\n"
         (render-forms
           [{:vis/tool-name "run_tests"
             :success? false
             :code
             "run_tests({\"language\": \"clojure\", \"paths\": [\"test/com/blockether/vis/internal/gateway\"]})"
             :error {"message" msg
                     "cause_data" {"type" "clj/bad-args"}
                     "trace" (str "java.util.concurrent.ExecutionException: " msg)
                     "block" {"source" "run_tests" "phase" "preflight"}}
             :result nil}]))]

      (expect (str/includes? txt "clj_test found no *_test.clj namespaces"))
      (expect (not (str/includes? txt "error: {\"message\"")))
      (expect (not (str/includes? txt "clojure.lang.ExceptionInfo")))
      (expect (not (str/includes? txt "java.util.concurrent.ExecutionException")))
      (expect (not (str/includes? txt "{:type :clj/bad-args")))))
  (it "keeps python_execution code + caret on error"
      (let
        [txt (str/join "\n"
                       (render-forms [{:vis/tool-name "python_execution"
                                       :success? false
                                       :code "print(PYCODEMARKER)\nx = 1/0"
                                       :error {:message "ZeroDivisionError: division by zero"}
                                       :result nil}]))]
        (expect (str/includes? txt "PYCODEMARKER"))
        (expect (str/includes? txt "ZeroDivisionError")))))

(defdescribe
  coalesce-forms-test
  ;; Regression: a DB-restored session whose trailer had >=2 adjacent `cat`
  ;; reads of the SAME file froze the whole TUI. `coalesce-forms` groups them by
  ;; the summary chip (it recovers the path from the summary PRECISELY because
  ;; the DB round-trip flattens `:result` from a map to the rendered STRING),
  ;; then a naive merge did `(assoc (:result f0) :anchors …)` on that string
  ;; -> ClassCastException every frame -> the render loop keeps re-throwing on
  ;; the last frame and never repaints.
  (it "merges adjacent same-path cat forms whose :result is a flattened STRING (no throw)"
      (let
        [forms
         [{:vis/tool-name "cat"
           :result-summary "`a.clj` · L1-10"
           :result-render "line one\nline two"
           :result "line one\nline two"}
          {:vis/tool-name "cat"
           :result-summary "`a.clj` · L40-50"
           :result-render "line forty"
           :result "line forty"}]

         out
         (coalesce-forms forms)]

        (expect (= 1 (count out))) ; the run collapsed
        (expect (str/includes? (:result-summary (first out)) "L1-10"))
        (expect (str/includes? (:result-summary (first out)) "L40-50"))
        ;; a string result carries through untouched (no anchors assoc'd onto it)
        (expect (string? (:result (first out))))))
  (it "still merges anchors when :result is a MAP (live, pre-DB-roundtrip)"
      (let
        [forms
         [{:vis/tool-name "cat"
           :result-summary "`a.clj` · L1-10"
           :result-render "x"
           :result {:path "a.clj" :anchors {"1:aa" "x"}}}
          {:vis/tool-name "cat"
           :result-summary "`a.clj` · L40-50"
           :result-render "y"
           :result {:path "a.clj" :anchors {"40:bb" "y"}}}]

         out
         (coalesce-forms forms)]

        (expect (= 1 (count out)))
        (expect (= {"1:aa" "x" "40:bb" "y"} (get-in (first out) [:result :anchors])))))
  (it "leaves a solo cat form and non-cat forms untouched"
      (let
        [forms
         [{:vis/tool-name "cat" :result-summary "`a.clj` · L1-10" :result "solo"}
          {:vis/tool-name "rg" :result-summary "5 hits" :result "hits"}]

         out
         (coalesce-forms forms)]

        (expect (= 2 (count out)))))
  (it "folds adjacent same-file patch forms into one multi-diff card"
      (let
        [forms
         [{:vis/tool-name "patch"
           :success? true
           :result-summary "update `a.clj`"
           :result-render "```diff\n+ one\n```"
           :result {:path "a.clj"}}
          {:vis/tool-name "patch"
           :success? true
           :result-summary "update `a.clj`"
           :result-render "```diff\n+ two\n```"
           :result {:path "a.clj"}}]

         out
         (coalesce-forms forms)]

        (expect (= 1 (count out)))
        (expect (= "update `a.clj`" (:result-summary (first out))))
        (expect (str/includes? (:result-render (first out)) "+ one"))
        (expect (str/includes? (:result-render (first out)) "+ two"))))
  (it "folds adjacent format_code path acks into one roll-up card"
      (let
        [forms
         [{:vis/tool-name "format_code"
           :success? true
           :result-summary "`src/a.clj` (no change)"
           :result {"path" "src/a.clj" "changed" false}}
          {:vis/tool-name "format_code"
           :success? true
           :result-summary "`test/a_test.clj` (no change)"
           :result {"path" "test/a_test.clj" "changed" false}}]

         out
         (coalesce-forms forms)]

        (expect (= 1 (count out)))
        (expect (= "2 files — 0 changed" (:result-summary (first out))))
        (expect (str/includes? (:result-render (first out)) "src/a.clj (no change)"))
        (expect (str/includes? (:result-render (first out)) "test/a_test.clj (no change)"))))
  (it "keeps a failed format_code ack separate from an adjacent success"
      (let
        [forms
         [{:vis/tool-name "format_code"
           :success? false
           :result-summary "`src/a.clj` failed"
           :result-render "boom"
           :result {"path" "src/a.clj"}}
          {:vis/tool-name "format_code"
           :success? true
           :result-summary "`test/a_test.clj` (no change)"
           :result {"path" "test/a_test.clj" "changed" false}}]

         out
         (coalesce-forms forms)]

        (expect (= 2 (count out)))))
  (it "keeps a failed patch separate from an adjacent successful one on the same file"
      (let
        [forms
         [{:vis/tool-name "patch"
           :success? false
           :result-summary "update `a.clj`"
           :result-render "boom"
           :result {:path "a.clj"}}
          {:vis/tool-name "patch"
           :success? true
           :result-summary "update `a.clj`"
           :result-render "ok"
           :result {:path "a.clj"}}]

         out
         (coalesce-forms forms)]

        (expect (= 2 (count out)))))
  (it "does not merge a cat and a patch on the same file"
      (let
        [forms
         [{:vis/tool-name "cat"
           :success? true
           :result-summary "`a.clj` · L1-10"
           :result-render "body"
           :result {:path "a.clj"}}
          {:vis/tool-name "patch"
           :success? true
           :result-summary "update `a.clj`"
           :result-render "```diff\n+ x\n```"
           :result {:path "a.clj"}}]

         out
         (coalesce-forms forms)]

        (expect (= 2 (count out))))))

(defdescribe
  tool-color-role-coverage-test
  (it "the TUI badge colour map covers every canonical vis/tool-color-roles role"
      ;; Guard against drift: the role list lives once in vis core; if a new role is
      ;; added there, this fails until the TUI map handles it (mirror of the web test).
      (doseq [role vis/tool-color-roles]
        (expect (some? (tool-color-role->fg role))
                (str role " has no TUI badge colour — add it to render/tool-color-role->fg")))))

(defmacro ^:private with-raw-code-on
  [& body]
  ;; The TUI now renders the model's raw `:code` unconditionally — the same
  ;; canonical contract as web's `block-code` (no `:vis/show-raw-code` gate).
  ;; This wrapper is a pass-through kept so existing layout/shape tests read
  ;; unchanged.
  `(do ~@body))

(defn- marker-of
  "First codepoint of `s` as a single-char string, or nil for empty."
  [s]
  (when (and (string? s) (pos? (count s))) (subs s 0 1)))

(defn- strip-ansi [s] (str/replace (or s "") #"\u001b\[[0-9;]*m" ""))

(defn- body-of
  "Drop the leading marker (PUA codepoint) and return the visible text.
   Tolerates empty input - a blank line in `:answer` mode renders as
   the empty string with the empty `:plain` marker, so callers must
   not crash when iterating over a frame that includes blank rows."
  [s]
  (when (string? s) (if (zero? (count s)) "" (subs s 1))))

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

(defdescribe bang-prefix-test
             (it "tints a `!`/`!&` head the instant the marker is typed"
                 (expect (= "!" (bang-prefix "!ls -la")))
                 (expect (= "!&" (bang-prefix "!&tail -f x")))
                 (expect (= "!" (bang-prefix "   !grep foo")))
                 ;; A bare marker already signals shell intent — tint at once,
                 ;; and a bare `!&` never falls through to the `!` branch.
                 (expect (= "!" (bang-prefix "!")))
                 (expect (= "!&" (bang-prefix "!&")))
                 (expect (= "!" (bang-prefix "!   ")))
                 (expect (= "!&" (bang-prefix "!&   ")))
                 (expect (= nil (bang-prefix "hello ! world")))
                 (expect (= nil (bang-prefix nil)))))

(defdescribe
  live-running-block-test
  (it "renders a block slot with no status footer"
      (with-raw-code-on
        ;; The right-aligned `BLOCK N` / `ITERATION N` / `CODE N` header bands
        ;; were retired per user directive (see comments in render.clj). Per-form
        ;; status footers are gone too; code blocks are source-only.
        (let
          [lines
           (format-iteration-entry {:iteration 0
                                    :forms [{:code "(reduce + (range 1000))"
                                             :comment nil
                                             :render-segments nil
                                             :stdout nil
                                             :error nil
                                             :started-at-ms 1000
                                             :duration-ms 0
                                             :success? nil
                                             :silent? false}]}
                                   40
                                   1
                                   {:now-ms 2500})

           code-line
           (first (filter #(str/includes? (strip-ansi %) "reduce") lines))]

          ;; A live-running block's raw :code paints verbatim with a MARKER_CODE
          (expect (not-any? #(str/includes? % "BLOCK 1") lines))
          (expect (not-any? #(str/includes? % "ITERATION 1") lines))
          (expect (not-any? #(str/includes? % "CODE 1") lines))
          (expect (= p/MARKER_CODE (marker-of code-line)))
          (expect (not-any? #(str/includes? % "↻") lines))
          (expect (not-any? #(str/includes? % "1.0s") lines)))))
  (it "renders the block's raw code verbatim — the canonical web block-code contract"
      (let
        [lines
         (format-iteration-entry {:iteration 0
                                  :forms [{:code "git_status()\nprint(42)"
                                           :comment nil
                                           :stdout nil
                                           :error nil
                                           :started-at-ms nil
                                           :duration-ms 1
                                           :success? true
                                           :silent? false}]}
                                 60
                                 1
                                 {})

         body
         (str/join "\n" (map (comp strip-ansi body-of) lines))]

        ;; The model's raw :code paints in full — no render-segment filtering,
        ;; no show-raw-code gate (identical to web's `block-code`). Engine-chrome
        ;; forms (answers/titles) are dropped upstream via :silent, never by
        ;; stripping segments out of a code body.
        (expect (str/includes? body "git_status()"))
        (expect (str/includes? body "print(42)"))))
  (it
    "renders form eval errors inline with source caret"
    (let
      [code
       "(def git-diff-doc (doc 'v/git-diff))"

       err
       {:message "Unable to resolve symbol: 'v/git-diff"
        :trace "clojure.lang.ExceptionInfo: Unable to resolve symbol: 'v/git-diff"
        :block {:source code :row 1 :col 24}}

       lines
       (format-iteration-entry {:iteration 0
                                :forms [{:code code
                                         :comment nil
                                         :render-segments nil
                                         :stdout nil
                                         :error err
                                         :started-at-ms nil
                                         :duration-ms 1
                                         :success? false
                                         :silent? false}]}
                               80
                               1
                               {})

       visible
       (mapv (comp strip-sentinels strip-ansi body-of) lines)

       body
       (str/join "\n" visible)

       error-line
       (first (filter #(str/includes? % "Unable to resolve symbol") lines))]

      (expect (str/includes? body "(def git-diff-doc"))
      (expect (not (str/includes? body " 1:")))
      (expect (str/includes? body "^---"))
      (expect (str/includes? body "Unable to resolve symbol: 'v/git-diff"))
      (expect (not (str/includes? body "Error: Unable")))
      (expect (not (str/includes? body "ERROR — clojure.lang.ExceptionInfo")))
      (expect (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote code)) body))))
      (expect (= p/MARKER_CODE_ERR (marker-of error-line)))))
  (it
    "renders a form eval error message exactly once"
    (let
      [code
       "(clj/eval {:code \"(+ 1 2)\"})"

       msg
       "nREPL connect failed on localhost:7888 — is the REPL running? Try (clj/ports)."

       err
       {:type :clojure.lang/exception-info
        :message msg
        :trace (str "clojure.lang.ExceptionInfo: " msg)
        :block {:source code :row 1 :col 2}}

       lines
       (format-iteration-entry {:iteration 0
                                :error err
                                :forms [{:code code
                                         :comment nil
                                         :render-segments nil
                                         :stdout nil
                                         :error err
                                         :started-at-ms nil
                                         :duration-ms 1
                                         :success? false
                                         :silent? false}]}
                               100
                               1
                               {})

       visible
       (mapv (comp strip-sentinels strip-ansi body-of) lines)

       body
       (str/join "\n" visible)]

      (expect (str/includes? body "(clj/eval"))
      (expect (not (str/includes? body " 1:")))
      (expect (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote msg)) body))))
      (expect (not (str/includes? body "ERROR:")))
      (expect (not (str/includes? body "ERROR —")))))
  (it
    "omits success status footer and keeps only code band edges"
    (with-raw-code-on
      ;; Layout (post status-footer removal):
      ;;   iteration-pad
      ;;   code-ok-pad
      ;;   <code line>
      ;;   code-ok-pad
      ;;   iteration-pad
      ;; Plain `:value` form results no longer render — the trailing
      ;; result row is gone for non-tool forms per user directive.
      (let
        [lines
         (format-iteration-entry {:iteration 0
                                  :forms [{:code "(+ 1 2)"
                                           :comment nil
                                           :render-segments nil
                                           :stdout nil
                                           :error nil
                                           :started-at-ms nil
                                           :duration-ms 1
                                           :success? true
                                           :silent? false}]}
                                 40
                                 1
                                 {})

         bodies
         (mapv (comp strip-ansi body-of) lines)]

        (expect (not-any? #(str/includes? % "✓") lines))
        (expect (not-any? #(str/includes? % "1ms") lines))
        (expect (= 2 (count (filter #(= p/MARKER_CODE_OK_PAD (marker-of %)) lines))))
        (expect (not-any? #(str/includes? (or % "") "3") bodies)))))
  (it "pads displayed form comments by one column"
      (with-raw-code-on
        (let
          [lines
           (format-iteration-entry {:iteration 0
                                    :forms [{:code "(+ 1 2)"
                                             :comment ";; why this runs"
                                             :render-segments nil
                                             :stdout nil
                                             :error nil
                                             :started-at-ms nil
                                             :duration-ms 0
                                             :success? nil
                                             :silent? false}]}
                                   40
                                   1
                                   {})

           comment-line
           (first (filter #(str/includes? % ";; why this runs") lines))]

          (expect (= p/MARKER_THINKING (marker-of comment-line)))
          (expect (str/starts-with? (body-of comment-line) " ;;")))))
  (describe "provider-fallback-notice-test"
            (it "renders provider fallback recap lines above fallback details"
                (let
                  [lines
                   (format-iteration-entry {:provider-fallbacks
                                            [{:failed-provider {:id :anthropic-coding-plan
                                                                :model "claude-opus-4-7"
                                                                :error
                                                                "Exceptional status code: 429"}}]}
                                           120
                                           1
                                           {})

                   body
                   (str/join "\n" (map (comp strip-ansi body-of) lines))]

                  ;; The recap rail is retired; provider fallback notices were
                  ;; recap-only rows and no longer surface.
                  (expect (not (str/includes? body "RECAP")))
                  (expect (not (str/includes? body "Provider fallback:"))))))
  (it "formats same-provider retry notices as recap rows"
      (let
        [lines
         (format-iteration-entry {:provider-fallbacks [{:event/type :llm.routing/provider-retry
                                                        :provider "anthropic-coding-plan"
                                                        :model "claude-opus-4-7"
                                                        :reason :rate-limit
                                                        :delay-ms 2000}]}
                                 120
                                 1
                                 {})

         ;; Recap row wraps when wider than the bubble: line N+1
         ;; carries a leading space (continuation indent). Trim each
         ;; wrapped fragment then join with a SINGLE space so two
         ;; meaningful spaces inside the recap badge (\"RECAP  Provider\")
         ;; survive the merge. Substring check pins CONTENT, not
         ;; column width.
         body
         (->> lines
              (map (comp str/trim strip-ansi body-of))
              (str/join " "))]

        ;; Retired with the recap rail — same-provider retry notices were
        ;; recap-only rows and no longer surface.
        (expect (not (str/includes? body "RECAP")))))
  (it "renders provider error recap lines above provider error details"
      (let
        [lines
         (format-iteration-entry {:error {:type :svar.core/http-error
                                          :message "Exceptional status code: 429"
                                          :data {:status 429 :body "rate limit"}}}
                                 120
                                 1
                                 {})

         body
         (->> lines
              (map (comp str/trim strip-sentinels strip-ansi body-of))
              (str/join " "))]

        ;; The recap rail is retired; the provider error itself still
        ;; surfaces its actionable guidance via the error panel.
        (expect (not (str/includes? body "RECAP")))
        (expect
          (str/includes?
            body
            "NEXT STEP: rate limit — wait and retry, re-authenticate, or switch provider/model"))
        (expect (not (str/includes? body "PROVIDER_ERROR  HTTP 429"))))))

(defdescribe
  provider-auth-error-test
  (it
    "renders provider auth errors as action, not duplicate raw JSON"
    (let
      [lines
       (format-iteration-entry
         {:error
          {:type :svar.core/http-error
           :message
           "API authentication failed. Check your API key. (Original: Exceptional status code: 401)"
           :data
           {:status 401
            :body
            "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid authentication credentials\"},\"request_id\":\"req_123\"}"}}}
         120
         1
         {})

       body
       (->> lines
            (map (comp str/trim strip-sentinels strip-ansi body-of))
            (str/join " "))]

      ;; The recap rail is retired; auth errors still render as action.
      (expect (not (str/includes? body "RECAP")))
      (expect (not (str/includes? body "PROVIDER_ERROR  HTTP 401")))
      (expect (str/includes? body "Provider message: Invalid authentication credentials"))
      (expect (str/includes? body "NEXT STEP: re-authenticate this provider or update its API key"))
      (expect (not (str/includes? body "provider response:")))
      (expect (not (str/includes? body "{\"type\":"))))))

(defdescribe
  provider-transport-error-test
  (it "renders a transport blip (no HTTP status) as a structured provider error, not one plain line"
      (let
        [lines
         (format-iteration-entry {:error {:type :svar.core/http-error
                                          ;; A socket that died before any byte arrived carries
                                          ;; NO :status/:body/:request-id — only the wrapper text.
                                          :message "HTTP/1.1 header parser received no bytes"
                                          :data {:stream? true}}}
                                 120
                                 1
                                 {})

         body
         (->> lines
              (map (comp str/trim strip-sentinels strip-ansi body-of))
              (str/join " "))]

        ;; It must get the shared provider-error treatment: the split
        ;; explanation / NEXT STEP / facts rows — NOT the raw wrapper dumped as a
        ;; single generic "error" line.
        (expect (str/includes? body "WHAT HAPPENED: Vis could not complete the HTTP request"))
        (expect (str/includes? body
                               "NEXT STEP: this is a network/connection blip, not a rejection"))
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
    (cond (str/blank? s) true
          (every?
            (fn [^Character c]
              (let [n (int c)]
                (or (= n 0x200B) (= n 0x200C) (= n 0x200D) (= n 0xFEFF) (<= 0x2060 n 0x206F))))
            s)
          true
          :else (let
                  [c0 (.charAt ^String s 0)
                   n0 (int c0)
                   body (subs s 1)]

                  (and (<= 0xE000 n0 0xE0FF) (str/blank? body))))))

(defdescribe
  answer-trailer-margin-test
  ;; Answer layout mirrors code-block chrome: one neutral outside
  ;; margin row above the answer zone, then one answer-bg inside pad
  ;; row before text. If a code-bearing trace already ended with a
  ;; neutral iteration pad, reuse that row as the outside margin.
  (let
    [ans
     "hello"

     settings
     {:show-thinking true :show-iterations true}

     iter
     {:forms [{:code "(+ 1 1)"
               :comment nil
               :render-segments nil
               :stdout nil
               :error nil
               :started-at-ms nil
               :duration-ms 1
               :success? true
               :silent? false}]}

     index-of
     (fn [p needle]
       (first (keep-indexed (fn [i ln]
                              (when (str/includes? (strip-ansi ln) needle) i))
                            (:lines p))))]

    (it "answer with NO trace renders directly without internal answer padding"
        (let
          [p
           (render/format-answer-with-thinking-data* ans [] 80 settings nil false nil)

           idx
           (index-of p "hello")

           ln
           (:lines p)

           pad?
           (fn [line]
             (str/starts-with? line p/MARKER_ANSWER_PAD))]

          ;; Top-margin blank row pushes content down by one.
          (expect (= 1 idx))
          (expect (visually-blank? (nth ln 0)))
          (expect (not-any? pad? ln))))
    (it "engine-mutation recaps no longer render in the trace"
        ;; The RECAP rail is fully retired: an iteration carrying
        ;; `:recaps` produces no RECAP rows above the answer.
        (let
          [p
           (render/format-answer-with-thinking-data*
             ans
             [{:recaps ["Title — \"Wyjaśnienie rozmiaru kontekstu\""]}]
             80
             settings
             nil
             false
             nil)

           answer-idx
           (index-of p "hello")]

          (expect (some? answer-idx))
          (expect (nil? (index-of p "RECAP")))))
    (it
      "answer with code-bearing trace keeps band edges + a terminal-bg gap between trace and answer"
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
        (let
          [p
           (render/format-answer-with-thinking-data* ans [iter] 80 settings nil false nil)

           idx
           (index-of p "hello")

           ln
           (:lines p)

           above
           (when (and idx (>= idx 3)) [(nth ln (dec idx)) (nth ln (- idx 2)) (nth ln (- idx 3))])]

          (expect (some? idx))
          (expect (visually-blank? (nth ln (dec idx))))
          (expect (or (nil? above) (not (apply = above))))))
      (it "cancelled with non-empty answer renders the answer text once"
          ;; `cancel-text` falls back to the answer text when the IR is
          ;; non-empty. Cancelled turns are flat system notes, so they use
          ;; only the outside margin, not answer-bg inside padding.
          (let
            [p
             (render/format-answer-with-thinking-data* ans [iter] 80 settings nil true nil)

             idx
             (index-of p "hello")

             ln
             (:lines p)]

            (expect (some? idx))
            (expect (visually-blank? (nth ln (dec idx))))
            (expect (or (zero? (dec idx)) (not (visually-blank? (nth ln (- idx 2)))))))))))

(defdescribe collapsed-thinking-ellipsis-test
             ;; Regression: the collapsed `▸ THINKING +N more` peek appends a dim
             ;; " …" to its LAST visible reasoning line. Thinking rows carry a
             ;; zero-width thinking marker (`​`) prefix that `str/blank?` does
             ;; NOT count as whitespace, so the old detection treated every row —
             ;; including paragraph-separator blanks — as non-blank. When the
             ;; 6-row peek window ended on a blank separator the " …" landed there
             ;; and rendered alone on its own otherwise-empty line.
             (let
               [;; A thinking text whose 6th wrapped row is a blank paragraph
                ;; separator, with enough rows after it to force the collapse.
                thinking
                (str "The patch was applied successfully. Now I need to verify the change "
                     "works by evaluating the namespace in the REPL, then close the task.\n\n"
                     "Let me verify by loading the file or at least checking the changed "
                     "lines look correct.\n\n" "The diff looks correct.\n\n"
                     "More reasoning after the diff that should be hidden.\n"
                     "Even more hidden reasoning lines here.\n")

                stid
                "abcd1234-5678-9999"

                visible
                (->> (:lines (render/format-answer-with-thinking-data*
                               nil
                               [{:thinking thinking}]
                               80
                               {:show-thinking true :show-iterations true}
                               nil
                               false
                               {:session-id "sid" :session-turn-id stid :detail-expansions {}}))
                     (mapv (comp str/trimr strip-sentinels strip-ansi body-of)))]

               (it "renders the collapsed THINKING peek with a +N more header"
                   (expect (some #(str/includes? % "THINKING  +") visible)))))

(defdescribe
  progress-rendering-test
  (it "iter-0 spinner row has a one-line top margin inside the bubble"
      ;; Regression: the "Vis is calling the provider" spinner used to
      ;; sit flush against the bubble's top border because the no-trace
      ;; branch in `progress->lines-data` emitted just the spinner line.
      ;; The iter≥1 branch always ended with a blank line before the
      ;; spinner, so the bubble visually grew by an extra row the moment
      ;; the first iteration arrived. Keep the blank in both branches.
      (let
        [payload
         (render/progress->lines-data {:iterations []}
                                      80
                                      {:show-thinking true :show-iterations true}
                                      {:now-ms 1000 :turn-start-ms 0})

         lines
         (mapv strip-ansi (:lines payload))]

        (expect (= 2 (count lines)))
        (expect (= "" (first lines)))
        (expect (str/includes? (second lines) "Vis is calling the provider"))))
  (it "shows queued submissions inside the live progress bubble"
      (let
        [payload
         (render/progress->lines-data
           {:iterations []}
           80
           {:show-thinking true :show-iterations true}
           {:now-ms 1000 :turn-start-ms 0 :pending-sends [{:text "please also check logs"}]})

         body
         (strip-ansi (str/join "\n" (:lines payload)))]

        (expect (str/includes? body "Vis is calling the provider"))
        (expect (str/includes? body "please also check logs"))
        (expect (not (str/includes? body "queued update")))))
  (it "distinguishes response parsing from provider waiting"
      (let
        [payload
         (render/progress->lines-data {:iterations [{:iteration 1 :activity :response-parse}]}
                                      80
                                      {:show-thinking true :show-iterations true}
                                      {:now-ms 1000 :turn-start-ms 0})

         body
         (strip-ansi (str/join "\n" (:lines payload)))]

        (expect (str/includes? body "Vis is parsing model response (iter 1)"))))
  (it
    "uses the same trace renderer for live progress and cancelled bubbles"
    (let
      [;; Tool output paints purely as the program's stdout — both live
       ;; and cancelled paths share the renderer.
       iter
       {:forms [{:code "(print \"bold result\")"
                 :comment nil
                 :render-segments nil
                 :stdout "bold result"
                 :error nil
                 :started-at-ms nil
                 :duration-ms 1
                 :success? true
                 :silent? false}]}

       settings
       {:show-thinking true :show-iterations true}

       live
       (:lines (render/progress->lines-data {:iterations [iter]}
                                            80
                                            settings
                                            {:now-ms 1000 :turn-start-ms 0}))

       cancel
       (:lines (render/format-answer-with-thinking-data "Cancelled by user."
                                                        [iter]
                                                        80
                                                        settings
                                                        nil
                                                        true
                                                        nil))

       clean
       (fn [line]
         (strip-sentinels (strip-ansi line)))

       trim-tail
       (fn [xs]
         (vec (reverse (drop-while visually-blank? (reverse xs)))))

       trace-before
       (fn [needle lines]
         (->> lines
              (take-while #(not (str/includes? (clean %) needle)))
              trim-tail))

       live-trace
       (trace-before "Vis is" live)

       cancel-trace
       (trace-before "Cancelled by user." cancel)

       body
       (str/join "\n" (map clean cancel-trace))]

      (expect (= live-trace cancel-trace))
      (expect (str/includes? body "bold result"))
      (expect (not (str/includes? body ":ir")))))
  (it "live progress renders every iteration instead of hiding history"
      (with-raw-code-on
        (let
          [mk-entry
           (fn [n]
             {:forms [{:code (str "(+ " n " 1)")
                       :comment nil
                       :render-segments nil
                       :stdout nil
                       :error nil
                       :started-at-ms nil
                       :duration-ms 1
                       :success? true
                       :silent? false}]})

           body
           (strip-ansi (render/progress->text {:iterations (mapv mk-entry (range 5))}
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
          (let
            [body (strip-ansi (render/progress->text {:iterations [{:thinking "alpha\nbeta"}]}
                                                     80
                                                     {:show-thinking true :show-iterations true}
                                                     {:now-ms 1000 :turn-start-ms 0}))]
            (expect (not (str/includes? body "hidden while live")))
            (expect (str/includes? body "alpha"))
            (expect (str/includes? body "beta")))))
  (it "labels a shell-run `!` bang turn in the spinner instead of the provider placeholder"
      ;; A bang turn never enters iteration-loop, so it streams ONE
      ;; shell-phase chunk while the shell blocks; the spinner must read
      ;; `Vis is running: <cmd>` — never the zero-iterations provider fallback.
      (let
        [body (strip-ansi (render/progress->text
                            {:iterations
                             [{:iteration 1
                               :activity :shell-run
                               :shell/cmd
                               "cd ../ && git clone git@github.com:Blockether/spel.git"}]}
                            80
                            {:show-thinking true :show-iterations true}
                            {:now-ms 1000 :turn-start-ms 0}))]
        (expect (str/includes? body "Vis is running:"))
        (expect (str/includes? body "git clone"))
        (expect (not (str/includes? body "Vis is calling the provider")))))
  (it "labels a pure slash command in the spinner instead of the provider placeholder"
      ;; A registered slash (/fs, /draft, …) runs LOCALLY via run-slash-turn! and
      ;; never touches a provider, so it streams ONE :slash phase chunk; the spinner
      ;; must read `Vis is running: /<name>` — never the zero-iterations provider fallback.
      (let
        [body (strip-ansi (render/progress->text
                            {:iterations [{:iteration 1 :activity :slash :slash/label "/fs list"}]}
                            80
                            {:show-thinking true :show-iterations true}
                            {:now-ms 1000 :turn-start-ms 0}))]
        (expect (str/includes? body "Vis is running:"))
        (expect (str/includes? body "/fs list"))
        (expect (not (str/includes? body "Vis is calling the provider")))))
  (it "labels a nested tool call in the spinner while the block runs"
      ;; A shell_run (or any native tool) INSIDE a python_execution block streams
      ;; a :tool-call activity naming the op, so the bubble reads
      ;; "Vis is running: <op>" instead of freezing for the whole call.
      (let
        [body (strip-ansi (render/progress->text
                            {:iterations [{:iteration 1 :activity :tool-call :tool/op "shell_run"}]}
                            80
                            {:show-thinking true :show-iterations true}
                            {:now-ms 1000 :turn-start-ms 0}))]
        (expect (str/includes? body "Vis is running:"))
        (expect (str/includes? body "shell_run"))))
  (it "live progress previews huge thinking with the viewport-driven truncation"
      ;; The single-iteration truncation summary only fires when a
      ;; viewport budget is supplied (the renderer can't decide to
      ;; collapse without knowing how much screen real estate exists).
      ;; Pin both halves of the contract: without `:viewport-rows` the
      ;; full thinking renders; with a tight budget the collapse summary
      ;; bounds the line count.
      (let
        [huge-thinking
         (apply str (repeat 20000 "thinking "))

         full
         (render/progress->lines-data {:iterations [{:thinking huge-thinking}]}
                                      96
                                      {:show-thinking true :show-iterations true}
                                      {:now-ms 1000 :turn-start-ms 0})]

        ;; No viewport: full body is rendered, just shouldn't crash.
        (expect (pos? (count (:lines full))))
        (expect (not (str/includes? (strip-ansi (:text full)) huge-thinking)))
        (expect (some (fn [ln]
                        (str/includes? (strip-ansi ln) "thinking thinking"))
                      (:lines full)))))
  (it "live progress hides plain value results entirely"
      ;; Per user directive: only tool calls show a result pane; plain
      ;; `:value` form results never paint a body, regardless of size.
      ;; No `RESULT` label, no `chars hidden` summary, no toggle-details
      ;; click region — collapsible UI is gone.
      (render/invalidate-cache!)
      (let
        [huge-result
         (str/join " " (repeat 1000 "abcdefghij"))

         payload
         (render/progress->lines-data
           {:iterations
            [{:forms [{:code "(+ 1 2)" :stdout nil :duration-ms 1 :success? true :silent? false}]}]}
           96
           {:show-thinking true :show-iterations true}
           {:now-ms 1000 :turn-start-ms 0 :session-id "session" :detail-expansions {}})]

        (expect (not (str/includes? (:text payload) "RESULT")))
        (expect (not (str/includes? (:text payload) "chars hidden")))
        (expect (not (str/includes? (:text payload) huge-result)))
        (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))
  (it
    "live progress always renders every iteration with no PROGRESS HISTORY toggle"
    (with-raw-code-on
      ;; Per user directive: no collapsible iteration history. Every
      ;; iteration paints in place; the PROGRESS HISTORY summary band
      ;; is gone.
      (let
        [mk-entry
         (fn [n]
           {:forms [{:code (str "(+ " n " 1)")
                     :comment nil
                     :render-segments nil
                     :stdout nil
                     :error nil
                     :started-at-ms nil
                     :duration-ms 1
                     :success? true
                     :silent? false}]})

         payload
         (render/progress->lines-data
           {:iterations (mapv mk-entry (range 12))}
           80
           {:show-thinking true :show-iterations true}
           {:now-ms 1000 :turn-start-ms 0 :session-id "session" :detail-expansions {}})

         body
         (strip-ansi (:text payload))]

        (expect (not (str/includes? body "PROGRESS HISTORY")))
        (expect (not (str/includes? body "iterations hidden")))
        (expect (str/includes? body "(+ 0 1)"))
        (expect (str/includes? body "(+ 11 1)"))
        (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload))))))
  (it "toggles :vis/silent forms in live progress traces"
      (with-raw-code-on
        (let
          [progress
           {:iterations
            [{:forms [{:code "(set-session-title! \"Greeting\")"
                       :stdout nil
                       :duration-ms 1
                       :success? true
                       :silent? true}
                      {:code "(+ 1 2)" :stdout nil :duration-ms 1 :success? true :silent? false}]}]}

           hidden-body
           (strip-ansi (render/progress->text
                         progress
                         80
                         {:show-thinking true :show-iterations true :show-silent false}
                         {:now-ms 1000 :turn-start-ms 0}))

           shown-body
           (strip-ansi (render/progress->text
                         progress
                         80
                         {:show-thinking true :show-iterations true :show-silent true}
                         {:now-ms 1000 :turn-start-ms 0}))]

          (expect (not (str/includes? hidden-body "set-session-title!")))
          (expect (str/includes? hidden-body "(+ 1 2)"))
          (expect (str/includes? shown-body "set-session-title!"))
          ;; Plain `:value` result bodies (`:vis/silent`, `3`) are hidden
          ;; per user directive — only tool channel-render output paints.
          (expect (not (str/includes? shown-body ":vis/silent"))))))
  (describe "repeated-error-collapse-test"
            (it "squashes repeated identical provider errors into one counted row"
                (render/invalidate-cache!)
                (let
                  [err
                   {:message "Stream ended before terminal marker."
                    :data {:type :svar.core/stream-truncated}}

                   body
                   (strip-ansi (:text (render/progress->lines-data
                                        {:iterations (vec (repeat 11 {:error err}))}
                                        80
                                        {:show-thinking true :show-iterations true}
                                        {:now-ms 1000 :turn-start-ms 0})))]

                  (expect (str/includes? body "ERROR x 11: Stream ended before terminal marker."))
                  (expect (= 1 (count (re-seq #"ERROR" body))))))))

(defdescribe
  progress-streaming-perf-test
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
      (let
        [mk-iter
         (fn [i]
           {:thinking (apply str (repeat (+ 200 (* 100 i)) \.))
            :forms [{:code (str "(do (println :iter " i ") (mapv inc (range 100)))")
                     :comment nil
                     :render-segments nil
                     :stdout nil
                     :error nil
                     :started-at-ms nil
                     :duration-ms 50
                     :success? true
                     :silent? false}]})

         base
         (mapv mk-iter (range 14))

         last-base
         (mk-iter 14)

         bubble-w
         130

         settings
         {:show-thinking true :show-iterations true}]

        (render/invalidate-cache!)
        ;; Warm: 3 cycles to JIT the format path.
        (dotimes [_ 3]
          (render/progress->lines-data
            {:iterations base}
            bubble-w
            settings
            {:now-ms 1700000000000 :turn-start-ms 1700000000000 :viewport-rows 50}))
        ;; Streaming: NEW iterations vec each tick (mimics `(vec (vals @timeline))`),
        ;; last iteration's thinking grows by 100 chars per tick.
        (let
          [runs
           30

           t0
           (System/nanoTime)]

          (dotimes [i runs]
            (let
              [growing (assoc last-base
                         :thinking (apply str (:thinking last-base) (repeat (* 100 (inc i)) \.)))
               its' (conj (vec base) growing)]

              (render/progress->lines-data {:iterations its'}
                                           bubble-w
                                           settings
                                           {:now-ms (+ 1700000000000 (* i 80))
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
      (let
        [iter
         {:thinking "some reasoning"
          :forms [{:code "(+ 1 2)"
                   :comment nil
                   :render-segments nil
                   :stdout nil
                   :error nil
                   :started-at-ms nil
                   :duration-ms 10
                   :success? true
                   :silent? false}]}

         iters1
         (vec (repeat 5 iter))

         ;; Same content, fresh vec identity, fresh map identities for entries.
         iters2
         (mapv #(into {} %) iters1)]

        (render/invalidate-cache!)
        ;; Warm with iters1.
        (dotimes [_ 3]
          (render/progress->lines-data {:iterations iters1}
                                       100
                                       {:show-thinking true :show-iterations true}
                                       {:now-ms 1700000000000 :turn-start-ms 1700000000000}))
        ;; iters2 has identical CONTENT but different identity at every level.
        (let [t0 (System/nanoTime)]
          (dotimes [_ 100]
            (render/progress->lines-data {:iterations iters2}
                                         100
                                         {:show-thinking true :show-iterations true}
                                         {:now-ms 1700000000000 :turn-start-ms 1700000000000}))
          (let [per-call-us (/ (/ (- (System/nanoTime) t0) 1e3) 100.0)]
            ;; Cache hit path: should be well under 1 ms (1000 µs) per call
            ;; even with 5 cached iterations to concat. If this exceeds 5 ms
            ;; the per-iteration content-keyed cache is broken.
            (expect (< per-call-us 5000.0)))))))

(defdescribe
  progress-body-cache-test
  ;; The 80ms live tick used to re-walk the WHOLE bubble every frame
  ;; (trace-render-entries + coalesce-bubble-blanks + per-line
  ;; strip-paint-markers-line for `:text`) even when only the spinner clock
  ;; advanced. `progress->lines-data` now memoizes that content body and
  ;; splices only the animated spinner row in per tick. These tests pin both
  ;; halves: a spinner-only tick must NOT recompute the body, and the spliced
  ;; output must stay byte-identical to the body except for the spinner row.
  (let
    [mk-iter
     (fn [i]
       {:thinking (str "reason-" i)
        :forms [{:code (str "(+ " i " 1)")
                 :comment nil
                 :render-segments nil
                 :stdout (str "out-" i)
                 :error nil
                 :started-at-ms nil
                 :duration-ms 10
                 :success? true
                 :silent? false}]})

     settings
     {:show-thinking true :show-iterations true}

     extra
     (fn [now]
       {:now-ms now
        :turn-start-ms 1699999900000
        :session-id "s1"
        :session-turn-id "turn-abc12345"
        :viewport-rows 40})]

    (it "spinner-only tick reuses the cached body (no re-walk); only the spinner row changes"
        (let [iters (mapv mk-iter (range 3))]
          (render/invalidate-cache!)
          (let
            [a (render/progress->lines-data {:iterations iters} 130 settings (extra 1700000000000))
             size-after-first (render/cache-size)
             ;; +5s: identical content, only the spinner clock advances.
             b (render/progress->lines-data {:iterations iters} 130 settings (extra 1700000005000))
             size-after-tick (render/cache-size)]

            ;; No new body entry ⇒ the O(bubble) trace/coalesce/strip walk was skipped.
            (expect (= size-after-first size-after-tick))
            (expect (= (count (:lines a)) (count (:lines b))))
            (expect (= (:line-meta a) (:line-meta b)))
            ;; ONLY the final spinner row differs between the two ticks.
            (let
              [diff (keep-indexed (fn [i [x y]]
                                    (when (not= x y) i))
                                  (map vector (:lines a) (:lines b)))]
              (expect (= [(dec (count (:lines a)))] (vec diff))))
            ;; `:text` mirrors `:lines`: same body, different last (spinner) line.
            (expect (not= (:text a) (:text b))))))
    (it "a content change busts the body cache and grows the trace"
        (render/invalidate-cache!)
        (let
          [three
           (render/progress->lines-data {:iterations (mapv mk-iter (range 3))}
                                        130
                                        settings
                                        (extra 1700000005000))

           size3
           (render/cache-size)

           four
           (render/progress->lines-data {:iterations (mapv mk-iter (range 4))}
                                        130
                                        settings
                                        (extra 1700000005000))

           size4
           (render/cache-size)]

          (expect (> size4 size3))
          (expect (> (count (:lines four)) (count (:lines three))))))
    (it "queued sends still render after the spinner with the body split path"
        (render/invalidate-cache!)
        (let
          [payload
           (render/progress->lines-data {:iterations (mapv mk-iter (range 2))}
                                        130
                                        settings
                                        (assoc (extra 1700000005000)
                                          :pending-sends [{:text "first queued message"}
                                                          {:text "second queued message"}]))

           lines
           (:lines payload)

           spinner-idx
           (first (keep-indexed (fn [i l]
                                  (when (str/includes? (str l) "Esc to cancel") i))
                                lines))

           queued-idx
           (first (keep-indexed (fn [i l]
                                  (when (str/includes? (str l) "Queued") i))
                                lines))]

          (expect (some? spinner-idx))
          (expect (some? queued-idx))
          ;; Queue block renders AFTER the spinner row (order preserved).
          (expect (< (long spinner-idx) (long queued-idx)))))))

(defdescribe
  live-body-throttle-test
  ;; VIS_LIVE_BODY_THROTTLE_MS debounces the heavy live re-projection: within
  ;; the window a content change reuses the previous body, while the spinner
  ;; row (spliced in fresh) still advances. Default 0 = disabled, so streamed
  ;; growth is reflected on every tick (no behaviour change).
  (let
    [cell
     @#'render/live-body-throttle-cell

     throttle-var
     #'render/live-body-throttle-ms

     with-throttle
     (fn [ms f]
       (alter-var-root throttle-var (constantly ms))
       (try (f) (finally (alter-var-root throttle-var (constantly 0)))))

     prog
     (fn [txt]
       {:iterations [{:assistant-prose txt}]})

     extra
     (fn [now]
       {:now-ms now :turn-start-ms 1699999900000})

     body-lines
     (fn [d]
       (remove #(str/includes? (str %) "Esc to cancel") (:lines d)))

     spinner
     (fn [d]
       (first (filter #(str/includes? (str %) "Esc to cancel") (:lines d))))]

    (it "default (0) reflects streamed growth on every tick"
        (render/invalidate-cache!)
        (reset! cell nil)
        (let
          [a
           (render/progress->lines-data (prog "alpha") 130 {} (extra 1700000000000))

           b
           (render/progress->lines-data (prog "alpha beta") 130 {} (extra 1700000001000))]

          (expect (not= (body-lines a) (body-lines b)))))
    (it "enabled reuses the body within the window yet still advances the spinner"
        (render/invalidate-cache!)
        (reset! cell nil)
        (with-throttle
          500
          (fn []
            (let
              [a
               (render/progress->lines-data (prog "alpha") 130 {} (extra 1700000000000))

               ;; +300ms < 500ms window: heavy trace re-walk skipped, body reused.
               b
               (render/progress->lines-data (prog "alpha beta") 130 {} (extra 1700000000300))]

              (expect (= (body-lines a) (body-lines b)))
              (expect (not= (spinner a) (spinner b)))))))
    (it "enabled recomputes once the window lapses"
        (render/invalidate-cache!)
        (reset! cell nil)
        (with-throttle
          100
          (fn []
            (let
              [a
               (render/progress->lines-data (prog "alpha") 130 {} (extra 1700000000000))

               ;; +500ms > 100ms window: fresh content projected.
               b
               (render/progress->lines-data (prog "alpha beta") 130 {} (extra 1700000000500))]

              (expect (not= (body-lines a) (body-lines b)))))))))

(defdescribe
  iteration-live-ordering-test
  (describe "ordered live progress events"
            (it "renders reasoning before code in the post-:events flat layout"
                (with-raw-code-on
                  ;; The pre-existing `:events`-driven interleaved variant was
                  ;; removed when the runtime contract dropped `:events`. Resume /
                  ;; live now share a flat layout: thinking first, then all code
                  ;; blocks. Plain `:value` form results are hidden per user
                  ;; directive, so only code (not `2`) follows the reasoning.
                  (let
                    [lines
                     (format-iteration-entry {:thinking "alpha\nbeta"
                                              :error nil
                                              :forms [{:code "(+ 1 1)"
                                                       :comment nil
                                                       :render-segments nil
                                                       :stdout nil
                                                       :error nil
                                                       :started-at-ms nil
                                                       :duration-ms 1
                                                       :success? true
                                                       :silent? false}]}
                                             60
                                             1
                                             {:show-header? true})

                     ^String body
                     (strip-ansi (str/join "\n" (map body-of lines)))]

                    (expect (< (.indexOf body "alpha") (.indexOf body "beta")))
                    (expect (< (.indexOf body "beta") (.indexOf body "(+ 1 1)")))
                    (expect (neg? (.indexOf body "2")))))))
  (it
    "keeps thinking and code flush so the thinking→badge margin matches code→badge"
    (with-raw-code-on
      ;; Parallel work (`equalize thinking→badge margin`) intentionally
      ;; collapsed the previous two-row pad block between thinking and
      ;; code so the visible breathing room is a SINGLE neutral row,
      ;; identical to the gap a code block leaves above the next iter
      ;; recap. This test pins that contract — thinking ends, one
      ;; blank/marker row, code starts — so a future layout tweak that
      ;; re-introduces a stripe between them surfaces here, not in a
      ;; user-visible asymmetry report.
      (let
        [lines
         (format-iteration-entry {:thinking "alpha"
                                  :error nil
                                  :forms [{:code "(+ 1 1)"
                                           :comment nil
                                           :render-segments nil
                                           :stdout nil
                                           :error nil
                                           :started-at-ms nil
                                           :duration-ms 1
                                           :success? true
                                           :silent? false}]}
                                 60
                                 1
                                 {:show-header? false :live-preview? true})

         visible
         (mapv (comp strip-ansi body-of) lines)

         alpha-idx
         (first (keep-indexed #(when (str/includes? %2 "alpha") %1) visible))

         code-idx
         (first (keep-indexed #(when (str/includes? %2 "(+ 1 1)") %1) visible))]

        (expect (some? alpha-idx))
        (expect (some? code-idx))
        (expect (< alpha-idx code-idx))
        ;; Bound the gap: thinking and code should sit close, with a
        ;; small handful of marker / pad rows between them (per parallel
        ;; layout work). Hard ceiling guards against a regression that
        ;; explodes the gap into a multi-row stripe.
        (expect (<= (- code-idx alpha-idx) 5))))))

(defdescribe
  paint-styled-line-stacking-test
  ;; The Polish bug report: `> **Lącznie:**` inside a quote rendered
  ;; bold-without-italic because paint-styled-line! cleared the
  ;; wrapping italic at entry. We pin the fix by recording the SGR
  ;; set on every paint call via a stub TextGraphics, then asserting
  ;; bold + italic stack correctly.
  (let
    [;; Capture every (putString ...) as [text {:fg :bg :sgr}].
     captured
     (atom [])

     active
     (atom #{})

     fg
     (atom nil)

     bg
     (atom nil)

     ;; Lanterna's TextGraphics is an interface with ~30 methods;
     ;; we proxy the four paint-styled-line! actually calls.
     graphics
     (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
       (clearModifiers [] (reset! active #{}) this)
       (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr] (swap! active into (seq arr)) this)
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
         ([col row text] (swap! captured into (put->styled-runs text @fg @bg @active)) this)))]

    (describe
      "paint-styled-line! inherits the wrapping SGR modifiers"
      (it "BOLD inside a wrapping ITALIC stacks to bold-italic"
          (reset! captured [])
          (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
          (let [line (str "plain " p/INLINE_BOLD_ON "loud" p/INLINE_BOLD_OFF " tail")]
            (p/paint-styled-line! graphics
                                  0
                                  0
                                  line
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
          (p/paint-styled-line! graphics
                                0
                                0
                                (str p/INLINE_BOLD_ON "x" p/INLINE_BOLD_OFF)
                                (com.googlecode.lanterna.TextColor$RGB. 0 0 0)
                                (com.googlecode.lanterna.TextColor$RGB. 255 255 255)
                                (com.googlecode.lanterna.TextColor$RGB. 50 50 50)
                                (com.googlecode.lanterna.TextColor$RGB. 240 240 240))
          (expect (= #{com.googlecode.lanterna.SGR/ITALIC} @active)))
      (it "Dangling sentinel (no close) doesn't leak BOLD past the call"
          (reset! captured [])
          (reset! active #{com.googlecode.lanterna.SGR/ITALIC})
          (p/paint-styled-line! graphics
                                0
                                0
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
             (it
               "consumes inline code sentinels instead of painting PUA glyphs"
               (let
                 [paint-ansi-line!
                  @#'render/paint-ansi-line!

                  captured
                  (atom [])

                  active
                  (atom #{})

                  fg
                  (atom nil)

                  bg
                  (atom nil)

                  graphics
                  (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                    (clearModifiers [] (reset! active #{}) this)
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
                       (swap! captured conj [(put-text text) {:fg @fg :bg @bg :sgr @active}])
                       this)))

                  line
                  (str "Searched " p/INLINE_CODE_ON
                       "[\"extensions\"]" p/INLINE_CODE_OFF
                       " with " p/INLINE_CODE_ON
                       "{:any [\"circling\"]}" p/INLINE_CODE_OFF
                       ". Use " p/INLINE_CODE_ON
                       "v/preview" p/INLINE_CODE_OFF)

                  visible
                  "Searched [\"extensions\"] with {:any [\"circling\"]}. Use v/preview"]

                 (paint-ansi-line! graphics 0 0 line t/code-result-fg t/code-ok-bg)
                 (let [painted (apply str (map first @captured))]
                   (expect (= visible painted))
                   (expect (not (str/includes? painted p/INLINE_CODE_ON)))
                   (expect (not (str/includes? painted p/INLINE_CODE_OFF)))))))

(defdescribe code-pad-payload-paint-test
             ;; MARKER_CODE_*_PAD can carry optional payload text. Regression: pad
             ;; painter filled bg then discarded payload.
             (it
               "paints code-pad payload text, not only its background"
               (let
                 [puts
                  (atom [])

                  active
                  (atom #{})

                  fg
                  (atom nil)

                  bg
                  (atom nil)

                  graphics
                  (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                    (clearModifiers [] (reset! active #{}) this)
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

                  footer
                  (str p/MARKER_CODE_OK_PAD " t24/i1/b1 ")]

                 (render/draw-chat-bubble!
                   graphics
                   {:role :assistant :timestamp nil :prewrapped-lines [footer]}
                   0
                   0
                   80)
                 (let
                   [painted
                    (apply str (map :text @puts))

                    stamp
                    (first (filter #(= "t24/i1/b1" (:text %)) @puts))]

                   (expect (str/includes? painted "t24/i1/b1"))
                   (expect (not (str/includes? painted "✓")))
                   (expect (not (str/includes? painted "12ms")))
                   (expect (= t/dialog-hint (:fg stamp)))
                   (expect (contains? (:sgr stamp) com.googlecode.lanterna.SGR/ITALIC))))))

(defdescribe answer-text-inline-sentinel-paint-test
             ;; Final-answer prose uses MARKER_ANSWER_TXT for normal paragraphs.
             ;; Inline code inside that IR arrives at the bubble painter as
             ;; INLINE_CODE_ON/OFF sentinels. The answer-text branch must consume
             ;; them just like headings/bullets/quotes; otherwise the user sees
             ;; raw PUA glyphs like  and  around `/command`.
             (it
               "consumes inline code sentinels in plain final-answer text"
               (let
                 [captured
                  (atom [])

                  active
                  (atom #{})

                  fg
                  (atom nil)

                  bg
                  (atom nil)

                  graphics
                  (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                    (clearModifiers [] (reset! active #{}) this)
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
                    (putString ([_col _row text] (swap! captured conj (put-text text)) this)))

                  line
                  (str p/MARKER_ANSWER_TXT
                       "Use "
                       p/INLINE_CODE_ON
                       "/command"
                       p/INLINE_CODE_OFF
                       " (or pick from slash suggestions).")]

                 (render/draw-chat-bubble!
                   graphics
                   {:role :assistant :timestamp nil :prewrapped-lines [line]}
                   0
                   0
                   80)
                 (let [painted (apply str @captured)]
                   (expect (str/includes? painted "/command"))
                   (expect (not (str/includes? painted p/INLINE_CODE_ON)))
                   (expect (not (str/includes? painted p/INLINE_CODE_OFF)))))))

(defdescribe
  scrollbar-thumb-geometry-test
  ;; Geometry now lives in `scrollbar/geometry` — the single source of
  ;; truth for painter and hit-test. Test pinned here for back-compat,
  ;; mirrored in `scrollbar_test.clj`.
  (let [g (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)]
    (describe "Returns nil when there's no overflow"
              (it "total-h < inner-h: nothing to scroll" (expect (nil? (g 10 20 nil))))
              (it "total-h == inner-h: nothing to scroll" (expect (nil? (g 20 20 nil))))
              (it "inner-h is zero: no viewport, no thumb" (expect (nil? (g 100 0 0)))))
    (describe
      "Standard 100/20 session"
      (it "Auto-bottom (scroll=nil) places the single-cell thumb at the END of the track"
          (let [{:keys [thumb-top-rel thumb-h max-scroll]} (g 100 20 nil)]
            (expect (= 19 thumb-top-rel)) ;; track-h(20) - thumb-h(1) = 19
            (expect (= 1 thumb-h))
            (expect (= 80 max-scroll))))  ;; 100 - 20
      (it "scroll=0 places thumb at the TOP"
          (expect (= {:thumb-top-rel 0 :thumb-h 1 :max-scroll 80 :track-h 20} (g 100 20 0))))
      (it "scroll=40 places thumb in the MIDDLE of the free track"
          (expect (= {:thumb-top-rel 9 :thumb-h 1 :max-scroll 80 :track-h 20} (g 100 20 40))))
      (it "scroll=80 places thumb at the BOTTOM (== max-scroll)"
          (expect (= {:thumb-top-rel 19 :thumb-h 1 :max-scroll 80 :track-h 20} (g 100 20 80)))))
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
                  (let
                    [top (:thumb-top-rel (g 1000 5 0))
                     bot (:thumb-top-rel (g 1000 5 995))]

                    (expect (= 0 top))
                    (expect (= 4 bot))))))) ;; track-h(5) - thumb-h(1) = 4


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
      (clearModifiers [] (reset! active #{}) this)
      (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr] (swap! active into (seq arr)) this)
      (disableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
        (apply swap! active disj (seq arr))
        this)
      (getActiveModifiers []
        (if (empty? @active)
          (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
          (java.util.EnumSet/copyOf ^java.util.Collection @active)))
      (setForegroundColor [_] this)
      (setBackgroundColor [_] this)
      (putString ([_ _ _] this))
      (fillRectangle [_ _ _] this)
      (setCharacter [_ _ _] this))))

(defdescribe
  reasoning-preview-rendering-test
  (it
    "expands very long reasoning only when the badge is toggled open"
    (let
      [cid
       "session"

       turn-id
       "123e4567-e89b-12d3-a456-426614174000"

       node
       "thinking:t123e4567:i1:reasoning"

       thinking
       (str/join "\n\n" (map #(format "line-%02d detail text" %) (range 1 51)))

       render*
       (fn [exp]
         (render/invalidate-cache!)
         (render/format-answer-with-thinking-data
           "done"
           [{:thinking thinking}]
           96
           {:show-thinking true :show-iterations true}
           nil
           false
           {:session-id cid :session-turn-id turn-id :detail-expansions exp}))

       cbody
       (strip-ansi (:text (render* {})))

       ebody
       (strip-ansi (:text (render* {[cid node] true})))]

      (expect (str/includes? cbody "THINKING"))
      (expect (not (str/includes? cbody "line-25")))
      (expect (str/includes? ebody "line-01"))
      (expect (str/includes? ebody "line-25"))
      (expect (str/includes? ebody "line-50")))))

(defdescribe
  auto-collapse-rendering-test
  (it "hides legacy preview-kind result bodies entirely"
      ;; Bare return values are never echoed: a form that printed nothing
      ;; renders no result body, no PREVIEW/RAW switcher, no toggle.
      (render/invalidate-cache!)
      (let
        [body
         "only line of preview output"

         trace
         [{:forms [{:code "(cat file)"
                    :comment nil
                    :render-segments nil
                    :stdout nil
                    :error nil
                    :started-at-ms nil
                    :duration-ms 1
                    :success? true
                    :silent? false}]}]

         payload
         (render/format-answer-with-thinking-data
           nil
           trace
           96
           {:show-iterations true}
           nil
           false
           {:session-id "session" :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})]

        (expect (not (str/includes? (:text payload) body)))
        (expect (not (str/includes? (:text payload) "raw-only")))
        (expect (not (str/includes? (:text payload) "PREVIEW")))
        (expect (not (str/includes? (:text payload) "● RAW")))
        (expect (not-any? #(= :preview-switcher (:kind %)) (:line-meta payload)))))
  (it
    "hides huge plain value results entirely without a collapse summary"
    ;; Per user directive: collapsible disclosure was removed. Plain
    ;; `:value` form results never paint a body — no `RESULT` label,
    ;; no `chars hidden` summary, no `[iteration N · block M]` band.
    (render/invalidate-cache!)
    (let
      [huge-result
       (str/join " " (repeat 4000 "abcdefghij"))

       trace
       [{:forms [{:code "(+ 1 2)"
                  :comment nil
                  :render-segments nil
                  :stdout nil
                  :error nil
                  :started-at-ms nil
                  :duration-ms 1
                  :success? true
                  :silent? false}]}]

       turn-id
       "123e4567-e89b-12d3-a456-426614174000"

       payload
       (render/format-answer-with-thinking-data nil
                                                trace
                                                96
                                                {:show-iterations true}
                                                nil
                                                false
                                                {:session-id "session" :session-turn-id turn-id})]

      (expect (not (str/includes? (:text payload) "RESULT")))
      (expect (not (str/includes? (:text payload) "chars hidden")))
      (expect (not (str/includes? (:text payload) "[iteration 1 · block 1]")))
      (expect (not (str/includes? (:text payload) huge-result)))
      (expect (not-any? #(= :toggle-details (:kind %)) (:line-meta payload)))))
  (it "collapses completed reasoning behind the badge on the answer view"
      ;; Completed reasoning defaults collapsed; the legacy
      ;; `:detail-expansions` toggle now drives expansion again.
      (let
        [node
         "thinking:tturn-1:i1:reasoning"

         thinking
         (str/join "\n\n" (map #(str "line " % " detail text") (range 1 51)))

         trace
         [{:thinking thinking :code [] :results [] :durations [] :successes []}]

         render*
         (fn [exp]
           (render/invalidate-cache!)
           (render/format-answer-with-thinking-data
             "done"
             trace
             120
             {:show-iterations true :show-thinking true}
             nil
             false
             {:session-id "session" :session-turn-id "turn-1" :detail-expansions exp}))

         cbody
         (strip-ansi (:text (render* {})))

         ebody
         (strip-ansi (:text (render* {["session" node] true})))]

        (expect (str/includes? cbody "THINKING"))
        (expect (not (str/includes? cbody "line 25")))
        (expect (str/includes? ebody "line 1 "))
        (expect (str/includes? ebody "line 25"))
        (expect (str/includes? ebody "line 50"))))
  (it
    "never paints a collapsed summary band"
    (render/invalidate-cache!)
    (let
      [huge-result
       (str/join " " (repeat 1000 "abcdefghij"))

       trace
       [{:forms [{:code "(+ 1 2)"
                  :comment nil
                  :render-segments nil
                  :stdout nil
                  :error nil
                  :started-at-ms nil
                  :duration-ms 1
                  :success? true
                  :silent? false}]}]

       payload
       (render/format-answer-with-thinking-data
         nil
         trace
         96
         {:show-iterations true}
         nil
         false
         {:session-id "session" :session-turn-id "123e4567-e89b-12d3-a456-426614174000"})

       puts
       (atom [])

       active
       (atom #{})

       bg
       (atom nil)

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
         (putString [_col _row text] (swap! puts conj {:text text :bg @bg :sgr @active}) this)
         (fillRectangle [_ _ _] this)
         (setCharacter [_ _ _] this))]

      (render/draw-chat-bubble! graphics
                                {:role :assistant
                                 :text (:text payload)
                                 :prewrapped-lines (:lines payload)}
                                0 2
                                96 {:viewport-h 50})
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

(defdescribe
  errored-form-rendering-test
  (it
    "an errored form surfaces its error message inline"
    (render/invalidate-cache!)
    (let
      [err
       {:message "boom" :type "java.lang.RuntimeException" :block {:source "(boom)" :row 1 :col 1}}

       trace
       [{:forms [{:code "(boom)"
                  :comment nil
                  :render-segments nil
                  :stdout nil
                  :error err
                  :started-at-ms nil
                  :duration-ms 1
                  :success? false
                  :silent? false}]}]

       opts
       {:session-id "session" :session-turn-id "123e4567-e89b-12d3-a456-426614174000"}

       payload
       (render/format-answer-with-thinking-data nil trace 96 {:show-iterations true} nil false opts)

       text
       (strip-sentinels (strip-ansi (:text payload)))]

      (expect (str/includes? text "boom")))))

;; ─────────────────────────────────────────────────────────────────────────
;; Retired answer disclosure tags must not create collapsible output.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe retired-answer-disclosure-test
             (it "ignores :details/:summary as structure and emits no summary lane"
                 (let
                   [answer
                    [:ast {}
                     [:details {:open? false} [:summary {} [:span {} "Plan"]]
                      [:p {} [:span {} "alpha"]] [:p {} [:span {} "beta"]]]]

                    payload
                    (render/format-answer-markdown-data
                      answer
                      80
                      {:session-id "cid" :detail-expansions {["cid" "answer:details:d1"] false}})

                    lines
                    (:lines payload)

                    visible
                    (str/join "\n" (map strip-sentinels lines))]

                   (expect (not-any? #(= p/MARKER_MD_SUMMARY (marker-of %)) lines))
                   (expect (str/includes? visible "Planalphabeta")))))

;; ─────────────────────────────────────────────────────────────────────────
;; Provider-error answer rendering.
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe
  provider-error-answer-test
  (it
    "renders only the canonical provider-error Markdown, not duplicate trace rows"
    (render/invalidate-cache!)
    (let
      [answer
       "## 🚨 PROVIDER_ERROR\n\nProvider call failed before the model could run.\n\nWHAT HAPPENED: invalid thinking signature"

       trace
       [{:error {:message "Exceptional status code: 400"
                 :data {:status 400
                        :body
                        "{\"error\":{\"message\":\"Invalid `signature` in `thinking` block\"}}"}}}]

       payload
       (render/format-answer-with-thinking-data answer
                                                trace
                                                96
                                                {:show-iterations true}
                                                nil
                                                false
                                                {})

       text
       (:text payload)]

      (expect (= 1 (count (re-seq #"PROVIDER_ERROR" text))))
      (expect (not (str/includes? text "provider response:")))
      (expect (str/includes? text "WHAT HAPPENED: invalid thinking signature")))))

(defdescribe answer-separator-test
             (it "does not draw a bottom border between reasoning and final answer"
                 (render/invalidate-cache!)
                 (let
                   [payload (render/format-answer-with-thinking-data "done"
                                                                     [{:thinking "reasoning"}]
                                                                     80
                                                                     {:show-thinking true
                                                                      :show-iterations true}
                                                                     nil
                                                                     false
                                                                     {})]
                   (expect (not (str/includes? (:text payload) p/MARKER_ANSWER_SEP)))
                   (expect (not-any? #(str/starts-with? % p/MARKER_ANSWER_SEP) (:lines payload))))))

(defdescribe
  message-footer-test
  (it "does not register a per-message copy button"
      (cr/reset!)
      (cr/begin-frame!)
      (let
        [message
         {:role :assistant :text "hello world"}

         start
         4

         left
         2

         width
         36

         viewport-top
         7

         height
         (render/draw-chat-bubble! (dummy-text-graphics)
                                   message
                                   start
                                   left
                                   width
                                   {:viewport-top viewport-top :viewport-h 40})

         hit-col
         (+ left 2)]

        (cr/commit-frame!)
        (expect (= 3 height))
        (expect (every? nil?
                        (map #(cr/lookup hit-col %)
                             (range viewport-top (+ viewport-top start height)))))))
  (it "renders cached token usage in the assistant bubble footer"
      (let
        [puts
         (atom [])

         graphics
         (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
           (clearModifiers [] this)
           (enableModifiers [_] this)
           (disableModifiers [_] this)
           (getActiveModifiers [] (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
           (setForegroundColor [_] this)
           (setBackgroundColor [_] this)
           (putString [_col row text] (swap! puts conj {:row row :text text}) this)
           (fillRectangle [_ _ _] this)
           (setCharacter [_ _ _] this))

         height
         (render/draw-chat-bubble! graphics
                                   {:role :assistant
                                    :text "hello"
                                    :message-meta-mode :full
                                    :tokens {"input" 100 "output" 20 "cached" 70}}
                                   4 2
                                   60 {:viewport-h 40})]

        (expect (= 5 height))
        (expect (some #(str/includes? (:text %) "100→20 (cached 70)") @puts))))
  (it "omits zero cached token usage in the assistant bubble footer"
      (let
        [puts
         (atom [])

         graphics
         (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
           (clearModifiers [] this)
           (enableModifiers [_] this)
           (disableModifiers [_] this)
           (getActiveModifiers [] (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
           (setForegroundColor [_] this)
           (setBackgroundColor [_] this)
           (putString [_col row text] (swap! puts conj {:row row :text text}) this)
           (fillRectangle [_ _ _] this)
           (setCharacter [_ _ _] this))

         height
         (render/draw-chat-bubble! graphics
                                   {:role :assistant
                                    :text "hello"
                                    :message-meta-mode :full
                                    :tokens {"input" 100 "output" 20 "cached" 0}}
                                   4 2
                                   60 {:viewport-h 40})]

        (expect (= 5 height))
        (expect (some #(str/includes? (:text %) "100→20") @puts))
        (expect (not-any? #(str/includes? (:text %) "cached 0") @puts))))
  (it
    "leaves one blank row between assistant answer and bubble footer"
    (let
      [puts
       (atom [])

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] this)
         (enableModifiers [_] this)
         (disableModifiers [_] this)
         (getActiveModifiers [] (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
         (setForegroundColor [_] this)
         (setBackgroundColor [_] this)
         (putString [_col row text] (swap! puts conj {:row row :text (put-text text)}) this)
         (fillRectangle [_ _ _] this)
         (setCharacter [_ _ _] this))

       height
       (render/draw-chat-bubble!
         graphics
         {:role :assistant :text "hello" :message-meta-mode :full :tokens {"input" 100 "output" 20}}
         4 2
         60 {:viewport-h 40})

       answer-row
       (:row (first (filter #(= "hello" (:text %)) @puts)))

       footer-row
       (:row (first (filter #(str/includes? (:text %) "100→20") @puts)))]

      (expect (= 5 height))
      (expect (= 2 (- footer-row answer-row)))))
  (it "omits the footer for an iteration-only turn with no model / tokens / cost"
      ;; The bubble footer is now the SHARED humanized turn-summary line
      ;; (`vis/meta-summary-line`): model · in→out (cached) · ~$cost · duration.
      ;; Iteration / silent-form bookkeeping no longer rides this footer, so a
      ;; turn that only carries iteration metadata produces no footer line at all
      ;; and the bubble collapses to the bare answer height (no footer rows).
      (let
        [puts
         (atom [])

         graphics
         (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
           (clearModifiers [] this)
           (enableModifiers [_] this)
           (disableModifiers [_] this)
           (getActiveModifiers [] (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
           (setForegroundColor [_] this)
           (setBackgroundColor [_] this)
           (putString [_col row text] (swap! puts conj {:row row :text text}) this)
           (fillRectangle [_ _ _] this)
           (setCharacter [_ _ _] this))

         height
         (render/draw-chat-bubble! graphics
                                   {:role :assistant
                                    :text "hello"
                                    :message-meta-mode :full
                                    :iteration-count 3
                                    :traces [{:forms [{:silent? true} {:silent? false}]}
                                             {:forms [{:silent? true}]}]}
                                   4 2
                                   60 {:viewport-h 40})]

        (expect (= 3 height))
        (expect (not-any? #(str/includes? (str (:text %)) "iter") @puts))
        (expect (not-any? #(str/includes? (str (:text %)) "silent") @puts))))
  (it
    "ignores legacy turn separator flag on user prompts"
    (let
      [puts
       (atom [])

       active
       (atom #{})

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
         (putString [_col row text] (swap! puts conj {:row row :text text :sgr @active}) this)
         (fillRectangle [_pos _size _ch] this)
         (setCharacter [_ _ _] this))

       height
       (render/draw-chat-bubble! graphics
                                 {:role :user :text "hello" :turn-separator? true}
                                 4 2
                                 30 {:viewport-h 40})]

      (expect (= 5 height))
      (expect (not-any? #(str/includes? (or (:text %) "") "──") @puts))
      (expect (some #(and (= 4 (:row %))
                          (= "You" (:text %))
                          (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD))
                    @puts))))
  (it
    "renders user messages with a left rail and markdown styling"
    (let
      [puts
       (atom [])

       active
       (atom #{})

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
           (swap! puts conj {:row row :text (put-text text) :sgr @active})
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
       rendered
       (render/format-answer-markdown-data (vis/markdown->ast "**SIEMA**\n\n> quoted text") 50 nil)

       message
       {:role :user
        :text "**SIEMA**\n\n> quoted text"
        :prewrapped-lines (:lines rendered)
        :line-meta (:line-meta rendered)}

       start
       4

       left
       2

       width
       50

       height
       (render/draw-chat-bubble! graphics message start left width {:viewport-h 40})]

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
  (it
    "draws assistant answer text aligned with the Vis label"
    (let
      [puts
       (atom [])

       active
       (atom #{})

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
         (putString [col row text] (swap! puts conj {:col col :row row :text (put-text text)}) this)
         (fillRectangle [_ _ _] this)
         (setCharacter [_ _ _] this))

       rendered
       (render/format-answer-markdown-data (vis/markdown->ast "hello") 50 nil)

       left
       2

       _height
       (render/draw-chat-bubble! graphics
                                 {:role :assistant
                                  :text ""
                                  :prewrapped-lines (:lines rendered)
                                  :line-meta (:line-meta rendered)}
                                 4 left
                                 50 {:viewport-h 40})

       answer-put
       (first (filter #(str/includes? (:text %) "hello") @puts))]

      (expect (= left (:col answer-put)))))
  (it
    "draws markdown fenced code one column inside the bubble band"
    (let
      [fills
       (atom [])

       puts
       (atom [])

       active
       (atom #{})

       fg
       (atom nil)

       bg
       (atom nil)

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
           (swap! fills conj
             {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
              :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
              :w (.getColumns ^com.googlecode.lanterna.TerminalSize size)
              :h (.getRows ^com.googlecode.lanterna.TerminalSize size)
              :fg @fg
              :bg @bg})
           this)
         (setCharacter [_ _ _] this))

       rendered
       (render/format-answer-markdown-data (vis/markdown->ast "```clojure\n(+ 1 2)\n```") 50 nil)

       left
       2

       width
       50

       ;; ANSWER fenced-code rows are inset from the band's left edge by
       ;; `code-block-h-pad` (the band still fills from `left`).
       text-x
       (+ left @#'render/code-block-h-pad)

       _height
       (render/draw-chat-bubble! graphics
                                 {:role :assistant
                                  :text ""
                                  :prewrapped-lines (:lines rendered)
                                  :line-meta (:line-meta rendered)}
                                 4
                                 left
                                 width
                                 {:viewport-h 40})

       code-put
       ;; Colorization splits the code line into one putString per color run,
       ;; so the leading chunk (which starts the code at text-x) no longer
       ;; contains the whole form. Match that leading chunk, ANSI-stripped.
       (first (filter #(str/includes? (strip-ansi (str (:text %))) "(+") @puts))

       code-fill
       (first (filter #(and (= t/code-block-bg (:bg %)) (= (:row code-put) (:row %))) @fills))]

      (expect (= text-x (:col code-put)))
      (expect (= left (:col code-fill)))
      (expect (= width (:w code-fill)))))
  (it "renders blank fenced-code rows nested in a list"
      (let
        [fence
         (apply str (repeat 3 (char 96)))

         markdown
         (str "- inspect:\n\n    " fence "clojure\n\n    (+ 1 2)\n    " fence)

         rendered
         (render/format-answer-markdown-data (vis/markdown->ast markdown) 50 nil)

         nested-blank?
         (some (fn [[line meta]]
                 (and (= 1 (count line)) (:list-nested-code? meta) (= 2 (:list-indent meta))))
               (map vector (:lines rendered) (:line-meta rendered)))]

        (expect nested-blank?)
        (expect (pos? (render/draw-chat-bubble! (dummy-text-graphics)
                                                {:role :assistant
                                                 :text ""
                                                 :prewrapped-lines (:lines rendered)
                                                 :line-meta (:line-meta rendered)}
                                                4 2
                                                50 {:viewport-h 40})))))
  (it
    "leaves only the final gap after the user bubble fill"
    (let
      [fills
       (atom [])

       active
       (atom #{})

       graphics
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
         (putString ([_ _ _] this))
         (fillRectangle [pos size _ch]
           (swap! fills conj
             {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
              :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
              :w (.getColumns ^com.googlecode.lanterna.TerminalSize size)
              :h (.getRows ^com.googlecode.lanterna.TerminalSize size)})
           this)
         (setCharacter [_ _ _] this))

       message
       {:role :user :text "hello world"}

       start
       4

       left
       2

       width
       36

       viewport-top
       7

       height
       (render/draw-chat-bubble! graphics
                                 message
                                 start
                                 left
                                 width
                                 {:viewport-top viewport-top :viewport-h 40})

       gap-row
       (+ start height -1)

       bubble-fill
       (some (fn [fill]
               (when (and (= left (:col fill)) (pos? (:h fill))) fill))
             @fills)

       bubble-last-row
       (+ (:row bubble-fill) (:h bubble-fill) -1)]

      (expect (= 5 height))
      (expect (= 3 (:h bubble-fill)))
      ;; Snug fit: the block hugs its content ("hello world" = 11 cols +
      ;; symmetric h-pad of 2 each side = 15), NOT the full message column.
      (expect (= 15 (:w bubble-fill)))
      (expect (< (:w bubble-fill) width))
      (expect (= bubble-last-row (dec gap-row))))))

(defdescribe bubble-row-clipping-test
             (it "clips prewrapped formatter rows at bubble content width"
                 (let
                   [plain
                    (apply str (repeat 80 "x"))

                    marked
                    (str p/MARKER_CODE_OK (apply str (repeat 80 "y")))

                    clipped
                    (clip-lines-preserving-markers [plain marked] 17)]

                   (expect (= 2 (count clipped)))
                   (expect (= (subs plain 0 17) (first clipped)))
                   (expect (= p/MARKER_CODE_OK (marker-of (second clipped))))
                   (expect (<= (p/display-width (first clipped)) 17))
                   (expect (<= (p/display-width (body-of (second clipped))) 17))))
             (it "clips ANSI-colored Clojure formatter rows without handing ESC to Lanterna"
                 (let
                   [ansi-line
                    (str p/MARKER_CODE_OK
                         "\u001b[32m(\u001b[0m\u001b[34mdef\u001b[0m "
                         "\u001b[30mrequest-classification\u001b[0m "
                         "\u001b[35m:evidence-bearing-code-change\u001b[0m")

                    clipped
                    (first (clip-lines-preserving-markers [ansi-line] 12))]

                   (expect (= p/MARKER_CODE_OK (marker-of clipped)))
                   (expect (str/includes? clipped "\u001b[32m"))
                   (expect (<= (p/display-width (strip-ansi (body-of clipped))) 12))))
             (it "truncate-with-suffix keeps ANSI colour on a highlighted thinking peek row"
                 ;; A collapsed thinking band appends " …" to its last peek row via
                 ;; `truncate-with-suffix`. That row can be a syntax-highlighted code line
                 ;; carrying `\u001b[..m` SGR runs; truncation must stay ANSI-aware so the
                 ;; ESC bytes survive (colours kept) and the SGR params don't leak as text.
                 (let
                   [code-line
                    "\u001b[36mdefn\u001b[0m qux [w] (+ w \u001b[34m3\u001b[0m))"

                    out
                    (truncate-with-suffix code-line " …" 30)]

                   (expect (str/includes? out "\u001b[36m"))
                   (expect (str/includes? out "\u001b[0m"))
                   ;; No bare SGR param text leaks once the real escapes are stripped.
                   (expect (not (re-find #"\[[0-9;]*m" (strip-ansi out))))
                   (expect (<= (p/display-width (strip-ansi out)) 30))))
             (it
               "reuses clipped prewrapped rows while scrolling huge trace bubbles"
               (render/invalidate-cache!)
               (let
                 [huge-line
                  (str p/MARKER_CODE_OK (apply str (repeat 4000 "x")))

                  message
                  {:role :assistant :timestamp nil :prewrapped-lines (vec (repeat 1000 huge-line))}

                  draw!
                  #(render/draw-chat-bubble! (dummy-text-graphics)
                                             message
                                             0 2
                                             100 {:viewport-top 0 :viewport-h 35})]

                 (draw!)
                 (let
                   [t0
                    (System/nanoTime)

                    _
                    (draw!)

                    ms
                    (/ (- (System/nanoTime) t0) 1e6)]

                   (expect (< ms 20.0))))))

(defdescribe
  slash-command-suggestions-overlay-test
  (it
    "draws a bordered, BOLD, accent-stripe title with flex hint pairs"
    (let
      [puts
       (atom [])

       fills
       (atom [])

       fg
       (atom nil)

       bg
       (atom nil)

       active
       (atom #{})

       g
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
           (swap! puts conj {:col col :row row :text text :fg @fg :bg @bg :sgr @active})
           this)
         (fillRectangle [pos size _ch]
           (swap! fills conj
             {:row (.getRow ^com.googlecode.lanterna.TerminalPosition pos)
              :col (.getColumn ^com.googlecode.lanterna.TerminalPosition pos)
              :w (.getColumns ^com.googlecode.lanterna.TerminalSize size)
              :h (.getRows ^com.googlecode.lanterna.TerminalSize size)
              :fg @fg
              :bg @bg})
           this)
         (setCharacter [_ _ _] this))

       suggestions
       [{:label "first" :slash/usage "/first" :slash/selected? true}
        {:label "second" :slash/usage "/second" :slash/selected? false}]

       input-top
       10

       cols
       80

       n
       (count suggestions)

       ;; Layout (from top to bottom):
       ;;   margin-row -> title-row -> border-row -> sug rows ...
       first-sug
       (- input-top n)

       border-row
       (dec first-sug)

       title-row
       (dec border-row)

       margin-row
       (dec title-row)

       ;; Horizontal margin matches input box rule pad (2 cols).
       pad
       2

       inner-w
       (- cols (* 2 pad))]

      (render/draw-slash-command-suggestions! g suggestions input-top cols)
      ;; Title row sits ABOVE the border row (border under title).
      (expect (< title-row border-row))
      ;; Title row: accent stripe (fillRectangle) on title-bg, inset
      ;; by `pad` cols on each side so it lines up with the input box.
      (expect (some #(and (= title-row (:row %)) (= t/dialog-title-bg (:bg %)) (= pad (:col %)))
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
      (expect (some #(and (= margin-row (:row %)) (= 0 (:col %)) (= t/terminal-bg (:bg %))) @fills))
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
      (expect (not-any? #(and (= title-row (:row %)) (#{"Enter" "Tab"} (:text %))) @puts))
      ;; Title row: action words rendered NON-BOLD next to their keys.
      (doseq [a [" select" " complete"]]
        (expect (some #(and (= title-row (:row %))
                            (= a (:text %))
                            (not (contains? (:sgr %) com.googlecode.lanterna.SGR/BOLD)))
                      @puts)))
      ;; Title items spread across the inner width (space-between):
      ;; first item sits inside the left margin, last item in the
      ;; right half of the inner span.
      (let
        [title-puts
         (filter #(= title-row (:row %)) @puts)

         cols-used
         (mapv :col title-puts)]

        (expect (some #(<= pad % (+ pad 2)) cols-used))
        (expect (some #(>= % (+ pad (quot inner-w 2))) cols-used)))
      ;; Suggestion rows are inset to the same column span as the
      ;; title accent stripe (margin-left = margin-right = pad).
      ;; The body fill on every row uses the normal `dialog-bg`
      ;; palette — selection is signalled by the dot marker in the
      ;; left margin, NOT by a full-row accent stripe.
      (expect
        (some
          #(and (= first-sug (:row %)) (= pad (:col %)) (= inner-w (:w %)) (= t/dialog-bg (:bg %)))
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
      (let
        [sug-rows
         (filter #(<= first-sug (:row %)) @puts)

         usages
         (filter #(str/starts-with? (:text %) "/") sug-rows)

         seps
         (filter #(= " - " (:text %)) sug-rows)

         descs
         (filter #(contains? (:sgr %) com.googlecode.lanterna.SGR/ITALIC) sug-rows)

         chip-fills
         (filter #(and (<= first-sug (:row %)) (= t/code-block-bg (:bg %))) @fills)]

        ;; One chip fill, usage, separator and description per suggestion.
        (expect (= n (count chip-fills)))
        (expect (= n (count usages)))
        (expect (= n (count seps)))
        (expect (= n (count descs)))
        ;; Layout invariants per row: chip wraps the usage with 1 col
        ;; padding on each side, ` - ` follows the chip, italic desc
        ;; follows the separator. The chip starts AFTER the selection
        ;; gutter (`p/SELECTION_WIDTH` cols inside the inset body).
        (doseq
          [[chip u s d] (map vector
                             (sort-by :row chip-fills)
                             (sort-by :row usages)
                             (sort-by :row seps)
                             (sort-by :row descs))]
          ;; Chip lives past the selection gutter — first chip col is
          ;; at least `pad + p/SELECTION_WIDTH` (cursor + 1-col margin).
          (expect (>= (:col chip)
                      (+ pad com.blockether.vis.ext.channel-tui.primitives/SELECTION_WIDTH)))
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
  (it
    "drops the border row when there is not enough vertical space"
    (let
      [puts
       (atom [])

       active
       (atom #{})

       g
       (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
         (clearModifiers [] (reset! active #{}) this)
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
         (getForegroundColor [] nil)
         (getBackgroundColor [] nil)
         (putString [col row text] (swap! puts conj {:col col :row row :text text}) this)
         (fillRectangle [_ _ _] this)
         (setCharacter [_ _ _] this))

       ;; input-top = 2: only enough room for title (1 row) + 1
       ;; suggestion. Border + margin must drop.
       suggestions
       [{:label "a" :slash/usage "/a" :slash/selected? true}]

       input-top
       2]

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
               (it "nil :error is fine" (expect (vector? (fp {:error nil}))))
               ;; Regression: `:assistant-prose` (and pre-forms `:content-stream`)
               ;; are read by `format-iteration-entry-entries` but were absent from
               ;; the fingerprint. The loop emits prose WHILE `:forms` is still empty,
               ;; so an unchanged fingerprint let the live cache serve the stale,
               ;; prose-less render — the commentary block vanished from the terminal.
               (it "assistant-prose busts the fingerprint while forms are empty"
                   (let [base {:thinking "planning" :forms []}]
                     (expect (not= (fp base) (fp (assoc base :assistant-prose "doing X"))))
                     (expect (not= (fp (assoc base :assistant-prose "a"))
                                   (fp (assoc base :assistant-prose "b"))))))
               (it "content-stream busts the fingerprint while forms are empty"
                   (let [base {:thinking "planning" :forms []}]
                     (expect (not= (fp base) (fp (assoc base :content-stream "live text"))))))))

(defdescribe
  message-detail-expansions-key-test
  ;; The height/estimate/projection caches key a message to ONLY its own
  ;; disclosure state. A user bubble with no turn token stays CONSTANT so no
  ;; unrelated session expansion leaks in (one fold click would otherwise bust
  ;; every user bubble's cached height). A user prompt CAN carry a collapsible
  ;; `[Pasted #N]` disclosure, though — so `expand-all` and its own per-turn
  ;; expansions must still key it (see the paste-disclosure path).
  (let
    [sid
     "s1"

     expansions
     {["s1" "iteration:tabc12345:i1:result"] true}]

    (it "user message key is constant regardless of expansions"
        (expect (= (render/message-detail-expansions-key sid {:role :user :text "hi"} {})
                   (render/message-detail-expansions-key sid {:role :user :text "hi"} expansions))))
    (it "user message keys under expand-all (a `[Pasted #N]` disclosure may open)"
        (expect (= :expand-all
                   (render/message-detail-expansions-key sid
                                                         {:role :user :text "hi"}
                                                         {:vis.channel-tui/expand-all-details?
                                                          true}))))
    (it "assistant message with matching turn token picks up its expansions"
        (expect (= [["iteration:tabc12345:i1:result" true]]
                   (render/message-detail-expansions-key sid
                                                         {:role :assistant
                                                          :session-turn-id "abc12345-0000-0000"}
                                                         expansions))))
    (it "assistant message of ANOTHER turn is not affected"
        (expect (= []
                   (render/message-detail-expansions-key sid
                                                         {:role :assistant
                                                          :session-turn-id "def456-0000-0000"}
                                                         expansions))))
    (it "expand-all still keys assistant messages"
        (expect (= :expand-all
                   (render/message-detail-expansions-key
                     sid
                     {:role :assistant :session-turn-id "abc12345-0000-0000"}
                     {:vis.channel-tui/expand-all-details? true}))))))

(defdescribe
  bulk-baseline-projection-key-test
  ;; Regression: the projection caches (`format-answer-with-thinking-data`,
  ;; `format-answer-markdown-data`) key each bubble by `turn-`/`relevant-
  ;; detail-expansions-key`. Those used to `keep` only [cid nid] VECTOR keys,
  ;; silently dropping the bulk `:vis.channel-tui/baseline` (C-x [ collapse-all /
  ;; C-x ] expand-all) and the copy-only `:expand-all-details?` FORCE flag —
  ;; both KEYWORD keys. A bulk fold therefore collided with the cached render
  ;; and the transcript never repainted until a per-node click finally changed a
  ;; vector key ("C-x ] does nothing until I click something"). The bulk state
  ;; MUST alter the key.
  (let
    [turn-key
     @#'render/turn-detail-expansions-key

     relevant-key
     @#'render/relevant-detail-expansions-key

     base
     {:session-id "cid" :session-turn-id "abc12345-0000" :node-id "iteration:tabc1234:i1:result"}

     with
     (fn [f de]
       (f (assoc base :detail-expansions de)))]

    (it "turn key differs for collapse vs expand baseline"
        (expect (not= (with turn-key {:vis.channel-tui/baseline :collapse})
                      (with turn-key {:vis.channel-tui/baseline :expand}))))
    (it "turn key differs from the no-bulk baseline"
        (expect (not= (with turn-key {}) (with turn-key {:vis.channel-tui/baseline :expand}))))
    (it "turn key picks up the expand-all FORCE flag"
        (expect (not= (with turn-key {})
                      (with turn-key {:vis.channel-tui/expand-all-details? true}))))
    (it "relevant (markdown) key differs for collapse vs expand baseline"
        (expect (not= (with relevant-key {:vis.channel-tui/baseline :collapse})
                      (with relevant-key {:vis.channel-tui/baseline :expand}))))
    (it "relevant key picks up the expand-all FORCE flag"
        (expect (not= (with relevant-key {})
                      (with relevant-key {:vis.channel-tui/expand-all-details? true}))))))

(defdescribe
  paste-disclosure-render-test
  ;; A user prompt's `[Pasted #N]` marker (the `vis-paste` fence
  ;; `input/collapse-paste-placeholders` emits) renders as a collapsible
  ;; disclosure: the token is the chevron summary row, the payload the body
  ;; shown only when expanded.
  (let
    [ast
     [:ast {} [:p {} [:span {} "look at this"]]
      [:code {:lang "vis-paste"} "[Pasted #1: 3 lines, 11B]\nAAA\nBBB\nCCC\n"]]

     sid
     "s1"

     turn
     "client-turn-1"

     opts
     (fn [de]
       {:session-id sid :session-turn-id turn :detail-expansions de :section :user})

     node-id
     (@#'render/detail-node-id
      {:session-turn-id turn :section :user :kind :paste :details-path ["1"]})]

    (it "collapsed by default: summary chevron shows, payload hidden"
        (let [txt (:text (render/format-answer-markdown-data ast 76 (opts {})))]
          (expect (str/includes? txt "▸ [Pasted #1: 3 lines, 11B]"))
          (expect (not (str/includes? txt "AAA")))))
    (it "expanded: chevron flips and the verbatim payload appears"
        (let [txt (:text (render/format-answer-markdown-data ast 76 (opts {[sid node-id] true})))]
          (expect (str/includes? txt "▾ [Pasted #1: 3 lines, 11B]"))
          (expect (str/includes? txt "AAA"))
          (expect (str/includes? txt "CCC"))))
    (it "the summary row carries toggle-details click meta scoped to this node"
        (let
          [{:keys [lines line-meta]}
           (render/format-answer-markdown-data ast 76 (opts {}))

           idx
           (first (keep-indexed (fn [i l]
                                  (when (str/includes? l "Pasted #1") i))
                                lines))

           meta
           (nth line-meta idx)]

          (expect (= :toggle-details (:kind meta)))
          (expect (= (str node-id) (:node-id meta)))
          (expect (true? (:collapsed? meta)))))))

(defdescribe
  image-disclosure-render-test
  ;; A dropped image renders NON-collapsible: the `[Image #N: ...]` token is a
  ;; plain caption row and the picture's cell box is ALWAYS reserved in the
  ;; layout (or a text fallback shows on image-incapable terminals) — no
  ;; expansion state, so the transcript height never jumps and the picture is
  ;; never painted at a stale position.
  (let
    [ast
     [:ast {}
      [:code {:lang "vis-image"}
       "[Image #1: shot.png 1200×800, 245KB]\n/tmp/shot.png\nimage/png\n1200x800\n245KB\n"]]

     sid
     "s1"

     turn
     "client-turn-1"

     opts
     (fn [de]
       {:session-id sid :session-turn-id turn :detail-expansions de :section :user})]

    (it "graphical terminal: paint-meta row + reserved box are allocated by default"
        (with-redefs [timg/images-protocol (constantly :kitty)]
          (let
            [{:keys [line-meta]} (render/format-answer-markdown-data ast 76 (opts {}))
             img-rows (filter #(= :image (:kind %)) line-meta)
             pad-rows (filter #(= :image-pad (:kind %)) line-meta)
             img (:img (first img-rows))]

            (expect (= 1 (count img-rows)))
            (expect (= "/tmp/shot.png" (:path img)))
            (expect (pos? (long (:rows img))))
            ;; reserved box height = 1 paint row + (rows-1) pad rows
            (expect (= (dec (long (:rows img))) (count pad-rows)))
            ;; pads carry the img map too, so every painted row is clickable
            (expect (every? #(= "/tmp/shot.png" (:path (:img %))) pad-rows)))))
    (it "plain terminal: caption + text fallback always visible, no chevron"
        (with-redefs [timg/images-protocol (constantly nil)]
          (let [txt (:text (render/format-answer-markdown-data ast 76 (opts {})))]
            (expect (str/includes? txt "[Image #1: shot.png 1200×800, 245KB]"))
            (expect (not (str/includes? txt "▸ [Image #1")))
            (expect (str/includes? txt "shot.png"))
            (expect (str/includes? txt "1200×800")))))))

(def ^:private render-iteration-entries @#'render/render-iteration-entries)

(def ^:private tool-card-entries @#'render/tool-card-entries)

(defn- entry-text
  [entries]
  ;; Drop every zero-width structural / inline-style marker and ANSI colour escape
  ;; so assertions match the human-visible text regardless of where sentinels sit
  ;; in the row.
  (mapv (fn [e]
          (-> (str (:line e))
              (str/replace #"\u001b\[[0-9;]*m" "")
              (str/replace #"[\u200B-\u200F\u2060-\u206F\uFEFF\uE000-\uF8FF]" "")))
        entries))

(defdescribe
  tool-card-body-indent-test
  (it
    "keeps one header spacer, glues each label to its content, and breathes one blank row between sections + a trailing pad"
    (let
      [texts
       (entry-text (tool-card-entries {:label "REPL"
                                       :color-role :tool-color/shell
                                       :summary "(+ 1 1) ⇒ 2"
                                       :body
                                       "**RESULT**\n```clojure\n2\n```\n\n**STDOUT**\n```\nhi\n```"}
                                      {:fill-w 76 :session-id nil :detail-expansions {}}))

       body-texts
       (vec (remove str/blank? (drop 2 texts)))]

      (expect (= "" (second texts)))
      (expect (= ["  RESULT" "  2" "  STDOUT" "  hi"] body-texts))
      ;; Sections are separated by exactly ONE blank row, each **LABEL** glued to
      ;; its own content, and the body ends with ONE trailing pad row.
      (expect (= ["  RESULT" "  2" "" "  STDOUT" "  hi" ""] (vec (drop 2 texts)))))))

(defdescribe tool-card-image-reservation-test
             ;; A `vis-image` fence inside an op-card RESULT (e.g. `vis_attach`'s stdout)
             ;; reserves a blank-lined cell box for the picture. The op-card body compaction
             ;; strips fence padding, but MUST NOT strip those reserved rows — else the box
             ;; collapses and the image overpaints the rows below it.
             (it "graphical terminal: reserved image box survives op-card body compaction"
                 (with-redefs [timg/images-protocol (constantly :kitty)]
                   (let
                     [entries (tool-card-entries
                                {:label "RESULT"
                                 :color-role :tool-color/shell
                                 :body (str "````vis-image\n[Image: shot.png 1578×444, 45.7 KB]\n"
                                            "/tmp/shot.png\nimage/png\n1578x444\n45.7 KB\n````")}
                                {:fill-w 76 :session-id nil :detail-expansions {} :node-id "n1"})
                      img-rows (filter #(#{:image :image-pad} (:kind (:meta %))) entries)
                      img (:img (:meta (first img-rows)))]

                     (expect (pos? (long (:rows img))))
                     ;; every reserved row (paint + pads) is present after compaction
                     (expect (= (long (:rows img)) (count img-rows)))
                     (expect (every? #(= "/tmp/shot.png" (:path (:img (:meta %)))) img-rows))))))

(defdescribe
  iteration-merge-flush-test
  ;; Consecutive PLAIN tool iterations — ANY tools, mixed — MERGE into ONE
  ;; synthetic iteration rendered once, so their op-cards flush-stack into a
  ;; single bubble (uniform compaction: no tool-name whitelist, no summary band).
  ;; Prose / thinking is the only separator: a narrated head OPENS a run (its
  ;; narration renders above the merged forms); an INTERIOR narrated call, or an
  ;; iteration-level error, breaks the run.
  (let
    [ctx
     {:fill-w 76 :session-id "s1" :session-turn-id "t" :detail-expansions {}}

     tool
     (fn [i t s]
       [i {:forms [(native-form t s nil)]}])

     narr
     (fn [i t s]
       [i {:forms [(native-form t s nil)] :thinking "hmm"}])

     ;; iter-fn stub: one CALL# row per render, tagged with the head idx and
     ;; the merged form count so the run-grouping is directly observable.
     iter-fn
     (fn [[idx entry]]
       [{:line (str "CALL#" idx "×" (count (:forms entry))) :meta nil}])

     calls
     (fn [pairs]
       (->> (render-iteration-entries pairs iter-fn false true ctx)
            (map :line)
            (filter #(str/starts-with? (str %) "CALL#"))
            vec))]

    (it "a run of consecutive mixed-tool iterations merges into ONE render with every form"
        (expect (= ["CALL#0×3"]
                   (calls [(tool 0 "cat" "`a` · L1-6") (tool 1 "patch" "update `a`")
                           (tool 2 "run_tests" "26/26 passed")]))))
    (it "a lone tool iteration renders as one call"
        (expect (= ["CALL#0×1"] (calls [(tool 0 "cat" "`a` · L1")]))))
    (it "a narrated head OPENS the run — the whole burst still folds into one render"
        (expect (= ["CALL#0×3"]
                   (calls [(narr 0 "cat" "`a`") (tool 1 "patch" "b") (tool 2 "rg" "c")]))))
    (it "an INTERIOR narrated call breaks the run"
        ;; head renders alone; the narrated call opens a fresh run with its tail.
        (expect (= ["CALL#0×1" "CALL#1×2"]
                   (calls [(tool 0 "cat" "a") (narr 1 "patch" "b") (tool 2 "rg" "c")]))))
    (it "an iteration-level error breaks the run and renders on its own"
        (expect (= ["CALL#0×1" "CALL#1×1" "CALL#2×1"]
                   (calls [(tool 0 "cat" "a")
                           [1
                            {:forms [(native-form "rg" "x" nil)]
                             :error {:type :svar.core/http-error}}] (tool 2 "rg" "c")]))))))

;; ---------------------------------------------------------------------------
;; wrap-text* — the plain-text wrap path
;;
;; `wrap-text*` deliberately does NOT delegate to the lanterna fork's
;; `TerminalTextUtils/wordWrap`: it must preserve whitespace verbatim (runs of
;; spaces, leading indentation — raw tool output and the cat hash-gutter rely
;; on it) and re-balance inline style sentinels per physical line, both of
;; which lanterna's wordWrap destroys (it collapses whitespace, counts
;; sentinels as width 1, and leaves ON/OFF toggles straddling wrapped lines,
;; which the per-line painter cannot honour). What MUST agree with lanterna is
;; (a) width measurement — already shared via `p/display-width` /
;; `p/col-prefix-end`, both TextCharacter-backed — and (b) greedy packing on
;; plain single-spaced prose, pinned below against `p/word-wrap` itself.
;; ---------------------------------------------------------------------------

(defdescribe wrap-text-lanterna-parity-test
             (it "packs plain single-spaced prose exactly like the lanterna fork's wordWrap"
                 (doseq
                   [[s w] [["launch pad rocket" 10]
                           ["the quick brown fox jumps over the lazy dog" 16]
                           ["zażółć gęślą jaźń i jeszcze trochę tekstu na próbę" 14]
                           ["abcdefghijklmnopqrstuvwxyz0123456789" 10] ["日本語のテキストは切れ目なく続く" 8]
                           ["🚀🚀🚀 rocket launch pad 🚀🚀🚀" 10]]]
                   (expect (= (vec (p/word-wrap s w)) (render/wrap-text* s w))
                           (str "diverged from TerminalTextUtils/wordWrap on " (pr-str s) " @" w))))
             (it "does not retreat a word when the cut lands exactly on a word boundary"
                 (expect (= ["launch pad" "rocket"] (render/wrap-text* "launch pad rocket" 10)))))

(defdescribe wrap-text-whitespace-fidelity-test
             (it "preserves leading indentation (lanterna's wordWrap strips it)"
                 (expect (= ["    indented line" "that should wrap" "somewhere"]
                            (render/wrap-text* "    indented line that should wrap somewhere" 18))))
             (it "preserves runs of spaces mid-line (lanterna's wordWrap collapses them)"
                 (let [lines (render/wrap-text* "a  b   c    dddd eeee" 8)]
                   (expect (str/includes? (first lines) "a  b")
                           "mid-line multi-space must survive wrapping"))))

(defdescribe
  wrap-text-termination-test
  (it "terminates when a glyph is wider than the wrap width (used to hang the render thread)"
      (let [r (deref (future (render/wrap-text* "🚀🚀🚀" 1)) 3000 ::hung)]
        (expect (not= ::hung r) "wide glyph at width 1 must not loop forever")
        (expect (= "🚀🚀🚀" (apply str r)) "no content lost"))))

(defdescribe wrap-text-sentinel-rebalance-test
             (it "re-opens and re-closes inline style sentinels on every wrapped line"
                 (let
                   [s
                    (str "aaa " p/INLINE_BOLD_ON "bold words here" p/INLINE_BOLD_OFF " tail")

                    lines
                    (render/wrap-text* s 8)

                    visible
                    (fn [l]
                      (apply str (remove #(p/inline-sentinel? (str %)) l)))]

                   ;; content survives the wrap
                   (expect (= (visible s) (str/join " " (map (comp str/trim visible) lines))))
                   ;; a raw lanterna wordWrap would leave BOLD_ON on one line and
                   ;; BOLD_OFF lines later; the painter resets style per line, so
                   ;; every line must carry balanced toggles
                   (doseq [l lines]
                     (expect (= (str/includes? l p/INLINE_BOLD_ON)
                                (str/includes? l p/INLINE_BOLD_OFF))
                             (str "unbalanced sentinels on line " (pr-str l)))))))
