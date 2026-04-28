(ns com.blockether.vis.ext.common-editing.editing-test
  "Tests for the editing extension's error-rescue helpers.

   The story: the LLM occasionally over-escapes regex patterns when
   calling `(vis/rg ...)` — e.g. `\\|` to mean a literal pipe,
   `\\.` to mean a literal dot — and RE2/J rejects only the malformed
   ones with `\"invalid escape sequence: `\\X`\"`.

   The extension contract gives every symbol an `:on-error-fn` hook
   that runs after `:fn` throws and can return `{:fn :args}` to
   retry. `rescue-grep-args` is that hook for `grep-files`.

   These tests cover:

     1. The hook directly — round-trips an error → retry plan.
     2. The hook end-to-end through `extension/invoke-symbol-wrapper`,
        proving the rescue actually runs in the same code path the
        SCI sandbox uses.

   Note on parse-time errors: when the LLM emits a Clojure source
   string with an unsupported escape (e.g. raw `\\|` instead of
   `\\\\|`), edamame fails BEFORE the tool fn is dispatched, so
   :on-error-fn never sees that case. The iteration loop surfaces the
   parse error to the LLM, which self-corrects on the next turn."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis-sdk.core :as ext]
   [com.blockether.vis.ext.common-editing.editing :as editing]
   [lazytest.core :refer [defdescribe it expect]])
  (:import [com.google.re2j Pattern]))

;; =============================================================================
;; Reach private vars
;; =============================================================================

(def rescue-grep-args        #'editing/rescue-grep-args)
(def rescue-path-args        #'editing/rescue-path-args)
(def strip-bad-escape        #'editing/strip-bad-escape)
(def extract-bad-escape-char #'editing/extract-bad-escape-char)
(def rescue-parse-error      #'editing/rescue-parse-error)
(def line-col->index         #'editing/line-col->index)
(def patch-tool              #'editing/patch)
(def apply-one-replacement   #'editing/apply-one-replacement)

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- re2j-compile-error
  "Build the same exception shape `compile-safe-pattern` throws when
   RE2/J rejects a pattern. Mirrors the wrapping in editing.clj so the
   tests don't need to re-import RE2/J just to provoke real errors."
  [pattern]
  (try
    (Pattern/compile (str pattern))
    nil
    (catch Exception e
      (ex-info (str "Invalid regex pattern: " pattern)
        {:type    :ext.common-editing.editing/invalid-regex
         :pattern pattern
         :error   (ex-message e)}))))

(defn- path-error
  [msg path]
  (ex-info (str msg ": " path)
    {:type :ext.common-editing.editing/path-traversal :path path}))

(defn- delete-tree!
  [^java.io.File root]
  (when (.exists root)
    (doseq [file (reverse (file-seq root))]
      (.delete file))))

(defn- with-temp-workspace
  [f]
  (let [workspace-name (str ".tmp-editing-test-" (java.util.UUID/randomUUID))
        workspace-dir  (io/file workspace-name)]
    (.mkdirs workspace-dir)
    (try
      (f workspace-name)
      (finally
        (delete-tree! workspace-dir)))))

;; =============================================================================
;; extract-bad-escape-char
;; =============================================================================

(defdescribe extract-bad-escape-char-test

  (it "pulls the offending char out of an RE2/J error message"
    (expect (= \q (extract-bad-escape-char
                    "error parsing regexp: invalid escape sequence: `\\q`")))
    (expect (= \1 (extract-bad-escape-char
                    "error parsing regexp: invalid escape sequence: `\\1`"))))

  (it "returns nil when the message is not an escape error"
    (expect (nil? (extract-bad-escape-char "missing closing ]: `[abc`")))
    (expect (nil? (extract-bad-escape-char nil)))
    (expect (nil? (extract-bad-escape-char "")))))

;; =============================================================================
;; strip-bad-escape
;; =============================================================================

(defdescribe strip-bad-escape-test

  (it "removes the lone backslash in front of a punctuation meta char"
    ;; strip-bad-escape touches ONLY the char RE2/J flagged — if a
    ;; pattern has multiple bad escapes RE2/J reports them one at a
    ;; time and the rescue gets called once per retry.
    (expect (= "foo|bar"   (strip-bad-escape "foo\\|bar"  \|)))
    (expect (= "(group\\)" (strip-bad-escape "\\(group\\)" \()))
    (expect (= "\\(group)" (strip-bad-escape "\\(group\\)" \))))
    (expect (= "a.b"       (strip-bad-escape "a\\.b"      \.)))
    (expect (= "end$"      (strip-bad-escape "end\\$"     \$))))

  (it "refuses to touch alphanumeric escapes — those are real RE2/J classes"
    (expect (nil? (strip-bad-escape "\\dfoo"  \d)))
    (expect (nil? (strip-bad-escape "x\\wy"   \w)))
    (expect (nil? (strip-bad-escape "back\\1" \1))))

  (it "is a no-op when the bad char is not in the input"
    (expect (= "plain" (strip-bad-escape "plain" \|)))))

;; =============================================================================
;; rescue-grep-args — the actual on-error-fn
;; =============================================================================

(defdescribe rescue-grep-args-test

  (it "retries with the backslash stripped when RE2/J would reject `\\|`"
    ;; RE2/J actually accepts `\\|` (literal pipe), so to exercise the
    ;; rescue path we pretend `compile-safe-pattern` raised the canonical
    ;; "invalid escape sequence: `\|`" message — exactly the shape
    ;; we'd see if RE2/J had rejected it.
    (let [err (ex-info "Invalid regex pattern: \\|"
                {:type    :ext.common-editing.editing/invalid-regex
                 :pattern "\\|"
                 :error   "error parsing regexp: invalid escape sequence: `\\|`"})
          ret (rescue-grep-args err {} :grep ["\\|" "src"])]
      (expect (= :grep (:fn ret)))
      (expect (= ["|" "src"] (:args ret)))
      (expect (not (contains? ret :error)))))

  (it "retries on a real RE2/J runtime error (`\\(` over-escape)"
    ;; `\\(` IS a real escape error in RE2/J because `(` is a group
    ;; opener. Wait — RE2/J accepts `\\(` as literal `(`. Use `\\1`
    ;; instead, which RE2/J rejects, to drive a real wrapped error.
    ;; Then we strip the backslash and check we get a retry. (Note:
    ;; `\\1` is alphanumeric so the rescue should refuse to fix it —
    ;; covered in the next test. For this case, hand-craft an error
    ;; message naming a punctuation char.)
    (let [err (ex-info "Invalid regex pattern: foo\\(bar"
                {:type    :ext.common-editing.editing/invalid-regex
                 :pattern "foo\\(bar"
                 :error   "error parsing regexp: invalid escape sequence: `\\(`"})
          ret (rescue-grep-args err {} :grep ["foo\\(bar"])]
      (expect (= ["foo(bar"] (:args ret)))
      (expect (= :grep (:fn ret)))))

  (it "surfaces the original error when the bad escape is alphanumeric"
    ;; `\\q` is a real RE2/J error and we should NOT pretend to fix
    ;; it — stripping the `\\` gives `q`, which silently changes the
    ;; pattern's meaning. Hand it back to the LLM instead.
    (let [err (re2j-compile-error "\\q")
          _   (expect (some? err))
          ret (rescue-grep-args err {} :grep ["\\q" "src"])]
      (expect (= {:error err} ret))))

  (it "surfaces the original error when the regex is broken in some other way"
    (let [err (re2j-compile-error "[abc")
          _   (expect (some? err))
          ret (rescue-grep-args err {} :grep ["[abc"])]
      (expect (= {:error err} ret))))

  (it "rewrites an absolute path arg to a path relative to CWD"
    (let [cwd  (System/getProperty "user.dir")
          abs  (str cwd "/src")
          err  (path-error "not a relative path" abs)
          ret  (rescue-grep-args err {} :grep ["TODO" abs])]
      (expect (= "TODO" (first (:args ret))))
      (expect (= "src"  (second (:args ret))))
      (expect (= :grep  (:fn ret)))))

  (it "does NOT touch the path when there's only one arg (pattern only)"
    (let [err (path-error "not a relative path" "/whatever")
          ret (rescue-grep-args err {} :grep ["TODO"])]
      ;; Only one arg — nothing to fix; rescue returns unchanged args.
      (expect (= ["TODO"] (:args ret))))))

;; =============================================================================
;; rescue-path-args — shared by read/list/patch
;; =============================================================================

(defdescribe rescue-path-args-test

  (it "rewrites an absolute path under CWD to a relative path"
    (let [cwd (System/getProperty "user.dir")
          abs (str cwd "/extensions/common/vis-common-editing/deps.edn")
          err (path-error "not a relative path" abs)
          ret (rescue-path-args err {} :read [abs])]
      (expect (= ["extensions/common/vis-common-editing/deps.edn"] (:args ret)))
      (expect (= :read (:fn ret)))))

  (it "strips a leading slash when the path is outside CWD"
    (let [err (path-error "Path escapes working directory" "/etc/passwd")
          ret (rescue-path-args err {} :read ["/etc/passwd"])]
      (expect (= ["etc/passwd"] (:args ret)))))

  (it "surfaces unrelated errors untouched"
    (let [err (ex-info "File not found: foo" {:type :ext.common-editing.editing/not-found})
          ret (rescue-path-args err {} :read ["foo"])]
      (expect (= {:error err} ret)))))

;; =============================================================================
;; End-to-end through invoke-symbol-wrapper
;;
;; Proves the rescue actually fires inside the same wrapper SCI uses.
;; =============================================================================

(defn- fake-grep-symbol
  "Tiny grep-files stand-in: on every call, if the pattern still
   contains a backslash, throw the same ex-info shape RE2/J errors
   would surface as. Once the rescue strips the backslash, the call
   succeeds and returns :ok. Lets us verify the rescue retries via
   :fn / :args without touching the real filesystem."
  [calls bad-char]
  (ext/symbol 'grep-files
    (fn [pattern]
      (swap! calls conj pattern)
      (if (str/includes? pattern "\\")
        (throw (ex-info (str "Invalid regex pattern: " pattern)
                 {:type    :ext.common-editing.editing/invalid-regex
                  :pattern pattern
                  :error   (str "error parsing regexp: invalid escape sequence: `\\"
                             bad-char "`")}))
        :ok))
    {:doc      "fake grep"
     :arglists '([pattern])
     :on-error-fn @rescue-grep-args}))

(defn- wrap-extension
  "Build the smallest possible valid extension around one symbol so we
   can call invoke-symbol-wrapper without dragging in the registry."
  [sym]
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.common-editing.test-fake
     :ext/doc       "Fake editing extension for tests."
     :ext/group     "filesystem"
     :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
     ;; The spec demands :ext/prompt be a fn (env→string); the
     ;; non-fn shorthand is normalized by `ext/extension` itself, but
     ;; passing a fn directly avoids relying on that normalization in
     ;; tests.
     :ext/prompt    (constantly "placeholder")
     :ext/symbols   [sym]}))

(defdescribe rescue-grep-args-end-to-end-test

  (it "on-error-fn fires through invoke-symbol-wrapper and the retry succeeds"
    (let [calls  (atom [])
          sym    (fake-grep-symbol calls "(")
          ext    (wrap-extension sym)
          result (ext/invoke-symbol-wrapper ext sym ["foo\\(bar"] {})]
      (expect (= :ok result))
      ;; First call: original (with backslash). Second: rescued.
      (expect (= ["foo\\(bar" "foo(bar"] @calls))))

  (it "on-error-fn re-throws when the rescue gives up (alphanumeric escape)"
    (let [calls  (atom [])
          sym    (fake-grep-symbol calls "q")
          ext    (wrap-extension sym)
          thrown (try (ext/invoke-symbol-wrapper ext sym ["foo\\q"] {})
                   (catch Throwable t t))]
      (expect (some? thrown))
      ;; Only one underlying call — rescue refused to retry on `\q`.
      (expect (= 1 (count @calls)))
      (expect (= "foo\\q" (first @calls))))))

;; =============================================================================
;; rescue-parse-error — :ext/on-parse-error-fn
;;
;; This is the OTHER half of the story — the LLM emits raw `\X` inside
;; a string literal in the source it sends. SCI/edamame rejects with
;; "[line L, col C] Unsupported escape character: \X" BEFORE any tool
;; fn dispatch, so symbol-level :on-error-fn is useless. The
;; extension-level :ext/on-parse-error-fn hook can still rewrite the
;; source and let the iteration loop retry the parse.
;; =============================================================================

(defn- edamame-parse [^String code]
  ;; Real edamame round-trip so the error message format we depend on
  ;; doesn't drift silently. Throws on parse error — callers wrap.
  (require '[edamame.core :as eda])
  ((resolve 'eda/parse-string-all) code {:all true}))

(defn- edamame-error-msg [^String code]
  (try (edamame-parse code) nil
    (catch Throwable t (ex-message t))))

(defdescribe line-col->index-test

  (it "resolves [line, col] into a 0-based char index, accounting for newlines"
    (let [code "abc\ndef\nghi"]
      ;; (line 1, col 1) → 'a'
      (expect (= 0 (line-col->index code 1 1)))
      ;; (line 2, col 1) → 'd'
      (expect (= 4 (line-col->index code 2 1)))
      ;; (line 3, col 2) → 'h'
      (expect (= 9 (line-col->index code 3 2)))))

  (it "returns nil when the position runs past the source"
    (expect (nil? (line-col->index "x" 1 99)))
    (expect (nil? (line-col->index "" 1 2)))))

(defdescribe rescue-parse-error-test

  (it "doubles the lone backslash in front of a regex meta char"
    (let [code  "(vis/rg \"foo\\|bar\")"
          err   (edamame-error-msg code)
          _     (expect (some? err))
          fixed (rescue-parse-error {:code code :error err :environment {}})]
      (expect (= "(vis/rg \"foo\\\\|bar\")" fixed))
      ;; And critically: the rewrite parses cleanly.
      (expect (nil? (edamame-error-msg fixed)))))

  (it "repairs `\\.` and `\\$` and `\\(` the same way"
    (doseq [bad ["(re-find \"a\\.b\")"
                 "(re-find \"end\\$\")"
                 "(re-find \"\\(group\")"]]
      (let [err   (edamame-error-msg bad)
            fixed (rescue-parse-error {:code bad :error err :environment {}})]
        (expect (some? fixed))
        (expect (nil? (edamame-error-msg fixed))))))

  (it "returns nil for a real letter typo — doesn't pretend to fix a single backslash followed by a letter"
    ;; Doubling a backslash before a letter would parse fine but might
    ;; silently convert intent (a typo for the digit-class escape).
    ;; Rule: only rescue punctuation.
    ;;
    ;; Construct the input programmatically: a string literal
    ;; containing the bytes  ( r e - f i n d  " f o o \ q b a r " )
    ;; The single backslash before `q` is what makes it an
    ;; unsupported-escape input — we cannot embed that as a Clojure
    ;; string literal directly because the Clojure reader rejects
    ;; `\q` itself, so we synthesize via `str` + the backslash char.
    (let [code (str "(re-find \"foo" \\ "qbar\")")
          err  (edamame-error-msg code)]
      (expect (some? err))
      (expect (nil? (rescue-parse-error {:code code :error err :environment {}})))))

  (it "returns nil when the error isn't an Unsupported-escape one"
    (expect (nil? (rescue-parse-error
                    {:code "(unbalanced "
                     :error "EOF while reading"
                     :environment {}}))))

  (it "returns nil on non-string inputs (defensive against malformed ctx)"
    (expect (nil? (rescue-parse-error {:code nil :error "x" :environment {}})))
    (expect (nil? (rescue-parse-error {:code "x" :error nil :environment {}}))))

  (it "only repairs the FIRST offending site — caller re-runs the rescue if more remain"
    ;; Two stray escapes on different lines. The hook fixes ONE; the
    ;; iteration loop calls the hook again on the new error. We model
    ;; that here by parsing the rewrite and rescuing again.
    (let [code  "(do \"a\\|b\"\n    \"c\\|d\")"
          err1  (edamame-error-msg code)
          fix1  (rescue-parse-error {:code code :error err1 :environment {}})
          err2  (edamame-error-msg fix1)
          fix2  (rescue-parse-error {:code fix1 :error err2 :environment {}})]
      (expect (some? fix1))
      (expect (some? fix2))
      (expect (nil? (edamame-error-msg fix2))))))

;; =============================================================================
;; patch — canonical shape: vector of {:path :search :replace} maps.
;;
;; The legacy modes (path+search+replace, path+SEARCH/REPLACE marker text,
;; Codex `*** Begin Patch` payloads) were removed by the BUG_REPORT_2
;; canonicalization pass. Tests below assert the canonical shape works
;; AND that every legacy shape now throws a teaching ex-info pointing the
;; caller at the canonical example.
;; =============================================================================

(defdescribe patch-tool-canonical-test

  (it "applies a single-edit vector"
    (with-temp-workspace
      (fn [workspace-name]
        (let [path (str workspace-name "/single.txt")]
          (spit path "hello old world")
          (let [result (patch-tool [{:path path :search "old" :replace "new"}])]
            (expect (= :ok (:status result)))
            (expect (= "hello new world" (slurp path))))))))

  (it "applies multiple edits across multiple files"
    (with-temp-workspace
      (fn [workspace-name]
        (let [first-path  (str workspace-name "/one.txt")
              second-path (str workspace-name "/two.txt")]
          (spit first-path "alpha")
          (spit second-path "beta")
          (let [result (patch-tool [{:path first-path :search "alpha" :replace "ALPHA"}
                                    {:path second-path :search "beta" :replace "BETA"}])]
            (expect (= 2 (:files-touched result)))
            (expect (= "ALPHA" (slurp first-path)))
            (expect (= "BETA" (slurp second-path))))))))

  (it "applies multiple edits to the same file in vector order"
    (with-temp-workspace
      (fn [workspace-name]
        (let [path (str workspace-name "/multi.txt")]
          (spit path "alpha beta gamma")
          (let [result (patch-tool [{:path path :search "alpha" :replace "A"}
                                    {:path path :search "gamma" :replace "G"}])]
            (expect (= 1 (:files-touched result)))
            (expect (= "A beta G" (slurp path))))))))

  (it "creates a new file when :search is empty"
    (with-temp-workspace
      (fn [workspace-name]
        (let [path   (str workspace-name "/created.txt")
              result (patch-tool [{:path path :search "" :replace "hello world\n"}])]
          (expect (= :ok (:status result)))
          (expect (= "hello world\n" (slurp path)))
          (expect (true? (:created? (first (:files result))))))))))

(defdescribe patch-tool-rejects-legacy-shapes-test

  (it "hard-fails the path+search+replace triple shape"
    (let [thrown (try (patch-tool "path.txt" "old" "new")
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-arity
                (:type (ex-data thrown))))
      ;; Teaching message points at the canonical example.
      (expect (str/includes? (ex-message thrown) ":path :search :replace"))
      (expect (str/includes? (ex-message thrown) "(vis/patch [{"))))

  (it "hard-fails the path+SEARCH/REPLACE marker-text shape"
    (let [marker-text "<<<<<<< SEARCH\nfoo\n=======\nbar\n>>>>>>> REPLACE"
          thrown      (try (patch-tool "path.txt" marker-text)
                        (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-arity
                (:type (ex-data thrown))))
      (expect (str/includes? (ex-message thrown) ":path :search :replace"))))

  (it "hard-fails Codex `*** Begin Patch` payloads"
    (let [payload (str "*** Begin Patch\n"
                    "*** Update File: x.txt\n"
                    "<<<<<<< SEARCH\nold\n=======\nnew\n>>>>>>> REPLACE\n"
                    "*** End Patch")
          thrown  (try (patch-tool payload)
                    (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-arity
                (:type (ex-data thrown))))
      (expect (str/includes? (ex-message thrown) ":path :search :replace"))))

  (it "hard-fails an empty edit vector"
    (let [thrown (try (patch-tool [])
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-arity
                (:type (ex-data thrown))))
      (expect (str/includes? (ex-message thrown) "empty"))))

  (it "hard-fails entries with unsupported keys"
    (let [thrown (try (patch-tool [{:path "x.txt" :search "a" :replace "b"
                                    :patch-text "legacy"}])
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-edit
                (:type (ex-data thrown))))
      (expect (str/includes? (ex-message thrown) "unsupported keys"))
      (expect (str/includes? (ex-message thrown) ":patch-text"))))

  (it "hard-fails entries with non-string :search"
    (let [thrown (try (patch-tool [{:path "x.txt" :search nil :replace "b"}])
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-edit
                (:type (ex-data thrown))))
      (expect (str/includes? (ex-message thrown) ":search must be a string"))))

  (it "hard-fails entries that are not maps"
    (let [thrown (try (patch-tool ["not-a-map"])
                   (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-invalid-edit
                (:type (ex-data thrown))))
      (expect (str/includes? (ex-message thrown) "is not a map")))))

;; =============================================================================
;; Symbol-level wiring — rg-symbol carries the parse rescue
;; =============================================================================

(defdescribe rg-symbol-parse-error-wiring-test

  (it "rg-symbol carries :ext.symbol/on-parse-error-fn"
    (let [hook (:ext.symbol/on-parse-error-fn editing/rg-symbol)]
      (expect (fn? hook))
      ;; Smoke-test the symbol-level hook directly: the broken `\|` case
      ;; gets repaired so the LLM's intent (literal pipe) is preserved.
      (let [code "(vis/rg \"a\\|b\")"
            err  (edamame-error-msg code)
            out  (hook {:code code :error err
                        :sym 'rg :environment {}})]
        (expect (= "(vis/rg \"a\\\\|b\")" out)))))

  (it "the registered extension routes parse rescue through the symbol hook"
    (require '[com.blockether.vis.ext.common-editing.core :as core])
    (let [registered @(resolve 'core/extension)
          code       "(vis/rg \"a\\|b\")"
          err        (edamame-error-msg code)]
      ;; No extension-level catch-all anymore.
      (expect (nil? (:ext/on-parse-error-fn registered)))
      ;; But try-rescue-parse-error finds the symbol hook by name.
      (expect (= "(vis/rg \"a\\\\|b\")"
                (ext/try-rescue-parse-error [registered] code err {}))))))

;; =============================================================================
;; patch :patch-no-match diagnostics — Bug 2.C.1
;; =============================================================================
;;
;; When `:search` doesn't match the file content, the existing exception
;; only says "SEARCH block N not found in <path>" with no hint about why.
;; The model then re-reads the file and re-guesses, often hitting the
;; same whitespace mismatch again. The diagnostic helper documented in
;; BUG_REPORT_2 §2.C should:
;;
;;   1. detect the whitespace-only-difference case and tag the ex-info
;;      with `:near-match {:line N :hint "whitespace differs"}`,
;;   2. include the closest on-disk line in the human-readable message
;;      so the model can re-emit with the right indentation without
;;      re-reading the whole file.
;;
;; The patch STILL fails (we don't auto-fuzz writes); we just hand the
;; LLM enough information to fix the next attempt in one shot.

(defdescribe patch-no-match-near-match-test

  (it "adds :near-match {:line :hint} when the only difference is leading whitespace"
    (let [content (str "(let [expr-label  (label-text \"code\" 1)\n"
                    "      expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]\n"
                    "                    (str (repeat-str \\space pl) expr-label \" \"))\n"
                    "      fa-pad      (max 0 (- fill-w (count full-label) 1))]\n"
                    "  ...)\n")
          ;; Identical content but with 38 spaces of indent on line 3
          ;; instead of the file's 20 spaces. This is exactly the
          ;; iter-15 shape from BUG_REPORT_2.
          search  (str "expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]\n"
                    "                                      (str (repeat-str \\space pl) expr-label \" \")")
          thrown  (try
                    (apply-one-replacement
                      content
                      {:search search :replace "x" :index 1}
                      "render.clj")
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (let [data (ex-data thrown)]
        (expect (= :ext.common-editing.editing/patch-no-match (:type data)))
        ;; The new fields the model needs:
        (expect (map? (:near-match data)))
        (expect (integer? (:line (:near-match data))))
        ;; The whitespace-collapsed search matches line 2 (1-indexed)
        ;; — the `expr-hdr (let ...` line.
        (expect (= 2 (:line (:near-match data))))
        (expect (= "whitespace differs" (:hint (:near-match data)))))
      ;; And the human-readable message points at the closest line
      ;; instead of just saying "not found".
      (expect (str/includes? (ex-message thrown) "Closest line"))
      (expect (str/includes? (ex-message thrown) "whitespace"))))

  (it "omits :near-match when no whitespace-collapsed candidate exists"
    ;; Genuinely-not-in-file search: no hint, original behavior preserved.
    (let [content "alpha beta gamma\n"
          search  "completely unrelated payload"
          thrown  (try
                    (apply-one-replacement
                      content
                      {:search search :replace "x" :index 1}
                      "x.txt")
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? thrown))
      (expect (= :ext.common-editing.editing/patch-no-match
                (:type (ex-data thrown))))
      (expect (nil? (:near-match (ex-data thrown))))
      ;; Message stays simple when there's no useful hint to give.
      (expect (not (str/includes? (ex-message thrown) "Closest line"))))))
