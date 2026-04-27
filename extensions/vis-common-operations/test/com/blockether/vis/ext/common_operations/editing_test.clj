(ns com.blockether.vis.ext.common-operations.editing-test
  "Tests for the editing extension's error-rescue helpers.

   The story: the LLM occasionally over-escapes regex patterns when
   calling `(vis/grep-files ...)` — e.g. `\\|` to mean a literal pipe,
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
   [clojure.string :as str]
   [com.blockether.vis.extension :as ext]
   [com.blockether.vis.ext.common-operations.editing :as editing]
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
        {:type    :ext.common-operations.editing/invalid-regex
         :pattern pattern
         :error   (ex-message e)}))))

(defn- path-error
  [msg path]
  (ex-info (str msg ": " path)
    {:type :ext.common-operations.editing/path-traversal :path path}))

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
                {:type    :ext.common-operations.editing/invalid-regex
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
                {:type    :ext.common-operations.editing/invalid-regex
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
          abs (str cwd "/extensions/vis-common-operations/deps.edn")
          err (path-error "not a relative path" abs)
          ret (rescue-path-args err {} :read [abs])]
      (expect (= ["extensions/vis-common-operations/deps.edn"] (:args ret)))
      (expect (= :read (:fn ret)))))

  (it "strips a leading slash when the path is outside CWD"
    (let [err (path-error "Path escapes working directory" "/etc/passwd")
          ret (rescue-path-args err {} :read ["/etc/passwd"])]
      (expect (= ["etc/passwd"] (:args ret)))))

  (it "surfaces unrelated errors untouched"
    (let [err (ex-info "File not found: foo" {:type :ext.common-operations.editing/not-found})
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
                 {:type    :ext.common-operations.editing/invalid-regex
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
    {:ext/namespace 'com.blockether.vis.ext.common-operations.test-fake
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
    (let [code  "(vis/grep-files \"foo\\|bar\")"
          err   (edamame-error-msg code)
          _     (expect (some? err))
          fixed (rescue-parse-error {:code code :error err :environment {}})]
      (expect (= "(vis/grep-files \"foo\\\\|bar\")" fixed))
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
;; Symbol-level wiring — grep-files-symbol carries the parse rescue
;; =============================================================================

(defdescribe grep-files-symbol-parse-error-wiring-test

  (it "grep-files-symbol carries :ext.symbol/on-parse-error-fn"
    (let [hook (:ext.symbol/on-parse-error-fn editing/grep-files-symbol)]
      (expect (fn? hook))
      ;; Smoke-test the symbol-level hook directly: the broken `\|` case
      ;; gets repaired so the LLM's intent (literal pipe) is preserved.
      (let [code "(vis/grep-files \"a\\|b\")"
            err  (edamame-error-msg code)
            out  (hook {:code code :error err
                        :sym 'grep-files :environment {}})]
        (expect (= "(vis/grep-files \"a\\\\|b\")" out)))))

  (it "the registered extension routes parse rescue through the symbol hook"
    (require '[com.blockether.vis.ext.common-operations.core :as core])
    (let [registered @(resolve 'core/extension)
          code       "(vis/grep-files \"a\\|b\")"
          err        (edamame-error-msg code)]
      ;; No extension-level catch-all anymore.
      (expect (nil? (:ext/on-parse-error-fn registered)))
      ;; But try-rescue-parse-error finds the symbol hook by name.
      (expect (= "(vis/grep-files \"a\\\\|b\")"
                (ext/try-rescue-parse-error [registered] code err {}))))))
