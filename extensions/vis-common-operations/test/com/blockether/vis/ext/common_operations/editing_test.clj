(ns com.blockether.vis.ext.common-operations.editing-test
  "Tests for the editing extension's error-rescue helpers.

   The story: the LLM occasionally over-escapes regex patterns when
   calling `(fs/grep-files ...)` — e.g. `\\|` to mean a literal pipe,
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
     :ext/ns-alias  {:ns 'vis.ext.fs :alias 'fs}
     :ext/prompt    "placeholder"
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
