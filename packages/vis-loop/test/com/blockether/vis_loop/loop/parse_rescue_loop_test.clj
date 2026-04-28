(ns com.blockether.vis-loop.loop.parse-rescue-loop-test
  "Tests for the iteration loop's parse-error rescue DRIVER.

   Background. Symbol-level parse rescue hooks may repair only the FIRST
   offending site in one call \u2014 multi-site failures (e.g. the model
   emits `\"foo\\|bar\\|baz\"` with three stray escapes) are the driver's
   responsibility to converge on by re-running the rescue chain until
   the source either parses cleanly or stops shrinking.

   Before the fix described in BUG_REPORT_2.md (\u00a72.A), the driver
   ran the rescue ONCE: a single `\\|` was repaired, but the
   resulting string still didn't parse, so the driver dropped the
   rewrite and surfaced the raw edamame error to the LLM. This test
   pins the looped-driver behavior so it cannot regress.

   Driver under test: `try-extension-parse-rescue` in
   `iteration.core`. We hit it via the public `execute-code` entry
   that exercises the same path used by every iteration."
  (:require
   [clojure.string :as str]
   [com.blockether.vis-extension.extension :as ext]
   [com.blockether.vis-loop.loop.runtime.conversation.environment.query.iteration.core :as iter]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(def ^:private try-extension-parse-rescue
  #'iter/try-extension-parse-rescue)

(defn- preceding-backslash-count
  [code index]
  (loop [position (dec index)
         count    0]
    (if (and (<= 0 position) (= \\ (.charAt ^String code position)))
      (recur (dec position) (inc count))
      count)))

(defn- rescue-one-unsupported-escape
  "Test fixture hook: doubles exactly one unsupported regex escape.
   The loop driver must call it repeatedly when a source string has
   multiple bad sites. This intentionally lives in core tests so core
   does not depend on the common-editing extension test classpath."
  [{:keys [code error]}]
  (when (and (string? code) (str/includes? (str error) "Unsupported escape character"))
    (let [targets #{\| \. \( \) \$ \* \+ \? \[ \] \{ \}}
          length  (count code)]
      (loop [index 0]
        (cond
          (>= index (dec length)) nil
          (and (= \\ (.charAt ^String code index))
            (targets (.charAt ^String code (inc index)))
            (even? (preceding-backslash-count code index)))
          (str (subs code 0 index) "\\" (subs code index))
          :else (recur (inc index)))))))

(def ^:private rg-symbol
  (ext/symbol 'rg (fn [& _] nil)
    {:doc               "fixture"
     :arglists          '([pattern])
     :on-parse-error-fn rescue-one-unsupported-escape}))

(defn- minimal-environment
  "Build the smallest possible environment shape that
   `try-extension-parse-rescue` reads from. Only `:extensions`
   (a deref-able holder of an extension vec) is required."
  []
  (let [ext (ext/extension
              {:ext/namespace 'com.blockether.vis.test.parse-rescue
               :ext/doc       "Loop test fixture."
               :ext/group     "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
               :ext/prompt    (constantly "placeholder")
               :ext/symbols   [rg-symbol]})]
    {:extensions (atom [ext])
     :sci-ctx    (sci/init {})}))

(defn- parses? [^String code]
  (try
    (require '[edamame.core :as eda])
    ((resolve 'eda/parse-string-all) code {:all true})
    true
    (catch Throwable _ false)))

(defn- parse-error-msg [^String code]
  (try
    (require '[edamame.core :as eda])
    ((resolve 'eda/parse-string-all) code {:all true})
    nil
    (catch Throwable t (ex-message t))))

(defdescribe try-extension-parse-rescue-loop-test

  (it "repairs a single `\\|` site (baseline; pre-fix already passed)"
    (let [env  (minimal-environment)
          code "(vis/rg \"a\\|b\")"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (= "(vis/rg \"a\\\\|b\")" out))
      (expect (parses? out))))

  (it "loops the rescue across THREE `\\|` sites until the source parses (Bug 2.A.1)"
    ;; Pre-fix: returns nil (single-shot rescue gives up on 2+ sites).
    ;; Post-fix: returns a fully repaired string that parses cleanly.
    (let [env  (minimal-environment)
          code "(vis/rg \"foo\\|bar\\|baz\\|qux\")"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (string? out))
      (expect (parses? out))
      ;; Every original `\|` is now `\\|`.
      (expect (str/includes? out "\\\\|"))))

  (it "loops across `\\|` AND `\\.` AND `\\(` mixed escapes"
    (let [env  (minimal-environment)
          code "(vis/rg \"a\\|b\\.c\\(d\")"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (string? out))
      (expect (parses? out))))

  (it "still returns nil when the rescue has nothing to repair"
    ;; A real broken form the rescue can't fix: single-quoted string
    ;; literal the reader rejects. The hook's `rescue-parse-error`
    ;; only handles Unsupported-escape errors; other shapes return
    ;; nil from every iteration of the loop.
    (let [env  (minimal-environment)
          code "(vis/rg 'unterminated"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (nil? out))))

  (it "bounded: a pathological hook that returns a non-shrinking rewrite must not loop forever"
    ;; If a hook keeps returning the same error shape (or makes no
    ;; progress), the driver MUST give up. We simulate that with an
    ;; extension whose hook trivially returns its input wrapped in
    ;; a no-op transformation that re-raises the same parse error.
    (let [pathological-hook (fn [{:keys [code]}]
                              ;; Return code unchanged \u2014 should be
                              ;; detected as no-progress and stop.
                              code)
          rg (ext/symbol 'rg (fn [& _] nil)
               {:doc      "fixture"
                :arglists '([pattern])
                :on-parse-error-fn pathological-hook})
          ext (ext/extension
                {:ext/namespace 'com.blockether.vis.test.pathological
                 :ext/doc       "pathological"
                 :ext/group     "filesystem"
                 :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
                 :ext/prompt    (constantly "x")
                 :ext/symbols   [rg]})
          env {:extensions (atom [ext]) :sci-ctx (sci/init {})}
          code "(vis/rg \"a\\|b\")"
          err  (parse-error-msg code)
          start-ms (System/currentTimeMillis)
          out  (try-extension-parse-rescue env code err)
          elapsed (- (System/currentTimeMillis) start-ms)]
      (expect (nil? out))
      ;; Sanity: bailout must be sub-second.
      (expect (< elapsed 1000)))))
