(ns com.blockether.vis.extension-test
  "Pure tests for the extension contract \u2014 lives in `vis-extension`
   so the standalone library carries its own coverage and consumers
   that pull only this jar can run them in isolation.

   Cross-package tests that exercise vis-core's runtime composition
   (`active-extensions`, `assemble-system-prompt`, registry
   interaction) live in `vis-core/test/.../extension_api_test.clj`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.extension :as ext]
   [lazytest.core :refer [defdescribe it expect]]))

(def cat-symbol
  (ext/symbol 'cat (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def retries-value
  (ext/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-prompt-rendering-test

  (it "renders canonical prompt text from symbol docstrings + arglists"
    (expect
      (= (str "Filesystem tools (use vis/ prefix; positional args only)\n"
           "- (vis/cat path) or (vis/cat path offset limit) \u2014 Read a file preview.\n"
           "- vis/max-retries \u2014 Maximum retry attempts.\n"
           "RULES:\n"
           "- Discover paths first.")
        (ext/render-prompt
          {:ext/doc "Filesystem tools"
           :ext/ns-alias {:ns 'vis.ext.tools :alias 'vis}
           :ext/symbols [cat-symbol retries-value]
           :usage-note "positional args only"
           :notes ["RULES:" "- Discover paths first."]})))))

(defdescribe extension-builder-test

  (it "extension/symbol validates docstring + arglists"
    (let [s (ext/symbol 'cat (fn [& _] nil)
              {:doc "Read a file." :arglists '([path])})]
      (expect (= 'cat (:ext.symbol/sym s)))
      (expect (= "Read a file." (:ext.symbol/doc s)))
      (expect (= ["(cat path)"] (:ext.symbol/examples s)))))

  (it "extension/value carries doc + value"
    (let [v (ext/value 'cap 42 {:doc "Cap."})]
      (expect (= 42 (:ext.symbol/val v)))
      (expect (= "Cap." (:ext.symbol/doc v)))))

  (it "extension/symbol accepts :autobind-fn"
    (let [autobind-fn (fn [_] {:bindings []})
          symbol-entry (ext/symbol 'cat (fn [& _] nil)
                         {:doc "Read a file."
                          :arglists '([path])
                          :autobind-fn autobind-fn})]
      (expect (= autobind-fn (:ext.symbol/autobind-fn symbol-entry)))))

  (it "extension/extension fills :ext/activation-fn + :ext/classes defaults"
    (let [e (ext/extension
              {:ext/namespace 'com.acme.ext.fs
               :ext/doc       "Filesystem tools"
               :ext/group     "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
               :ext/prompt    "placeholder"
               :ext/symbols   [cat-symbol retries-value]})]
      (expect (fn? (:ext/activation-fn e)))
      (expect (true? ((:ext/activation-fn e) {})))
      (expect (= {} (:ext/classes e)))
      (expect (= {} (:ext/imports e))))))

;; =============================================================================
;; try-rescue-parse-error
;;
;; The iteration loop walks active extensions when SCI/edamame rejects
;; the LLM's source. Resolution order:
;;   1. Per-symbol `:ext.symbol/on-parse-error-fn` whose symbol name
;;      is mentioned in the broken source.
;;   2. Extension-level `:ext/on-parse-error-fn` as a catch-all.
;; First non-nil rewrite wins.
;; =============================================================================

(defn- sym-with-parse-rescue
  [sym-name hook]
  (ext/symbol sym-name (fn [& _] nil)
    {:doc "fixture"
     :arglists '([])
     :on-parse-error-fn hook}))

(defn- ext-with-syms
  ([ns-sym alias-sym syms] (ext-with-syms ns-sym alias-sym syms nil))
  ([ns-sym alias-sym syms ext-hook]
   (ext/extension
     (cond-> {:ext/namespace ns-sym
              :ext/doc       "fixture"
              :ext/group     "filesystem"
              :ext/prompt    "placeholder"
              :ext/ns-alias  {:ns (clojure.core/symbol (str "vis.ext." alias-sym))
                              :alias alias-sym}
              :ext/symbols   syms}
       ext-hook (assoc :ext/on-parse-error-fn ext-hook)))))

(defdescribe try-rescue-parse-error-test

  (it "fires a SYMBOL-level hook only when the broken code mentions it"
    (let [grep (sym-with-parse-rescue 'rg
                 (fn [{:keys [code]}] (str/replace code "X" "Y")))
          ext  (ext-with-syms 'ns-a 'vis [grep])]
      ;; Code mentions `vis/rg` — hook fires.
      (expect (= "(vis/rg \"Y\")"
                (ext/try-rescue-parse-error [ext] "(vis/rg \"X\")" "err" {})))
      ;; Code does NOT mention rg — hook is skipped.
      (expect (nil?
                (ext/try-rescue-parse-error [ext] "(other-tool \"X\")" "err" {})))))

  (it "matches both bare and ns-aliased call forms"
    (let [grep (sym-with-parse-rescue 'rg (fn [_] "REPAIRED"))
          ext  (ext-with-syms 'ns 'vis [grep])]
      (expect (= "REPAIRED"
                (ext/try-rescue-parse-error [ext] "(rg \"x\")" "err" {})))
      (expect (= "REPAIRED"
                (ext/try-rescue-parse-error [ext] "(vis/rg \"x\")" "err" {})))))

  (it "walks every matching symbol; first non-nil rewrite wins"
    (let [a (sym-with-parse-rescue 'foo (fn [_] nil))
          b (sym-with-parse-rescue 'foo (fn [_] "FIRST-WIN"))
          c (sym-with-parse-rescue 'foo (fn [_] (throw (ex-info "never reached" {}))))]
      (expect (= "FIRST-WIN"
                (ext/try-rescue-parse-error
                  [(ext-with-syms 'na 'a [a b c])]
                  "(a/foo)" "err" {})))))

  (it "skips a symbol-level hook that throws and keeps walking"
    (let [boom (sym-with-parse-rescue 'foo (fn [_] (throw (RuntimeException. "boom"))))
          good (sym-with-parse-rescue 'foo (fn [_] "REPAIRED"))]
      (expect (= "REPAIRED"
                (ext/try-rescue-parse-error
                  [(ext-with-syms 'na 'a [boom good])]
                  "(a/foo)" "err" {})))))

  (it "falls back to the EXTENSION-level hook when no symbol matches"
    (let [grep (sym-with-parse-rescue 'rg (fn [_] "NEVER"))
          ext  (ext-with-syms 'ns 'vis [grep] (fn [_] "FROM-EXT"))]
      ;; No mention of rg — symbol hook skipped — ext hook fires.
      (expect (= "FROM-EXT"
                (ext/try-rescue-parse-error [ext] "(unrelated)" "err" {})))))

  (it "prefers SYMBOL-level rescue over the extension-level fallback"
    (let [grep (sym-with-parse-rescue 'rg (fn [_] "FROM-SYM"))
          ext  (ext-with-syms 'ns 'vis [grep] (fn [_] "FROM-EXT"))]
      (expect (= "FROM-SYM"
                (ext/try-rescue-parse-error [ext] "(vis/rg \"x\")" "err" {})))))

  (it "passes :code, :error, :sym, :environment to symbol hooks"
    (let [seen (atom nil)
          grep (sym-with-parse-rescue 'rg
                 (fn [ctx] (reset! seen ctx) nil))
          ext  (ext-with-syms 'ns 'vis [grep])]
      (ext/try-rescue-parse-error [ext] "(vis/rg)" "the-err" {:env :sentinel})
      (expect (= {:code        "(vis/rg)"
                  :error       "the-err"
                  :sym         'rg
                  :environment {:env :sentinel}}
                @seen))))

  (it "is a no-op on an empty extension list"
    (expect (nil? (ext/try-rescue-parse-error [] "x" "e" {})))
    (expect (nil? (ext/try-rescue-parse-error nil "x" "e" {})))))
