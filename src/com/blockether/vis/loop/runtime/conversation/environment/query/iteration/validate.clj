(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.validate
  "Syntax parsing + code-block and answer validation.

   Pure predicates that decide whether a code block or a final answer is
   well-formed BEFORE any execution happens. Each returns nil (ok) or an
   error string (reject).

   Parsing uses edamame (the same parser SCI uses)."
  (:require
   [clojure.string :as str]
   [edamame.core :as edamame]))

;; ---------------------------------------------------------------------------
;; Syntax parsing (was parse.clj)
;; ---------------------------------------------------------------------------

(def edamame-opts
  "Edamame parser options matching Clojure/SCI syntax.
   :all enables fn literals, deref, var, regex, quote, etc."
  {:all true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(defn check-syntax
  "Parses code with edamame. Returns parsed forms or throws."
  [code]
  (edamame/parse-string-all code edamame-opts))

(defn check-bare-list
  "Detects unquoted list literals like (6 7 8) that parse fine but fail at eval.
   Returns error string or nil."
  [forms]
  (let [first-form (first forms)]
    (when (and (= 1 (count forms))
            (list? first-form) (seq first-form)
            (let [head (first first-form)]
              (not (or (symbol? head) (keyword? head)
                     (list? head) (set? head) (map? head) (vector? head)))))
      (str "Bare list literal: " (pr-str first-form)
        ". Quote it: '(" (str/join " " first-form) ")"))))

(defn parse-clojure-syntax
  "Validates Clojure syntax using edamame (same parser as SCI).
   Returns nil if valid, or an error string if broken."
  [code]
  (try
    (let [forms (check-syntax code)]
      (or (check-bare-list forms)
        nil))
    (catch Throwable e
      (ex-message e))))

;; ---------------------------------------------------------------------------
;; Bare string detection
;; ---------------------------------------------------------------------------

(def ^:private BARE_STRING_RE
  #"^\s*\"[^\"]*\"\s*$")

(defn bare-string-code-block?
  "True when a code expression is a bare string literal — prose that the LLM
   should have put in :answer instead of :code."
  [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

;; ---------------------------------------------------------------------------
;; Data literal detection
;; ---------------------------------------------------------------------------

(defn- data-literal-form?
  [form]
  (or (map? form)
    (vector? form)
    (set? form)
    (keyword? form)
    (number? form)
    (boolean? form)
    (nil? form)
    (char? form)
    (instance? java.util.regex.Pattern form)
    (and (seq? form)
      (= 'quote (first form))
      (= 2 (count form)))))

(defn- bare-data-literal-shape?
  [expr]
  (let [s (str/trim (str expr))]
    (or (str/starts-with? s "{")
      (str/starts-with? s "[")
      (str/starts-with? s "#{")
      (str/starts-with? s ":")
      (str/starts-with? s "'{")
      (str/starts-with? s "'[")
      (str/starts-with? s "'#{")
      (str/starts-with? s "':")
      (re-matches #"[-+]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][-+]?\d+)?" s)
      (= s "true")
      (= s "false")
      (= s "nil"))))

(defn bare-data-literal-code-block?
  "True when a code expression is just a single literal data form."
  [expr]
  (or (try
        (let [forms (check-syntax (str expr))]
          (and (= 1 (count forms))
            (data-literal-form? (first forms))))
        (catch Throwable _ false))
    (bare-data-literal-shape? expr)))

;; ---------------------------------------------------------------------------
;; Comment-only detection
;; ---------------------------------------------------------------------------

(defn- comment-only-block?
  "True when the expr parses to zero forms — only `;;` comments / `#_` discards."
  [^String expr]
  (try
    (zero? (count (edamame/parse-string-all (str/trim expr) edamame-opts)))
    (catch Throwable _ false)))

;; ---------------------------------------------------------------------------
;; Composite code-block error
;; ---------------------------------------------------------------------------

(defn literal-code-block-error
  "Returns a validation error when `expr` is literal payload incorrectly
   emitted in :code, or when it contains no executable form at all.
   Returns nil when the block is fine."
  [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer with answer-type text, not in :code."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))


