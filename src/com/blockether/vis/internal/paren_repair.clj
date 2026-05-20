(ns com.blockether.vis.internal.paren-repair
  "Delimiter repair for LLM-emitted Clojure blocks.

  Adapted from clojure-mcp's `clojure-mcp.sexp.paren-utils` /
  `clojure-mcp.delimiter`: use Parinfer indent mode, then accept the
  repaired text only when Edamame confirms delimiter balance."
  (:require [edamame.core :as edamame])
  (:import [com.oakmac.parinfer Parinfer]))

(def ^:private delimiter-parse-opts
  {:all true
   :read-cond second
   :readers (fn [_tag] (fn [data] data))
   :auto-resolve name})

(defn delimiter-error?
  "True when `s` has a delimiter-shaped Edamame parse error."
  [s]
  (try
    (edamame/parse-string-all (or s "") delimiter-parse-opts)
    false
    (catch clojure.lang.ExceptionInfo ex
      (let [data (ex-data ex)]
        (and (= :edamame/error (:type data))
          (contains? data :edamame/opened-delimiter))))
    (catch Exception _
      true)))

(defn parinfer-repair
  "Return repaired code text, or nil when Parinfer cannot make it safe."
  [code-str]
  (let [code-str (or code-str "")
        res      (Parinfer/indentMode code-str nil nil nil false)]
    (when (and (.success res)
            (not= (.text res) code-str)
            (not (delimiter-error? (.text res))))
      (.text res))))

(defn maybe-repair-delimiters
  "Try Parinfer only for delimiter errors; return repaired text or nil."
  [code-str]
  (when (delimiter-error? code-str)
    (parinfer-repair code-str)))
