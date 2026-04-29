(ns com.blockether.vis.ext.lang-clojure.core
  "Aggregator for the `vis-language-clojure` extension. Registers two
   extensions from one classpath manifest:

     clj  (zedit)
     z    (every public fn of rewrite-clj.zip)

   The `z/` alias exists specifically so the agent can navigate and
   edit zippers from inside a `(clj/zedit ...)` callback without
   inventing non-existent symbols. Every entry in `z/` is a real
   `rewrite-clj.zip` public var; the loader builds the binding list
   from `ns-publics` at extension-load time so any future rewrite-clj
   release lights up automatically."
  (:require
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.ext.lang-clojure.zedit :as zedit]
   [rewrite-clj.zip]))

(defn- var->symbol-entry
  "Convert a `rewrite-clj.zip` public var into an SDK symbol entry.

   - Functions (non-macro, non-special-form) get `sdk/symbol` with
     arglists/doc lifted from var metadata.
   - Macros are skipped: SCI cannot eval host-defined macros from a
     bare var binding, and rewrite-clj.zip publishes a handful of
     them. The model can read the doc and `(z/<macro> ...)` would
     just blow up.
   - Plain values get `sdk/value`."
  [sym v]
  (let [m         (meta v)
        macro?    (:macro m)
        arglists  (some-> (:arglists m) vec)
        doc       (or (:doc m) (str "rewrite-clj.zip/" sym))
        target    @v]
    (cond
      macro?
      nil

      (fn? target)
      (sdk/symbol sym target
        (cond-> {:doc doc}
          arglists (assoc :arglists arglists)
          (nil? arglists) (assoc :arglists '([& args]))))

      :else
      (sdk/value sym target {:doc doc}))))

(def ^:private z-namespace-symbols
  "Every public var of `rewrite-clj.zip`, exposed under the `z/` alias
   inside the SCI sandbox. Built once at extension-load time."
  (->> (ns-publics 'rewrite-clj.zip)
    (sort-by key)
    (keep (fn [[sym v]] (var->symbol-entry sym v)))
    vec))

(def clojure-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.lang-clojure.core
     :ext/doc       "Clojure structured editing: clj/zedit + the rewrite-clj zipper bound under z/."
     :ext/version   "0.5.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.clj :alias 'clj}
     :ext/group     "languages"
     :ext/prompt    zedit/clojure-prompt
     :ext/symbols   zedit/clojure-symbols}))

(def zip-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.lang-clojure.zip-bindings
     :ext/doc       "rewrite-clj.zip published under the `z/` alias for use inside (clj/zedit) callbacks."
     :ext/version   "0.5.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.clj.z :alias 'z}
     :ext/group     "languages"
     :ext/symbols   z-namespace-symbols}))

(sdk/register-extension! clojure-extension)
(sdk/register-extension! zip-extension)
