(ns com.blockether.vis.ext.lang-clojure.core
  "Aggregator for the `vis-language-clojure` extension.

   Single extension under the `z/` alias, carrying:
     - `z/zedit` (the structured-edit entry point), AND
     - every public var of `rewrite-clj.zip` (so `zfn` callbacks can
       call `z/find-value`, `z/replace`, `z/right`, `z/sexpr`, ...).

   Earlier revision split this into two registrations (`clj/zedit`
   plus `z/<rewrite-clj publics>`), which forced the model to mentally
   toggle between two namespaces for one editing flow and produced the
   `unresolved-symbol` failure class we kept seeing in post-mortems.
   ONE alias, ONE surface."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.zedit :as zedit]
   [rewrite-clj.zip]))

(defn- var->symbol-entry
  "Convert a `rewrite-clj.zip` public var into an SDK symbol entry.

   - Functions (non-macro, non-special-form) get `vis/symbol` with
     arglists/doc lifted from var metadata.
   - Macros are skipped: SCI cannot eval host-defined macros from a
     bare var binding, and rewrite-clj.zip publishes a handful of
     them. The model can read the doc and `(z/<macro> ...)` would
     just blow up.
   - Plain values get `vis/value`."
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
      (vis/symbol sym target
        (cond-> {:doc doc}
          arglists (assoc :arglists arglists)
          (nil? arglists) (assoc :arglists '([& args]))))

      :else
      (vis/value sym target {:doc doc}))))

(def ^:private rewrite-clj-zip-symbols
  "Every public var of `rewrite-clj.zip`, exposed under the `z/` alias
   inside the SCI sandbox. Built once at extension-load time so any
   future rewrite-clj release lights up automatically."
  (->> (ns-publics 'rewrite-clj.zip)
    (sort-by key)
    (keep (fn [[sym v]] (var->symbol-entry sym v)))
    vec))

(def clojure-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.lang-clojure.core
     :ext/doc       "Clojure structured editing under the `z/` alias: z/zedit entry + the full rewrite-clj.zip API."
     :ext/version   "0.6.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.clj :alias 'z}
     :ext/kind      "languages"
     :ext/prompt    zedit/z-prompt
     :ext/symbols   (into [zedit/zedit-symbol] rewrite-clj-zip-symbols)}))

(vis/register-extension! clojure-extension)
