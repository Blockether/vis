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
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.zedit :as zedit]
   [rewrite-clj.zip])
  (:import
   (java.io File)))

(def ^:private clojure-marker-files
  #{"deps.edn" "bb.edn" "project.clj" "build.boot" "shadow-cljs.edn"})

(def ^:private clojure-source-exts
  #{"clj" "cljc" "cljs" "edn"})

(def ^:private ignored-scan-dirs
  #{".git" ".hg" ".svn" "target" "node_modules" ".cpcache" ".clj-kondo"})

(def ^:private activation-scan-limit 2000)

(defn- extension [^File f]
  (when-let [n (some-> f .getName)]
    (when-let [idx (str/last-index-of n ".")]
      (subs n (inc idx)))))

(defn- clojure-source-file? [^File f]
  (and (.isFile f)
    (contains? clojure-source-exts (extension f))))

(defn- scan-children [^File dir]
  (->> (or (seq (.listFiles dir)) [])
    (remove (fn [^File f]
              (and (.isDirectory f)
                (contains? ignored-scan-dirs (.getName f)))))))

(defn- clojure-project?
  "True when `root` looks like a Clojure project. Kept local to the language
   extension so activation does not depend on foundation's environment scan."
  ([] (clojure-project? (File. ".")))
  ([^File root]
   (let [root (.getCanonicalFile root)]
     (or (some (fn [name] (.exists (File. root name))) clojure-marker-files)
       (loop [stack (seq (scan-children root))
              seen  0]
         (cond
           (or (nil? stack) (>= seen activation-scan-limit)) false
           (clojure-source-file? (first stack)) true
           (.isDirectory ^File (first stack))
           (recur (concat (scan-children (first stack)) (next stack)) (inc seen))
           :else
           (recur (next stack) (inc seen))))))))

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
     :ext/activation-fn (fn [_] (clojure-project?))
     :ext/prompt    zedit/z-prompt
     :ext/symbols   (into [zedit/zedit-symbol] rewrite-clj-zip-symbols)}))

(vis/register-extension! clojure-extension)
