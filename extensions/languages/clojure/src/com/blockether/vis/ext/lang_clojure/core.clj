(ns com.blockether.vis.ext.lang-clojure.core
  "Aggregator for the `vis-language-clojure` extension.

   Single extension under the `z/` alias:
     - `z/patch`, `z/locators`, `z/symbols` from Vis, and
     - the rewrite-clj zipper API, including SCI-callable macro helpers
       `z/edit->`, `z/edit->>`, `z/subedit->`, `z/subedit->>`.

   `z/patch` keeps the v/patch-shaped map but uses rewrite-clj zipper
   locators internally."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.lsp :as lsp]
   [com.blockether.vis.ext.lang-clojure.patch :as patch]
   [com.blockether.vis.ext.lang-clojure.repair :as repair]
   [com.blockether.vis.ext.lang-clojure.xref :as xref]
   [rewrite-clj.zip]
   [rewrite-clj.zip.subedit])
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

(defn- thread-macro
  [helper-sym thread-sym]
  (fn [_form _env zloc & body]
    (let [g (gensym "zloc__")]
      (list helper-sym
        zloc
        (list 'fn* [g]
          (apply list thread-sym g body))))))

(def ^:private macro-symbols
  {'edit->     (thread-macro 'vis.ext.clj/edit-node 'clojure.core/->)
   'edit->>    (thread-macro 'vis.ext.clj/edit-node 'clojure.core/->>)
   'subedit->  (thread-macro 'vis.ext.clj/subedit-node 'clojure.core/->)
   'subedit->> (thread-macro 'vis.ext.clj/subedit-node 'clojure.core/->>)})

(defn- macro-entry
  [sym macro-fn doc arglists]
  (vis/value sym {:vis.sci/macro-fn macro-fn}
    {:doc (or doc (str "SCI-callable rewrite-clj.zip macro " sym))
     :arglists arglists}))

(defn- var->symbol-entry
  "Convert a rewrite-clj.zip public var into an SDK symbol entry.

   Functions/values are direct bindings. Four threading helpers are macros;
   expose SCI-local macro shims that expand to same-namespace helper calls so
   `(z/subedit-> ...)` works inside the sandbox."
  [sym v]
  (let [m        (meta v)
        arglists (some-> (:arglists m) vec)
        doc      (or (:doc m) (str "rewrite-clj.zip/" sym))
        target   @v]
    (cond
      (contains? macro-symbols sym)
      (macro-entry sym (get macro-symbols sym) doc arglists)

      (:macro m)
      nil

      (fn? target)
      (vis/symbol sym target
        (cond-> {:doc doc}
          arglists (assoc :arglists arglists)
          (nil? arglists) (assoc :arglists '([& args]))))

      :else
      (vis/value sym target {:doc doc}))))

(def ^:private rewrite-clj-zip-symbols
  (->> (ns-publics 'rewrite-clj.zip)
    (sort-by key)
    (keep (fn [[sym v]] (var->symbol-entry sym v)))
    vec))

(defn- clojure-environment-info
  [_environment]
  "Clojure/EDN workspace detected. Prefer the `z/` alias before raw text edits: use `z/xref-analyze!`, `z/who-calls`, `z/calls-who`, and `z/context-for` for semantic graph context; use `z/locators`, `z/symbols`, `z/locators-for-symbol`, or `z/locator-for-ref` for rewrite-clj locator rows; use `z/repair-range`, `z/repair-locator`, or `z/repair-file` for parse repair over row/col ranges; use `z/diagnostics`, `z/rename-plan`, or `z/clean-ns-plan` for clojure-lsp dry-run semantic checks; then use `z/patch` for structural Clojure/EDN changes. Locator rows include `:path`, `:index`, `:tag`, `:value`, `:locator`, `:source`, and `:span`; add `:replace` to a row to turn it into a patch edit. Use `v/patch` only for comments/plain text or non-Clojure files.")

(def clojure-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.lang-clojure.core
     :ext/doc       "Clojure/EDN intelligence under the `z/` alias: clj-xref graph queries, z/patch zipper edits, z/locators/z/symbols discovery, and rewrite-clj zipper API."
     :ext/version   "0.7.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.clj :alias 'z}
     :ext/kind      "languages"
     :ext/activation-fn (fn [_] (clojure-project?))
     :ext/environment-info-fn clojure-environment-info
     :ext/prompt    patch/z-prompt
     :ext/symbols   (into [patch/patch-symbol patch/locators-symbol patch/symbols-symbol patch/locator-for-symbol-symbol]
                      (concat repair/symbols xref/symbols lsp/symbols rewrite-clj-zip-symbols))}))

(vis/register-extension! clojure-extension)
