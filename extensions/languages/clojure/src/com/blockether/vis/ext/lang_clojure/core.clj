(ns com.blockether.vis.ext.lang-clojure.core
  "Aggregator for the `vis-language-clojure` extension.

   Single extension under the `z/` alias:
     - `z/patch`, `z/forms`, `z/locators`, `z/symbols` from Vis, and
     - the rewrite-clj zipper API, including SCI-callable macro helpers
       `z/edit->`, `z/edit->>`, `z/subedit->`, `z/subedit->>`.

   `z/patch` keeps the v/patch-shaped map but uses rewrite-clj zipper
   locators internally."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.patch :as patch]
   [com.blockether.vis.ext.lang-clojure.repair :as repair]
   [rewrite-clj.node]
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

(defn- rewrite-doc-fallback
  [sym _v]
  (str "rewrite-clj.zip/" sym))

(defn- raw-opts
  [sym]
  {:symbol sym :raw? true :doc-fn rewrite-doc-fallback})

(defn- macro-entry
  "Build a value entry for an SCI-callable macro shim. The shim's value is
   the marker map `{:vis.sci/macro-fn ...}`."
  [sym v macro-fn]
  (vis/value v {:symbol sym
                :val {:vis.sci/macro-fn macro-fn}
                :doc-fn rewrite-doc-fallback}))

(defn- var->symbol-entry
  "Convert a rewrite-clj.zip public var into a raw SDK entry.

   Functions use `vis/symbol` with `:raw? true`, so they compose as plain
   Clojure values instead of observed tool envelopes. Values stay values, and
   macros are skipped unless we supply an SCI macro marker. Four threading
   helpers are macros; expose SCI-local macro shims that expand to
   same-namespace helper calls so `(z/subedit-> ...)` works inside the sandbox."
  [sym v]
  (let [m      (meta v)
        target @v]
    (cond
      (contains? macro-symbols sym)
      (macro-entry sym v (get macro-symbols sym))

      (:macro m)
      nil

      (fn? target)
      (vis/symbol v (raw-opts sym))

      :else
      (vis/value v {:symbol sym :doc-fn rewrite-doc-fallback}))))

(def ^:private rewrite-clj-zip-symbols
  ;; Top-level :require already loaded rewrite-clj.zip. Walk its publics
  ;; and convert each one into a raw symbol, macro value, or plain value entry.
  (->> (ns-publics 'rewrite-clj.zip)
    (sort-by key)
    (keep (fn [[sym v]] (var->symbol-entry sym v)))
    vec))

(defn- clojure-prompt
  [_environment]
  (str "Clojure/EDN: use z/forms, z/locators, or z/symbols to pick rows; add :replace and call z/patch. Use data replacements by default, z/source only for exact bytes, z/patch-check for risky batches, and v/patch outside Clojure/EDN."
    "\n\n"
    patch/z-prompt))

(defn- lazy-lsp-call
  [sym & args]
  (apply (or (requiring-resolve (symbol "com.blockether.vis.ext.lang-clojure.lsp" (name sym)))
           (throw (ex-info "Clojure LSP helper did not resolve"
                    {:type :lang-clojure/missing-lsp-helper
                     :symbol sym})))
    args))

;; -----------------------------------------------------------------------------
;; Lazy LSP shims. Each defn carries the canonical docstring + arglists on its
;; var so `vis/symbol` reads them from var meta. The body just trampolines to
;; the actual lsp helper, loaded on first call.
;; -----------------------------------------------------------------------------

(defn- diagnostics-lazy
  "Return clojure-lsp diagnostics for the project or selected files. Opts: {:project-root p, :filenames [...], :namespace [...], :settings {...}}. Loads clojure-lsp on first call."
  ([] (lazy-lsp-call 'diagnostics))
  ([opts] (lazy-lsp-call 'diagnostics opts)))

(defn- ^{:arglists '([from to] [from to opts])} rename-plan-lazy
  "Dry-run clojure-lsp semantic rename. Returns changed paths and old/new text edits; does not write. Loads clojure-lsp on first call."
  [from to & [opts]]
  (apply lazy-lsp-call 'rename-plan from to (when opts [opts])))

(defn- clean-ns-plan-lazy
  "Dry-run clojure-lsp clean-ns. Returns changed paths and old/new text edits; does not write. Loads clojure-lsp on first call."
  ([] (lazy-lsp-call 'clean-ns-plan))
  ([opts] (lazy-lsp-call 'clean-ns-plan opts)))

(def ^:private lsp-symbols
  [(vis/symbol #'diagnostics-lazy
     {:symbol 'diagnostics

      :journal-render-fn vis/render-pr-str-journal
      :channel-render-fn vis/render-pr-str-channel})
   (vis/symbol #'rename-plan-lazy
     {:symbol 'rename-plan

      :journal-render-fn vis/render-pr-str-journal
      :channel-render-fn vis/render-pr-str-channel})
   (vis/symbol #'clean-ns-plan-lazy
     {:symbol 'clean-ns-plan

      :journal-render-fn vis/render-pr-str-journal
      :channel-render-fn vis/render-pr-str-channel})])

(def clojure-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.lang-clojure.core
     :ext/doc       "Clojure/EDN intelligence under the `z/` alias: z/patch zipper edits, z/forms/z/locators/z/symbols discovery, and rewrite-clj zipper API."
     :ext/version   "0.7.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/alias  {:ns 'vis.ext.clj :alias 'z}
     :ext/kind      "languages"
     :ext/activation-fn (fn [_] (clojure-project?))
     :ext/prompt    clojure-prompt
     :ext/symbols   (into [patch/source-symbol patch/lit-symbol patch/patch-symbol patch/patch-check-symbol patch/forms-symbol patch/locators-symbol patch/symbols-symbol patch/locator-for-symbol-symbol patch/inspect-symbol]
                      (concat repair/symbols lsp-symbols rewrite-clj-zip-symbols))}))

(vis/register-extension! clojure-extension)
