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

(defn- ensure-var-doc-meta
  "Return a copy of `v`'s metadata with `:doc` and `:arglists` filled in when
   the source var didn't carry them. rewrite-clj.zip is a third-party lib and
   some of its vars ship without docstrings; the new `vis/symbol` API requires
   one, so we synthesize a minimal stub instead of crashing extension load."
  [sym v]
  (let [m (meta v)]
    {:doc      (or (:doc m) (str "rewrite-clj.zip/" sym))
     :arglists (or (some-> (:arglists m) vec)
                 (when-not (fn? @v) nil)
                 '([& args]))}))

(defn- macro-entry
  "Build a value entry for an SCI-callable macro shim. The shim's value is
   the marker map `{:vis.sci/macro-fn ...}`; doc/arglists are looked up on
   the underlying rewrite-clj.zip var so the SCI sandbox sees the same docs
   the library ships."
  [sym v macro-fn]
  (let [{:keys [doc arglists]} (ensure-var-doc-meta sym v)
        ;; Wrap a real var around the macro marker map so `vis/value` can
        ;; derive `:doc` and `:arglists` from var meta. Naming the var after
        ;; the SCI symbol keeps the prompt listing aligned with the call.
        macro-var (intern *ns*
                    (with-meta sym {:doc doc :arglists arglists
                                    :private true})
                    {:vis.sci/macro-fn macro-fn})]
    (vis/value macro-var)))

(defn- var->symbol-entry
  "Convert a rewrite-clj.zip public var into an SDK symbol entry.

   Functions/values are direct bindings. Four threading helpers are macros;
   expose SCI-local macro shims that expand to same-namespace helper calls so
   `(z/subedit-> ...)` works inside the sandbox."
  [sym v]
  (let [m      (meta v)
        target @v]
    (cond
      (contains? macro-symbols sym)
      (macro-entry sym v (get macro-symbols sym))

      (:macro m)
      nil

      (fn? target)
      ;; rewrite-clj.zip vars: feed straight in. The new `vis/symbol` API
      ;; reads `:doc`/`:arglists` from var meta and uses `:sym` to override
      ;; the SCI-visible name. We re-meta the var first because some lib
      ;; vars ship without `:doc` or `:arglists`. Renderers default to the
      ;; pr-str pair: rewrite-clj returns zippers/data, never strings.
      (let [{:keys [doc arglists]} (ensure-var-doc-meta sym v)]
        (alter-meta! v assoc :doc doc :arglists arglists)
        (vis/symbol v
          {:sym sym
           :journal-render-fn vis/render-pr-str-journal
           :channel-render-fn vis/render-pr-str-channel}))

      :else
      (let [{:keys [doc]} (ensure-var-doc-meta sym v)]
        (alter-meta! v assoc :doc doc)
        (vis/value v {:sym sym})))))

(def ^:private rewrite-clj-zip-symbols
  ;; Top-level :require already loaded rewrite-clj.zip. Walk its publics
  ;; and convert each one into a v/symbol or v/value entry.
  (->> (ns-publics 'rewrite-clj.zip)
    (sort-by key)
    (keep (fn [[sym v]] (var->symbol-entry sym v)))
    vec))

(defn- clojure-environment-info
  [_environment]
  "Clojure/EDN workspace detected. Prefer the `z/` alias before raw text edits: use `z/forms` first for top-level semantic rows, `z/locators` with `{:depth :all}` for deep zipper rows, `z/symbols`, or `z/locator-for-symbol` for symbol rows; use `z/patch` by adding `:replace` to a chosen span row; use `z/patch-check` before risky batches; use `z/repair-range`, `z/repair-locator`, or `z/repair-file` for parse repair; use `z/diagnostics`, `z/rename-plan`, or `z/clean-ns-plan` for clojure-lsp dry-run semantic checks. Locator rows include `:path`, `:index`, `:span`, `:kind`, `:name`, `:digest`, `:source-preview`, and full `:source`; `:span` rows are safer than lossy sexpr equality. Use `z/inspect` for raw rewrite-clj zlocs. Use `v/patch` only for comments/plain text or non-Clojure files.")

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
     {:sym 'diagnostics

      :journal-render-fn vis/render-pr-str-journal
      :channel-render-fn vis/render-pr-str-channel})
   (vis/symbol #'rename-plan-lazy
     {:sym 'rename-plan

      :journal-render-fn vis/render-pr-str-journal
      :channel-render-fn vis/render-pr-str-channel})
   (vis/symbol #'clean-ns-plan-lazy
     {:sym 'clean-ns-plan

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
     :ext/ns-alias  {:ns 'vis.ext.clj :alias 'z}
     :ext/kind      "languages"
     :ext/activation-fn (fn [_] (clojure-project?))
     :ext/environment-info-fn clojure-environment-info
     :ext/prompt    patch/z-prompt
     :ext/symbols   (into [patch/patch-symbol patch/patch-check-symbol patch/forms-symbol patch/locators-symbol patch/symbols-symbol patch/locator-for-symbol-symbol patch/inspect-symbol]
                      (concat repair/symbols lsp-symbols rewrite-clj-zip-symbols))}))

(vis/register-extension! clojure-extension)
