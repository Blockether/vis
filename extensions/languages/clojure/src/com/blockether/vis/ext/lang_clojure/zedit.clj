(ns com.blockether.vis.ext.lang-clojure.zedit
  "Structured Clojure-source editing under the `clj/` alias in the SCI
   sandbox.

   Surface (intentionally small):

     (clj/zedit path zfn)        ; rewrite-clj structured edit on a Clojure file

   `zfn` receives a `rewrite-clj.zip` zipper located at the file root
   and must return a zipper. The result is serialized back via
   `z/root-string` and written to disk.

   The full `rewrite-clj.zip` namespace is published into the sandbox
   under the `z/` alias so the model can write `z/find-value`, `z/edit`,
   `z/replace`, `z/right`, `z/sexpr`, etc. without inventing symbols.
   That alias is the recovery path for the `unresolved-symbol` failure
   class we kept seeing in turn post-mortems.

   Hard guard: every path stays inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.core :as sdk]
   [rewrite-clj.zip :as z])
  (:import
   (java.io File)))

;; =============================================================================
;; Path safety (mirrors vis-common-editing; kept local so this extension
;; has no implicit dependency on the editing extension's internals).
;; =============================================================================

(defn- safe-path
  ^File [p]
  ;; Resolve `p` against `(fs/cwd)` and reject any traversal that escapes
  ;; the working directory.

  (let [cwd        (fs/cwd)
        resolved   (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure/path-escape :path (str p)})))
    (.toFile normalized)))

(defn- ensure-existing-file! [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.lang-clojure/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.lang-clojure/path-is-dir :path (.getPath f)})))
  f)

(defn- rel-path [^File f]
  (let [cwd (.toAbsolutePath (fs/path (fs/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))

;; =============================================================================
;; zedit
;; =============================================================================

(defn- zedit-file
  "rewrite-clj structured edit. `zfn` receives a zipper at the file root
   and must return a zipper. The new source is written back."
  [path zfn]
  (let [f    (ensure-existing-file! (safe-path path))
        zloc (z/of-file f {:track-position? true})
        zout (zfn zloc)]
    (when (nil? zout)
      (throw (ex-info "clj/zedit zfn must return a zipper, got nil"
               {:type :ext.lang-clojure/zedit-nil-result})))
    (spit f (z/root-string zout))
    {:path (rel-path f)}))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def zedit-symbol
  (sdk/symbol 'zedit zedit-file
    {:doc      "Structured edit of a Clojure file using rewrite-clj. zfn receives a zipper at the file root and must return a zipper. Use the `z/` alias (rewrite-clj.zip) for navigation/edit ops: z/find-value, z/edit, z/replace, z/right, z/sexpr, etc."
     :arglists '([path zfn])
     :examples ["(clj/zedit \"src/x.clj\" (fn [z] (z/edit z str/upper-case)))"
                "(clj/zedit \"src/x.clj\" (fn [zloc] (-> zloc (z/find-value z/next 'OLD) (z/replace 'NEW))))"]}))

(def clojure-symbols
  [zedit-symbol])

(def clojure-prompt
  "CLOJURE STRUCTURED EDITING. `clj/` alias:
  (clj/zedit path zfn)   rewrite-clj edit. zfn gets zipper at file root, MUST return zipper.

Full `rewrite-clj.zip` bound under `z/`:
  (z/of-string s)                  zipper from string (rare; zedit gives one)
  (z/sexpr zloc)                   s-expr at node
  (z/node zloc) (z/value zloc)     raw node / leaf value
  (z/tag zloc)                     :list :vector :map :token ...

  (z/find zloc move pred)          walk till pred truthy
  (z/find-value zloc move v)       walk till value = v
  (z/find-value zloc v)            default move = z/next
  (z/find-next-value zloc move v)  step then find-value
  (z/right) (z/left) (z/down) (z/up) (z/next) (z/prev)   nav

  (z/replace zloc x)               replace node
  (z/edit zloc f & args)           apply f, replace
  (z/insert-right/-left zloc x)    insert sibling
  (z/append-child zloc x)          append into list/vector/map
  (z/remove zloc)                  drop node
  (z/root-string zloc)             render full tree

Pattern - swap a const:
  (clj/zedit \"src/foo.clj\"
    (fn [z] (-> z (z/find-value z/next 'NAME) z/right (z/replace 4))))

Pattern - add require:
  (clj/zedit \"src/foo.clj\"
    (fn [z] (-> z (z/find-value z/next :require) z/up
                  (z/append-child '[clojure.string :as str]))))

`z/` = host `rewrite-clj.zip`. Don't prefix `rewrite-clj.zip/`. Don't invent symbols. Missing fn? See https://cljdoc.org/d/rewrite-clj/rewrite-clj/.")
