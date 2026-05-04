(ns com.blockether.vis.ext.lang-clojure.zedit
  "Structured Clojure-source editing.

   Surface (intentionally small):

     (z/zedit path zfn)        ; rewrite-clj structured edit on a Clojure file

   `zfn` receives a `rewrite-clj.zip` zipper located at the file root
   and must return a zipper. The result is serialized back via
   `z/root-string` and written to disk.

   `z/zedit` lives under the same `z/` alias as the rewrite-clj zipper
   API itself (`z/find-value`, `z/replace`, `z/right`, `z/sexpr`, ...)
   so the model has ONE namespace for Clojure structured editing
   instead of toggling between `clj/` (entry) and `z/` (navigation).
   That alias is the recovery path for the `unresolved-symbol` failure
   class we kept seeing in turn post-mortems.

   Hard guard: every path stays inside the conversation's working
   directory (`fs/cwd`); `..` traversal is rejected before any I/O."
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.core :as vis]
   [rewrite-clj.zip :as z])
  (:import
   (java.io File)))

;; =============================================================================
;; Path safety (mirrors vis-foundation; kept local so this extension
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
      (throw (ex-info "z/zedit zfn must return a zipper, got nil"
               {:type :ext.lang-clojure/zedit-nil-result})))
    (spit f (z/root-string zout))
    {:path (rel-path f)}))

;; =============================================================================
;; Symbol declarations
;; =============================================================================

(def zedit-symbol
  (vis/symbol 'zedit zedit-file
    {:doc      "Structured Clojure edit. zfn gets root zipper, returns zipper, file is rewritten."
     :arglists '([path zfn])
     :examples ["(z/zedit \"src/x.clj\" (fn [zl] (z/edit zl str/upper-case)))"
                "(z/zedit \"src/x.clj\" (fn [zl] (-> zl (z/find-value z/next 'OLD) (z/replace 'NEW))))"]}))

(def z-prompt
  "`z/` Clojure structured edit:
  Use z/zedit for Clojure edits. Do not use v/bash for grep/sed/nl/cat/find on Clojure source; use v/rg/v/cat/v/glob for search/read/path discovery.
  (z/zedit path (fn [zl] ...)) ; root zipper -> return zipper -> file rewritten; full rewrite-clj.zip API under z/
  Go-to/default way to change non-trivial .clj/.cljc/.cljs/.edn forms/data.
  nav: z/down z/up z/right z/left z/next z/prev ; skips whitespace/comments, preserves output
  find whole-file: (z/find-value zloc z/next 'sym) ; short form searches siblings only
  inspect/edit: z/sexpr z/tag z/position-span z/replace z/edit z/append-child z/remove
Example: (z/zedit \"src/foo.clj\" #(-> % (z/find-value z/next 'OLD) (z/replace 'NEW)))")
