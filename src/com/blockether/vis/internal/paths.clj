(ns com.blockether.vis.internal.paths
  "Cross-platform path helpers. A LEAF namespace (no project deps) so any
   layer — core, extensions, tests — can normalize without a require cycle."
  (:require [clojure.string :as str]))

(defn unixify
  "Normalize a path string to `/` separators on every OS. Java's `File`/`Path`
   APIs yield `\\` on Windows, and there is no stdlib/`fs` call that hands back
   a `/`-string there — so this is the single canonical normalizer.

   Use it ONLY where a path is DATA: compared, glob-matched, shown to the model,
   or embedded in a URL / wire / DB. NEVER for real filesystem I/O — `io/file`,
   `.exists`, JGit, nio all take native paths fine. Returns nil for nil input."
  ^String [s]
  (when s (.replace (str s) "\\" "/")))

(defn pathological-index-root?
  "True when `dir` MUST NOT be recursively indexed for fuzzy file-finding or
   content search: the user's HOME directory or a filesystem root (`/`, `C:\\`).
   These are never project workspaces — on a real machine they carry Library/,
   ~/.m2, node_modules, and caches with millions of files, so a full fff scan
   never finishes and freezes the tool (observed: `vis` launched from `~`
   hangs). Callers degrade to a no-index path instead of walking the tree.
   `dir` is a java.io.File; comparison is canonical. Never throws."
  [^java.io.File dir]
  (try (let [canon
             (.getCanonicalFile dir)

             home
             (when-let [h (not-empty (System/getProperty "user.home"))]
               (.getCanonicalFile (java.io.File. ^String h)))]

         (boolean (or (nil? (.getParentFile ^java.io.File canon)) ; filesystem root
                      (and home (= canon home))))) ; the home directory itself
       (catch Throwable _ false)))

(defn abbreviate-home
  "Shorten an absolute path for DISPLAY by replacing the user's home dir with
   `~`, matching the footer/navigator/dialogs. Only rewrites when `path` is at
   or under home (so `/etc/x` is left alone); returns `path` unchanged
   otherwise. Nil-safe."
  ^String [path]
  (let [path
        (some-> path
                str)

        home
        (System/getProperty "user.home")]

    (cond (nil? path) path
          (and (seq home) (= path home)) "~"
          (and (seq home) (str/starts-with? path (str home "/"))) (str "~" (subs path (count home)))
          :else path)))