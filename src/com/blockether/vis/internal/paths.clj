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

(defn abbreviate-home
  "Shorten an absolute path for DISPLAY by replacing the user's home dir with
   `~`, matching the footer/navigator/dialogs. Only rewrites when `path` is at
   or under home (so `/etc/x` is left alone); returns `path` unchanged
   otherwise. Nil-safe."
  ^String [path]
  (let
    [path
     (some-> path
             str)

     home
     (System/getProperty "user.home")]

    (cond (nil? path) path
          (and (seq home) (= path home)) "~/"
          (and (seq home) (str/starts-with? path (str home "/"))) (str "~" (subs path (count home)))
          :else path)))

(defn logs-dir
  "Directory for vis diagnostic logs — `~/.vis/logs`. A DEDICATED subdir (not
   `~/.vis` itself) so the native file tools and the Python sandbox can be
   granted always-on access to logs without exposing `config.edn`, the session
   DB, or gateway tokens. Returns the path string (native separators are fine
   for real I/O)."
  ^String []
  (str (System/getProperty "user.home") "/.vis/logs"))

(defn ensure-logs-dir!
  "Create `~/.vis/logs` (and parents) when absent; return its path string.
   Never throws."
  ^String []
  (let [d (logs-dir)]
    (try (.mkdirs (java.io.File. d)) (catch Throwable _ nil))
    d))