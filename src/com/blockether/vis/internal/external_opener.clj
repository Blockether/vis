(ns com.blockether.vis.internal.external-opener
  "Shell out to the host OS opener so Vis can hand a URL or local file
   path off to the user's preferred external browser/viewer.

   Responsibilities, in order:

     1. Classify the candidate target into a whitelisted scheme keyword:
        `:http`, `:https`, `:file`, `:rel`, or `:rejected`.

     2. Resolve the target to a host-friendly form. Relative paths are
        anchored at the current working directory and re-checked for
        `..` traversal. Returns nil when the path escapes.

     3. Build the OS-appropriate command vector
        (`open` / `xdg-open` / `cmd /c start`) for `ProcessBuilder`.

     4. Spawn it with stdio redirected to /dev/null so a chatty opener
        cannot corrupt terminal output.

   Pure-ish: every step except `open!` itself is a function of its
   args plus `os.name` and the current working directory. `open!`
   shells out and never throws; errors land in the returned result map."
  (:require [babashka.process :as process]
            [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import (java.io File)
           (java.nio.file Path Paths)))

;; =============================================================================
;; Scheme classification
;; =============================================================================

(def ^:private scheme-re
  "Match the scheme-and-colon prefix of a URI, RFC-3986 style:
   `scheme = ALPHA *( ALPHA / DIGIT / \"+\" / \"-\" / \".\" )`.
   Used to peel a leading scheme off `s` so we can check it against
   the whitelist without false-positives on Windows drive letters
   (`C:\\foo`)."
  #"^([A-Za-z][A-Za-z0-9+\-.]*):")

(defn classify-scheme
  "Return one of `:http`, `:https`, `:file`, `:rel`, or `:rejected`
   for `s`. `:rel` covers anything without an explicit scheme, like
   `src/foo.clj` or `./diagram.png`."
  [s]
  (cond (or (nil? s) (str/blank? (str s))) :rejected
        :else (let [t
                    (str/trim (str s))

                    m
                    (re-find scheme-re t)]

                (cond (nil? m) :rel
                      :else (case (str/lower-case (nth m 1))
                              "http"
                              :http

                              "https"
                              :https

                              "file"
                              :file

                              :rejected)))))

;; =============================================================================
;; cwd-anchored path safety
;; =============================================================================

(defn- cwd-path
  "Normalized absolute explicit workspace cwd as a Path. Indirected
   so tests can redefine it."
  ^Path []
  (.normalize (.toAbsolutePath (.toPath (workspace/cwd)))))

(defn- path-of
  ^Path [^String first-segment & more-segments]
  (Paths/get first-segment (into-array String more-segments)))

(defn- under-cwd?
  "True when the resolved absolute path lives under `cwd-path`."
  [^File f]
  (let [cwd
        (cwd-path)

        resolved
        (.normalize (.toPath f))]

    (.startsWith resolved cwd)))

(defn- resolve-segment ^Path [^Path base ^String segment] (.resolve base segment))

(defn- file-url->path
  "Strip the `file:` scheme from `s` and decode percent-escapes.

   Recognized shapes:
     file:///abs/path  -> /abs/path
     file://host/path  -> /path
     file:/abs/path    -> /abs/path
     file:relative     -> relative"
  ^String [^String s]
  (let [no-scheme
        (str/replace-first s #"(?i)^file:" "")

        stripped
        (cond (str/starts-with? no-scheme "//")
              (let [after-slashes
                    (subs no-scheme 2)

                    slash-idx
                    (.indexOf after-slashes "/")]

                (if (neg? slash-idx) "" (subs after-slashes slash-idx)))
              :else no-scheme)]

    (try (java.net.URLDecoder/decode stripped "UTF-8") (catch Throwable _ stripped))))

(defn- strip-line-anchor
  "Drop a trailing `#Lline` anchor a file-link produced. Returns
   `[path line-or-nil]`."
  [^String s]
  (let [m (re-find #"^(.*)#L(\d+)$" s)]
    (if m [(nth m 1) (parse-long (nth m 2))] [s nil])))

(defn safe-target
  "Resolve `s` to a host-friendly opener target. Returns:

     {:scheme :http|:https|:file|:rel
      :target \"<absolute path or full URL>\"
      :line   N | nil}

   or nil when the input is rejected (bad scheme, blank, `..` escape)."
  [s]
  (when-not (or (nil? s) (str/blank? (str s)))
    (let [scheme (classify-scheme s)]
      (case scheme
        :rejected
        nil

        (:http :https)
        {:scheme scheme :target (str/trim (str s)) :line nil}

        :file
        (let [decoded (file-url->path (str/trim (str s)))
              [path* line] (strip-line-anchor decoded)
              ^String path path*
              ^Path cwd (cwd-path)
              ^Path resolved
              (if (str/starts-with? path "/") (path-of path) (resolve-segment cwd path))
              ^Path file (.normalize resolved)
              f (.toFile file)]

          (when (under-cwd? f) {:scheme :file :target (.getAbsolutePath f) :line line}))

        :rel
        (let [[path* line] (strip-line-anchor (str/trim (str s)))
              ^String path path*
              ^Path cwd (cwd-path)
              ^Path file (.normalize (resolve-segment cwd path))
              f (.toFile file)]

          (when (under-cwd? f) {:scheme :rel :target (.getAbsolutePath f) :line line}))))))

;; =============================================================================
;; OS dispatch
;; =============================================================================

(defn os-name
  "Lower-cased `os.name` system property. Indirected so tests can
   `with-redefs` it."
  ^String []
  (str/lower-case (or (System/getProperty "os.name") "")))

(defn open-command
  "Vec of process args for the host OS. Pure modulo `os-name`.
   Returns nil for unsupported platforms.

   The Linux branch starts with `xdg-open`; the caller is responsible
   for falling back through the chain (`gio open`, `kde-open`,
   `gnome-open`) when spawn fails."
  [^String target]
  (let [os (os-name)]
    (cond (str/includes? os "mac") ["open" target]
          (str/includes? os "darwin") ["open" target]
          (str/includes? os "windows") ["cmd" "/c" "start" "" target]
          (or (str/includes? os "linux")
              (str/includes? os "bsd")
              (str/includes? os "sunos")
              (str/includes? os "aix"))
          ["xdg-open" target]
          :else nil)))

(def ^:private linux-fallbacks [["gio" "open"] ["kde-open5"] ["kde-open"] ["gnome-open"]])

(defn- editor-target
  "Format a local file target for editor CLIs that accept optional
   line suffixes."
  [target line]
  (if line (str target ":" line) target))

(defn- file-editor-commands
  "Preferred GUI editor commands for local file links. These are tried
   before the generic OS opener so Markdown file-link resources go
   to an editor, not a browser/file manager. Missing commands are fine:
   `open-file-in-editor!` falls back to `open!`."
  [target line]
  (let [t (editor-target target line)]
    [["code" "-g" t] ["cursor" "-g" t] ["cursor" "--goto" t] ["zed" t]]))

;; =============================================================================
;; Side-effecting spawn
;; =============================================================================

(defn- spawn!
  "Spawn `argv` with stdio redirected to /dev/null. Returns nil on
   success, otherwise the Throwable for the caller to inspect."
  [argv]
  (try (process/process {:cmd argv :out :discard :err :discard}) nil (catch Throwable t t)))

(defn- spawn-first!
  "Try argv candidates in order. Returns the winning argv, or nil if
   every candidate failed to spawn."
  [commands]
  (loop [[argv & more] commands]
    (when argv
      (let [err (spawn! argv)]
        (if (nil? err) argv (recur more))))))

(defn open!
  "Open `s` via the host OS opener. Never throws.

   Returns:
     {:status  :ok | :rejected-scheme | :path-escape | :no-opener | :spawn-failed
      :command argv-vec | nil
      :scheme  keyword | nil
      :target  resolved-target | nil
      :error   nil | error-string}"
  [s]
  (let [scheme (classify-scheme s)]
    (cond
      (= scheme :rejected) {:status :rejected-scheme
                            :command nil
                            :scheme nil
                            :target nil
                            :error (str "Rejected scheme for: " (pr-str s))}
      :else
      (if-let [{:keys [target] :as resolved} (safe-target s)]
        (if-let [argv (open-command target)]
          (let [err (spawn! argv)]
            (cond
              (nil? err)
              {:status :ok :command argv :scheme (:scheme resolved) :target target :error nil}
              (and (= "xdg-open" (first argv)) (instance? java.io.IOException err))
              (loop [chain linux-fallbacks]
                (if-let [head (first chain)]
                  (let [argv* (conj (vec head) target)
                        err* (spawn! argv*)]

                    (if (nil? err*)
                      {:status :ok
                       :command argv*
                       :scheme (:scheme resolved)
                       :target target
                       :error nil}
                      (recur (next chain))))
                  {:status :spawn-failed
                   :command argv
                   :scheme (:scheme resolved)
                   :target target
                   :error
                   "No working opener found on PATH: xdg-open / gio / kde-open / gnome-open all failed."}))
              :else {:status :spawn-failed
                     :command argv
                     :scheme (:scheme resolved)
                     :target target
                     :error (.getMessage ^Throwable err)}))
          {:status :no-opener
           :command nil
           :scheme (:scheme resolved)
           :target target
           :error (str "No opener available for OS: " (System/getProperty "os.name"))})
        {:status :path-escape
         :command nil
         :scheme scheme
         :target nil
         :error (str "Path escapes the working directory: " (pr-str s))}))))

(defn open-file-in-editor!
  "Open local file target `s` in a GUI editor when possible, preserving
   `#Lline` anchors for editor CLIs. Falls back to `open!` for missing
   editors, non-local targets, rejected paths, and unsupported shapes.
   Never throws."
  [s]
  (if-let [{:keys [scheme target line]} (safe-target s)]
    (if (#{:file :rel} scheme)
      (if-let [argv (spawn-first! (file-editor-commands target line))]
        {:status :ok :command argv :scheme scheme :target target :line line :error nil}
        (open! s))
      (open! s))
    (open! s)))
