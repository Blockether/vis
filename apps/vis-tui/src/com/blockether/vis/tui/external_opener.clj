(ns com.blockether.vis.tui.external-opener
  "Shell out to the host OS opener so Vis can hand a URL or local file
   path off to the user's preferred external browser/viewer.

   Responsibilities, in order:

     1. Classify the candidate target into a whitelisted scheme keyword:
        `:http`, `:https`, `:file`, `:rel`, or `:rejected`.

     2. Resolve the target to a host-friendly form. Relative paths are
        anchored at the current working directory and re-checked for
        `..` traversal. Returns nil when the path escapes.

     3. Build the ordered OS-appropriate command candidates (`open`, or
        `xdg-open` and its fallbacks, with `wslview` / `explorer.exe`
        first on WSL).

     4. Spawn each candidate with stdio redirected to /dev/null so a chatty
        opener cannot corrupt terminal output. A candidate that fails to
        start or exits non-zero soon after the start passes to the next.

   Pure-ish: every step except `open!` itself is a function of its
   args plus `os.name` and the current working directory. `open!`
   shells out and never throws; errors land in the returned result map."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.workspace :as workspace])
  (:import (java.io File)
           (java.nio.file Path Paths)
           (java.util.concurrent TimeUnit)))

;; Scheme classification

(def ^:private scheme-re
  "Match the scheme-and-colon prefix of a URI, RFC-3986 style:
   `scheme = ALPHA *( ALPHA / DIGIT / \"+\" / \"-\" / \".\" )`.
   Used to peel a leading scheme off `s` so we can check it against
   the whitelist without false-positives on drive-letter-like
   prefixes (`C:/foo`)."
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

;; cwd-anchored path safety

(defn- cwd-path
  "Normalized absolute explicit workspace cwd as a Path. Indirected
   so tests can redefine it."
  ^Path []
  (.normalize (.toAbsolutePath (.toPath ^File (workspace/cwd)))))

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

;; OS dispatch

(defn os-name
  "Lower-cased `os.name` system property. Indirected so tests can
   `with-redefs` it."
  ^String []
  (str/lower-case (or (System/getProperty "os.name") "")))

(def ^:private wsl-host?
  (delay (boolean (or (System/getenv "WSL_DISTRO_NAME")
                      (System/getenv "WSL_INTEROP")
                      (try (str/includes? (str/lower-case (slurp "/proc/version")) "microsoft")
                           (catch Throwable _ false))))))

(defn wsl?
  "True when the JVM runs inside Windows Subsystem for Linux. There the Linux
   openers usually find no browser, so the Windows host must open URLs.
   Indirected so tests can `with-redefs` it."
  []
  @wsl-host?)

(def ^:private linux-fallbacks [["gio" "open"] ["kde-open5"] ["kde-open"] ["gnome-open"]])

(defn open-commands
  "Ordered candidate argv vectors for `target` on the host OS. Pure modulo
   `os-name` and `wsl?`. Returns nil for unsupported platforms.

   The caller tries each candidate until one starts and does not fail early.
   Unix hosts try `xdg-open`, then `gio open`, `kde-open` and `gnome-open`.
   On WSL, `wslview` and `explorer.exe` come first. `explorer.exe` gets only
   URLs, because it cannot read a Linux file path."
  [^String target]
  (let [os (os-name)]
    (cond (or (str/includes? os "mac") (str/includes? os "darwin")) [["open" target]]
          (some #(str/includes? os %) ["linux" "bsd" "sunos" "aix"])
          (let [unix (into [["xdg-open" target]] (map #(conj % target)) linux-fallbacks)]
            (if (wsl?)
              (into (cond-> [["wslview" target]]
                      (#{:http :https} (classify-scheme target))
                      (conj ["explorer.exe" target]))
                    unix)
              unix))
          :else nil)))

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

;; Side-effecting spawn

(def ^:private early-exit-ms
  "How long `spawn!` waits for an opener to fail. An opener that hands the
   target off exits fast; one that still runs counts as started."
  1500)

(def ^:private exit-code-ignored
  "Openers whose exit code says nothing about the result. `explorer.exe`
   exits with 1 also after it opens the URL."
  #{"explorer.exe"})

(defn- early-failure
  "An ex-info when `child` exits non-zero within `early-exit-ms`, else nil.
   `xdg-open` exits with 3 when it finds no browser, and the spawn itself
   succeeds, so only the exit code shows that failure."
  [argv ^Process child]
  (when (and (.waitFor child early-exit-ms TimeUnit/MILLISECONDS)
             (not (zero? (.exitValue child)))
             (not (contains? exit-code-ignored (first argv))))
    (ex-info (str (first argv) " exited with code " (.exitValue child))
             {:command argv :exit (.exitValue child)})))

(defn- spawn!
  "Spawn `argv` with stdio redirected to /dev/null, then wait briefly for an
   early failure. Returns nil on success, otherwise the Throwable for the
   caller to inspect: the spawn error or an ex-info for a non-zero exit."
  [argv]
  (try (let [child (.start (doto (ProcessBuilder. ^java.util.List argv)
                             (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                             (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)))]
         ;; No input belongs to a detached opener; close the pipe rather than
         ;; leaving it waiting on the TUI or holding a descriptor until GC.
         (.close (.getOutputStream child))
         (early-failure argv child))
       (catch Throwable t t)))

(defn- spawn-first!
  "Try argv candidates in order. Returns `{:command argv}` for the first
   candidate that starts without an early failure, else `{:errors [msg ...]}`
   with one message for each failed candidate."
  [commands]
  (loop [[argv & more]
         commands

         errors
         []]

    (if-not argv
      {:errors errors}
      (if-let [err (spawn! argv)]
        (recur more (conj errors (str (first argv) ": " (or (ex-message err) (str (class err))))))
        {:command argv}))))

(defn- launch!
  "Open the resolved `target` with the first working OS opener. Returns the
   result map of `open!`. Never throws."
  [scheme target]
  (if-let [commands (open-commands target)]
    (let [{:keys [command errors]} (spawn-first! commands)]
      (if command
        {:status :ok :command command :scheme scheme :target target :error nil}
        {:status :spawn-failed
         :command (first commands)
         :scheme scheme
         :target target
         :error (str "No working opener for " target ": " (str/join "; " errors))}))
    {:status :no-opener
     :command nil
     :scheme scheme
     :target target
     :error (str "No opener available for OS: " (System/getProperty "os.name"))}))

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
    (if (= scheme :rejected)
      {:status :rejected-scheme
       :command nil
       :scheme nil
       :target nil
       :error (str "Rejected scheme for: " (pr-str s))}
      (if-let [{:keys [target] :as resolved} (safe-target s)]
        (launch! (:scheme resolved) target)
        {:status :path-escape
         :command nil
         :scheme scheme
         :target nil
         :error (str "Path escapes the working directory: " (pr-str s))}))))

(defn open-local!
  "Open the LOCAL file at `path` with the generic OS opener (Preview /
   default viewer), WITHOUT the cwd confinement `open!` applies.

   For targets vis itself produced — e.g. the inline-image temp PNGs
   `plt.show()` writes under the system temp dir — never for arbitrary
   model-provided links. Returns the same result-map shape as `open!`.
   Never throws."
  [path]
  (let [f (File. (str path))]
    (if (.isFile f)
      (launch! :file (.getAbsolutePath f))
      {:status :rejected-scheme
       :command nil
       :scheme nil
       :target nil
       :error (str "Not a local file: " (pr-str path))})))

(defn open-file-in-editor!
  "Open local file target `s` in a GUI editor when possible, preserving
   `#Lline` anchors for editor CLIs. Falls back to `open!` for missing
   editors, non-local targets, rejected paths, and unsupported shapes.
   Never throws."
  [s]
  (if-let [{:keys [scheme target line]} (safe-target s)]
    (if (#{:file :rel} scheme)
      (if-let [argv (:command (spawn-first! (file-editor-commands target line)))]
        {:status :ok :command argv :scheme scheme :target target :line line :error nil}
        (open! s))
      (open! s))
    (open! s)))
