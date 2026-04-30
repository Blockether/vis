(ns com.blockether.vis.ext.channel-tui.external-opener
  "Shell out to the host OS's URL/file opener so the TUI can hand a
   clicked link/image/file reference off to the user's preferred
   external viewer.

   Responsibilities, in order:

     1. Classify the candidate target (URL or path string) into a
        `:scheme` keyword \u2014 one of `:http`, `:https`, `:file`,
        `:rel`, or `:rejected`. Anything not on the whitelist is
        rejected at this layer; nothing dangerous reaches a
        ProcessBuilder.

     2. Resolve the target to a host-friendly form. Relative paths
        are anchored at `(fs/cwd)` and re-checked for `..`-traversal
        \u2014 same guard the editing tools enforce. Returns nil when
        the path escapes.

     3. Build the OS-appropriate command vector
        (`open` / `xdg-open` / `cmd /c start`) for `ProcessBuilder`.

     4. Spawn it with stdio redirected to /dev/null so a chatty
        opener (e.g. `xdg-open` printing to stderr on Linux) cannot
        corrupt the Lanterna screen buffer.

   Pure-ish: every step except `open!` itself is a function of its
   args plus `os.name` / `(fs/cwd)`. `open!` shells out and never
   throws \u2014 errors land in the returned result map."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str])
  (:import
   (java.io File)))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Scheme classification
;; =============================================================================

(def ^:private scheme-re
  "Match the scheme-and-colon prefix of a URI, RFC-3986 style:
   `scheme = ALPHA *( ALPHA / DIGIT / \"+\" / \"-\" / \".\" )`.
   Used to peel a leading scheme off `s` so we can check it against
   the whitelist without false-positives on Windows drive letters
   (`C:\\foo`) \u2014 the `\"\\\\\"` after `:` rules those out."
  #"^([A-Za-z][A-Za-z0-9+\-.]*):")

(defn classify-scheme
  "Return one of `:http`, `:https`, `:file`, `:rel`, or `:rejected`
   for `s`. `:rel` covers anything without an explicit scheme \u2014
   bare paths like `src/foo.clj` or `./diagram.png` \u2014 which is
   the common case for `(md/file-link \u2026)` and relative `(md/image
   \u2026)` references.

   Pure."
  [s]
  (cond
    (or (nil? s) (str/blank? (str s))) :rejected
    :else
    (let [t (str/trim (str s))
          m (re-find scheme-re t)]
      (cond
        (nil? m)
        ;; No scheme prefix => relative path. The cwd-escape check
        ;; happens later in `safe-target`.
        :rel

        :else
        (case (str/lower-case (nth m 1))
          "http"  :http
          "https" :https
          "file"  :file
          :rejected)))))

;; =============================================================================
;; cwd-anchored path safety
;; =============================================================================

(defn- under-cwd?
  "True when the resolved absolute path lives under `(fs/cwd)`."
  [^File f]
  (let [cwd      (.normalize (.toAbsolutePath (fs/path (fs/cwd))))
        resolved (.normalize (.toPath f))]
    (.startsWith resolved cwd)))

(defn- file-url->path
  "Strip the `file:` scheme from `s` and decode percent-escapes.

   Recognised shapes (RFC 8089 + the ad-hoc `file:relative` some
   producers emit):
     file:///abs/path       → /abs/path
     file://host/abs/path   → /abs/path        (host dropped — we don’t support remote)
     file:/abs/path         → /abs/path
     file:relative          → relative          (rare)

   The result preserves leading `/` for absolute paths so callers
   can still distinguish abs vs rel — my earlier `/{0,3}` glob
   collapsed the two and silently re-anchored absolute file:// URLs
   under `(fs/cwd)`, which then double-prefixed and escaped."
  ^String [^String s]
  (let [no-scheme (str/replace-first s #"(?i)^file:" "")
        stripped  (cond
                    ;; file:///abs/path  or  file://host/abs/path
                    (str/starts-with? no-scheme "//")
                    (let [after-slashes (subs no-scheme 2)
                          slash-idx     (.indexOf after-slashes "/")]
                      (if (neg? slash-idx)
                        ;; file://host  with no path — nothing useful
                        ""
                        (subs after-slashes slash-idx)))
                    :else no-scheme)]
    (try (java.net.URLDecoder/decode stripped "UTF-8")
      (catch Throwable _ stripped))))

(defn- strip-line-anchor
  "Drop a trailing `#Lline` anchor a `(md/file-link path line)` call
   produced. Returns `[path line-or-nil]`. Pure."
  [^String s]
  (let [m (re-find #"^(.*)#L(\d+)$" s)]
    (if m
      [(nth m 1) (parse-long (nth m 2))]
      [s nil])))

(defn safe-target
  "Resolve `s` to a host-friendly opener target. Returns:

     {:scheme :http|:https|:file|:rel
      :target \"<absolute path or full URL>\"
      :line   N | nil}

   or nil when the input is rejected (bad scheme, blank,
   `..`-escape).

   Pure modulo `(fs/cwd)`."
  [s]
  (when-not (or (nil? s) (str/blank? (str s)))
    (let [scheme (classify-scheme s)]
      (case scheme
        :rejected nil

        (:http :https)
        ;; URLs pass through as-is. We deliberately do NOT validate
        ;; further (e.g. unicode hostnames) \u2014 the host opener
        ;; will reject anything malformed downstream.
        {:scheme scheme :target (str/trim (str s)) :line nil}

        :file
        (let [decoded     (file-url->path (str/trim (str s)))
              [path line] (strip-line-anchor decoded)
              ;; Absolute paths bypass cwd-anchoring; relative paths
              ;; resolve under (fs/cwd). Both flow through the same
              ;; cwd-escape guard before we hand the result back.
              file        (.normalize
                            (if (str/starts-with? path "/")
                              (fs/path path)
                              (fs/path (fs/cwd) path)))
              f           (.toFile file)]
          (when (under-cwd? f)
            {:scheme :file :target (.getAbsolutePath f) :line line}))

        :rel
        (let [[path line] (strip-line-anchor (str/trim (str s)))
              file        (.normalize (fs/path (fs/cwd) path))
              f           (.toFile file)]
          (when (under-cwd? f)
            {:scheme :rel :target (.getAbsolutePath f) :line line}))))))

;; =============================================================================
;; OS dispatch
;; =============================================================================

(defn- os-name
  "Lower-cased `os.name` system property. Indirected so tests can
   `with-redefs` it."
  ^String []
  (str/lower-case (or (System/getProperty "os.name") "")))

(defn open-command
  "Vec of process args for the host OS. Pure modulo `os-name`.
   Returns nil for unsupported platforms.

   The Linux branch starts with `xdg-open`; the caller is responsible
   for falling back through the chain (`gio open`, `kde-open`,
   `gnome-open`) when spawn fails. Encoding fallback chains here
   would make the fn impure and the failure path harder to test."
  [^String target]
  (let [os (os-name)]
    (cond
      (str/includes? os "mac")     ["open" target]
      (str/includes? os "darwin")  ["open" target]
      (str/includes? os "windows") ["cmd" "/c" "start" "" target]
      ;; Bucket linux + every BSD-ish unix into the freedesktop chain.
      (or (str/includes? os "linux")
        (str/includes? os "bsd")
        (str/includes? os "sunos")
        (str/includes? os "aix"))
      ["xdg-open" target]
      :else nil)))

(def ^:private linux-fallbacks
  "Ordered fallback openers for the Linux/BSD branch when `xdg-open`
   isn't on PATH or fails to spawn. `kde-open5` covers KDE's split
   between v4 and v5; `gio open` is the systemd-friendly default
   for GNOME 3.x+."
  [["gio" "open"] ["kde-open5"] ["kde-open"] ["gnome-open"]])

;; =============================================================================
;; Side-effecting spawn
;; =============================================================================

(defn- spawn!
  "Spawn `argv` with stdio redirected to /dev/null so a chatty opener
   cannot corrupt the Lanterna screen. Returns nil on success,
   otherwise the Throwable for the caller to inspect."
  [argv]
  (try
    (let [pb (ProcessBuilder. ^java.util.List argv)
          dn java.lang.ProcessBuilder$Redirect/DISCARD]
      (.redirectOutput pb dn)
      (.redirectError pb dn)
      (.start pb)
      nil)
    (catch Throwable t t)))

(defn open!
  "Open `s` via the host OS opener. Never throws.

   Returns:
     {:status   :ok | :rejected-scheme | :path-escape | :no-opener | :spawn-failed
      :command  argv-vec | nil
      :scheme   keyword | nil
      :target   resolved-target | nil
      :error    nil | error-string}"
  [s]
  (let [scheme (classify-scheme s)]
    (cond
      (= scheme :rejected)
      {:status :rejected-scheme :command nil :scheme nil :target nil
       :error (str "Rejected scheme for: " (pr-str s))}

      :else
      (if-let [{:keys [target] :as resolved} (safe-target s)]
        (if-let [argv (open-command target)]
          (let [err (spawn! argv)]
            (cond
              (nil? err)
              {:status :ok :command argv :scheme (:scheme resolved)
               :target target :error nil}

              ;; On Linux, fall through the freedesktop chain when
              ;; `xdg-open` itself is missing.
              (and (= "xdg-open" (first argv))
                (instance? java.io.IOException err))
              (loop [chain linux-fallbacks]
                (if-let [head (first chain)]
                  (let [argv* (conj (vec head) target)
                        err*  (spawn! argv*)]
                    (if (nil? err*)
                      {:status :ok :command argv* :scheme (:scheme resolved)
                       :target target :error nil}
                      (recur (next chain))))
                  {:status :spawn-failed :command argv :scheme (:scheme resolved)
                   :target target
                   :error (str "No working opener found on PATH: xdg-open / gio / kde-open / gnome-open all failed.")}))

              :else
              {:status :spawn-failed :command argv :scheme (:scheme resolved)
               :target target :error (.getMessage ^Throwable err)}))
          {:status :no-opener :command nil :scheme (:scheme resolved)
           :target target
           :error (str "No opener available for OS: "
                    (System/getProperty "os.name"))})
        ;; safe-target returned nil — path-escape (the only way
        ;; a known-good scheme can land here).
        {:status :path-escape :command nil :scheme scheme :target nil
         :error (str "Path escapes the working directory: " (pr-str s))}))))
