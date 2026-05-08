(ns com.blockether.vis.ext.foundation.environment.agents
  "Project-guidance discovery: reads `<repo>/AGENTS.md` (preferred)
   or `<repo>/CLAUDE.md` (fallback) and exposes its contents as the
   data behind `(vis/main-agent-instructions)` plus the
   `<project-guidance>` block in the system prompt.

   Strict precedence: AGENTS.md wins; CLAUDE.md is only consulted
   when AGENTS.md is absent. No user-global merge - the rules a
   project ships in its repo root are the rules. See plan §1 Q4.

   Size policy: inline up to `MAX_BYTES` verbatim. Beyond that,
   render the first `MAX_BYTES` followed by an inline truncation
   marker that names the missing byte count and points at
   `(vis/main-agent-instructions)` for the full content. Same
   threshold whether the source is AGENTS.md or the CLAUDE.md
   fallback (your repo's CLAUDE.md is 47 KB and would truncate).
   See plan §1 Q5.

   Failure modes (file unreadable, permissions, I/O error, etc.)
   land in the scan-warnings vec, NOT in the rendered prompt block -
   model isn't bound by rules it can't see, but the user/agent
   knows something is broken. See plan §1 Q10."
  (:require
   [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

(def ^:const MAX_BYTES
  "Byte-truncate ceiling for inlined project guidance. Fixed
   constant, no config knob (plan Q5)."
  16384)

(defn- repo-cwd ^java.io.File []
  ;; Treat the JVM's working directory as the repo root.
  ;; Discovery is cwd-keyed; vis snapshot cache uses the same
  ;; convention, so cd-ing into a sibling repo gets fresh state.
  (java.io.File. ^String (System/getProperty "user.dir")))

(defn- read-bytes-safely
  "Read up to `(inc MAX_BYTES)` bytes from `path`. Returns
   {:bytes byte-array :total-bytes long :truncated? bool}
   or {:error string} on I/O failure. Reading one extra byte
   lets us detect truncation deterministically without `(.length file)`,
   which can race with a writer."
  [^java.io.File f]
  (try
    (let [total (.length f)
          cap   (inc MAX_BYTES)
          n     (long (min cap total))
          buf   (byte-array n)]
      (with-open [in (java.io.FileInputStream. f)]
        (loop [off 0]
          (when (< off n)
            (let [r (.read in buf off (- n off))]
              (when (pos? r) (recur (+ off r)))))))
      {:bytes       buf
       :total-bytes total
       :truncated?  (> total MAX_BYTES)})
    (catch Throwable t
      (tel/log! {:level :warn :id ::read-failed
                 :data  {:path  (.getAbsolutePath f)
                         :error (ex-message t)}})
      {:error (ex-message t)})))

(defn- truncation-marker
  "Inline marker that closes a truncated block. Names the missing
   byte count and points at the read-fn that returns the full
   content."
  [^long original-bytes]
  (str "\n\n[TRUNCATED - " (- original-bytes MAX_BYTES)
    " more bytes. Read full content via (vis/main-agent-instructions).]"))

(defn- ->utf8-string ^String [^bytes b ^long len]
  (String. b 0 (int len) java.nio.charset.StandardCharsets/UTF_8))

(defn- read-instructions-file
  "Returns the structured map for a single candidate file.
     {:found? true :source <kw> :path \"...\" :bytes long
      :content \"...\" :truncated? bool :original-bytes long}
   or {:found? false :source <kw> :path \"...\" :error \"...\"}
   when the file exists but couldn't be read."
  [source ^java.io.File f]
  (let [{:keys [bytes total-bytes truncated?] err :error} (read-bytes-safely f)]
    (cond
      err
      {:found?         false
       :source         source
       :path           (.getAbsolutePath f)
       :error          err}

      truncated?
      (let [head    (->utf8-string bytes MAX_BYTES)
            content (str head (truncation-marker total-bytes))]
        {:found?         true
         :source         source
         :path           (.getAbsolutePath f)
         :bytes          (count (.getBytes content "UTF-8"))
         :content        content
         :truncated?     true
         :original-bytes total-bytes})

      :else
      (let [content (->utf8-string bytes total-bytes)]
        {:found?         true
         :source         source
         :path           (.getAbsolutePath f)
         :bytes          total-bytes
         :content        content
         :truncated?     false
         :original-bytes total-bytes}))))

(defn scan-in
  "Scan the given `root` directory for project-guidance files.
   Same return shape as [[scan]]; pure I/O. Exposed for testing
   against fixture roots."
  [^java.io.File root]
  (let [agents-file (java.io.File. root "AGENTS.md")
        claude-file (java.io.File. root "CLAUDE.md")]
    (cond
      (.isFile agents-file)
      (let [r (read-instructions-file :repo agents-file)]
        (if (:found? r)
          {:result r :warnings []}
          {:result   {:found? false}
           :warnings [{:source :agents-md
                       :reason (:error r)
                       :path   (:path r)}]}))

      (.isFile claude-file)
      (let [r (read-instructions-file :repo:claude-md-fallback claude-file)]
        (if (:found? r)
          {:result r :warnings []}
          {:result   {:found? false}
           :warnings [{:source :claude-md-fallback
                       :reason (:error r)
                       :path   (:path r)}]}))

      :else
      {:result {:found? false} :warnings []})))

(defn scan
  "Scan the cwd for project-guidance files. See [[scan-in]]."
  []
  (scan-in (repo-cwd)))

;; ---------------------------------------------------------------------------
;; Cache.
;;
;; Private `defonce` so the atom survives a `(require :reload)` during
;; an extension reload (per plan caveat: extensions holding mutable
;; state across reload MUST use defonce). Keyed on canonical cwd so a
;; mid-session `cd` invalidates implicitly. Explicit invalidation via
;; `reload!`. See plan §1 Q8.
;; ---------------------------------------------------------------------------

(defonce ^:private state
  (atom {:cwd nil :result nil :warnings nil}))

(defn- canonical-cwd ^String []
  (try (.getCanonicalPath ^java.io.File (repo-cwd))
    (catch Throwable _ (System/getProperty "user.dir"))))

(defn current
  "Return the cached scan result, computing it on first access or
   when cwd changes. Recomputed automatically when cwd differs."
  []
  (let [cwd     (canonical-cwd)
        cached  @state]
    (if (= cwd (:cwd cached))
      cached
      (let [{:keys [result warnings]} (scan)
            v {:cwd cwd :result result :warnings warnings}]
        (reset! state v)
        v))))

(defn reload!
  "Invalidate and recompute the cached scan. Returns the new scan
   result map (same shape as `(current)`). Surfaces as
   `(vis/reload-instructions!)`."
  []
  (let [{:keys [result warnings]} (scan)
        v {:cwd (canonical-cwd) :result result :warnings warnings}]
    (reset! state v)
    v))

;; ---------------------------------------------------------------------------
;; Public-ish helpers used by the foundation aggregator and the
;; render module. Returned shapes match plan §3 / §1 Q6 (always
;; maps; `:found?` flag; never nil).
;; ---------------------------------------------------------------------------

(defn instructions
  "Return the structured map behind `(vis/main-agent-instructions)`.
   Always a map. `:found?` discriminates present vs absent.

     present:  {:found? true  :source :repo|:repo:claude-md-fallback
                :path \"...\" :bytes N :content \"...\"
                :truncated? bool :original-bytes N}
     absent:   {:found? false}"
  []
  (:result (current)))

(defn scan-warnings
  "Vec of warning maps for AGENTS.md / CLAUDE.md read failures.
   Empty when the file isn't present at all (absence isn't a
   warning) or when a present file read cleanly."
  []
  (or (:warnings (current)) []))
