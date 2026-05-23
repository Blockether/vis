(ns com.blockether.vis.internal.agents
  "Project-guidance discovery — internal, no extension required.

   Reads `AGENTS.md` (preferred) or `CLAUDE.md` (fallback) at the
   active workspace root and exposes its contents as the data behind
   `(vis/main-agent-instructions)` and the per-iteration `:session/env`
   `:project/agents-md?` flag.

   Strict precedence: AGENTS.md wins; CLAUDE.md is only consulted when
   AGENTS.md is absent. No user-global merge — the rules a project ships
   in its repo root are the rules.

   Size policy: inline up to `MAX_BYTES` verbatim. Beyond that, render
   the first `MAX_BYTES` followed by an inline truncation marker that
   names the missing byte count and points at `vis/main-agent-instructions`
   for the full content.

   Failure modes (file unreadable, permissions, I/O error) land in the
   read-warning vec, NOT in the rendered prompt. The model isn't bound
   by rules it can't see, but the host knows something is broken.

   This namespace replaces the foundation-core/environment/agents.clj
   that used to live in the extension. Project-guidance discovery is
   core functionality (drives the system prompt + slim ctx digest); the
   extension layer no longer owns it."
  (:require
   [com.blockether.vis.internal.workspace :as workspace]
   [taoensso.telemere :as tel]))

(def ^:const MAX_BYTES
  "Byte-truncate ceiling for inlined project guidance."
  16384)

(defn- repo-cwd ^java.io.File []
  ;; Treat the active workspace root as the repo root. The channel
  ;; rebinds `*workspace-root*` per turn; calling outside that binding
  ;; throws — that's a channel-layer bug, not silent degradation.
  (workspace/cwd))

(defn- read-bytes-safely
  "Read up to `(inc MAX_BYTES)` bytes from `f`. Returns
   `{:bytes byte-array :total-bytes long :truncated? bool}` or
   `{:error string}` on I/O failure. Reading one extra byte lets us
   detect truncation deterministically without `.length` races."
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
  [^long original-bytes]
  (str "\n\n[TRUNCATED - " (- original-bytes MAX_BYTES)
    " more bytes. Read full content via (vis/main-agent-instructions).]"))

(defn- ->utf8-string ^String [^bytes b ^long len]
  (String. b 0 (int len) java.nio.charset.StandardCharsets/UTF_8))

(defn- read-instructions-file
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
  "Scan `root` for project-guidance files. Pure I/O. Returned shape
   matches `scan`; exposed for testing against fixture roots."
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
  "Scan the active workspace root for project-guidance files."
  []
  (scan-in (repo-cwd)))

(defonce ^:private state
  (atom {:cwd nil :marker nil :result nil :warnings nil}))

(defn- canonical-cwd ^String []
  (try (.getCanonicalPath ^java.io.File (repo-cwd))
    (catch Throwable _
      (or workspace/*workspace-root*
        (System/getProperty "user.dir")))))

(defn- file-marker
  [^java.io.File f]
  {:path          (.getAbsolutePath f)
   :file?         (.isFile f)
   :last-modified (when (.isFile f) (.lastModified f))
   :length        (when (.isFile f) (.length f))})

(defn- guidance-marker
  []
  (let [root (repo-cwd)]
    {:agents (file-marker (java.io.File. root "AGENTS.md"))
     :claude (file-marker (java.io.File. root "CLAUDE.md"))}))

(defn- rescan!
  [cwd marker]
  (let [{:keys [result warnings]} (scan)
        v {:cwd cwd :marker marker :result result :warnings warnings}]
    (reset! state v)
    v))

(defn current
  "Return the cached scan, recomputing when cwd or AGENTS.md/CLAUDE.md
   marker changes. Common path is stat-only — content is re-read only
   when the marker changes."
  []
  (let [cwd     (canonical-cwd)
        marker  (guidance-marker)
        cached  @state]
    (if (and (= cwd (:cwd cached)) (= marker (:marker cached)))
      cached
      (rescan! cwd marker))))

(defn reload!
  "Revalidate project guidance and return the current scan result."
  []
  (current))

(defn instructions
  "Return the structured map behind `(vis/main-agent-instructions)`.
   Always a map. `:found?` discriminates present vs absent.

     present:  `{:found? true :source :repo|:repo:claude-md-fallback
                 :path \"\u2026\" :bytes N :content \"\u2026\"
                 :truncated? bool :original-bytes N}`
     absent:   `{:found? false}`"
  []
  (:result (current)))

(defn read-warnings
  "Vec of warning maps for AGENTS.md / CLAUDE.md read failures.
   Empty when the file isn't present at all (absence isn't a warning)
   or when a present file read cleanly."
  []
  (or (:warnings (current)) []))
