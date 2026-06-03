(ns com.blockether.vis.internal.agents
  "Project-guidance discovery — internal, no extension required.

   Reads `AGENTS.md` (preferred) or `CLAUDE.md` (fallback) at the
   active workspace root and exposes its full contents as the data
   behind `(vis/main-agent-instructions)` and the PROJECT-INSTRUCTIONS
   system block in `internal.prompt`.

   Strict precedence: AGENTS.md wins; CLAUDE.md is only consulted when
   AGENTS.md is absent. No user-global merge — the rules a project ships
   in its repo root are the rules.

   Size policy: NO truncation. The whole file goes into the system
   prompt verbatim. Provider prompt caching amortizes the cost across
   every iter in the session; trimming would risk dropping the very
   rule the user is testing. The cwd + (path, mtime, length) marker
   cache below ensures we read the file at most once per change.

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

(defn- repo-cwd ^java.io.File []
  ;; Treat the active workspace root as the repo root. The channel
  ;; rebinds `*workspace-root*` per turn; calling outside that binding
  ;; throws — that's a channel-layer bug, not silent degradation.
  (workspace/cwd))

(defn- read-bytes-safely
  "Read the entire file `f` into a byte array. Returns
   `{:bytes byte-array :total-bytes long}` or `{:error string}` on
   I/O failure. No size cap — the whole file rides in the system
   prompt; provider prompt caching keeps the cost amortized."
  [^java.io.File f]
  (try
    (let [total (.length f)
          n     (long total)
          buf   (byte-array n)]
      (with-open [in (java.io.FileInputStream. f)]
        (loop [off 0]
          (when (< off n)
            (let [r (.read in buf off (- n off))]
              (when (pos? r) (recur (+ off r)))))))
      {:bytes buf :total-bytes total})
    (catch Throwable t
      (tel/log! {:level :warn :id ::read-failed
                 :data  {:path  (.getAbsolutePath f)
                         :error (ex-message t)}})
      {:error (ex-message t)})))

(defn- ->utf8-string ^String [^bytes b ^long len]
  (String. b 0 (int len) java.nio.charset.StandardCharsets/UTF_8))

(defn- read-instructions-file
  [source ^java.io.File f]
  (let [{:keys [bytes total-bytes] err :error} (read-bytes-safely f)]
    (if err
      {:found? false
       :source source
       :path   (.getAbsolutePath f)
       :error  err}
      (let [content (->utf8-string bytes total-bytes)]
        {:found?  true
         :source  source
         :path    (.getAbsolutePath f)
         :bytes   total-bytes
         :content content}))))

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
      (.getPath (workspace/cwd)))))

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
                 :path \"…\" :bytes N :content \"…\"}`
     absent:   `{:found? false}`

   No truncation: `:content` is the full file verbatim. Caller-side
   display limits (e.g. tape pretty-printing) are caller business."
  []
  (:result (current)))

(defn read-warnings
  "Vec of warning maps for AGENTS.md / CLAUDE.md read failures.
   Empty when the file isn't present at all (absence isn't a warning)
   or when a present file read cleanly."
  []
  (or (:warnings (current)) []))
