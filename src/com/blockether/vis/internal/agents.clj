(ns com.blockether.vis.internal.agents
  "Project-guidance discovery — internal, no extension required.

   STACKED context files, pi-style: guidance is collected from THREE
   layers and all of them ride into the PROJECT-INSTRUCTIONS system
   block, outermost first:

     1. user-global   `~/.vis/AGENTS.md` (or `~/.vis/CLAUDE.md`)
     2. ancestors     `AGENTS.md` / `CLAUDE.md` in every ancestor
                      directory of the workspace root (outermost first)
     3. workspace     `AGENTS.md` / `CLAUDE.md` at the workspace root
   4. added roots   `AGENTS.md` / `CLAUDE.md` at each ADDED filesystem
                    root's own directory (folders granted beyond the
                    primary workspace — no ancestor walk)

   Per DIRECTORY precedence is strict: AGENTS.md wins; CLAUDE.md is
   only consulted when AGENTS.md is absent in that directory. Across
   directories nothing is dropped — nearer files are rendered LATER so
   they positionally override outer rules on conflict.

   Size policy: NO truncation. Every file goes into the system prompt
   verbatim. Provider prompt caching amortizes the cost across every
   iter in the session; trimming would risk dropping the very rule the
   user is testing. The cwd + per-file (path, mtime, length) marker
   cache below ensures files are re-read at most once per change.

   Failure modes (file unreadable, permissions, I/O error) land in the
   read-warning vec, NOT in the rendered prompt. The model isn't bound
   by rules it can't see, but the host knows something is broken.

   This namespace replaces the foundation-core/environment/agents.clj
   that used to live in the extension. Project-guidance discovery is
   core functionality (drives the system prompt + slim ctx digest); the
   extension layer no longer owns it."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel]))

(defn- repo-cwd
  ^java.io.File []
  ;; Treat the active workspace root as the repo root. The channel
  ;; rebinds `*workspace-root*` per turn; calling outside that binding
  ;; falls back to the process cwd (REPL / test / one-off CLI paths).
  (workspace/cwd))

(defn- global-config-dir ^java.io.File [] (java.io.File. (System/getProperty "user.home") ".vis"))

(defn- read-bytes-safely
  "Read the entire file `f` into a byte array. Returns
   `{:bytes byte-array :total-bytes long}` or `{:error string}` on
   I/O failure. No size cap — the whole file rides in the system
   prompt; provider prompt caching keeps the cost amortized."
  [^java.io.File f]
  (try (let [total
             (.length f)

             n
             (long total)

             buf
             (byte-array n)]

         (with-open [in (java.io.FileInputStream. f)]
           (loop [off 0]
             (when (< off n)
               (let [r (.read in buf off (- n off))]
                 (when (pos? r) (recur (+ off r)))))))
         {:bytes buf :total-bytes total})
       (catch Throwable t
         (tel/log! {:level :warn
                    :id ::read-failed
                    :data {:path (.getAbsolutePath f) :error (ex-message t)}})
         {:error (ex-message t)})))

(defn- ->utf8-string
  ^String [^bytes b ^long len]
  (String. b 0 (int len) java.nio.charset.StandardCharsets/UTF_8))

(defn- read-instructions-file
  [source ^java.io.File f]
  (let [{:keys [bytes total-bytes] err :error} (read-bytes-safely f)]
    (if err
      {:found? false :source source :path (.getAbsolutePath f) :error err}
      (let [content (->utf8-string bytes total-bytes)]
        {:found? true
         :source source
         :path (.getAbsolutePath f)
         :bytes total-bytes
         :content content}))))

;; =============================================================================
;; Single-root scan (legacy shape — kept for the foundation shim + tests)
;; =============================================================================

(defn scan-in
  "Scan `root` for project-guidance files. Pure I/O. Single-directory,
   legacy result shape (`:source :repo` / `:repo:claude-md-fallback`);
   the stacked multi-file scan is `scan-roots` / `scan`. Exposed for
   testing against fixture roots."
  [^java.io.File root]
  (let [agents-file
        (java.io.File. root "AGENTS.md")

        claude-file
        (java.io.File. root "CLAUDE.md")]

    (cond (.isFile agents-file) (let [r (read-instructions-file :repo agents-file)]
                                  (if (:found? r)
                                    {:result r :warnings []}
                                    {:result {:found? false}
                                     :warnings
                                     [{:source :agents-md :reason (:error r) :path (:path r)}]}))
          (.isFile claude-file)
          (let [r (read-instructions-file :repo:claude-md-fallback claude-file)]
            (if (:found? r)
              {:result r :warnings []}
              {:result {:found? false}
               :warnings [{:source :claude-md-fallback :reason (:error r) :path (:path r)}]}))
          :else {:result {:found? false} :warnings []})))

;; =============================================================================
;; Stacked scan (global → ancestors → workspace root)
;; =============================================================================

(defn- guidance-candidate
  "Pick the guidance file for one directory: AGENTS.md wins, CLAUDE.md
   is the per-directory fallback. Returns `{:source :agents-md|:claude-md
   :file f}` or nil."
  [^java.io.File dir]
  (let [a
        (java.io.File. dir "AGENTS.md")

        c
        (java.io.File. dir "CLAUDE.md")]

    (cond (.isFile a) {:source :agents-md :file a}
          (.isFile c) {:source :claude-md :file c}
          :else nil)))

(defn- ancestor-chain
  "Canonical directory chain from the filesystem root DOWN TO `root`
   inclusive — outermost first, so render order = precedence order
   (nearer files land later and positionally override)."
  [^java.io.File root]
  (loop [^java.io.File d
         (try (.getCanonicalFile root) (catch Throwable _ (.getAbsoluteFile root)))

         acc
         ()]

    (if d (recur (.getParentFile d) (cons d acc)) (vec acc))))

(defn- read-guidance-entry
  "Read one candidate into a stacked-file entry, or a warning on I/O
   failure. `scope` is :global | :ancestor | :project | :extra-root."
  [scope {:keys [source ^java.io.File file]}]
  (let [{:keys [bytes total-bytes] err :error} (read-bytes-safely file)]
    (if err
      {:warning {:source source :scope scope :reason err :path (.getAbsolutePath file)}}
      {:entry {:scope scope
               :source source
               :path (.getAbsolutePath file)
               :bytes total-bytes
               :content (->utf8-string bytes total-bytes)}})))

(defn origin-label
  "Human label for a stacked-file entry: file name + scope qualifier."
  [{:keys [scope source]}]
  (str (case source
         :agents-md
         "AGENTS.md"

         :claude-md
         "CLAUDE.md"

         (str source))
       (case scope
         :global
         " (user-global)"

         :ancestor
         " (ancestor directory)"

         :project
         " (workspace root)"

         :extra-root
         " (added folder)"

         "")))

(defn- combined-content
  "Render the stacked entries into ONE guidance string with per-file
   origin headers (single file → verbatim, no header). This is the
   `:content` consumers of the legacy single-file shape see."
  [entries]
  (if (= 1 (count entries))
    (:content (first entries))
    (str/join "\n\n"
              (map (fn [e]
                     (str "# ── " (origin-label e) ": " (:path e) " ──\n" (:content e)))
                   entries))))

(defn- legacy-source
  "Back-compat top-level `:source` derived from the INNERMOST entry:
   the workspace-root file keeps the historical :repo /
   :repo:claude-md-fallback keywords; a stack that ends on an outer
   layer reports its scope."
  [{:keys [scope source]}]
  (if (= :project scope)
    (case source
      :agents-md
      :repo

      :claude-md
      :repo:claude-md-fallback

      source)
    scope))

(defn scan-roots
  "Stacked scan: guidance file in `global-dir` first (when non-nil),
   then AGENTS.md / CLAUDE.md from every ancestor of `workspace-root`
   (outermost first) down to the workspace root itself, then each added
   `extra-root`'s OWN directory (no ancestor walk — only what the user
   granted). Pure I/O; exposed for testing against fixture roots.

   Precedence is render order: user-global → ancestors → workspace root →
   added roots. Nearer files land LATER and positionally override outer
   rules, so the primary workspace stays authoritative over any added
   folder. An extra-root whose guidance file coincides with an already-seen
   path (the workspace root, an ancestor, or a duplicate) is dropped.

   Returns `{:result r :warnings [...]}` where `r` is:

     present: {:found? true
               :files  [{:scope :global|:ancestor|:project|:extra-root
                         :source :agents-md|:claude-md
                         :path \"…\" :bytes N :content \"…\"} …]
               ;; legacy single-file view (innermost file + combined content)
               :source <legacy kw> :path \"…\" :bytes <total> :content \"…\"}
     absent:  {:found? false}"
  ([^java.io.File global-dir ^java.io.File workspace-root]
   (scan-roots global-dir workspace-root nil))
  ([^java.io.File global-dir ^java.io.File workspace-root extra-roots]
   (let [chain
         (ancestor-chain workspace-root)

         n
         (count chain)

         dirs
         (concat (when global-dir [[:global global-dir]])
                 (map-indexed (fn [i d]
                                [(if (= i (dec n)) :project :ancestor) d])
                              chain)
                 (map (fn [d]
                        ;; Canonicalize so dedup matches the (already-canonical)
                        ;; ancestor/workspace candidate paths — a symlinked or
                        ;; relative extra-root path would otherwise escape dedup.
                        (let [c (try (.getCanonicalFile ^java.io.File d) (catch Throwable _ d))]
                          [:extra-root c]))
                      extra-roots))

         ;; The global dir may coincide with an ancestor (e.g. workspace under
         ;; ~/.vis) — drop the duplicate read; the same dedup drops an extra-root
         ;; that coincides with the workspace root, an ancestor, or another extra.
         reads
         (loop [ds
                dirs

                seen
                #{}

                acc
                []]

           (if (empty? ds)
             acc
             (let [[scope ^java.io.File d]
                   (first ds)

                   cand
                   (guidance-candidate d)

                   path
                   (some-> cand
                           ^java.io.File (:file)
                           .getAbsolutePath)]

               (if (and cand (not (contains? seen path)))
                 (recur (rest ds) (conj seen path) (conj acc (read-guidance-entry scope cand)))
                 (recur (rest ds) seen acc)))))

         entries
         (vec (keep :entry reads))

         warnings
         (vec (keep :warning reads))]

     {:result (if (seq entries)
                (let [innermost (peek entries)]
                  {:found? true
                   :files entries
                   :source (legacy-source innermost)
                   :path (:path innermost)
                   :bytes (reduce + 0 (map :bytes entries))
                   :content (combined-content entries)})
                {:found? false})
      :warnings warnings})))

(defn- extra-root-dirs
  "The added filesystem roots' REAL (trunk) dirs as Files, for the stacked
   guidance scan. Only the trunk is read — a draft clone wouldn't carry a
   freshly-added rules file, and live roots have trunk==clone anyway. Reads
   the per-turn `*filesystem-roots*` binding (empty when unbound / single-root)."
  []
  (into []
        (keep (fn [{:keys [trunk]}]
                (some-> trunk
                        str
                        str/trim
                        not-empty
                        java.io.File.)))
        workspace/*filesystem-roots*))

(defn scan
  "Scan the user-global `~/.vis` dir, the active workspace root's ancestor
   chain, and each added filesystem root's own directory for project-guidance
   files (stacked)."
  []
  (scan-roots (global-config-dir) (repo-cwd) (extra-root-dirs)))

;; =============================================================================
;; Marker cache — stat-only revalidation on the hot path
;; =============================================================================

(defonce ^:private state (atom {:cwd nil :marker nil :result nil :warnings nil}))

(defn- canonical-cwd
  ^String []
  (try (.getCanonicalPath ^java.io.File (repo-cwd)) (catch Throwable _ (.getPath (workspace/cwd)))))

(defn- file-marker
  [^java.io.File f]
  {:path (.getAbsolutePath f)
   :file? (.isFile f)
   :last-modified (when (.isFile f) (.lastModified f))
   :length (when (.isFile f) (.length f))})

(defn- dir-marker
  [^java.io.File d]
  [(file-marker (java.io.File. d "AGENTS.md")) (file-marker (java.io.File. d "CLAUDE.md"))])

(defn- guidance-marker
  []
  {:global (dir-marker (global-config-dir))
   :chain (mapv dir-marker (ancestor-chain (repo-cwd)))
   :extras (mapv dir-marker (extra-root-dirs))})

(defn- rescan!
  [cwd marker]
  (let [{:keys [result warnings]}
        (scan)

        v
        {:cwd cwd :marker marker :result result :warnings warnings}]

    (reset! state v)
    v))

(defn current
  "Return the cached scan, recomputing when cwd or any stacked
   AGENTS.md/CLAUDE.md marker changes. Common path is stat-only —
   content is re-read only when a marker changes."
  []
  (let [cwd
        (canonical-cwd)

        marker
        (guidance-marker)

        cached
        @state]

    (if (and (= cwd (:cwd cached)) (= marker (:marker cached))) cached (rescan! cwd marker))))

(defn reload! "Revalidate project guidance and return the current scan result." [] (current))

(defn instructions
  "Return the structured map behind `(vis/main-agent-instructions)`.
   Always a map. `:found?` discriminates present vs absent.

     present:  `{:found? true
                 :files  [{:scope :source :path :bytes :content} …]
                 :source <legacy kw> :path \"…\" :bytes N :content \"…\"}`
     absent:   `{:found? false}`

   `:files` is the stacked view (user-global → ancestors → workspace
   root, outermost first). The top-level `:source`/`:path` describe the
   INNERMOST file and `:content` is the combined origin-headed render —
   the legacy single-file consumers keep working unchanged.

   No truncation: every `:content` is the full file verbatim.
   Caller-side display limits (e.g. tape pretty-printing) are caller
   business."
  []
  (:result (current)))

(defn read-warnings
  "Vec of warning maps for AGENTS.md / CLAUDE.md read failures.
   Empty when no file is present at all (absence isn't a warning)
   or when every present file read cleanly."
  []
  (or (:warnings (current)) []))
