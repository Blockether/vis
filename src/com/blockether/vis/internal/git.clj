(ns com.blockether.vis.internal.git
  "Shared workspace inspection, backed by the native `git` binary.

   UI surfaces and extensions depend on this namespace instead of embedding
   their own git calls. It reports small, renderable facts about the current
   repository (branch, dirty buckets, ahead/behind, porcelain entries) by
   shelling out to `git status --porcelain=v2` and `git rev-parse`, parsed
   into stable Clojure maps. No JGit — the only git implementation is the one
   already on the user's PATH, so behaviour matches their shell exactly.

   Workspace lifecycle mutations stay in
   `com.blockether.vis.internal.workspace`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.util.concurrent TimeUnit]))

(def ^:private default-cache-ms 15000)

(def ^:private default-git-timeout-secs 10)

(defonce ^:private working-tree-status-cache (atom {:cwd nil :expires-at 0 :value nil}))

;; One in-flight refresh at a time — a CAS guard so a burst of footer repaints
;; can't spawn N concurrent git walks.
(defonce ^:private status-refreshing? (atom false))

(defn cwd-file
  "Canonical current working directory as a File. Indirected for tests."
  ^File []
  (.getCanonicalFile (workspace/cwd)))

;; =============================================================================
;; git subprocess primitive
;; =============================================================================

(defn run-git
  "Run `git <args>` in `dir` (a File). `args` is a seq of stringable tokens.
   Returns `{:exit <int|nil> :out <stdout> :err <stderr> :duration-ms <long>}`;
   `:exit` is nil when the process could not be spawned or timed out. Never
   throws — repo-less and git-less environments degrade to `{:exit nil …}`.
   The 3-arity opts map takes `:timeout-secs` (default 10)."
  ([^File dir args] (run-git dir args nil))
  ([^File dir args {:keys [timeout-secs]}]
   (let [t0 (System/currentTimeMillis)]
     (try
       (let [cmd (into ["git"] (map str) args)
             pb (ProcessBuilder. ^java.util.List cmd)]

         (.directory pb (or dir (cwd-file)))
         (let [p (.start pb)
               out (future (slurp (io/reader (.getInputStream p))))
               err (future (slurp (io/reader (.getErrorStream p))))
               done (.waitFor p (long (or timeout-secs default-git-timeout-secs)) TimeUnit/SECONDS)]

           (when-not done
             (.destroyForcibly p)
             (doseq [^java.io.InputStream s [(.getInputStream p) (.getErrorStream p)]]
               (try (.close s)
                    (catch Throwable t
                      (tel/log! :debug
                                ["git: failed to close timed-out process stream"
                                 (ex-message t)])))))
           {:exit (when done (.exitValue p))
            :out (deref out 2000 "")
            :err (deref err 2000 "")
            :timed-out? (not done)
            :duration-ms (- (System/currentTimeMillis) t0)}))
       (catch Throwable t
         (tel/log! :warn
                   ["git: run-git failed"
                    {:dir (some-> dir
                                  .getPath)
                     :args (vec (map str args))
                     :error (ex-message t)}])
         {:exit nil
          :out ""
          :err ""
          :timed-out? false
          :duration-ms (- (System/currentTimeMillis) t0)})))))

(defn- git-ok
  "stdout of `git <args>` when it exits 0, else nil."
  [^File dir args]
  (let [{:keys [exit out]} (run-git dir args)]
    (when (= 0 exit) out)))

;; =============================================================================
;; repository discovery
;; =============================================================================

(defn repo-work-tree
  "Canonical work-tree directory (top level) of the repository containing
   `start`, or nil when `start` is outside any git repo."
  ^File [^File start]
  (when (and start (.exists ^File start))
    (let [dir (if (.isDirectory ^File start) start (.getParentFile ^File start))]
      (when-let [top (git-ok dir ["rev-parse" "--show-toplevel"])]
        (let [top (str/trim top)]
          (when (seq top) (.getCanonicalFile (io/file top))))))))

(defn in-repository?
  "True when `start` is inside a git repository."
  [^File start]
  (boolean (repo-work-tree start)))

(defn vcs-kind
  "The `:vcs/kind` for a workspace `root`: `:git` when the root sits inside a
   git repository, else `:none`. Single source of truth so the model-facing
   CTX, the env, and channels all agree — staying inside the ctx-spec set
   #{:git :hg :jj :fossil :none}. NOTE: `:rift` (the CoW-clone sandbox
   mechanism) is NOT a VCS and must never appear here; sandbox-ness lives on
   `:workspace/sandbox?`."
  [root]
  (if (and root
           (let [f (File. (str root))]
             (and (.exists f) (in-repository? f))))
    :git
    :none))

(defn repo-name
  "Human label for the repository containing `start` — its top-level dir name."
  [^File start]
  (some-> (repo-work-tree start)
          .getName))

;; =============================================================================
;; porcelain v2 status parsing
;; =============================================================================
;; `git status --porcelain=v2 --branch -z` gives, NUL-separated:
;;   # branch.oid <sha|(initial)>
;;   # branch.head <name|(detached)>
;;   # branch.upstream <name>      (only when an upstream is configured)
;;   # branch.ab +<ahead> -<behind> (only when an upstream is configured)
;;   1 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>           changed
;;   2 <XY> <sub> … <path>  + a following token <origPath>  renamed/copied
;;   u <XY> <sub> … <path>                                  unmerged
;;   ? <path>                                               untracked
;;   ! <path>                                               ignored (with --ignored)

(defn porcelain-tokens
  "Structured status of the repo containing `dir`, or nil when `dir` is outside
   a repo / git is unavailable. Options: `:ignored?` adds `!` entries,
   `:untracked-all?` recurses untracked dirs into individual files.

   Returns {:branch <str> :head <sha|nil> :detached? bool :upstream? bool
            :ahead <int> :behind <int> :entries [{:x :y :type :path :dir?}]}."
  [^File dir {:keys [ignored? untracked-all?]}]
  (let [args
        (cond-> ["status" "--porcelain=v2" "--branch" "-z"]
          ignored?
          (conj "--ignored=matching")

          untracked-all?
          (conj "--untracked-files=all"))

        {:keys [exit out]}
        (run-git dir args)]

    (when (= 0 exit)
      (let [toks (vec (remove empty? (str/split (or out "") #"\u0000")))]
        (loop [i 0
               acc {:branch nil
                    :head nil
                    :detached? false
                    :upstream? false
                    :ahead 0
                    :behind 0
                    :entries []}]

          (if (>= i (count toks))
            acc
            (let [t (nth toks i)]
              (cond (str/starts-with? t "# branch.oid ")
                    (recur (inc i)
                           (let [v (str/trim (subs t 13))]
                             (assoc acc :head (when-not (= v "(initial)") v))))
                    (str/starts-with? t "# branch.head ") (recur (inc i)
                                                                 (let [v (str/trim (subs t 14))]
                                                                   (assoc acc
                                                                     :branch v
                                                                     :detached? (= v
                                                                                   "(detached)"))))
                    (str/starts-with? t "# branch.upstream ") (recur (inc i)
                                                                     (assoc acc :upstream? true))
                    (str/starts-with? t "# branch.ab ")
                    (recur (inc i)
                           (let [[a b] (str/split (str/trim (subs t 12)) #" ")]
                             (assoc acc
                               :ahead (parse-long (str (some-> a
                                                               (subs 1))))
                               :behind (parse-long (str (some-> b
                                                                (subs 1)))))))
                    (str/starts-with? t "# ") (recur (inc i) acc)
                    (str/starts-with? t "1 ")
                    (let [xy (subs t 2 4)
                          path (nth (str/split t #" " 9) 8 "")]

                      (recur (inc i)
                             (update acc
                                     :entries
                                     conj
                                     {:x (nth xy 0) :y (nth xy 1) :type :changed :path path})))
                    (str/starts-with? t "2 ")
                    (let [xy (subs t 2 4)
                          path (nth (str/split t #" " 10) 9 "")]

                      ;; type-2 records carry the original path in the NEXT token.
                      (recur (+ i 2)
                             (update acc
                                     :entries
                                     conj
                                     {:x (nth xy 0) :y (nth xy 1) :type :changed :path path})))
                    (str/starts-with? t "u ")
                    (let [xy (subs t 2 4)
                          path (nth (str/split t #" " 11) 10 "")]

                      (recur (inc i)
                             (update acc
                                     :entries
                                     conj
                                     {:x (nth xy 0) :y (nth xy 1) :type :unmerged :path path})))
                    (str/starts-with? t "? ")
                    (recur (inc i) (update acc :entries conj {:type :untracked :path (subs t 2)}))
                    (str/starts-with? t "! ")
                    (let [path (subs t 2)]
                      (recur (inc i)
                             (update acc
                                     :entries
                                     conj
                                     {:type :ignored :path path :dir? (str/ends-with? path "/")})))
                    :else (recur (inc i) acc)))))))))

(defn- branch-label
  "Footer branch label; detached HEADs shorten to `detached:<sha8>`."
  [{:keys [branch detached? head]}]
  (cond detached? (str "detached:" (if head (subs head 0 (min 8 (count head))) "unknown"))
        (str/blank? branch) "unknown"
        :else branch))

;; =============================================================================
;; environment-facing counters (env block + footer)
;; =============================================================================

(defn- count-buckets
  "Split porcelain entries into JGit-equivalent counters. Staged side (X):
   A→added, D→removed, else-modifying→changed. Worktree side (Y): D→missing,
   else-modifying→modified. `?`→untracked, `u`→conflicting."
  [entries]
  (reduce (fn [m {:keys [type x y]}]
            (case type
              :untracked
              (update m :untracked inc)

              :unmerged
              (update m :conflicting inc)

              :ignored
              m

              :changed
              (cond-> m
                (= x \A)
                (update :added inc)

                (= x \D)
                (update :removed inc)

                (and (not= x \.) (not= x \A) (not= x \D))
                (update :changed inc)

                (= y \D)
                (update :missing inc)

                (and (not= y \.) (not= y \D))
                (update :modified inc))

              m))
          {:modified 0 :untracked 0 :added 0 :changed 0 :missing 0 :removed 0 :conflicting 0}
          entries))

(defn status-counts
  "Detailed, environment-facing counters from a parsed porcelain map (as
   returned by the private status reader). Mirrors the historical JGit shape."
  [porcelain]
  (let [{:keys [modified untracked added changed missing removed conflicting]}
        (count-buckets (:entries porcelain))

        changes?
        (boolean (some pos? [modified untracked added changed missing removed conflicting]))]

    {:clean? (not changes?)
     :dirty? changes?
     :changes? changes?
     :modified modified
     :untracked untracked
     :added added
     :changed changed
     :missing missing
     :removed removed
     :conflicting conflicting}))

(defn- dirty-counts
  "User-facing dirty buckets (footer): modified / created / deleted."
  [porcelain]
  (let [{:keys [modified changed added untracked missing removed]} (count-buckets (:entries
                                                                                    porcelain))]
    {:modified (+ modified changed) :created (+ added untracked) :deleted (+ missing removed)}))

;; =============================================================================
;; porcelain-like status entries + snapshot
;; =============================================================================

(defn- entry-code
  "Compact status code for one parsed entry (`M`/`A`/`D`/`??`/`UU`)."
  [{:keys [type x y]}]
  (case type
    :untracked
    "??"

    :unmerged
    "UU"

    :changed
    (cond (or (= x \D) (= y \D)) "D"
          (= x \A) "A"
          :else "M")

    nil))

(defn status-snapshot
  "Read branch, head and porcelain-like entries for `start` via git."
  [^File start]
  (let [dir (if (and start (.isFile ^File start)) (.getParentFile ^File start) start)]
    (when-let [p (porcelain-tokens (or dir (cwd-file)) nil)]
      {:branch (branch-label p)
       :head (:head p)
       :clean? (empty? (:entries p))
       :entries (->> (:entries p)
                     (keep (fn [e]
                             (when-let [c (entry-code e)]
                               {:status c :file (:path e)})))
                     distinct
                     vec)})))

(defn file-dirty?
  "True when `f` is a TRACKED file carrying UNCOMMITTED changes — modified in
   the worktree, staged, deleted/missing, or conflicting. An UNTRACKED
   (brand-new) file is NOT dirty (write is how you create one) and a clean
   tracked file is fine. Repo-less / nil-safe → false."
  [^File f]
  (boolean (when (and f (.exists ^File f))
             (let [cf
                   (.getCanonicalFile ^File f)

                   dir
                   (.getParentFile ^java.io.File cf)

                   {:keys [exit out]}
                   (run-git dir ["status" "--porcelain" "-z" "--" (.getPath cf)])]

               (and (= 0 exit)
                    (some (fn [line]
                            (and (seq line) (not (str/starts-with? line "??"))))
                          (remove empty? (str/split (or out "") #"\u0000"))))))))

;; =============================================================================
;; working-tree status (footer) + cache
;; =============================================================================

(defn working-tree-status
  "Return git facts for `start` (default cwd).

   Outside git, returns `{:workspace? false}` so callers can explicitly render
   the absence of a workspace. Inside git, returns:

     {:workspace? true
      :repo <repo-name>
      :branch <branch-name>
      :modified 2 :created 1 :deleted 0
      :upstream? true :ahead 4 :behind 0}"
  ([] (working-tree-status (cwd-file)))
  ([^File start]
   (if-let [top (repo-work-tree start)]
     (if-let [p (porcelain-tokens top nil)]
       (merge {:workspace? true :repo (.getName top) :branch (branch-label p)}
              (dirty-counts p)
              {:upstream? (:upstream? p) :ahead (:ahead p) :behind (:behind p)})
       {:workspace? false})
     {:workspace? false})))

(defn- refresh-status-async!
  "Recompute the working-tree status for `cwd` (a path string; `start` is the
   File to walk, or nil for the process cwd) OFF the caller's thread and publish
   it into the cache. Deduped via `status-refreshing?` so a burst of repaints
   spawns at most one git walk. Never throws."
  [cwd ^File start ttl-ms]
  (when (compare-and-set! status-refreshing? false true)
    (future (try
              (let [value (if start (working-tree-status start) (working-tree-status))]
                (reset! working-tree-status-cache
                  {:cwd cwd :expires-at (+ (System/currentTimeMillis) (long ttl-ms)) :value value}))
              (catch Throwable t
                (tel/log! :warn
                          ["git: async working-tree status refresh failed"
                           {:cwd cwd
                            :start (some-> start
                                           .getPath)
                            :error (ex-message t)}]))
              (finally (reset! status-refreshing? false))))))

(defn- serve-cached-status
  "Stale-while-revalidate read for the render hot path: return the cached
   status for `cwd` IMMEDIATELY (never blocks on git) and kick off an async
   refresh when the entry is missing, expired, or for another cwd. A repo
   switch shows blank for one refresh cycle rather than stalling the frame —
   git status is advisory chrome, never worth a synchronous ~2ms git walk on
   the render thread (which caused a periodic hitch every time the TTL lapsed)."
  [cwd ^File start now-ms ttl-ms]
  (let [{:keys [expires-at value] cached-cwd :cwd}
        @working-tree-status-cache

        same?
        (= cwd cached-cwd)]

    (when (or (not same?) (>= (long now-ms) (long expires-at)))
      (refresh-status-async! cwd start ttl-ms))
    (when same? value)))

(defn cached-working-tree-status
  "Cached `working-tree-status` for hot render paths.

   Stale-while-revalidate: the TUI footer repaints often, so this NEVER shells
   out to git on the caller's (render) thread. It returns the last resolved
   value instantly and refreshes in the background when the 15s TTL lapses, so
   a footer repaint costs a cache read, not a git walk. Keyed by cwd so every
   UI namespace shares one resolved view."
  ([] (cached-working-tree-status (System/currentTimeMillis) default-cache-ms))
  ([^File start] (cached-working-tree-status start (System/currentTimeMillis) default-cache-ms))
  ([now-ms ttl-ms] (serve-cached-status (.getPath (cwd-file)) nil now-ms ttl-ms))
  ([^File start now-ms ttl-ms]
   (serve-cached-status (.getPath (.getCanonicalFile start)) start now-ms ttl-ms)))
(defn seed-working-tree-status!
  "Synchronously resolve and publish the working-tree status for `start` into
   the shared cache, so the FIRST render after a root switch shows the NEW
   repo instead of a blank/\"No git\" frame.

   `cached-working-tree-status` is stale-while-revalidate: on a cache MISS
   (a cwd it hasn't seen) it returns nil immediately and refreshes in the
   background — correct for the hot repaint path, but it means a `/root`
   switch paints the wrong repo (or \"No git\") for one refresh cycle. A root
   switch is a rare, user-initiated event (not per-frame), so paying one
   synchronous ~2ms git walk HERE — on the dispatch thread, never the render
   thread — is the right trade: the next footer paint is already warm.

   No-op outside a git repo (publishes `{:workspace? false}`). Never throws."
  [^File start]
  (when start
    (try (let [canon
               (.getCanonicalFile start)

               value
               (working-tree-status canon)]

           ;; Honor the in-flight CAS guard: if a refresh is already running for
           ;; this cwd, our synchronous value would race it. We publish unconditionally
           ;; because a root switch is authoritative — our value IS the new truth.
           (reset! working-tree-status-cache {:cwd (.getPath canon)
                                              :expires-at (+ (System/currentTimeMillis)
                                                             (long default-cache-ms))
                                              :value value}))
         (catch Throwable t
           (tel/log! :warn
                     ["git: seed-working-tree-status! failed" (.getPath start) (ex-message t)])))))

;; =============================================================================
;; file-picker helper: per-path status + ignore snapshot
;; =============================================================================

(defn git-status-snapshot
  "One-shot status + ignore snapshot for the repo containing `top` (a
   work-tree File). Returns a map the file picker indexes into:

     {:repo-root <File>
      :path-status {<repo-rel-path> <kw ∈ #{:conflict :deleted :modified
                                            :added :untracked}>}
      :ignored-exact #{<repo-rel-path> …}
      :ignored-prefixes [\"dir/\" …]}"
  [^File top]
  (let [p
        (porcelain-tokens top {:ignored? true :untracked-all? true})

        entries
        (:entries p)

        status-of
        (fn [{:keys [type x y]}]
          (case type
            :unmerged
            :conflict

            :untracked
            :untracked

            :changed
            (cond (or (= x \D) (= y \D)) :deleted
                  (= x \A) :added
                  :else :modified)

            nil))

        path-status
        (reduce (fn [m e]
                  (if-let [s (status-of e)]
                    (assoc m (:path e) s)
                    m))
                {}
                (remove #(= :ignored (:type %)) entries))

        ignored
        (filter #(= :ignored (:type %)) entries)]

    {:repo-root top
     :path-status path-status
     :ignored-exact (into #{} (comp (remove :dir?) (map :path)) ignored)
     :ignored-prefixes (into [] (comp (filter :dir?) (map :path)) ignored)}))
