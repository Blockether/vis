(ns com.blockether.vis.internal.workspace.git
  "Shared workspace inspection, backed by the native `git` binary.

   UI surfaces and extensions depend on this namespace instead of embedding
   their own git calls. It reports small, renderable facts about the current
   repository (branch, dirty buckets, ahead/behind, porcelain entries) by
   shelling out to `git status --porcelain=v2` and `git rev-parse`, parsed
   into stable Clojure maps. No JGit — the only git implementation is the one
   already on the user's PATH, so behaviour matches their shell exactly.

   Workspace lifecycle mutations stay in
   `com.blockether.vis.internal.workspace.core`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.util.concurrent TimeUnit]))

;; Kept in lockstep with the TUI workspace poll (screen.clj `workspace-refresh-ms`,
;; 4s): a shorter TTL is pointless (the poll can't surface a change faster than it
;; ticks) and a longer one is the whole reason the footer used to lag the repo state by
;; ~15s. Stale-while-revalidate, so a poll on a stale entry returns instantly and
;; the refreshed value lands on the next tick — worst-case footer latency ~4-8s.
(def ^:private default-cache-ms 4000)

(def ^:private default-git-timeout-secs 10)

(defonce ^:private working-tree-status-cache (atom {}))

(defn cwd-file
  "Canonical current working directory as a File. Indirected for tests."
  ^File []
  (.getCanonicalFile (workspace/cwd)))

;; git subprocess primitive

(defn- git-argv
  "Argv for `git <args>`. EVERY Vis invocation is prefixed with
   `--no-optional-locks`. The footer poll, the rewind baseline, the environment
   snapshot and the git verbs all read status far more often than the user types, and a
   plain `git status` REFRESHES the index — it takes `.git/index.lock` to write
   the refreshed stat cache back. That lock is OPTIONAL for a reader and
   REQUIRED for the user's own `add`/`commit`/`checkout`, so polling without
   this flag makes their git fail with `Unable to create '.git/index.lock':
   File exists`. Locks a command genuinely needs are unaffected."
  [args]
  (into ["git" "--no-optional-locks"] (map str) args))

(defn- terminate!
  "End `p` without stranding a lock. SIGTERM first (`.destroy`) — git's lockfile
   handler unlinks `.git/index.lock` on the way out — and escalate to SIGKILL
   (`.destroyForcibly`) only for a child that ignores it. A bare
   `destroyForcibly` on a git that happens to be rewriting the index leaves the
   lock file behind for good, and every later git command in that repository
   dies on it."
  [^Process p]
  (when (and p (.isAlive p))
    (.destroy p)
    (try (when-not (.waitFor p 2 TimeUnit/SECONDS) (.destroyForcibly p))
         (catch Throwable t
           ;; Interrupted while waiting: the TERM is already delivered, so let git
           ;; finish its own cleanup rather than SIGKILL it mid index write.
           (cancellation/preserve-interrupt! t)))))

(defn run-git
  "Run `git <args>` in `dir` (a File). `args` is a seq of stringable tokens.
   Returns `{:exit <int|nil> :out <stdout> :err <stderr> :duration-ms <long>}`;
   `:exit` is nil when the process could not be spawned, timed out, or the
   calling thread was interrupted. Never throws — repo-less and git-less
   environments degrade to `{:exit nil …}`.
   The 3-arity opts map takes `:timeout-secs` (default 10).

   The child is ALWAYS reaped. A cancelled turn re-arms the thread's interrupt
   flag, so `.waitFor` throws INSTANTLY on this and every following call: an
   abandoned `git status` would keep running, holding `.git/index.lock`, while
   the caller spawns the next one."
  ([^File dir args] (run-git dir args nil))
  ([^File dir args {:keys [timeout-secs]}]
   (let [t0
         (util/now-ms)

         spawned
         (volatile! nil)]

     (try (let [pb (ProcessBuilder. ^java.util.List (git-argv args))]
            (.directory pb (or dir (cwd-file)))
            (let [p (.start pb)
                  _ (vreset! spawned p)
                  out (future (slurp (io/reader (.getInputStream p))))
                  err (future (slurp (io/reader (.getErrorStream p))))
                  done
                  (.waitFor p (long (or timeout-secs default-git-timeout-secs)) TimeUnit/SECONDS)]

              (when-not done
                (terminate! p)
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
               :duration-ms (- (util/now-ms) t0)}))
          (catch Throwable t
            (cancellation/preserve-interrupt! t)
            (terminate! @spawned)
            (tel/log! :warn
                      ["git: run-git failed"
                       {:dir (some-> dir
                                     .getPath)
                        :args (vec (map str args))
                        :error (ex-message t)}])
            {:exit nil :out "" :err "" :timed-out? false :duration-ms (- (util/now-ms) t0)})))))

(defn- git-ok
  "stdout of `git <args>` when it exits 0, else nil."
  [^File dir args]
  (let [{:keys [exit out]} (run-git dir args)]
    (when (= 0 exit) out)))

;; repository discovery

(defn- git-marker
  "The nearest `.git` entry at `dir` or above it, or nil. Git finds a repository
   from this entry, so its path changes when a repository appears or goes away."
  ^File [^File dir]
  (when-let [^File repo (paths/nearest-ancestor #(.exists (File. ^File % ".git"))
                                                (.getAbsoluteFile dir))]
    (File. repo ".git")))

(defn- git-env-discovery?
  "True when the process environment points git at a repository explicitly."
  []
  (boolean (or (System/getenv "GIT_DIR") (System/getenv "GIT_WORK_TREE"))))

(defonce ^:private work-trees
  ;; [dir marker] -> work tree that git reported. Perf pass 2026-11: every context
  ;; render ran `git rev-parse --show-toplevel` twice for the same root.
  (atom {}))

(defn repo-work-tree
  "Canonical work-tree directory (top level) of the repository containing
   `start`, or nil when `start` is outside any git repo. Without a `.git` entry
   above `start`, the answer is nil without a git run. A found work tree is
   reused while the nearest `.git` entry stays the same."
  ^File [^File start]
  (when (and start (.exists ^File start))
    (let [dir
          (if (.isDirectory ^File start) start (.getParentFile ^File start))

          marker
          (git-marker dir)

          env?
          (git-env-discovery?)

          k
          [(.getAbsolutePath ^File dir)
           (some-> marker
                   .getPath)]]

      (when (or marker env?)
        (or (when-not env? (get @work-trees k))
            (when-let [top (git-ok dir ["rev-parse" "--show-toplevel"])]
              (let [top (str/trim top)]
                (when (seq top)
                  (let [tree (.getCanonicalFile (io/file top))]
                    (when-not env? (swap! work-trees #(assoc (if (< (count %) 256) % {}) k tree)))
                    tree)))))))))

(defn in-repository?
  "True when `start` is inside a git repository."
  [^File start]
  (boolean (repo-work-tree start)))

(defn vcs-kind
  "The `:vcs/kind` for a workspace `root`: `:git` when the root sits inside a
   git repository, else `:none`. Single source of truth so the model-facing
   CTX, the env, and channels all agree — staying inside the ctx-spec set
   #{:git :hg :jj :fossil :none}."
  [root]
  (if (and root
           (let [f (File. (str root))]
             (and (.exists f) (in-repository? f))))
    :git
    :none))

;; porcelain v2 status parsing
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

;; environment-facing counters (env block + footer)

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
    {:modified (+ (long modified) (long changed))
     :created (+ (long added) (long untracked))
     :deleted (+ (long missing) (long removed))}))

;; working-tree status (footer) + cache

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

(defn- fresh-status
  "Working-tree status of `start` for the cache key `cwd`. Logs a failure and throws it
   again, so the cache keeps its last value."
  [cwd ^File start]
  (try (working-tree-status start)
       (catch Throwable t
         (tel/log! :warn
                   ["git: working-tree status failed"
                    {:cwd cwd :start (.getPath start) :error (ex-message t)}])
         (throw t))))

(defn workspace-status
  "Working-tree status of the repository at `root`, as a gateway session fact.
   Each canonical root keeps its value, so tab switches, turn ends and footer syncs do
   not wait for git. The first call for a root runs git on this thread, so a real root
   never gives nil. After `default-cache-ms`, a call returns the kept value at once and
   refreshes it on a future. Never throws: nil for a nil or blank `root`, or after a
   logged failure."
  [root]
  (when-let [start (some-> root
                           str
                           str/trim
                           not-empty
                           io/file)]
    (try (let [cwd (.getPath (.getCanonicalFile start))]
           (util/refreshed working-tree-status-cache
                           default-cache-ms
                           cwd
                           #(fresh-status cwd start)
                           true))
         (catch Throwable t
           (tel/log! :warn ["git: workspace-status failed" (str root) (ex-message t)])
           nil))))

;; file-picker helper: per-path status + ignore snapshot
