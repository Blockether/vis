(ns com.blockether.vis.ext.foundation-core.merge-ops
  "Merge-resolve SCI op family (PLAN.md section 7.2).

   Surfaces under the SCI alias `mr/` so the model can drive a merge
   resolution without leaving the sandbox:

     (mr/status)                  read the current merge state
     (mr/accept-ours \"foo.txt\")   keep the trunk side of a conflict
     (mr/accept-theirs \"foo.txt\") keep the branch side
     (mr/mark-resolved \"foo.txt\") stage a manually-edited file
     (mr/continue! {:message ...}) commit the merge (no message -> 'merge-resolve')
     (mr/abort!)                   restore HEAD via `git reset --merge`

   Operations run against `(workspace/cwd)` via JGit (`Git/open`).
   The merge-resolve sub-session pins to the parent workspace whose
   `:root` is the conflicting tree (PLAN.md section 7.1 made trunk
   the merge target).

   All git access goes through JGit — no `sh/sh` shell-outs. Read-only
   `(mr/status)` is safe to call as an introspection probe regardless
   of merge state. Mutators throw structured ex-info when no merge is
   in progress (JGit raises its own checked exceptions on conflicting
   ref state)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.render :as render]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io File]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.api CheckoutCommand$Stage ResetCommand$ResetType]
   [org.eclipse.jgit.lib RepositoryState]))

;; =============================================================================
;; JGit handle
;; =============================================================================

(defn- open-git
  "Open the JGit repository discovered upward from `(workspace/cwd)`.
   Caller owns closing via `with-open` (Git is AutoCloseable)."
  ^Git []
  (Git/open ^File (workspace/cwd)))

(defn- merge-in-progress?*
  [^Git git]
  (let [state (.getRepositoryState (.getRepository git))]
    (boolean (#{RepositoryState/MERGING RepositoryState/MERGING_RESOLVED} state))))

(defn- conflict-paths*
  [^Git git]
  ;; `Status.getConflicting` returns a Set<String> of paths whose index
  ;; entries are in the conflict (UU/AA/DD/...) state.
  (set (.getConflicting (.call (.status git)))))

(defn- conflicts*
  [^Git git]
  (mapv (fn [path] {:path path :state "UU"}) (sort (conflict-paths* git))))

(defn- sha
  "Resolve `rev` to its sha string, or nil when the ref doesn't exist
   (e.g. MERGE_HEAD when not merging)."
  [^Git git rev]
  (some-> (.resolve (.getRepository git) ^String rev) .getName))

;; =============================================================================
;; Public Clojure helpers (each opens / closes one JGit handle)
;; =============================================================================

(defn status
  "Return a plain map describing the active merge.
   `:in-progress?` false when nothing to resolve."
  []
  (with-open [git (open-git)]
    (let [active? (merge-in-progress?* git)]
      (cond-> {:in-progress? active?}
        active? (assoc :head       (sha git "HEAD")
                  :merge-head (sha git "MERGE_HEAD")
                  :branch     (.getBranch (.getRepository git))
                  :conflicts  (conflicts* git))))))

(defn accept-ours
  "Resolve `path` by keeping the trunk side (HEAD) of the conflict.
   JGit `CheckoutCommand.setStage(OURS)` rewrites the working tree
   from the OURS stage of the index; `AddCommand` then advances the
   index so the merge progresses."
  [path]
  (with-open [git (open-git)]
    (.. git checkout (setStage CheckoutCommand$Stage/OURS) (addPath path) call)
    (.. git add (addFilepattern path) call))
  {:path path :op :accept-ours})

(defn accept-theirs
  "Resolve `path` by keeping the branch side (MERGE_HEAD). JGit
   `CheckoutCommand.setStage(THEIRS)` + `AddCommand`."
  [path]
  (with-open [git (open-git)]
    (.. git checkout (setStage CheckoutCommand$Stage/THEIRS) (addPath path) call)
    (.. git add (addFilepattern path) call))
  {:path path :op :accept-theirs})

(defn mark-resolved
  "Stage a path the model already edited by hand (e.g. via `v/patch`).
   Pure `AddCommand`; no checkout."
  [path]
  (with-open [git (open-git)]
    (.. git add (addFilepattern path) call))
  {:path path :op :mark-resolved})

(defn continue!
  "Commit the merge with `:message` (default: 'merge-resolve').
   Refuses when conflicts are still outstanding. Publishes a
   `:session/merge-resolve-finished` event on success."
  [{:keys [message channel-id session-id]}]
  (with-open [git (open-git)]
    (let [outstanding (conflicts* git)]
      (when (seq outstanding)
        (throw (ex-info "merge has unresolved conflicts; resolve them before continue!"
                 {:type :merge-ops/unresolved-conflicts
                  :conflicts outstanding}))))
    (let [msg    (or (some-> message str str/trim not-empty) "merge-resolve")
          commit (.. git commit (setMessage msg) call)
          new-sha (.getName commit)]
      (when channel-id
        (try (vis/publish-channel-event! channel-id
               {:type :session/merge-resolve-finished
                :session-id session-id
                :result :continued
                :head new-sha
                :message msg})
          (catch Throwable _ nil)))
      {:result :continued :head new-sha :message msg})))

(defn abort!
  "Restore HEAD via JGit `ResetCommand` to `ORIG_HEAD` with
   `ResetType/HARD` — the programmatic equivalent of
   `git merge --abort` (which internally resets the worktree + index
   to ORIG_HEAD, then clears MERGE_HEAD / MERGE_MSG). `ResetType/MERGE`
   is fragile in JGit 7.x: it throws UnsupportedOperationException
   when applied outside specific conflict-state preconditions. HARD
   reset to ORIG_HEAD is the documented JGit recipe. Publishes
   `:session/merge-resolve-finished` with `:result :aborted`."
  [{:keys [channel-id session-id]}]
  (with-open [git (open-git)]
    (.. git reset
      (setMode ResetCommand$ResetType/HARD)
      (setRef "ORIG_HEAD")
      call))
  (when channel-id
    (try (vis/publish-channel-event! channel-id
           {:type :session/merge-resolve-finished
            :session-id session-id
            :result :aborted})
      (catch Throwable _ nil)))
  {:result :aborted})

;; =============================================================================
;; SCI-facing tool wrappers (envelope contract)
;; =============================================================================

(defn- ok [v] (extension/success {:result v}))

(defn- ir-status-body [s]
  (if-not (:in-progress? s)
    (render/markdown->ir "No merge in progress.")
    (let [paths   (mapv :path (:conflicts s))
          summary (str "Merging " (:branch s)
                    "\nHEAD = " (subs (or (:head s) "") 0 (min 8 (count (or (:head s) ""))))
                    "\nMERGE_HEAD = " (subs (or (:merge-head s) "") 0 (min 8 (count (or (:merge-head s) ""))))
                    "\nConflicts (" (count paths) "):\n"
                    (if (seq paths)
                      (str/join "\n" (map #(str "  - " %) paths))
                      "  (none - ready for (mr/continue!))"))]
      (render/markdown->ir summary))))

(defn- status-tool [] (ok (status)))
(defn- accept-ours-tool [path] (ok (accept-ours path)))
(defn- accept-theirs-tool [path] (ok (accept-theirs path)))
(defn- mark-resolved-tool [path] (ok (mark-resolved path)))
(defn- continue!-tool
  ([] (continue!-tool {}))
  ([opts] (ok (continue! opts))))
(defn- abort!-tool
  ([] (abort!-tool {}))
  ([opts] (ok (abort! opts))))

(defn- render-status-channel [{:keys [result]}]
  (ir-status-body result))

(defn- render-default-channel [{:keys [result]}]
  (render/markdown->ir (pr-str result)))

(def status-symbol
  (extension/symbol 'status (fn ([] (status-tool)))
    {:doc "Read the active merge-resolve state: {:in-progress? :head :merge-head :branch :conflicts [...]}. Pure JGit read; safe to call regardless of merge state."
     :arglists '([])
     :render-fn render-status-channel}))

(def accept-ours-symbol
  (extension/symbol 'accept-ours (fn ([path] (accept-ours-tool path)))
    {:doc "Resolve a conflict by keeping the trunk side (HEAD). JGit CheckoutCommand(stage=OURS) + AddCommand."
     :arglists '([path])
     :render-fn render-default-channel}))

(def accept-theirs-symbol
  (extension/symbol 'accept-theirs (fn ([path] (accept-theirs-tool path)))
    {:doc "Resolve a conflict by keeping the branch side (MERGE_HEAD). JGit CheckoutCommand(stage=THEIRS) + AddCommand."
     :arglists '([path])
     :render-fn render-default-channel}))

(def mark-resolved-symbol
  (extension/symbol 'mark-resolved (fn ([path] (mark-resolved-tool path)))
    {:doc "Stage a path the model already edited by hand (e.g. via v/patch). JGit AddCommand."
     :arglists '([path])
     :render-fn render-default-channel}))

(def continue!-symbol
  (extension/symbol 'continue! (fn ([] (continue!-tool {})) ([opts] (continue!-tool opts)))
    {:doc "Commit the merge via JGit CommitCommand. Opts: {:message :channel-id :session-id}."
     :arglists '([] [opts])
     :render-fn render-default-channel}))

(def abort!-symbol
  (extension/symbol 'abort! (fn ([] (abort!-tool {})) ([opts] (abort!-tool opts)))
    {:doc "Restore HEAD via JGit ResetCommand(mode=MERGE) — equivalent to `git merge --abort`."
     :arglists '([] [opts])
     :render-fn render-default-channel}))

(def merge-ops-symbols
  [status-symbol accept-ours-symbol accept-theirs-symbol
   mark-resolved-symbol continue!-symbol abort!-symbol])
