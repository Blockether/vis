(ns com.blockether.vis.internal.foundation.workspace-slashes
  "Declarative `/draft …` slash tree.

   Drafts are OPT-IN. By default a session works directly in the user's
   real cwd (trunk). `/draft new <label>` clones cwd into an isolated
   draft (an isolated workspace named `<label>`) and enters it; `/draft apply`
   lands the draft's changes into cwd and leaves the draft; `/draft
   abandon` discards it and leaves. The header shows `<label> (DRAFT)`
   while you're in one.

     /draft                show whether you're on trunk or in a draft
     /draft new <label>    clone cwd into a draft named <label>, enter it
     /draft apply          land the draft's changes into cwd, leave the draft
     /draft abandon [why]  discard the draft, leave it
     /draft blank <label>  like /draft new, but the draft starts EMPTY —
                           no files at all are carried in
     /draft clean <label>  like /draft new, but seeded from the LAST COMMIT —
                           your uncommitted changes stay in cwd

   Filesystem (`/cd`) — session-scoped, every channel. What the jail ALLOWS is
   derived from config (`jail.filesystem` in vis.yml, global or project); `/cd`
   moves the session's PRIMARY LIVE root within that grant:

     /cd [path]            show / CHANGE the session's filesystem root

   Vis owns no git lifecycle — `apply` copies the changed files into the
   user's real cwd, uncommitted. Handlers are PURE w.r.t. the channel."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Helpers
;; =============================================================================
(defn- ctx-session-state-id [ctx] (:session/state-id ctx))

(defn- ctx-db [ctx] (or (:db-info ctx) (:db ctx)))

(defn- session-workspace
  "The workspace (trunk or draft) the current session is in."
  [ctx]
  (let [db (ctx-db ctx)]
    (or (when-let [state-id (ctx-session-state-id ctx)]
          (workspace/for-session db state-id))
        (when-let [wid (:workspace/id ctx)]
          (workspace/get db wid)))))

(defn- err [msg & {:as extras}] (merge {:slash/status :error :slash/title msg} extras))

(defn- sync-confinement!
  "Push the freshly-mutated workspace `ws` into the live sandbox confinement
   pointer (`:workspace-atom`, deref'd by the gateway's `sandbox-roots-fn` on
   every real-fs access) so a `/cd` change takes effect THIS turn — not
   only from the next `run-turn!` workspace re-resolve. No-op when the ctx has no
   atom (other channels build their own ctx). Returns `ws` for threading."
  [ctx ws]
  (when ws
    (some-> (:workspace-atom ctx)
            (reset! ws)))
  ws)

(defn- change-line
  [{:keys [status path]}]
  (str (case status
         :add
         "+ "

         :modify
         "~ "

         :delete
         "- "

         "  ")
       path))

;; =============================================================================
;; Handlers
;; =============================================================================
(defn- handle-create
  "Shared `/draft new` + `/draft blank` + `/draft clean` implementation. `kind`
   is :new (clone cwd exactly as it stands, uncommitted work included), :blank
   (start with NO files at all) or :clean (clone, then rewind to the COMMITTED
   HEAD so uncommitted work stays behind in cwd)."
  [ctx kind]
  (let
    [db
     (ctx-db ctx)

     state-id
     (ctx-session-state-id ctx)

     label
     (some-> (str/join " " (:command/argv ctx))
             str/trim
             not-empty)

     current
     (session-workspace ctx)

     blank?
     (= :blank kind)

     clean?
     (= :clean kind)

     usage
     (case kind
       :blank
       "/draft blank <label>"

       :clean
       "/draft clean <label>"

       "/draft new <label>")]

    (cond
      (nil? state-id) (err (str "Send a message first, then " usage " (session not ready yet)"))
      (workspace/draft? current)
      (err (str "Already in draft '"
                (workspace/display-label current)
                "' — /draft apply, /draft stash, or /draft abandon it first"))
      ;; A draft MUST be named — an unlabeled draft is anonymous and
      ;; indistinguishable in the tab strip / draft list. The TUI prompts for
      ;; the label (see the `:slash/prompt-arg` on this spec); other channels
      ;; get this explicit nudge instead of a silent "draft" default.
      (nil? label) (err (str "Name the draft: " usage))
      (not (workspace/isolated-workspaces-supported? (or (:root current) (workspace/trunk-root))))
      (let [root (or (:root current) (workspace/trunk-root))]
        (err "No workspace backend can create an isolated draft here"
             :slash/body (str "Drafts require isolation, rollback, merge-back, and retained "
                              "revisions. "
                              (workspace/isolation-unavailable-hint root))
             :slash/data {:capability-matrix (workspace/workspace-capability-matrix root)}))
      :else
      (try
        (let
          [draft
           (workspace/create!
             db
             {:session-state-id state-id :label label :from current :blank? blank? :clean? clean?})]
          {:slash/status :ok
           :slash/title (str (case kind
                               :blank
                               "Blank draft '"

                               :clean
                               "Clean draft '"

                               "Draft '")
                             (workspace/display-label draft)
                             "' — you're in it now")
           :slash/body
           (case kind
             :blank
             "Started EMPTY — nothing from your repo was carried in. /draft apply lands created files into your repo · /draft abandon discards."

             :clean
             "Started from your last commit — your uncommitted changes stayed in your repo, untouched. /draft apply lands this draft's changes into your repo · /draft abandon discards."

             "Edits here stay isolated. /draft apply lands them into your repo · /draft abandon discards.")
           :slash/data
           {:workspace-id (:id draft) :label (:label draft) :blank? blank? :clean? clean?}})
        ;; The only expected failure is "no commit to rewind to" — everything
        ;; else is a real fault and must keep its own error path.
        (catch clojure.lang.ExceptionInfo e
          (if (= :workspace/clean-seed-unavailable (:type (ex-data e)))
            (err "This project has no commit yet — there is nothing to start a clean draft from"
                 :slash/body
                 "Make a first commit, or use /draft new to carry your working tree in as it is.")
            (throw e)))))))

(defn- handle-new
  "`/draft new <label>` — clone cwd into a draft named <label> and enter it."
  [ctx]
  (handle-create ctx :new))

(defn- handle-new-blank
  "`/draft blank <label>` — like /draft new, but the draft starts EMPTY: no
   files at all are carried into it."
  [ctx]
  (handle-create ctx :blank))

(defn- handle-new-clean
  "`/draft clean <label>` — like /draft new, but the draft is seeded from the
   COMMITTED HEAD: every committed file is there, and the uncommitted work in
   cwd stays behind in cwd."
  [ctx]
  (handle-create ctx :clean))

(defn- handle-apply
  "`/draft apply` — land the draft's changes into cwd, then leave the draft."
  [ctx]
  (let
    [db
     (ctx-db ctx)

     state-id
     (ctx-session-state-id ctx)

     current
     (session-workspace ctx)]

    (cond (nil? current) (err "No active workspace")
          (not (workspace/draft? current)) (err "Not in a draft — /draft new <label> to start one")
          :else (let
                  [{:keys [landed changed]}
                   (workspace/apply! db {:workspace-id (:id current)})

                   label
                   (workspace/display-label current)]

                  (workspace/abandon! db {:workspace-id (:id current) :reason "applied"})
                  (when state-id (workspace/exit-to-trunk! db state-id))
                  {:slash/status :ok
                   :slash/title (str "Applied "
                                     landed
                                     " file"
                                     (when (not= 1 landed) "s")
                                     " — left draft '"
                                     label
                                     "', back in your repo")
                   :slash/body (->> changed
                                    (map change-line)
                                    (str/join "\n"))
                   :slash/data {:landed landed :changed changed}}))))

(defn- handle-abandon
  "`/draft abandon [reason]` — discard the draft and leave it."
  [ctx]
  (let
    [db
     (ctx-db ctx)

     state-id
     (ctx-session-state-id ctx)

     current
     (session-workspace ctx)

     reason
     (some-> (str/join " " (:command/argv ctx))
             str/trim
             not-empty)]

    (cond (nil? current) (err "No active workspace")
          (not (workspace/draft? current)) (err "Not in a draft")
          :else (let [label (workspace/display-label current)]
                  (workspace/abandon! db {:workspace-id (:id current) :reason reason})
                  (when state-id (workspace/exit-to-trunk! db state-id))
                  {:slash/status :ok
                   :slash/title (str "Abandoned draft '" label "' — back in your repo")
                   :slash/body (when reason (str "Reason: " reason))
                   :slash/data {:workspace-id (:id current) :reason reason}}))))

(defn- handle-status
  "Bare `/draft` — are you on trunk or in a draft?"
  [ctx]
  (let
    [db
     (ctx-db ctx)

     current
     (session-workspace ctx)]

    (cond
      (workspace/draft? current)
      (let [st (workspace/status db (:id current))]
        {:slash/status :ok
         :slash/title (str "Draft '" (workspace/display-label current) "'")
         :slash/body
         (str (:workspace/changed st 0)
              " file(s) changed · "
              "/draft apply to land them, /draft stash to park it, /draft abandon to discard")
         :slash/data {:workspace-id (:id current)}})
      :else
      {:slash/status :ok
       :slash/title "On trunk — your real repo"
       :slash/body
       "Editing your repo directly. /draft new <label> to start an isolated draft, /draft list + /draft resume <label> to re-enter a stashed one."})))

(defn- handle-stash
  "`/draft stash` — leave the draft WITHOUT discarding it, so `/draft resume`
   can re-enter it later. The non-destructive twin of /draft abandon."
  [ctx]
  (let
    [db
     (ctx-db ctx)

     state-id
     (ctx-session-state-id ctx)

     current
     (session-workspace ctx)]

    (cond (nil? current) (err "No active workspace")
          (not (workspace/draft? current)) (err "Not in a draft — nothing to stash")
          :else (let [label (workspace/display-label current)]
                  (workspace/stash! db state-id)
                  {:slash/status :ok
                   :slash/title (str "Stashed draft '" label "' — back on trunk")
                   :slash/body (str "Parked, not discarded. /draft resume "
                                    label
                                    " to re-enter it · /draft list to see every stashed draft.")
                   :slash/data {:workspace-id (:id current) :label label}}))))

(defn- handle-list
  "`/draft list` — every active/stashed draft in this repo, newest first, with
   the current one marked. The gateway keeps stashed drafts alive until they are
   applied or abandoned, so this is how you find one to /draft resume."
  [ctx]
  (let
    [db
     (ctx-db ctx)

     current
     (session-workspace ctx)

     repo-id
     (:repo-id current)

     drafts
     (when repo-id (workspace/list-drafts db repo-id))

     current-id
     (when (workspace/draft? current) (:id current))]

    (cond (nil? current) (err "No active workspace")
          (empty? drafts) {:slash/status :ok
                           :slash/title "No drafts yet"
                           :slash/body "Editing trunk directly. /draft new <label> to start one."}
          :else {:slash/status :ok
                 :slash/title (str (count drafts) " draft" (when (not= 1 (count drafts)) "s"))
                 :slash/body (->> drafts
                                  (map (fn [d]
                                         (str (if (= current-id (:id d)) "* " "  ")
                                              (workspace/display-label d)
                                              (when (= current-id (:id d)) " (current)"))))
                                  (str/join "\n"))
                 :slash/data {:drafts (mapv (fn [d]
                                              {:workspace-id (:id d)
                                               :label (workspace/display-label d)
                                               :current? (= current-id (:id d))})
                                            drafts)}})))

(defn- handle-resume
  "`/draft resume [label]` — re-enter a stashed draft by label. With no label,
   lists the stashed drafts to choose from. Refuses while already in a draft."
  [ctx]
  (let
    [db
     (ctx-db ctx)

     state-id
     (ctx-session-state-id ctx)

     current
     (session-workspace ctx)

     label
     (some-> (str/join " " (:command/argv ctx))
             str/trim
             not-empty)

     repo-id
     (:repo-id current)

     drafts
     (when repo-id (workspace/list-drafts db repo-id))]

    (cond
      (nil? state-id) (err "Send a message first, then /draft resume <label>")
      (workspace/draft? current)
      (err (str "Already in draft '"
                (workspace/display-label current)
                "' — /draft stash, /draft apply, or /draft abandon it first"))
      (empty? drafts)
      (err "No stashed drafts to resume" :slash/body "/draft new <label> to start one.")
      (nil? label) {:slash/status :ok
                    :slash/title "Which draft? — /draft resume <label>"
                    :slash/body (->> drafts
                                     (map #(str "  " (workspace/display-label %)))
                                     (str/join "\n"))
                    :slash/data {:drafts (mapv #(workspace/display-label %) drafts)}}
      :else
      (let [match (filter #(= label (workspace/display-label %)) drafts)]
        (cond
          (empty? match) (err (str "No stashed draft named '" label "'")
                              :slash/body
                              (str "Stashed: "
                                   (str/join ", " (map workspace/display-label drafts))))
          (next match) (err
                         (str "Multiple drafts named '" label "' — abandon the duplicates first"))
          :else
          (let [d (first match)]
            (workspace/resume! db {:session-state-id state-id :workspace-id (:id d)})
            {:slash/status :ok
             :slash/title (str "Resumed draft '" (workspace/display-label d) "'")
             :slash/body
             "Back in your draft. /draft apply to land it · /draft stash to park it again · /draft abandon to discard."
             :slash/data {:workspace-id (:id d) :label (workspace/display-label d)}}))))))

(defn- argv-path
  "The handler's whole argv as one `~`-expanded path string, or nil."
  [ctx]
  (some-> (str/join " " (:command/argv ctx))
          str/trim
          not-empty
          paths/expand-home))

(defn- handle-fs-root
  "`/cd <path>` — change the session's primary workspace root. The session then
   works in <path>: shell cwd, relative paths, file tools, and search all follow.
   Bare (no path) shows the current root."
  [ctx]
  (let
    [db
     (ctx-db ctx)

     state-id
     (ctx-session-state-id ctx)

     current
     (session-workspace ctx)

     path
     (argv-path ctx)]

    (cond (nil? path) {:slash/status :ok
                       :slash/title (str "Root: " (or (:root current) "(none)"))
                       :slash/body "/cd <path> to work in a different directory."
                       :slash/data {:root (:root current)}}
          (nil? state-id) (err "Send a message first, then /cd <path> (session not ready yet)")
          :else (try (let [ws (sync-confinement! ctx (workspace/change-root! db state-id path))]
                       {:slash/status :ok
                        :slash/title (str "Root changed — now working in " (:root ws))
                        :slash/body "Shell, file tools, and search operate here from the next turn."
                        :slash/data {:root (:root ws) :workspace-id (:id ws)}})
                     (catch Exception e
                       (err (str "Can't change root to '" path
                                 "': " (or (ex-message e) (str e)))))))))

;; =============================================================================
;; Specs vec
;; =============================================================================
(defn- build-specs
  "Slash specs vec. Commands are always discoverable; handlers report runtime
   capability availability for the active workspace."
  []
  (into
    [{:slash/name "draft"
      :slash/doc "Drafts — isolated workspace copies of your repo (opt-in)."
      :slash/usage
      "/draft <new <label> | clean <label> | blank <label> | apply | stash | resume <label> | list | abandon>"
      :slash/ui {:kind :navigator}
      :slash/run-fn handle-status}
     {:slash/name "new"
      :slash/parent ["draft"]
      :slash/doc
      "Clone cwd into an isolated draft named <label> and enter it — uncommitted changes included."
      :slash/usage "/draft new <label>"
      :slash/prompt-arg "Draft label (e.g. feature-x)"
      :slash/requires #{:session}
      :slash/run-fn handle-new}
     {:slash/name "apply"
      :slash/parent ["draft"]
      :slash/doc "Land the draft's changes into your repo and leave the draft."
      :slash/usage "/draft apply"
      :slash/requires #{:session}
      :slash/run-fn handle-apply}
     {:slash/name "abandon"
      :slash/parent ["draft"]
      :slash/doc "Discard the draft and leave it."
      :slash/usage "/draft abandon [reason]"
      :slash/requires #{:session}
      :slash/run-fn handle-abandon}
     {:slash/name "stash"
      :slash/parent ["draft"]
      :slash/doc "Park the draft without discarding it — resume it later."
      :slash/usage "/draft stash"
      :slash/requires #{:session}
      :slash/run-fn handle-stash}
     {:slash/name "resume"
      :slash/parent ["draft"]
      :slash/doc "Re-enter a stashed draft by <label>."
      :slash/usage "/draft resume <label>"
      :slash/prompt-arg "Draft label to resume"
      :slash/requires #{:session}
      :slash/run-fn handle-resume}
     {:slash/name "list"
      :slash/parent ["draft"]
      :slash/doc "List every stashed/active draft in this repo."
      :slash/usage "/draft list"
      :slash/run-fn handle-list}
     {:slash/name "blank"
      :slash/parent ["draft"]
      :slash/doc
      "Like /draft new, but the draft starts with NO files at all — not even what is committed."
      :slash/usage "/draft blank <label>"
      :slash/prompt-arg "Draft label (e.g. feature-x)"
      :slash/requires #{:session}
      :slash/run-fn handle-new-blank}
     {:slash/name "clean"
      :slash/parent ["draft"]
      :slash/doc
      "Like /draft new, but seeded from your last commit — uncommitted changes stay in your repo."
      :slash/usage "/draft clean <label>"
      :slash/prompt-arg "Draft label (e.g. feature-x)"
      :slash/requires #{:session}
      :slash/run-fn handle-new-clean}]
    ;; Filesystem — session-scoped, every channel. What the jail ALLOWS is
    ;; derived from config (`jail.filesystem`); `/cd` moves the session's
    ;; PRIMARY LIVE root within that grant.
    [{:slash/name "cd"
      :slash/doc "Show or change the session's filesystem root (the directory vis works in)."
      :slash/usage "/cd [path]"
      :slash/run-fn handle-fs-root}]))

(def specs
  "Declarative slash specs vec hooked onto foundation-core's manifest\n   via `:ext/slash-commands`. Capability checks happen when commands run."
  (build-specs))
