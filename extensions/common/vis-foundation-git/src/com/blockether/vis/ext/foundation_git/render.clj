(ns com.blockether.vis.ext.foundation-git.render
  "Op-card renderers for the whole `git/` surface — one `{:summary :body}`
   projection per tool so a git op shows as a clean card in BOTH the TUI and
   the web (unified), surfacing only what matters instead of the raw args +
   result dump.

   CONTRACT (mirrors editing/core.clj): a renderer receives the tool's
   UNWRAPPED result value AFTER it has round-tripped the GraalPy boundary — so
   keys are KEYWORDS in snake_case with a trailing `?`/`!` stripped and every
   kebab `-` folded to `_` (`:short-sha` → `:short_sha`, `:amend?` → `:amend`,
   `:up-to-date?` → `:up_to_date`). Write renderers against THAT shape."
  (:require
   [clojure.string :as str]))

;; ----------------------------------------------------------------------------
;; Shared helpers
;; ----------------------------------------------------------------------------

(defn- s
  "Round-tripped scalar back to a plain string (`nil` stays `nil`)."
  [x]
  (cond
    (nil? x)     nil
    (keyword? x) (name x)
    :else        (str x)))

(defn- code
  "Backtick-wrap a value for the card headline."
  [x]
  (str "`" (s x) "`"))

(defn- first-line
  "First non-blank line of a (possibly multi-line) message."
  [msg]
  (some-> msg s str/split-lines first str/trim not-empty))

(defn- plural
  "`N word`, pluralised when `n` != 1 (`-es` for ch/sh/s/x stems)."
  [n word]
  (let [suffix (if (some #(str/ends-with? word %) ["ch" "sh" "s" "x"]) "es" "s")]
    (str n " " word (when (not= 1 (long n)) suffix))))

(defn- short7
  "Abbreviate a full sha to git's 7-char default."
  [sha]
  (when-let [x (s sha)] (subs x 0 (min 7 (count x)))))

(defn- block
  "Fenced code block from a seq of already-formatted lines, or nil when empty."
  [lines]
  (let [ls (remove nil? lines)]
    (when (seq ls) (str "```\n" (str/join "\n" ls) "\n```"))))

;; ----------------------------------------------------------------------------
;; Write ops
;; ----------------------------------------------------------------------------

(defn render-add
  "git_add → `staged N paths` + the staged pathspecs."
  [{:keys [paths]}]
  (let [n (count paths)]
    {:summary (if (= 1 n)
                (str "staged " (code (first paths)))
                (str "staged " (plural n "path")))
     :body    (block (map #(str "  " (s %)) paths))}))

(defn render-commit
  "git_commit / git_amend → `commit `sha` · <subject>`; full message as body
   when it spans multiple lines."
  [{:keys [short_sha message amend]}]
  (let [ls (some-> message s str/split-lines)]
    {:summary (str (if amend "amended " "commit ") (code short_sha)
                (when-let [f (first-line message)] (str " · " f)))
     :body    (when (> (count ls) 1) (block (map #(str "  " %) ls)))}))

(defn render-push
  "git_push → `pushed `branch` → remote · STATUS…` + per-ref update rows."
  [{:keys [remote branch force delete updates]}]
  {:summary (str (if delete "deleted " "pushed ") (code branch) " → " (s remote)
              (when force " (force)")
              (when (seq updates)
                (str " · " (str/join ", " (distinct (map #(s (:status %)) updates))))))
   :body    (block (for [u updates]
                     (str "  " (s (:status u)) "  " (s (:remote_name u))
                       (when-let [m (some-> (:message u) s not-empty)] (str " — " m)))))})

(defn render-fetch
  "git_fetch → `fetched remote · STATUS · N updates` + per-ref ranges."
  [{:keys [remote status summary updates]}]
  (let [n (:updated summary)]
    {:summary (str "fetched " (s remote) " · " (s status)
                (when (and n (pos? (long n))) (str " · " (plural n "update"))))
     :body    (block (for [u updates]
                       (str "  " (s (:ref u))
                         (when-let [r (some-> (:range u) s not-empty)] (str "  " r))
                         (when-let [k (some-> (:kind u) s not-empty)] (str " (" k ")")))))}))

(defn render-reset
  "git_reset → `reset --mode → `sha`` (or per-path reset) + head before→after."
  [{:keys [mode to short_sha head_before head_after paths]}]
  (if (seq paths)
    {:summary (str "reset " (plural (count paths) "path") " to " (code to))
     :body    (block (map #(str "  " (s %)) paths))}
    {:summary (str "reset" (when mode (str " --" (s mode))) " → " (code short_sha)
                (when (and to (not= (s to) (s short_sha))) (str " (" (s to) ")")))
     :body    (when (and head_before head_after (not= head_before head_after))
                (block [(str "  " (short7 head_before) " → " (short7 head_after))]))}))

(defn render-branch
  "git_branch → op-specific card: created / deleted / renamed / listed."
  [{:keys [name from short_sha force deleted old new mode branches] :as r}]
  (cond
    (contains? r :branches)
    {:summary (str (plural (count branches) "branch") (when mode (str " (" (s mode) ")")))
     :body    (block (for [b branches]
                       (str "  " (s (:short b))
                         (when (:short_sha b) (str "  " (s (:short_sha b)))))))}

    (contains? r :deleted)
    {:summary (str "deleted " (plural (count deleted) "branch") (when force " (force)"))
     :body    (block (map #(str "  " (s %)) deleted))}

    (and old new)
    {:summary (str "renamed branch " (code old) " → " (code new))}

    :else
    {:summary (str "created branch " (code name)
                (when short_sha (str " @ " (s short_sha)))
                (when (and from (not= "HEAD" (s from))) (str " from " (code from)))
                (when force " (force)"))}))

(defn render-checkout
  "git_checkout → restored-paths / branch-switch / detached-HEAD card."
  [{:keys [paths files_restored start_point branch sha short_head created detached] :as r}]
  (cond
    (contains? r :files_restored)
    {:summary (str "restored " (plural files_restored "file")
                (when start_point (str " from " (code start_point))))
     :body    (block (map #(str "  " (s %)) paths))}

    branch
    {:summary (str (if created "created + checked out " "checked out ") (code branch)
                (when short_head (str " @ " (s short_head))))}

    :else
    {:summary (str (if detached "detached HEAD at " "checked out ")
                (code (or short_head sha)))}))

(defn render-cherry-pick
  "git_cherry_pick → `cherry-pick STATUS · N commits` + picked / conflicts."
  [{:keys [status picked failing_paths]}]
  {:summary (str "cherry-pick " (s status) " · " (plural (count picked) "commit")
              (when (seq failing_paths) (str " · " (plural (count failing_paths) "conflict"))))
   :body    (block (concat (for [p picked] (str "  picked " (s (:short_sha p))))
                     (for [fp failing_paths] (str "  conflict " (s fp)))))})

(defn render-rebase
  "git_rebase → `rebase STATUS @ sha` + conflicts / failing paths / hint."
  [{:keys [status conflicts failing_paths short_current hint]}]
  {:summary (str "rebase " (s status) (when short_current (str " @ " (s short_current))))
   :body    (block (concat (for [c conflicts] (str "  conflict " (s c)))
                     (for [fp failing_paths] (str "  failed " (s fp)))
                     (when hint [(str "  → " (s hint))])))})

;; ----------------------------------------------------------------------------
;; Observation ops
;; ----------------------------------------------------------------------------

(def ^:private status-buckets
  "Ordered [bucket-key marker] pairs for the status card body."
  [[:added "A"] [:modified "M"] [:deleted "D"] [:untracked "?"] [:conflicted "U"]])

(defn render-status
  "git_status → `on `branch` @ head · N changes` (or `clean`) + marked files."
  [{:keys [branch head changes]}]
  (let [total (reduce + 0 (map (fn [[k _]] (count (get changes k))) status-buckets))]
    {:summary (str "on " (code branch) (when head (str " @ " (s head)))
                " · " (if (zero? total) "clean" (plural total "change")))
     :body    (block (mapcat (fn [[k mark]]
                               (for [f (get changes k)] (str "  " mark "  " (s f))))
                       status-buckets))}))

(defn- diff-files-block
  "Body for a diff/show file vector: a `+A -D  path` header per file so a
   MULTI-file diff reads as distinct sections, each followed by its unified
   patch when `is_patch` supplied one (else the numstat header stands alone)."
  [files]
  (block
    (mapcat (fn [f]
              (cons (str "  +" (or (:add f) 0) " -" (or (:del f) 0) "  " (s (:file f)))
                (some-> (:patch f) s not-empty str/split-lines)))
      files)))

(defn render-diff
  "git_diff → `diff `from`…`to` · N files +A -D` + per-file numstat / patch,
   with any working-tree untracked files listed after the tracked diff."
  [{:keys [from to stat files untracked]}]
  (let [fc (or (:files stat) (count files))]
    {:summary (str "diff " (code from) (when to (str "…" (code to)))
                " · " (plural fc "file") " +" (or (:add stat) 0) " -" (or (:del stat) 0)
                (when (seq untracked) (str " · " (count untracked) " untracked")))
     :body    (not-empty
                (str/join "\n"
                  (remove nil?
                    [(diff-files-block files)
                     (when (seq untracked)
                       (block (for [u untracked] (str "  ?  " (s u)))))])))}))

(defn render-log
  "git_log → `N commits on `branch`` + `sha subject` rows."
  [{:keys [branch commits]}]
  {:summary (str (plural (count commits) "commit") (when branch (str " on " (code branch))))
   :body    (block (for [c commits]
                     (str "  " (s (:short_sha c)) "  " (or (first-line (:subject c)) ""))))})

(defn render-show
  "git_show → `commit `sha` · subject` + author line and file numstat/patch."
  [{:keys [short_sha subject author files]}]
  {:summary (str "commit " (code short_sha) (when subject (str " · " (first-line subject))))
   :body    (let [num (diff-files-block files)]
              (block (remove nil?
                       (concat (when author [(str "  " (s author))])
                         (when num [num])))))})

(defn render-blame
  "git_blame → `blame `path` · N lines` (body omitted — the legend is large)."
  [{:keys [path total lines]}]
  {:summary (str "blame " (code path) " · " (plural (or total (count lines)) "line"))})

;; ----------------------------------------------------------------------------
;; Merge-resolve ops
;; ----------------------------------------------------------------------------

(defn- conflict-str
  "One conflict entry as a path string (handles both `path` and `{:path :state}`)."
  [c]
  (if (map? c)
    (str (s (:path c)) (when-let [st (some-> (:state c) s not-empty)] (str " (" st ")")))
    (s c)))

(defn render-merge-status
  "git_merge_status → in-progress card with conflict list, or a no-op note."
  [{:keys [in_progress branch conflicts]}]
  (if in_progress
    {:summary (str "merge in progress" (when branch (str " on " (code branch)))
                (when (seq conflicts) (str " · " (plural (count conflicts) "conflict"))))
     :body    (block (map #(str "  " (conflict-str %)) conflicts))}
    {:summary "no merge in progress"}))

(defn render-merge-accept-ours
  "git_merge_accept_ours → `kept ours · `path``."
  [{:keys [path]}]
  {:summary (str "kept ours · " (code path))})

(defn render-merge-accept-theirs
  "git_merge_accept_theirs → `kept theirs · `path``."
  [{:keys [path]}]
  {:summary (str "kept theirs · " (code path))})

(defn render-merge-mark-resolved
  "git_merge_mark_resolved → `marked resolved · `path``."
  [{:keys [path]}]
  {:summary (str "marked resolved · " (code path))})

(defn render-merge-continue
  "git_merge_continue → `merge committed `sha` · subject`."
  [{:keys [head message]}]
  {:summary (str "merge committed"
              (when head (str " " (code (short7 head))))
              (when-let [f (first-line message)] (str " · " f)))})

(defn render-merge-abort
  "git_merge_abort → `merge aborted`."
  [_]
  {:summary "merge aborted"})

(defn render-merge
  "git_merge → `merge STATUS @ sha` + conflicts / failing paths / hint."
  [{:keys [status conflicts failing_paths hint head]}]
  {:summary (str "merge " (s status) (when head (str " @ " (short7 head))))
   :body    (block (concat (for [c conflicts] (str "  conflict " (conflict-str c)))
                     (for [[p st] failing_paths] (str "  failed " (s p) " (" (s st) ")"))
                     (when hint [(str "  → " (s hint))])))})
