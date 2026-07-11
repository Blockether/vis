(ns com.blockether.vis.ext.channel-tui.magit
  "Magit-style PORCELAIN layer for the TUI — the pure, testable half of the
   C-x g status buffer. Everything runs over the native `git` CLI via
   `internal.git/run-git`; nothing here touches a terminal.

   Three surfaces:

   - `status-model`   — one snapshot of the repo: head/upstream facts plus the
                        magit sections (untracked / unstaged / staged /
                        unmerged / stashes / unpushed / unpulled / recent
                        commits).
   - actions          — `stage-file!`, `unstage-file!`, `discard-file!`,
                        `commit!`, `push!`, `pull!`, `fetch!`, branch and
                        stash verbs. Each returns `{:ok? bool :msg str}` so
                        the dialog can echo success/failure verbatim.
   - `status-rows`    — PURE projection of a model (+ the expanded-diff set)
                        into renderable row maps; the dialog only paints and
                        moves a cursor over what this returns.

   Keeping the model/actions/rows split pure means the whole magit feature is
   exercisable in tests against a throw-away repo, with zero lanterna."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.git :as git])
  (:import [java.io File]))

(def ^:private network-timeout-secs
  "push/pull/fetch cross the network — give them a longer leash than the
   default 10s local-command timeout."
  120)

(defn- as-file ^File [root] (if (instance? File root) root (File. (str root))))

(defn- git!
  ([root args] (git/run-git (as-file root) args))
  ([root args opts] (git/run-git (as-file root) args opts)))

(defn- ok-out
  "Trimmed stdout of `git <args>` when it exits 0, else nil."
  [root args]
  (let [{:keys [exit out]} (git! root args)]
    (when (= 0 exit) (str/trimr (str out)))))

(defn- ok-lines
  [root args]
  (when-let [out (ok-out root args)]
    (if (str/blank? out) [] (str/split-lines out))))

(defn- first-error-line
  "Most useful single line out of a failed git call, for the echo bar."
  [{:keys [err out timed-out?]}]
  (or (when timed-out? "git timed out")
      (->> (concat (str/split-lines (str err)) (str/split-lines (str out)))
           (map str/trim)
           (remove str/blank?)
           first)
      "git command failed"))

(defn- action-result
  "Normalize a `run-git` result into the `{:ok? :msg}` contract every dialog
   action shares. `ok-msg` is the human confirmation on success."
  [ok-msg result]
  (if (= 0 (:exit result)) {:ok? true :msg ok-msg} {:ok? false :msg (first-error-line result)}))

;; =============================================================================
;; Model
;; =============================================================================

(def ^:private recent-commit-count 10)

(def ^:private field-sep
  "ASCII unit separator — safe against `%s` subjects containing spaces/colons."
  "\u001f")

(defn upstream-name
  "Short name of the current branch's upstream (`origin/main`), or nil."
  [root]
  (some-> (ok-out root ["rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}"])
          str/trim
          not-empty))

(defn head-subject
  "Subject line of HEAD's commit, or nil on an unborn branch."
  [root]
  (some-> (ok-out root ["log" "-1" "--format=%s"])
          str/trim
          not-empty))

(defn last-commit-message
  "Full message of HEAD's commit (for amend pre-fill), or nil."
  [root]
  (some-> (ok-out root ["log" "-1" "--format=%B"])
          str/trimr
          not-empty))

(defn- parse-commit-lines
  [lines]
  (->> lines
       (keep (fn [line]
               (let [[sha subject] (str/split line (re-pattern field-sep) 2)]
                 (when (seq (str sha)) {:sha sha :subject (str subject)}))))
       vec))

(defn recent-commits
  "Latest `n` commits as `[{:sha :subject}]`, newest first."
  [root n]
  (parse-commit-lines (ok-lines root ["log" (str "--format=%h" field-sep "%s") "-n" (str n)])))

(defn range-commits
  "Commits in `range-spec` (e.g. \"@{upstream}..HEAD\") as `[{:sha :subject}]`,
   newest first. Empty when the range is empty or unresolvable."
  [root range-spec]
  (parse-commit-lines (ok-lines root ["log" (str "--format=%h" field-sep "%s") range-spec])))

(defn stashes
  "Stash list as `[{:ref \"stash@{0}\" :message}]`, newest first."
  [root]
  (->> (ok-lines root ["stash" "list" (str "--format=%gd" field-sep "%s")])
       (keep (fn [line]
               (let [[ref message] (str/split line (re-pattern field-sep) 2)]
                 (when (seq (str ref)) {:ref ref :message (str message)}))))
       vec))

(defn local-branches
  "Local branches as `[{:name :current?}]`, current first."
  [root]
  (let [current (ok-out root ["rev-parse" "--abbrev-ref" "HEAD"])]
    (->> (ok-lines root ["for-each-ref" "refs/heads" "--format=%(refname:short)"])
         (mapv (fn [b]
                 {:name b :current? (= b current)}))
         (sort-by (complement :current?))
         vec)))

(defn- staged-entry? [{:keys [x type]}] (and (= :changed type) x (not (contains? #{\. \?} x))))

(defn- unstaged-entry? [{:keys [y type]}] (and (= :changed type) y (not= \. y)))

(defn status-model
  "One full magit snapshot of the repo at `root`, or nil outside a repo.

   {:branch :detached? :head :upstream? :upstream :ahead :behind
    :head-subject
    :untracked [path …]
    :unstaged  [{:path :code} …]     ;; worktree side (porcelain Y)
    :staged    [{:path :code} …]     ;; index side   (porcelain X)
    :unmerged  [{:path :code} …]
    :stashes   [{:ref :message} …]
    :unpushed  [{:sha :subject} …]   ;; @{upstream}..HEAD — magit's `Unmerged into`
    :unpulled  [{:sha :subject} …]   ;; HEAD..@{upstream} — magit's `Unpulled from`
    :commits   [{:sha :subject} …]}"
  [root]
  (when-let [p (git/porcelain-tokens (as-file root) {:untracked-all? true})]
    (let [entries (:entries p)
          upstream? (boolean (:upstream? p))]

      {:branch (:branch p)
       :detached? (boolean (:detached? p))
       :head (:head p)
       :upstream? upstream?
       :upstream (when upstream? (upstream-name root))
       :ahead (long (or (:ahead p) 0))
       :behind (long (or (:behind p) 0))
       :head-subject (head-subject root)
       :untracked (->> entries
                       (filter #(= :untracked (:type %)))
                       (mapv :path))
       :unstaged (->> entries
                      (filter unstaged-entry?)
                      (mapv (fn [{:keys [path y]}]
                              {:path path :code (str y)})))
       :staged (->> entries
                    (filter staged-entry?)
                    (mapv (fn [{:keys [path x]}]
                            {:path path :code (str x)})))
       :unmerged (->> entries
                      (filter #(= :unmerged (:type %)))
                      (mapv (fn [{:keys [path x y]}]
                              {:path path :code (str x y)})))
       :stashes (stashes root)
       :unpushed (if upstream? (range-commits root "@{upstream}..HEAD") [])
       :unpulled (if upstream? (range-commits root "HEAD..@{upstream}") [])
       :commits (recent-commits root recent-commit-count)})))

(defn file-diff-lines
  "Diff body for one file row, as plain lines (for the TAB fold).
   `area` picks the side: :staged → `diff --cached`, :unstaged/:unmerged →
   `diff`, :untracked → the file's own content rendered as additions."
  [root {:keys [path area]}]
  (case area
    :untracked
    (try (let [f (File. (as-file root) (str path))]
           (when (.isFile f)
             (->> (str/split-lines (slurp f))
                  (take 400)
                  (mapv #(str "+" %)))))
         (catch Throwable _ nil))

    (let [args (if (= :staged area)
                 ["diff" "--cached" "--no-color" "--" path]
                 ["diff" "--no-color" "--" path])]
      (some->> (ok-lines root args)
               ;; drop the `diff --git`/index/--- +++ preamble: the row above
               ;; already names the file, the hunks are the signal.
               (drop-while #(not (str/starts-with? % "@@")))
               vec))))

;; =============================================================================
;; Actions — all return {:ok? bool :msg str}
;; =============================================================================

(defn stage-file! [root path] (action-result (str "Staged " path) (git! root ["add" "--" path])))

(defn stage-all! [root] (action-result "Staged all changes" (git! root ["add" "-A"])))

(defn unstage-file!
  "Magit's `u`: `git reset -q -- path`. Unlike `restore --staged` this also
   works on an unborn HEAD (before the first commit), exactly like magit."
  [root path]
  (action-result (str "Unstaged " path) (git! root ["reset" "-q" "--" path])))

(defn unstage-all! [root] (action-result "Unstaged everything" (git! root ["reset" "-q"])))

(defn- head-has-path? [root path] (some? (ok-out root ["cat-file" "-e" (str "HEAD:" path)])))

(defn- head-exists? [root] (some? (ok-out root ["rev-parse" "--verify" "--quiet" "HEAD"])))

(defn discard-file!
  "Magit's `k`: throw the change away.
   - untracked file        → delete it (`git clean -f`)
   - UNSTAGED change       → restore the INDEX version in the worktree only
                             (`git checkout -- path`) — the staged half of the
                             same file survives, exactly like magit
   - staged change in HEAD → restore the HEAD version in index AND worktree
   - added, not in HEAD    → unstage only (the file survives as untracked —
     never silently delete content git has never stored)."
  [root {:keys [path area]}]
  (cond (= :untracked area) (action-result (str "Deleted " path)
                                           (git! root ["clean" "-f" "--" path]))
        (= :unstaged area) (action-result (str "Discarded unstaged changes in " path)
                                          (git! root ["checkout" "--" path]))
        (head-has-path? root path) (action-result (str "Discarded changes in " path)
                                                  (git! root ["checkout" "HEAD" "--" path]))
        :else
        (let [r (unstage-file! root path)]
          (if (:ok? r) {:ok? true :msg (str "Unstaged " path " (new file kept as untracked)")} r))))

(defn commit!
  "Refuses an empty message and — like magit's `Nothing staged` guard — a
   non-amend commit over a clean index (a bare `git commit` failure would echo
   a whole status dump). The index guard only applies when HEAD resolves:
   `diff --cached --quiet` reports clean on an unborn branch even with staged
   files."
  [root message {:keys [amend?]}]
  (cond (str/blank? (str message)) {:ok? false :msg "Empty commit message"}
        (and (not amend?)
             (head-exists? root)
             (= 0 (:exit (git! root ["diff" "--cached" "--quiet"]))))
        {:ok? false :msg "Nothing staged (stage with s/S first)"}
        :else (action-result (if amend? "Amended commit" "Committed")
                             (git! root
                                   (cond-> ["commit" "-m" (str message)]
                                     amend?
                                     (conj "--amend"))))))

(defn push!
  "`opts`: :set-upstream? adds `-u origin <branch>`, :force? uses
   `--force-with-lease` (never bare --force)."
  [root {:keys [set-upstream? force?]}]
  (let [branch
        (ok-out root ["rev-parse" "--abbrev-ref" "HEAD"])

        args
        (cond-> ["push"]
          force?
          (conj "--force-with-lease")

          set-upstream?
          (into ["-u" "origin" (str branch)]))]

    (action-result "Pushed" (git! root args {:timeout-secs network-timeout-secs}))))

(defn pull!
  [root]
  (action-result "Pulled" (git! root ["pull"] {:timeout-secs network-timeout-secs})))

(defn fetch!
  [root]
  (action-result "Fetched"
                 (git! root ["fetch" "--all" "--prune"] {:timeout-secs network-timeout-secs})))

(defn checkout-branch!
  [root branch]
  (action-result (str "Checked out " branch) (git! root ["checkout" branch])))

(defn create-branch!
  "Create AND check out `name` (magit's `b c`)."
  [root name]
  (action-result (str "Created " name) (git! root ["checkout" "-b" (str name)])))

(defn delete-branch!
  [root name {:keys [force?]}]
  (action-result (str "Deleted " name) (git! root ["branch" (if force? "-D" "-d") (str name)])))

(defn stash-push!
  "Stash the working tree INCLUDING untracked files (the least surprising
   default for a status-buffer 'stash everything' verb). Git exits 0 with
   'No local changes to save' when there is nothing to stash — surface that
   as a failure so the dialog doesn't claim success."
  [root message]
  (let [r (git! root
                (cond-> ["stash" "push" "--include-untracked"]
                  (not (str/blank? (str message)))
                  (into ["-m" (str message)])))]
    (if (and (= 0 (:exit r)) (str/includes? (str (:out r)) "No local changes to save"))
      {:ok? false :msg "No local changes to save"}
      (action-result "Stashed" r))))

(defn stash-pop!
  [root ref]
  (action-result (str "Popped " ref) (git! root ["stash" "pop" (str ref)])))

(defn stash-apply!
  [root ref]
  (action-result (str "Applied " ref) (git! root ["stash" "apply" (str ref)])))

(defn stash-drop!
  [root ref]
  (action-result (str "Dropped " ref) (git! root ["stash" "drop" (str ref)])))

;; =============================================================================
;; Pure status-buffer rows
;; =============================================================================

(def ^:private code-labels
  "Porcelain status letter → magit-style row label."
  {"M" "modified"
   "A" "new file"
   "D" "deleted"
   "R" "renamed"
   "C" "copied"
   "T" "typechange"
   "U" "conflict"})

(defn- code-label
  [code]
  (get code-labels
       (some-> code
               str
               (subs 0 1))
       "changed"))

(defn- file-row
  [area {:keys [path code]}]
  {:kind :file
   :area area
   :path path
   :code code
   :text (str "  " (format "%-10s" (code-label code)) " " path)})

(defn- untracked-row [path] {:kind :file :area :untracked :path path :text (str "  " path)})

(defn- section-rows
  "One section: a `:section` header + its member rows + expanded diff rows."
  [{:keys [area title rows expanded diff-fn]}]
  (when (seq rows)
    (into [{:kind :section :area area :text (str title " (" (count rows) ")")}]
          (mapcat (fn [row]
                    (if (and expanded (contains? expanded [(:area row) (:path row)]))
                      (into [row]
                            (map (fn [l]
                                   {:kind :diff :text (str "    " l)}))
                            (or (when diff-fn (diff-fn row)) ["(no diff)"]))
                      [row])))
          rows)))

(defn- blank-row [] {:kind :blank :text ""})

(defn head-rows
  "The `Head:` / `Merge:` preamble of the status buffer."
  [{:keys [branch detached? head head-subject upstream? upstream ahead behind]}]
  (let [sha8
        (some-> head
                (subs 0 (min 8 (count (str head)))))

        head-label
        (if detached? (str "detached " sha8) (str branch))

        sync
        (cond-> []
          (pos? (long (or ahead 0)))
          (conj (str "ahead " ahead))

          (pos? (long (or behind 0)))
          (conj (str "behind " behind)))]

    (cond-> [{:kind :info
              :text (str "Head:     " head-label (when head-subject (str "  " head-subject)))}]
      upstream?
      (conj {:kind :info
             :text (str "Merge:    "
                        (or upstream "@{upstream}")
                        (when (seq sync) (str "  (" (str/join ", " sync) ")")))})

      (not upstream?)
      (conj {:kind :info :text "Merge:    (no upstream configured)"}))))

(defn status-rows
  "PURE projection: model (+ `expanded` set of `[area path]`, + optional
   `diff-fn` row→lines) → renderable rows. Every row is
   `{:kind :info|:section|:file|:diff|:stash|:commit|:blank :text …}`.
   Section order mirrors magit: unpushed commits render as
   `Unmerged into <upstream>` and `Recent commits` appears ONLY when nothing
   is unpushed (magit's `unpushed-to-upstream-or-recent`); `Unpulled from
   <upstream>` closes the buffer when the branch is behind."
  ([model expanded] (status-rows model expanded nil))
  ([{:keys [untracked unstaged staged unmerged stashes commits upstream unpushed unpulled]
     :as model} expanded diff-fn]
   (let [commit-rows
         (fn [area title commits*]
           (when (seq commits*)
             (into [{:kind :section :area area :text title}]
                   (map (fn [{:keys [sha subject]}]
                          {:kind :commit :sha sha :text (str "  " sha " " subject)}))
                   commits*)))

         upstream-label
         (or upstream "@{upstream}")

         sections
         [(section-rows {:area :untracked
                         :title "Untracked files"
                         :rows (mapv untracked-row untracked)
                         :expanded expanded
                         :diff-fn diff-fn})
          (section-rows {:area :unmerged
                         :title "Unmerged (conflicts)"
                         :rows (mapv #(file-row :unmerged %) unmerged)
                         :expanded expanded
                         :diff-fn diff-fn})
          (section-rows {:area :unstaged
                         :title "Unstaged changes"
                         :rows (mapv #(file-row :unstaged %) unstaged)
                         :expanded expanded
                         :diff-fn diff-fn})
          (section-rows {:area :staged
                         :title "Staged changes"
                         :rows (mapv #(file-row :staged %) staged)
                         :expanded expanded
                         :diff-fn diff-fn})
          (when (seq stashes)
            (into [{:kind :section :area :stashes :text (str "Stashes (" (count stashes) ")")}]
                  (map (fn [{:keys [ref message]}]
                         {:kind :stash :ref ref :text (str "  " ref ": " message)}))
                  stashes))
          (if (seq unpushed)
            (commit-rows :unpushed
                         (str "Unmerged into " upstream-label " (" (count unpushed) ")")
                         unpushed)
            (commit-rows :commits "Recent commits" commits))
          (commit-rows :unpulled
                       (str "Unpulled from " upstream-label " (" (count unpulled) ")")
                       unpulled)]

         clean?
         (not (or (seq untracked) (seq unstaged) (seq staged) (seq unmerged)))]

     (vec (concat (head-rows model)
                  [(blank-row)]
                  (when clean?
                    [{:kind :info :text "Nothing to commit — working tree clean"} (blank-row)])
                  (->> sections
                       (remove nil?)
                       (interpose [(blank-row)])
                       (mapcat identity)))))))

(defn selectable?
  "Rows the dialog cursor can land on."
  [{:keys [kind]}]
  (contains? #{:file :section :stash :commit :repo} kind))

(defn first-selectable
  "Index of the first selectable row at-or-after `idx` (wrapping search both
   directions handled by the caller); nil when nothing is selectable."
  [rows idx]
  (let [n (count rows)]
    (or (some #(when (selectable? (nth rows %)) %) (range (min (max idx 0) n) n))
        (some #(when (selectable? (nth rows %)) %) (range (min (max idx 0) n))))))

(defn next-selectable
  "Index of the nearest selectable row moving from `idx` by `dir` (+1/-1);
   returns `idx` unchanged when there is none further that way."
  [rows idx dir]
  (let [n (count rows)]
    (loop [i (+ (long idx) (long dir))]
      (cond (or (neg? i) (>= i n)) idx
            (selectable? (nth rows i)) i
            :else (recur (+ i (long dir)))))))

(defn section-of
  "The `[area path]`-bearing rows covered by a `:section` header row —
   used for section-wide stage/unstage (`s`/`u` on the header). Stops at the
   next `:section` OR `:repo` header and only matches rows of the SAME repo
   (`:root`), so a multi-root buffer never bleeds one repo's files into
   another's section."
  [rows section-idx]
  (let [{:keys [area root]} (nth rows section-idx)]
    (->> (subvec (vec rows) (inc (long section-idx)))
         (take-while #(not (contains? #{:section :repo} (:kind %))))
         (filterv #(and (= :file (:kind %)) (= area (:area %)) (= root (:root %)))))))

;; =============================================================================
;; Multi-root workspaces (trunk + extra filesystem roots, drafts)
;; =============================================================================

(defn- wget
  "Read `k` from a map tolerating BOTH key shapes the workspace record travels
   in: the in-process kebab keys (`:repo-root`) and the canonical snake wire
   keys (`:repo_root`) the detached TUI receives from the gateway."
  [m k]
  (when m
    (let [v (clojure.core/get m k)]
      (if (some? v) v (clojure.core/get m (keyword (str/replace (name k) "-" "_")))))))

(defn- root-label
  "Human label for a repo entry: the basename of its REAL (trunk) directory."
  [path]
  (let [n (some-> path
                  str
                  as-file
                  .getName)]
    (if (str/blank? (str n)) (str path) (str n))))

(defn workspace-roots
  "Ordered repo entries the magit buffer shows for one SESSION: the primary
   workspace root first, then every extra filesystem root (`/fs`, the dir
   picker). Accepts the gateway `session-workspace-info` map in EITHER key
   shape (in-process kebab or canonical snake wire); `ws` nil → one entry for
   `fallback-root`.

   DRAFT semantics: each entry's `:root` is the path the session actually
   EDITS — for a draft that's the CLONE (primary `:root`, extras' `:clone`),
   never the trunk — with `:trunk` carrying the real dir it was forked from
   and `:draft?` set. Deduped by `:root`, order preserved.

   Entry: `{:root abs-path :trunk abs-path :label basename :draft? bool}`."
  [ws fallback-root]
  (let [primary-root
        (or (wget ws :root)
            (some-> fallback-root
                    str
                    not-empty))

        primary-trunk
        (or (wget ws :repo-root) primary-root)

        primary
        (when primary-root
          {:root (str primary-root)
           :trunk (str primary-trunk)
           :label (root-label primary-trunk)
           :draft? (boolean (or (wget ws :draft?) (wget ws :fork-ms)))})

        extras
        (->> (or (wget ws :filesystem-roots) [])
             (keep (fn [e]
                     (let [trunk
                           (wget e :trunk)

                           clone
                           (or (wget e :clone) trunk)]

                       (when clone
                         {:root (str clone)
                          :trunk (str (or trunk clone))
                          :label (root-label (or trunk clone))
                          :draft? (boolean (or (wget e :fork-ms)
                                               (and trunk (not= (str clone) (str trunk)))))})))))]

    (->> (into (if primary [primary] []) extras)
         (reduce (fn [{:keys [seen out] :as acc} {:keys [root] :as entry}]
                   (if (contains? seen root) acc {:seen (conj seen root) :out (conj out entry)}))
                 {:seen #{} :out []})
         :out)))

(defn repo-row
  "The repo header row a multi-root buffer shows above each repo's sections."
  [{:keys [label root draft?]}]
  {:kind :repo :root root :text (str "Repository " label (when draft? " (draft)") " — " root)})

(defn multi-status-rows
  "Rows for one OR many repos. `repos` is `[{:root :label :draft? :model}]`
   (`:model` a `status-model`, nil outside a git repo). ONE repo renders
   exactly like `status-rows` — the single-root buffer looks unchanged —
   while several repos each get a `:repo` header row. EVERY row is tagged
   with its `:root` so the dialog's verbs route to the right repo.

   `expanded` is a set of `[root area path]` triples; `diff-fn` receives the
   root-tagged file row."
  [repos expanded diff-fn]
  (let [multi? (> (count repos) 1)]
    (vec (mapcat (fn [{:keys [root model] :as repo}]
                   (let [expanded-here (into #{}
                                             (keep (fn [[r a p]]
                                                     (when (= r root) [a p])))
                                             (or expanded #{}))
                         diff-here (when diff-fn
                                     (fn [row]
                                       (diff-fn (assoc row :root root))))
                         body (if model
                                (status-rows model expanded-here diff-here)
                                [{:kind :info :text "Not a git repository"}])
                         body (mapv #(assoc % :root root) body)]

                     (if multi?
                       (into [(repo-row repo)] (conj body (assoc (blank-row) :root root)))
                       body)))
                 repos))))

(defn load-repos
  "Attach a fresh `:model` snapshot to every repo entry — one `status-model`
   read per root. The refresh path of a multi-root buffer."
  [repo-entries]
  (mapv #(assoc % :model (status-model (:root %))) repo-entries))
