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

;; ============================================================================
;; MAGIT KEYBINDINGS — the TUI status buffer (C-x g), key-for-key
;; ============================================================================
;;
;; This is the reference for every key the magit status buffer answers to.
;; The keys themselves are DISPATCHED in the dialog layer, not here:
;;   - single-verb dispatch : dialogs.clj `magit-char-action!`  (case on the char)
;;   - navigation / motion  : dialogs.clj magit-dialog! key loop (arrows, TAB, …)
;; This porcelain file supplies the ACTIONS those keys call (`stage-file!`,
;; `commit!`, `push!`, …). Keys are CASE-SENSITIVE, exactly like Emacs magit:
;; `s` ≠ `S`, `u` ≠ `U`, `f` ≠ `F`.
;;
;; ── Navigation / cursor ─────────────────────────────────────────────────────
;;   ↑ / ↓        move the cursor one selectable row (files, commits, stashes)
;;   n / p        jump to next / previous SECTION header (magit section motion)
;;   PageUp/Down  scroll the buffer a page (magit uses SPC/DEL for this)
;;   Home / End   first selectable row / bottom of the buffer
;;   TAB          fold/unfold the diff under the row (a file/commit/stash's patch)
;;   RET          visit: open a file's contents, or a commit/stash's full patch
;;   q / Esc      bury (close) the status buffer
;;
;; ── Staging (the heart of magit) ────────────────────────────────────────────
;;   s            stage the thing at point — a whole file, or just its hunk
;;   u            unstage the thing at point — a whole file, or just its hunk
;;   S            stage ALL unstaged + untracked changes
;;   U            unstage EVERYTHING back to the working tree
;;
;; ── Discarding / committing ─────────────────────────────────────────────────
;;   k            discard the change at point (throw it away — asks first)
;;   c            commit flow (message prompt; supports amend)
;;
;; ── History / inspection ────────────────────────────────────────────────────
;;   l            log — the graph log viewer
;;   C-w          copy the sha / path / ref at point (magit-copy-section-value)
;;
;; ── Remote / sync ───────────────────────────────────────────────────────────
;;   P            push flow (upstream, force, remote, dry-run, no-verify; Gerrit-aware)
;;   F            pull  (fetch + merge/rebase the upstream)
;;   f            fetch (no merge)
;;
;; ── Branch / stash ──────────────────────────────────────────────────────────
;;   b            branch flow (checkout / create / delete)
;;   z            stash flow (push / pop / apply / drop)
;;
;; ── Buffer ──────────────────────────────────────────────────────────────────
;;   g            refresh the status buffer from disk
;;
;; ── FAITHFUL to vanilla Emacs magit — no divergence ──────────────────────────
;; Every key above is bound to the SAME verb Emacs magit binds it to:
;;   k discard · g refresh · C-w copy-section-value · n/p section motion ·
;;   s/u/S/U stage · c commit · l log · P/F/f remote · b branch · z stash ·
;;   TAB toggle · RET visit · q bury.  Case-sensitive, exactly like magit.
;;
;; Magit keys we do NOT implement yet are deliberately left UNBOUND — never
;; remapped to a wrong verb — so magit muscle memory is never betrayed:
;;   x reset (magit-reset-quickly) · r rebase · y show-refs · d/D + e/E diff/ediff
;;   · m merge · M remote · V revert · X reset transient · A cherry-pick ·
;;   a/v apply/reverse · t tag · w/W am/patch · !/: run-git · i/I gitignore ·
;;   j jump · +/-/0 diff-context · G refresh-all · SPC/DEL scroll ·
;;   M-n/M-p sibling motion · ^ parent · C-TAB/S-TAB visibility cycling.
;; ============================================================================

(set! *unchecked-math* :warn-on-boxed)

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

(def ^:private log-graph-format
  "git pretty-format for the graph log viewer (magit's log look): the
   auto-colored abbreviated sha, its ref decorations, the subject, then a
   dim `— author, relative-date` trailer. `%C(auto)`/`%C(reset)` emit git's
   own ANSI, which the log viewer paints through `render/paint-ansi-line!`."
  (str "%C(auto)%h%C(reset)%C(auto)%d%C(reset) %s" "%C(dim white) — %an, %ar%C(reset)"))

(defn log-graph-lines
  "Pretty `git log --graph` for the fullscreen log viewer, as ANSI-colored
   lines (git's own `--color=always` graph). `opts`:
   - `:all?` include every ref, not just HEAD's history (default false)
   - `:max`  commit cap (default 250).
   Returns a vec of lines; a one-line notice when the repo has no commits."
  ([root] (log-graph-lines root {}))
  ([root {:keys [all? max] :or {max 250}}]
   (let
     [args (cond->
             ["log" "--graph" "--color=always" (str "--max-count=" (long max))
              (str "--pretty=format:" log-graph-format)]
             all?
             (conj "--all"))]
     (or (not-empty (ok-lines root args)) ["(no commits yet)"]))))

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
    (let
      [entries (:entries p)
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

    (let
      [args (if (= :staged area)
              ["diff" "--cached" "--no-color" "--" path]
              ["diff" "--no-color" "--" path])]
      (some->> (ok-lines root args)
               ;; drop the `diff --git`/index/--- +++ preamble: the row above
               ;; already names the file, the hunks are the signal.
               (drop-while #(not (str/starts-with? % "@@")))
               vec))))

(defn commit-diff-lines
  "Diff body for one commit row, as plain lines (for the TAB fold): the patch
   `git show` produces for `sha`, kept from the first `diff --git` header down so
   every file the commit touched is named."
  [root {:keys [sha]}]
  (when sha
    (some->> (ok-lines root ["show" "--no-color" "--format=" sha])
             (drop-while str/blank?)
             vec)))

(defn stash-diff-lines
  "Diff body for one stash row, as plain lines (for the TAB fold): the patch
   `git stash show -p` produces for `ref`, from the first `diff --git` header
   down so every file the stash touched is named."
  [root {:keys [ref]}]
  (when ref
    (some->> (ok-lines root ["stash" "show" "-p" "--no-color" ref])
             (drop-while str/blank?)
             vec)))

(defn visit-file-lines
  "Working-tree content of `path` (relative to `root`) as plain lines for the
   fullscreen `RET`-visit viewer, or nil when it isn't a readable regular file
   (a deleted/renamed entry has no working-tree body — the caller falls back to
   the diff)."
  [root path]
  (try (let [f (File. (as-file root) (str path))]
         (when (.isFile f) (vec (str/split-lines (slurp f)))))
       (catch Throwable _ nil)))

(defn split-diff-hunks
  "PURE: split a unified diff (as `lines`, WITH its `diff --git`/`index`/`--- `/
   `+++ ` preamble) into `{:header [lines] :hunks [[lines] ...]}` — the preamble
   shared by every hunk, then one line-vector per `@@` hunk. A valid one-hunk
   patch for hunk N is therefore `(into header (nth hunks N))`, which `git apply`
   accepts. Header is everything before the first `@@`; each hunk runs from its
   `@@` line up to (not including) the next one."
  [lines]
  (let
    [header
     (vec (take-while #(not (str/starts-with? % "@@")) lines))

     body
     (drop (count header) lines)

     hunks
     (reduce (fn [acc l]
               (if (str/starts-with? l "@@")
                 (conj acc [l])
                 (if (seq acc) (conj (pop acc) (conj (peek acc) l)) acc)))
             []
             body)]

    {:header header :hunks hunks}))

(defn- raw-diff-lines
  "Diff for `path` (`git diff [--cached] -- path`) as RAW lines WITH preamble and
   WITHOUT trimming — trailing whitespace on a blank context line must survive so
   the reconstructed patch stays byte-faithful for `git apply`."
  [root path cached?]
  (let
    [args
     (cond-> ["diff" "--no-color"]
       cached?
       (conj "--cached")

       true
       (into ["--" path]))

     {:keys [exit out]}
     (git! root args)]

    (when (and (= 0 exit) (seq (str out))) (str/split-lines (str out)))))

(defn- apply-hunk-patch!
  "Write a one-hunk patch to a temp file and `git apply --cached [--reverse]` it
   (run-git has no stdin, so a temp file is the path in). Returns the git result."
  [root patch-lines reverse?]
  (let [tmp (File/createTempFile "vis-hunk" ".patch")]
    (try (spit tmp (str (str/join "\n" patch-lines) "\n"))
         (git! root
               (cond-> ["apply" "--cached"]
                 reverse?
                 (conj "--reverse")

                 true
                 (conj (.getPath tmp))))
         (finally (.delete tmp)))))

(defn stage-hunk!
  "Magit's `s` on a diff HUNK: stage just hunk `hunk` (0-based, in `@@` order) of
   `path`'s UNSTAGED diff — reconstruct that one hunk into a patch and
   `git apply --cached` it, leaving the file's other hunks unstaged."
  [root {:keys [path hunk]}]
  (let [{:keys [header hunks]} (split-diff-hunks (raw-diff-lines root path false))]
    (if-let [h (get hunks hunk)]
      (action-result (str "Staged hunk in " path) (apply-hunk-patch! root (into header h) false))
      {:ok? false :msg "No such hunk to stage"})))

(defn unstage-hunk!
  "Magit's `u` on a staged diff HUNK: unstage just hunk `hunk` (0-based) of
   `path`'s STAGED diff — reverse-apply that one hunk to the index."
  [root {:keys [path hunk]}]
  (let [{:keys [header hunks]} (split-diff-hunks (raw-diff-lines root path true))]
    (if-let [h (get hunks hunk)]
      (action-result (str "Unstaged hunk in " path) (apply-hunk-patch! root (into header h) true))
      {:ok? false :msg "No such hunk to unstage"})))

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
  "`opts`: :remote (default \"origin\"), :set-upstream? adds `-u <remote> <branch>`,
   :force? uses `--force-with-lease` (never bare --force), :dry-run? adds
   `--dry-run`, :no-verify? adds `--no-verify` to skip pre-push hooks."
  [root {:keys [set-upstream? force? remote dry-run? no-verify?]}]
  (let
    [branch
     (ok-out root ["rev-parse" "--abbrev-ref" "HEAD"])

     remote
     (or remote "origin")

     args
     (cond-> ["push"]
       force?
       (conj "--force-with-lease")

       dry-run?
       (conj "--dry-run")

       no-verify?
       (conj "--no-verify")

       set-upstream?
       (into ["-u" remote (str branch)])

       (and (not set-upstream?) (not= "origin" remote))
       (conj remote))]

    (action-result "Pushed" (git! root args {:timeout-secs network-timeout-secs}))))

(defn current-branch
  "Short name of the current branch (`main`), or nil on a detached/unborn HEAD."
  [root]
  (let [b (ok-out root ["rev-parse" "--abbrev-ref" "HEAD"])]
    (when-not (or (str/blank? (str b)) (= "HEAD" b)) b)))

(defn remotes
  "Configured remotes as `[{:name :url}]` (the PUSH url), in git's own order.
   Deduplicated so a remote with distinct fetch/push urls shows once."
  [root]
  (->> (ok-lines root ["remote" "-v"])
       (keep (fn [line]
               (let [[name url kind] (str/split (str/trim (str line)) #"\s+")]
                 (when (and name url (= "(push)" kind)) {:name name :url url}))))
       distinct
       vec))

(defn- gerrit-url?
  "Heuristic: does a remote URL point at a Gerrit server? Gerrit's SSH port is
   29418, its hosts usually carry `gerrit`, and its authenticated HTTP path is
   `/a/`."
  [url]
  (let [u (str url)]
    (or (str/includes? u ":29418") (str/includes? u "gerrit") (str/includes? u "/a/"))))

(defn- gitreview-file ^File [root] (File. (as-file root) ".gitreview"))

(defn gerrit-remote
  "Name of the remote that talks to a Gerrit server, or nil. Prefers an explicit
   `gerrit` remote, then any remote whose URL smells like Gerrit, then falls back
   to the first remote when a `.gitreview` file is present."
  [root]
  (let [rs (remotes root)]
    (or (some #(when (= "gerrit" (:name %)) (:name %)) rs)
        (some #(when (gerrit-url? (:url %)) (:name %)) rs)
        (when (.exists (gitreview-file root)) (or (:name (first rs)) "origin")))))

(defn gerrit?
  "True when this repo appears to target a Gerrit server."
  [root]
  (some? (gerrit-remote root)))

(defn gerrit-target-branch
  "Branch a Gerrit review targets: the upstream branch (minus its remote), else
   the current branch, else `master`."
  [root]
  (or (some-> (upstream-name root)
              (str/replace #"^[^/]+/" ""))
      (current-branch root)
      "master"))

(defn refs-for-spec
  "PURE: the Gerrit push refspec `HEAD:refs/for/<branch>`, with an optional
   `%topic=<topic>` appended when `topic` is non-blank."
  [branch topic]
  (str "HEAD:refs/for/"
       branch
       (when-not (str/blank? (str topic)) (str "%topic=" (str/trim (str topic))))))

(defn gerrit-push!
  "Push HEAD to Gerrit for review (`refs/for/<branch>`) — a regular push, just
   carrying an optional Gerrit topic. `opts`: :remote :branch :topic :dry-run?
   :no-verify?. `remote`/`branch` default to the detected Gerrit remote and
   upstream branch."
  [root {:keys [remote branch topic dry-run? no-verify?]}]
  (let
    [remote
     (or remote (gerrit-remote root) "origin")

     branch
     (or branch (gerrit-target-branch root))

     spec
     (refs-for-spec branch topic)

     args
     (cond-> ["push"]
       dry-run?
       (conj "--dry-run")

       no-verify?
       (conj "--no-verify")

       :always
       (into [remote spec]))]

    (action-result (str "Pushed for review → refs/for/"
                        branch
                        (when-not (str/blank? (str topic))
                          (str " (topic " (str/trim (str topic)) ")")))
                   (git! root args {:timeout-secs network-timeout-secs}))))

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
  (let
    [r (git! root
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

(def ^:private default-collapsed-sections
  "Sections magit folds shut on entry, shown as a header only until TAB opens
   them. Recent commits stay OPEN so the project's latest history is always in
   view; the noisy working-tree churn (unstaged edits) plus the stash list and
   the unpushed/unpulled logs start collapsed so the buffer opens tidy."
  #{:stashes :unstaged :unpushed :unpulled})

(defn section-open?
  "Is `area`'s section currently expanded? `expanded` carries `[:section area]`
   toggle keys that FLIP a section from its default — a default-collapsed
   section (stash list, commit log) opens when its key is present; a
   default-open working-tree section closes when its key is present."
  [expanded area]
  (let [flipped? (contains? (or expanded #{}) [:section area])]
    (if (contains? default-collapsed-sections area) flipped? (not flipped?))))

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
  "One section: a `:section` header + its member rows + expanded diff rows.
   Collapsed (see `section-open?`) it renders as the header alone. Diff rows of a
   stageable file (staged/unstaged) carry `:area`/`:path`/`:hunk`/`:stageable?`
   so the cursor can land on them for hunk-level `s`/`u`."
  [{:keys [area title rows expanded diff-fn]}]
  (when (seq rows)
    (let
      [open?
       (section-open? expanded area)

       header
       {:kind :section
        :area area
        :members rows
        :collapsible? true
        :collapsed? (not open?)
        :text (str title " (" (count rows) ")")}]

      (if-not open?
        [header]
        (into [header]
              (mapcat
                (fn [row]
                  (if (and expanded (contains? expanded [(:area row) (:path row)]))
                    (let
                      [stageable? (and (= :file (:kind row))
                                       (contains? #{:staged :unstaged} (:area row)))]
                      (into [row]
                            ;; number diff lines by their enclosing `@@` hunk so a
                            ;; stageable file's rows carry `:hunk`/`:stageable?`
                            (:acc (reduce (fn [{:keys [hunk acc]} l]
                                            (let
                                              [head? (str/starts-with? (str l) "@@")
                                               hunk (if head? (inc (long hunk)) hunk)]

                                              {:hunk hunk
                                               :acc (conj acc
                                                          (cond-> {:kind :diff :text (str "    " l)}
                                                            (and stageable? (nat-int? hunk))
                                                            (assoc :area
                                                              (:area row) :path
                                                              (:path row) :hunk
                                                              hunk :stageable?
                                                              true)))}))
                                          {:hunk -1 :acc []}
                                          (or (when diff-fn (diff-fn row)) ["(no diff)"])))))
                    [row])))
              rows)))))

(defn- blank-row [] {:kind :blank :text ""})

(defn head-rows
  "The `Head:` / `Merge:` preamble of the status buffer."
  [{:keys [branch detached? head head-subject upstream? upstream ahead behind]}]
  (let
    [sha8
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

    (cond->
      [{:kind :info
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
   (let
     [commit-rows
      (fn [area title commits*]
        (when (seq commits*)
          (let
            [open?
             (section-open? expanded area)

             header
             {:kind :section :area area :collapsible? true :collapsed? (not open?) :text title}]

            (if-not open?
              [header]
              (into
                [header]
                (mapcat (fn [{:keys [sha subject]}]
                          (let
                            [row
                             {:kind :commit :area area :sha sha :text (str "  " sha " " subject)}]
                            (if (and expanded (contains? expanded [area sha]))
                              (into [row]
                                    (map (fn [l]
                                           {:kind :diff :text (str "    " l)}))
                                    (or (when diff-fn (diff-fn row)) ["(no diff)"]))
                              [row]))))
                commits*)))))

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
         (let
           [open?
            (section-open? expanded :stashes)

            header
            {:kind :section
             :area :stashes
             :collapsible? true
             :collapsed? (not open?)
             :text (str "Stashes (" (count stashes) ")")}]

           (if-not open?
             [header]
             (into [header]
                   (mapcat (fn [{:keys [ref message]}]
                             (let
                               [row {:kind :stash
                                     :area :stashes
                                     :ref ref
                                     :text (str "  " ref ": " message)}]
                               (if (and expanded (contains? expanded [:stashes ref]))
                                 (into [row]
                                       (map (fn [l]
                                              {:kind :diff :text (str "    " l)}))
                                       (or (when diff-fn (diff-fn row)) ["(no diff)"]))
                                 [row]))))
                   stashes))))
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
  "Rows the dialog cursor can land on. A `:diff` row is landable only when it is
   `:stageable?` (a hunk of a staged/unstaged file), so plain diff peeks stay
   scroll-only."
  [{:keys [kind stageable?]}]
  (or (contains? #{:file :section :stash :commit :repo} kind) (and (= :diff kind) stageable?)))

(defn first-selectable
  "Index of the first selectable row at-or-after `idx` (wrapping search both
   directions handled by the caller); nil when nothing is selectable."
  [rows idx]
  (let
    [n
     (count rows)

     idx
     (long idx)]

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

(defn next-section
  "Index of the nearest `:section`/`:repo` header from `idx` moving by `dir`
   (+1/-1); `idx` unchanged when there is none that way. Magit's `n`/`p`
   section-to-section jump."
  [rows idx dir]
  (let [n (count rows)]
    (loop [i (+ (long idx) (long dir))]
      (cond (or (neg? i) (>= i n)) idx
            (contains? #{:section :repo} (:kind (nth rows i))) i
            :else (recur (+ i (long dir)))))))

(defn section-of
  "The `[area path]`-bearing rows covered by a `:section` header row —
   used for section-wide stage/unstage (`s`/`u` on the header). Stops at the
   next `:section` OR `:repo` header and only matches rows of the SAME repo
   (`:root`), so a multi-root buffer never bleeds one repo's files into
   another's section."
  [rows section-idx]
  (let [{:keys [area root members]} (nth rows section-idx)]
    (if (seq members)
      ;; header carries its member file rows verbatim, so section-wide
      ;; stage/unstage works even while the section is COLLAPSED (no file
      ;; rows are rendered to scan). Re-tag with the header's root.
      (mapv #(assoc % :root root) members)
      (->> (subvec (vec rows) (inc (long section-idx)))
           (take-while #(not (contains? #{:section :repo} (:kind %))))
           (filterv #(and (= :file (:kind %)) (= area (:area %)) (= root (:root %))))))))

;; =============================================================================
;; Multi-root workspaces (trunk + extra filesystem roots, drafts)
;; =============================================================================

(defn- wget
  "Read `k` from a map tolerating BOTH key shapes the workspace record travels
   in: the in-process kebab keys (`:repo-root`) and the canonical snake wire
   keys (`repo_root`, boolean `foo?` -> `is_foo`) the detached TUI receives
   from the gateway."
  [m k]
  (when m
    (let [v (clojure.core/get m k)]
      (if (some? v)
        v
        (let
          [n (name k)
           n (if (str/ends-with? n "?")
               (let [base (subs n 0 (dec (count n)))]
                 (if (str/starts-with? base "is-") base (str "is-" base)))
               n)
           sk (str/replace n "-" "_")]

          (if-some [v' (clojure.core/get m (keyword sk))]
            v'
            (clojure.core/get m sk)))))))

(defn- root-label
  "Human label for a repo entry: the basename of its REAL (trunk) directory."
  [path]
  (let
    [n (some-> path
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
   EDITS — for a draft that's the CLONE (primary `:root`, extras' `:draft-dir`
   when `:isolated`), never the trunk — with `:trunk` carrying the real dir
   (the extra's `:dir`) and `:draft?` set. Deduped by `:root`, order preserved.

   Entry: `{:root abs-path :trunk abs-path :label basename :draft? bool}`."
  [ws fallback-root]
  (let
    [primary-root
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
                  (let
                    [dir
                     (wget e :dir)

                     isolated?
                     (boolean (wget e :isolated))

                     working
                     (or (when isolated? (wget e :draft-dir)) dir)]

                    (when dir
                      {:root (str working)
                       :trunk (str dir)
                       :label (root-label dir)
                       :draft? isolated?})))))]

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
    (vec
      (mapcat
        (fn [{:keys [root model] :as repo}]
          (let
            [expanded-here (into #{}
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

            (if multi? (into [(repo-row repo)] (conj body (assoc (blank-row) :root root))) body)))
        repos))))

(defn load-repos
  "Attach a fresh `:model` snapshot to every repo entry — one `status-model`
   read per root — then DROP every root that is not a git repository (`:model`
   nil). A non-repo root has nothing to show, so it never earns a header in the
   multi-root buffer. The refresh path of a multi-root buffer.

   Fallback: when NOT ONE root is a git repository the first entry is kept (with
   its nil `:model`) so a repo-less session still renders the single-root
   `Not a git repository` empty state instead of a blank buffer."
  [repo-entries]
  (let
    [with-models
     (mapv #(assoc % :model (status-model (:root %))) repo-entries)

     repos
     (filterv :model with-models)]

    (if (seq repos) repos (vec (take 1 with-models)))))
