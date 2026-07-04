(ns com.blockether.vis.ext.foundation-git.core
  "Git observation tools for the LLM under the `git/` alias.

   Ships three observation tools: `git/diff`, `git/status`,
   `git/log`. All three are read-only, JGit-backed, and run inside the
   currently bound workspace root (channels rebind `*workspace-root*`
   per turn).

   The extension activates only when the active workspace root is inside
   a git repo. No host `git` binary is required for observation tools."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-git.merge-ops :as merge-ops]
   [com.blockether.vis.ext.foundation-git.render :as render]
   [com.blockether.vis.ext.foundation-git.write-ops :as write-ops]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.git :as git-core]
   [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- in-repo?
  "True when the active workspace root sits inside a git repo. Uses JGit
   only; returns false on any failure so symbols stay hidden when there
   is no repo to read."
  [env]
  (let [root (:workspace/root env)]
    (when (string? root)
      (try (git-core/in-repository? (io/file root))
        (catch Throwable _ false)))))

(defn- activation-fn
  "Per-turn activation. Returns true only when the workspace root is in
   a git repository; false silently hides all `git/*` symbols from the
   model for that turn."
  [env]
  (boolean (in-repo? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env-root
  "Canonical workspace root from the env. Throws if missing — the
   activation-fn should have prevented the symbol from firing without
   one, so this is a defensive last line."
  [env]
  (or (:workspace/root env)
    (throw (ex-info "git/* tool fired without :workspace/root in env"
             {:type :foundation-git/no-workspace}))))

;; =============================================================================
;; Boundary wire constructors
;; =============================================================================
;;
;; The Clojure↔GraalPy boundary is strings-only: every map a tool returns
;; crosses to Python and MUST be built with STRING keys and carry NO
;; keyword/symbol values. `com.blockether.vis.internal.git` (git-core) is an
;; internal library that speaks idiomatic keyword Clojure — its maps only
;; cross because THIS extension embeds them in tool results, so we stringify
;; them here, at the point they enter the crossing value.

(defn- numstat->wire
  "git-core per-file numstat `{:file :add :del :binary? :patch}` -> the
   string-keyed shape the model reads. `binary`/`patch` are present only
   when git-core supplied them."
  [{:keys [file add del binary? patch]}]
  (cond-> {"file" file "add" add "del" del}
    binary?       (assoc "binary" true)
    (some? patch) (assoc "patch" patch)))

(defn- show->wire
  "git-core `show-commit` canonical map -> the string-keyed shape the model
   reads. Keeps the full canonical commit surface (git_show is the DETAIL
   view, unlike git_log's slimmed rows), with `files`/`stat` as string-keyed
   numstat."
  [{:keys [sha short-sha author email at committer committer-email committed-at
           subject body parents files stat]}]
  {"sha"             sha
   "short_sha"       short-sha
   "author"          author
   "email"           email
   "at"              at
   "committer"       committer
   "committer_email" committer-email
   "committed_at"    committed-at
   "subject"         subject
   "body"            body
   "parents"         (vec parents)
   "files"           (mapv numstat->wire files)
   "stat"            {"files" (:files stat) "add" (:add stat) "del" (:del stat)}})

;; =============================================================================
;; Tools
;; =============================================================================

(defn git-diff-fn
  "Diff inside the active workspace's repository.

   No opts -> default workspace diff (branch workspaces diff against the
   commit they were spawned from; trunk workspaces diff working tree vs
   HEAD). With opts the model can ask for an arbitrary range or path.

   Opts map keys:
     :from   base ref (string; sha, branch name, HEAD~N, ...).
     :to     target ref (string). nil with :from means working tree.
     :path   repo-relative path to restrict the diff to (string).

   Returns (keys present only when they carry signal — read optional
   ones with `.get`):
     {:head      10-char short sha
      :from      <resolved base ref>
      :stat      {:files N :add N :del N}
      :files     [{:file :add :del} ...]
      :branch    \"vis/abc\"            — workspace diffs only
      :kind      :trunk | :branch | :range — when known
      :to        <target ref>          — absent = working tree
      :path      <:path filter>        — when given
      :patch     <unified-diff text>   — ONLY with is_patch: True; the
                                         concatenated PER-FILE diff (each
                                         :files entry also carries its own),
                                         ALWAYS a string (\"\" when clean) so
                                         (.get \"patch\") never returns nil
      :untracked [\"path\" ...]}        — WT diffs only: files git sees
                                         but numstat can't line-count
                                         (new/untracked); absent when none."
  ([env] (git-diff-fn env nil))
  ([env opts]
   (when (and (some? opts) (not (map? opts)))
     (throw (ex-info (str "git_diff expected optional opts dict, got " (pr-str opts) ". "
                       "Call git_diff(), git_diff({\"from\": \"sha\"}), or git_diff({\"from\": \"a\", \"to\": \"b\", \"path\": \"src\"}).")
              {:type :foundation-git/invalid-opts
               :opts opts
               :expected "nil or map"
               :examples ["git_diff()"
                          "git_diff({\"from\": \"HEAD~3\"})"
                          "git_diff({\"from\": \"main\", \"to\": \"feature\"})"
                          "git_diff({\"path\": \"src/foo.clj\"})"
                          "git_diff({\"from\": \"HEAD~1\", \"is_patch\": True})"]})))
   (let [from (get opts "from")
         to   (get opts "to")
         path (get opts "path")
         _ (doseq [[k v] {"from" from "to" to "path" path}]
             (when (and (some? v) (not (string? v)))
               (throw (ex-info (str "git_diff " k " must be a string, got " (pr-str v))
                        {:type :foundation-git/invalid-opts :opts opts :key k :value v}))))
         root        (env-root env)
         root-file   (io/file root)
         ws-id       (:workspace/id env)
         db-info     (:db-info env)
         ws          (when (and db-info ws-id) (vis/workspace-get db-info ws-id))
         ws-base     (when (and (= :branch (:kind ws)) (:commit-id ws))
                       (:commit-id ws))
         ;; explicit :from wins over the workspace default; explicit :to
         ;; switches us into ref-to-ref mode (no working tree, no :untracked).
         base        (or from ws-base "HEAD")
         new-rev     (cond
                       (some? to)               to
                       (and from (not ws-base)) nil  ;; :from + no :to -> base..WT
                       ws-base                  "HEAD"  ;; default workspace shape
                       :else                    nil)
         patch?      (boolean (get opts "is_patch"))
         status      (git-core/status-snapshot root-file)
         files       (vec (or (git-core/diff-numstat root-file base new-rev path patch?) []))
         ;; WT diffs only: status entries the numstat didn't already cover
         ;; (untracked files have no +/- line). Tracked changes ride :files —
         ;; shipping the full porcelain duplicated them on every prompt.
         tracked     (set (map :file files))
         untracked   (when-not new-rev
                       (->> (:entries status)
                         (map :file)
                         (remove tracked)
                         vec))
         head        (:head status)
         +sum        (reduce + 0 (map :add files))
         -sum        (reduce + 0 (map :del files))
         kind        (cond from :range :else (:kind ws))
         range-mode? (= kind :range)]
     (extension/success
       {:result (cond-> {"from"  base
                         "stat"  {"files" (count files) "add" +sum "del" -sum}
                         "files" (mapv numstat->wire files)}
                  head (assoc "head" (let [h (str head)]
                                       (if (> (count h) 10) (subs h 0 10) h)))
                  kind (assoc "kind" (name kind))
                  new-rev (assoc "to" new-rev)
                  (seq untracked) (assoc "untracked" untracked)
                  (and (not range-mode?) (:branch ws))
                  (assoc "branch" (:branch ws))
                  path (assoc "path" path)
                  patch? (assoc "patch" (->> files (keep :patch) (str/join "\n"))))}))))

(defn- group-status-by-bucket
  "Fold flat porcelain `[{:status CODE :file PATH} ...]` (git-core's
   keyword-keyed entries) into a STRING-keyed map of bucket -> vec of file
   paths — the bucket is stated ONCE per group instead of on every file.
   Buckets are single snake-safe words (\"added\" \"modified\" \"deleted\"
   \"untracked\" \"conflicted\"): shipping the raw porcelain code as the key
   (\"M\", \"??\") made the model-facing pin render a dirty tree as a bare
   header it read as CLEAN. The value crosses the strings-only boundary, so
   the bucket keys are plain strings."
  [entries]
  (let [g (group-by :status entries)
        bucket-order [["added" "A"] ["modified" "M"] ["deleted" "D"]
                      ["untracked" "??"] ["conflicted" "UU"]]]
    (reduce (fn [m [bucket code]]
              (if-let [fs (seq (get g code))]
                (assoc m bucket (mapv :file fs))
                m))
      {} bucket-order)))

(defn- short-head
  "10-char short sha from a snapshot's :head, or nil when absent."
  [snapshot]
  (when-let [h (some-> (:head snapshot) str not-empty)]
    (if (> (count h) 10) (subs h 0 10) h)))

(defn- context-repo-status
  "Status for a filesystem-root working copy that lives in a DIFFERENT git
  repository than the primary workspace. Returns nil for roots that are
  not a separate repo or are clean — subdirs of the primary repo are
  already covered by its snapshot, and unchanged roots would only bloat
  the result."
  [^java.io.File primary-work-tree ^String root]
  (let [work-tree (git-core/repo-work-tree (io/file root))]
    (when (and work-tree
            (or (nil? primary-work-tree)
              (not= (.getCanonicalPath ^java.io.File work-tree)
                (.getCanonicalPath ^java.io.File primary-work-tree))))
      (let [snapshot (git-core/status-snapshot (io/file root))]
        (when (seq (:entries snapshot))
          {"root"    root
           "branch"  (:branch snapshot)
           "head"    (short-head snapshot)
           "changes" (group-status-by-bucket (:entries snapshot))})))))

(defn git-status-fn
  "Working-tree status of the active workspace, GROUPED BY CHANGE KIND.

   Returns:
     {:branch \"main\"
      :head   \"short-sha\"
      :changes {:modified [\"a.clj\", \"b.clj\"], :untracked [\"new.txt\"], ...}}
   where the bucket keys are :added (staged add), :modified, :deleted,
   :untracked, :conflicted. Each bucket is listed ONCE with its file paths
   instead of repeating on every file. A CLEAN tree is simply `:changes {}`
   — there is no separate boolean. `:head` is the 10-char short sha (a
   valid git ref for any follow-up git op); this result rides every later
   prompt, so it carries no derivable or oversized fields.

   Added filesystem roots that are SEPARATE git repositories (a different
   repository than the primary workspace — e.g. another project the model
   may edit) are scanned too: any DIRTY one appears under :context-repos
   as {:root :branch :head :changes}. Roots sharing the primary repo
   (subdirs) or clean roots are omitted, so a commit never silently misses
   edits in an added root while the result stays lean."
  ([env] (git-status-fn env nil))
  ([env _opts]
   (let [primary-root      (io/file (env-root env))
         snapshot          (or (git-core/status-snapshot primary-root)
                             {:branch nil :head nil :entries []})
         primary-work-tree (git-core/repo-work-tree primary-root)
         extra-roots       (->> (workspace/env-filesystem-roots env)
                             (keep :clone)
                             (keep #(some-> ^String % str/trim not-empty))
                             distinct)
         context-repos     (->> extra-roots
                             (keep #(context-repo-status primary-work-tree %))
                             seq)]
     (extension/success
       {:result (cond-> {"branch"  (:branch snapshot)
                         "changes" (group-status-by-bucket (:entries snapshot))}
                  (:head snapshot)
                  (assoc "head" (short-head snapshot))
                  context-repos
                  (assoc "context_repos" (vec context-repos)))}))))

(defn- coerce-log-limit
  "Normalize the git_log() argument into a 1..200 integer.
   Accepts nil, an integer, a numeric string (e.g. \"5\"), or a map
   carrying `:limit` / `:n` (whose value may itself be an int or string).
   Throws a `:foundation-git/invalid-opts` ex-info with examples for
   anything else, so the model sees a clear usage error instead of a
   raw JVM ClassCastException from `(long ...)`."
  ^long [arg]
  (let [v   (cond
              (nil? arg) 20
              (map? arg) (or (get arg "limit") (get arg "n") 20)
              :else      arg)
        raw (cond
              (integer? v) v
              (string? v)  (parse-long (str/trim v))
              :else        nil)]
    (when (or (nil? raw) (neg? (long raw)))
      (throw (ex-info (str "git_log expected nil, a positive integer, or {\"limit\": N}, got "
                        (pr-str arg) ". "
                        "Call git_log(), git_log(50), or git_log({\"limit\": 50}).")
               {:type :foundation-git/invalid-opts
                :opts arg
                :expected "nil, positive integer, or {\"limit\": N}"
                :examples ["git_log()" "git_log(50)" "git_log({\"limit\": 50})"]})))
    (max 1 (min 200 (long raw)))))

(defn- coerce-log-opts "Map form for git/log. Accepts `:limit/:n` for count, `:path/:ref` for\n   commit selection, `:since/:until/:author` for time + author filters, and\n   the field selectors `:subject_only` (one-line scan: only short_sha +\n   subject) and `:is_body` (opt-in full body; default DROPS it).\n   Returns a normalised opts map; anything not-a-map throws the same\n   :foundation-git/invalid-opts shape as `coerce-log-limit`." [m] (when-not (map? m) (throw (ex-info (str "git_log expected a dict, got " (pr-str m)) {:type :foundation-git/invalid-opts, :opts m}))) {:limit (coerce-log-limit (or (get m "limit") (get m "n"))), :path (when-let [p (get m "path")] (when (string? p) p)), :ref (when-let [r (get m "ref")] (when (string? r) r)), :since (get m "since"), :until (get m "until"), :author (when-let [a (get m "author")] (when (string? a) a)), :subject-only (boolean (get m "subject_only")), :is-body (boolean (get m "is_body"))})

(defn- slim-commit "Drop redundant/derivable fields from a canonical commit map so a log of\n   N commits doesn't repeat author==committer / at==committed-at / empty\n   bodies / single-parent lists N times.\n\n   `opts`:\n     :subject-only?  -> return ONLY {:short_sha :subject} (one-line scan mode)\n     :is-body?       -> include :body (opt-in; default DROPS it - :subject\n                        already carries the headline, the multi-KB body is\n                        what bloats context)\n     :body-cap       -> when included + set, hard-cap the body to that many\n                        chars (used for multi-commit logs)\n\n   Otherwise ALWAYS keeps :sha :short-sha :author :email :at :subject;\n   :committer / :committer-email / :committed-at only when they DIFFER from\n   :author / :email / :at; :parents only for a merge commit (>1 parent)." [{:keys [sha short-sha author email at subject body committer committer-email committed-at parents]} {:keys [subject-only? is-body? body-cap]}] (if subject-only? {"short_sha" short-sha, "subject" subject} (cond-> {"sha" sha, "short_sha" short-sha, "author" author, "email" email, "at" at, "subject" subject} (and is-body? (string? body) (seq (str/trim body))) (assoc "body" (if (and body-cap (> (count body) body-cap)) (str (subs body 0 body-cap) "\n[...body truncated...]") body)) (not= committer author) (assoc "committer" committer) (not= committer-email email) (assoc "committer_email" committer-email) (not= committed-at at) (assoc "committed_at" committed-at) (> (count parents) 1) (assoc "parents" (vec parents)))))

(defn git-log-fn "Recent commits in the active workspace. Default limit is 20; max 200.\n\n   Signatures:\n     git_log()                                              ; default 20\n     git_log(50)                                            ; integer limit\n     git_log({\"limit\": 50})                                ; dict form, also accepts \"n\"\n     git_log({\"path\": \"src/foo.clj\", \"limit\": 5})         ; commits touching that path\n     git_log({\"ref\": \"main\", \"limit\": 5})                 ; log from a branch/sha\n     git_log({\"subject_only\": True, \"limit\": 40})          ; one-line scan: short_sha + subject only\n     git_log({\"limit\": 1, \"is_body\": True})                ; include the full commit body\n\n   Each commit dict ALWAYS carries: sha, short_sha, author, email, at,\n   subject. To keep a history scan cheap, the rest are present ONLY when\n   they add information:\n     body          -> ONLY when you pass is_body True (opt-in; the full\n                      message body is what bloats context). Capped per-commit;\n                      a multi-commit log caps each body harder than a single one.\n     committer / committer_email / committed_at\n                   -> only when they DIFFER from author / email / at\n     parents       -> only for a merge commit (more than one parent)\n   So a normal commit is just {sha, short_sha, author, email, at, subject}.\n   Pass subject_only True for the leanest scan: each commit is ONLY\n   {short_sha, subject}.\n   Read them in Python like r[\"commits\"][0][\"subject\"]; guard the\n   optional keys (e.g. c.get(\"body\"), c.get(\"committer\"))." ([env] (git-log-fn env nil)) ([env arg] (let [root (io/file (env-root env)) {:keys [limit path ref since until author subject-only is-body]} (cond (nil? arg) {:limit 20} (map? arg) (coerce-log-opts arg) :else {:limit (coerce-log-limit arg)}) body-cap (when (and is-body (> (or limit 1) 1)) 256) status (git-core/status-snapshot root) commits (->> (or (git-core/recent-commits root limit {:path path, :ref ref, :since since, :until until, :author author}) []) (mapv (fn* [p1__44706#] (slim-commit p1__44706# {:subject-only? subject-only, :is-body? is-body, :body-cap body-cap}))))] (extension/success {:result {"branch" (:branch status), "commits" commits}}))))

(defn git-show-fn
  "Detailed view of one commit.

   Signatures:
     git_show(\"sha\")
     git_show({\"rev\": \"sha\"})
     git_show({\"rev\": \"sha\", \"is_patch\": True})  ; per-file unified diff

   Returns the canonical commit map (sha, author, email, committer,
   committed-at, parents, subject, body) plus :files (per-file +/-
   numstat against the first parent) and :stat totals. With
   `is_patch: True` every file entry also carries `:patch` with the
   unified-diff text (truncated at ~64KB per file)."
  ([env arg]
   (let [rev      (cond
                    (string? arg) arg
                    (map? arg)    (get arg "rev")
                    :else         (throw (ex-info (str "git_show expected a sha string or {\"rev\": sha}, got " (pr-str arg))
                                           {:type :foundation-git/invalid-opts :opts arg
                                            :examples ["git_show(\"HEAD\")"
                                                       "git_show(\"abc1234\")"
                                                       "git_show({\"rev\": \"HEAD~1\"})"
                                                       "git_show({\"rev\": \"HEAD\", \"is_patch\": True})"]})))
         is_patch (when (map? arg) (get arg "is_patch"))]
     (when-not (and (string? rev) (seq rev))
       (throw (ex-info (str "git_show expected a sha string or {\"rev\": sha}, got " (pr-str arg))
                {:type :foundation-git/invalid-opts :opts arg})))
     (let [root   (io/file (env-root env))
           result (git-core/show-commit root rev {:with-patch? (boolean is_patch)})]
       (when-not result
         (throw (ex-info (str "git/show could not resolve revision: " rev)
                  {:type :foundation-git/unknown-rev :rev rev})))
       (extension/success {:result (show->wire result)})))))

(defn- legendize-blame
  "Reshape a raw blame result into a commit-legend form so per-line commit
   identity (author/email/at) is stated ONCE per distinct commit instead of
   on every consecutive line.

   In : {:path :head :total :truncated? :ignored-revs
         :lines [{:line :sha :short-sha :author :email :at :content :source-line} ...]}
   Out: {:path :head :total :truncated? :ignored-revs
         :commits {<short-sha> {:sha <full> :author :email :at} ...}
         :lines   [{:line N :sha <short-sha> :content} ...]}

   `:commits` keys are SHORT-sha strings (clean Python dict keys); each line's
   :sha is that same short string, used as the legend key. Per-line
   :author/:email/:at/:source-line are dropped (now in the legend)."
  [{:keys [lines] :as result}]
  (let [commits (persistent!
                  (reduce
                    (fn [acc {:keys [sha short-sha author email at]}]
                      (if (and short-sha (not (contains? acc short-sha)))
                        (assoc! acc short-sha {"sha"    sha
                                               "author" author
                                               "email"  email
                                               "at"     at})
                        acc))
                    (transient {})
                    lines))]
    {"path"         (:path result)
     "head"         (:head result)
     "total"        (:total result)
     "truncated"    (:truncated? result)
     "ignored_revs" (vec (:ignored-revs result))
     "commits"      commits
     "lines"        (mapv (fn [{:keys [line short-sha content]}]
                            {"line" line "sha" short-sha "content" content})
                      lines)}))

(defn- coerce-line-num
  "Normalize a git_blame line-range bound (:from / :to) to a positive int.
   Accepts nil (bound absent), an integer, or a numeric string — Python
   callers naturally pass \"10\". Throws a `:foundation-git/invalid-opts`
   ex-info for anything else so the model sees a clear usage error instead
   of a raw JVM ClassCastException from `(long ...)` in blame-file."
  [k v]
  (cond
    (nil? v)     nil
    (integer? v) v
    (string? v)  (or (parse-long (str/trim v))
                   (throw (ex-info (str "git_blame " (name k) " must be a line number, got " (pr-str v)
                                     ". Call git_blame({\"path\": \"src/foo.clj\", \"from\": 10, \"to\": 40}).")
                            {:type :foundation-git/invalid-opts :key k :value v})))
    :else        (throw (ex-info (str "git_blame " (name k) " must be a line number, got " (pr-str v)
                                   ". Call git_blame({\"path\": \"src/foo.clj\", \"from\": 10, \"to\": 40}).")
                          {:type :foundation-git/invalid-opts :key k :value v}))))

(defn git-blame-fn
  "Per-line blame for one tracked file.

   Signatures:
     git_blame(\"src/foo.clj\")                                          ; whole file
     git_blame({\"path\": \"src/foo.clj\", \"from\": 10, \"to\": 40})       ; line range
     git_blame({\"path\": \"src/foo.clj\",
                \"ignore_revs\": [\"abc123\", \"def456\"]})                ; peel past those commits

   `ignore_revs` accepts sha prefixes or full shas. For every line
   attributed to one of those commits, the function re-blames the file
   from that commit's parent and re-maps the line by content proximity.
   Useful for skipping noisy whitespace / formatter / mass-rename
   commits so the real authorship surfaces.

   Returns a commit-legend shape so commit identity is stated ONCE per
   distinct commit instead of on every line:

     {path, head, total, ignored_revs,
      commits: {<short_sha>: {sha, author, email, at}, ...},
      lines:   [{line, sha, content}, ...]}

   Each line's `sha` is the SHORT sha used as the `commits` legend key.
   Read author in Python like r[\"commits\"][line[\"sha\"]][\"author\"].
   Outside the work tree or on untracked files throws
   `:foundation-git/not-tracked`."
  ([env arg]
   (let [path        (cond
                       (string? arg) arg
                       (map? arg)    (get arg "path")
                       :else         (throw (ex-info (str "git_blame expected a path string or opts dict, got " (pr-str arg))
                                              {:type :foundation-git/invalid-opts :opts arg
                                               :examples ["git_blame(\"src/foo.clj\")"
                                                          "git_blame({\"path\": \"src/foo.clj\", \"from\": 10, \"to\": 40})"
                                                          "git_blame({\"path\": \"src/foo.clj\", \"ignore_revs\": [\"abc1234\"]})"]})))
         from        (when (map? arg) (get arg "from"))
         to          (when (map? arg) (get arg "to"))
         ignore_revs (when (map? arg) (get arg "ignore_revs"))]
     (when-not (and (string? path) (seq path))
       (throw (ex-info (str "git_blame requires a non-blank path, got " (pr-str arg))
                {:type :foundation-git/invalid-opts :opts arg})))
     (when (and (some? ignore_revs) (not (sequential? ignore_revs)))
       (throw (ex-info (str "git_blame ignore_revs must be a list of sha strings, got " (pr-str ignore_revs))
                {:type :foundation-git/invalid-opts :opts arg :key :ignore_revs})))
     (let [root   (io/file (env-root env))
           result (git-core/blame-file root path
                    {:from (coerce-line-num :from from) :to (coerce-line-num :to to)
                     :ignore-revs (when (seq ignore_revs)
                                    (vec (filter string? ignore_revs)))})]
       (when-not result
         (throw (ex-info (str "git_blame failed: file is outside the repo or not tracked: " path)
                  {:type :foundation-git/not-tracked :path path})))
       (when (:binary? result)
         (throw (ex-info (str "git_blame refused: " path " is a binary blob, per-line blame is meaningless. "
                           "Use git_log({\"path\": \"" path "\"}) for commit history instead.")
                  {:type :foundation-git/binary :path path :head (:head result)})))
       (extension/success {:result (legendize-blame result)})))))

(def ^{:doc "await git_diff()                                              # workspace default
await git_diff({\"from\": \"main\", \"to\": \"feat\", \"path\": \"src\", \"is_patch\": True})

Returns {\"from\": ref, \"stat\": {\"files\": N, \"add\": N, \"del\": N},
         \"files\": [{\"file\": path, \"add\": N, \"del\": N}, ...]}
plus optional keys, read with .get: \"head\" (10-char sha), \"branch\",
\"kind\", \"to\" (absent => working tree), \"path\", \"untracked\" [paths].
is_patch True adds \"patch\" (unified text, ~64KB/file cap) to each file entry.

Gotcha: line counts are \"add\"/\"del\", not \"+\"/\"-\"."
       :arglists '([] [opts])} diff git-diff-fn)

(def ^{:doc "await git_status()

Returns {\"branch\": str, \"head\": short_sha,
         \"changes\": {\"added\"/\"modified\"/\"deleted\"/\"untracked\"/\"conflicted\": [paths]}}.
Each bucket is a list of file paths and is absent when empty.
\"head\" is the 10-char short sha (a valid ref for follow-up ops).

Gotcha: a clean tree is changes == {} (no bool flag) — check `if r[\"changes\"]`."
       :arglists '([] [opts])} status git-status-fn)

(def ^{:doc "await git_log()                                              # default 20, max 200
await git_log(50)
await git_log({\"limit\": 5, \"path\": \"src/foo.clj\", \"ref\": \"main\",
               \"since\": D, \"until\": D, \"author\": S,
               \"subject_only\": True, \"is_body\": True})

Returns {\"branch\": str, \"commits\": [commit, ...]}. Each commit ALWAYS has
\"sha\", \"short_sha\", \"author\", \"email\", \"at\", \"subject\". Optional, read
with .get: \"body\" (only with is_body True), \"committer\"/\"committer_email\"/
\"committed_at\" (only when they differ from author/email/at), \"parents\"
(only on a merge). subject_only True trims each to {\"short_sha\", \"subject\"}.

Gotcha: \"body\" is dropped unless is_body True; default commit has no body."
       :arglists '([] [arg])} log git-log-fn)

(def ^{:doc "await git_show(\"HEAD\")
await git_show({\"rev\": \"abc1234\", \"is_patch\": True})

Returns the commit map (\"sha\", \"short_sha\", \"author\", \"email\", \"at\",
\"subject\", \"body\", \"committer\", ...) plus \"files\"
[{\"file\", \"add\", \"del\"}, ...] (numstat vs first parent) and
\"stat\": {\"files\": N, \"add\": N, \"del\": N}.
is_patch True adds \"patch\" (unified text) to each file entry.

Gotcha: line counts are \"add\"/\"del\", not \"+\"/\"-\"."
       :arglists '([arg])} show git-show-fn)

(def ^{:doc "await git_blame(\"src/foo.clj\")
await git_blame({\"path\": \"src/foo.clj\", \"from\": 10, \"to\": 40,
                 \"ignore_revs\": [\"abc1234\"]})

Returns a commit-legend shape:
  {\"path\", \"head\", \"total\": N, \"ignored_revs\": [...],
   \"commits\": {short_sha: {\"sha\", \"author\", \"email\", \"at\"}},
   \"lines\": [{\"line\": N, \"sha\": short_sha, \"content\": str}]}.
ignore_revs peels past noisy commits (whitespace/formatters/renames).

Gotcha: each line's \"sha\" is the SHORT sha (the legend key) — read author
via r[\"commits\"][line[\"sha\"]][\"author\"], not off the line itself."
       :arglists '([arg])} blame git-blame-fn)

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

;; All `git/*` symbols carry their `:tag :observation | :mutation`
;; INLINE on their `vis/symbol` opts; `register-extension!` walks the
;; symbol vec and populates the op registry automatically. No
;; per-extension `register-op!` boilerplate.

(def diff-symbol
  (vis/symbol #'diff
    {:before-fn inject-env
     :render    render/render-diff
     :color-role :tool-color/read
     :tag       :observation}))

(def status-symbol
  (vis/symbol #'status
    {:before-fn inject-env
     :render    render/render-status
     :color-role :tool-color/read
     :tag       :observation}))

(def log-symbol
  (vis/symbol #'log
    {:before-fn inject-env
     :render    render/render-log
     :color-role :tool-color/read
     :tag       :observation}))

(def show-symbol
  (vis/symbol #'show
    {:before-fn inject-env
     :render    render/render-show
     :color-role :tool-color/read
     :tag       :observation}))

(def blame-symbol
  (vis/symbol #'blame
    {:before-fn inject-env
     :render    render/render-blame
     :color-role :tool-color/read
     :tag       :observation}))

(def git-symbols
  ;; Observation + merge-resolve ops all live under the `git/` alias.
  ;; Merge ops are gated behind the existing `:ext/activation-fn`
  ;; (must sit in a git repo); they no-op the Python sandbox layer when
  ;; called outside an active merge (JGit-side checks surface as
  ;; structured exceptions for the model to read).
  (vec (concat
         [diff-symbol status-symbol log-symbol show-symbol blame-symbol]
         merge-ops/merge-ops-symbols
         write-ops/write-ops-symbols)))

(def ^:private prompt-text
  (str
    "git_ surface active — JGit-backed, no host git binary needed. Workspace\n"
    "VCS truth (branch, head, dirty, mainline) already rides in context under\n"
    "context[\"workspace\"]; read it there before probing. Bare Python\n"
    "functions (snake_case, dict options with snake_case keys):\n"
    "  OBSERVE (read-only):\n"
    "    git_status()                              -> {branch, head, changes: {added/modified/deleted/untracked/conflicted: [paths]}} (empty changes = clean)\n"
    "    git_diff({\"from\":.., \"to\":.., \"path\":.., \"is_patch\":True})  numstat (+untracked for WT diffs); is_patch adds unified text\n"
    "    git_log({\"limit\":.., \"path\":.., \"ref\":.., \"since\":.., \"until\":.., \"author\":.., \"subject_only\":True, \"is_body\":True})  commits (default 20, max 200); each has sha/short_sha/author/email/at/subject; body is opt-in via is_body, committer*/parents only when they differ; subject_only trims to short_sha+subject\n"
    "    git_show(sha)  or  git_show({\"rev\":.., \"is_patch\":True})  one commit: per-file numstat (+ patch)\n"
    "    git_blame(path)  or  git_blame({\"path\":.., \"from\":.., \"to\":..})  per-line blame -> {commits:{<short_sha>:{author,email,at}}, lines:[{line,sha,content}]}; author via r[\"commits\"][line[\"sha\"]][\"author\"]\n"
    "  WRITE (mutating):\n"
    "    git_add(paths)  git_commit({\"message\": ..})  git_amend(..)\n"
    "    git_push(..)  git_fetch(..)  git_reset(..)  git_branch(..)\n"
    "    git_checkout(..)  git_cherry_pick(..)  git_rebase(..)\n"
    "    git_merge({\"branch\":.., \"is_no_ff\":True, \"is_ff_only\":True, \"is_squash\":True})  merge a branch INTO current; clean->commit/ff, conflicts->MERGE-RESOLVE below\n"
    "  MERGE-RESOLVE (drive a conflicted git_merge, or any active merge):\n"
    "    git_merge_status()  git_merge_accept_ours()  git_merge_accept_theirs()\n"
    "    git_merge_mark_resolved()  git_merge_continue()  git_merge_abort()\n"
    "doc(\"git_status\"), doc(\"git_diff\"), etc. for full opts — do NOT apropos to rediscover this surface."))

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-git"
     :ext/description    "JGit-backed git/ surface: observation (diff, status, log, show, blame) + write ops (add, commit, amend, push, fetch, reset, branch, checkout, cherry-pick, rebase, merge) + merge-resolve sub-session ops (merge-status / merge-accept-ours / merge-accept-theirs / merge-mark-resolved / merge-continue! / merge-abort!). Activates only when the active workspace sits inside a repo."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/engine            {:ext.engine/alias 'git
                             :ext.engine/symbols git-symbols}
     :ext/prompt-fn         (fn [_env] prompt-text)
     :ext/kind           "git"}))

(vis/register-extension! vis-extension)
