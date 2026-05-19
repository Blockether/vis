(ns com.blockether.vis.ext.foundation.workspace
  "Foundation workspace tool: v/workspace.diff.

   PLAN.md §8 — only ONE LLM-facing workspace tool ships in v1. Spawn,
   merge, discard, switch, list are all user-driven (slash commands or
   TUI), not agent tools. The agent sees its own workspace only and can
   inspect it via this diff."
  (:require
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]))

(defn- git-out
  [dir args]
  (let [argv (cond-> ["git"]
               dir (into ["-C" (str dir)])
               true (into args))
        {:keys [exit out err]} (apply sh/sh argv)]
    (when-not (zero? exit)
      (throw (ex-info (str "git failed: " (str/join " " argv))
               {:type :foundation.workspace/git-failed
                :argv argv :exit exit :err err})))
    (str/trimr (or out ""))))

(defn- parse-numstat
  "Parse `git diff --numstat` lines into [{:file :+ :-}]."
  [out]
  (->> (str/split-lines (str out))
    (remove str/blank?)
    (mapv (fn [line]
            (let [[adds dels file] (str/split line #"\t" 3)
                  parse-n (fn [s] (try (Long/parseLong (str s))
                                    (catch Exception _ 0)))]
              {:file file
               :+    (parse-n adds)
               :-    (parse-n dels)})))))

(defn- parse-porcelain
  "Parse `git status --porcelain` lines into [{:status :file}]."
  [out]
  (->> (str/split-lines (str out))
    (remove str/blank?)
    (mapv (fn [line]
            (let [len (count line)]
              {:status (str/trim (subs line 0 (min 2 len)))
               :file   (when (> len 3) (str/triml (subs line 3 len)))})))))

(defn workspace-diff-fn
  "Inspect the diff of the currently bound workspace.

   For a branch-kind workspace, diffs the worktree's HEAD against its
   parent's HEAD (recorded as `commit_id` at spawn time). For a
   trunk-kind workspace, returns the working-tree diff against HEAD.

   Returns a plain Clojure map (foundation tool contract):
     {:branch    \"vis/abc\"
      :head      \"sha\"
      :kind      :trunk | :branch
      :stat      {:files N :+ N :- N}
      :files     [{:file :+ :-} ...]
      :porcelain [{:status :file} ...]}"
  [env]
  (let [ws-id (:workspace/id env)]
    (when-not ws-id
      (throw (ex-info "v/workspace.diff: no workspace bound to this session"
               {:type :foundation.workspace/no-workspace}))))
  (let [db-info  (:db-info env)
        ws-id    (:workspace/id env)
        ws       (vis/workspace-get db-info ws-id)
        root     (:root ws)
        branch   (:branch ws)
        kind     (:kind ws)
        base     (when (and (= :branch kind) (:commit-id ws))
                   (:commit-id ws))
        diff-arg (if base [base "HEAD"] ["HEAD"])
        numstat  (parse-numstat (git-out root (into ["diff" "--numstat"] diff-arg)))
        porc     (parse-porcelain (git-out root ["status" "--porcelain"]))
        head     (git-out root ["rev-parse" "HEAD"])
        +sum     (reduce + 0 (map :+ numstat))
        -sum     (reduce + 0 (map :- numstat))]
    (extension/success
      {:result {:branch    branch
                :head      head
                :kind      kind
                :stat      {:files (count numstat) :+ +sum :- -sum}
                :files     numstat
                :porcelain porc}})))

(def ^{:doc "Diff stat + porcelain for the currently bound workspace. For a branch workspace, diffs the worktree HEAD against the commit it was spawned from. For a trunk workspace, returns the working-tree diff against HEAD. Returns {:branch :head :kind :stat {:files :+ :-} :files [...] :porcelain [...]}."
       :arglists '([])} workspace.diff workspace-diff-fn)

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

(def workspace-diff-symbol
  (vis/symbol #'workspace.diff
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def workspace-symbols
  [workspace-diff-symbol])
