(ns com.blockether.vis.ext.channel-tui.magit-test
  "End-to-end coverage for the magit layer: every model read and every action
   is exercised against REAL throw-away repositories via the actual `git`
   binary (never mocks), plus pure coverage for the status-buffer row
   projection and the C-x g / footer wiring."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.magit :as magit]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;;; ── Throw-away repo helpers ─────────────────────────────────────────────────

(defn- git-run! [dir & args] (git/run-git (io/file dir) (vec args)))

(defn- temp-dir! [] (str (Files/createTempDirectory "magit-test" (make-array FileAttribute 0))))

(defn- init-repo!
  "Fresh repo on branch `main` with one committed file `a.txt`."
  []
  (let [dir (temp-dir!)]
    (git-run! dir "init" "-b" "main")
    (git-run! dir "config" "user.email" "test@vis.dev")
    (git-run! dir "config" "user.name" "Vis Test")
    (git-run! dir "config" "commit.gpgsign" "false")
    (spit (str dir "/a.txt") "one\n")
    (git-run! dir "add" "-A")
    (git-run! dir "commit" "-m" "initial")
    dir))

(defn- add-bare-remote!
  "Create a bare repo and wire it as `origin` of `dir`. Returns its path."
  [dir]
  (let [remote (temp-dir!)]
    (git-run! remote "init" "--bare" "-b" "main")
    (git-run! dir "remote" "add" "origin" remote)
    remote))

;;; ── status-model ────────────────────────────────────────────────────────────

(defdescribe status-model-test
             (it "reads head facts of a fresh repo"
                 (let [dir
                       (init-repo!)

                       m
                       (magit/status-model dir)]

                   (expect (= "main" (:branch m)))
                   (expect (false? (:detached? m)))
                   (expect (false? (:upstream? m)))
                   (expect (nil? (:upstream m)))
                   (expect (= "initial" (:head-subject m)))
                   (expect (= 1 (count (:commits m))))
                   (expect (= "initial" (:subject (first (:commits m)))))
                   (expect (empty? (:staged m)))
                   (expect (empty? (:unstaged m)))
                   (expect (empty? (:untracked m)))))
             (it "classifies untracked / unstaged / staged"
                 (let [dir (init-repo!)]
                   (spit (str dir "/new.txt") "fresh\n")
                   (spit (str dir "/a.txt") "one\ntwo\n")
                   (let [m (magit/status-model dir)]
                     (expect (= ["new.txt"] (:untracked m)))
                     (expect (= [{:path "a.txt" :code "M"}] (:unstaged m)))
                     (expect (empty? (:staged m))))
                   (git-run! dir "add" "a.txt")
                   (let [m (magit/status-model dir)]
                     (expect (= [{:path "a.txt" :code "M"}] (:staged m)))
                     (expect (empty? (:unstaged m))))))
             (it "shows a file staged AND re-modified on both sides"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "one\ntwo\n")
                   (git-run! dir "add" "a.txt")
                   (spit (str dir "/a.txt") "one\ntwo\nthree\n")
                   (let [m (magit/status-model dir)]
                     (expect (= ["a.txt"] (mapv :path (:staged m))))
                     (expect (= ["a.txt"] (mapv :path (:unstaged m)))))))
             (it "reports a deleted tracked file as D"
                 (let [dir (init-repo!)]
                   (io/delete-file (str dir "/a.txt"))
                   (let [m (magit/status-model dir)]
                     (expect (= [{:path "a.txt" :code "D"}] (:unstaged m))))))
             (it "returns nil outside a repository"
                 (expect (nil? (magit/status-model (temp-dir!))))))

;;; ── stage / unstage ─────────────────────────────────────────────────────────

(defdescribe stage-unstage-test
             (it "stages and unstages one file"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "one\ntwo\n")
                   (expect (:ok? (magit/stage-file! dir "a.txt")))
                   (expect (= ["a.txt"] (mapv :path (:staged (magit/status-model dir)))))
                   (expect (:ok? (magit/unstage-file! dir "a.txt")))
                   (expect (empty? (:staged (magit/status-model dir))))
                   (expect (= ["a.txt"] (mapv :path (:unstaged (magit/status-model dir)))))))
             (it "stages everything with stage-all! and clears with unstage-all!"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "changed\n")
                   (spit (str dir "/new.txt") "fresh\n")
                   (expect (:ok? (magit/stage-all! dir)))
                   (let [m (magit/status-model dir)]
                     (expect (= #{"a.txt" "new.txt"} (set (mapv :path (:staged m)))))
                     (expect (empty? (:untracked m))))
                   (expect (:ok? (magit/unstage-all! dir)))
                   (let [m (magit/status-model dir)]
                     (expect (empty? (:staged m)))
                     (expect (= ["new.txt"] (:untracked m)))))))

;;; ── unborn HEAD (before the first commit) ───────────────────────────────────

(defdescribe unborn-repo-test
             (it "stages and unstages before the first commit — magit works in a fresh repo"
                 (let [dir (temp-dir!)]
                   (git-run! dir "init" "-b" "main")
                   (git-run! dir "config" "user.email" "test@vis.dev")
                   (git-run! dir "config" "user.name" "Vis Test")
                   (spit (str dir "/f.txt") "x\n")
                   (expect (:ok? (magit/stage-file! dir "f.txt")))
                   (expect (= ["f.txt"] (mapv :path (:staged (magit/status-model dir)))))
                   (expect (:ok? (magit/unstage-file! dir "f.txt")))
                   (let [m (magit/status-model dir)]
                     (expect (empty? (:staged m)))
                     (expect (= ["f.txt"] (:untracked m)))))))

;;; ── discard ─────────────────────────────────────────────────────────────────

(defdescribe discard-test
             (it "deletes an untracked file"
                 (let [dir (init-repo!)]
                   (spit (str dir "/junk.txt") "junk\n")
                   (expect (:ok? (magit/discard-file! dir {:path "junk.txt" :area :untracked})))
                   (expect (not (.exists (io/file dir "junk.txt"))))))
             (it "restores a modified tracked file to HEAD"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "mangled\n")
                   (expect (:ok? (magit/discard-file! dir {:path "a.txt" :area :unstaged})))
                   (expect (= "one\n" (slurp (str dir "/a.txt"))))))
             (it "restores a STAGED modification to HEAD (index and worktree)"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "mangled\n")
                   (git-run! dir "add" "a.txt")
                   (expect (:ok? (magit/discard-file! dir {:path "a.txt" :area :staged})))
                   (expect (= "one\n" (slurp (str dir "/a.txt"))))
                   (let [m (magit/status-model dir)]
                     (expect (empty? (:staged m)))
                     (expect (empty? (:unstaged m))))))
             (it "discarding the UNSTAGED half keeps the staged half (magit semantics)"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "two\n")
                   (git-run! dir "add" "a.txt")
                   (spit (str dir "/a.txt") "three\n")
                   (expect (:ok? (magit/discard-file! dir {:path "a.txt" :area :unstaged})))
                   (expect (= "two\n" (slurp (str dir "/a.txt"))))
                   (let [m (magit/status-model dir)]
                     (expect (= ["a.txt"] (mapv :path (:staged m))))
                     (expect (empty? (:unstaged m))))))
             (it "NEVER deletes a newly-added file git has not stored — unstages it instead"
                 (let [dir (init-repo!)]
                   (spit (str dir "/new.txt") "precious\n")
                   (git-run! dir "add" "new.txt")
                   (let [r (magit/discard-file! dir {:path "new.txt" :area :staged})]
                     (expect (:ok? r))
                     (expect (str/includes? (:msg r) "untracked")))
                   (expect (.exists (io/file dir "new.txt")))
                   (expect (= ["new.txt"] (:untracked (magit/status-model dir)))))))

;;; ── commit ──────────────────────────────────────────────────────────────────

(defdescribe commit-test
             (it "commits staged changes"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "one\ntwo\n")
                   (magit/stage-file! dir "a.txt")
                   (expect (:ok? (magit/commit! dir "second" {})))
                   (let [m (magit/status-model dir)]
                     (expect (empty? (:staged m)))
                     (expect (= "second" (:head-subject m)))
                     (expect (= 2 (count (:commits m)))))))
             (it "rejects an empty message"
                 (let [dir (init-repo!)]
                   (expect (false? (:ok? (magit/commit! dir "   " {}))))))
             (it "refuses to commit over a clean index — magit's Nothing-staged guard"
                 (let [dir
                       (init-repo!)

                       r
                       (magit/commit! dir "nope" {})]

                   (expect (false? (:ok? r)))
                   (expect (str/includes? (:msg r) "Nothing staged"))))
             (it "amends the last commit without adding a new one"
                 (let [dir (init-repo!)]
                   (expect (:ok? (magit/commit! dir "initial, reworded" {:amend? true})))
                   (let [m (magit/status-model dir)]
                     (expect (= "initial, reworded" (:head-subject m)))
                     (expect (= 1 (count (:commits m)))))
                   (expect (= "initial, reworded" (magit/last-commit-message dir))))))

;;; ── branches ────────────────────────────────────────────────────────────────

(defdescribe branch-test
             (it "creates + checks out, lists with current marker, checks out back, deletes"
                 (let [dir (init-repo!)]
                   (expect (:ok? (magit/create-branch! dir "feature/x")))
                   (let [bs (magit/local-branches dir)]
                     (expect (= "feature/x" (:name (first bs))))
                     (expect (true? (:current? (first bs))))
                     (expect (= #{"feature/x" "main"} (set (map :name bs)))))
                   (expect (:ok? (magit/checkout-branch! dir "main")))
                   (expect (:ok? (magit/delete-branch! dir "feature/x" {})))
                   (expect (= ["main"] (mapv :name (magit/local-branches dir))))))
             (it "refuses -d on an unmerged branch, then -D force-deletes it"
                 (let [dir (init-repo!)]
                   (magit/create-branch! dir "wip")
                   (spit (str dir "/wip.txt") "wip\n")
                   (magit/stage-all! dir)
                   (magit/commit! dir "wip commit" {})
                   (magit/checkout-branch! dir "main")
                   (let [r (magit/delete-branch! dir "wip" {})]
                     (expect (false? (:ok? r)))
                     (expect (string? (:msg r))))
                   (expect (:ok? (magit/delete-branch! dir "wip" {:force? true}))))))

;;; ── stash ───────────────────────────────────────────────────────────────────

(defdescribe stash-test
             (it "reports the no-op stash on a clean tree as a failure"
                 (let [dir
                       (init-repo!)

                       r
                       (magit/stash-push! dir nil)]

                   (expect (false? (:ok? r)))
                   (expect (str/includes? (:msg r) "No local changes"))))
             (it "stashes (incl. untracked), lists, applies, drops and pops"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "dirty\n")
                   (spit (str dir "/new.txt") "fresh\n")
                   (expect (:ok? (magit/stash-push! dir "wip work")))
                   (let [m (magit/status-model dir)]
                     (expect (empty? (:unstaged m)))
                     (expect (empty? (:untracked m)))
                     (expect (= 1 (count (:stashes m))))
                     (expect (= "stash@{0}" (:ref (first (:stashes m)))))
                     (expect (str/includes? (:message (first (:stashes m))) "wip work")))
                   ;; apply keeps the stash entry
                   (expect (:ok? (magit/stash-apply! dir "stash@{0}")))
                   (expect (= 1 (count (:stashes (magit/status-model dir)))))
                   (expect (= "dirty\n" (slurp (str dir "/a.txt"))))
                   ;; drop removes it; re-stash then pop restores AND removes
                   (expect (:ok? (magit/stash-drop! dir "stash@{0}")))
                   (expect (empty? (:stashes (magit/status-model dir))))
                   (expect (:ok? (magit/stash-push! dir nil)))
                   (expect (:ok? (magit/stash-pop! dir "stash@{0}")))
                   (expect (empty? (:stashes (magit/status-model dir))))
                   (expect (= "dirty\n" (slurp (str dir "/a.txt")))))))

;;; ── remote: push / pull / fetch ─────────────────────────────────────────────

(defdescribe remote-test
             (it "push -u sets the upstream and clears ahead; plain push after a commit"
                 (let [dir (init-repo!)]
                   (add-bare-remote! dir)
                   (expect (:ok? (magit/push! dir {:set-upstream? true})))
                   (let [m (magit/status-model dir)]
                     (expect (true? (:upstream? m)))
                     (expect (= "origin/main" (:upstream m)))
                     (expect (zero? (:ahead m))))
                   (spit (str dir "/b.txt") "b\n")
                   (magit/stage-all! dir)
                   (magit/commit! dir "second" {})
                   (expect (= 1 (:ahead (magit/status-model dir))))
                   (expect (:ok? (magit/push! dir {})))
                   (expect (zero? (:ahead (magit/status-model dir))))))
             (it "fetch and pull succeed against the wired remote"
                 (let [dir (init-repo!)]
                   (add-bare-remote! dir)
                   (magit/push! dir {:set-upstream? true})
                   (expect (:ok? (magit/fetch! dir)))
                   (expect (:ok? (magit/pull! dir)))))
             (it "ahead/behind surface as magit's Unmerged-into / Unpulled-from sections"
                 (let [dir (init-repo!)]
                   (add-bare-remote! dir)
                   (magit/push! dir {:set-upstream? true})
                   ;; ahead 1 → Unmerged into, Recent commits hidden
                   (spit (str dir "/b.txt") "b\n")
                   (magit/stage-all! dir)
                   (magit/commit! dir "second" {})
                   (let [m (magit/status-model dir)
                         texts (mapv :text (magit/status-rows m #{}))]

                     (expect (= ["second"] (mapv :subject (:unpushed m))))
                     (expect (empty? (:unpulled m)))
                     (expect (some #(str/includes? % "Unmerged into origin/main (1)") texts))
                     (expect (not-any? #(= "Recent commits" %) texts)))
                   ;; push, then fall behind → Unpulled from, Recent commits back
                   (magit/push! dir {})
                   (git-run! dir "reset" "--hard" "HEAD~1")
                   (let [m (magit/status-model dir)
                         texts (mapv :text (magit/status-rows m #{}))]

                     (expect (= ["second"] (mapv :subject (:unpulled m))))
                     (expect (empty? (:unpushed m)))
                     (expect (some #(str/includes? % "Unpulled from origin/main (1)") texts))
                     (expect (some #(= "Recent commits" %) texts)))))
             (it "push without any remote fails with a real git message"
                 (let [dir
                       (init-repo!)

                       r
                       (magit/push! dir {})]

                   (expect (false? (:ok? r)))
                   (expect (string? (:msg r)))
                   (expect (not (str/blank? (:msg r)))))))

;;; ── remotes & gerrit ────────────────────────────────────────────────────────

(defdescribe refs-for-spec-test
             (it "builds a bare refs/for target with no topic"
                 (expect (= "HEAD:refs/for/main" (magit/refs-for-spec "main" nil))))
             (it "appends and trims a topic"
                 (expect (= "HEAD:refs/for/main%topic=my-feature"
                            (magit/refs-for-spec "main" "  my-feature  "))))
             (it "ignores a blank topic"
                 (expect (= "HEAD:refs/for/release/1.2"
                            (magit/refs-for-spec "release/1.2" "   ")))))

(defdescribe remotes-gerrit-test
             (it "lists remotes by their push url, deduped, in git order"
                 (let [dir (init-repo!)]
                   (git-run! dir "remote" "add" "origin" "https://github.com/acme/repo.git")
                   (git-run! dir "remote" "add" "gerrit" "ssh://u@gerrit.acme.com:29418/repo")
                   (let [rs (magit/remotes dir)]
                     (expect (= #{"origin" "gerrit"} (set (map :name rs))))
                     (expect (= "https://github.com/acme/repo.git"
                                (:url (first (filter #(= "origin" (:name %)) rs))))))))
             (it "detects a gerrit remote by name and by the 29418 port"
                 (let [dir (init-repo!)]
                   (git-run! dir "remote" "add" "origin" "https://github.com/acme/repo.git")
                   (git-run! dir "remote" "add" "gerrit" "ssh://u@review.acme.com:29418/repo")
                   (expect (true? (magit/gerrit? dir)))
                   (expect (= "gerrit" (magit/gerrit-remote dir)))))
             (it "is not gerrit with only a plain github remote"
                 (let [dir (init-repo!)]
                   (git-run! dir "remote" "add" "origin" "https://github.com/acme/repo.git")
                   (expect (false? (magit/gerrit? dir)))
                   (expect (nil? (magit/gerrit-remote dir)))))
             (it "falls back to .gitreview for detection"
                 (let [dir (init-repo!)]
                   (git-run! dir "remote" "add" "origin" "https://plain.example.com/repo.git")
                   (spit (str dir "/.gitreview") "[gerrit]\nhost=review.example.com\n")
                   (expect (true? (magit/gerrit? dir)))
                   (expect (= "origin" (magit/gerrit-remote dir)))))
             (it "gerrit push targets refs/for/<upstream-branch> with a topic"
                 (let [dir
                       (init-repo!)

                       remote
                       (add-bare-remote! dir)]

                   (git-run! dir "remote" "rename" "origin" "gerrit")
                   (magit/push! dir {:set-upstream? true :remote "gerrit"})
                   (let [r (magit/gerrit-push! dir {:topic "cool-topic"})]
                     (expect (:ok? r))
                     ;; the review ref landed in the bare remote
                     (let [refs (git/run-git (io/file remote)
                                             ["for-each-ref" "--format=%(refname)"])]
                       (expect (str/includes? (str (:out refs)) "refs/for/main")))))))

;;; ── diffs ───────────────────────────────────────────────────────────────────

(defdescribe diff-test
             (it "unstaged diff starts at the hunk and carries the added line"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "one\ntwo\n")
                   (let [lines (magit/file-diff-lines dir {:path "a.txt" :area :unstaged})]
                     (expect (str/starts-with? (first lines) "@@"))
                     (expect (some #(= "+two" %) lines)))))
             (it "staged diff reads the index side"
                 (let [dir (init-repo!)]
                   (spit (str dir "/a.txt") "one\ntwo\n")
                   (git-run! dir "add" "a.txt")
                   (spit (str dir "/a.txt") "one\ntwo\nthree\n")
                   (let [staged (magit/file-diff-lines dir {:path "a.txt" :area :staged})
                         unstaged (magit/file-diff-lines dir {:path "a.txt" :area :unstaged})]

                     (expect (some #(= "+two" %) staged))
                     (expect (not-any? #(= "+three" %) staged))
                     (expect (some #(= "+three" %) unstaged)))))
             (it "untracked files render their content as additions"
                 (let [dir (init-repo!)]
                   (spit (str dir "/new.txt") "alpha\nbeta\n")
                   (expect (= ["+alpha" "+beta"]
                              (magit/file-diff-lines dir {:path "new.txt" :area :untracked}))))))

;;; ── pure status-buffer rows ─────────────────────────────────────────────────

(def ^:private sample-model
  {:branch "main"
   :detached? false
   :head "abcdef0123456789"
   :head-subject "feat: things"
   :upstream? true
   :upstream "origin/main"
   :ahead 2
   :behind 1
   :untracked ["new.txt"]
   :unstaged [{:path "a.txt" :code "M"}]
   :staged [{:path "b.txt" :code "A"} {:path "c.txt" :code "D"}]
   :unmerged []
   :stashes [{:ref "stash@{0}" :message "On main: wip"}]
   :commits [{:sha "abc1234" :subject "feat: things"}]})

(defdescribe
  status-rows-test
  (it "renders head facts, sections with counts, stashes and commits in order"
      (let [rows
            (magit/status-rows sample-model #{[:section :stashes] [:section :commits]})

            texts
            (mapv :text rows)

            section-texts
            (mapv :text (filter #(= :section (:kind %)) rows))]

        (expect (str/includes? (first texts) "Head:"))
        (expect (str/includes? (first texts) "main"))
        (expect (some #(and (str/includes? % "origin/main")
                            (str/includes? % "ahead 2")
                            (str/includes? % "behind 1"))
                      texts))
        (expect (= ["Untracked files (1)" "Unstaged changes (1)" "Staged changes (2)" "Stashes (1)"
                    "Recent commits"]
                   section-texts))
        ;; file rows carry magit-style labels
        (expect (some #(and (= :file (:kind %)) (str/includes? (:text %) "new file")) rows))
        (expect (some #(and (= :file (:kind %)) (str/includes? (:text %) "deleted")) rows))
        (expect (some #(and (= :stash (:kind %)) (= "stash@{0}" (:ref %))) rows))
        (expect (some #(and (= :commit (:kind %)) (= "abc1234" (:sha %))) rows))))
  (it "folds the stash list and commit log shut by default (magit fold)"
      (let [rows (magit/status-rows sample-model #{})]
        (expect (not-any? #(= :stash (:kind %)) rows))
        (expect (not-any? #(= :commit (:kind %)) rows))
        ;; the section HEADERS still render so the counts stay visible
        (expect (some #(and (= :section (:kind %)) (= :stashes (:area %))) rows))
        (expect (some #(and (= :section (:kind %)) (= :commits (:area %))) rows))))
  (it "section-open? flips a section from its default (working open, log closed)"
      (expect (magit/section-open? #{} :unstaged))
      (expect (magit/section-open? #{} :staged))
      (expect (not (magit/section-open? #{} :stashes)))
      (expect (not (magit/section-open? #{} :commits)))
      (expect (magit/section-open? #{[:section :stashes]} :stashes))
      (expect (not (magit/section-open? #{[:section :unstaged]} :unstaged))))
  (it "expands a stash's diff lines directly under its row"
      (let [diff-fn
            (fn [{:keys [ref]}]
              [(str "@@ " ref " @@") "+stashed"])

            rows
            (magit/status-rows sample-model #{[:section :stashes] [:stashes "stash@{0}"]} diff-fn)

            idx
            (first (keep-indexed #(when (= :stash (:kind %2)) %1) rows))

            after
            (subvec rows (inc idx) (+ idx 3))]

        (expect (= [:diff :diff] (mapv :kind after)))
        (expect (str/includes? (:text (first after)) "stash@{0}"))
        (expect (str/includes? (:text (second after)) "+stashed"))))
  (it "renders unpushed as Unmerged-into and hides Recent commits (magit order)"
      (let [rows
            (magit/status-rows (assoc sample-model
                                 :unpushed [{:sha "abc1234" :subject "feat: things"}]
                                 :unpulled [{:sha "def5678" :subject "fix: other"}])
                               #{})

            section-texts
            (mapv :text (filter #(= :section (:kind %)) rows))]

        (expect (= ["Untracked files (1)" "Unstaged changes (1)" "Staged changes (2)" "Stashes (1)"
                    "Unmerged into origin/main (1)" "Unpulled from origin/main (1)"]
                   section-texts))))
  (it "notes a missing upstream instead of ahead/behind"
      (let [rows (magit/status-rows (assoc sample-model
                                      :upstream? false
                                      :upstream nil)
                                    #{})]
        (expect (some #(str/includes? (:text %) "no upstream") (take 3 rows)))))
  (it "shows the clean-tree line when nothing is pending"
      (let [rows (magit/status-rows (assoc sample-model
                                      :untracked []
                                      :unstaged []
                                      :staged []
                                      :unmerged [])
                                    #{})]
        (expect (some #(str/includes? (:text %) "working tree clean") rows))))
  (it "expands a file's diff lines directly under its row"
      (let [diff-fn
            (fn [_row]
              ["@@ -1 +1,2 @@" "+two"])

            rows
            (magit/status-rows sample-model #{[:unstaged "a.txt"]} diff-fn)

            idx
            (first (keep-indexed #(when (= "a.txt" (:path %2)) %1) rows))

            after
            (subvec rows (inc idx) (+ idx 3))]

        (expect (= [:diff :diff] (mapv :kind after)))
        (expect (str/includes? (:text (second after)) "+two"))))
  (it "keeps a collapsed buffer diff-free"
      (let [rows (magit/status-rows sample-model
                                    #{}
                                    (fn [_]
                                      ["+never"]))]
        (expect (not-any? #(= :diff (:kind %)) rows))))
  (it "expands a commit's diff lines directly under its row"
      (let [diff-fn
            (fn [{:keys [sha]}]
              [(str "@@ " sha " @@") "+added"])

            rows
            (magit/status-rows sample-model #{[:section :commits] [:commits "abc1234"]} diff-fn)

            idx
            (first (keep-indexed #(when (= "abc1234" (:sha %2)) %1) rows))

            after
            (subvec rows (inc idx) (+ idx 3))]

        (expect (= [:diff :diff] (mapv :kind after)))
        (expect (str/includes? (:text (first after)) "abc1234"))
        (expect (str/includes? (:text (second after)) "+added"))))
  (it "cursor helpers: selectable rows, movement bounds, section membership"
      (let [rows
            (magit/status-rows sample-model #{})

            first-sel
            (magit/first-selectable rows 0)]

        (expect (magit/selectable? (nth rows first-sel)))
        (expect (not (magit/selectable? {:kind :info :text "x"})))
        (expect (not (magit/selectable? {:kind :diff :text "+x"})))
        ;; moving up from the first selectable stays put
        (expect (= first-sel (magit/next-selectable rows first-sel -1)))
        ;; moving down lands on another selectable row
        (let [nxt (magit/next-selectable rows first-sel 1)]
          (expect (> nxt first-sel))
          (expect (magit/selectable? (nth rows nxt))))
        ;; section-of collects exactly the staged files under the staged header
        (let [staged-idx (first (keep-indexed
                                  #(when (and (= :section (:kind %2)) (= :staged (:area %2))) %1)
                                  rows))]
          (expect (= ["b.txt" "c.txt"] (mapv :path (magit/section-of rows staged-idx))))))))

;;; ── C-x g + footer button wiring ────────────────────────────────────────────

(defdescribe magit-wiring-test
             (it "C-x g is bound to :open-magit"
                 (expect (= :open-magit (keymap/prefix-action-for \g)))
                 (expect (= "C-x g" (keymap/label-for :open-magit))))
             (it "the footer git segment is a clickable :footer-git button"
                 (let [spans (#'footer/git-footer-spans
                              {:workspace? true :repo "vis" :branch "main"})]
                   (expect (= :footer-git (:kind (first spans))))
                   (expect (str/includes? (:text (first spans)) "vis"))
                   ;; the C-x g chord rides on the chip (discoverability gripe)
                   (expect (str/includes? (:text (first spans)) "C-x g"))
                   ;; a real repo chip is tinted green so it reads like the sibling
                   ;; resources/filesystem buttons, not muted decoration
                   (expect (= :git (:tint (first spans))))))
             (it "the DRAFT footer segment is clickable too"
                 (let [spans (#'footer/git-footer-spans
                              {:workspace? true :draft? true :draft-root "/tmp/draft"})]
                   (expect (= :footer-git (:kind (first spans))))
                   (expect (str/includes? (:text (first spans)) "C-x g"))
                   ;; a draft chip is tinted amber (isolated-tree warning colour)
                   (expect (= :draft (:tint (first spans))))))
             (it "outside a workspace the dead 'No git' label stays a plain span"
                 (let [spans (#'footer/git-footer-spans {:workspace? false})]
                   (expect (nil? (:kind (first spans)))))))

;;; ── async network verbs (push/pull/fetch never freeze the modal) ─────────────

(defdescribe async-network-run-test
             (it "runs the thunk off-thread and returns its result, ticking while it works"
                 (let [ticks
                       (atom 0)

                       result
                       (#'dialogs/run-async-with-ticker!
                        (fn []
                          (Thread/sleep 120)
                          {:ok? true :msg "done"})
                        (fn []
                          (swap! ticks inc))
                        20)]

                   (expect (= {:ok? true :msg "done"} result))
                   ;; the spinner ticked at least once — the UI thread was NOT blocked
                   (expect (pos? @ticks))))
             (it "turns a thrown thunk into an :ok? false result instead of escaping"
                 (let [result (#'dialogs/run-async-with-ticker!
                               (fn []
                                 (throw (ex-info "boom" {})))
                               (fn [])
                               10)]
                   (expect (false? (:ok? result)))
                   (expect (str/includes? (str (:msg result)) "boom"))))
             (it "a fast thunk settles immediately"
                 (expect (:ok? (#'dialogs/run-async-with-ticker!
                                (fn []
                                  {:ok? true})
                                (fn [])
                                10)))))

;;; ── Multi-root workspaces (one root + extra filesystem roots, drafts) ───────

(defdescribe
  workspace-roots-test
  (it "nil workspace falls back to the launch root"
      (expect (= [{:root "/here" :trunk "/here" :label "here" :draft? false}]
                 (magit/workspace-roots nil "/here"))))
  (it "blank fallback and nil workspace yield no roots"
      (expect (= [] (magit/workspace-roots nil "")) (= [] (magit/workspace-roots nil nil))))
  (it "trunk workspace: primary root, no draft flag"
      (let [roots (magit/workspace-roots {:root "/w/proj"} "/ignored")]
        (expect (= [{:root "/w/proj" :trunk "/w/proj" :label "proj" :draft? false}] roots))))
  (it "extra filesystem roots follow the primary, in order"
      (let [roots (magit/workspace-roots {:root "/w/proj"
                                          :filesystem-roots [{:trunk "/w/other" :clone "/w/other"}
                                                             {:trunk "/w/third" :clone "/w/third"}]}
                                         nil)]
        (expect (= ["/w/proj" "/w/other" "/w/third"] (mapv :root roots)))
        (expect (= ["proj" "other" "third"] (mapv :label roots)))
        (expect (every? (comp false? :draft?) roots))))
  (it "draft workspace: every entry points at the CLONE, labelled by the trunk"
      (let [roots (magit/workspace-roots
                    {:root "/clones/proj"
                     :repo-root "/real/proj"
                     :draft? true
                     :fork-ms 5
                     :filesystem-roots [{:trunk "/real/other" :clone "/clones/other" :fork-ms 5}]}
                    nil)]
        (expect (= [{:root "/clones/proj" :trunk "/real/proj" :label "proj" :draft? true}
                    {:root "/clones/other" :trunk "/real/other" :label "other" :draft? true}]
                   roots))))
  (it "accepts the canonical snake wire shape the detached TUI receives"
      (let [kebab
            (magit/workspace-roots {:root "/c/p"
                                    :repo-root "/r/p"
                                    :draft? true
                                    :fork-ms 1
                                    :filesystem-roots [{:trunk "/r/o" :clone "/c/o" :fork-ms 1}]}
                                   nil)

            snake
            (magit/workspace-roots {:root "/c/p"
                                    :repo_root "/r/p"
                                    :draft? true
                                    :fork_ms 1
                                    :filesystem_roots [{:trunk "/r/o" :clone "/c/o" :fork_ms 1}]}
                                   nil)]

        (expect (= kebab snake))))
  (it "an extra root missing its :clone falls back to the trunk path"
      (let [roots (magit/workspace-roots {:root "/w/proj" :filesystem-roots [{:trunk "/w/other"}]}
                                         nil)]
        (expect (= "/w/other" (:root (second roots))))
        (expect (false? (:draft? (second roots))))))
  (it "a clone differing from its trunk marks the extra a draft even without fork-ms"
      (let [roots (magit/workspace-roots {:root "/w/proj"
                                          :filesystem-roots [{:trunk "/r/o" :clone "/c/o"}]}
                                         nil)]
        (expect (true? (:draft? (second roots))))))
  (it "dedupes an extra root equal to the primary"
      (let [roots (magit/workspace-roots {:root "/w/proj"
                                          :filesystem-roots [{:trunk "/w/proj" :clone "/w/proj"}
                                                             {:trunk "/w/other" :clone "/w/other"}]}
                                         nil)]
        (expect (= ["/w/proj" "/w/other"] (mapv :root roots))))))

(defdescribe
  multi-status-rows-test
  (it "ONE root renders exactly like status-rows (tagged with its root)"
      (let [dir (init-repo!)]
        (spit (str dir "/new.txt") "x\n")
        (let [repos (magit/load-repos (magit/workspace-roots nil dir))
              multi (magit/multi-status-rows repos #{} nil)
              plain (magit/status-rows (magit/status-model dir) #{})]

          (expect (not-any? #(= :repo (:kind %)) multi))
          (expect (= (mapv :text plain) (mapv :text multi)))
          (expect (every? #(= dir (:root %)) multi)))))
  (it "several roots each get a repo header and their own full section stack"
      (let [a
            (init-repo!)

            b
            (init-repo!)]

        (spit (str a "/left.txt") "l\n")
        (spit (str b "/right.txt") "r\n")
        (let [repos
              (magit/load-repos [{:root a :trunk a :label "alpha" :draft? false}
                                 {:root b :trunk b :label "beta" :draft? true}])

              rows
              (magit/multi-status-rows repos #{} nil)

              headers
              (filterv #(= :repo (:kind %)) rows)]

          (expect (= 2 (count headers)))
          (expect (str/includes? (:text (first headers)) "alpha"))
          (expect (str/includes? (:text (second headers)) "beta (draft)"))
          ;; every row is root-tagged, and each repo's file shows under it only
          (expect (every? (comp some? :root) rows))
          (expect (= [a]
                     (->> rows
                          (filter #(= "left.txt" (:path %)))
                          (mapv :root))))
          (expect (= [b]
                     (->> rows
                          (filter #(= "right.txt" (:path %)))
                          (mapv :root)))))))
  (it "a non-repo root renders 'Not a git repository' under its header"
      (let [a
            (init-repo!)

            plain
            (temp-dir!)

            repos
            (magit/load-repos [{:root a :trunk a :label "alpha" :draft? false}
                               {:root plain :trunk plain :label "plain" :draft? false}])

            rows
            (magit/multi-status-rows repos #{} nil)

            idx
            (first (keep-indexed #(when (and (= :repo (:kind %2)) (= plain (:root %2))) %1) rows))]

        (expect (some? idx))
        (expect (= "Not a git repository" (:text (nth rows (inc idx)))))))
  (it "expanded diffs are scoped per repo even for identical relative paths"
      (let [a
            (init-repo!)

            b
            (init-repo!)]

        (spit (str a "/a.txt") "one\nA-EDIT\n")
        (spit (str b "/a.txt") "one\nB-EDIT\n")
        (let [repos
              (magit/load-repos [{:root a :trunk a :label "alpha" :draft? false}
                                 {:root b :trunk b :label "beta" :draft? false}])

              diff-fn
              (fn [row]
                (magit/file-diff-lines (:root row) row))

              rows
              (magit/multi-status-rows repos #{[a :unstaged "a.txt"]} diff-fn)

              diffs
              (filterv #(= :diff (:kind %)) rows)]

          (expect (seq diffs))
          (expect (every? #(= a (:root %)) diffs))
          (expect (some #(str/includes? (:text %) "A-EDIT") diffs))
          (expect (not-any? #(str/includes? (:text %) "B-EDIT") diffs)))))
  (it
    "section-of never bleeds one repo's files into another's section"
    (let [a
          (init-repo!)

          b
          (init-repo!)]

      (spit (str a "/a.txt") "one\nA\n")
      (spit (str b "/a.txt") "one\nB\n")
      (let [repos
            (magit/load-repos [{:root a :trunk a :label "alpha" :draft? false}
                               {:root b :trunk b :label "beta" :draft? false}])

            rows
            (magit/multi-status-rows repos #{} nil)

            section-idxs
            (keep-indexed #(when (and (= :section (:kind %2)) (= :unstaged (:area %2))) %1) rows)]

        (expect (= 2 (count section-idxs)))
        (doseq [idx section-idxs]
          (let [files (magit/section-of rows idx)]
            (expect (= 1 (count files)))
            (expect (= (:root (nth rows idx)) (:root (first files)))))))))
  (it "repo header rows are selectable so whole-repo verbs can target them"
      (expect (magit/selectable? (magit/repo-row {:label "x" :root "/x" :draft? false})))))

(defdescribe
  draft-workspace-magit-test
  (it
    "a draft session's buffer shows CLONE state and verbs never touch the trunk"
    (let [trunk
          (init-repo!)

          store
          (temp-dir!)

          clone
          (str store "/proj")]

      ;; fork the trunk the way a draft backend does: a full copy
      (git-run! store "clone" "-q" trunk clone)
      (git-run! clone "config" "user.email" "test@vis.dev")
      (git-run! clone "config" "user.name" "Vis Test")
      ;; the DRAFT edit exists only in the clone
      (spit (str clone "/a.txt") "one\nDRAFT\n")
      (let [ws
            {:root clone :repo_root trunk :draft? true :fork_ms 1 :filesystem_roots []}

            roots
            (magit/workspace-roots ws nil)

            repos
            (magit/load-repos roots)

            rows
            (magit/multi-status-rows repos #{} nil)]

        ;; buffer reads the clone: the draft edit is visible
        (expect (= [clone] (mapv :root roots)))
        (expect (true? (:draft? (first roots))))
        (expect (some #(and (= :file (:kind %)) (= "a.txt" (:path %))) rows))
        ;; the trunk stays clean before AND after acting on the draft
        (expect (empty? (:unstaged (magit/status-model trunk))))
        (expect (:ok? (magit/stage-file! clone "a.txt")))
        (expect (= ["a.txt"] (mapv :path (:staged (magit/status-model clone)))))
        (expect (empty? (:staged (magit/status-model trunk))))
        (expect (empty? (:unstaged (magit/status-model trunk)))))))
  (it
    "a draft with an extra filesystem root shows BOTH clones, never the trunks"
    (let [trunk-a
          (init-repo!)

          trunk-b
          (init-repo!)

          store
          (temp-dir!)

          clone-a
          (str store "/a")

          clone-b
          (str store "/b")]

      (git-run! store "clone" "-q" trunk-a clone-a)
      (git-run! store "clone" "-q" trunk-b clone-b)
      (spit (str clone-a "/a.txt") "one\nDRAFT-A\n")
      (spit (str clone-b "/fresh.txt") "DRAFT-B\n")
      (let [ws
            {:root clone-a
             :repo_root trunk-a
             :draft? true
             :fork_ms 1
             :filesystem_roots [{:trunk trunk-b :clone clone-b :fork_ms 1}]}

            roots
            (magit/workspace-roots ws nil)

            rows
            (magit/multi-status-rows (magit/load-repos roots) #{} nil)]

        (expect (= [clone-a clone-b] (mapv :root roots)))
        (expect (every? :draft? roots))
        ;; both drafts' changes render, each under its own repo header
        (expect (= [clone-a]
                   (->> rows
                        (filter #(= "a.txt" (:path %)))
                        (mapv :root))))
        (expect (= [clone-b]
                   (->> rows
                        (filter #(= "fresh.txt" (:path %)))
                        (mapv :root))))
        ;; and neither trunk leaked into the buffer
        (expect (not-any? #(= trunk-a (:root %)) rows))
        (expect (not-any? #(= trunk-b (:root %)) rows))))))
