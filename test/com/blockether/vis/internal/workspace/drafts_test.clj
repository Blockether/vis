(ns com.blockether.vis.internal.workspace.drafts-test
  "The draft lifecycle boundary: `create!` picks the backend, `approve!` lands a
   draft on its `vis/<label>` branch without touching the trunk checkout,
   `discard!` keeps that branch, and every step runs through the `:draft/*`
   op hooks. Real git repositories under a temp dir; the drafts store is
   rebound so ~/.vis is never touched."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.workspace.core :as ws]
            [com.blockether.vis.internal.workspace.drafts :as drafts]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (.delete ^java.io.File f)))

(defn- git!
  "Run git in `root`; the trimmed output, or a throw naming the failure."
  [root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb (io/file root))
    (.redirectErrorStream pb true)
    (let [p (.start pb)
          out (slurp (.getInputStream p))
          exit (.waitFor p)]

      (when-not (zero? exit) (throw (ex-info "git failed" {:args args :out out})))
      (.trim ^String out))))

(defn- init-repo!
  "A real repository with one commit and a pending tracked edit plus an
   untracked file, so a draft has something to carry and to approve."
  [root]
  (git! root "init" "-q" "-b" "main")
  (git! root "config" "user.name" "Vis Test")
  (git! root "config" "user.email" "vis-test@example.invalid")
  (git! root "config" "commit.gpgsign" "false")
  (spit (io/file root "a.txt") "x\n")
  (git! root "add" "a.txt")
  (git! root "commit" "-q" "-m" "init")
  (spit (io/file root "a.txt") "x\npending\n")
  (spit (io/file root "new.txt") "untracked\n"))

(defn- with-repo
  "Run `(f store base env)` against a fresh repo, an in-memory store and a
   throwaway drafts home; everything is deleted afterwards."
  [prefix f]
  (let [base
        (temp-dir prefix)

        store
        (assoc (ps/db-open! :memory) :backend :sqlite)]

    (try (init-repo! base)
         (binding [ws/*drafts-home* (str base "-store")]
           (f store base {:db-info store :session-id "sess-drafts"}))
         (finally (ps/db-close! store) (delete-tree! (str base "-store")) (delete-tree! base)))))

(defn- seed-trunk!
  [store base]
  (ps/db-workspace-insert!
    store
    {:id (str (random-uuid)) :repo-id "rt" :repo-root base :root base :state :active :fork-ms 1}))

(defn- wait-until
  "True once `pred` holds, polling for at most five seconds; clone removal
   after a discard runs in the background."
  [pred]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (loop []

      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 50) (recur))))))

(defn- rift-available?
  [base]
  (boolean (some #(and (= :rift (:backend %)) (:available? %))
                 (ws/workspace-capability-matrix base))))

(defn- approve-roundtrip!
  "Create, approve twice and discard one draft; the observations each step
   makes, for either backend."
  [store base env]
  (let [seed
        (seed-trunk! store base)

        draft
        (drafts/create! env {:session-state-id (str (random-uuid)) :label "feature x" :from seed})

        droot
        (:root draft)

        worktree-listed?
        (str/includes? (git! base "worktree" "list") droot)

        carried?
        (and (= "x\npending\n" (slurp (io/file droot "a.txt"))) (.exists (io/file droot "new.txt")))

        _
        (spit (io/file droot "b.txt") "added in the draft\n")

        first-pass
        (drafts/approve! env {:workspace-id (:id draft) :message "feat: land b"})

        second-pass
        (drafts/approve! env {:workspace-id (:id draft)})

        branch
        (:branch first-pass)

        subject
        (git! base "log" "-1" "--format=%s" branch)

        body
        (git! base "log" "-1" "--format=%b" branch)

        landed
        (str/split-lines (git! base "show" "--name-only" "--format=" branch))

        trunk-head-subject
        (git! base "log" "-1" "--format=%s" "HEAD")

        status
        (drafts/status draft)

        _
        (drafts/discard! env {:workspace-id (:id draft) :reason "test"})

        removed?
        (wait-until #(not (.exists (io/file droot))))]

    {:draft draft
     :worktree-listed? worktree-listed?
     :carried? carried?
     :first first-pass
     :second second-pass
     :branch branch
     :subject subject
     :body body
     :landed (set landed)
     :trunk-head-subject trunk-head-subject
     :trunk-pending (slurp (io/file base "a.txt"))
     :status status
     :discarded-state (:state (ws/get store (:id draft)))
     :branch-after-discard (git! base "branch" "--list" branch)
     :root-after-discard (not removed?)}))

(defn- expect-roundtrip!
  [{:keys [draft carried? first second branch subject body landed trunk-head-subject trunk-pending
           status discarded-state branch-after-discard root-after-discard]}]
  (expect (= "feature-x" (:label draft)))
  (expect carried?)
  (expect (= :approved (:status first)))
  (expect (= "vis/feature-x" branch))
  (expect (= #{"a.txt" "new.txt" "b.txt"} (set (:files first))))
  (expect (= "feat: land b" subject))
  (expect (str/includes? body "Vis-Session: sess-drafts"))
  (expect (str/includes? body "Vis-Draft: feature-x"))
  (expect (= #{"a.txt" "new.txt" "b.txt"} landed))
  (expect (= :nothing-to-approve (:status second)))
  ;; the trunk checkout is untouched: same HEAD, same pending edit
  (expect (= "init" trunk-head-subject))
  (expect (= "x\npending\n" trunk-pending))
  (expect (= 1 (:ahead status)))
  (expect (= 0 (:pending status)))
  (expect (= :discarded discarded-state))
  ;; discarding keeps the approved branch and removes the working copy
  (expect (str/includes? branch-after-discard "vis/feature-x"))
  (expect (false? root-after-discard)))

(defdescribe
  approve-roundtrip-test
  (it
    "worktree: creates a vis/<label> worktree carrying pending work, lands one commit per approve, keeps the branch after discard"
    (with-repo "vis-drafts-wt"
               (fn [store base env]
                 (binding [ws/*draft-backend* :worktree]
                   (let [out (approve-roundtrip! store base env)]
                     (expect (= :worktree (:workspace-backend (:draft out))))
                     (expect (:worktree-listed? out))
                     (expect-roundtrip! out)))))))

(it "rift: the clone commits on vis/<label> and the trunk repository fetches the branch"
    (with-repo "vis-drafts-rift"
               (fn [store base env]
                 ;; Rift needs its native library; a host without it cannot exercise this backend.
                 (if (rift-available? base)
                   (binding [ws/*draft-backend* :rift]
                     (let [out (approve-roundtrip! store base env)]
                       (expect (= :rift (:workspace-backend (:draft out))))
                       (expect-roundtrip! out)))
                   (expect (not (rift-available? base)))))))

(it "auto prefers the worktree backend inside a committed git repository"
    (with-repo "vis-drafts-auto"
               (fn [_store base _env]
                 (binding [ws/*draft-backend* :auto]
                   (expect (= :worktree (ws/draft-backend-for base)))
                   (expect (ws/isolated-workspaces-supported? base))))))

(it "clean drafts seed from HEAD and leave pending trunk work behind"
    (with-repo "vis-drafts-clean"
               (fn [store base env]
                 (binding [ws/*draft-backend* :worktree]
                   (let [draft (drafts/create! env
                                               {:session-state-id (str (random-uuid))
                                                :label "clean"
                                                :from (seed-trunk! store base)
                                                :clean? true})]
                     (expect (= "x\n" (slurp (io/file (:root draft) "a.txt"))))
                     (expect (false? (.exists (io/file (:root draft) "new.txt"))))
                     (expect (= :nothing-to-approve
                                (:status (drafts/approve! env {:workspace-id (:id draft)}))))
                     ;; what the checkout left out is not an agent deletion: apply! keeps
                     ;; trunk's untracked file and its pending edit
                     (spit (io/file (:root draft) "c.txt") "made\n")
                     (let [{:keys [changed]} (ws/apply! store {:workspace-id (:id draft)})]
                       (expect (= [["c.txt" :add]] (mapv (juxt :path :status) changed)))
                       (expect (= "untracked\n" (slurp (io/file base "new.txt"))))
                       (expect (= "x\npending\n" (slurp (io/file base "a.txt"))))))))))

;; Regression: a worktree never receives what trunk ignores, and reading that
  ;; absence as a deletion would erase the user's local files on apply.
(it "apply! never deletes trunk's ignored files, which no worktree draft receives"
    (with-repo "vis-drafts-ignored"
               (fn [store base env]
                 (spit (io/file base ".gitignore") "secret.env\n")
                 (spit (io/file base "secret.env") "TOKEN=1\n")
                 (git! base "add" ".gitignore")
                 (git! base "commit" "-q" "-m" "ignore")
                 (binding [ws/*draft-backend* :worktree]
                   (let [draft (drafts/create! env
                                               {:session-state-id (str (random-uuid))
                                                :label "ignored"
                                                :from (seed-trunk! store base)})]
                     (expect (false? (.exists (io/file (:root draft) "secret.env"))))
                     (spit (io/file (:root draft) "new.txt") "edited in the draft\n")
                     (.delete (io/file (:root draft) "a.txt"))
                     (let [{:keys [changed]} (ws/apply! store {:workspace-id (:id draft)})]
                       (expect (= {"new.txt" :modify "a.txt" :delete}
                                  (into {} (map (juxt :path :status)) changed)))
                       (expect (= "TOKEN=1\n" (slurp (io/file base "secret.env"))))
                       (expect (false? (.exists (io/file base "a.txt"))))))))))

(defdescribe
  draft-op-hooks-test
  (it
    "an around hook on :draft/approve can veto the landing and an after hook observes it"
    (with-repo
      "vis-drafts-hooks"
      (fn [store base env]
        (let [seen (atom [])]
          (try (extension/register-op-hook! {:op :draft/approve
                                             :phase :around
                                             :owner :ext/drafts-test
                                             :fn (fn [_env _op args next]
                                                   (if (= "veto me" (:message (first args)))
                                                     (extension/failure {:error {:message
                                                                                 "policy says no"}})
                                                     (next args)))})
               (extension/register-op-hook! {:op :draft/approve
                                             :phase :after
                                             :owner :ext/drafts-test
                                             :fn (fn [_env op _args result]
                                                   (swap! seen conj [op (:status (:result result))])
                                                   result)})
               (binding [ws/*draft-backend* :worktree]
                 (let [draft (drafts/create! env
                                             {:session-state-id (str (random-uuid))
                                              :label "guarded"
                                              :from (seed-trunk! store base)})
                       vetoed (try (drafts/approve! env
                                                    {:workspace-id (:id draft) :message "veto me"})
                                   nil
                                   (catch clojure.lang.ExceptionInfo e
                                     (assoc (ex-data e) :message (ex-message e))))
                       allowed (drafts/approve! env {:workspace-id (:id draft) :message "fine"})]

                   (expect (= :draft/blocked (:type vetoed)))
                   (expect (= "policy says no" (:message vetoed)))
                   ;; nothing landed while vetoed: the first real commit is the allowed one
                   (expect (= :approved (:status allowed)))
                   (expect (= "fine" (git! base "log" "-1" "--format=%s" (:branch allowed))))
                   (expect (= 1
                              (parse-long
                                (git! base "rev-list" "--count" (str "HEAD.." (:branch allowed))))))
                   ;; after hooks observe every outcome, the refused one included
                   (expect (= [[:draft/approve nil] [:draft/approve :approved]] @seen))))
               (finally (extension/unregister-op-hooks-for-owner! :ext/drafts-test))))))))

(defdescribe draft-backend-off-test
             (it "the off setting refuses to create drafts and says which toggle to flip"
                 (with-repo
                   "vis-drafts-off"
                   (fn [store base env]
                     (binding [ws/*draft-backend* :off]
                       (expect (nil? (ws/draft-backend-for base)))
                       (expect (false? (ws/isolated-workspaces-supported? base)))
                       (expect (str/includes? (ws/isolation-unavailable-hint base) "draft_backend"))
                       (let [thrown (try (drafts/create! env
                                                         {:session-state-id (str (random-uuid))
                                                          :label "nope"
                                                          :from (seed-trunk! store base)})
                                         nil
                                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                         (expect (= :workspace/drafts-disabled (:type thrown)))))))))
