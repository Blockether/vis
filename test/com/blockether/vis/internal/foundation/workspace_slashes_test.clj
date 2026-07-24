(ns com.blockether.vis.internal.foundation.workspace-slashes-test
  "Declarative `/draft …` slash tree (1:1 — a session has exactly one
   active draft).

   Tests build a registry env carrying foundation-core's slash specs,
   then dispatch through `slash/dispatch` and assert the envelopes.
   `/draft` clones cwd, so dispatch tests point `user.dir` at a tiny temp
  tree and abandon created backend workspaces in `finally`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.workspace-slashes :as ws-slashes]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.ext.workspace-rift]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.slash :as slash]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- with-cwd
  "Run `f` with JVM user.dir pointed at `base` so `/draft` (which clones
   cwd) clones the tiny temp tree, not the whole repo. Restored after."
  [base f]
  (let [orig (System/getProperty "user.dir")]
    (try (System/setProperty "user.dir" base) (f) (finally (System/setProperty "user.dir" orig)))))

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- env-with
  [store]
  {:extensions (atom [(extension/extension {:ext/name "test.draft-slashes"
                                            :ext/description "Draft slash specs under test."
                                            :ext/slash-commands ws-slashes/specs})])
   :db-info store})

(defn- seed-workspace!
  "Lightweight workspace row rooted at `base` (no clone) to pin the
   session before the real draft is minted."
  [store base]
  (let [id (str (java.util.UUID/randomUUID))]
    (ps/db-workspace-insert! store
                             {:id id
                              :repo-id "test"
                              :repo-root base
                              :root base
                              :workspace-kind :trunk
                              :workspace-backend :live
                              :state :active})))

(defn- pin-session!
  [store workspace-id]
  (let
    [ds
     (:datasource store)

     sid
     (str (java.util.UUID/randomUUID))

     st
     (str (java.util.UUID/randomUUID))]

    (jdbc/execute! ds
                   ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)" sid "tui"
                    1])
    (jdbc/execute! ds
                   [(str "INSERT INTO session_state "
                         "(id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES (?,?,?,?,?)") st sid workspace-id 0 1])
    st))

(defn- dispatch!
  [env store state-id line]
  (slash/dispatch env
                  {:channel/id :tui :session/id "soul" :session/state-id state-id :db-info store}
                  line))

(defdescribe ctx-contract-test
             (it "requires the canonical namespaced session state key"
                 (with-store (fn [store]
                               (let
                                 [env
                                  (env-with store)

                                  out
                                  (slash/dispatch env
                                                  {:channel/id :tui
                                                   :session/id "soul"
                                                   :session-state-id (str
                                                                       (java.util.UUID/randomUUID))
                                                   :db-info store}
                                                  "/draft new flat-key")]

                                 (expect (= :error (get-in out [:result :slash/status])))
                                 (expect (str/includes? (get-in out [:result :slash/title])
                                                        "session not ready")))))))
;; =============================================================================
;; Specs shape
;; =============================================================================
(defdescribe
  specs-shape-test
  (it "exposes the full slash spec set (/draft tree, /draft-blank, /cd)"
      (expect (= 9 (count ws-slashes/specs))))
  (it "exposes a TOP-LEVEL /cd (change the session's root)"
      (let [tops (filter #(nil? (:slash/parent %)) ws-slashes/specs)]
        (expect (contains? (set (map :slash/name tops)) "cd"))))
  (it "/cd carries no channel availability gate (every channel gets it)"
      (let [fs-fam (filter #(#{"cd"} (:slash/name %)) ws-slashes/specs)]
        (expect (every? #(nil? (:slash/availability-fn %)) fs-fam))))
  (it
    "subcommands are new + apply + abandon + stash + resume + list under `:slash/parent [\"draft\"]`"
    (let [subs (filter #(= ["draft"] (:slash/parent %)) ws-slashes/specs)]
      (expect (= 6 (count subs)))
      (expect (= #{"new" "apply" "abandon" "stash" "resume" "list"} (set (map :slash/name subs))))))
  (it "registered through `:ext/slash-commands` without path collisions"
      (let [env (env-with nil)]
        ;; 9 specs: /draft + new/apply/abandon/stash/resume/list + /draft-blank
        ;; + /cd. active-slashes is pure aggregation (no synthetic nodes) —
        ;; count == spec count.
        (expect (= 9 (count (slash/active-slashes env))))
        (expect (some? (slash/slash-by-path env ["draft" "apply"])))
        (expect (some? (slash/slash-by-path env ["cd"]))))))
;; =============================================================================
;; Dispatch
;; =============================================================================
(defn- setup!
  "Seed + pin a session, then mint a real draft (clone of `base`) as its
   active draft. Returns [env state-id draft]."
  [store base]
  (let
    [seed
     (seed-workspace! store base)

     state-id
     (pin-session! store (:id seed))

     env
     (env-with store)

     draft
     (workspace/create! store {:session-state-id state-id})]

    [env state-id draft]))

(defdescribe
  dispatch-apply-test
  (it "/draft apply lands edits AND deletions made in the draft"
      (let [base (temp-dir "vis-draft-apply")]
        (try (if-not (workspace/isolated-workspaces-supported? base)
               ;; No copy-on-write workspace backend in this environment (e.g. CI
               ;; without rift's native lib / a CoW filesystem) — the live draft
               ;; round-trip can't run. `capability-gating-test` covers the
               ;; unavailable path; here we just confirm it IS unavailable.
               (expect (not (workspace/isolated-workspaces-supported? base)))
               (do (spit (io/file base "a.txt") "original\n")
                   (spit (io/file base "gone.txt") "remove me\n")
                   (with-cwd base
                             (fn []
                               (with-store
                                 (fn [store]
                                   (let [[env state-id draft] (setup! store base)]
                                     (try
                                       (Thread/sleep 8)
                                       (spit (io/file (:root draft) "a.txt") "EDITED\n")
                                       (io/delete-file (io/file (:root draft) "gone.txt"))
                                       (let [out (dispatch! env store state-id "/draft apply")]
                                         (expect (= :ok (get-in out [:result :slash/status])))
                                         (expect (= 2 (get-in out [:result :slash/data :landed])))
                                         (expect (= "EDITED\n" (slurp (io/file base "a.txt"))))
                                         (expect (not (.exists (io/file base "gone.txt")))))
                                       (finally
                                         (try (workspace/abandon! store {:workspace-id (:id draft)})
                                              (catch Throwable _ nil)))))))))))
             (finally (delete-tree! base))))))

(defdescribe
  dispatch-abandon-test
  (it
    "/draft abandon discards the draft and pins a fresh one"
    (let [base (temp-dir "vis-draft-abandon")]
      (try (if-not (workspace/isolated-workspaces-supported? base)
             ;; No CoW workspace backend here (CI) — skip the live round-trip.
             (expect (not (workspace/isolated-workspaces-supported? base)))
             (do (spit (io/file base "seed.txt") "seed\n")
                 (with-cwd
                   base
                   (fn []
                     (with-store
                       (fn [store]
                         (let
                           [[env state-id draft] (setup! store base)
                            out (dispatch! env store state-id "/draft abandon not-good")
                            ;; abandon discards the draft and re-pins the session to
                            ;; a fresh active workspace (trunk) — read it off the
                            ;; session.
                            fresh (:id (workspace/for-session store state-id))]

                           (try (expect (= :ok (get-in out [:result :slash/status])))
                                (expect (= (:id draft)
                                           (get-in out [:result :slash/data :workspace-id])))
                                ;; a different, fresh workspace is now the session's
                                ;; active one
                                (expect (some? fresh))
                                (expect (not= (:id draft) fresh))
                                (expect (= :discarded (:state (workspace/get store (:id draft)))))
                                (finally
                                  ;; abandon! already trashed the draft's clone; this is
                                  ;; a belt-and-braces no-op if the clone is already
                                  ;; gone.
                                  (try (workspace/abandon! store {:workspace-id (:id draft)})
                                       (catch Throwable _ nil)))))))))))
           (finally (delete-tree! base))))))

(defdescribe
  dispatch-draft-blank-test
  (it "/draft-blank mints an EMPTY draft — trunk's files are NOT carried in"
      (let [base (temp-dir "vis-draft-blank")]
        (try (if-not (workspace/isolated-workspaces-supported? base)
               ;; No CoW workspace backend here (CI) — skip the live round-trip.
               (expect (not (workspace/isolated-workspaces-supported? base)))
               (do (spit (io/file base "seed.txt") "seed\n")
                   (with-cwd base
                             (fn []
                               (with-store
                                 (fn [store]
                                   (let
                                     [seed (seed-workspace! store base)
                                      state-id (pin-session! store (:id seed))
                                      env (env-with store)
                                      out (dispatch! env store state-id "/draft-blank scratch")
                                      draft (workspace/for-session store state-id)]

                                     (try
                                       (expect (= :ok (get-in out [:result :slash/status])))
                                       (expect (true? (get-in out [:result :slash/data :blank?])))
                                       ;; a real isolated draft is pinned to the session…
                                       (expect (workspace/draft? draft))
                                       (expect (not= base (:root draft)))
                                       ;; …that does NOT contain the file sitting on HEAD
                                       (expect (not (.exists (io/file (:root draft) "seed.txt"))))
                                       (finally
                                         (try (workspace/abandon! store {:workspace-id (:id draft)})
                                              (catch Throwable _ nil)))))))))))
             (finally (delete-tree! base)))))
  (it "/draft-blank requires a label, like /draft new"
      (with-store (fn [store]
                    (let [base (temp-dir "vis-draft-blank-nolabel")]
                      (try (let
                             [seed (seed-workspace! store base)
                              state-id (pin-session! store (:id seed))
                              env (env-with store)
                              out (dispatch! env store state-id "/draft-blank")]

                             (expect (= :error (get-in out [:result :slash/status])))
                             (expect (str/includes? (get-in out [:result :slash/title])
                                                    "/draft-blank <label>")))
                           (finally (delete-tree! base))))))))

(defdescribe
  capability-gating-test
  (it "/draft remains discoverable when no isolation backend is available"
      (with-redefs [workspace/isolated-workspaces-supported? (constantly false)]
        (let [names (set (map :slash/name ((var ws-slashes/build-specs))))]
          (expect (= #{"draft" "new" "apply" "abandon" "stash" "resume" "draft-blank" "cd" "fs"
                       "add" "remove" "list" "create"}
                     names)))))
  (it "/draft new reports the unavailable capability matrix"
      (with-store
        (fn [store]
          (let [base (temp-dir "vis-draft-unavailable")]
            (try (let
                   [seed (seed-workspace! store base)
                    state-id (pin-session! store (:id seed))
                    env (env-with store)]

                   (with-redefs
                     [workspace/isolated-workspaces-supported? (constantly false)
                      workspace/workspace-capability-matrix
                      (constantly
                        [{:backend :rift :available? false :capabilities #{:isolated-fork}}])]

                     (let [out (dispatch! env store state-id "/draft new test")]
                       (expect (= :error (get-in out [:result :slash/status])))
                       (expect (= :rift
                                  (get-in out
                                          [:result :slash/data :capability-matrix 0 :backend]))))))
                 (finally (delete-tree! base))))))))

(defdescribe dispatch-root-test
             (it "/cd <path> repoints the session's primary filesystem root"
                 (let
                   [a
                    (temp-dir "vis-slash-root-a")

                    b
                    (temp-dir "vis-slash-root-b")]

                   (try (with-store
                          (fn [store]
                            (let
                              [trunk
                               (workspace/create-trunk-at! store a)

                               state-id
                               (pin-session! store (:id trunk))

                               env
                               (env-with store)

                               out
                               (dispatch! env store state-id (str "/cd " b))]

                              (expect (= :ok (get-in out [:result :slash/status])))
                              (expect (= (workspace/normalize-root b)
                                         (:root (workspace/for-session store state-id)))))))
                        (finally (delete-tree! a) (delete-tree! b)))))
             (it "bare /cd reports the current root without changing anything"
                 (let [a (temp-dir "vis-slash-root-show")]
                   (try (with-store (fn [store]
                                      (let
                                        [trunk (workspace/create-trunk-at! store a)
                                         state-id (pin-session! store (:id trunk))
                                         env (env-with store)
                                         out (dispatch! env store state-id "/cd")]

                                        (expect (= :ok (get-in out [:result :slash/status])))
                                        (expect (= (:id trunk)
                                                   (:id (workspace/for-session store state-id)))))))
                        (finally (delete-tree! a))))))

(defdescribe
  draft-follows-current-root-test
  (it
    "/draft new forks the session's current /root, not the process launch directory"
    (let
      [launch-root
       (temp-dir "vis-draft-launch-root")

       current-root
       (temp-dir "vis-draft-current-root")]

      (try (spit (io/file launch-root "launch-only.txt") "launch\n")
           (spit (io/file current-root "current-only.txt") "current\n")
           (if-not (workspace/isolated-workspaces-supported? current-root)
             (expect (not (workspace/isolated-workspaces-supported? current-root)))
             (with-cwd
               launch-root
               (fn []
                 (with-store
                   (fn [store]
                     (let
                       [trunk
                        (workspace/create-trunk-at! store launch-root)

                        state-id
                        (pin-session! store (:id trunk))

                        env
                        (env-with store)]

                       (dispatch! env store state-id (str "/cd " current-root))
                       (let
                         [out
                          (dispatch! env store state-id "/draft new moved-root")

                          draft
                          (workspace/for-session store state-id)]

                         (try (expect (= :ok (get-in out [:result :slash/status])))
                              (expect (= (workspace/normalize-root current-root)
                                         (:repo-root draft)))
                              (expect (.exists (io/file (:root draft) "current-only.txt")))
                              (expect (not (.exists (io/file (:root draft) "launch-only.txt"))))
                              (finally (try (workspace/abandon! store {:workspace-id (:id draft)})
                                            (catch Throwable _ nil)))))))))))
           (finally (delete-tree! launch-root) (delete-tree! current-root))))))

(defdescribe
  dispatch-stash-resume-test
  (it
    "/draft stash parks the draft (kept :active) and /draft resume re-enters it"
    (let [base (temp-dir "vis-draft-stash")]
      (try
        (if-not (workspace/isolated-workspaces-supported? base)
          ;; No CoW workspace backend here (CI) — skip the live round-trip.
          (expect (not (workspace/isolated-workspaces-supported? base)))
          (do
            (spit (io/file base "seed.txt") "seed\n")
            (with-cwd
              base
              (fn []
                (with-store
                  (fn [store]
                    (let
                      [[env state-id draft] (setup! store base)
                       label (workspace/display-label draft)]

                      (try
                        ;; STASH — leaves the draft, but never discards it.
                        (let [out (dispatch! env store state-id "/draft stash")]
                          (expect (= :ok (get-in out [:result :slash/status])))
                          (expect (= (:id draft) (get-in out [:result :slash/data :workspace-id]))))
                        ;; Session is back on trunk; the draft stays :active.
                        (expect (not (workspace/draft? (workspace/for-session store state-id))))
                        (expect (= :active (:state (workspace/get store (:id draft)))))
                        ;; LIST — the stashed draft is discoverable.
                        (let
                          [out (dispatch! env store state-id "/draft list")
                           ids (map :workspace-id (get-in out [:result :slash/data :drafts]))]

                          (expect (= :ok (get-in out [:result :slash/status])))
                          (expect (some #(= (:id draft) %) ids)))
                        ;; RESUME — re-pins the session to the same draft.
                        (let [out (dispatch! env store state-id (str "/draft resume " label))]
                          (expect (= :ok (get-in out [:result :slash/status])))
                          (expect (= (:id draft) (get-in out [:result :slash/data :workspace-id])))
                          (expect (= (:id draft) (:id (workspace/for-session store state-id)))))
                        (finally (try (workspace/abandon! store {:workspace-id (:id draft)})
                                      (catch Throwable _ nil)))))))))))
        (finally (delete-tree! base))))))

;; =============================================================================
;; /cd — path helpers (~ expansion)
;; =============================================================================
(defdescribe expand-home-test
             (it "expands a bare ~ to the user's home directory"
                 (expect (= (System/getProperty "user.home") (#'ws-slashes/expand-home "~"))))
             (it "expands a leading ~/ prefix and keeps the rest of the path"
                 (expect (= (str (System/getProperty "user.home") "/code/proj")
                            (#'ws-slashes/expand-home "~/code/proj"))))
             (it "passes an absolute path through untouched"
                 (expect (= "/tmp/somewhere" (#'ws-slashes/expand-home "/tmp/somewhere"))))
             (it "does not expand a ~ that is not the leading segment"
                 (expect (= "/a/~b" (#'ws-slashes/expand-home "/a/~b")))))
