(ns com.blockether.vis.ext.foundation-core.workspace-slashes-test
  "Declarative `/draft …` slash tree.

   Tests build a registry env containing foundation-core's slash specs,
   then dispatch through the engine's `slash/dispatch` surface and
   assert the envelopes. The slash specs under test are pure data
   (no atom, no register-slash!).

   Dispatch tests use a real rift clone of a tiny temp dir (instant on
   CoW filesystems) and abandon the clone in `finally`, so ~/.rifts
   stays clean."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.foundation-core.workspace-slashes :as ws-slashes]
   [com.blockether.vis.ext.persistance-sqlite.core :as ps]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.slash :as slash]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]
   [next.jdbc :as jdbc]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath
    (.toFile
      (java.nio.file.Files/createTempDirectory
        prefix
        (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- with-store [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- env-with
  "Build a slash env carrying foundation-core's draft specs."
  [store]
  {:extensions (atom
                 [(extension/extension
                    {:ext/name           "test.draft-slashes"
                     :ext/description    "Draft slash specs under test."
                     :ext/slash-commands ws-slashes/specs})])
   :db-info store})

(defn- seed-workspace!
  "Insert a lightweight 'current' workspace row rooted at `base` (no
   clone), so `/draft` forks from a tiny tree instead of the whole repo."
  [store base]
  (let [id (str (java.util.UUID/randomUUID))]
    (ps/db-workspace-insert! store
      {:id        id
       :repo-id   "test"
       :repo-root base
       :kind      :branch
       :branch    (str "seed-" (subs id 0 8))
       :root      base
       :state     :active
       :commit-id "0"})))

(defn- pin-session!
  "Create a minimal session_soul + session_state pinned to `workspace-id`.
   Returns the session-state id (`:session/state-id` for the handlers)."
  [store workspace-id]
  (let [ds  (:datasource store)
        sid (str (java.util.UUID/randomUUID))
        st  (str (java.util.UUID/randomUUID))]
    (jdbc/execute! ds ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)"
                       sid "tui" 1])
    (jdbc/execute! ds [(str "INSERT INTO session_state "
                         "(id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES (?,?,?,?,?)")
                       st sid workspace-id 0 1])
    st))

(defn- dispatch! [env store state-id line]
  (slash/dispatch env
    {:channel/id       :tui
     :session/id       "soul"
     :session/state-id state-id
     :db-info          store}
    line))

;; =============================================================================
;; Specs shape
;; =============================================================================

(defdescribe specs-shape-test
  (it "exposes 4 slash specs (parent /draft + 3 subcommands)"
    (expect (= 4 (count ws-slashes/specs))))

  (it "every subcommand is under `:slash/parent [\"draft\"]`"
    (let [subs (filter #(= ["draft"] (:slash/parent %)) ws-slashes/specs)]
      (expect (= 3 (count subs)))
      (expect (= #{"apply" "list" "abandon"}
                (set (map :slash/name subs))))))

  (it "registered through `:ext/slash-commands` without path collisions"
    (let [env (env-with nil)]
      (expect (= 4 (count (slash/active-slashes env))))
      (expect (some? (slash/slash-by-path env ["draft" "apply"]))))))

;; =============================================================================
;; Dispatch — `/draft` fork, list, apply
;; =============================================================================

(defdescribe dispatch-draft-flow-test
  (it "/draft [label] forks a clone, list + apply operate on it"
    (let [base (temp-dir "vis-draft-flow")]
      (try
        (spit (io/file base "seed.txt") "seed\n")
        (with-store
          (fn [store]
            (binding [workspace/*workspace-root* base]
              (let [seed     (seed-workspace! store base)
                    state-id (pin-session! store (:id seed))
                    env      (env-with store)
                    drafted  (dispatch! env store state-id "/draft my-feature")
                    draft-id (get-in drafted [:result :slash/data :workspace-id])]
                (try
                  ;; fork
                  (expect (= :ok (get-in drafted [:result :slash/status])))
                  (expect (some? draft-id))
                  (expect (= "my-feature" (get-in drafted [:result :slash/data :label])))
                  ;; the session is now repointed onto the new draft
                  (expect (= draft-id (:id (workspace/for-session store state-id))))
                  ;; list
                  (let [listed (dispatch! env store state-id "/draft list")]
                    (expect (= :ok (get-in listed [:result :slash/status]))))
                  ;; apply — nothing edited since fork, lands 0
                  (let [applied (dispatch! env store state-id "/draft apply")]
                    (expect (= :ok (get-in applied [:result :slash/status])))
                    (expect (= 0 (get-in applied [:result :slash/data :landed]))))
                  (finally
                    (when draft-id
                      (try (workspace/abandon! store {:workspace-id draft-id})
                        (catch Throwable _ nil)))))))))
        (finally (delete-tree! base)))))

  (it "/draft apply lands an edit made in the clone"
    (let [base (temp-dir "vis-draft-apply")]
      (try
        (spit (io/file base "a.txt") "original\n")
        (with-store
          (fn [store]
            (binding [workspace/*workspace-root* base]
              (let [seed     (seed-workspace! store base)
                    state-id (pin-session! store (:id seed))
                    env      (env-with store)
                    drafted  (dispatch! env store state-id "/draft edit")
                    draft-id (get-in drafted [:result :slash/data :workspace-id])
                    ws       (workspace/get store draft-id)]
                (try
                  ;; edit a file inside the clone AFTER the fork
                  (Thread/sleep 8)
                  (spit (io/file (:root ws) "a.txt") "EDITED\n")
                  (let [applied (dispatch! env store state-id "/draft apply")]
                    (expect (= :ok (get-in applied [:result :slash/status])))
                    (expect (= 1 (get-in applied [:result :slash/data :landed])))
                    (expect (= "EDITED\n" (slurp (io/file base "a.txt")))))
                  (finally
                    (when draft-id
                      (try (workspace/abandon! store {:workspace-id draft-id})
                        (catch Throwable _ nil)))))))))
        (finally (delete-tree! base))))))
