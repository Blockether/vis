(ns com.blockether.vis.ext.foundation-core.workspace-slashes-test
  "PLAN.md §12 step 6 — declarative `/workspace …` slash tree.

   Tests live at the unit level: build a registry env containing
   foundation-core's slash specs, then dispatch through the engine's
   slash/dispatch surface and assert the envelopes. The slash specs
   under test are pure data (no atom, no register-slash!)."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
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

(defn- git!
  [dir args]
  (let [argv   (into ["git" "-C" (.getCanonicalPath (io/file dir))] args)
        result (apply sh/sh argv)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/join " " argv))
               (assoc result :argv argv))))
    (str/trim (or (:out result) ""))))

(defn- init-repo!
  [root]
  (.mkdirs (io/file root))
  (git! root ["init"])
  (git! root ["config" "user.name"  "Vis Test"])
  (git! root ["config" "user.email" "vis-test@example.invalid"])
  (spit (io/file root "seed.txt") "seed\n")
  (git! root ["add" "seed.txt"])
  (git! root ["commit" "-m" "seed"]))

(defn- with-store [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- env-with
  "Build a slash env carrying foundation-core's workspace specs."
  [store]
  {:extensions (atom
                 [(extension/extension
                    {:ext/name           "test.workspace-slashes"
                     :ext/description    "Workspace slash specs under test."
                     :ext/slash-commands ws-slashes/specs})])
   :db-info store})

(defn- pin-session!
  "Create a minimal session_soul + session_state row and pin it to
   `workspace-id`. Returns the session-state id (the value the slash
   handlers expect at `:session/state-id`)."
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

;; =============================================================================
;; Specs shape
;; =============================================================================

(defdescribe specs-shape-test
  (it "exposes 8 slash specs (parent /workspace + 7 subcommands)"
    (expect (= 8 (count ws-slashes/specs))))

  (it "every subcommand is under `:slash/parent [\"workspace\"]`"
    (let [subs (filter #(= ["workspace"] (:slash/parent %)) ws-slashes/specs)]
      (expect (= 7 (count subs)))
      (expect (= #{"new" "commit" "apply" "discard" "list" "switch" "label"}
                (set (map :slash/name subs))))))

  (it "registered through `:ext/slash-commands` without path collisions"
    (let [env (env-with nil)]
      (expect (= 8 (count (slash/active-slashes env))))
      (expect (some? (slash/slash-by-path env ["workspace" "commit"]))))))

;; =============================================================================
;; Dispatch — `/workspace new` and friends
;; =============================================================================

(defdescribe dispatch-workspace-new-test
  (it "creates a branch workspace pinned to the slash session"
    (let [base (temp-dir "vis-ws-slashes-new")]
      (try
        (init-repo! base)
        (with-store
          (fn [store]
            (binding [workspace/*workspace-root* base]
              (let [trunk     (workspace/ensure-trunk! store {})
                    state-id  (pin-session! store (:id trunk))
                    env       (env-with store)
                    out       (slash/dispatch env
                                {:channel/id       :tui
                                 :session/id       "soul"
                                 :session/state-id state-id
                                 :db-info          store}
                                "/workspace new my-label")]
                (expect (= :ok (get-in out [:result :slash/status])))
                (let [{:keys [workspace-id branch label]} (get-in out [:result :slash/data])]
                  (expect (some? workspace-id))
                  (expect (str/starts-with? branch "vis/"))
                  (expect (= "my-label" label)))))))
        (finally (delete-tree! base))))))

(defdescribe dispatch-workspace-list-test
  (it "lists active workspaces for the current repo"
    (let [base (temp-dir "vis-ws-slashes-list")]
      (try
        (init-repo! base)
        (with-store
          (fn [store]
            (binding [workspace/*workspace-root* base]
              (let [trunk    (workspace/ensure-trunk! store {})
                    state-id (pin-session! store (:id trunk))
                    env      (env-with store)
                    out      (slash/dispatch env
                               {:channel/id       :tui
                                :session/id       "soul"
                                :session/state-id state-id
                                :db-info          store}
                               "/workspace list")]
                (expect (= :ok (get-in out [:result :slash/status])))
                (expect (str/includes?
                          (or (get-in out [:result :slash/body]) "")
                          (:branch trunk)))))))
        (finally (delete-tree! base))))))

(defdescribe dispatch-workspace-label-test
  (it "sets label on the active workspace"
    (let [base (temp-dir "vis-ws-slashes-label")]
      (try
        (init-repo! base)
        (with-store
          (fn [store]
            (binding [workspace/*workspace-root* base]
              (let [trunk    (workspace/ensure-trunk! store {})
                    state-id (pin-session! store (:id trunk))
                    env      (env-with store)
                    out      (slash/dispatch env
                               {:channel/id       :tui
                                :session/id       "soul"
                                :session/state-id state-id
                                :db-info          store}
                               "/workspace label frontend")]
                (expect (= :ok (get-in out [:result :slash/status])))
                (expect (= "frontend" (:label (workspace/get store (:id trunk)))))))))
        (finally (delete-tree! base))))))

(defdescribe availability-test
  (it "/workspace commit hidden on trunk workspace (availability-fn=false)"
    (let [base (temp-dir "vis-ws-slashes-avail")]
      (try
        (init-repo! base)
        (with-store
          (fn [store]
            (binding [workspace/*workspace-root* base]
              (let [trunk    (workspace/ensure-trunk! store {})
                    state-id (pin-session! store (:id trunk))
                    env      (env-with store)
                    out      (slash/dispatch env
                               {:channel/id       :tui
                                :session/id       "soul"
                                :session/state-id state-id
                                :db-info          store}
                               "/workspace commit anything")]
                (expect (= :unavailable (:reason out)))))))
        (finally (delete-tree! base))))))
