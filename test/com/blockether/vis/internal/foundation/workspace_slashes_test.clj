(ns com.blockether.vis.internal.foundation.workspace-slashes-test
  "Declarative `/draft …` slash tree (1:1 — a session has exactly one
   active draft).

   Tests build a registry env carrying foundation-core's slash specs,
   then dispatch through `slash/dispatch` and assert the envelopes.
   `/draft` clones cwd, so dispatch tests point `user.dir` at a tiny temp
   tree (instant on CoW filesystems) and abandon created clones in
   `finally` so ~/.rifts stays clean."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.workspace-slashes :as ws-slashes]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
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
(defn- delete-tree! [root] (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true)))
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
  {:extensions (atom [(extension/extension {:ext/name "test.draft-slashes",
                                            :ext/description "Draft slash specs under test.",
                                            :ext/slash-commands ws-slashes/specs})]),
   :db-info store})
(defn- seed-workspace!
  "Lightweight workspace row rooted at `base` (no clone) to pin the
   session before the real draft is minted."
  [store base]
  (let [id (str (java.util.UUID/randomUUID))]
    (ps/db-workspace-insert!
      store
      {:id id, :repo-id "test", :repo-root base, :root base, :state :active, :fork-ms 0})))
(defn- pin-session!
  [store workspace-id]
  (let [ds (:datasource store)
        sid (str (java.util.UUID/randomUUID))
        st (str (java.util.UUID/randomUUID))]
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
    {:channel/id :tui, :session/id "soul", :session/state-id state-id, :db-info store}
    line))
;; =============================================================================
;; Specs shape
;; =============================================================================
(defdescribe specs-shape-test
  (it "exposes 5 slash specs (/draft + 3 subcommands, /dir)"
    (expect (= 5 (count ws-slashes/specs))))
  (it "subcommands are new + apply + abandon under `:slash/parent [\"draft\"]`"
    (let [subs (filter #(= ["draft"] (:slash/parent %)) ws-slashes/specs)]
      (expect (= 3 (count subs)))
      (expect (= #{"new" "apply" "abandon"} (set (map :slash/name subs))))))
  (it "registered through `:ext/slash-commands` without path collisions"
    (let [env (env-with nil)]
                    ;; 5 specs: /draft + 3 subcommands + /dir. active-slashes is
                    ;; pure aggregation (no synthetic nodes); count == spec count.
      (expect (= 5 (count (slash/active-slashes env))))
      (expect (some? (slash/slash-by-path env ["draft" "apply"]))))))
;; =============================================================================
;; Dispatch
;; =============================================================================
(defn- setup!
  "Seed + pin a session, then mint a real draft (clone of `base`) as its
   active draft. Returns [env state-id draft]."
  [store base]
  (let [seed (seed-workspace! store base)
        state-id (pin-session! store (:id seed))
        env (env-with store)
        draft (workspace/create! store {:session-state-id state-id})]
    [env state-id draft]))
(defdescribe dispatch-apply-test
  (it "/draft apply lands edits AND deletions made in the draft"
    (let [base (temp-dir "vis-draft-apply")]
      (try (spit (io/file base "a.txt") "original\n")
        (spit (io/file base "gone.txt") "remove me\n")
        (with-cwd
          base
          (fn []
            (with-store
              (fn [store]
                (let [[env state-id draft] (setup! store base)]
                  (try (Thread/sleep 8)
                    (spit (io/file (:root draft) "a.txt") "EDITED\n")
                    (io/delete-file (io/file (:root draft) "gone.txt"))
                    (let [out (dispatch! env store state-id "/draft apply")]
                      (expect (= :ok (get-in out [:result :slash/status])))
                      (expect (= 2 (get-in out [:result :slash/data :landed])))
                      (expect (= "EDITED\n" (slurp (io/file base "a.txt"))))
                      (expect (not (.exists (io/file base "gone.txt")))))
                    (finally
                      (try (workspace/abandon! store {:workspace-id (:id draft)})
                        (catch Throwable _ nil)))))))))
        (finally (delete-tree! base))))))
(defdescribe dispatch-abandon-test
  (it "/draft abandon discards the draft and pins a fresh one"
    (let [base (temp-dir "vis-draft-abandon")]
      (try (spit (io/file base "seed.txt") "seed\n")
        (with-cwd
          base
          (fn []
            (with-store
              (fn [store]
                (let [[env state-id draft] (setup! store base)
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
                    (expect (= :discarded
                              (:state (workspace/get store (:id draft)))))
                    (finally
                                         ;; abandon! already trashed the draft's clone; this is
                                         ;; a belt-and-braces no-op if the clone is already
                                         ;; gone.
                      (try (workspace/abandon! store {:workspace-id (:id draft)})
                        (catch Throwable _ nil)))))))))
        (finally (delete-tree! base))))))
(defdescribe rift-gating-test
  (it "/draft specs present when rift-supported? is true"
    (with-redefs [workspace/rift-supported? (constantly true)]
      (let [names (set (map :slash/name ((var ws-slashes/build-specs))))]
        (expect (contains? names "draft"))
        (expect (= #{"abandon" "new" "apply" "draft" "dir"} names)))))
  (it "/draft specs omitted on an unsupported FS — only /dir remains"
    (with-redefs [workspace/rift-supported? (constantly false)]
      (let [names (set (map :slash/name ((var ws-slashes/build-specs))))]
        (expect (not (contains? names "draft")))
        (expect (= #{"dir"} names))))))
