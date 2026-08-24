(ns com.blockether.vis.internal.foundation.workspace-slashes-test
  "Filesystem-root slash command tests."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.workspace-slashes :as ws-slashes]
            [com.blockether.vis.internal.paths :as paths]
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

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- env-with
  [store]
  {:extensions (atom [(extension/extension {:ext/name "test.workspace-slashes"
                                            :ext/description "Workspace slash specs under test."
                                            :ext/slash-commands ws-slashes/specs})])
   :db-info store})

(defn- pin-session!
  [store workspace-id]
  (let [ds
        (:datasource store)

        sid
        (str (java.util.UUID/randomUUID))

        state-id
        (str (java.util.UUID/randomUUID))]

    (jdbc/execute! ds
                   ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)" sid "tui"
                    1])
    (jdbc/execute! ds
                   [(str "INSERT INTO session_state "
                         "(id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES (?,?,?,?,?)") state-id sid workspace-id 0 1])
    state-id))

(defn- dispatch!
  [env store state-id line]
  (slash/dispatch env
                  {:channel/id :tui :session/id "soul" :session/state-id state-id :db-info store}
                  line))

(defdescribe specs-shape-test
             (it "exposes only /cd and no human draft commands"
                 (expect (= ["cd"] (mapv :slash/name ws-slashes/specs)))
                 (expect (nil? (slash/slash-by-path (env-with nil) ["draft"])))
                 (expect (= 1 (count (slash/active-slashes (env-with nil))))))
             (it "/cd is available in every channel"
                 (expect (nil? (:slash/availability-fn (first ws-slashes/specs))))))

(defdescribe dispatch-root-test
             (it "/cd <path> repoints the session's primary filesystem root"
                 (let [a
                       (temp-dir "vis-slash-root-a")

                       b
                       (temp-dir "vis-slash-root-b")]

                   (try (with-store
                          (fn [store]
                            (let [trunk
                                  (workspace/create-trunk-at! store a)

                                  state-id
                                  (pin-session! store (:id trunk))

                                  out
                                  (dispatch! (env-with store) store state-id (str "/cd " b))]

                              (expect (= :ok (get-in out [:result :slash/status])))
                              (expect (= (workspace/normalize-root b)
                                         (:root (workspace/for-session store state-id)))))))
                        (finally (delete-tree! a) (delete-tree! b)))))
             (it "bare /cd reports the current root without changing it"
                 (let [a (temp-dir "vis-slash-root-show")]
                   (try (with-store (fn [store]
                                      (let [trunk (workspace/create-trunk-at! store a)
                                            state-id (pin-session! store (:id trunk))
                                            out (dispatch! (env-with store) store state-id "/cd")]

                                        (expect (= :ok (get-in out [:result :slash/status])))
                                        (expect (= (:id trunk)
                                                   (:id (workspace/for-session store state-id)))))))
                        (finally (delete-tree! a))))))

(defdescribe expand-home-test
             (it "expands a bare ~ to the user's home directory"
                 (expect (= (System/getProperty "user.home") (paths/expand-home "~"))))
             (it "expands a leading ~/ prefix and keeps the rest of the path"
                 (expect (= (str (System/getProperty "user.home")
                                 java.io.File/separator
                                 "code"
                                 java.io.File/separator
                                 "proj")
                            (paths/expand-home "~/code/proj"))))
             (it "passes an absolute path through untouched"
                 (expect (= "/tmp/somewhere" (paths/expand-home "/tmp/somewhere"))))
             (it "does not expand a ~ that is not the leading segment"
                 (expect (= "/a/~b" (paths/expand-home "/a/~b"))))
             (it "is nil-safe and leaves paths unchanged without a home"
                 (expect (nil? (paths/expand-home nil "/home/test")))
                 (expect (= "~/x" (paths/expand-home "~/x" nil))))
             (it "does not treat ~user as the current user's home"
                 (expect (= "~other/x" (paths/expand-home "~other/x" "/home/test")))))
