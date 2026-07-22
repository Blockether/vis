(ns com.blockether.vis.internal.gateway.workspace-roundtrip-test
  "END-TO-END guards for gateway workspace management.

   The gateway serves workspace data in THE canonical string-keyed wire shape
   (`wire/canonical`) on every transport, and `client/decode-workspace` passes it
   through VERBATIM — one representation, no re-hydration. These tests exercise
   both wire projection and daemon-owned draft lifecycle operations."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.ext.workspace-rift]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace :as ws]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]))

(def ^:private decode-workspace
  "The real, private client decoder — the seam under test."
  (deref #'client/decode-workspace))

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- pin-session!
  [store workspace-id]
  (let
    [sid
     (str (random-uuid))

     state-id
     (str (random-uuid))

     ds
     (:datasource store)]

    (jdbc/execute! ds
                   ["INSERT INTO session_soul (id, channel, created_at) VALUES (?,?,?)" sid "tui"
                    1])
    (jdbc/execute!
      ds
      ["INSERT INTO session_state (id, session_soul_id, workspace_id, version, created_at) VALUES (?,?,?,?,?)"
       state-id sid workspace-id 0 1])
    {:sid sid :state-id state-id}))

(defn- session-workspace-shape
  "The exact map `state/session-workspace-info` emits for a workspace."
  [ws-rec]
  {:id (:id ws-rec)
   :draft? (ws/draft? ws-rec)
   :root (:root ws-rec)
   :repo-root (:repo-root ws-rec)
   :label (:label ws-rec)
   :fork-ms (:fork-ms ws-rec)
   :filesystem-roots (mapv #(ws/public-filesystem-root % true) (ws/filesystem-roots ws-rec))})

(defn- hop
  "Server-encode then client-decode, exactly like the gateway HTTP boundary."
  [info]
  (decode-workspace (wire/parse-json (wire/json-str info))))

(defdescribe
  filesystem-add-survives-the-wire-test
  (it
    "an added root reaches every channel in the canonical string-keyed shape"
    (with-store
      (fn [store]
        (let
          [base
           (temp-dir "vis-rt-base")

           extra
           (temp-dir "vis-rt-extra")]

          (try
            (let
              [seed
               (ps/db-workspace-insert! store
                                        {:id (str (random-uuid))
                                         :repo-id "rt"
                                         :repo-root base
                                         :root base
                                         :state :active
                                         :fork-ms 0})

               wid
               (:id seed)]

              ;; C-a: add `extra` as an extra filesystem root (trunk session → live).
              (ws/add-filesystem-root! store wid extra)
              (let
                [info
                 (session-workspace-shape (ws/get store wid))

                 decoded
                 (hop info)

                 raw
                 (wire/parse-json (wire/json-str info))]

                ;; The decode is a VERBATIM passthrough — one canonical shape.
                (expect (= raw decoded))
                ;; Channels read these snake_case STRING keys off `@ws-info`:
                (expect (= [(ws/normalize-root extra)]
                           (mapv #(get % "dir") (get decoded "filesystem_roots"))))
                (expect (= 1 (count (get decoded "filesystem_roots"))))
                (expect (= base (get decoded "root")))
                (expect (= base (get decoded "repo_root")))
                ;; The added root is auto-isolated (fork baseline): it rides
                ;; `"isolated" true` + a `"draft_dir"` clone path — plain scalars
                ;; on the wire, never keywords.
                (expect (true? (get (first (get decoded "filesystem_roots")) "isolated")))
                (expect (string? (get (first (get decoded "filesystem_roots")) "draft_dir")))
                ;; The `?` boolean rides as `is_draft` — no kebab / `?` key survives.
                (expect (contains? decoded "is_draft"))
                (expect (nil? (get decoded "draft?")))
                (expect (nil? (:filesystem-roots decoded)))))
            (finally (delete-tree! base) (delete-tree! extra))))))))

(defn- draft-list-shape
  "The exact per-draft map `state/list-drafts` emits (before the wire hop): the
   canonical parked-draft descriptor every channel's drafts picker reads."
  [ws-rec current?]
  (wire/canonical {:workspace-id (:id ws-rec)
                   :label (ws/display-label nil ws-rec nil)
                   :root (:root ws-rec)
                   :repo-root (:repo-root ws-rec)
                   :fork-ms (:fork-ms ws-rec)
                   :current? current?}))

(defdescribe
  draft-list-survives-the-wire-test
  (it
    "a parked draft reaches every channel's picker in the canonical string shape"
    (let
      [d1
       {:id (str (random-uuid))
        :repo-id "rt"
        :repo-root "/repo"
        :root "/repo/.vis/draft-a"
        :label "feature-a"
        :fork-ms 111
        :state :active}

       d2
       {:id (str (random-uuid))
        :repo-id "rt"
        :repo-root "/repo"
        :root "/repo/.vis/draft-b"
        :label "feature-b"
        :fork-ms 222
        :state :active}

       ;; the session is currently IN d2 — it rides is_current true.
       info
       [(draft-list-shape d1 false) (draft-list-shape d2 true)]

       decoded
       (wire/parse-json (wire/json-str info))]

      ;; Verbatim passthrough — ONE canonical shape on both transports.
      (expect (= decoded info))
      ;; Channels read these snake_case STRING keys off each picker row:
      (expect (= [(:id d1) (:id d2)] (mapv #(get % "workspace_id") decoded)))
      (expect (= ["feature-a" "feature-b"] (mapv #(get % "label") decoded)))
      (expect (= [111 222] (mapv #(get % "fork_ms") decoded)))
      ;; The `?` boolean rides as `is_current` — no kebab / `?` key survives.
      (expect (= [false true] (mapv #(get % "is_current") decoded)))
      (expect (every? #(nil? (get % "current?")) decoded))
      (expect (= ["/repo" "/repo"] (mapv #(get % "repo_root") decoded))))))

(defdescribe
  gateway-draft-manager-lifecycle-test
  (it
    "creates and abandons through daemon state while rejecting foreign-repo ids"
    (with-store
      (fn [store]
        (let
          [base
           (temp-dir "vis-gateway-draft-base")

           drafts-home
           (temp-dir "vis-gateway-drafts")

           foreign-root
           (temp-dir "vis-gateway-foreign")]

          (try
            (spit (io/file base "seed.txt") "seed")
            (let
              [trunk
               (ps/db-workspace-insert! store
                                        {:repo-id "seed-repo"
                                         :repo-root base
                                         :root base
                                         :workspace-kind :trunk
                                         :workspace-backend :live
                                         :state :active})

               {:keys [sid state-id]}
               (pin-session! store (:id trunk))

               foreign
               (ps/db-workspace-insert! store
                                        {:repo-id "foreign-repo"
                                         :repo-root foreign-root
                                         :root (str foreign-root "/draft")
                                         :workspace-kind :draft
                                         :workspace-backend :rift
                                         :state :active
                                         :fork-ms 1
                                         :apply-fork-ms 1})]

              (binding [ws/*drafts-home* (io/file drafts-home)]
                (with-redefs [lp/db-info (constantly store)]
                  (let
                    [created (state/create-draft! sid "picker-created" false)
                     draft-id (get created "id")]

                    (expect (true? (get created "is_draft")))
                    (expect (= "picker-created" (get created "label")))
                    (expect (= [draft-id] (mapv #(get % "workspace_id") (state/list-drafts sid))))
                    (let [abandoned (state/abandon-draft! sid draft-id "picker cleanup")]
                      (expect (false? (get abandoned "is_draft")))
                      (expect (= :discarded (:state (ws/get store draft-id))))
                      (expect (empty? (state/list-drafts sid))))
                    (let
                      [before (:id (ws/for-session store state-id))
                       resume-error (try (state/resume-draft! sid (:id foreign))
                                         nil
                                         (catch clojure.lang.ExceptionInfo e (ex-data e)))
                       abandon-error (try (state/abandon-draft! sid (:id foreign) "wrong repo")
                                          nil
                                          (catch clojure.lang.ExceptionInfo e (ex-data e)))]

                      (expect (= :workspace/draft-repo-mismatch (:type resume-error)))
                      (expect (= :workspace/draft-repo-mismatch (:type abandon-error)))
                      (expect (= before (:id (ws/for-session store state-id)))))))))
            (finally (delete-tree! base)
                     (delete-tree! drafts-home)
                     (delete-tree! foreign-root))))))))
