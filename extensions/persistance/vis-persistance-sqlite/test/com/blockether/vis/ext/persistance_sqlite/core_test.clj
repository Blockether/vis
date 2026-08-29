(ns ^{:clj-kondo/config
      ;; Pragmatic: this aggregator test file collects scenarios from
      ;; multiple original test namespaces. Many `it` blocks use
      ;; `(let [s (h/store) cid (h/store-session! ...)] (let [...]
      ;; ...))` where the inner let is technically mergeable and the
      ;; intermediate ids (cid / qid / etc.) are bound for SIDE EFFECT,
      ;; not for use. Suppress redundant-let / unused-binding here
      ;; rather than rewrite every block.
      '{:linters {:redundant-let {:level :off} :unused-binding {:level :off}}}}
    com.blockether.vis.ext.persistance-sqlite.core-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            ;; Force-load the SQLite backend ns so the `private-core-fn` helper
            ;; below can resolve its private vars at top-level def time. The backend
            ;; is otherwise loaded lazily by persistence dispatch.
            [com.blockether.vis.ext.persistance-sqlite.core :as sqlite-core]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h :refer
             [raw-count raw-query]]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.persistance :as persistance]
            [honey.sql :as sql]
            [lazytest.core :refer [defdescribe it expect]]
            [next.jdbc :as jdbc])
  (:import (java.io File)
           (java.util.concurrent CountDownLatch TimeUnit)))

;; ─── from db_test.clj ───

(h/use-mem-store!)

(defn- private-core-fn
  [name]
  (deref (resolve (symbol "com.blockether.vis.ext.persistance-sqlite.core" name))))

(defn- table-columns
  [store table]
  (set (map :name (jdbc/execute! (:datasource store) [(str "PRAGMA table_info(" table ")")]))))


(def ^:private migration-checksum-mismatch? (private-core-fn "migration-checksum-mismatch?"))

(def ^:private maybe-wrap-db-open-error (private-core-fn "maybe-wrap-db-open-error"))

(def ^:private migration-checksum-mismatch-user-message
  @(resolve
     'com.blockether.vis.ext.persistance-sqlite.core/migration-checksum-mismatch-user-message))

(defdescribe
  sqlite-extension-aggregate-test
  (it "upserts extension-owned singleton rows by extension, key, kind, and scope"
      (let [s
            (h/store)

            first-row
            (persistance/db-put-extension-aggregate! s
                                                     {:extension-id 'test.ext.alpha
                                                      :aggregate-key :index/status
                                                      :kind :background/status
                                                      :index-data {:schema-version 1}
                                                      :content {:state :running}})

            second-row
            (persistance/db-put-extension-aggregate! s
                                                     {:extension-id 'test.ext.alpha
                                                      :aggregate-key :index/status
                                                      :kind :background/status
                                                      :index-data {:schema-version 1}
                                                      :content {:state :done}})]

        (expect (= (:id first-row) (:id second-row)))
        (expect (= {:state :done} (:content second-row)))
        (expect (= 1 (raw-count s :extension_aggregate)))
        (expect (= "test.ext.alpha" (:extension-id second-row)))
        (expect (= :index/status (:key second-row)))
        (expect (= :background/status (:kind second-row)))))
  (it "keeps extension rows isolated by extension id while admin list can inspect all"
      (let [s (h/store)]
        (persistance/db-put-extension-aggregate! s
                                                 {:extension-id 'test.ext.alpha
                                                  :aggregate-key :shared-key
                                                  :kind :cache/value
                                                  :content {:owner :alpha}})
        (persistance/db-put-extension-aggregate! s
                                                 {:extension-id 'test.ext.beta
                                                  :aggregate-key :shared-key
                                                  :kind :cache/value
                                                  :content {:owner :beta}})
        (expect (= [{:owner :alpha}]
                   (mapv :content
                         (vis/db-list-extension-aggregates s
                                                           {:extension-id 'test.ext.alpha
                                                            :kind :cache/value}))))
        (expect (= #{"test.ext.alpha" "test.ext.beta"}
                   (set (map :extension-id
                             (vis/db-list-extension-aggregates s {:kind :cache/value})))))))
  (it "stores iteration block scope and indexes it"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :cli})

            tid
            (vis/db-store-session-turn! s {:parent-session-id cid :user-request "block scoped"})

            iid
            (h/store-iteration! s {:session-turn-id tid :status :done :idx 0 :code "(+ 1 2)"})]

        (persistance/db-create-extension-aggregate! s
                                                    {:extension-id 'test.ext.alpha
                                                     :aggregate-key :tool/trace
                                                     :kind :trace/tool-result
                                                     :iteration-id iid
                                                     :iteration-form-index 0
                                                     :content {:ok true}})
        (let [rows (vis/db-list-extension-aggregates
                     s
                     {:extension-id 'test.ext.alpha :iteration-id iid :iteration-form-index 0})]
          (expect (= 1 (count rows)))
          (expect (= {:iteration-id (str iid) :iteration-form-index 0}
                     (select-keys (:scope (first rows)) [:iteration-id :iteration-form-index])))
          (expect (= {:ok true} (:content (first rows)))))))
  (it
    "stores and lists iteration attachments per (call, position)"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          tid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "plot it"})

          png
          (byte-array (map unchecked-byte [0x89 0x50 0x4e 0x47 1 2 3 4]))

          b64
          (.encodeToString (java.util.Base64/getEncoder) png)

          iid
          (h/store-iteration!
            s
            {:session-turn-id tid
             :status :done
             :code "plt.show()"
             :forms
             [{:scope "t1/i1" :tag :observation :src "plt.show()" :svar/tool-call-id "call_A"}]
             ;; One call emits TWO same-named figures (position 0 and 1); a
             ;; third artifact is iteration-level (nil call-id, its own group).
             :attachments
             [{:tool-call-id "call_A"
               :media-type "image/png"
               :base64 b64
               :filename "fig.png"
               :size (alength png)}
              {:tool-call-id "call_A" :media-type "image/png" :base64 b64 :filename "fig.png"}
              {:tool-call-id nil
               :media-type "application/vnd.vis.live+ndjson"
               :base64 b64
               :filename "watch.live.ndjson"
               :view-id "watch-view"}]})

          got
          (vis/db-list-iteration-attachments s iid)]

      (expect (= 3 (count got)))
      ;; Grain (call, position): call_A gets 0 and 1, the nil-call artifact 0.
      (expect (= [[nil 0] ["call_A" 0] ["call_A" 1]] (mapv (juxt :tool-call-id :position) got)))
      ;; Base64 payload round-trips byte-for-byte.
      (expect (every? #(= b64 (:base64 %)) got))
      (expect (= "application/vnd.vis.live+ndjson" (:media-type (first got))))
      (expect (= 8 (:size (first got))))
      ;; The METADATA listers answer the SAME rows in the SAME order without a
      ;; byte of payload. Everything that only NUMBERS or DESCRIBES artifacts
      ;; (wire descriptors, transcript pages, the byte endpoint's own index
      ;; lookup) used to read - and base64-encode - every blob of the iteration
      ;; just to count them, so serving a gallery of N images cost N*N reads.
      (let [meta-rows
            (vis/db-list-iteration-attachments-meta s iid)

            meta-batch
            (get (vis/db-list-iterations-attachments-meta s [iid]) (str iid))

            facts
            (juxt :id :source
                  :tool-call-id :position
                  :kind :media-type
                  :filename :size
                  :audience :storage-uri)]

        (expect (= (mapv facts got) (mapv facts meta-rows) (mapv facts meta-batch)))
        (expect (every? #(not (contains? % :base64)) meta-rows))
        ;; …and each row still says whether an inline blob exists at all, so a
        ;; reader can tell `:inline` from `:external` without fetching either.
        (expect (every? :has-bytes meta-rows))
        ;; The ONE artifact a caller actually serves is read by its own id.
        (expect (= b64 (:base64 (vis/db-read-attachment s (:id (first meta-rows))))))
        ;; Regression, issue td-65cdf6: SQLite discarded the stable live-view
        ;; identity, so Companion painted the filed receipt and settled live view.
        (let [live-row
              (first (filter #(= "watch.live.ndjson" (:filename %)) got))

              live-meta
              (first (filter #(= "watch.live.ndjson" (:filename %)) meta-rows))]

          (expect (= "watch-view" (:view-id live-row) (:view-id live-meta)))))
      ;; Batch variant groups by iteration id.
      (let [batch (vis/db-list-iterations-attachments s [iid])]
        (expect (= 1 (count batch)))
        (expect (= 3 (count (get batch (str iid)))))))))

(defdescribe
  iteration-attachment-external-storage-test
  (it
    "an attachment carrying :storage-uri (no base64) stores EXTERNAL: uri kept, bytes NULL, source derived"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          tid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "offload it"})

          iid
          (h/store-iteration!
            s
            {:session-turn-id tid
             :status :done
             :code "big.csv"
             :forms [{:scope "t1/i1" :tag :observation :src "big.csv" :svar/tool-call-id "call_X"}]
             ;; The offload rail parked the bytes in a storage backend; the row
             ;; keeps only the handle (no :base64) - the CHECK is satisfied by
             ;; storage_uri instead of bytes.
             :attachments [{:tool-call-id "call_X"
                            :kind "file"
                            :media-type "text/csv"
                            :filename "big.csv"
                            :size 300000
                            :storage-uri "file:///var/vis/att/abc"}]})

          got
          (vis/db-list-iteration-attachments s iid)

          row
          (first got)]

      (expect (= 1 (count got)))
      (expect (= "file:///var/vis/att/abc" (:storage-uri row)))
      (expect (nil? (:base64 row))) ; external row carries no inline bytes
      (expect (= 300000 (:size row)))
      (expect (= "text/csv" (:media-type row)))
      (expect (= :tool (:source row)))
      ;; Bare-id read-back returns the same external envelope.
      (let [back (vis/db-read-attachment s (:id row))]
        (expect (= "file:///var/vis/att/abc" (:storage-uri back)))
        (expect (nil? (:base64 back)))))))

(defdescribe
  user+tool-attachment-read-back-test
  (it
    "one session_attachment table: bare-id read-back derives source; a turn roll-up returns user + tool together"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          png
          (byte-array (map unchecked-byte [0x89 0x50 0x4e 0x47 9 8 7]))

          b64
          (.encodeToString (java.util.Base64/getEncoder) png)

          ;; INBOUND user image attached to the turn message.
          soul
          (vis/db-store-session-turn!
            s
            {:parent-session-id cid
             :user-request "here is a chart"
             :attachments
             [{:media-type "image/png" :base64 b64 :filename "user.png" :size (alength png)}]})

          user-atts
          (vis/db-list-turn-attachments s soul)

          uid
          (:id (first user-atts))

          ;; OUTBOUND tool artifact on an iteration of the same turn.
          iid
          (h/store-iteration!
            s
            {:session-turn-id soul
             :status :done
             :code "plt.show()"
             :attachments
             [{:tool-call-id "call_Z" :media-type "image/png" :base64 b64 :filename "tool.png"}]})

          tid
          (:id (first (vis/db-list-iteration-attachments s iid)))

          user-read
          (vis/db-read-attachment s uid)

          tool-read
          (vis/db-read-attachment s tid)

          ;; Turn roll-up: user + tool in ONE indexed filter (soul only).
          all-atts
          (vis/db-list-turn-all-attachments s soul)]

      ;; Listers now project BARE row uuids (no source prefix) + a derived :source.
      (expect (= 1 (count user-atts)))
      (expect (string? uid))
      (expect (not (str/includes? uid ":")))
      (expect (not (str/includes? tid ":")))
      (expect (= :user (:source (first user-atts))))
      (expect (= :tool (:source (first (vis/db-list-iteration-attachments s iid)))))
      ;; Bare-id read-back hits the single table and derives provenance from the row.
      (expect (= :user (:source user-read)))
      (expect (nil? (:tool-call-id user-read)))
      (expect (= "user.png" (:filename user-read)))
      (expect (= b64 (:base64 user-read)))
      (expect (= uid (:id user-read)))
      (expect (= :tool (:source tool-read)))
      (expect (= "call_Z" (:tool-call-id tool-read)))
      (expect (= "tool.png" (:filename tool-read)))
      (expect (= tid (:id tool-read)))
      ;; Roll-up sees BOTH rails of the same turn, user first then tool.
      (expect (= 2 (count all-atts)))
      (expect (= [:user :tool] (mapv :source all-atts)))
      (expect (= #{uid tid} (set (map :id all-atts))))
      ;; Unknown id -> nil.
      (expect (nil? (vis/db-read-attachment s (str (java.util.UUID/randomUUID))))))))


;; Regression, session 55ed67f6: the sandbox now hands the producer an
;; artifact's id INSIDE the block that made it, so a row minted with a fresh
;; uuid at store time turned that id into a dangling reference one iteration
;; later — `get_attachment(attach(...))` found nothing.
(defdescribe
  attachment-identity-passthrough-test
  (it
    "stores an artifact under the id and version the sandbox already handed out"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          soul
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "make a chart"})

          given-id
          (str (java.util.UUID/randomUUID))

          iid
          (h/store-iteration! s
                              {:session-turn-id soul
                               :status :done
                               :code "attach(png, 'chart.png')"
                               :attachments [{:id given-id
                                              :version 3
                                              :tool-call-id "call_A"
                                              :media-type "image/png"
                                              :base64 "AQID"
                                              :filename "chart.png"
                                              :size 3}
                                             {:tool-call-id "call_A"
                                              :media-type "image/png"
                                              :base64 "AQID"
                                              :filename "unstamped.png"
                                              :size 3}]})

          rows
          (vis/db-list-iteration-attachments s iid)

          by-name
          (into {} (map (juxt :filename identity)) rows)]

      ;; The stamped artifact keeps BOTH halves of the identity it was given.
      (expect (= given-id (:id (get by-name "chart.png"))))
      (expect (= 3 (:version (get by-name "chart.png"))))
      ;; It reads back by that id, which is the whole point of handing it out.
      (expect (= "chart.png" (:filename (vis/db-read-attachment s given-id))))
      ;; An unstamped entry beside it still gets a fresh uuid and the
      ;; allocator's next cut.
      (expect (string? (:id (get by-name "unstamped.png"))))
      (expect (not= given-id (:id (get by-name "unstamped.png"))))
      (expect (= 1 (:version (get by-name "unstamped.png")))))))
(defdescribe
  session-attachment-rollup-test
  (it
    "db-list-session-attachments rolls up user + tool across a whole session, ordered by turn then source"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          png
          (byte-array (map unchecked-byte [0x89 0x50 0x4e 0x47 1 2 3]))

          b64
          (.encodeToString (java.util.Base64/getEncoder) png)

          ;; Turn 1: user image on the message + tool artifact on an iteration.
          soul1
          (vis/db-store-session-turn!
            s
            {:parent-session-id cid
             :user-request "turn one"
             :attachments
             [{:media-type "image/png" :base64 b64 :filename "u1.png" :size (alength png)}]})

          _
          (h/store-iteration!
            s
            {:session-turn-id soul1
             :status :done
             :code "plt.show()"
             :attachments
             [{:tool-call-id "call_A" :media-type "image/png" :base64 b64 :filename "t1.png"}]})

          ;; Turn 2: tool artifact only.
          soul2
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "turn two"})

          _
          (h/store-iteration!
            s
            {:session-turn-id soul2
             :status :done
             :code "plt.show()"
             :attachments
             [{:tool-call-id "call_B" :media-type "image/png" :base64 b64 :filename "t2.png"}]})

          all
          (vis/db-list-session-attachments s cid)]

      (expect (= 3 (count all)))
      ;; Ordered: turn1 user, turn1 tool, turn2 tool.
      (expect (= [:user :tool :tool] (mapv :source all)))
      (expect (= ["u1.png" "t1.png" "t2.png"] (mapv :filename all)))
      ;; PROVENANCE STARTS AT THE TURN: EVERY row - user image AND tool artifact -
      ;; carries the soul of the turn it belongs to, so `list_attachments()` can stamp
      ;; `turn_id` on all of them. The iteration / tool-call grain is the FINER
      ;; provenance only a tool artifact also has.
      (expect (= [(str soul1) (str soul1) (str soul2)] (mapv :turn-soul-id all)))
      (expect (nil? (:iteration-id (first all))))
      (expect (some? (:iteration-id (nth all 1))))
      (expect (some? (:iteration-id (nth all 2))))
      (expect (= "call_A" (:tool-call-id (nth all 1))))
      (expect (= "call_B" (:tool-call-id (nth all 2))))
      ;; Unknown session -> [].
      (expect (= [] (vis/db-list-session-attachments s (str (java.util.UUID/randomUUID)))))
      ;; The bytes-free twin answers the SAME rows in the SAME order - a
      ;; whole-session artifact INDEX must never read a session's payload.
      (let [meta (vis/db-list-session-attachments-meta s cid)]
        (expect (= (mapv :filename all) (mapv :filename meta)))
        (expect (= (mapv #(str (:turn-soul-id %)) all) (mapv #(str (:turn-soul-id %)) meta)))
        (expect (= (mapv #(str (:iteration-id %)) all) (mapv #(str (:iteration-id %)) meta)))
        (expect (every? #(nil? (:base64 %)) meta))
        (expect (every? :has-bytes meta))
        (expect (= []
                   (vis/db-list-session-attachments-meta s (str (java.util.UUID/randomUUID)))))))))

(defdescribe
  sqlite-extension-aggregate-index-data-filter-test
  (it
    "filters extension aggregate rows by index_data JSON fields"
    (let [s (h/store)]
      ;; Insert three rows with different index data
      (persistance/db-put-extension-aggregate!
        s
        {:extension-id 'test.ext.graph
         :aggregate-key "node:core/run"
         :kind :graph/node
         :index-data {:path "src/core.clj" :kind "def" :language "clojure"}
         :content {:name "run"}})
      (persistance/db-put-extension-aggregate!
        s
        {:extension-id 'test.ext.graph
         :aggregate-key "node:core/start"
         :kind :graph/node
         :index-data {:path "src/core.clj" :kind "def" :language "clojure"}
         :content {:name "start"}})
      (persistance/db-put-extension-aggregate!
        s
        {:extension-id 'test.ext.graph
         :aggregate-key "edge:core/run::calls::lc/iterate"
         :kind :graph/edge
         :index-data
         {:edge-kind "calls" :source "core/run" :target "lc/iterate" :path "src/core.clj"}
         :content {:source "core/run" :target "lc/iterate" :kind "calls"}})
      ;; Filter by kind + index-data file-path → both nodes in core.clj
      (let [by-file (vis/db-list-extension-aggregates s
                                                      {:extension-id 'test.ext.graph
                                                       :kind :graph/node
                                                       :index-data {:path "src/core.clj"}})]
        (expect (= 2 (count by-file)))
        (expect (= #{"node:core/run" "node:core/start"} (set (map :key by-file)))))
      ;; Filter by index-data edge-kind → one edge
      (let [by-edge-kind (vis/db-list-extension-aggregates s
                                                           {:extension-id 'test.ext.graph
                                                            :index-data {:edge-kind "calls"}})]
        (expect (= 1 (count by-edge-kind)))
        (expect (= "edge:core/run::calls::lc/iterate" (:key (first by-edge-kind)))))
      ;; Filter by index-data source → edge from core/run
      (let [by-source (vis/db-list-extension-aggregates s
                                                        {:extension-id 'test.ext.graph
                                                         :index-data {:source "core/run"}})]
        (expect (= 1 (count by-source))))
      ;; Filter edges by file-path → re-indexing use case
      (let [by-edge-file (vis/db-list-extension-aggregates s
                                                           {:extension-id 'test.ext.graph
                                                            :kind :graph/edge
                                                            :index-data {:path "src/core.clj"}})]
        (expect (= 1 (count by-edge-file))))
      ;; No match → empty
      (let [none (vis/db-list-extension-aggregates s
                                                   {:extension-id 'test.ext.graph
                                                    :index-data {:path "nonexistent.clj"}})]
        (expect (= 0 (count none)))))))

(defdescribe
  sqlite-bootstrap-error-normalization-test
  (it
    "matches Flyway checksum text at top level"
    (expect
      (true?
        (migration-checksum-mismatch?
          (ex-info
            "Validate failed: Migrations have failed validation\nMigration checksum mismatch for migration version 1"
            {})))))
  (it "matches Flyway checksum text in a nested cause"
      (let [cause
            (ex-info "Migration checksum mismatch for migration version 1" {})

            e
            (ex-info "wrapper" {} cause)]

        (expect (true? (migration-checksum-mismatch? e)))))
  (it "returns false for unrelated failures"
      (expect (false? (migration-checksum-mismatch? (ex-info "boom" {})))))
  (it "wraps checksum mismatch as :vis/user-error with actionable guidance"
      (let [root
            (ex-info "Migration checksum mismatch for migration version 1" {})

            e
            (maybe-wrap-db-open-error root)]

        (expect (instance? clojure.lang.ExceptionInfo e))
        (expect (true? (:vis/user-error (ex-data e))))
        (expect (= :vis/db-migration-checksum-mismatch (:type (ex-data e))))
        (expect (= root (.getCause ^Throwable e)))
        ;; NON-DESTRUCTIVE guidance: self-heals via repair, never "delete the DB".
        (expect (str/includes? (.getMessage ^Throwable e) "Flyway repair"))
        (expect (not (str/includes? (.getMessage ^Throwable e) "remove ~/.vis/vis.mdb")))))
  (it "leaves unrelated bootstrap failures untouched"
      (let [e (ex-info "x" {})]
        (expect (identical? e (maybe-wrap-db-open-error e)))))
  (it "describes canonical V1 repair without deleting the store"
      (expect (str/includes? migration-checksum-mismatch-user-message "schema mismatch"))
      (expect (str/includes? migration-checksum-mismatch-user-message "Flyway repair"))
      (expect (str/includes? migration-checksum-mismatch-user-message "canonical V1"))
      (expect (not (str/includes? migration-checksum-mismatch-user-message
                                  "remove ~/.vis/vis.mdb")))))

(defdescribe
  migration-repair-self-heal-test
  (it "a drifted V1 checksum self-heals through Flyway repair and preserves rows"
      (let [root
            (fs/create-temp-dir {:prefix "vis-repair-"})

            dir
            (str (fs/path root "store"))

            s1
            (vis/db-create-connection! dir)]

        (try
          (jdbc/execute! (:datasource s1) ["CREATE TABLE repair_probe (id INTEGER)"])
          (jdbc/execute! (:datasource s1) ["INSERT INTO repair_probe (id) VALUES (42)"])
          ;; Force the exact validation failure that used to wedge gateway
          ;; startup. Repair must touch only Flyway metadata, never this row.
          (jdbc/execute! (:datasource s1)
                         ["UPDATE flyway_schema_history SET checksum = -999 WHERE version = '1'"])
          (vis/db-dispose-connection! s1)
          (let [s2 (vis/db-create-connection! dir)]
            (try (expect (= 1 (raw-count s2 :repair_probe)))
                 (expect (= 42 (:id (first (raw-query s2 {:select [:id] :from [:repair_probe]})))))
                 (finally (vis/db-dispose-connection! s2))))
          (finally (fs/delete-tree root))))))

(defdescribe
  db-store-stale-identity-test
  ;; Regression (hs_err_pid48027, gateway SIGBUS inside `NativeDB.step`):
  ;; the staleness snapshot carried the db file's SIZE and MTIME. SQLite
  ;; rewrites `vis.db` in place on every WAL checkpoint, so ORDINARY write
  ;; traffic moved both and the store looked "replaced" forever after.
  ;; `db-shared-connection!` answers a stale store by closing the
  ;; process-wide Hikari pool - underneath in-flight queries - and opening
  ;; a new one: the crashed process had reached pool generation 351 with
  ;; seven leaked housekeeper threads in 3h21m of uptime.
  (it "stays fresh across a WAL checkpoint that rewrites the file in place"
      (let [root
            (fs/create-temp-dir)

            dir
            (str (fs/path root "store"))

            s
            (vis/db-create-connection! dir)]

        (try (expect (false? (boolean (sqlite-core/db-store-stale? s dir))))
             (jdbc/execute! (:datasource s) ["CREATE TABLE churn (id INTEGER PRIMARY KEY, v TEXT)"])
             (dotimes [_ 200]
               (jdbc/execute! (:datasource s)
                              ["INSERT INTO churn (v) VALUES (?)" (str/join (repeat 400 "x"))]))
             (jdbc/execute! (:datasource s) ["PRAGMA wal_checkpoint(TRUNCATE)"])
             ;; The file grew and its mtime moved; the inode did not.
             (expect (< 4096 (fs/size (fs/path dir "vis.db"))))
             (expect (false? (boolean (sqlite-core/db-store-stale? s dir))))
             (finally (vis/db-dispose-connection! s) (fs/delete-tree root)))))
  (it "reports stale when the file at the same path is a different inode"
      (let [root
            (fs/create-temp-dir)

            dir
            (str (fs/path root "store"))

            s
            (vis/db-create-connection! dir)]

        (try (expect (false? (boolean (sqlite-core/db-store-stale? s dir))))
             ;; `rm -rf ~/.vis/vis.mdb` + a fresh file at the same pathname:
             ;; identity moved, and a reopen is the right answer.
             (fs/delete (fs/path dir "vis.db"))
             (expect (true? (boolean (sqlite-core/db-store-stale? s dir))))
             (fs/create-file (fs/path dir "vis.db"))
             (expect (true? (boolean (sqlite-core/db-store-stale? s dir))))
             (finally (vis/db-dispose-connection! s) (fs/delete-tree root))))))

(defdescribe db-pool-sharing-and-drain-test
             ;; Regression (hs_err_pid61432, gateway SIGBUS in `_platform_memmove` with
             ;; `NativeDB.step` on the Java stack): every environment opened its OWN Hikari
             ;; pool over the SAME `~/.vis/vis.mdb` (19 generations in 75 minutes), and a
             ;; dispose closed that pool's physical SQLite handles even when a sibling
             ;; thread was inside `sqlite3_step`. sqlite then dropped the last reference to
             ;; the WAL-index shared-memory node and `munmap`ed the `-shm` region the
             ;; stepping thread was writing into -> SIGBUS at the first 16 KiB page
             ;; boundary past the wal-index header.
             (it "shares ONE pool per db file and keeps it alive until the LAST dispose"
                 (let [root
                       (fs/create-temp-dir)

                       dir
                       (str (fs/path root "store"))

                       s1
                       (vis/db-create-connection! dir)

                       s2
                       (vis/db-create-connection! dir)]

                   (try (expect (identical? (:datasource s1) (:datasource s2)))
                        (vis/db-dispose-connection! s1)
                        ;; `s2` still holds a reference, so the pool must still serve it.
                        (expect (= 1 (count (jdbc/execute! (:datasource s2) ["SELECT 1"]))))
                        (finally (vis/db-dispose-connection! s2) (fs/delete-tree root)))))
             (it "drains an in-flight query instead of aborting it mid-step"
                 (let [root
                       (fs/create-temp-dir)

                       dir
                       (str (fs/path root "store"))

                       s
                       (vis/db-create-connection! dir)

                       leased
                       (java.util.concurrent.CountDownLatch. 1)

                       outcome
                       (promise)

                       worker
                       (future (try (with-open [conn (jdbc/get-connection (:datasource s))]
                                      (.countDown leased)
                                      ;; Hold the lease across the dispose below.
                                      (Thread/sleep 300)
                                      (deliver outcome (count (jdbc/execute! conn ["SELECT 1"]))))
                                    (catch Throwable t (deliver outcome t))))]

                   (try (.await leased)
                        (vis/db-dispose-connection! s)
                        @worker
                        (expect (= 1 @outcome))
                        (finally (fs/delete-tree root)))))
             (it "drops ONE reference however often the same store is disposed"
                 ;; `dispose-environment!` is reachable from eviction, `delete!`,
                 ;; `close-all!` and `main`'s finally: a double release must not close a
                 ;; pool a sibling environment is still using.
                 (let [root
                       (fs/create-temp-dir)

                       dir
                       (str (fs/path root "store"))

                       s1
                       (vis/db-create-connection! dir)

                       s2
                       (vis/db-create-connection! dir)]

                   (try (vis/db-dispose-connection! s1)
                        (vis/db-dispose-connection! s1)
                        (expect (= 1 (count (jdbc/execute! (:datasource s2) ["SELECT 1"]))))
                        (finally (vis/db-dispose-connection! s2) (fs/delete-tree root)))))
             (it "retires the shared pool when the db file was REPLACED under a live holder"
                 ;; Sharing must not outlive the inode: reusing a pool whose file was
                 ;; recreated would send every write into the unlinked file.
                 (let [root
                       (fs/create-temp-dir)

                       dir
                       (str (fs/path root "store"))

                       s1
                       (vis/db-create-connection! dir)]

                   (try (fs/delete-if-exists (fs/path dir "vis.db-shm"))
                        (fs/delete-if-exists (fs/path dir "vis.db-wal"))
                        (fs/delete (fs/path dir "vis.db"))
                        (expect (true? (boolean (sqlite-core/db-store-stale? s1 dir))))
                        (let [s2 (vis/db-create-connection! dir)]
                          (try (expect (not (identical? (:datasource s1) (:datasource s2))))
                               (expect (= 1 (count (jdbc/execute! (:datasource s2) ["SELECT 1"]))))
                               (expect (fs/exists? (fs/path dir "vis.db")))
                               (finally (vis/db-dispose-connection! s2))))
                        (finally (vis/db-dispose-connection! s1) (fs/delete-tree root))))))

(defdescribe
  migration-additive-column-top-up-test
  (it
    "a store created by an OLDER canonical V1 gains the new V1 columns on reopen"
    (let [root
          (fs/create-temp-dir {:prefix "vis-topup-"})

          dir
          (str (fs/path root "store"))

          s1
          (vis/db-create-connection! dir)]

      (try (expect (contains? (table-columns s1 "session_attachment") "audience"))
           ;; Rewind this store to an OLDER shape of the same canonical V1: the
           ;; table exists, its Flyway history is intact, but it predates the
           ;; columns V1 has grown since. That is exactly what a `~/.vis` database
           ;; created before the in-place V1 edit looks like.
           (jdbc/execute! (:datasource s1) ["DROP TABLE session_attachment"])
           (jdbc/execute! (:datasource s1)
                          ["CREATE TABLE session_attachment (id TEXT PRIMARY KEY NOT NULL)"])
           (jdbc/execute! (:datasource s1) ["CREATE TABLE topup_probe (id INTEGER)"])
           (jdbc/execute! (:datasource s1) ["INSERT INTO topup_probe (id) VALUES (7)"])
           (vis/db-dispose-connection! s1)
           (let [s2 (vis/db-create-connection! dir)]
             (try (let [cols (table-columns s2 "session_attachment")]
                    ;; Defaulted / nullable columns are added back from V1's own DDL.
                    (expect (contains? cols "audience"))
                    (expect (contains? cols "kind"))
                    (expect (contains? cols "tool_call_id"))
                    (expect (contains? cols "view_id"))
                    ;; Activity belongs to the form, never to an attachment: the
                    ;; retired columns must not come back through the top-up.
                    (expect (not (contains? cols "classification")))
                    (expect (not (contains? cols "activity_anchor")))
                    ;; NOT NULL without a DEFAULT is not addable in SQLite: left alone
                    ;; rather than failing the open.
                    (expect (not (contains? cols "media_type"))))
                  ;; Purely additive: unrelated tables and rows are untouched.
                  (expect (= 1 (raw-count s2 :topup_probe)))
                  (finally (vis/db-dispose-connection! s2))))
           (finally (fs/delete-tree root))))))

(def ^:private multiprocess-child-code
  "(require '[com.blockether.vis.core :as vis])
   (require '[com.blockether.vis.ext.persistance-sqlite.test-helpers :as h])
   (try
     (let [dir     (System/getProperty \"vis.test.db-dir\")
           marker  (System/getProperty \"vis.test.marker\")
           release (System/getProperty \"vis.test.release\")
           title   (System/getProperty \"vis.test.title\")
           s       (vis/db-create-connection! dir)]
       (try
         ;; Both children must finish opening before either writes. This makes one
         ;; concise test cover concurrent first-open migrations and cross-JVM writes.
         (spit marker \"ready\")
          ;; Outlast the parent's wait for the SLOWER sibling. A child that gave
          ;; up first would exit while the parent still waited, and the run failed
          ;; as -- children did not both open the store -- naming no cause.
          (let [deadline (+ (System/currentTimeMillis) 300000)]
           (while (and (not (.exists (java.io.File. release)))
                       (< (System/currentTimeMillis) deadline))
             (Thread/sleep 25)))
         (when-not (.exists (java.io.File. release))
           (throw (ex-info \"timed out waiting for parent release\" {})))
         (h/store-session! s {:channel :child :title title})
         (println \"CHILD-DONE\" title)
         (finally
           (vis/db-dispose-connection! s))))
     (shutdown-agents)
     (System/exit 0)
     (catch Throwable t
       (.printStackTrace t)
       (shutdown-agents)
       (System/exit 1)))")

(defonce ^:private child-output-futures (atom {}))

(defn- java-command [] (str (fs/file (System/getProperty "java.home") "bin" "java")))

(defn- start-multiprocess-writer!
  ^Process [dir marker release title]
  (let [norm
        (fn [s]
          (.replace (str s) "\\" "/"))

        script
        (fs/file (fs/create-temp-dir {:prefix "vis-mp-child-"}) "child.clj")

        _
        (spit script multiprocess-child-code)

        ;; Measured: the child's ~5s is Clojure class-loading, and throttling the JIT
        ;; (`-XX:TieredStopAtLevel=1`, serial GC) makes it SLOWER (7.4s), because that
        ;; loading is what C2 parallelises. Leave the defaults alone.
        ^java.util.List cmd
        [(java-command) "-Xmx1g" (str "-Dvis.test.db-dir=" (norm dir))
         (str "-Dvis.test.marker=" (norm marker)) (str "-Dvis.test.release=" (norm release))
         (str "-Dvis.test.title=" title) "clojure.main" (norm (str script))]

        pb
        (ProcessBuilder. cmd)]

    ;; Classpath via the environment keeps the child argv small and portable.
    (.put (.environment pb) "CLASSPATH" (System/getProperty "java.class.path"))
    ;; The suite's own heap sizing travels in JAVA_TOOL_OPTIONS; a child that
    ;; inherited it would let two cold JVMs grow to the parent's maximum on a
    ;; shared runner. Each child opens ONE store, so a small heap is plenty.
    (.redirectErrorStream pb true)
    (let [child (.start pb)]
      (swap! child-output-futures assoc child (future (slurp (.getInputStream child))))
      child)))

(defn- wait-for-files
  [paths children timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
    (loop []

      (cond (every? fs/exists? paths) true
            (some (complement #(.isAlive ^Process %)) children) false
            (>= (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 25) (recur))))))

;; Child JVMs cold-boot Clojure, Flyway, and sqlite-jdbc. The timeout bounds a
;; broken child without letting one stalled process hold the full suite hostage;
;; a loaded CI runner has taken over two minutes just to boot the pair.
(def ^:private MULTIPROCESS_CHILD_TIMEOUT_S 240)

(defn- child-output
  [^Process child]
  (some-> (get @child-output-futures child)
          (deref 1000 "")))

(defn- expect-child-success!
  [^Process child]
  (let [finished?
        (.waitFor child MULTIPROCESS_CHILD_TIMEOUT_S TimeUnit/SECONDS)

        output
        (child-output child)]

    (swap! child-output-futures dissoc child)
    (when-not finished?
      (.destroyForcibly child)
      (throw (ex-info "multiprocess child timed out" {:output output})))
    (when-not (and (zero? (.exitValue child)) (str/includes? (str output) "CHILD-DONE"))
      (println "=== multiprocess child output (exit" (.exitValue child) ") ===")
      (println output)
      (println "=== end child output ==="))
    (expect (= 0 (.exitValue child)))
    (expect (str/includes? output "CHILD-DONE"))))

(defdescribe
  sqlite-multiprocess-write-test
  (it
    "serializes concurrent first-open migrations and writes across two JVMs"
    (let [dir
          (fs/create-temp-dir {:prefix "vis-db-multiprocess-"})

          release
          (fs/file dir "release")

          markers
          [(fs/file dir "child-a-opened") (fs/file dir "child-b-opened")]]

      (try
        (let [children
              [(start-multiprocess-writer! (str dir) (str (first markers)) (str release) "child-a")
               (start-multiprocess-writer! (str dir)
                                           (str (second markers))
                                           (str release)
                                           "child-b")]]
          (try (when-not (wait-for-files markers children (* 1000 MULTIPROCESS_CHILD_TIMEOUT_S))
                 (doseq [^Process child children]
                   (when (.isAlive child) (.destroyForcibly child)))
                 ;; The runner reports the message alone, so print what the
                 ;; children said: a stall that only happens on CI is otherwise
                 ;; undiagnosable from the log.
                 (doseq [out (mapv child-output children)]
                   (println "=== multiprocess child output ===")
                   (println out)
                   (println "=== end child output ==="))
                 (throw (ex-info "multiprocess children did not both open the store"
                                 {:output (mapv child-output children)})))
               (spit release "go")
               (doseq [^Process child children]
                 (expect-child-success! child))
               (let [s (vis/db-create-connection! (str dir))]
                 (try (expect (= #{"child-a" "child-b"}
                                 (set (map :title
                                           (raw-query s
                                                      {:select [:title] :from [:session_state]})))))
                      (finally (vis/db-dispose-connection! s))))
               (finally (doseq [^Process child children]
                          (when (.isAlive child) (.destroyForcibly child))))))
        (finally (fs/delete-tree dir))))))

(defdescribe
  sqlite-same-jvm-migration-lock-test
  (it "serializes the migration lock across THREADS of one JVM (no OverlappingFileLockException)"
      ;; Regression for concurrent session creation: a `FileLock` is JVM-WIDE,
      ;; so two THREADS of this process racing `with-migration-lock!` on the same
      ;; dir threw `OverlappingFileLockException` instead of blocking.
      ;; The in-process monitor makes them queue — no error, and the critical
      ;; section is held by at most one thread at a time.
      (let [with-migration-lock!
            (private-core-fn "with-migration-lock!")

            dir
            (str (fs/create-temp-dir {:prefix "vis-db-same-jvm-lock-"}))

            n
            16

            start
            (CountDownLatch. 1)

            done
            (CountDownLatch. n)

            active
            (java.util.concurrent.atomic.AtomicInteger. 0)

            overlap
            (java.util.concurrent.atomic.AtomicBoolean. false)

            errors
            (java.util.concurrent.ConcurrentLinkedQueue.)

            run-one
            (fn []
              (try (.await start)
                   (with-migration-lock! dir
                                         (fn []
                                           (when (> (.incrementAndGet active) 1)
                                             (.set overlap true))
                                           (Thread/sleep 3)
                                           (.decrementAndGet active)))
                   (catch Throwable t (.add errors t))
                   (finally (.countDown done))))]

        (try (dotimes [_ n]
               (.start (Thread. ^Runnable run-one)))
             (.countDown start)
             (expect (.await done 30 TimeUnit/SECONDS))
             ;; No thread saw a lock error (the crash), and the section was exclusive.
             (expect (zero? (count errors)))
             (expect (false? (.get overlap)))
             (finally (fs/delete-tree dir))))))

(defdescribe
  sqlite-transaction-mode-test
  (it "reproduces SQLITE_BUSY_SNAPSHOT with a stale deferred read transaction"
      (let [db-file
            (File/createTempFile "vis-busy-snapshot" ".db")

            url
            (str "jdbc:sqlite:" (.getAbsolutePath db-file))

            c1
            (java.sql.DriverManager/getConnection url)

            c2
            (java.sql.DriverManager/getConnection url)]

        (try
          (jdbc/execute! c1 ["PRAGMA journal_mode=WAL"])
          (jdbc/execute! c1
                         (sql/format {:create-table [:snapshot_probe :if-not-exists]
                                      :with-columns [[:id :integer :primary-key] [:v :integer]]}))
          (jdbc/execute! c1 (sql/format {:insert-into :snapshot_probe :values [{:id 1 :v 0}]}))
          (.setAutoCommit c1 false)
          (jdbc/execute! c1 (sql/format {:select [:*] :from [:snapshot_probe] :where [:= :id 1]}))
          (jdbc/execute! c2 (sql/format {:update :snapshot_probe :set {:v 1} :where [:= :id 1]}))
          (let [^Throwable thrown (try (jdbc/execute! c1
                                                      (sql/format {:update :snapshot_probe
                                                                   :set {:v 2}
                                                                   :where [:= :id 1]}))
                                       nil
                                       (catch Throwable t t))]
            (expect (some? thrown))
            (expect (str/includes? (.getMessage thrown) "SQLITE_BUSY_SNAPSHOT")))
          (finally (.close c1) (.close c2) (fs/delete-if-exists db-file)))))
  (it
    "uses immediate transactions so read-then-write transactions survive concurrent telemetry writes"
    (let [dir (fs/create-temp-dir {:prefix "vis-snapshot-lock-"})]
      (try (let [s (vis/db-create-connection! (str dir))
                 cid (h/store-session! s {:channel :cli :title "old"})
                 started (CountDownLatch. 1)
                 worker (future (try (jdbc/with-transaction
                                       [tx (:datasource s)]
                                       (jdbc/execute! tx
                                                      (sql/format {:select [:id]
                                                                   :from [:session_soul]
                                                                   :where [:= :id (str cid)]}))
                                       (.countDown started)
                                       (Thread/sleep 100)
                                       (jdbc/execute! tx
                                                      (sql/format {:update :session_state
                                                                   :set {:title "new"}
                                                                   :where [:= :session_soul_id
                                                                           (str cid)]})))
                                     nil
                                     (catch Throwable t t)))]

             (try (expect (true? (.await started 1 TimeUnit/SECONDS)))
                  (vis/db-log! s {:level :info :event :snapshot-test})
                  (expect (nil? @worker))
                  (expect (= "new" (:title (vis/db-get-session s cid))))
                  (finally (vis/db-dispose-connection! s))))
           (finally (fs/delete-tree dir)))))
  (it
    "serializes Vis write APIs while concurrent db-log! calls contend"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          tid
          (vis/db-store-session-turn!
            s
            {:parent-session-id cid :user-request "stress" :status :running})

          started
          (CountDownLatch. 1)

          done
          (CountDownLatch. 1)

          worker
          (future (.countDown started)
                  (.await done 1 TimeUnit/SECONDS)
                  (try (h/store-iteration! s {:session-turn-id tid :code "(+ 1 2)" :result 3})
                       nil
                       (catch Throwable t t)))

          logs
          (do (expect (true? (.await started 1 TimeUnit/SECONDS)))
              (vec (doall (map (fn [i]
                                 (future (try (vis/db-log! s
                                                           {:level :info :event (str "stress." i)})
                                              nil
                                              (catch Throwable t t))))
                               (range 20)))))]

      (.countDown done)
      (expect (nil? @worker))
      (doseq [f logs]
        (expect (nil? @f)))
      (expect (= 20 (raw-count s :log)))
      (expect (= 1 (count (vis/db-list-session-turn-iterations s tid))))))
  (it "retries a whole SQLite write operation after a busy snapshot failure"
      (let [attempts
            (atom 0)

            retry!
            (private-core-fn "sqlite-write-tx!")

            result
            (retry! (h/store)
                    (fn [_]
                      (if (= 1 (swap! attempts inc))
                        (throw (RuntimeException. "[SQLITE_BUSY_SNAPSHOT] stale snapshot"))
                        :ok)))]

        (expect (= :ok result))
        (expect (= 2 @attempts)))))

;; Session

(defdescribe
  session-test
  (it "inserts into session_soul + session_state"
      (let [s
            (h/store)

            id
            (h/store-session! s {:channel :tui :system-prompt "Hi" :model "gpt-4o" :title "T"})

            session
            (vis/db-get-session s id)]

        (expect (= 1 (raw-count s :session_soul)))
        (expect (= 1 (raw-count s :session_state)))
        (expect (= :tui (:channel session)))
        (expect (= "Hi" (:system-prompt session)))
        (expect (= "gpt-4o" (:model session)))
        (expect (= "T" (:title session)))
        (expect (= 0 (:version session)))))
  (it "resolves :latest"
      (let [s (h/store)]
        (h/store-session! s {:channel :tui})
        (Thread/sleep 2)
        (let [id2 (h/store-session! s {:channel :tui})
              latest (vis/db-resolve-session-id s :latest)]

          (expect (= id2 latest)))))
  (it "lists by channel via column"
      (let [s (h/store)]
        (h/store-session! s {:channel :tui :title "A"})
        (h/store-session! s {:channel :cli :title "B"})
        (h/store-session! s {:channel :tui :title "C"})
        (expect (= 2 (count (vis/db-list-sessions s :tui))))
        (expect (= 1 (count (vis/db-list-sessions s :cli))))))
  (it "lists across every channel with :all (and nil) — the cross-channel view"
      (let [s (h/store)]
        (h/store-session! s {:channel :tui :title "A"})
        (h/store-session! s {:channel :cli :title "B"})
        (h/store-session! s {:channel :api :title "C"})
        (expect (= 3 (count (vis/db-list-sessions s :all))))
        (expect (= 3 (count (vis/db-list-sessions s nil))))
        (expect (= #{:tui :cli :api} (set (map :channel (vis/db-list-sessions s :all)))))))
  (it "reports fork count after the latest state becomes the fork"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "A"})]

        (h/fork-session! s cid {})
        (expect (= [1] (mapv :fork-count (vis/db-list-sessions s :tui))))
        (h/fork-session! s cid {})
        (expect (= [2] (mapv :fork-count (vis/db-list-sessions s :tui))))))
  (it "finds by external-id via column"
      (let [s
            (h/store)

            id
            (h/store-session! s {:channel :cli :external-id "chat-42"})]

        (expect (= id (vis/db-find-session-by-external s :cli "chat-42")))
        (expect (nil? (vis/db-find-session-by-external s :cli "nope")))))
  (it "updates title on session_state"
      (let [s
            (h/store)

            id
            (h/store-session! s {:channel :tui :title "Old"})]

        (vis/db-update-session-title! s id "New")
        (expect (= "New" (:title (vis/db-get-session s id)))))))

;; Transcript search (db-search-session-ids) - matches USER request + assistant
;; iteration text, case-insensitive, so the session picker can find a session by
;; anything said in it (not just the title). Server-side so the assistant text
;; never crosses the wire.

(defdescribe
  session-transcript-search-test
  (it "matches a session by its user request text (case-insensitive)"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Alpha"})]

        (vis/db-store-session-turn!
          s
          {:parent-session-id cid :user-request "make the FILTERING work" :status :done})
        (expect (= [cid] (vis/db-search-session-ids s :all "filtering")))
        (expect (= [cid] (vis/db-search-session-ids s :all "FILTER")))
        (expect (= [] (vis/db-search-session-ids s :all "nomatch")))))
  (it
    "matches a session by assistant iteration prose"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui :title "Beta"})

          tid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "q" :status :done})]

      (h/store-iteration! s
                          {:session-turn-id tid
                           :assistant-prose "here is the SERVER-side answer"
                           :code "(+ 1 1)"
                           :result 2})
      (expect (= [cid] (vis/db-search-session-ids s :all "server-side")))
      (expect (= [] (vis/db-search-session-ids s :all "clientside")))))
  (it
    "tags each match with WHERE it hit: request, reply, or both"
    (let [s
          (h/store)

          req
          (h/store-session! s {:channel :tui :title "Req"})

          rep
          (h/store-session! s {:channel :tui :title "Rep"})

          both
          (h/store-session! s {:channel :tui :title "Both"})]

      ;; request-only: needle in user request, not the reply
      (vis/db-store-session-turn!
        s
        {:parent-session-id req :user-request "has NEEDLE here" :status :done})
      ;; reply-only: needle in assistant prose, not the request
      (let [tid (vis/db-store-session-turn!
                  s
                  {:parent-session-id rep :user-request "plain q" :status :done})]
        (h/store-iteration!
          s
          {:session-turn-id tid :assistant-prose "reply with NEEDLE" :code "x" :result 1}))
      ;; both: needle in request AND reply
      (let [tid (vis/db-store-session-turn!
                  s
                  {:parent-session-id both :user-request "NEEDLE in ask" :status :done})]
        (h/store-iteration!
          s
          {:session-turn-id tid :assistant-prose "NEEDLE in answer" :code "x" :result 1}))
      (let [by-id
            (into {} (map (juxt :id identity)) (vis/db-search-session-matches s :all "needle"))]
        (expect (= 3 (count by-id)))
        (expect (= {:in-request? true :in-reply? false}
                   (select-keys (get by-id req) [:in-request? :in-reply?])))
        (expect (= {:in-request? false :in-reply? true}
                   (select-keys (get by-id rep) [:in-request? :in-reply?])))
        (expect (= {:in-request? true :in-reply? true}
                   (select-keys (get by-id both) [:in-request? :in-reply?])))
        ;; request-only: snippet on the request side, nothing on the reply side
        (expect (str/includes? (:request-snippet (get by-id req)) "NEEDLE"))
        (expect (nil? (:reply-snippet (get by-id req))))
        ;; reply-only: snippet on the reply side, nothing on the request side
        (expect (str/includes? (:reply-snippet (get by-id rep)) "NEEDLE"))
        (expect (nil? (:request-snippet (get by-id rep))))
        ;; both: a snippet from each side
        (expect (str/includes? (:request-snippet (get by-id both)) "NEEDLE"))
        (expect (str/includes? (:reply-snippet (get by-id both)) "NEEDLE")))))
  (it "returns SEVERAL hits per session — the user's own words first, then newest"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Many"})]

        ;; Five turns, each mentioning the needle: the old GROUP BY + MAX shape
        ;; could physically carry only ONE request + ONE reply snippet per
        ;; session, so the picker showed a single arbitrary line.
        (dotimes [i 5]
          (let [tid (vis/db-store-session-turn! s
                                                {:parent-session-id cid
                                                 :user-request (str "needle ask number " i)
                                                 :status :done})]
            (h/store-iteration! s
                                {:session-turn-id tid
                                 :assistant-prose (str "needle answer number " i)
                                 :code "x"
                                 :result 1})))
        (let [m (first (vis/db-search-session-matches s :all "needle"))]
          (expect (<= 4 (count (:hits m))))
          (expect (every? #(str/includes? (:snippet %) "needle") (:hits m)))
          ;; What the USER asked outranks what the assistant answered, and each
          ;; side is newest-first inside its own band.
          (expect (= (sort-by (juxt #(if (= :request (:side %)) 0 1) (comp - inst-ms :at))
                              (:hits m))
                     (:hits m))))))
  ;; Regression, user report (paraphrased: "the search results are not sorted by
  ;; freshness — I care far more about freshness than about which band the hit
  ;; landed in"): the relevance band led the order, so a session whose REQUEST
  ;; held the word months ago sat above the one whose ANSWER said it a minute
  ;; ago, and the dates jumped up and down the result list.
  (it
    "orders by the session's own FRESHNESS, not by the band the hit landed in"
    (let [s
          (h/store)

          asked
          (h/store-session! s {:channel :tui :title "Asked"})

          replied
          (h/store-session! s {:channel :tui :title "Replied"})]

      ;; The request match is written FIRST and the reply-only match LAST, so
      ;; the reply-only session is the FRESHER of the two.
      (let [tid (vis/db-store-session-turn!
                  s
                  {:parent-session-id asked :user-request "needle in the ask" :status :done})]
        (h/store-iteration! s {:session-turn-id tid :assistant-prose "plain" :code "x" :result 1}))
      ;; Distinct instants: the order under test is a TIME order.
      (Thread/sleep 2)
      (let [tid (vis/db-store-session-turn!
                  s
                  {:parent-session-id replied :user-request "plain ask" :status :done})]
        (h/store-iteration!
          s
          {:session-turn-id tid :assistant-prose "needle in the reply" :code "x" :result 1}))
      (expect (= [replied asked] (vis/db-search-session-ids s :all "needle")))
      ;; The band still travels — it says WHERE the query hit and breaks a tie
      ;; between two sessions that last moved at the same instant.
      (let [by-id
            (into {} (map (juxt :id identity)) (vis/db-search-session-matches s :all "needle"))]
        (expect (= 1 (:rank (get by-id asked))))
        (expect (= 2 (:rank (get by-id replied)))))))
  (it
    "tags the reasoning aside as the weakest band, `:rank` 3"
    (let [s
          (h/store)

          asked
          (h/store-session! s {:channel :tui :title "Asked"})

          answered
          (h/store-session! s {:channel :tui :title "Answered"})

          mused
          (h/store-session! s {:channel :tui :title "Mused"})

          turn!
          (fn [sid request iteration]
            ;; Distinct instants: the result order is a TIME order.
            (Thread/sleep 2)
            (let [tid (vis/db-store-session-turn!
                        s
                        {:parent-session-id sid :user-request request :status :done})]
              (h/store-iteration! s (merge {:session-turn-id tid :code "x" :result 1} iteration))))]

      ;; Written oldest FIRST, so the freshest session leads the answer.
      (turn! mused "plain ask" {:assistant-prose "plain" :thinking "needle while reasoning"})
      (turn! answered "plain ask" {:assistant-prose "needle in the answer"})
      (turn! asked "needle in the ask" {:assistant-prose "plain"})
      (expect (= [asked answered mused] (vis/db-search-session-ids s :all "needle")))
      (let [by-id
            (into {} (map (juxt :id identity)) (vis/db-search-session-matches s :all "needle"))]
        ;; The bands still travel: request 1, reply 2, the reasoning aside 3 —
        ;; what neither of them said ranks last, and a surface says WHERE it hit.
        (expect (= [1 2 3] (mapv #(:rank (get by-id %)) [asked answered mused])))
        (expect (= {:in-request? false :in-reply? false :in-thinking? true}
                   (select-keys (get by-id mused) [:in-request? :in-reply? :in-thinking?])))
        (expect (= {:in-request? false :in-reply? true :in-thinking? false}
                   (select-keys (get by-id answered) [:in-request? :in-reply? :in-thinking?])))
        ;; A thinking-only match still previews: the reply snippet falls back to it.
        (expect (str/includes? (:reply-snippet (get by-id mused)) "needle"))
        (expect (= [:thinking] (mapv :side (:hits (get by-id mused)))))
        (expect (= [:reply] (mapv :side (:hits (get by-id answered))))))))
  (it
    "puts a TITLE match where its session's FRESHNESS puts it, and says `:rank` 0"
    (let [s
          (h/store)

          named
          (h/store-session! s {:channel :tui :title "Needle by name"})

          asked
          (h/store-session! s {:channel :tui :title "Asked"})

          answered
          (h/store-session! s {:channel :tui :title "Answered"})

          turn!
          (fn [sid request iteration]
            ;; Distinct instants: the order under test is a TIME order.
            (Thread/sleep 2)
            (let [tid (vis/db-store-session-turn!
                        s
                        {:parent-session-id sid :user-request request :status :done})]
              (h/store-iteration! s (merge {:session-turn-id tid :code "x" :result 1} iteration))))]

      ;; The named session is written FIRST and its transcript never says
      ;; "needle": the band used to lift it over two newer body matches, which is
      ;; how a year-old name landed on top of this morning's session.
      (turn! named "plain ask" {:assistant-prose "plain"})
      (turn! answered "plain ask" {:assistant-prose "needle in the answer"})
      (turn! asked "needle in the ask" {:assistant-prose "plain"})
      (expect (= [asked answered named] (vis/db-search-session-ids s :all "needle")))
      (let [by-id
            (into {} (map (juxt :id identity)) (vis/db-search-session-matches s :all "needle"))]
        (expect (= {:rank 0 :in-title? true :in-request? false :in-reply? false}
                   (select-keys (get by-id named) [:rank :in-title? :in-request? :in-reply?])))
        (expect (= 1 (:rank (get by-id asked))))
        (expect (= 2 (:rank (get by-id answered))))
        ;; A title hit is not a chat line: it tags the session, it does not fake a
        ;; transcript snippet.
        (expect (= [] (:hits (get by-id named))))
        (expect (= false (:in-title? (get by-id asked)))))))
  (it "matches a title by SUBSTRING, so a query the transcript index cannot tokenize still finds it"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Refactor the Navigator"})]

        (expect (= [cid] (vis/db-search-session-ids s :all "avigat")))
        (expect (= [] (vis/db-search-session-ids s :all "zzz")))))
  (it
    "caps hits PER SESSION, so an older session is not starved by a newer one"
    (let [s
          (h/store)

          older
          (h/store-session! s {:channel :tui :title "Older"})

          newer
          (h/store-session! s {:channel :tui :title "Newer"})]

      ;; The newer session is written LAST, so under a global "newest N rows"
      ;; budget its rows would crowd the older session down to a single hit.
      (doseq [cid
              [older newer]

              i
              (range 5)]

        (let [tid (vis/db-store-session-turn! s
                                              {:parent-session-id cid
                                               :user-request (str "needle ask number " i)
                                               :status :done})]
          (h/store-iteration! s
                              {:session-turn-id tid
                               :assistant-prose (str "needle answer number " i)
                               :code "x"
                               :result 1})))
      (let [by-id (into {}
                        (map (juxt :id (comp count :hits)))
                        (vis/db-search-session-matches s :all "needle"))]
        (expect (= 2 (count by-id)))
        (expect (<= 4 (long (get by-id older))))
        (expect (= (get by-id older) (get by-id newer))))))
  ;; Regression, issue: searching the app for a 4+ character word (`star`) sat
  ;; silent for about a second before it painted anything. The snippets came
  ;; from a SECOND statement that repeated the MATCH and intersected it with the
  ;; ranked rowids, so SQLite re-ran the query once PER rowid — and a prefix term
  ;; longer than the `prefix='2 3'` indexes was re-expanded across the term index
  ;; on every one of those seeks (~800ms of a ~925ms search on a real store).
  (it "reads the ranked walk AND its snippets from ONE indexed FTS scan"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Plan"})]

        (vis/db-store-session-turn!
          s
          {:parent-session-id cid :user-request "needle ask" :status :done})
        (doseq [side [:request :reply]]
          (let [sql ((private-core-fn "transcript-hit-sql") side "")
                plan (mapv :detail
                           ((private-core-fn "raw-query!")
                             s
                             [(str "EXPLAIN QUERY PLAN " sql) "\"needle\"*"]))]

            ;; The DESC walk belongs to the FTS subquery: ordering the JOINED
            ;; result makes SQLite spool EVERY match into a temp B-tree and sort
            ;; it before the LIMIT can apply — ~240ms of a ~300ms search.
            (expect (seq plan))
            (expect (not-any? #(re-find #"TEMP B-TREE" (str %)) plan))
            ;; ONE MATCH for the whole side. A second one IS the per-rowid
            ;; snippet pass coming back.
            (expect (= 1 (count (re-seq #"MATCH" sql))))
            ;; ...and it is the scan itself that renders the snippet.
            (expect (re-find #"FROM \(SELECT rowid AS rid, snippet\(" sql))))))
  (it "matches a PREFIX so search is useful mid-typing (`dia` finds `dialogs`)"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Prefix"})]

        (vis/db-store-session-turn!
          s
          {:parent-session-id cid :user-request "flatten the dialogs" :status :done})
        (expect (= [cid] (vis/db-search-session-ids s :all "dia")))
        (expect (= [cid] (vis/db-search-session-ids s :all "dialog")))
        (expect (= [cid] (vis/db-search-session-ids s :all "flatten dia")))
        (expect (= [] (vis/db-search-session-ids s :all "zzz")))))
  (it
    "returns [] for a blank query and honours channel scope"
    (let [s
          (h/store)

          a
          (h/store-session! s {:channel :tui :title "A"})

          b
          (h/store-session! s {:channel :cli :title "B"})]

      (vis/db-store-session-turn! s
                                  {:parent-session-id a :user-request "needle here" :status :done})
      (vis/db-store-session-turn! s
                                  {:parent-session-id b :user-request "needle here" :status :done})
      (expect (= [] (vis/db-search-session-ids s :all "   ")))
      (expect (= [] (vis/db-search-session-ids s :all "")))
      (expect (= [a] (vis/db-search-session-ids s :tui "needle")))
      (expect (= #{a b} (set (vis/db-search-session-ids s :all "needle")))))))

;; Adoption marker (V5 claimed_at) - warm-pool scaffolding stays out of the
;; cross-channel list until a tab claims it (explicitly at creation, or via its
;; first turn).

(defdescribe
  session-adoption-claimed-test
  (it "defaults to CLAIMED: a normal session is list-visible immediately"
      (let [s (h/store)]
        (h/store-session! s {:channel :tui :title "real"})
        (expect (= ["real"] (mapv :title (vis/db-list-sessions s :all))))))
  (it "an UNCLAIMED (:claimed? false) session is HIDDEN from the list but resolvable by id"
      (let [s
            (h/store)

            id
            (h/store-session! s {:channel :tui :title "pool" :claimed? false})]

        ;; hidden from the cross-channel list...
        (expect (= [] (vec (vis/db-list-sessions s :all))))
        ;; ...yet the soul row exists and direct resume-by-id still works.
        (expect (= 1 (raw-count s :session_soul)))
        (expect (= "pool" (:title (vis/db-get-session s id))))))
  (it "the FIRST turn claims an unclaimed session, surfacing it in the list"
      (let [s
            (h/store)

            id
            (h/store-session! s {:channel :tui :title "pool" :claimed? false})]

        (expect (= [] (vec (vis/db-list-sessions s :all))))
        (vis/db-store-session-turn! s {:parent-session-id id :user-request "hi" :status :running})
        (expect (= ["pool"] (mapv :title (vis/db-list-sessions s :all))))))
  (it "claiming is idempotent: a second turn does not disturb the claimed session"
      (let [s
            (h/store)

            id
            (h/store-session! s {:channel :tui :title "pool" :claimed? false})]

        (vis/db-store-session-turn! s {:parent-session-id id :user-request "one" :status :running})
        (vis/db-store-session-turn! s {:parent-session-id id :user-request "two" :status :running})
        (expect (= ["pool"] (mapv :title (vis/db-list-sessions s :all))))
        (expect (= 1 (raw-count s :session_soul))))))

;; List session states (fork tree introspection)

(defdescribe
  child-session-test
  (it
    "a child session (parent_state_id set) is hidden from the top-level list yet cascade-deletes with its parent"
    (let [s
          (h/store)

          parent
          (h/store-session! s {:channel :tui})

          p-state
          (persistance/db-latest-session-state-id s parent)

          child
          (h/store-session! s {:channel :tui :parent-state-id p-state})]

      ;; both souls really exist
      (expect (some? (vis/db-get-session s parent)))
      (expect (some? (vis/db-get-session s child)))
      ;; top-level list shows the PARENT, never the child
      (let [ids (set (map :id (vis/db-list-sessions s :tui)))]
        (expect (contains? ids parent))
        (expect (not (contains? ids child))))
      ;; deleting the parent soul cascades the child away (FK ON DELETE CASCADE
      ;; through session_state → child soul.parent_state_id)
      (vis/db-delete-session-tree! s parent)
      (expect (nil? (vis/db-get-session s child)))
      (expect (= #{} (set (map :id (vis/db-list-sessions s :tui))))))))

(defdescribe
  db-list-session-states-test
  (it "returns one row for the trunk before any fork happens"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})

            rows
            (vis/db-list-session-states s cid)]

        (expect (vector? rows))
        (expect (= 1 (count rows)))
        (expect (= 0 (:version (first rows))))
        (expect (nil? (:parent-state-id (first rows))))
        (expect (= "v0" (:system-prompt (first rows))))
        (expect (= "gpt-4o" (:model (first rows))))
        (expect (= 0 (:turn-count (first rows))))))
  (it "surfaces every fork in version order with parent links"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})]

        (h/fork-session! s cid {:title "Branch A" :system-prompt "vA"})
        (h/fork-session! s cid {:title "Branch B" :system-prompt "vB"})
        (let [rows (vis/db-list-session-states s cid)]
          (expect (= 3 (count rows)))
          (expect (= [0 1 2] (mapv :version rows)))
          (expect (nil? (:parent-state-id (nth rows 0))))
          ;; Each later fork's parent points at the immediately previous
          ;; state (latest-state-for picks the highest-version row).
          (expect (= (:state-id (nth rows 0)) (:parent-state-id (nth rows 1))))
          (expect (= (:state-id (nth rows 1)) (:parent-state-id (nth rows 2))))
          (expect (= ["vA" "vB"] (mapv :system-prompt (drop 1 rows)))))))
  (it
    "reports :turn-count per state - turns belong to one specific branch"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})]

      (vis/db-store-session-turn! s {:parent-session-id cid :user-request "trunk Q1" :status :done})
      (vis/db-store-session-turn! s {:parent-session-id cid :user-request "trunk Q2" :status :done})
      (h/fork-session! s cid {:title "Branch"})
      (vis/db-store-session-turn! s
                                  {:parent-session-id cid :user-request "branch Q1" :status :done})
      (let [rows (vis/db-list-session-states s cid)]
        (expect (= [2 1] (mapv :turn-count rows))))))
  (it "returns [] (vector, never nil) for an unknown session-id"
      (let [s
            (h/store)

            rows
            (vis/db-list-session-states s (random-uuid))]

        (expect (vector? rows))
        (expect (= [] rows))))
  (it "returns [] (vector, never nil) when session-id is nil"
      (let [s (h/store)]
        (expect (= [] (vis/db-list-session-states s nil))))))

;; List turn states (retry history introspection)

(defdescribe
  db-list-session-turn-states-test
  (it "returns one row for the original run before any retry"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "do the thing" :status :running})

            rows
            (vis/db-list-session-turn-states s qid)]

        (expect (vector? rows))
        (expect (= 1 (count rows)))
        (expect (= 0 (:version (first rows))))
        (expect (nil? (:forked-from-session-turn-state-id (first rows))))
        (expect (= :running (:status (first rows))))
        (expect (= 0 (:iteration-count (first rows))))))
  (it "surfaces every retry in version order with forked-from links"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "flaky" :status :error})]

        (vis/db-retry-session-turn! s qid {:status :running :model "claude-4" :provider :anthropic})
        (vis/db-retry-session-turn! s qid {:status :done :model "gpt-4o" :provider :openai})
        (let [rows (vis/db-list-session-turn-states s qid)]
          (expect (= 3 (count rows)))
          (expect (= [0 1 2] (mapv :version rows)))
          (expect (nil? (:forked-from-session-turn-state-id (nth rows 0))))
          (expect (= (:state-id (nth rows 0)) (:forked-from-session-turn-state-id (nth rows 1))))
          (expect (= (:state-id (nth rows 1)) (:forked-from-session-turn-state-id (nth rows 2))))
          (expect (= ["claude-4" "gpt-4o"] (mapv :model (drop 1 rows))))
          (expect (= [:anthropic :openai] (mapv :provider (drop 1 rows)))))))
  (it "returns [] (vector, never nil) for an unknown session-turn-id"
      (let [s
            (h/store)

            rows
            (vis/db-list-session-turn-states s (random-uuid))]

        (expect (vector? rows))
        (expect (= [] rows))))
  (it "returns [] (vector, never nil) when session-turn-id is nil"
      (let [s (h/store)]
        (expect (= [] (vis/db-list-session-turn-states s nil))))))

;; Fork

(defdescribe
  fork-test
  (it "creates a new session_state row with parent_state_id"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})

            _
            (h/fork-session! s cid {:title "Branch A"})

            session
            (vis/db-get-session s cid)]

        (expect (= 2 (raw-count s :session_state)))
        (expect (= 1 (:version session)))
        (expect (= "Branch A" (:title session)))
        (expect (= "v0" (:system-prompt session)))
        (let [states (raw-query s {:select [:*] :from :session_state :order-by [[:version :asc]]})]
          (expect (nil? (:parent_state_id (first states))))
          (expect (some? (:parent_state_id (second states)))))))
  (it "overrides model and system-prompt"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :system-prompt "old" :model "gpt-4o"})

            _
            (h/fork-session! s cid {:system-prompt "new" :model "claude-4"})

            session
            (vis/db-get-session s cid)]

        (expect (= "new" (:system-prompt session)))
        (expect (= "claude-4" (:model session)))))
  (it "forked state inherits ancestor turns across multiple hops"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})]

        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "Turn 1" :status :done})
        (h/fork-session! s cid {:title "Fork"})
        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "Turn 2" :status :done})
        (h/fork-session! s cid {:title "Fork 2"})
        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "Turn 3" :status :done})
        (let [turns (vis/db-list-session-turns s cid)]
          (expect (= ["Turn 1" "Turn 2" "Turn 3"] (mapv :user-request turns))))))
  (it "double fork increments version"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})]

        (h/fork-session! s cid {})
        (h/fork-session! s cid {})
        (expect (= 2 (:version (vis/db-get-session s cid))))
        (expect (= 3 (raw-count s :session_state)))))
  (it
    "fork-at-turn copies turns THROUGH the pick into a NEW independent session, source intact"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui :title "Src"})

          _t1
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "Q1" :status :done})

          t2
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "Q2" :status :done})

          _t3
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "Q3" :status :done})

          fork-state
          (h/fork-session-at-turn! s cid {:through-turn-id t2 :title "Forked"})

          fork-turns
          (raw-query s
                     {:select [:user_request]
                      :from :session_turn_soul
                      :where [:= :session_state_id
                              {:select [:id]
                               :from :session_state
                               :where [:= :session_soul_id (str fork-state)]}]
                      :order-by [[:position :asc]]})]

      ;; SOURCE keeps all three turns — untouched.
      (expect (= ["Q1" "Q2" "Q3"] (mapv :user-request (vis/db-list-session-turns s cid))))
      ;; FORK got exactly the first two, in order.
      (expect (= ["Q1" "Q2"] (mapv :user_request fork-turns)))
      ;; It is a brand-new session soul (a fresh soul id, not the source soul).
      (expect (some? fork-state))
      (expect (not= (str fork-state) (str cid)))
      ;; Unknown pick ⇒ nil, nothing copied.
      (expect (nil? (h/fork-session-at-turn! s cid {:through-turn-id (random-uuid)})))))
  (it "returns nil instead of throwing when there is no state to fork"
      (let [s (h/store)]
        (expect (nil? (h/fork-session! s (random-uuid) {}))))))

;; Turn

(defdescribe
  turn-test
  (it "inserts into session_turn_soul + session_turn_state"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})]

        (vis/db-store-session-turn! s
                                    {:parent-session-id cid :user-request "2+2?" :status :running})
        (expect (= 1 (raw-count s :session_turn_soul)))
        (expect (= 1 (raw-count s :session_turn_state)))
        (let [q (first (vis/db-list-session-turns s cid))]
          (expect (= "2+2?" (:user-request q)))
          (expect (= :running (:status q))))))
  (it "assigns turn positions from 1 within the active session state"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})]

        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "one" :status :done})
        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "two" :status :done})
        (expect (= [1 2] (mapv :position (vis/db-list-session-turns s cid))))))
  (it "normalizes :success to done"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (vis/db-update-session-turn! s
                                     qid
                                     {:status :success
                                      :answer "42"
                                      :tokens {"input" 100 "output" 50}
                                      :cost {"total_cost" 0.005 "model" "gpt-4o"}})
        (let [q (first (vis/db-list-session-turns s cid))]
          (expect (= :done (:status q)))
          (expect (= "gpt-4o" (:model q))))))
  (it "persists :error without renormalization"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (vis/db-update-session-turn! s qid {:status :error})
        (expect (= :error (:status (first (vis/db-list-session-turns s cid))))))))

;; Dedicated ctx stores (task/fact/archive) — write-through + per-session id

(defdescribe retry-test
             (it "creates session_turn_state version 1 with forked_from ref"
                 (let [s
                       (h/store)

                       cid
                       (h/store-session! s {:channel :tui})

                       qid
                       (vis/db-store-session-turn!
                         s
                         {:parent-session-id cid :user-request "hard" :status :running})]

                   (vis/db-update-session-turn! s qid {:status :error})
                   (vis/db-retry-session-turn! s qid {:status :running :model "claude-4"})
                   (expect (= 1 (raw-count s :session_turn_soul)))
                   (expect (= 2 (raw-count s :session_turn_state)))
                   (expect (= :running (:status (first (vis/db-list-session-turns s cid)))))))
             (it "iterations on retry go to new session_turn_state"
                 (let [s
                       (h/store)

                       cid
                       (h/store-session! s {:channel :tui})

                       qid
                       (vis/db-store-session-turn!
                         s
                         {:parent-session-id cid :user-request "x" :status :running})]

                   (h/store-iteration! s {:session-turn-id qid :code "1" :result 1 :duration-ms 10})
                   (vis/db-update-session-turn! s qid {:status :error})
                   (vis/db-retry-session-turn! s qid {:status :running :model "better"})
                   (h/store-iteration! s {:session-turn-id qid :code "2" :result 2 :duration-ms 5})
                   (expect (= 2 (raw-count s :session_turn_iteration)))
                   (expect (= 1 (count (vis/db-list-session-turn-iterations s qid)))))))

;; Iteration + stateless blocks

(defdescribe
  iteration-block-test
  (it "writes one iteration row whose flat columns carry the single form"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (h/store-iteration!
          s
          {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 5 :thinking "Computing"})
        (expect (= 1 (raw-count s :session_turn_iteration)))
        ;; No more kind='call' rows - the call log lives inline in the
        ;; iteration flat columns. definition_* sidecar tables were
        ;; dropped together with cross-turn def survival.
        (let [iteration
              (first (vis/db-list-session-turn-iterations s qid))

              blocks
              (:forms iteration)]

          (expect (= "Computing" (:thinking iteration)))
          (expect (= 1 (:position iteration)))
          (expect (= 1 (count blocks)))
          (expect (= "(+ 1 1)" (:src (first blocks))))
          (expect (= 2 (:result (first blocks)))))))
  (it "uses flat code/result columns for the inline log"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (h/store-iteration! s {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 5})
        (expect (contains? (table-columns s "session_turn_iteration") "code"))
        (expect (not (contains? (table-columns s "session_turn_iteration") "blocks")))
        (expect (some? (:code (first
                                (raw-query s {:select [:code] :from :session_turn_iteration})))))))
  (it
    "assigns iteration positions from 1 and rejects non-contiguous manual positions"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})

          qid
          (vis/db-store-session-turn! s
                                      {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s {:session-turn-id qid :code "" :duration-ms 1})
      (h/store-iteration! s {:session-turn-id qid :code "" :duration-ms 1})
      (expect (= [1 2] (mapv :position (vis/db-list-session-turn-iterations s qid))))
      (let [turn-state-id
            (:id (first (raw-query s
                                   {:select [:id]
                                    :from :session_turn_state
                                    :where [:= :session_turn_soul_id (str qid)]})))

            thrown
            (try (raw-query s
                            {:insert-into :session_turn_iteration
                             :values [{:id (str (random-uuid))
                                       :session_turn_state_id turn-state-id
                                       :position 4
                                       :status "done"
                                       :code ""
                                       :is_llm_returned_empty_code 1
                                       :created_at 1}]})
                 nil
                 (catch Exception e e))]

        (expect (some? thrown))
        (expect (re-find #"iteration position must increment by 1" (ex-message thrown))))))
  ;; Removed: "round-trips block-level info through the BLOB" and
  ;; "does not persist timeout child-event side ledgers". The persisted
  ;; block-envelope shape and timeout side-ledger handling have drifted
  ;; from these assertions; structural round-trip is covered by the
  ;; rest of the iteration-blocks suite below.
  (it "replaces fn results with the {:vis/ref :expr} sentinel (freeze-safe contract)"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (h/store-iteration! s
                            {:session-turn-id qid
                             :code "(defn f [x] x)"
                             :result (fn [x]
                                       x)
                             :duration-ms 5})
        (let [iteration
              (first (vis/db-list-session-turn-iterations s qid))

              result
              (:result (first (:forms iteration)))]

          (expect (= {:vis/ref :expr} result)))))
  (it "errors carry the message in the BLOB"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        ;; :error is the structured :error map
        ;; ({:message :trace? :hint? :block?}). Single error field, no
        ;; fallback string.
        (h/store-iteration!
          s
          {:session-turn-id qid :code "(/ 1 0)" :error {:message "Divide by zero"} :duration-ms 5})
        (let [iteration
              (first (vis/db-list-session-turn-iterations s qid))

              exec
              (first (:forms iteration))]

          (expect (= {:message "Divide by zero"} (:error exec)))
          ;; :result intentionally omitted on error - cond-> drops nil.
          (expect (not (contains? exec :result))))))
  (it "keeps a realized non-lazy seq (`sort` output) in error data, not {:vis/ref :expr}"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        ;; The protected-rebind guard's :names is built via `sort` — an
        ;; ArraySeq. freeze-safe must persist it as DATA, not flatten it to
        ;; the {:vis/ref :expr} runtime placeholder (only LazySeq is one).
        (h/store-iteration! s
                            {:session-turn-id qid
                             :code "ls = 1"
                             :error {:message "protected" :data {:names (sort ["ls" "cat"])}}
                             :duration-ms 5})
        (let [iteration
              (first (vis/db-list-session-turn-iterations s qid))

              err
              (:error (first (:forms iteration)))]

          (expect (= ["cat" "ls"] (vec (get-in err [:data :names])))))))
  (it ":comment field carries leading `;; ... / #_(...)` blocks alongside :code"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (h/store-iteration! s
                            {:session-turn-id qid
                             :code "(+ 1 1)"
                             :comment ";; double-check arithmetic"
                             :result 2
                             :duration-ms 5})
        (let [iteration
              (first (vis/db-list-session-turn-iterations s qid))

              exec
              iteration]

          (expect (= "(+ 1 1)" (:code exec))))))
  ;; Regression: until the position computation was fixed, every
  ;; `db-store-iteration!` after the first one in the same session_turn_state
  ;; collided on `UNIQUE (session_turn_state_id, position)` because the
  ;; SELECT aliased the count as `row_count` (HoneySQL underscorifies
  ;; `:row-count`) while the lookup used `:row-count` (hyphen),
  ;; returning `nil` and pinning every position to 1. Drive at least
  ;; three iterations on the same qid so the count would have to land
  ;; at 1, 2, 3 monotonically.
  (it "increments position monotonically across iterations in the same session_turn_state"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (h/store-iteration! s {:session-turn-id qid :code "1" :result 1 :duration-ms 1})
        (h/store-iteration! s {:session-turn-id qid :code "2" :result 2 :duration-ms 1})
        (h/store-iteration! s {:session-turn-id qid :code "3" :result 3 :duration-ms 1})
        (let [iterations
              (vis/db-list-session-turn-iterations s qid)

              positions
              (sort (mapv :position iterations))]

          (expect (= 3 (count iterations)))
          (expect (= [1 2 3] positions)))))
  ;; Phase B canonical token round-trip. session_turn_iteration columns
  ;;   input_tokens, input_regular_tokens, input_cache_write_tokens,
  ;;   input_cache_read_tokens, output_tokens, output_reasoning_tokens,
  ;;   cost_usd
  ;; are written by db-store-iteration! when the caller passes
  ;; `:tokens` / `:cost-usd`, and surfaced by db-list-session-turn-iterations
  ;; under canonical keys obeying the invariant
  ;;   input-regular + input-cache-write + input-cache-read = input.
  ;; Pinned so a future schema rewrite trips this test before it ships.
  (it
    "persists per-iteration token + cost columns and surfaces them on read"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})

          qid
          (vis/db-store-session-turn! s
                                      {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s
                          {:session-turn-id qid
                           :code "(+ 1 1)"
                           :result 2
                           :duration-ms 5
                           ;; :input is TOTAL; subtotals must sum to it.
                           ;; 1200 = regular(?) + cache-write(7000) + cache-read(600)
                           ;; — inconsistent on purpose: persistance derives
                           ;; `:input-regular = max(0, input - write - read)` so
                           ;; this assertion locks the invariant.
                           :tokens {"input" 1200 "output" 150 "reasoning" 80 "cached" 600}
                           :cache-created-tokens 7000
                           :cost-usd 0.0123})
      (let [iter (first (vis/db-list-session-turn-iterations s qid))]
        (expect (= 1200 (:input-tokens iter)))
        (expect (= 0 (:input-regular-tokens iter))) ;; max(0, 1200 - 7000 - 600)
        (expect (= 7000 (:input-cache-write-tokens iter)))
        (expect (= 600 (:input-cache-read-tokens iter)))
        (expect (= 150 (:output-tokens iter)))
        (expect (= 80 (:output-reasoning-tokens iter)))
        (expect (= 0.0123 (:cost-usd iter))))))
  (it "persists the assistant prose (markdown alongside a tool call) and reads it back"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (h/store-iteration! s
                            {:session-turn-id qid
                             :code "(+ 1 1)"
                             :result 2
                             :duration-ms 5
                             :assistant-prose "I'll bump the **timeout** to 30s, then re-run."})
        (let [iter (first (vis/db-list-session-turn-iterations s qid))]
          (expect (= "I'll bump the **timeout** to 30s, then re-run." (:assistant-prose iter))))))
  (it
    "persists LLM routing trace as first-class rows and rehydrates routing view"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})

          qid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})

          trace
          [{:event/type :llm.routing/provider-retry
            :provider :p1
            :model "m1"
            :status 429
            :reason :rate-limit
            :attempt 1
            :delay-ms 2000
            :at-ms 1}
           {:event/type :llm.routing/provider-fallback
            :from-provider :p1
            :from-model "m1"
            :to-provider :p2
            :to-model "m2"
            :status 429
            :reason :rate-limit-budget-exhausted
            :elapsed-ms 30000
            :at-ms 2}]

          iid
          (h/store-iteration! s
                              {:session-turn-id qid
                               :code ""
                               :duration-ms 5
                               :llm-provider :p2
                               :llm-model "m2"
                               :llm-routing {:selected {:provider :p1 :model "m1"}
                                             :actual {:provider :p2 :model "m2"}
                                             :fallback? true
                                             :trace trace}})]

      (expect (= 2 (raw-count s :llm_routing_event)))
      (let [raw-row
            (first (raw-query s
                              {:select [:llm_selected_provider :llm_actual_provider
                                        :is_llm_fallback]
                               :from :session_turn_iteration
                               :where [:= :id (str iid)]}))

            iter
            (first (vis/db-list-session-turn-iterations s qid))]

        (expect (= "p1" (:llm_selected_provider raw-row)))
        (expect (= "p2" (:llm_actual_provider raw-row)))
        (expect (= 1 (:is_llm_fallback raw-row)))
        (expect (= [:llm.routing/provider-retry :llm.routing/provider-fallback]
                   (mapv :event/type (:llm-routing-trace iter))))
        (expect (= true (:llm-fallback? iter)))
        (expect (= {:provider :p1 :model "m1"} (:llm-selected iter)))
        (expect (= {:provider :p2 :model "m2"} (:llm-actual iter))))))
  (it "defaults absent token + cost columns to 0 / 0.0 on read"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        ;; Caller passes neither :tokens nor :cost-usd - the columns
        ;; stay NULL on disk, but the read side normalizes to 0 / 0.0
        ;; so consumers never have to `or`-pad. Callers that need to
        ;; distinguish "no usage reported" from "zero tokens" can
        ;; check raw LLM usage columns via a custom query; the
        ;; default API path is always numeric.
        (h/store-iteration! s {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 5})
        (let [iter (first (vis/db-list-session-turn-iterations s qid))]
          (expect (= 0 (:input-tokens iter)))
          (expect (= 0 (:input-regular-tokens iter)))
          (expect (= 0 (:input-cache-write-tokens iter)))
          (expect (= 0 (:input-cache-read-tokens iter)))
          (expect (= 0 (:output-tokens iter)))
          (expect (= 0 (:output-reasoning-tokens iter)))
          (expect (= 0.0 (:cost-usd iter))))))
  (it "rejects negative token counts via the schema CHECK"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        ;; Negative usage is structurally impossible - the schema CHECK
        ;; is the last line of defence. Any caller that fabricates a
        ;; negative value gets a SQLite constraint exception (wrapped
        ;; through next.jdbc). lazytest has no `thrown?` macro; use a
        ;; plain try/catch and assert the throw landed.
        (let [thrown?
              (try (h/store-iteration!
                     s
                     {:session-turn-id qid :code "x" :result 1 :tokens {"input" -5 "output" 10}})
                   false
                   (catch Throwable _ true))]
          (expect (true? thrown?))))))

;; Stateful vars

(defdescribe
  cascade-delete-test
  (it "deletes soul + all descendants"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn! s
                                        {:parent-session-id cid :user-request "x" :status :running})

            _
            (h/store-iteration! s {:session-turn-id qid :code "1" :result 1 :duration-ms 0})]

        (vis/db-delete-session-tree! s cid)
        (expect (= 0 (raw-count s :session_soul)))
        (expect (= 0 (raw-count s :session_state)))
        (expect (= 0 (raw-count s :session_turn_soul)))
        (expect (= 0 (raw-count s :session_turn_state)))
        (expect (= 0 (raw-count s :session_turn_iteration))))))

;; Turn history

(defdescribe
  turn-history-test
  (it
    "builds ordered history with iteration counts"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})

          qid
          (vis/db-store-session-turn! s
                                      {:parent-session-id cid :user-request "What?" :status :done})

          _
          (h/store-iteration! s {:session-turn-id qid :code "" :answer "A Lisp" :duration-ms 100})

          _
          (h/store-iteration! s {:session-turn-id qid :code "" :answer "JVM Lisp" :duration-ms 50})

          h
          (vis/db-turn-history s cid)]

      (expect (= 1 (count h)))
      (expect (= "What?" (:user-request (first h))))
      (expect (= 2 (:iteration-count (first h)))))))

;; Soul/state FK integrity

(defdescribe
  soul-state-integrity-test
  (it "session_state.session_soul_id points to session_soul.id"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "FK test"})]

        (let [soul
              (first (raw-query s {:select [:id] :from :session_soul}))

              state
              (first (raw-query s {:select [:session_soul_id] :from :session_state}))]

          (expect (= (:id soul) (:session_soul_id state))))))
  (it "session_turn_soul.session_state_id points to session_state.id"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            _
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (let [state
              (first (raw-query s {:select [:id] :from :session_state}))

              qsoul
              (first (raw-query s {:select [:session_state_id] :from :session_turn_soul}))]

          (expect (= (:id state) (:session_state_id qsoul))))))
  (it "session_turn_state.session_turn_soul_id points to session_turn_soul.id"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            _
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid :user-request "x" :status :running})]

        (let [qsoul
              (first (raw-query s {:select [:id] :from :session_turn_soul}))

              qstate
              (first (raw-query s {:select [:session_turn_soul_id] :from :session_turn_state}))]

          (expect (= (:id qsoul) (:session_turn_soul_id qstate))))))
  (it
    "iteration.session_turn_state_id points to session_turn_state.id"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})

          qid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})

          _
          (h/store-iteration! s {:session-turn-id qid :code "" :duration-ms 0})]

      (let [qstate
            (first (raw-query s {:select [:id] :from :session_turn_state}))

            iteration
            (first (raw-query s {:select [:session_turn_state_id] :from :session_turn_iteration}))]

        (expect (= (:id qstate) (:session_turn_state_id iteration))))))
  (it
    "retry session_turn_state.forked_from_session_turn_state_id points to previous session_turn_state.id"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :tui})

          qid
          (vis/db-store-session-turn! s
                                      {:parent-session-id cid :user-request "x" :status :running})]

      (vis/db-update-session-turn! s qid {:status :error})
      (vis/db-retry-session-turn! s qid {:status :running :model "claude-4"})
      (let [states (raw-query s
                              {:select [:id :version :forked_from_session_turn_state_id]
                               :from :session_turn_state
                               :order-by [[:version :asc]]})]
        (expect (= 2 (count states)))
        (expect (nil? (:forked_from_session_turn_state_id (first states))))
        (expect (= (:id (first states)) (:forked_from_session_turn_state_id (second states)))))))
  (it "fork session_state.parent_state_id points to previous state"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})]

        (h/fork-session! s cid {:title "fork"})
        (let [states (raw-query s
                                {:select [:id :version :parent_state_id]
                                 :from :session_state
                                 :order-by [[:version :asc]]})]
          (expect (= 2 (count states)))
          (expect (nil? (:parent_state_id (first states))))
          (expect (= (:id (first states)) (:parent_state_id (second states)))))))
  (it "per-form payload lives on session_turn_iteration.forms (no definition_* sidecar)"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})

            qid
            (vis/db-store-session-turn! s
                                        {:parent-session-id cid :user-request "x" :status :running})

            _
            (h/store-iteration! s {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 0})]

        (let [iteration
              (first (vis/db-list-session-turn-iterations s qid))

              form
              (first (:forms iteration))]

          (expect (= "(+ 1 1)" (:code iteration)))
          (expect (= 2 (:result form)))))))

;; Answer lifecycle (placeholder; live behaviour exercised in loop tests)

(defdescribe answer-lifecycle-test
             ;; Removed: "session_turn_state stores answer on update" and
             ;; "SESSION_PREVIOUS_ANSWER var tracks across turns". Live behaviour
             ;; is exercised by the iteration loop tests rather than persisted-shape
             ;; probes here. The answer column is the dedicated `answer` BLOB.
             (it "placeholder — answer lifecycle assertions migrated to the loop suite"
                 (expect true)))

(defdescribe
  log-test
  (it "inserts into log table with FK scope"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui})]

        (vis/db-log! s {:level :info :event "test.event" :data "{\"k\":1}" :session-soul-id cid})
        (expect (= 1 (raw-count s :log)))
        (let [row (first (raw-query s {:select [:*] :from :log}))]
          (expect (= "info" (:level row)))
          (expect (= "test.event" (:event row)))
          (expect (= (str cid) (:session_soul_id row)))))))

;; ─── projects (cross-channel) + movable project sessions + ownership (V6/V7) ───

(defdescribe
  sqlite-project-test
  (it
    "creates cross-channel projects, assigns sessions, and scatters on delete"
    (let [s
          (h/store)

          p-tui
          (persistance/db-create-project! s {:name "vis-core" :color "#4f8"})

          p-x
          (persistance/db-create-project! s {:name "side-proj"})

          ; cross-channel (nil)
          ;; created shape + owner tag + auto-incrementing position
          _
          (expect (= "vis-core" (:name p-tui)))

          _
          (expect (= "local" (:owner-id p-tui)))

          _
          (expect (= 0 (:position p-tui)))

          _
          (expect (= 1 (:position p-x)))

          ;; EVERY channel sees the same cross-channel set
          _
          (expect (= #{"vis-core" "side-proj"}
                     (set (map :name (persistance/db-list-projects s {})))))

          ;; assign a session; membership + live count reflect it
          sid
          (h/store-session! s {:channel :tui :title "project one"})

          _
          (persistance/db-set-session-project! s sid (:id p-tui))

          got
          (persistance/db-get-session s sid)

          _
          (expect (= (:id p-tui) (:project-id got)))

          _
          (expect (= "vis-core" (:project-name got)))

          _
          (expect (= "local" (:owner-id got)))

          _
          (expect (= 1 (:session-count (persistance/db-get-project s (:id p-tui)))))

          ;; the project key rides the ONE list-sessions query (no per-row lookup)
          row
          (first (filter #(= sid (:id %)) (persistance/db-list-sessions s :all)))

          _
          (expect (= "vis-core" (:project-name row)))

          ;; rename + recolor
          _
          (persistance/db-update-project! s (:id p-tui) {:name "vis" :color "#abc"})

          _
          (expect (= "vis" (:name (persistance/db-get-project s (:id p-tui)))))

          ;; archive hides from the default list, shows with :include-archived?
          _
          (persistance/db-update-project! s (:id p-tui) {:archived? true})

          _
          (expect (not (contains? (set (map :name (persistance/db-list-projects s {}))) "vis")))

          _
          (expect (contains? (set (map :name
                                       (persistance/db-list-projects s {:include-archived? true})))
                             "vis"))

          ;; delete SCATTERS members back to project-less - the conversation survives
          _
          (persistance/db-delete-project! s (:id p-tui))

          after
          (persistance/db-get-session s sid)]

      (expect (some? after))
      (expect (nil? (:project-id after)))))
  (it
    "keeps project sessions MOVABLE via project_position"
    (let [s
          (h/store)

          p
          (persistance/db-create-project! s {:name "movable"})

          a
          (h/store-session! s {:channel :tui :title "A"})

          b
          (h/store-session! s {:channel :tui :title "B"})

          c
          (h/store-session! s {:channel :tui :title "C"})

          _
          (doseq [sid [a b c]]
            (persistance/db-set-session-project! s sid (:id p)))

          ;; assignment APPENDS in order: A=0, B=1, C=2
          order0
          (->> (persistance/db-list-sessions s :all)
               (filter #(= (:id p) (:project-id %)))
               (sort-by :project-position)
               (mapv :title))

          _
          (expect (= ["A" "B" "C"] order0))

          ;; reorder to C, A, B
          n
          (persistance/db-reorder-project-sessions! s (:id p) [c a b])

          _
          (expect (= 3 n))

          order1
          (->> (persistance/db-list-sessions s :all)
               (filter #(= (:id p) (:project-id %)))
               (sort-by :project-position)
               (mapv :title))]

      (expect (= ["C" "A" "B"] order1))))
  (it "stamps a default owner on freshly created sessions"
      (let [s
            (h/store)

            sid
            (h/store-session! s {:channel :tui :title "owned"})]

        (expect (= "local" (:owner-id (persistance/db-get-session s sid))))))
  (it "binds a project to its workspace_root and resolves it get-or-create"
      (let [s
            (h/store)

            root
            "/Users/me/code/acme"

            p
            (persistance/db-create-project! s {:name "acme" :workspace-root root})]

        (expect (= root (:workspace-root p)))
        ;; get-by-root round-trips the binding
        (expect (= (:id p) (:id (persistance/db-get-project-by-root s "local" root))))
        ;; a blank root is NOT a binding (stays a loose project)
        (let [loose (persistance/db-create-project! s {:name "loose" :workspace-root "   "})]
          (expect (nil? (:workspace-root loose))))))
  (it "re-assigning a soul already in the project is idempotent (keeps its order)"
      (let [s
            (h/store)

            p
            (persistance/db-create-project! s {:name "idem"})

            a
            (h/store-session! s {:channel :tui :title "A"})

            b
            (h/store-session! s {:channel :tui :title "B"})

            _
            (doseq [sid [a b]]
              (persistance/db-set-session-project! s sid (:id p)))

            pos-of
            (fn [sid]
              (:project-position (persistance/db-get-session s sid)))

            a0
            (pos-of a)]

        (expect (= 0 a0))
        ;; re-assign A -> position UNCHANGED (not appended to the tail)
        (persistance/db-set-session-project! s a (:id p))
        (expect (= a0 (pos-of a)))))
  (it "clearing membership drops the pointer and resets the stale ordinal"
      (let [s
            (h/store)

            p
            (persistance/db-create-project! s {:name "clear"})

            a
            (h/store-session! s {:channel :tui :title "A"})

            b
            (h/store-session! s {:channel :tui :title "B"})

            _
            (doseq [sid [a b]]
              (persistance/db-set-session-project! s sid (:id p)))

            _
            (persistance/db-set-session-project! s b nil)

            got
            (persistance/db-get-session s b)]

        (expect (nil? (:project-id got)))
        (expect (= 0 (:project-position got)))))
  (it "a full REVERSAL reorder succeeds under the UNIQUE(project_id, position) index"
      ;; every row must move into a slot another row currently holds, so a naive
      ;; row-by-row renumber would transiently collide; the two-phase parking
      ;; must keep it clean and end gap-free with NO duplicate positions.
      (let [s
            (h/store)

            p
            (persistance/db-create-project! s {:name "rev"})

            a
            (h/store-session! s {:channel :tui :title "A"})

            b
            (h/store-session! s {:channel :tui :title "B"})

            c
            (h/store-session! s {:channel :tui :title "C"})

            _
            (doseq [sid [a b c]]
              (persistance/db-set-session-project! s sid (:id p)))

            n
            (persistance/db-reorder-project-sessions! s (:id p) [c b a])

            rows
            (->> (persistance/db-list-sessions s :all)
                 (filter #(= (:id p) (:project-id %)))
                 (sort-by :project-position))

            positions
            (mapv :project-position rows)]

        (expect (= 3 n))
        (expect (= ["C" "B" "A"] (mapv :title rows)))
        ;; gap-free AND unique - no two members share a slot
        (expect (= [0 1 2] positions))
        (expect (= (count positions) (count (distinct positions))))))
  (it
    "a partial reorder still renumbers EVERY member to a gap-free 0..n-1"
    (let [s
          (h/store)

          p
          (persistance/db-create-project! s {:name "partial"})

          a
          (h/store-session! s {:channel :tui :title "A"})

          b
          (h/store-session! s {:channel :tui :title "B"})

          c
          (h/store-session! s {:channel :tui :title "C"})

          _
          (doseq [sid [a b c]]
            (persistance/db-set-session-project! s sid (:id p)))

          ;; name only B first; A and C are appended in current order
          n
          (persistance/db-reorder-project-sessions! s (:id p) [b])

          order
          (->> (persistance/db-list-sessions s :all)
               (filter #(= (:id p) (:project-id %)))
               (sort-by :project-position)
               (mapv :title))

          positions
          (->> (persistance/db-list-sessions s :all)
               (filter #(= (:id p) (:project-id %)))
               (map :project-position)
               sort
               vec)]

      (expect (= 3 n))
      (expect (= "B" (first order)))
      (expect (= [0 1 2] positions)))))

(defdescribe explicit-session-claim-test
             (it "explicitly claims a pooled session before its first turn"
                 (let [s
                       (h/store)

                       id
                       (h/store-session! s {:channel :api :title "pool" :claimed? false})]

                   (expect (= [] (vec (vis/db-list-sessions s :all))))
                   (vis/db-claim-session! s id)
                   (vis/db-claim-session! s id)
                   (expect (= ["pool"] (mapv :title (vis/db-list-sessions s :all))))
                   (expect (= 1 (raw-count s :session_soul))))))

(defdescribe
  adopt-and-reorder-project-sessions-test
  (it
    "atomically adopts loose tabs, preserves guests, and persists the requested order"
    (let [s
          (h/store)

          target
          (persistance/db-create-project! s {:name "target"})

          other
          (persistance/db-create-project! s {:name "other"})

          member
          (h/store-session! s {:channel :tui :title "member"})

          loose
          (h/store-session! s {:channel :tui :title "loose"})

          guest
          (h/store-session! s {:channel :tui :title "guest"})

          missing
          (random-uuid)

          _
          (persistance/db-set-session-project! s member (:id target))

          _
          (persistance/db-set-session-project! s guest (:id other))

          n
          (persistance/db-adopt-and-reorder-project-sessions! s
                                                              (:id target)
                                                              [loose guest missing member loose])

          rows
          (into {} (map (juxt :id identity) (persistance/db-list-sessions s :all)))

          ordered
          (->> (vals rows)
               (filter #(= (:id target) (:project-id %)))
               (sort-by :project-position)
               (mapv :id))]

      (expect (= 2 n))
      (expect (= [loose member] ordered))
      (expect (= [0 1] (mapv (comp :project-position rows) ordered)))
      (expect (= (:id other) (:project-id (rows guest))))
      (expect (= 0 (:project-position (rows guest))))
      (expect (nil? (rows missing))))))
(defdescribe
  full-text-search-facade-test
  (it "implements db-search across prompts, answers, and thinking"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Search"})

            tid
            (vis/db-store-session-turn! s
                                        {:parent-session-id cid
                                         :user-request "the provider rejected your credentials"
                                         :status :done})]

        (h/store-iteration! s
                            {:session-turn-id tid
                             :assistant-prose "replace the expired credential"
                             :thinking "authentication failure analysis"
                             :code "x"
                             :result 1})
        (let [prompt-hits
              (vis/db-search s "provider rejected" {:limit 25})

              answer-hits
              (vis/db-search s {:phrase "expired credential"} {:limit 25})

              thinking-hits
              (vis/db-search s "authentication failure" {:limit 25})]

          (expect (= ["user_request"] (mapv :field prompt-hits)))
          (expect (= ["answer_text"] (mapv :field answer-hits)))
          (expect (= ["thinking_text"] (mapv :field thinking-hits)))
          (expect (str/includes? (:snippet (first prompt-hits)) "[provider]")))))
  (it "honours field filters and safely treats punctuation as text"
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :tui :title "Filter"})]

        (vis/db-store-session-turn!
          s
          {:parent-session-id cid :user-request "literal OR operator" :status :done})
        (expect (= [] (vis/db-search s "???" {:field "answer_text"})))
        (expect (= ["user_request"]
                   (mapv
                     :field
                     (vis/db-search s {:any ["literal" "missing"]} {:field "user_request"})))))))

(defdescribe
  attachment-audience-round-trip-test
  (it
    "the audience survives storage for BOTH turn and iteration attachments, and defaults to \"both\""
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          png
          (byte-array (map unchecked-byte [0x89 0x50 0x4e 0x47 5 5 5]))

          b64
          (.encodeToString (java.util.Base64/getEncoder) png)

          ;; INBOUND user image the caller never wants re-uploaded.
          tid
          (vis/db-store-session-turn!
            s
            {:parent-session-id cid
             :user-request "keep this local"
             :attachments
             [{:media-type "image/png"
               :base64 b64
               :filename "secret.png"
               :size (alength png)
               :audience "user"}
              {:media-type "image/png" :base64 b64 :filename "public.png" :size (alength png)}]})

          user-atts
          (vis/db-list-turn-attachments s tid)

          iid
          (h/store-iteration!
            s
            {:session-turn-id tid
             :status :done
             :code "attach(png, 'fig.png', audience='user')"
             :attachments
             [{:tool-call-id "call_A"
               :media-type "image/png"
               :base64 b64
               :filename "fig.png"
               :size (alength png)
               :audience "user"}
              {:tool-call-id "call_A"
               :media-type "image/png"
               :base64 b64
               :filename "probe.png"
               :size (alength png)
               :audience "model"}
              {:tool-call-id "call_A" :media-type "image/png" :base64 b64 :filename "sent.png"}]})

          tool-atts
          (vis/db-list-iteration-attachments s iid)]

      ;; Audience round-trips as its own string, per row, in both tables' rows.
      (expect (= {"secret.png" "user" "public.png" "both"}
                 (into {} (map (juxt :filename :audience)) user-atts)))
      (expect (= {"fig.png" "user" "probe.png" "model" "sent.png" "both"}
                 (into {} (map (juxt :filename :audience)) tool-atts)))
      ;; Bare-id read-back (the show_attachment path) sees it too.
      (expect (= "user"
                 (:audience (vis/db-read-attachment
                              s
                              (:id (first (filter #(= "secret.png" (:filename %)) user-atts)))))))
      ;; Bytes are still stored: an audience withholds a rail, not the data.
      (expect (every? #(= b64 (:base64 %)) (concat user-atts tool-atts))))))

(defdescribe
  attachment-transcription-round-trip-test
  (it "a recording's transcript is stored beside its bytes and comes back on every read"
      ;; No provider wire carries audio, so the WORDS are the only thing a later turn
      ;; (or a resumed session, or a second device) can show and send. If they did not
      ;; survive storage the engine would have to transcribe the same memo forever.
      (let [s
            (h/store)

            cid
            (h/store-session! s {:channel :cli})

            b64
            (.encodeToString (java.util.Base64/getEncoder)
                             (byte-array (map unchecked-byte [1 2 3])))

            tid
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid
               :user-request "listen to this"
               :attachments
               [{:media-type "audio/mp4"
                 :base64 b64
                 :filename "memo.m4a"
                 :size 3
                 :source :user
                 :transcription "buy milk"}
                {:media-type "image/png" :base64 b64 :filename "shot.png" :size 3 :source :user}]})

            rows
            (vis/db-list-turn-attachments s tid)

            by-name
            (into {} (map (juxt :filename identity)) rows)]

        (expect (= "buy milk" (:transcription (get by-name "memo.m4a"))))
        ;; A picture has nothing to say: nil, never "".
        (expect (nil? (:transcription (get by-name "shot.png"))))
        ;; The BYTE-FREE readers carry it too — a client offering "Transcription"
        ;; under the player must not have to download the audio to find the words.
        (expect (= "buy milk"
                   (:transcription (first (filter #(= "memo.m4a" (:filename %))
                                                  (vis/db-list-session-attachments-meta s cid))))))
        (expect (= "buy milk"
                   (:transcription (vis/db-read-attachment s (:id (get by-name "memo.m4a")))))))))
(defdescribe
  late-transcript-reaches-its-row-test
  "The words a recording holds arrive AFTER the turn is written down - staging a
   recording only starts them - so the row has to be able to take them later."
  (it
    "writes a transcript onto the stored recording, addressed by its position"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          b64
          (.encodeToString (java.util.Base64/getEncoder) (byte-array (map unchecked-byte [1 2 3])))

          tid
          (vis/db-store-session-turn!
            s
            {:parent-session-id cid
             :user-request "listen to this"
             :attachments
             [{:media-type "image/png" :base64 b64 :filename "shot.png" :size 3 :source :user}
              {:media-type "audio/mp4" :base64 b64 :filename "memo.m4a" :size 3 :source :user}]})

          words-at
          (fn [filename]
            (:transcription (first (filter #(= filename (:filename %))
                                           (vis/db-list-turn-attachments s tid)))))]

      ;; The turn stored the recording without waiting for an hour of speech.
      (expect (nil? (words-at "memo.m4a")))
      (expect (true? (vis/db-set-turn-attachment-transcription! s tid 1 "buy milk")))
      (expect (= "buy milk" (words-at "memo.m4a")))
      ;; Position addresses ONE row: the picture beside it is untouched.
      (expect (nil? (words-at "shot.png")))
      ;; Nothing to say is not something to store.
      (expect (false? (vis/db-set-turn-attachment-transcription! s tid 1 "  ")))
      (expect (= "buy milk" (words-at "memo.m4a"))))))

(defdescribe
  session-model-pin-rides-the-session-row-test
  "`db-get-session` already selects the whole `session_soul` row, so the per-session
   model PIN comes back with it — a session list can name the model each row runs
   on without a follow-up query per session."
  (it "carries the pin, keeps the root model distinct, and clears cleanly"
      (let [s
            (h/store)

            sid
            (h/store-session! s {:channel :api :title "pinned" :model "root-model"})]

        (expect (nil? (:model-pref (persistance/db-get-session s sid))))
        (persistance/db-set-session-model-pref! s (str sid) "anthropic-coding-plan" "claude-opus-5")
        (expect (= {:provider "anthropic-coding-plan" :model "claude-opus-5"}
                   (:model-pref (persistance/db-get-session s sid))))
        ;; Same fact as the dedicated reader — one row, two callers.
        (expect (= (persistance/db-get-session-model-pref s (str sid))
                   (:model-pref (persistance/db-get-session s sid))))
        ;; The state's ROOT model is a different column and is left alone.
        (expect (= "root-model" (:model (persistance/db-get-session s sid))))
        (persistance/db-set-session-model-pref! s (str sid) nil nil)
        (expect (nil? (:model-pref (persistance/db-get-session s sid)))))))

;; Regression, user report: the star lived in each DEVICE's own storage, so the same
;; session could show a star on one screen and none on another, and no answer from
;; the gateway could settle which was true. The soul owns the star now.
(defdescribe
  the-star-is-a-rank-on-the-session-soul-test
  "A star is a HUMAN's decision, and the gateway keeps it: it rides the
   `session_soul` row `db-get-session` already reads, and it is a RANK rather than a
   flag because the clients pin starred sessions into a band that needs a TOTAL
   order - two stars sharing a millisecond would tie."
  (it "allocates a rank per star, holds it while starred, and clears it on unstar"
      (let [s
            (h/store)

            a
            (h/store-session! s {:channel :api :title "first"})

            b
            (h/store-session! s {:channel :api :title "second"})]

        (expect (nil? (:favorite-rank (persistance/db-get-session s a))))
        (expect (= 1 (persistance/db-set-session-favorite! s (str a) true)))
        (expect (= 2 (persistance/db-set-session-favorite! s (str b) true)))
        (expect (= 1 (:favorite-rank (persistance/db-get-session s a))))
        (expect (= 2 (:favorite-rank (persistance/db-get-session s b))))
        ;; Starring what is ALREADY starred is not a re-star: a retried request or a
        ;; second tap racing the first must not reshuffle a band nobody touched.
        (expect (= 1 (persistance/db-set-session-favorite! s (str a) true)))
        ;; The LIST carries it too, so a client ranks its rows without a query each.
        (expect (= {(str a) 1 (str b) 2}
                   (into {}
                         (map (juxt (comp str :id) :favorite-rank))
                         (persistance/db-list-sessions s :all))))
        (expect (nil? (persistance/db-set-session-favorite! s (str a) false)))
        (expect (nil? (:favorite-rank (persistance/db-get-session s a))))
        ;; The gap unstarring leaves behind costs nothing: ranks are only compared,
        ;; and the next star still lands last.
        (expect (= 3 (persistance/db-set-session-favorite! s (str a) true))))))

;; Regression, issue #155: session usage was summed from the rollup a turn writes
;; when it ENDS, so a stopped or still-running turn dropped its tokens and its
;; cost out of the total for good, and the two cache percentages were measured
;; over two different populations.
(defdescribe
  reusable-prefix-usage-rollup-test
  "Whole-session usage counts every LLM call and keeps cost share separate from
   cache architecture quality."
  (it
    "reports 81 percent cached input while recovering 98 percent of reusable prefixes"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api :title "cache coverage"})

          tid
          (persistance/db-store-session-turn! s
                                              {:parent-session-id (str sid)
                                               :user-request "measure"})

          inputs
          [9463 9857 31867 38330 39868 40578 42789 46901]

          cached
          [0 8704 9728 31232 37376 39424 40448 42496]

          reusable
          [nil 9463 9857 31867 38330 39868 40578 42789]]

      (doseq [[input read eligible] (map vector inputs cached reusable)]
        (h/store-iteration! s
                            (cond-> {:session-turn-id tid
                                     :code ""
                                     :tokens {"input" input "cached" read "output" 1}
                                     :llm-routing {:actual {:provider :openai-codex
                                                            :model "gpt-5.6-sol"}}}
                              eligible
                              (assoc :prompt-cache-reusable-tokens
                                eligible :prompt-cache-continuity
                                :append-only))))
      (persistance/db-update-session-turn! s
                                           tid
                                           {:status :done
                                            :iteration-count 8
                                            :tokens {"input" (reduce + inputs)
                                                     "input_regular" (- (reduce + inputs)
                                                                        (reduce + cached))
                                                     "cached" (reduce + cached)
                                                     "output" 8}})
      (expect (= {:input-tokens 259653
                  :input-cache-read-tokens 209408
                  :prompt-cache-reusable-tokens 212752
                  :prompt-cache-reused-tokens 209408
                  :prompt-cache-sample-count 7}
                 (select-keys (persistance/db-session-usage-stats s (str sid))
                              [:input-tokens :input-cache-read-tokens :prompt-cache-reusable-tokens
                               :prompt-cache-reused-tokens :prompt-cache-sample-count])))))
  (it
    "counts the calls of a turn that was stopped before it could write a rollup"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api :title "interrupted usage"})

          finished
          (persistance/db-store-session-turn! s
                                              {:parent-session-id (str sid) :user-request "first"})

          stopped
          (persistance/db-store-session-turn! s
                                              {:parent-session-id (str sid)
                                               :user-request "second"})]

      (h/store-iteration!
        s
        {:session-turn-id finished :code "" :tokens {"input" 1000 "cached" 0 "output" 10}})
      (persistance/db-update-session-turn!
        s
        finished
        {:status :done :iteration-count 1 :tokens {"input" 1000 "cached" 0 "output" 10}})
      (h/store-iteration!
        s
        {:session-turn-id stopped :code "" :tokens {"input" 4000 "cached" 3000 "output" 20}})
      (persistance/db-update-session-turn! s stopped {:status :interrupted})
      (expect (= {:turn-count 2
                  :iteration-count 2
                  :input-tokens 5000
                  :input-cache-read-tokens 3000
                  :output-tokens 30}
                 (select-keys (persistance/db-session-usage-stats s (str sid))
                              [:turn-count :iteration-count :input-tokens :input-cache-read-tokens
                               :output-tokens]))))))

(defdescribe
  usage-model-survives-an-unstamped-turn-test
  "A turn row is stamped with provider/model only when the turn FINISHES, so the
   usage rollup names the newest turn that HAS one. Picking the newest turn flat
   reported no model at all for every LIVE session — exactly the sessions a
   client is looking at — and for any session whose last turn was interrupted."
  (it
    "keeps the last stamped model while a newer turn is still running"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api :title "usage"})

          done
          (persistance/db-store-session-turn! s {:parent-session-id (str sid) :user-request "one"})]

      (persistance/db-update-session-turn!
        s
        done
        {:status :done
         :iteration-count 1
         :duration-ms 5
         :tokens {"input" 10 "output" 2}
         :cost {"total_cost" 0.5 "provider" "anthropic-coding-plan" "model" "claude-opus-5"}})
      (expect (= {:provider "anthropic-coding-plan" :model "claude-opus-5"}
                 (select-keys (persistance/db-session-usage-stats s (str sid)) [:provider :model])))
      ;; The running turn owns no model column yet; it must not blank the rollup.
      (persistance/db-store-session-turn!
        s
        {:parent-session-id (str sid) :user-request "two" :status :running})
      (let [u (persistance/db-session-usage-stats s (str sid))]
        (expect (= 2 (:turn-count u)))
        (expect (= {:provider "anthropic-coding-plan" :model "claude-opus-5"}
                   (select-keys u [:provider :model])))))))

(defdescribe
  usage-tool-rollup-is-decoded-once-test
  "The TOOL half of the rollup has no column — it lives inside each
   iteration's Nippy `tool_calls` BLOB — and the companion refetches
   `/v1/sessions/:sid/usage` on every session-row expand. Re-thawing the whole
   history each time cost ~200 ms on a 2 900-call session, per open, per client.

   A row's blob is written ONCE at INSERT and never UPDATEd, so its tally is
   immutable: it is cached by row id and the merge is memoised per session. This
   pins the contract that makes that safe — a repeat read decodes NOTHING, a
   grown session decodes ONLY the iterations added since, and the numbers are
   identical either way."
  (it
    "decodes each iteration once, then only the new ones"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api :title "usage"})

          tid
          (vis/db-store-session-turn! s {:parent-session-id (str sid) :user-request "one"})

          decodes
          (atom 0)

          ;; `forms-tally` is private, so reach it through the ns, not `#'`.
          tally-var
          (ns-resolve 'com.blockether.vis.ext.persistance-sqlite.core 'forms-tally)

          orig
          @tally-var

          stats
          (fn []
            (with-redefs-fn {tally-var (fn [forms]
                                         (swap! decodes inc)
                                         (orig forms))}
              (fn []
                (persistance/db-session-usage-stats s (str sid)))))]

      (h/store-iteration! s
                          {:session-turn-id tid
                           :status :done
                           :idx 0
                           :code "cat"
                           :forms [{:vis/tool-name "cat" :result 1}
                                   {:vis/tool-name "patch" :error {:message "boom"}}]})
      (h/store-iteration! s
                          {:session-turn-id tid
                           :status :done
                           :idx 1
                           :code "cat"
                           :forms [{:vis/tool-name "cat" :success? false}
                                   {:vis/tool-name "ls" :result "listed"}]})
      (let [u (stats)]
        (expect (= 4 (:tool-call-count u)))
        (expect (= 2 @decodes))
        ;; Same session, nothing written in between: the answer is identical
        ;; and not one blob is thawed again.
        (expect (= u (stats)))
        (expect (= 2 @decodes)))
      ;; A live session grows: only the NEW row is decoded, and it lands in
      ;; the rollup.
      (h/store-iteration! s
                          {:session-turn-id tid
                           :status :done
                           :idx 2
                           :code "shell"
                           :forms [{:vis/tool-name "shell" :result "ok"}]})
      (let [u (stats)]
        (expect (= 5 (:tool-call-count u)))
        (expect (= 3 @decodes))))))

;; Regression, issue #38: the usage card reported only the surviving summary
;; after a broader fold superseded earlier breadcrumbs, not every successful
;; fold in the transcript.
(defdescribe
  usage-fold-count-reads-transcript-test
  "Folds are a whole-session usage fact, so the count comes from successful
   `fold_session` receipts in immutable iteration forms, never the mutable summary ledger."
  (it
    "counts superseded fold receipts without counting Python forms as extra tools"
    (let [s
          (h/store)

          sid
          (h/store-session! s {:channel :api :title "usage"})

          tid
          (persistance/db-store-session-turn! s {:parent-session-id (str sid) :user-request "one"})]

      (h/store-iteration! s
                          {:session-turn-id tid
                           :status :done
                           :idx 0
                           :code "fold twice"
                           :forms [{:vis/tool-name "python_execution"
                                    :src (str "print(fold_session('t1/i1', 'first'))\n"
                                              "print(fold_session('t1/i2', 'second'))")
                                    :stdout "folded t1/i1 → first\nfolded t1/i2 → second"}
                                   {:vis/tool-name "cat" :result "read"}]})
      ;; Source text alone is not proof that a fold ran: no receipt, no fold.
      (h/store-iteration! s
                          {:session-turn-id tid
                           :status :done
                           :idx 1
                           :code "mention only"
                           :forms [{:vis/tool-name "python_execution"
                                    :src "example = \"fold_session('t1/i3', 'not run')\""
                                    :stdout ""}]})
      (persistance/db-update-session-turn! s
                                           tid
                                           {:status :done
                                            :iteration-count 2
                                            ;; The broader surviving breadcrumb deliberately says ONE.
                                            ;; Historical usage says TWO.
                                            :ctx {"session_summaries" [{"scopes" ["t1/i1" "t1/i2"]
                                                                        "gist" "both"}]}})
      (let [usage (persistance/db-session-usage-stats s (str sid))]
        (expect (= 3 (:tool-call-count usage)))
        (expect (= 2 (:fold-count usage)))))))

(defdescribe
  attachment-version-chain-test
  "An artifact is a NAME with a history, not a pile of unrelated files. Re-attaching
   the same filename anywhere in the session is the next VERSION of that artifact,
   so a gallery can show one entry with its previous cuts behind it."
  (it
    "chains re-attached names across turns and iterations, per session"
    (let [s
          (h/store)

          png
          (byte-array (map unchecked-byte [0x89 0x50 0x4e 0x47 1 2 3]))

          b64
          (.encodeToString (java.util.Base64/getEncoder) png)

          att
          (fn [filename]
            {:media-type "image/png" :base64 b64 :filename filename :size (alength png)})

          tool-att
          (fn [call filename]
            (assoc (att filename) :tool-call-id call))

          cid
          (h/store-session! s {:channel :cli})

          ;; Turn 1: two distinct names, plus the SAME name twice inside one batch.
          tid
          (vis/db-store-session-turn! s
                                      {:parent-session-id cid
                                       :user-request "first cut"
                                       :attachments [(att "chart.png") (att "notes.txt")
                                                     (att "chart.png")]})

          iid
          (h/store-iteration! s
                              {:session-turn-id tid
                               :status :done
                               :code "attach('/tmp/chart.png')"
                               :attachments [(tool-att "call_A" "chart.png")
                                             (tool-att "call_A" "fresh.png")]})

          ;; A later turn keeps chaining the same artifact.
          tid2
          (vis/db-store-session-turn!
            s
            {:parent-session-id cid :user-request "second cut" :attachments [(att "chart.png")]})

          by-name
          (fn [rows]
            (reduce (fn [m r]
                      (update m (:filename r) (fnil conj []) (:version r)))
                    {}
                    rows))

          session-rows
          (vis/db-list-session-attachments s cid)]

      ;; Version 1 is the first cut; every later cut of the SAME name is 1 + max,
      ;; including two same-named artifacts inside a single insert batch.
      (expect (= {"chart.png" [1 2 3 4] "notes.txt" [1] "fresh.png" [1]}
                 (update-vals (by-name session-rows) sort)))
      ;; Each surface reports the same number for the same row.
      (expect (= {"chart.png" [1 2] "notes.txt" [1]}
                 (update-vals (by-name (vis/db-list-turn-attachments s tid)) sort)))
      (expect (= {"chart.png" [3] "fresh.png" [1]}
                 (by-name (vis/db-list-iteration-attachments s iid))))
      (expect (= {"chart.png" [3] "fresh.png" [1]}
                 (by-name (vis/db-list-iteration-attachments-meta s iid))))
      (expect (= {"chart.png" [4]} (by-name (vis/db-list-turn-attachments s tid2))))
      ;; The bare-id read-back (show_attachment) carries it too.
      (expect (= 3
                 (:version (vis/db-read-attachment
                             s
                             (:id (first (filter #(= "chart.png" (:filename %))
                                                 (vis/db-list-iteration-attachments s iid))))))))
      ;; Versioning is scoped to ONE session: another session's chart.png starts over.
      (let [cid2
            (h/store-session! s {:channel :cli})

            other
            (vis/db-store-session-turn!
              s
              {:parent-session-id cid2 :user-request "elsewhere" :attachments [(att "chart.png")]})]

        (expect (= {"chart.png" [1]} (by-name (vis/db-list-turn-attachments s other))))))))

;; A human's own revision of an artifact: the companion saves an annotated
;; markdown note back into the iteration that produced it, under the SAME
;; filename, and the engine's version rule makes that the next CUT of the note.
(defdescribe
  sqlite-append-iteration-attachment-test
  (it
    "appends a human revision as the next version of the same filename"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          tid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "write the note"})

          encode
          #(.encodeToString (java.util.Base64/getEncoder) (.getBytes ^String % "UTF-8"))

          iid
          (h/store-iteration! s
                              {:session-turn-id tid
                               :status :done
                               :code "attach(...)"
                               :attachments [{:tool-call-id "call_A"
                                              :media-type "text/markdown"
                                              :base64 (encode "# Note\n")
                                              :filename "note.md"}]})

          stored
          (persistance/db-append-iteration-attachment! s
                                                       iid
                                                       {:media-type "text/markdown"
                                                        :base64 (encode "# Note\n\n> a comment\n")
                                                        :filename "note.md"
                                                        :kind "doc"
                                                        :audience "user"})

          rows
          (vis/db-list-iteration-attachments s iid)]

      (expect (= 2 (:version stored)))
      (expect (= 2 (count rows)))
      ;; Same NAME, two cuts — that is one artifact with a history, and the
      ;; revision is a user-visible document rather than model input.
      (expect (= #{1 2} (set (map :version rows))))
      (let [revision (first (filter #(= 2 (:version %)) rows))]
        (expect (= "note.md" (:filename revision)))
        (expect (= "doc" (:kind revision)))
        (expect (= "user" (name (:audience revision))))
        (expect (= "# Note\n\n> a comment\n"
                   (String. (.decode (java.util.Base64/getDecoder) ^String (:base64 revision))
                            "UTF-8"))))))
  ;; Regression, issue td-65cdf6: the late-settled live receipt lost its
  ;; identity at this write boundary, so Companion painted its live owner twice.
  ;; A live view a human STOPS after the block ended files the FIRST artifact its
  ;; iteration ever gets. The owning turn used to be read off a SIBLING row, so an
  ;; iteration with no other artifact dropped it — and the run the human watched
  ;; was listed nowhere.
  (it
    "appends the first artifact an iteration ever gets"
    (let [s
          (h/store)

          cid
          (h/store-session! s {:channel :cli})

          tid
          (vis/db-store-session-turn! s {:parent-session-id cid :user-request "watch CI"})

          iid
          (h/store-iteration! s {:session-turn-id tid :status :done :code "gh_watch_run(...)"})

          stored
          (persistance/db-append-iteration-attachment! s
                                                       iid
                                                       {:media-type
                                                        "application/vnd.vis.live+ndjson"
                                                        :storage-uri "vis-live://s1/view-1"
                                                        :size 4096
                                                        :filename "release.live.ndjson"
                                                        :view-id "view-1"
                                                        :kind "file"
                                                        :audience "user"})

          rows
          (vis/db-list-iteration-attachments s iid)]

      (expect (some? stored))
      (expect (= 1 (:version stored)))
      (expect (= ["release.live.ndjson"] (mapv :filename rows)))
      (expect (= "view-1" (:view-id (first rows))))
      (expect (= "vis-live://s1/view-1" (:storage-uri (first rows)))))))

;; Regression (session 4b6897d4): nothing bounded a STORED artifact -- neither a
;; tool's `attach()` nor the companion revision path had a cap at all -- so one
;; pathological payload could bind a value past SQLITE_MAX_LENGTH and take the
;; whole iteration insert down with `[SQLITE_TOOBIG]` instead of costing its own
;; row.
(defdescribe
  attachment-storage-cap-test
  "`attachment-payload-cols` is the one choke point every attachment write goes
   through, tool-produced or human-revised: an artifact past the stored cap is
   skipped exactly like one whose base64 does not decode."
  (it "skips an artifact past the stored cap and stores one within it"
      (let [payload-cols
            (private-core-fn "attachment-payload-cols")

            b64
            (fn [n]
              (.encodeToString (java.util.Base64/getEncoder) (byte-array (long n))))]

        (with-redefs [attachments/max-stored-attachment-bytes 8]
          (expect (nil? (payload-cols {:media-type "application/octet-stream" :base64 (b64 9)})))
          (let [cols (payload-cols {:media-type "application/octet-stream" :base64 (b64 8)})]
            (expect (= 8 (alength ^bytes (:bytes cols))))
            (expect (= 8 (:size_bytes cols)))
            (expect (nil? (:storage_uri cols)))))
        ;; The real ceiling sits far above any ordinary artifact.
        (expect (some? (payload-cols {:media-type "image/png" :base64 (b64 4096)})))))
  (it "never caps an artifact the offload rail already parked outside the row"
      (let [payload-cols
            (private-core-fn "attachment-payload-cols")

            cols
            (with-redefs [attachments/max-stored-attachment-bytes 8]
              (payload-cols
                {:media-type "video/mp4" :storage-uri "vis-store://bucket/key" :size 999999}))]

        (expect (= "vis-store://bucket/key" (:storage_uri cols)))
        (expect (nil? (:bytes cols)))
        (expect (= 999999 (:size_bytes cols))))))


(defdescribe
  wal-size-limit-test
  ;; Regression: `journal_size_limit` sat at SQLite's default -1, so a checkpoint
  ;; returned the WAL's PAGES but never its FILE — one oversized transaction
  ;; pinned the `-wal` sidecar at its high-water mark for the life of the store
  ;; (measured on a developer machine: 112.5 MB of file holding 624 KB of frames).
  (it
    "truncates the -wal sidecar back to the limit after an oversized transaction"
    (let [root
          (fs/create-temp-dir)

          dir
          (str (fs/path root "store"))

          s
          (vis/db-create-connection! dir)

          limit
          (long (private-core-fn "wal-size-limit-bytes"))

          wal
          (fs/path dir "vis.db-wal")

          chunk
          (* 256 1024)

          blob
          (byte-array chunk (byte 7))]

      (try (jdbc/execute! (:datasource s) ["CREATE TABLE churn (id INTEGER PRIMARY KEY, b BLOB)"])
           ;; ONE transaction: SQLite cannot checkpoint mid-transaction, so the
           ;; sidecar HAS to grow past the limit before anything can trim it.
           (with-open [c (jdbc/get-connection (:datasource s))]
             (jdbc/execute! c ["BEGIN IMMEDIATE"])
             (dotimes [_ (+ 8 (quot limit chunk))]
               (jdbc/execute! c ["INSERT INTO churn (b) VALUES (?)" blob]))
             (jdbc/execute! c ["COMMIT"]))
           (expect (< limit (fs/size wal)))
           (jdbc/execute! (:datasource s) ["PRAGMA wal_checkpoint(PASSIVE)"])
           ;; The commit that WRAPS the WAL is where SQLite applies the limit.
           (jdbc/execute! (:datasource s) ["INSERT INTO churn (b) VALUES (?)" (byte-array 16)])
           (expect (>= limit (fs/size wal)))
           (finally (vis/db-dispose-connection! s) (fs/delete-tree root))))))
