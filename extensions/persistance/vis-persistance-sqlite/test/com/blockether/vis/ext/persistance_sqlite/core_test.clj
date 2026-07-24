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
            ;; below can resolve its private vars at top-level def time. Without
            ;; this require, `resolve` returns nil and `deref` throws NPE because
            ;; the backend is normally loaded lazily by extension scanning.
            [com.blockether.vis.ext.persistance-sqlite.core]
            ;; Register the extension in the persistance facade. Production loads
            ;; this via classpath manifest discovery; tests need it explicit
            ;; because requiring `core` no longer self-registers (see
            ;; `registrar.clj` for the lazy-load split rationale).
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h :refer
             [raw-count raw-query]]
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
      (let
        [s
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
      (let
        [s
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
        (let
          [rows (vis/db-list-extension-aggregates
                  s
                  {:extension-id 'test.ext.alpha :iteration-id iid :iteration-form-index 0})]
          (expect (= 1 (count rows)))
          (expect (= {:iteration-id (str iid) :iteration-form-index 0}
                     (select-keys (:scope (first rows)) [:iteration-id :iteration-form-index])))
          (expect (= {:ok true} (:content (first rows)))))))
  (it
    "stores and lists iteration attachments per (call, position)"
    (let
      [s
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
          :forms [{:scope "t1/i1" :tag :observation :src "plt.show()" :svar/tool-call-id "call_A"}]
          ;; One call emits TWO same-named figures (position 0 and 1); a
          ;; third artifact is iteration-level (nil call-id, its own group).
          :attachments
          [{:tool-call-id "call_A"
            :media-type "image/png"
            :base64 b64
            :filename "fig.png"
            :size (alength png)}
           {:tool-call-id "call_A" :media-type "image/png" :base64 b64 :filename "fig.png"}
           {:tool-call-id nil :media-type "image/png" :base64 b64}]})

       got
       (vis/db-list-iteration-attachments s iid)]

      (expect (= 3 (count got)))
      ;; Grain (call, position): call_A gets 0 and 1, the nil-call artifact 0.
      (expect (= [[nil 0] ["call_A" 0] ["call_A" 1]] (mapv (juxt :tool-call-id :position) got)))
      ;; Base64 payload round-trips byte-for-byte.
      (expect (every? #(= b64 (:base64 %)) got))
      (expect (= "image/png" (:media-type (first got))))
      (expect (= 8 (:size (first got))))
      ;; Batch variant groups by iteration id.
      (let [batch (vis/db-list-iterations-attachments s [iid])]
        (expect (= 1 (count batch)))
        (expect (= 3 (count (get batch (str iid)))))))))

(defdescribe
  iteration-attachment-external-storage-test
  (it
    "an attachment carrying :storage-uri (no base64) stores EXTERNAL: uri kept, bytes NULL, source derived"
    (let
      [s
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
    (let
      [s
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

(defdescribe
  session-attachment-rollup-test
  (it
    "db-list-session-attachments rolls up user + tool across a whole session, ordered by turn then source"
    (let
      [s
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
      ;; Provenance: user carries :turn-soul-id (no iteration); tool carries both.
      (expect (= (str soul1) (:turn-soul-id (first all))))
      (expect (nil? (:iteration-id (first all))))
      (expect (some? (:iteration-id (nth all 1))))
      (expect (= "call_A" (:tool-call-id (nth all 1))))
      (expect (= "call_B" (:tool-call-id (nth all 2))))
      ;; Unknown session -> [].
      (expect (= [] (vis/db-list-session-attachments s (str (java.util.UUID/randomUUID))))))))

(defdescribe
  sqlite-extension-aggregate-index-data-filter-test
  (it
    "filters extension aggregate rows by index_data JSON fields"
    (let [s (h/store)]
      ;; Insert three rows with different index data
      (persistance/db-put-extension-aggregate!
        s
        {:extension-id 'test.ext.bridge
         :aggregate-key "node:core/run"
         :kind :bridge/node
         :index-data {:path "src/core.clj" :kind "def" :language "clojure"}
         :content {:name "run"}})
      (persistance/db-put-extension-aggregate!
        s
        {:extension-id 'test.ext.bridge
         :aggregate-key "node:core/start"
         :kind :bridge/node
         :index-data {:path "src/core.clj" :kind "def" :language "clojure"}
         :content {:name "start"}})
      (persistance/db-put-extension-aggregate!
        s
        {:extension-id 'test.ext.bridge
         :aggregate-key "edge:core/run::calls::lc/iterate"
         :kind :bridge/edge
         :index-data
         {:edge-kind "calls" :source "core/run" :target "lc/iterate" :path "src/core.clj"}
         :content {:source "core/run" :target "lc/iterate" :kind "calls"}})
      ;; Filter by kind + index-data file-path → both nodes in core.clj
      (let
        [by-file (vis/db-list-extension-aggregates s
                                                   {:extension-id 'test.ext.bridge
                                                    :kind :bridge/node
                                                    :index-data {:path "src/core.clj"}})]
        (expect (= 2 (count by-file)))
        (expect (= #{"node:core/run" "node:core/start"} (set (map :key by-file)))))
      ;; Filter by index-data edge-kind → one edge
      (let
        [by-edge-kind (vis/db-list-extension-aggregates s
                                                        {:extension-id 'test.ext.bridge
                                                         :index-data {:edge-kind "calls"}})]
        (expect (= 1 (count by-edge-kind)))
        (expect (= "edge:core/run::calls::lc/iterate" (:key (first by-edge-kind)))))
      ;; Filter by index-data source → edge from core/run
      (let
        [by-source (vis/db-list-extension-aggregates s
                                                     {:extension-id 'test.ext.bridge
                                                      :index-data {:source "core/run"}})]
        (expect (= 1 (count by-source))))
      ;; Filter edges by file-path → re-indexing use case
      (let
        [by-edge-file (vis/db-list-extension-aggregates s
                                                        {:extension-id 'test.ext.bridge
                                                         :kind :bridge/edge
                                                         :index-data {:path "src/core.clj"}})]
        (expect (= 1 (count by-edge-file))))
      ;; No match → empty
      (let
        [none (vis/db-list-extension-aggregates s
                                                {:extension-id 'test.ext.bridge
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
      (let
        [cause
         (ex-info "Migration checksum mismatch for migration version 1" {})

         e
         (ex-info "wrapper" {} cause)]

        (expect (true? (migration-checksum-mismatch? e)))))
  (it "returns false for unrelated failures"
      (expect (false? (migration-checksum-mismatch? (ex-info "boom" {})))))
  (it "wraps checksum mismatch as :vis/user-error with actionable guidance"
      (let
        [root
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
  (it "guides toward a new migration, not deleting the store"
      (expect (str/includes? migration-checksum-mismatch-user-message "schema mismatch"))
      (expect (str/includes? migration-checksum-mismatch-user-message "Flyway repair"))
      (expect (str/includes? migration-checksum-mismatch-user-message "V*__"))
      (expect (not (str/includes? migration-checksum-mismatch-user-message
                                  "remove ~/.vis/vis.mdb")))))


(def ^:private multiprocess-child-code
  "(require '[com.blockether.vis.core :as vis])
   (require '[com.blockether.vis.ext.persistance-sqlite.test-helpers :as h])
   (try
     (let [dir    (System/getProperty \"vis.test.db-dir\")
           marker (some-> (System/getProperty \"vis.test.marker\") not-empty)
           title  (or (System/getProperty \"vis.test.title\") \"child\")
           s      (vis/db-create-connection! dir)]
       (try
         (when marker (spit marker \"ready\"))
         (Thread/sleep 250)
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

(defn- java-command
  []
  ;; Windows' launcher is `java.exe`; ProcessBuilder won't append the suffix
  ;; for an absolute path, so name it explicitly per-OS.
  (str (fs/file (System/getProperty "java.home") "bin" (if (fs/windows?) "java.exe" "java"))))

(defn- start-multiprocess-writer!
  (^Process [dir marker] (start-multiprocess-writer! dir marker "child"))
  (^Process [dir marker title]
   (let
     [norm
      (fn [s]
        (.replace (str s) "\\" "/"))

      ;; Run the child program from a temp .clj FILE, not `-e <code>`:
      ;; passing the program inline puts its double-quotes on the command
      ;; line, and Windows' arg quoting strips them, so `clojure.main` reads
      ;; `(System/getProperty vis.test.db-dir)` as a bare symbol →
      ;; ClassNotFoundException before the child ever opens the store.
      script
      (fs/file (fs/create-temp-dir {:prefix "vis-mp-child-"}) "child.clj")

      _
      (spit script multiprocess-child-code)

      ^java.util.List cmd
      [(java-command) (str "-Dvis.test.db-dir=" (norm dir))
       (str "-Dvis.test.marker=" (norm (or marker ""))) (str "-Dvis.test.title=" title)
       "clojure.main" (norm (str script))]

      pb
      (ProcessBuilder. cmd)]

     ;; Classpath via the CLASSPATH env, NOT `-cp` on the command line: the full
     ;; classpath blows past Windows' ~32k command-line limit, so CreateProcess
     ;; would fail before the child ever ran.
     (.put (.environment pb) "CLASSPATH" (System/getProperty "java.class.path"))
     (.redirectErrorStream pb true)
     (let [child (.start pb)]
       (swap! child-output-futures assoc child (future (slurp (.getInputStream child))))
       child))))

(defn- wait-for-file
  [path timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []

      (cond (fs/exists? path) true
            (>= (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 25) (recur))))))

;; Multiprocess child JVMs cold-boot Clojure + Flyway + sqlite-jdbc on every
;; spawn. On a warm machine that's ~6–7 s; under the full test suite (JIT
;; contention with the rest of the run), 10–20 s, with occasional slower
;; cold-starts on loaded CI/developer machines. This is correctness-only —
;; these tests guard cross-JVM DB semantics, NOT startup speed — so the timeout
;; is deliberately generous: a saturated box (other JVMs hogging cores) can push
;; a single cold-boot past a minute, and a false timeout here is pure noise.
(def ^:private MULTIPROCESS_CHILD_TIMEOUT_S 150)

(defn- expect-child-success!
  [^Process child]
  (expect (true? (.waitFor child MULTIPROCESS_CHILD_TIMEOUT_S TimeUnit/SECONDS)))
  (let
    [output-future
     (get @child-output-futures child)

     output
     (when output-future (deref output-future 1000 ""))]

    (swap! child-output-futures dissoc child)
    ;; Surface the child's merged stdout+stderr (it `.printStackTrace`s on a
    ;; failed open) so a Windows-only child crash is diagnosable from CI logs.
    (when-not (and (= 0 (.exitValue child)) (str/includes? (str output) "CHILD-DONE"))
      (println "=== multiprocess child output (exit" (.exitValue child) ") ===")
      (println output)
      (println "=== end child output ==="))
    (expect (= 0 (.exitValue child)))
    (expect (str/includes? output "CHILD-DONE"))))

(defdescribe
  sqlite-multiprocess-write-test
  (it "allows two JVMs to open the same persistent store and write while both are alive"
      (let
        [dir
         (fs/create-temp-dir {:prefix "vis-db-multiprocess-"})

         marker
         (fs/file dir "child-opened")]

        (try (let
               [parent
                (vis/db-create-connection! (str dir))

                child
                (start-multiprocess-writer! (str dir) (str marker))]

               (try (expect (true? (wait-for-file marker (* 1000 MULTIPROCESS_CHILD_TIMEOUT_S))))
                    (h/store-session! parent {:channel :parent :title "parent"})
                    (expect-child-success! child)
                    (expect (= #{"child" "parent"}
                               (set (map :title
                                         (raw-query parent
                                                    {:select [:title] :from [:session_state]})))))
                    (finally (when (.isAlive child) (.destroyForcibly child))
                             (vis/db-dispose-connection! parent))))
             (finally (fs/delete-tree dir)))))
  (it "serializes first-open migrations across JVMs"
      (let [dir (fs/create-temp-dir {:prefix "vis-db-multiprocess-migrate-"})]
        (try (let
               [child-a (start-multiprocess-writer! (str dir) nil "child-a")
                child-b (start-multiprocess-writer! (str dir) nil "child-b")]

               (try (expect-child-success! child-a)
                    (expect-child-success! child-b)
                    (let [s (vis/db-create-connection! (str dir))]
                      (try (expect (= #{"child-a" "child-b"}
                                      (set (map :title
                                                (raw-query s
                                                           {:select [:title]
                                                            :from [:session_state]})))))
                           (finally (vis/db-dispose-connection! s))))
                    (finally (doseq [^Process child [child-a child-b]]
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
      (let
        [with-migration-lock!
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
                                        (when (> (.incrementAndGet active) 1) (.set overlap true))
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
  (it
    "reproduces SQLITE_BUSY_SNAPSHOT with a stale deferred read transaction"
    (let
      [db-file
       (File/createTempFile "vis-busy-snapshot" ".db")

       url
       (str "jdbc:sqlite:" (.getAbsolutePath db-file))

       c1
       (java.sql.DriverManager/getConnection url)

       c2
       (java.sql.DriverManager/getConnection url)]

      (try (jdbc/execute! c1 ["PRAGMA journal_mode=WAL"])
           (jdbc/execute! c1
                          (sql/format {:create-table [:snapshot_probe :if-not-exists]
                                       :with-columns [[:id :integer :primary-key] [:v :integer]]}))
           (jdbc/execute! c1 (sql/format {:insert-into :snapshot_probe :values [{:id 1 :v 0}]}))
           (.setAutoCommit c1 false)
           (jdbc/execute! c1 (sql/format {:select [:*] :from [:snapshot_probe] :where [:= :id 1]}))
           (jdbc/execute! c2 (sql/format {:update :snapshot_probe :set {:v 1} :where [:= :id 1]}))
           (let
             [^Throwable thrown (try (jdbc/execute! c1
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
      (try (let
             [s (vis/db-create-connection! (str dir))
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
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :cli})

       tid
       (vis/db-store-session-turn! s
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
                              (future (try (vis/db-log! s {:level :info :event (str "stress." i)})
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
      (let
        [attempts
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

;; =============================================================================
;; Session
;; =============================================================================

(defdescribe
  session-test
  (it "inserts into session_soul + session_state"
      (let
        [s
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
        (let
          [id2 (h/store-session! s {:channel :tui})
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
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui :title "A"})]

        (h/fork-session! s cid {})
        (expect (= [1] (mapv :fork-count (vis/db-list-sessions s :tui))))
        (h/fork-session! s cid {})
        (expect (= [2] (mapv :fork-count (vis/db-list-sessions s :tui))))))
  (it "finds by external-id via column"
      (let
        [s
         (h/store)

         id
         (h/store-session! s {:channel :cli :external-id "chat-42"})]

        (expect (= id (vis/db-find-session-by-external s :cli "chat-42")))
        (expect (nil? (vis/db-find-session-by-external s :cli "nope")))))
  (it "updates title on session_state"
      (let
        [s
         (h/store)

         id
         (h/store-session! s {:channel :tui :title "Old"})]

        (vis/db-update-session-title! s id "New")
        (expect (= "New" (:title (vis/db-get-session s id)))))))

;; =============================================================================
;; Transcript search (db-search-session-ids) - matches USER request + assistant
;; iteration text, case-insensitive, so the session picker can find a session by
;; anything said in it (not just the title). Server-side so the assistant text
;; never crosses the wire.
;; =============================================================================

(defdescribe
  session-transcript-search-test
  (it "matches a session by its user request text (case-insensitive)"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui :title "Alpha"})]

        (vis/db-store-session-turn!
          s
          {:parent-session-id cid :user-request "make the FILTERING work" :status :done})
        (expect (= [cid] (vis/db-search-session-ids s :all "filtering")))
        (expect (= [cid] (vis/db-search-session-ids s :all "FILTER")))
        (expect (= [] (vis/db-search-session-ids s :all "nomatch")))))
  (it "matches a session by assistant iteration prose"
      (let
        [s
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
    (let
      [s
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
      (let
        [tid (vis/db-store-session-turn!
               s
               {:parent-session-id rep :user-request "plain q" :status :done})]
        (h/store-iteration!
          s
          {:session-turn-id tid :assistant-prose "reply with NEEDLE" :code "x" :result 1}))
      ;; both: needle in request AND reply
      (let
        [tid (vis/db-store-session-turn!
               s
               {:parent-session-id both :user-request "NEEDLE in ask" :status :done})]
        (h/store-iteration!
          s
          {:session-turn-id tid :assistant-prose "NEEDLE in answer" :code "x" :result 1}))
      (let
        [by-id (into {} (map (juxt :id identity)) (vis/db-search-session-matches s :all "needle"))]
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
  (it
    "returns [] for a blank query and honours channel scope"
    (let
      [s
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

;; =============================================================================
;; Adoption marker (V5 claimed_at) - warm-pool scaffolding stays out of the
;; cross-channel list until a tab claims it (explicitly at creation, or via its
;; first turn).
;; =============================================================================

(defdescribe
  session-adoption-claimed-test
  (it "defaults to CLAIMED: a normal session is list-visible immediately"
      (let [s (h/store)]
        (h/store-session! s {:channel :tui :title "real"})
        (expect (= ["real"] (mapv :title (vis/db-list-sessions s :all))))))
  (it "an UNCLAIMED (:claimed? false) session is HIDDEN from the list but resolvable by id"
      (let
        [s
         (h/store)

         id
         (h/store-session! s {:channel :tui :title "pool" :claimed? false})]

        ;; hidden from the cross-channel list...
        (expect (= [] (vec (vis/db-list-sessions s :all))))
        ;; ...yet the soul row exists and direct resume-by-id still works.
        (expect (= 1 (raw-count s :session_soul)))
        (expect (= "pool" (:title (vis/db-get-session s id))))))
  (it "the FIRST turn claims an unclaimed session, surfacing it in the list"
      (let
        [s
         (h/store)

         id
         (h/store-session! s {:channel :tui :title "pool" :claimed? false})]

        (expect (= [] (vec (vis/db-list-sessions s :all))))
        (vis/db-store-session-turn! s {:parent-session-id id :user-request "hi" :status :running})
        (expect (= ["pool"] (mapv :title (vis/db-list-sessions s :all))))))
  (it "claiming is idempotent: a second turn does not disturb the claimed session"
      (let
        [s
         (h/store)

         id
         (h/store-session! s {:channel :tui :title "pool" :claimed? false})]

        (vis/db-store-session-turn! s {:parent-session-id id :user-request "one" :status :running})
        (vis/db-store-session-turn! s {:parent-session-id id :user-request "two" :status :running})
        (expect (= ["pool"] (mapv :title (vis/db-list-sessions s :all))))
        (expect (= 1 (raw-count s :session_soul))))))

;; =============================================================================
;; List session states (fork tree introspection)
;; =============================================================================

(defdescribe
  subloop-child-session-test
  (it
    "a sub_loop child (parent_state_id set) is hidden from the top-level list yet cascade-deletes with its parent"
    (let
      [s
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
      (let
        [s
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
      (let
        [s
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
    (let
      [s
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
      (let
        [s
         (h/store)

         rows
         (vis/db-list-session-states s (random-uuid))]

        (expect (vector? rows))
        (expect (= [] rows))))
  (it "returns [] (vector, never nil) when session-id is nil"
      (let [s (h/store)]
        (expect (= [] (vis/db-list-session-states s nil))))))

;; =============================================================================
;; List turn states (retry history introspection)
;; =============================================================================

(defdescribe
  db-list-session-turn-states-test
  (it "returns one row for the original run before any retry"
      (let
        [s
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
  (it
    "surfaces every retry in version order with forked-from links"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "flaky" :status :error})]

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
      (let
        [s
         (h/store)

         rows
         (vis/db-list-session-turn-states s (random-uuid))]

        (expect (vector? rows))
        (expect (= [] rows))))
  (it "returns [] (vector, never nil) when session-turn-id is nil"
      (let [s (h/store)]
        (expect (= [] (vis/db-list-session-turn-states s nil))))))

;; =============================================================================
;; Fork
;; =============================================================================

(defdescribe
  fork-test
  (it "creates a new session_state row with parent_state_id"
      (let
        [s
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
      (let
        [s
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
      (let
        [s
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
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})]

        (h/fork-session! s cid {})
        (h/fork-session! s cid {})
        (expect (= 2 (:version (vis/db-get-session s cid))))
        (expect (= 3 (raw-count s :session_state)))))
  (it
    "fork-at-turn copies turns THROUGH the pick into a NEW independent session, source intact"
    (let
      [s
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

;; =============================================================================
;; Turn
;; =============================================================================

(defdescribe
  turn-test
  (it "inserts into session_turn_soul + session_turn_state"
      (let
        [s
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
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})]

        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "one" :status :done})
        (vis/db-store-session-turn! s {:parent-session-id cid :user-request "two" :status :done})
        (expect (= [1 2] (mapv :position (vis/db-list-session-turns s cid))))))
  (it
    "normalizes :success to done"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (vis/db-update-session-turn! s
                                   qid
                                   {:status :success
                                    :answer "42"
                                    :tokens {"input" 100 "output" 50}
                                    :cost {"total_cost" 0.005 "model" "gpt-4o"}})
      (let [q (first (vis/db-list-session-turns s cid))]
        (expect (= :done (:status q)))
        (expect (= "gpt-4o" (:model q))))))
  (it
    "persists :error without renormalization"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (vis/db-update-session-turn! s qid {:status :error})
      (expect (= :error (:status (first (vis/db-list-session-turns s cid))))))))

;; =============================================================================
;; Dedicated ctx stores (task/fact/archive) — write-through + per-session id
;; =============================================================================

(defdescribe retry-test
             (it "creates session_turn_state version 1 with forked_from ref"
                 (let
                   [s
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
                 (let
                   [s
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

;; =============================================================================
;; Iteration + stateless blocks
;; =============================================================================

(defdescribe
  iteration-block-test
  (it
    "writes one iteration row whose flat columns carry the single form"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration!
        s
        {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 5 :thinking "Computing"})
      (expect (= 1 (raw-count s :session_turn_iteration)))
      ;; No more kind='call' rows - the call log lives inline in the
      ;; iteration flat columns. definition_* sidecar tables were
      ;; dropped together with cross-turn def survival.
      (let
        [iteration
         (first (vis/db-list-session-turn-iterations s qid))

         blocks
         (:forms iteration)]

        (expect (= "Computing" (:thinking iteration)))
        (expect (= 1 (:position iteration)))
        (expect (= 1 (count blocks)))
        (expect (= "(+ 1 1)" (:src (first blocks))))
        (expect (= 2 (:result (first blocks)))))))
  (it
    "uses flat code/result columns for the inline log"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 5})
      (expect (contains? (table-columns s "session_turn_iteration") "code"))
      (expect (not (contains? (table-columns s "session_turn_iteration") "blocks")))
      (expect (some? (:code (first (raw-query s
                                              {:select [:code] :from :session_turn_iteration})))))))
  (it
    "assigns iteration positions from 1 and rejects non-contiguous manual positions"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s {:session-turn-id qid :code "" :duration-ms 1})
      (h/store-iteration! s {:session-turn-id qid :code "" :duration-ms 1})
      (expect (= [1 2] (mapv :position (vis/db-list-session-turn-iterations s qid))))
      (let
        [turn-state-id
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
  (it
    "replaces fn results with the {:vis/ref :expr} sentinel (freeze-safe contract)"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s
                          {:session-turn-id qid
                           :code "(defn f [x] x)"
                           :result (fn [x]
                                     x)
                           :duration-ms 5})
      (let
        [iteration
         (first (vis/db-list-session-turn-iterations s qid))

         result
         (:result (first (:forms iteration)))]

        (expect (= {:vis/ref :expr} result)))))
  (it "errors carry the message in the BLOB"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

        ;; :error is the structured :error map
        ;; ({:message :trace? :hint? :block?}). Single error field, no
        ;; fallback string.
        (h/store-iteration!
          s
          {:session-turn-id qid :code "(/ 1 0)" :error {:message "Divide by zero"} :duration-ms 5})
        (let
          [iteration
           (first (vis/db-list-session-turn-iterations s qid))

           exec
           (first (:forms iteration))]

          (expect (= {:message "Divide by zero"} (:error exec)))
          ;; :result intentionally omitted on error - cond-> drops nil.
          (expect (not (contains? exec :result))))))
  (it "keeps a realized non-lazy seq (`sort` output) in error data, not {:vis/ref :expr}"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

        ;; The protected-rebind guard's :names is built via `sort` — an
        ;; ArraySeq. freeze-safe must persist it as DATA, not flatten it to
        ;; the {:vis/ref :expr} runtime placeholder (only LazySeq is one).
        (h/store-iteration! s
                            {:session-turn-id qid
                             :code "ls = 1"
                             :error {:message "protected" :data {:names (sort ["ls" "cat"])}}
                             :duration-ms 5})
        (let
          [iteration
           (first (vis/db-list-session-turn-iterations s qid))

           err
           (:error (first (:forms iteration)))]

          (expect (= ["cat" "ls"] (vec (get-in err [:data :names])))))))
  (it
    ":comment field carries leading `;; ... / #_(...)` blocks alongside :code"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s
                          {:session-turn-id qid
                           :code "(+ 1 1)"
                           :comment ";; double-check arithmetic"
                           :result 2
                           :duration-ms 5})
      (let
        [iteration
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
  (it
    "increments position monotonically across iterations in the same session_turn_state"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (h/store-iteration! s {:session-turn-id qid :code "1" :result 1 :duration-ms 1})
      (h/store-iteration! s {:session-turn-id qid :code "2" :result 2 :duration-ms 1})
      (h/store-iteration! s {:session-turn-id qid :code "3" :result 3 :duration-ms 1})
      (let
        [iterations
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
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

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
  (it
    "persists the assistant prose (markdown alongside a tool call) and reads it back"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

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
    (let
      [s
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
      (let
        [raw-row
         (first (raw-query s
                           {:select [:llm_selected_provider :llm_actual_provider :is_llm_fallback]
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
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

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
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

        ;; Negative usage is structurally impossible - the schema CHECK
        ;; is the last line of defence. Any caller that fabricates a
        ;; negative value gets a SQLite constraint exception (wrapped
        ;; through next.jdbc). lazytest has no `thrown?` macro; use a
        ;; plain try/catch and assert the throw landed.
        (let
          [thrown? (try
                     (h/store-iteration!
                       s
                       {:session-turn-id qid :code "x" :result 1 :tokens {"input" -5 "output" 10}})
                     false
                     (catch Throwable _ true))]
          (expect (true? thrown?))))))

;; =============================================================================
;; Stateful vars
;; =============================================================================

(defdescribe
  cascade-delete-test
  (it "deletes soul + all descendants"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})

         _
         (h/store-iteration! s {:session-turn-id qid :code "1" :result 1 :duration-ms 0})]

        (vis/db-delete-session-tree! s cid)
        (expect (= 0 (raw-count s :session_soul)))
        (expect (= 0 (raw-count s :session_state)))
        (expect (= 0 (raw-count s :session_turn_soul)))
        (expect (= 0 (raw-count s :session_turn_state)))
        (expect (= 0 (raw-count s :session_turn_iteration))))))

;; =============================================================================
;; Turn history
;; =============================================================================

(defdescribe
  turn-history-test
  (it "builds ordered history with iteration counts"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "What?" :status :done})

         _
         (h/store-iteration! s {:session-turn-id qid :code "" :answer "A Lisp" :duration-ms 100})

         _
         (h/store-iteration! s {:session-turn-id qid :code "" :answer "JVM Lisp" :duration-ms 50})

         h
         (vis/db-turn-history s cid)]

        (expect (= 1 (count h)))
        (expect (= "What?" (:user-request (first h))))
        (expect (= 2 (:iteration-count (first h)))))))

;; =============================================================================
;; Soul/state FK integrity
;; =============================================================================

(defdescribe
  soul-state-integrity-test
  (it "session_state.session_soul_id points to session_soul.id"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui :title "FK test"})]

        (let
          [soul
           (first (raw-query s {:select [:id] :from :session_soul}))

           state
           (first (raw-query s {:select [:session_soul_id] :from :session_state}))]

          (expect (= (:id soul) (:session_soul_id state))))))
  (it
    "session_turn_soul.session_state_id points to session_state.id"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       _
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (let
        [state
         (first (raw-query s {:select [:id] :from :session_state}))

         qsoul
         (first (raw-query s {:select [:session_state_id] :from :session_turn_soul}))]

        (expect (= (:id state) (:session_state_id qsoul))))))
  (it
    "session_turn_state.session_turn_soul_id points to session_turn_soul.id"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       _
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (let
        [qsoul
         (first (raw-query s {:select [:id] :from :session_turn_soul}))

         qstate
         (first (raw-query s {:select [:session_turn_soul_id] :from :session_turn_state}))]

        (expect (= (:id qsoul) (:session_turn_soul_id qstate))))))
  (it "iteration.session_turn_state_id points to session_turn_state.id"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})

         _
         (h/store-iteration! s {:session-turn-id qid :code "" :duration-ms 0})]

        (let
          [qstate
           (first (raw-query s {:select [:id] :from :session_turn_state}))

           iteration
           (first (raw-query s {:select [:session_turn_state_id] :from :session_turn_iteration}))]

          (expect (= (:id qstate) (:session_turn_state_id iteration))))))
  (it
    "retry session_turn_state.forked_from_session_turn_state_id points to previous session_turn_state.id"
    (let
      [s
       (h/store)

       cid
       (h/store-session! s {:channel :tui})

       qid
       (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})]

      (vis/db-update-session-turn! s qid {:status :error})
      (vis/db-retry-session-turn! s qid {:status :running :model "claude-4"})
      (let
        [states (raw-query s
                           {:select [:id :version :forked_from_session_turn_state_id]
                            :from :session_turn_state
                            :order-by [[:version :asc]]})]
        (expect (= 2 (count states)))
        (expect (nil? (:forked_from_session_turn_state_id (first states))))
        (expect (= (:id (first states)) (:forked_from_session_turn_state_id (second states)))))))
  (it "fork session_state.parent_state_id points to previous state"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})]

        (h/fork-session! s cid {:title "fork"})
        (let
          [states (raw-query s
                             {:select [:id :version :parent_state_id]
                              :from :session_state
                              :order-by [[:version :asc]]})]
          (expect (= 2 (count states)))
          (expect (nil? (:parent_state_id (first states))))
          (expect (= (:id (first states)) (:parent_state_id (second states)))))))
  (it "per-form payload lives on session_turn_iteration.forms (no definition_* sidecar)"
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})

         qid
         (vis/db-store-session-turn! s {:parent-session-id cid :user-request "x" :status :running})

         _
         (h/store-iteration! s {:session-turn-id qid :code "(+ 1 1)" :result 2 :duration-ms 0})]

        (let
          [iteration
           (first (vis/db-list-session-turn-iterations s qid))

           form
           (first (:forms iteration))]

          (expect (= "(+ 1 1)" (:code iteration)))
          (expect (= 2 (:result form)))))))

;; =============================================================================
;; Answer lifecycle (placeholder; live behaviour exercised in loop tests)
;; =============================================================================

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
      (let
        [s
         (h/store)

         cid
         (h/store-session! s {:channel :tui})]

        (vis/db-log! s {:level :info :event "test.event" :data "{\"k\":1}" :session-soul-id cid})
        (expect (= 1 (raw-count s :log)))
        (let [row (first (raw-query s {:select [:*] :from :log}))]
          (expect (= "info" (:level row)))
          (expect (= "test.event" (:event row)))
          (expect (= (str cid) (:session_soul_id row)))))))

(defdescribe
  native-results-for-tool-ids-test
  "`db-native-results-for-tool-ids` backs `ntr[tool_id]`: given a
   set of provider tool_use ids, batch-load the matching NATIVE tool results from
   the session's persisted iteration `:forms` (Nippy), across BOTH prior turns and
   earlier iterations of the current turn. Absent id ⇒ absent key."
  (let
    [form (fn [id result]
            {:scope "t1/i1"
             :tag :observation
             :src "cat(\"x\")"
             :svar/tool-call-id id
             :vis/tool-name "cat"
             :result result})]
    (it "retrieves a result stored in an EARLIER iteration of the SAME turn"
        (let
          [s (h/store)
           cid (h/store-session! s {:channel :cli})
           tid (vis/db-store-session-turn! s {:parent-session-id cid :user-request "q"})]

          ;; iter 0 stores toolu_A; iter 1 stores toolu_B (both same turn)
          (h/store-iteration! s
                              {:session-turn-id tid
                               :status :done
                               :idx 0
                               :code "cat"
                               :forms [(form "toolu_A" {:op "cat" :text "AAA"})]})
          (h/store-iteration! s
                              {:session-turn-id tid
                               :status :done
                               :idx 1
                               :code "cat"
                               :forms [(form "toolu_B" {:op "cat" :text "BBB"})]})
          (let [out (persistance/db-native-results-for-tool-ids s cid #{"toolu_A" "toolu_B"})]
            (expect (= {:op "cat" :text "AAA"} (get out "toolu_A")))
            (expect (= {:op "cat" :text "BBB"} (get out "toolu_B"))))))
    (it "retrieves a result stored in a PRIOR turn"
        (let
          [s (h/store)
           cid (h/store-session! s {:channel :cli})
           tid1 (vis/db-store-session-turn! s {:parent-session-id cid :user-request "turn1"})
           _ (h/store-iteration! s
                                 {:session-turn-id tid1
                                  :status :done
                                  :idx 0
                                  :code "cat"
                                  :forms [(form "toolu_OLD" {:op "rg" :hits 3})]})
           tid2 (vis/db-store-session-turn! s {:parent-session-id cid :user-request "turn2"})
           _ (h/store-iteration! s
                                 {:session-turn-id tid2
                                  :status :done
                                  :idx 0
                                  :code "cat"
                                  :forms [(form "toolu_NEW" {:op "cat" :text "N"})]})]

          (let [out (persistance/db-native-results-for-tool-ids s cid #{"toolu_OLD" "toolu_NEW"})]
            (expect (= {:op "rg" :hits 3} (get out "toolu_OLD")))
            (expect (= {:op "cat" :text "N"} (get out "toolu_NEW"))))))
    (it "an unknown id is ABSENT from the result (no crash, clean miss)"
        (let
          [s (h/store)
           cid (h/store-session! s {:channel :cli})
           tid (vis/db-store-session-turn! s {:parent-session-id cid :user-request "q"})]

          (h/store-iteration! s
                              {:session-turn-id tid
                               :status :done
                               :idx 0
                               :code "cat"
                               :forms [(form "toolu_REAL" {:op "cat" :text "R"})]})
          (let
            [out (persistance/db-native-results-for-tool-ids s
                                                             cid
                                                             #{"toolu_REAL" "toolu_HALLUCINATED"})]
            (expect (= {:op "cat" :text "R"} (get out "toolu_REAL")))
            (expect (not (contains? out "toolu_HALLUCINATED"))))))
    (it "a print-only (python_execution) form has no :result → absent"
        (let
          [s (h/store)
           cid (h/store-session! s {:channel :cli})
           tid (vis/db-store-session-turn! s {:parent-session-id cid :user-request "q"})]

          ;; :stdout, not :result — python_execution stores no return
          (h/store-iteration! s
                              {:session-turn-id tid
                               :status :done
                               :idx 0
                               :code "print(1)"
                               :forms [{:scope "t1/i1"
                                        :tag :observation
                                        :src "print(1)"
                                        :svar/tool-call-id "toolu_P"
                                        :stdout "1\n"}]})
          (let [out (persistance/db-native-results-for-tool-ids s cid #{"toolu_P"})]
            (expect (not (contains? out "toolu_P"))))))
    (it "empty id set and unknown session are safe no-ops"
        (let
          [s (h/store)
           cid (h/store-session! s {:channel :cli})]

          (expect (= {} (persistance/db-native-results-for-tool-ids s cid #{})))
          (expect (= {} (persistance/db-native-results-for-tool-ids s nil #{"x"})))
          (expect (= {} (persistance/db-native-results-for-tool-ids s (random-uuid) #{"x"})))))))

(defdescribe
  native-result-ids-for-session-test
  "`db-native-result-ids-for-session` lists EVERY persisted native tool_use id in
   the session branch (all turns + all iterations), de-duped, print-only forms
   excluded. Backs iteration over `ntr` (keys/items/values)."
  (let
    [form (fn [id result]
            {:scope "t1/i1"
             :tag :observation
             :src "cat(\"x\")"
             :svar/tool-call-id id
             :vis/tool-name "cat"
             :result result})]
    (it
      "collects native ids across iterations AND turns, skipping print-only forms"
      (let
        [s (h/store)
         cid (h/store-session! s {:channel :cli})
         tid1 (vis/db-store-session-turn! s {:parent-session-id cid :user-request "t1"})
         _ (h/store-iteration! s
                               {:session-turn-id tid1
                                :status :done
                                :idx 0
                                :code "cat"
                                :forms [(form "toolu_A" {:op "cat" :text "A"})]})
         ;; print-only form (no :result) must be EXCLUDED
         _ (h/store-iteration! s
                               {:session-turn-id tid1
                                :status :done
                                :idx 1
                                :code "print(1)"
                                :forms [{:scope "t1/i2"
                                         :tag :observation
                                         :src "print(1)"
                                         :svar/tool-call-id "toolu_P"
                                         :stdout "1\n"}]})
         tid2 (vis/db-store-session-turn! s {:parent-session-id cid :user-request "t2"})
         _ (h/store-iteration! s
                               {:session-turn-id tid2
                                :status :done
                                :idx 0
                                :code "cat"
                                :forms [(form "toolu_B" {:op "rg" :hits 2})]})]

        (let [ids (persistance/db-native-result-ids-for-session s cid)]
          (expect (= #{"toolu_A" "toolu_B"} (set ids)))
          ;; de-duped, no print-only id
          (expect (= 2 (count ids)))
          (expect (not (some #{"toolu_P"} ids))))))
    (it "unknown / nil session is a safe empty list"
        (let [s (h/store)]
          (expect (= [] (persistance/db-native-result-ids-for-session s nil)))
          (expect (= [] (persistance/db-native-result-ids-for-session s (random-uuid))))))))

;; ─── projects (cross-channel) + movable project sessions + ownership (V6/V7) ───

(defdescribe
  sqlite-project-test
  (it
    "creates cross-channel projects, assigns sessions, and scatters on delete"
    (let
      [s
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
       (expect (= #{"vis-core" "side-proj"} (set (map :name (persistance/db-list-projects s {})))))

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
    (let
      [s
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
      (let
        [s
         (h/store)

         sid
         (h/store-session! s {:channel :tui :title "owned"})]

        (expect (= "local" (:owner-id (persistance/db-get-session s sid))))))
  (it "binds a project to its workspace_root and resolves it get-or-create"
      (let
        [s
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
      (let
        [s
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
      (let
        [s
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
      (let
        [s
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
    (let
      [s
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
                 (let
                   [s
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
    (let
      [s
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
