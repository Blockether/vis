(ns ^{:clj-kondo/config
      ;; Pragmatic: this aggregator test file collects scenarios from
      ;; multiple original test namespaces. Many `it` blocks use
      ;; `(let [s (h/store) cid (vis/db-store-conversation! …)] (let […]
      ;; …))` where the inner let is technically mergeable and the
      ;; intermediate ids (cid / qid / etc.) are bound for SIDE EFFECT,
      ;; not for use. Suppress redundant-let / unused-binding here
      ;; rather than rewrite every block.
      '{:linters {:redundant-let     {:level :off}
                  :unused-binding    {:level :off}}}}
  com.blockether.vis.ext.persistance-sqlite.core-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h :refer [raw-count raw-query]]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.persistance :as persistance]
   [honey.sql :as sql]
   [lazytest.core :refer [defdescribe it expect]]
   [next.jdbc :as jdbc]
   [sci.core :as sci])
  (:import
   (java.io File)
   (java.util.concurrent CountDownLatch TimeUnit)))

;; ─── from db_test.clj ───

(h/use-mem-store!)

(defdescribe sqlite-extension-aggregate-test
  (it "upserts extension-owned singleton rows by extension, key, kind, and scope"
    (let [s (h/store)
          first-row (persistance/db-put-extension-aggregate! s
                      {:extension-id 'test.ext.alpha
                       :aggregate-key :index/status
                       :kind :background/status
                       :metadata {:schema-version 1}
                       :content {:state :running}})
          second-row (persistance/db-put-extension-aggregate! s
                       {:extension-id 'test.ext.alpha
                        :aggregate-key :index/status
                        :kind :background/status
                        :metadata {:schema-version 1}
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
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "block scoped"})
          iid (vis/db-store-iteration! s {:conversation-turn-id tid
                                          :status :done
                                          :blocks [{:idx 0 :code "(+ 1 2)"}]})]
      (persistance/db-create-extension-aggregate! s
        {:extension-id 'test.ext.alpha
         :aggregate-key :tool/trace
         :kind :trace/tool-result
         :iteration-id iid
         :iteration-block-index 0
         :content {:ok true}})
      (let [rows (vis/db-list-extension-aggregates s
                   {:extension-id 'test.ext.alpha
                    :iteration-id iid
                    :iteration-block-index 0})]
        (expect (= 1 (count rows)))
        (expect (= {:iteration-id (str iid)
                    :iteration-block-index 0}
                  (select-keys (:scope (first rows)) [:iteration-id :iteration-block-index])))
        (expect (= {:ok true} (:content (first rows))))))))

(defn- drop-conversation-intent-schema!
  [db-file]
  (let [ds (jdbc/get-datasource {:jdbcUrl (str "jdbc:sqlite:" db-file)})]
    (jdbc/execute! ds ["PRAGMA foreign_keys = OFF"])
    (doseq [table [:conversation_intent_focus
                   :conversation_intent_gate_ref
                   :conversation_intent_gate
                   :conversation_intent_plan
                   :conversation_intent_relation
                   :conversation_intent_ref
                   :conversation_intent]]
      (jdbc/execute! ds (sql/format {:drop-table [:if-exists table]})))))

(defn- private-core-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.persistance-sqlite.core" name))))

(def ^:private migration-checksum-mismatch?
  (private-core-fn "migration-checksum-mismatch?"))

(def ^:private maybe-wrap-db-open-error
  (private-core-fn "maybe-wrap-db-open-error"))

(def ^:private migration-checksum-mismatch-user-message
  (private-core-fn "migration-checksum-mismatch-user-message"))

(defdescribe sqlite-bootstrap-error-normalization-test
  (it "matches Flyway checksum text at top level"
    (expect (true? (migration-checksum-mismatch?
                     (ex-info "Validate failed: Migrations have failed validation\nMigration checksum mismatch for migration version 1"
                       {})))))

  (it "matches Flyway checksum text in a nested cause"
    (let [cause (ex-info "Migration checksum mismatch for migration version 1" {})
          e     (ex-info "wrapper" {} cause)]
      (expect (true? (migration-checksum-mismatch? e)))))

  (it "returns false for unrelated failures"
    (expect (false? (migration-checksum-mismatch? (ex-info "boom" {})))))

  (it "wraps checksum mismatch as :vis/user-error with actionable guidance"
    (let [root (ex-info "Migration checksum mismatch for migration version 1" {})
          e    (maybe-wrap-db-open-error root)]
      (expect (instance? clojure.lang.ExceptionInfo e))
      (expect (true? (:vis/user-error (ex-data e))))
      (expect (= :vis/db-migration-checksum-mismatch (:type (ex-data e))))
      (expect (= root (.getCause ^Throwable e)))
      (expect (str/includes? (.getMessage ^Throwable e) "~/.vis/vis.mdb"))
      (expect (str/includes? (.getMessage ^Throwable e) "packaged migration resources"))
      (expect (not (str/includes? (.getMessage ^Throwable e) "Flyway repair")))))

  (it "leaves unrelated bootstrap failures untouched"
    (let [e (ex-info "x" {})]
      (expect (identical? e (maybe-wrap-db-open-error e)))))

  (it "mentions reset path and does not suggest repair"
    (expect (str/includes? migration-checksum-mismatch-user-message "schema mismatch"))
    (expect (str/includes? migration-checksum-mismatch-user-message "~/.vis/vis.mdb"))
    (expect (str/includes? migration-checksum-mismatch-user-message "packaged migration resources"))
    (expect (not (str/includes? migration-checksum-mismatch-user-message "Flyway repair")))))

(def ^:private multiprocess-child-code
  "(require '[com.blockether.vis.core :as vis])
   (let [dir    (System/getProperty \"vis.test.db-dir\")
         marker (some-> (System/getProperty \"vis.test.marker\") not-empty)
         title  (or (System/getProperty \"vis.test.title\") \"child\")
         s      (vis/db-create-connection! dir)]
     (try
       (when marker (spit marker \"ready\"))
       (Thread/sleep 250)
       (vis/db-store-conversation! s {:channel :child :title title})
       (println \"CHILD-DONE\" title)
       (finally
         (vis/db-dispose-connection! s))))")

(defonce ^:private child-output-futures (atom {}))

(defn- java-command []
  (str (fs/file (System/getProperty "java.home") "bin" "java")))

(defn- start-multiprocess-writer!
  ([dir marker]
   (start-multiprocess-writer! dir marker "child"))
  ([dir marker title]
   (let [pb (ProcessBuilder.
              ^java.util.List
              [(java-command)
               "-cp" (System/getProperty "java.class.path")
               (str "-Dvis.test.db-dir=" dir)
               (str "-Dvis.test.marker=" (or marker ""))
               (str "-Dvis.test.title=" title)
               "clojure.main"
               "-e" multiprocess-child-code])]
     (.redirectErrorStream pb true)
     (let [child (.start pb)]
       (swap! child-output-futures assoc child (future (slurp (.getInputStream child))))
       child))))

(defn- wait-for-file
  [path timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (fs/exists? path) true
        (>= (System/currentTimeMillis) deadline) false
        :else (do
                (Thread/sleep 25)
                (recur))))))

(defn- expect-child-success!
  [^Process child]
  (expect (true? (.waitFor child 10 TimeUnit/SECONDS)))
  (let [output-future (get @child-output-futures child)
        output        (when output-future (deref output-future 1000 ""))]
    (swap! child-output-futures dissoc child)
    (expect (= 0 (.exitValue child)))
    (expect (str/includes? output "CHILD-DONE"))))

(defdescribe sqlite-multiprocess-write-test
  (it "allows two JVMs to open the same persistent store and write while both are alive"
    (let [dir    (fs/create-temp-dir {:prefix "vis-db-multiprocess-"})
          marker (fs/file dir "child-opened")]
      (try
        (let [parent (vis/db-create-connection! (str dir))
              child  (start-multiprocess-writer! (str dir) (str marker))]
          (try
            (expect (true? (wait-for-file marker 10000)))
            (vis/db-store-conversation! parent {:channel :parent :title "parent"})
            (expect-child-success! child)
            (expect (= #{"child" "parent"}
                      (set (map :title
                             (raw-query parent {:select [:title]
                                                :from   [:conversation_state]})))))
            (finally
              (when (.isAlive child)
                (.destroyForcibly child))
              (vis/db-dispose-connection! parent))))
        (finally
          (fs/delete-tree dir)))))

  (it "serializes first-open migrations across JVMs"
    (let [dir (fs/create-temp-dir {:prefix "vis-db-multiprocess-migrate-"})]
      (try
        (let [child-a (start-multiprocess-writer! (str dir) nil "child-a")
              child-b (start-multiprocess-writer! (str dir) nil "child-b")]
          (try
            (expect-child-success! child-a)
            (expect-child-success! child-b)
            (let [s (vis/db-create-connection! (str dir))]
              (try
                (expect (= #{"child-a" "child-b"}
                          (set (map :title
                                 (raw-query s {:select [:title]
                                               :from   [:conversation_state]})))))
                (finally
                  (vis/db-dispose-connection! s))))
            (finally
              (doseq [child [child-a child-b]]
                (when (.isAlive child)
                  (.destroyForcibly child))))))
        (finally
          (fs/delete-tree dir))))))

(defdescribe sqlite-transaction-mode-test
  (it "reproduces SQLITE_BUSY_SNAPSHOT with a stale deferred read transaction"
    (let [db-file (File/createTempFile "vis-busy-snapshot" ".db")
          url     (str "jdbc:sqlite:" (.getAbsolutePath db-file))
          c1      (java.sql.DriverManager/getConnection url)
          c2      (java.sql.DriverManager/getConnection url)]
      (try
        (jdbc/execute! c1 ["PRAGMA journal_mode=WAL"])
        (jdbc/execute! c1 (sql/format {:create-table [:snapshot_probe :if-not-exists]
                                       :with-columns [[:id :integer :primary-key]
                                                      [:v :integer]]}))
        (jdbc/execute! c1 (sql/format {:insert-into :snapshot_probe
                                       :values [{:id 1 :v 0}]}))
        (.setAutoCommit c1 false)
        (jdbc/execute! c1 (sql/format {:select [:*]
                                       :from   [:snapshot_probe]
                                       :where  [:= :id 1]}))
        (jdbc/execute! c2 (sql/format {:update :snapshot_probe
                                       :set    {:v 1}
                                       :where  [:= :id 1]}))
        (let [thrown (try
                       (jdbc/execute! c1 (sql/format {:update :snapshot_probe
                                                      :set    {:v 2}
                                                      :where  [:= :id 1]}))
                       nil
                       (catch Throwable t t))]
          (expect (some? thrown))
          (expect (str/includes? (.getMessage thrown) "SQLITE_BUSY_SNAPSHOT")))
        (finally
          (.close c1)
          (.close c2)
          (fs/delete-if-exists db-file)))))

  (it "uses immediate transactions so read-then-write transactions survive concurrent telemetry writes"
    (let [dir (fs/create-temp-dir {:prefix "vis-snapshot-lock-"})]
      (try
        (let [s       (vis/db-create-connection! (str dir))
              cid     (vis/db-store-conversation! s {:channel :cli :title "old"})
              started (CountDownLatch. 1)
              worker  (future
                        (try
                          (jdbc/with-transaction [tx (:datasource s)]
                            (jdbc/execute! tx
                              (sql/format {:select [:id]
                                           :from   [:conversation_soul]
                                           :where  [:= :id (str cid)]}))
                            (.countDown started)
                            (Thread/sleep 100)
                            (jdbc/execute! tx
                              (sql/format {:update :conversation_state
                                           :set    {:title "new"}
                                           :where  [:= :conversation_soul_id (str cid)]})))
                          nil
                          (catch Throwable t t)))]
          (try
            (expect (true? (.await started 1 TimeUnit/SECONDS)))
            (vis/db-log! s {:level :info :event :snapshot-test})
            (expect (nil? @worker))
            (expect (= "new" (:title (vis/db-get-conversation s cid))))
            (finally
              (vis/db-dispose-connection! s))))
        (finally
          (fs/delete-tree dir)))))

  (it "serializes Vis write APIs while concurrent db-log! calls contend"
    (let [s       (h/store)
          cid     (vis/db-store-conversation! s {:channel :cli})
          tid     (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                      :user-request "stress"
                                                      :status :running})
          started (CountDownLatch. 1)
          done    (CountDownLatch. 1)
          worker  (future
                    (.countDown started)
                    (.await done 1 TimeUnit/SECONDS)
                    (try
                      (vis/db-store-iteration! s {:conversation-turn-id tid
                                                  :blocks [{:code "(+ 1 2)" :result 3}]})
                      nil
                      (catch Throwable t t)))
          logs    (do
                    (expect (true? (.await started 1 TimeUnit/SECONDS)))
                    (vec (doall (map (fn [i]
                                       (future
                                         (try
                                           (vis/db-log! s {:level :info :event (str "stress." i)})
                                           nil
                                           (catch Throwable t t))))
                                  (range 20)))))]
      (.countDown done)
      (expect (nil? @worker))
      (doseq [f logs]
        (expect (nil? @f)))
      (expect (= 20 (raw-count s :log)))
      (expect (= 1 (count (vis/db-list-conversation-turn-iterations s tid))))))

  (it "retries a whole SQLite write operation after a busy snapshot failure"
    (let [attempts (atom 0)
          retry!   (private-core-fn "sqlite-write-tx!")
          result   (retry! (h/store)
                     (fn [_]
                       (if (= 1 (swap! attempts inc))
                         (throw (RuntimeException. "[SQLITE_BUSY_SNAPSHOT] stale snapshot"))
                         :ok)))]
      (expect (= :ok result))
      (expect (= 2 @attempts)))))

(defdescribe v1-schema-resource-only-test
  (it "does not repair local databases that are missing migration-owned tables"
    (let [dir (fs/create-temp-dir {:prefix "vis-v1-no-repair-"})]
      (try
        (let [s (vis/db-create-connection! (str dir))]
          (vis/db-dispose-connection! s))
        (drop-conversation-intent-schema! (fs/file dir "vis.db"))
        (let [s (vis/db-create-connection! (str dir))]
          (try
            (let [cid (vis/db-store-conversation! s {:channel :cli})
                  tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                          :user-request "missing schema"
                                                          :status :running})]
              (expect (some? (try
                               (vis/db-store-intent! s {:conversation-turn-id tid
                                                        :title "No repair"
                                                        :rationale "Schema must come from migration resource."})
                               nil
                               (catch Throwable t t)))))
            (finally
              (vis/db-dispose-connection! s))))
        (finally
          (fs/delete-tree dir))))))

;; =============================================================================
;; Conversation
;; =============================================================================

(defdescribe conversation-test
  (it "inserts into conversation_soul + conversation_state"
    (let [s  (h/store)
          id (vis/db-store-conversation! s {:channel :tui :system-prompt "Hi" :model "gpt-4o" :title "T"})
          conversation (vis/db-get-conversation s id)]
      (expect (= 1 (raw-count s :conversation_soul)))
      (expect (= 1 (raw-count s :conversation_state)))
      (expect (= :tui (:channel conversation)))
      (expect (= "Hi" (:system-prompt conversation)))
      (expect (= "gpt-4o" (:model conversation)))
      (expect (= "T" (:title conversation)))
      (expect (= 0 (:version conversation)))))

  (it "resolves :latest"
    (let [s (h/store)]
      (vis/db-store-conversation! s {:channel :tui})
      (Thread/sleep 2)
      (let [id2    (vis/db-store-conversation! s {:channel :tui})
            latest (vis/db-resolve-conversation-id s :latest)]
        (expect (= id2 latest)))))

  (it "lists by channel via metadata JSON"
    (let [s (h/store)]
      (vis/db-store-conversation! s {:channel :tui :title "A"})
      (vis/db-store-conversation! s {:channel :telegram :title "B"})
      (vis/db-store-conversation! s {:channel :tui :title "C"})
      (expect (= 2 (count (vis/db-list-conversations s :tui))))
      (expect (= 1 (count (vis/db-list-conversations s :telegram))))))

  (it "reports fork count after the latest state becomes the fork"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui :title "A"})]
      (vis/db-fork-conversation! s cid {})
      (expect (= [1] (mapv :fork-count (vis/db-list-conversations s :tui))))
      (vis/db-fork-conversation! s cid {})
      (expect (= [2] (mapv :fork-count (vis/db-list-conversations s :tui))))))

  (it "finds by external-id via metadata JSON"
    (let [s  (h/store)
          id (vis/db-store-conversation! s {:channel :telegram :external-id "chat-42"})]
      (expect (= id (vis/db-find-conversation-by-external s :telegram "chat-42")))
      (expect (nil? (vis/db-find-conversation-by-external s :telegram "nope")))))

  (it "updates title on conversation_state"
    (let [s  (h/store)
          id (vis/db-store-conversation! s {:channel :tui :title "Old"})]
      (vis/db-update-conversation-title! s id "New")
      (expect (= "New" (:title (vis/db-get-conversation s id)))))))

;; =============================================================================
;; List conversation states (fork tree introspection)
;; =============================================================================

(defdescribe db-list-conversation-states-test
  (it "returns one row for the trunk before any fork happens"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})
          rows (vis/db-list-conversation-states s cid)]
      (expect (vector? rows))
      (expect (= 1 (count rows)))
      (expect (= 0 (:version (first rows))))
      (expect (nil? (:parent-state-id (first rows))))
      (expect (= "v0" (:system-prompt (first rows))))
      (expect (= "gpt-4o" (:model (first rows))))
      (expect (= 0 (:turn-count (first rows))))))

  (it "surfaces every fork in version order with parent links"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})]
      (vis/db-fork-conversation! s cid {:title "Branch A" :system-prompt "vA"})
      (vis/db-fork-conversation! s cid {:title "Branch B" :system-prompt "vB"})
      (let [rows (vis/db-list-conversation-states s cid)]
        (expect (= 3 (count rows)))
        (expect (= [0 1 2] (mapv :version rows)))
        (expect (nil? (:parent-state-id (nth rows 0))))
        ;; Each later fork's parent points at the immediately previous
        ;; state (latest-state-for picks the highest-version row).
        (expect (= (:state-id (nth rows 0)) (:parent-state-id (nth rows 1))))
        (expect (= (:state-id (nth rows 1)) (:parent-state-id (nth rows 2))))
        (expect (= ["vA" "vB"] (mapv :system-prompt (drop 1 rows)))))))

  (it "reports :turn-count per state — turns belong to one specific branch"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "trunk Q1" :status :done})
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "trunk Q2" :status :done})
      (vis/db-fork-conversation! s cid {:title "Branch"})
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "branch Q1" :status :done})
      (let [rows (vis/db-list-conversation-states s cid)]
        (expect (= [2 1] (mapv :turn-count rows))))))

  (it "returns [] (vector, never nil) for an unknown conversation-id"
    (let [s    (h/store)
          rows (vis/db-list-conversation-states s (random-uuid))]
      (expect (vector? rows))
      (expect (= [] rows))))

  (it "returns [] (vector, never nil) when conversation-id is nil"
    (let [s (h/store)]
      (expect (= [] (vis/db-list-conversation-states s nil))))))

;; =============================================================================
;; List turn states (retry history introspection)
;; =============================================================================

(defdescribe db-list-conversation-turn-states-test
  (it "returns one row for the original run before any retry"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "do the thing" :status :running})
          rows (vis/db-list-conversation-turn-states s qid)]
      (expect (vector? rows))
      (expect (= 1 (count rows)))
      (expect (= 0 (:version (first rows))))
      (expect (nil? (:forked-from-conversation-turn-state-id (first rows))))
      (expect (= :running (:status (first rows))))
      (expect (= 0 (:iteration-count (first rows))))))

  (it "surfaces every retry in version order with forked-from links"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "flaky" :status :error})]
      (vis/db-retry-conversation-turn! s qid {:status :running :model "claude-4" :provider :anthropic})
      (vis/db-retry-conversation-turn! s qid {:status :done    :model "gpt-4o"   :provider :openai})
      (let [rows (vis/db-list-conversation-turn-states s qid)]
        (expect (= 3 (count rows)))
        (expect (= [0 1 2] (mapv :version rows)))
        (expect (nil? (:forked-from-conversation-turn-state-id (nth rows 0))))
        (expect (= (:state-id (nth rows 0)) (:forked-from-conversation-turn-state-id (nth rows 1))))
        (expect (= (:state-id (nth rows 1)) (:forked-from-conversation-turn-state-id (nth rows 2))))
        (expect (= ["claude-4" "gpt-4o"] (mapv :model (drop 1 rows))))
        (expect (= [:anthropic :openai]   (mapv :provider (drop 1 rows)))))))

  (it "returns [] (vector, never nil) for an unknown conversation-turn-id"
    (let [s    (h/store)
          rows (vis/db-list-conversation-turn-states s (random-uuid))]
      (expect (vector? rows))
      (expect (= [] rows))))

  (it "returns [] (vector, never nil) when conversation-turn-id is nil"
    (let [s (h/store)]
      (expect (= [] (vis/db-list-conversation-turn-states s nil))))))

;; =============================================================================
;; Fork
;; =============================================================================

(defdescribe fork-test
  (it "creates a new conversation_state row with parent_state_id"
    (let [s    (h/store)
          cid  (vis/db-store-conversation! s {:channel :tui :system-prompt "v0" :model "gpt-4o"})
          _    (vis/db-fork-conversation! s cid {:title "Branch A"})
          conversation (vis/db-get-conversation s cid)]
      (expect (= 2 (raw-count s :conversation_state)))
      (expect (= 1 (:version conversation)))
      (expect (= "Branch A" (:title conversation)))
      (expect (= "v0" (:system-prompt conversation)))
      (let [states (raw-query s {:select [:*] :from :conversation_state :order-by [[:version :asc]]})]
        (expect (nil? (:parent_state_id (first states))))
        (expect (some? (:parent_state_id (second states)))))))

  (it "overrides model and system-prompt"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui :system-prompt "old" :model "gpt-4o"})
          _   (vis/db-fork-conversation! s cid {:system-prompt "new" :model "claude-4"})
          conversation (vis/db-get-conversation s cid)]
      (expect (= "new" (:system-prompt conversation)))
      (expect (= "claude-4" (:model conversation)))))

  (it "turns on forked state are isolated"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "Turn 1" :status :done})
      (vis/db-fork-conversation! s cid {:title "Fork"})
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "Turn 2" :status :done})
      (let [turns (vis/db-list-conversation-turns s cid)]
        (expect (= 1 (count turns)))
        (expect (= "Turn 2" (:user-request (first turns)))))))

  (it "double fork increments version"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-fork-conversation! s cid {})
      (vis/db-fork-conversation! s cid {})
      (expect (= 2 (:version (vis/db-get-conversation s cid))))
      (expect (= 3 (raw-count s :conversation_state)))))

  (it "returns nil instead of throwing when there is no state to fork"
    (let [s (h/store)]
      (expect (nil? (vis/db-fork-conversation! s (random-uuid) {}))))))

;; =============================================================================
;; Turn
;; =============================================================================

(defdescribe turn-test
  (it "inserts into conversation_turn_soul + conversation_turn_state"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "2+2?" :status :running})
      (expect (= 1 (raw-count s :conversation_turn_soul)))
      (expect (= 1 (raw-count s :conversation_turn_state)))
      (let [q (first (vis/db-list-conversation-turns s cid))]
        (expect (= "2+2?" (:user-request q)))
        (expect (= :running (:status q))))))

  (it "assigns turn positions from 1 within the active conversation state"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "one" :status :done})
      (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "two" :status :done})
      (expect (= [1 2] (mapv :position (vis/db-list-conversation-turns s cid))))))

  (it "normalizes :success to done"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-update-conversation-turn! s qid {:status :success :answer "42"
                                               :tokens {:input 100 :output 50}
                                               :cost {:total-cost 0.005 :model "gpt-4o"}})
      (let [q (first (vis/db-list-conversation-turns s cid))]
        (expect (= :done (:status q)))
        (expect (= "gpt-4o" (:model q))))))

  (it "persists :error without renormalization"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-update-conversation-turn! s qid {:status :error})
      (expect (= :error (:status (first (vis/db-list-conversation-turns s cid))))))))

;; =============================================================================
;; Conversation-scoped intents: intent -> plan -> gate refs
;; =============================================================================

(defdescribe conversation-intent-test
  (it "persists conversation-scoped intent plan gate and exactly one proof ref"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "ship it"
                                                     :status :running})
          iid    (vis/db-store-iteration! s {:conversation-turn-id tid
                                             :blocks [{:code "(+ 1 2)"
                                                       :result 3}
                                                      {:code "(+ 2 3)"
                                                       :result 5}]})
          ref    (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/1")
          ref-2  (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/2")
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Ship it"
                                          :rationale "User asked for it."})
          plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                        :summary "Inspect and verify."
                                        :steps [{:id :verify}]})
          slot   [(:id intent) :verification]
          plan-dsl (:plan plan)
          gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                        :proposition "Verification passes."
                                        :expected-proof {:slots {slot {:required? true}}
                                                         :guard [:exists [:slot slot :ref]]}})
          _candidate (vis/db-offer-proof! s {:gate-id (:id gate)
                                             :slots {slot {:ref ref}}
                                             :refs [ref]})
          proven (vis/db-prove-gate! s {:gate-id (:id gate)
                                        :summary "Verification passed."
                                        :refs [ref]})
          fulfilled (vis/db-fulfill-intent! s (:id intent)
                      {:summary "Done."
                       :refs [ref]})
          proven-again (vis/db-prove-gate! s {:gate-id (:id gate)
                                              :summary "Verification passed again."
                                              :refs [ref]
                                              :slots {slot {:ref ref}}})
          fulfilled-again (vis/db-fulfill-intent! s (:id intent)
                            {:summary "Done again."
                             :refs [ref ref-2]})
          state  (vis/db-intents s {:conversation-turn-id tid})]
      (expect (= 1 (raw-count s :conversation_intent)))
      (expect (= 1 (raw-count s :conversation_intent_plan)))
      (expect (= 1 (raw-count s :conversation_intent_gate)))
      (expect (= 1 (raw-count s :conversation_intent_gate_ref)))
      (expect (= 2 (raw-count s :conversation_intent_ref)))
      (expect (= :verify (get-in plan-dsl [:steps 0 :id])))
      (expect (= :proven (:status proven)))
      (expect (= :proven (:status proven-again)))
      (expect (= "Verification passes." (:proposition gate)))
      (expect (= ref (get-in proven [:proof :slots slot :ref])))
      (expect (= :fulfilled (:status fulfilled)))
      (expect (= :fulfilled (:status fulfilled-again)))
      (expect (= "Done." (:fulfillment-summary fulfilled-again)))
      (expect (= ref (-> fulfilled :refs first :ref)))
      (expect (= :fulfillment-evidence (-> fulfilled :refs first :role)))
      (expect (= true (:success? state)))
      (expect (= ref (get-in (vis/db-list-iteration-blocks s iid) [0 :provenance :ref])))
      (expect (= ref-2 (-> fulfilled-again :refs last :ref)))
      (expect (= :vis/sci (get-in (vis/db-list-iteration-blocks s iid) [0 :rendering-kind])))))

  (it "derives abandonment refs from required impeded gates when refs are omitted"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "review ideas"
                                                     :status :running})
          iid    (vis/db-store-iteration! s {:conversation-turn-id tid
                                             :blocks [{:code "(v/needs-input \"Paste ideas.\")"
                                                       :result :vis/system
                                                       :rendering-kind :vis/system}]})
          ref    (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/1")
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Review ideas"
                                          :rationale "User asked."})
          plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                        :summary "Need user input."})
          gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                        :proposition "Ideas are available."
                                        :required? true})]
      (vis/db-impede-gate! s {:gate-id (:id gate)
                              :reason "User has not provided ideas."
                              :refs [ref]})
      (let [abandoned (vis/db-abandon-intent! s (:id intent)
                        {:reason "Cannot review ideas until user provides them."})
            state     (vis/db-intents s {:conversation-turn-id tid})]
        (expect (= :abandoned (:status abandoned)))
        (expect (= "Cannot review ideas until user provides them."
                  (:abandonment-reason abandoned)))
        (expect (= ref (-> abandoned :refs first :ref)))
        (expect (= :abandonment-evidence (-> abandoned :refs first :role)))
        (expect (= true (:success? state)))
        (expect (= ref (get-in (vis/db-list-iteration-blocks s iid) [0 :provenance :ref])))
        (expect (= 1 (raw-count s :conversation_intent_ref))))))

  (it "rejects compact provenance refs"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "ship it"
                                                     :status :running})
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Ship it"
                                          :rationale "User asked for it."})
          plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                        :summary "Plan"})
          gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                        :question "Verified?"})]
      (let [thrown (try
                     (vis/db-prove-gate! s {:gate-id (:id gate)
                                            :summary "Nope"
                                            :refs ["i1.1"]})
                     nil
                     (catch Exception e e))]
        (expect (some? thrown))
        (expect (str/includes? (ex-message thrown) "canonical")))))

  (it "explains canonical refs that are not observed yet and suggests nearest observed refs"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "ship it"
                                                     :status :running})
          _iid   (vis/db-store-iteration! s {:conversation-turn-id tid
                                             :blocks [{:code "(+ 1 2)"
                                                       :result 3}]})
          observed-ref (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/1")
          future-ref   (str "turn/" (subs (str tid) 0 8) "/iteration/2/block/1")
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Ship it"
                                          :rationale "User asked for it."})
          plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                        :summary "Plan"})
          gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                        :question "Verified?"})
          thrown (try
                   (vis/db-prove-gate! s {:gate-id (:id gate)
                                          :summary "Future ref guessed."
                                          :refs [future-ref]})
                   nil
                   (catch Exception e e))]
      (expect (some? thrown))
      (expect (str/includes? (ex-message thrown) "syntactically valid but is not observed yet"))
      (expect (str/includes? (ex-message thrown) "Current iteration refs are not valid until the next iteration"))
      (expect (str/includes? (ex-message thrown) "(v/latest-provenance-refs)"))
      (expect (str/includes? (ex-message thrown) observed-ref))
      (expect (= :not-observed-yet (:reason (ex-data thrown))))
      (expect (= observed-ref (-> thrown ex-data :nearest-observed first :ref)))))

  (it "supersedes the previous active plan for an intent"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "plan"
                                                     :status :running})
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Plan"
                                          :rationale "User asked."})
          p1     (vis/db-store-plan! s {:intent-id (:id intent) :summary "First"})
          p2     (vis/db-store-plan! s {:intent-id (:id intent) :summary "Second"})
          plans  (raw-query s {:select [:id :status]
                               :from :conversation_intent_plan
                               :order-by [[:created_at :asc]]})]
      (expect (= [[(str (:id p1)) "superseded"] [(str (:id p2)) "active"]]
                (mapv (juxt :id :status) plans)))))

  (it "reports unresolved focused intents as blocking"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "ship it"
                                                     :status :running})
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Ship it"
                                          :rationale "User asked for it."})
          state  (vis/db-intents s {:conversation-turn-id tid})]
      (expect (= false (:success? state)))
      (expect (some #(= :missing-active-plan (:type %)) (:violations state)))
      (expect (= [(:id intent)] (:focused-intent-ids state)))))

  (it "carries unresolved focus into the next turn and rejects unrelated new intent focus"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          t1     (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "finish schema"
                                                     :status :running})
          intent (vis/db-store-intent! s {:conversation-turn-id t1
                                          :title "Finish schema"
                                          :rationale "User asked."})
          t2     (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "unrelated work"
                                                     :status :running})
          inferred (vis/db-infer-focus! s t2 {:rationale "carry focus"})
          thrown (try
                   (vis/db-store-intent! s {:conversation-turn-id t2
                                            :title "Unrelated work"
                                            :rationale "User asked."})
                   nil
                   (catch Exception e e))]
      (expect (= [(:id intent)] (:focused-intent-ids inferred)))
      (expect (some? thrown))
      (expect (str/includes? (ex-message thrown) "unresolved intent"))))

  (it "persists a running lifecycle child event for deferred future results and rejects it as proof"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                     :user-request "ship it"
                                                     :status :running})
          fut    (future (Thread/sleep 1000) :done)
          iid    (vis/db-store-iteration! s {:conversation-turn-id tid
                                             :blocks [{:code "(future :done)"
                                                       :result fut}]})
          future-ref (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/1/tool/future")
          intent (vis/db-store-intent! s {:conversation-turn-id tid
                                          :title "Ship it"
                                          :rationale "User asked for it."})
          plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                        :summary "Plan"})
          gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                        :question "Did deferred work complete?"})
          [block] (vis/db-list-iteration-blocks s iid)
          thrown (try
                   (vis/db-prove-gate! s {:gate-id (:id gate)
                                          :summary "Future completed."
                                          :refs [future-ref]})
                   nil
                   (catch Exception e e))]
      (future-cancel fut)
      (expect (= {:vis/ref :expr} (:result block)))
      (expect (= future-ref (get-in block [:events 0 :provenance :ref])))
      (expect (= :running (get-in block [:events 0 :provenance :status])))
      (expect (some? thrown))
      (expect (str/includes? (ex-message thrown) "completed successful lifecycle event")))))

(it "persists await-proof timeout as terminal blocker-compatible lifecycle evidence"
  (let [s      (h/store)
        cid    (vis/db-store-conversation! s {:channel :tui})
        tid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                   :user-request "ship it"
                                                   :status :running})
        timeout-result {:success? false
                        :result nil
                        :provenance {:op :future/await
                                     :status :timeout
                                     :started-at-ms 10
                                     :finished-at-ms 15
                                     :duration-ms 5}
                        :error {:type "java.util.concurrent.TimeoutException"
                                :message "Timed out"
                                :trace []}}
        iid    (vis/db-store-iteration! s {:conversation-turn-id tid
                                           :blocks [{:code "(v/await-proof! f {:timeout-ms 1})"
                                                     :result timeout-result}]})
        await-ref (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/1/tool/future.await")
        intent (vis/db-store-intent! s {:conversation-turn-id tid
                                        :title "Ship it"
                                        :rationale "User asked for it."})
        plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                      :summary "Plan"})
        gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                      :question "Did deferred work complete?"})
        [block] (vis/db-list-iteration-blocks s iid)
        impeded (vis/db-impede-gate! s {:gate-id (:id gate)
                                        :reason "Deferred work timed out."
                                        :refs [await-ref]})
        proof-thrown (try
                       (vis/db-prove-gate! s {:gate-id (:id gate)
                                              :summary "Future completed."
                                              :refs [await-ref]})
                       nil
                       (catch Exception e e))]
    (expect (= await-ref (get-in block [:events 0 :provenance :ref])))
    (expect (= :timeout (get-in block [:events 0 :provenance :status])))
    (expect (= :impeded (:status impeded)))
    (expect (some? proof-thrown))
    (expect (str/includes? (ex-message proof-thrown) "completed successful lifecycle event"))))

;; =============================================================================
;; Retry
;; =============================================================================

(defdescribe retry-test
  (it "creates conversation_turn_state version 1 with forked_from ref"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "hard" :status :running})]
      (vis/db-update-conversation-turn! s qid {:status :error})
      (vis/db-retry-conversation-turn! s qid {:status :running :model "claude-4"})
      (expect (= 1 (raw-count s :conversation_turn_soul)))
      (expect (= 2 (raw-count s :conversation_turn_state)))
      (expect (= :running (:status (first (vis/db-list-conversation-turns s cid)))))))

  (it "iterations on retry go to new conversation_turn_state"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "1" :result 1}] :duration-ms 10})
      (vis/db-update-conversation-turn! s qid {:status :error})
      (vis/db-retry-conversation-turn! s qid {:status :running :model "better"})
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "2" :result 2}] :duration-ms 5})
      (expect (= 2 (raw-count s :iteration)))
      (expect (= 1 (count (vis/db-list-conversation-turn-iterations s qid)))))))

;; =============================================================================
;; Iteration + stateless blocks
;; =============================================================================

(defdescribe iteration-block-test
  (it "writes one iteration row whose :blocks BLOB carries every form"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(+ 1 1)" :result 2 :execution-time-ms 5}
                                           {:code "(* 3 4)" :result 12 :execution-time-ms 3}]
                                  :thinking "Computing" :duration-ms 50})
      (expect (= 1 (raw-count s :iteration)))
      ;; No more kind='call' rows — the call log lives inline in the
      ;; iteration.blocks Nippy blob.
      (expect (= 0 (raw-count s :expression_soul)))
      (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
            blocks    (vis/db-list-iteration-blocks s (:id iteration))]
        (expect (= "Computing" (:thinking iteration)))
        (expect (= 1 (:position iteration)))
        (expect (= 2 (count blocks)))
        (expect (= "(+ 1 1)" (:code (first blocks))))
        (expect (= 2 (:result (first blocks))))
        (expect (= "(* 3 4)" (:code (second blocks))))
        (expect (= 12 (:result (second blocks)))))))

  (it "assigns iteration positions from 1 and rejects non-contiguous manual positions"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 1})
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 1})
      (expect (= [1 2] (mapv :position (vis/db-list-conversation-turn-iterations s qid))))
      (let [turn-state-id (:id (first (raw-query s {:select [:id]
                                                    :from :conversation_turn_state
                                                    :where [:= :conversation_turn_soul_id (str qid)]})))
            thrown (try
                     (raw-query s {:insert-into :iteration
                                   :values [{:id (str (random-uuid))
                                             :conversation_turn_state_id turn-state-id
                                             :position 4
                                             :status "done"
                                             :llm_returned_empty_blocks 1
                                             :created_at 1}]})
                     nil
                     (catch Exception e e))]
        (expect (some? thrown))
        (expect (re-find #"iteration position must increment by 1" (ex-message thrown))))))

  (it "round-trips block-level provenance through the BLOB"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          provenance {:op :sci/eval
                      :started-at-ms 10
                      :finished-at-ms 11
                      :duration-ms 1}]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(+ 1 1)"
                                            :result 2
                                            :execution-time-ms 1
                                            :provenance provenance}]
                                  :duration-ms 5})
      (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
            [exec] (vis/db-list-iteration-blocks s (:id iteration))]
        (expect (= (assoc provenance
                     :ref (str "turn/" (subs (str qid) 0 8) "/iteration/1/block/1")
                     :status :done)
                  (:provenance exec)))
        (expect (= :vis/sci (:rendering-kind exec))))))

  (it "persists running tool-start child events when a block times out before tool completion"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(v/bash \"sleep 10\")"
                                            :error "Timeout (120s)"
                                            :timeout? true
                                            :execution-time-ms 120000
                                            :tool-events [{:phase :tool-start
                                                           :op :v/bash
                                                           :status :running
                                                           :started-at-ms 123
                                                           :tool {:sym 'bash :call "v/bash"}}]}]
                                  :duration-ms 120000})
      (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
            [exec] (vis/db-list-iteration-blocks s (:id iteration))
            [event] (:events exec)]
        (expect (= :timeout (get-in exec [:provenance :status])))
        (expect (= :running (get-in event [:provenance :status])))
        (expect (= :v/bash (get-in event [:provenance :op])))
        (expect (= (str (get-in exec [:provenance :ref]) "/tool/v.bash")
                  (get-in event [:provenance :ref])))
        (expect (= 123 (get-in event [:provenance :started-at-ms])))
        (expect (= "This event proves only that a tool was started, not that it completed."
                  (get-in event [:metadata :proof-note]))))))

  (it "replaces fn results with the {:vis/ref :expr} sentinel (freeze-safe contract)"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(defn f [x] x)" :result (fn [x] x)}]
                                  :duration-ms 5})
      (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
            [{:keys [result]}] (vis/db-list-iteration-blocks s (:id iteration))]
        (expect (= {:vis/ref :expr} result)))))

  (it "errors carry the message + stdout + stderr in the BLOB"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(/ 1 0)" :error "Divide by zero"
                                            :stdout "dbg" :stderr "warn"}]
                                  :duration-ms 5})
      (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
            [exec]    (vis/db-list-iteration-blocks s (:id iteration))]
        (expect (= "Divide by zero" (:error exec)))
        (expect (= "dbg" (:stdout exec)))
        (expect (= "warn" (:stderr exec)))
        ;; :result intentionally omitted on error — cond-> drops nil.
        (expect (not (contains? exec :result))))))

  (it ":comment field carries leading `;; … / #_(...)` blocks alongside :code"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(+ 1 1)"
                                            :comment ";; double-check arithmetic"
                                            :result 2 :execution-time-ms 1}]
                                  :duration-ms 5})
      (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
            [exec]    (vis/db-list-iteration-blocks s (:id iteration))]
        (expect (= "(+ 1 1)" (:code exec)))
        (expect (= ";; double-check arithmetic" (:comment exec))))))

  ;; Regression: until the position computation was fixed, every
  ;; `db-store-iteration!` after the first one in the same conversation_turn_state
  ;; collided on `UNIQUE (conversation_turn_state_id, position)` because the
  ;; SELECT aliased the count as `row_count` (HoneySQL underscorifies
  ;; `:row-count`) while the lookup used `:row-count` (hyphen),
  ;; returning `nil` and pinning every position to 1. Drive at least
  ;; three iterations on the same qid so the count would have to land
  ;; at 1, 2, 3 monotonically.
  (it "increments position monotonically across iterations in the same conversation_turn_state"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "1" :result 1}] :duration-ms 1})
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "2" :result 2}] :duration-ms 1})
      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "3" :result 3}] :duration-ms 1})
      (let [iterations (vis/db-list-conversation-turn-iterations s qid)
            positions  (sort (mapv :position iterations))]
        (expect (= 3 (count iterations)))
        (expect (= [1 2 3] positions)))))

  ;; Token + cost round-trip — iteration.llm_input_tokens /
  ;; llm_output_tokens / llm_reasoning_tokens / llm_cached_tokens /
  ;; llm_cost_usd are written by db-store-iteration! when the caller
  ;; passes :tokens / :cost-usd, and surfaced by db-list-conversation-turn-iterations
  ;; under :input-tokens / :output-tokens / :reasoning-tokens /
  ;; :cached-tokens / :cost-usd. Pinned so a future schema rewrite
  ;; that drops or renames the columns trips this test before it
  ;; ships.
  (it "persists per-iteration token + cost columns and surfaces them on read"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(+ 1 1)" :result 2}]
                                  :duration-ms 5
                                  :tokens   {:input 1200 :output 150 :reasoning 80 :cached 600}
                                  :cost-usd 0.0123})
      (let [iter (first (vis/db-list-conversation-turn-iterations s qid))]
        (expect (= 1200 (:input-tokens iter)))
        (expect (= 150  (:output-tokens iter)))
        (expect (= 80   (:reasoning-tokens iter)))
        (expect (= 600  (:cached-tokens iter)))
        (expect (= 0.0123 (:cost-usd iter))))))

  (it "defaults absent token + cost columns to 0 / 0.0 on read"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      ;; Caller passes neither :tokens nor :cost-usd — the columns
      ;; stay NULL on disk, but the read side normalizes to 0 / 0.0
      ;; so consumers never have to `or`-pad. Callers that need to
      ;; distinguish "no usage reported" from "zero tokens" can
      ;; check the raw column via :metadata or a custom query; the
      ;; default API path is always numeric.
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(+ 1 1)" :result 2}]
                                  :duration-ms 5})
      (let [iter (first (vis/db-list-conversation-turn-iterations s qid))]
        (expect (= 0   (:input-tokens iter)))
        (expect (= 0   (:output-tokens iter)))
        (expect (= 0   (:reasoning-tokens iter)))
        (expect (= 0   (:cached-tokens iter)))
        (expect (= 0.0 (:cost-usd iter))))))

  (it "persists raw LLM response diagnostics and executable code-block metadata"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          raw "```clojure\n(+ 1 1)\n```"]
      (vis/db-store-iteration! s {:conversation-turn-id qid
                                  :blocks [{:code "(+ 1 1)" :result 2}]
                                  :duration-ms 5
                                  :llm-raw-response raw
                                  :llm-executable-code "(+ 1 1)"
                                  :llm-executable-blocks [{:lang "clojure" :source "(+ 1 1)"}]})
      (let [iter (first (vis/db-list-conversation-turn-iterations s qid))]
        (expect (= raw (:llm-raw-response iter)))
        (expect (= raw (:llm-raw-response-preview iter)))
        (expect (= (count raw) (:llm-raw-response-length iter)))
        (expect (= "66668222ec30f95b93cbd218b2406162d0bdb0e0d02b95db890a9d08d60592ed"
                  (:llm-raw-response-sha256 iter)))
        (expect (= "(+ 1 1)" (:llm-executable-code iter)))
        (expect (= [{:lang "clojure" :source "(+ 1 1)"}]
                  (:llm-executable-blocks iter))))))

  (it "rejects negative token counts via the schema CHECK"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      ;; Negative usage is structurally impossible — the schema CHECK
      ;; is the last line of defence. Any caller that fabricates a
      ;; negative value gets a SQLite constraint exception (wrapped
      ;; through next.jdbc). lazytest has no `thrown?` macro; use a
      ;; plain try/catch and assert the throw landed.
      (let [thrown? (try (vis/db-store-iteration! s {:conversation-turn-id qid
                                                     :blocks [{:code "x" :result 1}]
                                                     :tokens   {:input -5 :output 10}})
                      false
                      (catch Throwable _ true))]
        (expect (true? thrown?))))))

;; =============================================================================
;; Stateful vars
;; =============================================================================

(defdescribe var-test
  (it "inserts expression_soul(var, stateful) + expression_state"
    (let [s    (h/store)
          cid  (vis/db-store-conversation! s {:channel :tui})
          qid  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid  (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                           :vars [{:name "x" :value 42 :code "(def x 42)"}]})
          vars (vis/db-list-iteration-vars s iid)]
      (expect (= 1 (raw-count s :expression_soul [:= :kind "var"])))
      (expect (= 1 (count vars)))
      (expect (= "x" (:name (first vars))))
      (expect (= 42 (:value (first vars))))
      (expect (= 0 (:version (first vars))))))

  (it "reuses soul, increments version"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 1}]})
          i2  (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 99}]})]
      (expect (= 1 (raw-count s :expression_soul [:= :kind "var"])))
      (expect (= 99 (:value (first (vis/db-list-iteration-vars s i2)))))
      (expect (= 1 (:version (first (vis/db-list-iteration-vars s i2)))))))

  (it "soul persists across queries"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          q1  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "t1" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id q1 :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 1}]})
          q2  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "t2" :status :done})
          i2  (vis/db-store-iteration! s {:conversation-turn-id q2 :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 100}]})]
      (expect (= 1 (raw-count s :expression_soul [:= :kind "var"])))
      (expect (= 100 (:value (first (vis/db-list-iteration-vars s i2)))))))

  (it "fn var stores {:vis/ref :expr}"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "f" :value (fn [x] x) :code "(defn f [x] x)"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      (expect (= {:vis/ref :expr} (:value v)))
      (expect (= "(defn f [x] x)" (:code v)))))

  (it "stores complex data via nippy"
    (let [s    (h/store)
          cid  (vis/db-store-conversation! s {:channel :tui})
          qid  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          data {:users [{:name "Alice"} {:name "Bob"}] :tags #{:a :b} :n 42}
          iid  (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                           :vars [{:name "db" :value data}]})]
      (expect (= data (:value (first (vis/db-list-iteration-vars s iid)))))))

  (it "latest var registry"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 1} {:name "y" :value "hi"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 99}]})
          reg (vis/db-latest-var-registry s cid)]
      (expect (= 99 (:value (get reg 'x))))
      (expect (= 1 (:version (get reg 'x))))
      (expect (= "hi" (:value (get reg 'y))))
      (expect (= 0 (:version (get reg 'y))))))

  (it "version history"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0 :vars [{:name "x" :value 1}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0 :vars [{:name "x" :value 50}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0 :vars [{:name "x" :value 99}]})
          h   (vis/db-var-history s cid 'x)]
      (expect (= 3 (count h)))
      (expect (= [1 50 99] (mapv :value h)))
      (expect (= [0 1 2] (mapv :version h)))))

  (it "builds a compact latest symbol index with provenance"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid
                                          :blocks [{:code "(def x 1)" :result 1}]
                                          :duration-ms 0
                                          :vars [{:name "x" :value 1 :code "(def x 1)"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid
                                          :blocks [{:code "(defn f [x] x)" :result (fn [x] x)}]
                                          :duration-ms 0
                                          :vars [{:name "f" :value (fn [x] x) :code "(defn f [x] x)"}]})
          idx (vis/db-var-history-index s cid {:limit 10})]
      (expect (= '[f x] (mapv :name idx)))
      (expect (= [:fn :data] (mapv :kind idx)))
      (expect (every? :restorable? idx))
      (expect (not-any? #(contains? % :value) idx))
      (expect (every? #(get-in % [:provenance :iteration-id]) idx))))

  (it "builds a newest-first value-free var history timeline"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0 :vars [{:name "x" :value 1 :code "(def x 1)"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0 :vars [{:name "x" :value 2 :code "(def x 2)"}]})
          tl  (vis/db-var-history-timeline s cid {:limit 10})]
      (expect (= [:redefined :defined] (mapv :event tl)))
      (expect (= [:persisted :persisted] (mapv :durability tl)))
      (expect (= '[x x] (mapv :symbol tl)))
      (expect (not-any? #(contains? % :value) tl)))))

;; =============================================================================
;; Cascade delete
;; =============================================================================

(defdescribe cascade-delete-test
  (it "deletes soul + all descendants"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "1" :result 1}]
                                          :duration-ms 0 :vars [{:name "x" :value 1}]})]
      (vis/db-delete-conversation-tree! s cid)
      (expect (= 0 (raw-count s :conversation_soul)))
      (expect (= 0 (raw-count s :conversation_state)))
      (expect (= 0 (raw-count s :conversation_turn_soul)))
      (expect (= 0 (raw-count s :conversation_turn_state)))
      (expect (= 0 (raw-count s :iteration)))
      (expect (= 0 (raw-count s :expression_soul)))
      (expect (= 0 (raw-count s :expression_state))))))

;; =============================================================================
;; Turn history
;; =============================================================================

(defdescribe turn-history-test
  (it "builds ordered history with iteration counts"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "What?" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :answer "A Lisp" :duration-ms 100})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :answer "JVM Lisp" :duration-ms 50})
          h   (vis/db-turn-history s cid)]
      (expect (= 1 (count h)))
      (expect (= "What?" (:user-request (first h))))
      (expect (= 2 (:iteration-count (first h)))))))

;; =============================================================================
;; Soul/state FK integrity
;; =============================================================================

(defdescribe soul-state-integrity-test
  (it "conversation_state.conversation_soul_id points to conversation_soul.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui :title "FK test"})]
      (let [soul  (first (raw-query s {:select [:id] :from :conversation_soul}))
            state (first (raw-query s {:select [:conversation_soul_id] :from :conversation_state}))]
        (expect (= (:id soul) (:conversation_soul_id state))))))

  (it "conversation_turn_soul.conversation_state_id points to conversation_state.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (let [state (first (raw-query s {:select [:id] :from :conversation_state}))
            qsoul (first (raw-query s {:select [:conversation_state_id] :from :conversation_turn_soul}))]
        (expect (= (:id state) (:conversation_state_id qsoul))))))

  (it "conversation_turn_state.conversation_turn_soul_id points to conversation_turn_soul.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          _   (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (let [qsoul  (first (raw-query s {:select [:id] :from :conversation_turn_soul}))
            qstate (first (raw-query s {:select [:conversation_turn_soul_id] :from :conversation_turn_state}))]
        (expect (= (:id qsoul) (:conversation_turn_soul_id qstate))))))

  (it "iteration.conversation_turn_state_id points to conversation_turn_state.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0})]
      (let [qstate (first (raw-query s {:select [:id] :from :conversation_turn_state}))
            iteration (first (raw-query s {:select [:conversation_turn_state_id] :from :iteration}))]
        (expect (= (:id qstate) (:conversation_turn_state_id iteration))))))

  (it "expression_soul.conversation_state_id (var rows) points to conversation_state.id"
    ;; Only var rows live in expression_soul now. Drive a `(def …)`
    ;; through the iteration so a var-soul row actually exists.
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks []
                                          :duration-ms 0
                                          :vars [{:name "x" :value 1 :code "(def x 1)"}]})]
      (let [state (first (raw-query s {:select [:id] :from :conversation_state}))
            esoul (first (raw-query s {:select [:conversation_state_id] :from :expression_soul}))]
        (expect (= (:id state) (:conversation_state_id esoul))))))

  (it "expression_state.expression_soul_id (var rows) points to expression_soul.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks []
                                          :duration-ms 0
                                          :vars [{:name "x" :value 1 :code "(def x 1)"}]})]
      (let [esoul  (first (raw-query s {:select [:id] :from :expression_soul}))
            estate (first (raw-query s {:select [:expression_soul_id] :from :expression_state}))]
        (expect (= (:id esoul) (:expression_soul_id estate))))))

  (it "expression_state.iteration_id (var rows) points to iteration.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks []
                                          :duration-ms 0
                                          :vars [{:name "x" :value 1 :code "(def x 1)"}]})]
      (let [iteration (first (raw-query s {:select [:id] :from :iteration}))
            estate (first (raw-query s {:select [:iteration_id] :from :expression_state}))]
        (expect (= (:id iteration) (:iteration_id estate))))))

  (it "retry conversation_turn_state.forked_from_conversation_turn_state_id points to previous conversation_turn_state.id"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})]
      (vis/db-update-conversation-turn! s qid {:status :error})
      (vis/db-retry-conversation-turn! s qid {:status :running :model "claude-4"})
      (let [states (raw-query s {:select [:id :version :forked_from_conversation_turn_state_id]
                                 :from :conversation_turn_state :order-by [[:version :asc]]})]
        (expect (= 2 (count states)))
        (expect (nil? (:forked_from_conversation_turn_state_id (first states))))
        (expect (= (:id (first states)) (:forked_from_conversation_turn_state_id (second states)))))))

  (it "fork conversation_state.parent_state_id points to previous state"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-fork-conversation! s cid {:title "fork"})
      (let [states (raw-query s {:select [:id :version :parent_state_id]
                                 :from :conversation_state :order-by [[:version :asc]]})]
        (expect (= 2 (count states)))
        (expect (nil? (:parent_state_id (first states))))
        (expect (= (:id (first states)) (:parent_state_id (second states)))))))

  (it "expression_soul holds ONLY var rows; blocks live inline on iteration.blocks"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid
                                          :blocks [{:code "(+ 1 1)" :result 2}]
                                          :duration-ms 0
                                          :vars [{:name "x" :value 42 :code "(def x 42)"}]})]
      (let [souls (raw-query s {:select [:kind :state_mode :name] :from :expression_soul})]
        ;; Per-form calls do not land in expression_soul. Only the var
        ;; soul lands there; the block log lives in the iteration.blocks
        ;; Nippy blob.
        (expect (= 1 (count souls)))
        (let [{:keys [kind state_mode name]} (first souls)]
          (expect (= "var" kind))
          (expect (= "stateful" state_mode))
          (expect (= "x" name)))
        (let [iteration (first (vis/db-list-conversation-turn-iterations s qid))
              [exec]    (vis/db-list-iteration-blocks s (:id iteration))]
          (expect (= "(+ 1 1)" (:code exec)))
          (expect (= 2 (:result exec))))))))

;; =============================================================================
;; System var versioning across iterations within a turn
;; =============================================================================

(defdescribe system-var-versioning-test
  (it "ITERATION_PREVIOUS_REASONING gets a new version each iteration, all under same soul"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "Explain monads" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "(+ 1 1)" :result 2}]
                                          :duration-ms 100
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "First I need to understand what a monad is" :code ";; SYSTEM var"}
                                                 {:name "TURN_USER_REQUEST" :value "Explain monads" :code ";; SYSTEM var"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [{:code "(str \"A monad is\")" :result "A monad is"}]
                                          :duration-ms 200
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "Now I can explain: a monad wraps computation" :code ";; SYSTEM var"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks []
                                          :duration-ms 50
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "Final check: the explanation covers functor, applicative, monad" :code ";; SYSTEM var"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "A monad is a design pattern..." :code ";; SYSTEM var"}]})]
      ;; Only 1 expression_soul for ITERATION_PREVIOUS_REASONING (reused across iterations)
      (expect (= 1 (raw-count s :expression_soul [:and [:= :kind "var"] [:= :name "ITERATION_PREVIOUS_REASONING"]])))
      ;; 3 versions of ITERATION_PREVIOUS_REASONING
      (let [history (vis/db-var-history s cid 'ITERATION_PREVIOUS_REASONING)]
        (expect (= 3 (count history)))
        (expect (= [0 1 2] (mapv :version history)))
        (expect (= "First I need to understand what a monad is" (:value (nth history 0))))
        (expect (= "Now I can explain: a monad wraps computation" (:value (nth history 1))))
        (expect (= "Final check: the explanation covers functor, applicative, monad" (:value (nth history 2)))))
      ;; TURN_USER_REQUEST has only 1 version (set once on first iteration)
      (let [qh (vis/db-var-history s cid 'TURN_USER_REQUEST)]
        (expect (= 1 (count qh)))
        (expect (= "Explain monads" (:value (first qh)))))
      ;; CONVERSATION_PREVIOUS_ANSWER has only 1 version (set on final iteration)
      (let [ah (vis/db-var-history s cid 'CONVERSATION_PREVIOUS_ANSWER)]
        (expect (= 1 (count ah)))
        (expect (= "A monad is a design pattern..." (:value (first ah)))))))

  (it "latest var registry returns max version for each system var"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "test" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "TURN_USER_REQUEST" :value "test" :code ";; SYSTEM"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "step 1" :code ";; SYSTEM"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "step 2" :code ";; SYSTEM"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "step 3" :code ";; SYSTEM"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "42" :code ";; SYSTEM"}]})
          reg (vis/db-latest-var-registry s cid)]
      (expect (= "test" (:value (get reg 'TURN_USER_REQUEST))))
      (expect (= 0 (:version (get reg 'TURN_USER_REQUEST))))
      (expect (= "step 3" (:value (get reg 'ITERATION_PREVIOUS_REASONING))))
      (expect (= 2 (:version (get reg 'ITERATION_PREVIOUS_REASONING))))
      (expect (= "42" (:value (get reg 'CONVERSATION_PREVIOUS_ANSWER))))
      (expect (= 0 (:version (get reg 'CONVERSATION_PREVIOUS_ANSWER))))))

  (it "user vars and system vars coexist, each with independent version chains"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "compute" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid
                                          :blocks [{:code "(def data [1 2 3])" :result [1 2 3]}]
                                          :duration-ms 10
                                          :vars [{:name "data" :value [1 2 3] :code "(def data [1 2 3])"}
                                                 {:name "TURN_USER_REQUEST" :value "compute" :code ";; SYSTEM"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "I need to sum the data" :code ";; SYSTEM"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid
                                          :blocks [{:code "(def result (reduce + data))" :result 6}]
                                          :duration-ms 5
                                          :vars [{:name "result" :value 6 :code "(def result (reduce + data))"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "Sum is 6, done" :code ";; SYSTEM"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "6" :code ";; SYSTEM"}]})
          reg (vis/db-latest-var-registry s cid)]
      ;; 5 distinct var souls
      (expect (= 5 (count reg)))
      ;; User vars
      (expect (= [1 2 3] (:value (get reg 'data))))
      (expect (= 0 (:version (get reg 'data))))
      (expect (= 6 (:value (get reg 'result))))
      (expect (= 0 (:version (get reg 'result))))
      ;; System vars
      (expect (= "compute" (:value (get reg 'TURN_USER_REQUEST))))
      (expect (= "Sum is 6, done" (:value (get reg 'ITERATION_PREVIOUS_REASONING))))
      (expect (= 1 (:version (get reg 'ITERATION_PREVIOUS_REASONING))))
      (expect (= "6" (:value (get reg 'CONVERSATION_PREVIOUS_ANSWER))))))

  (it "system vars version across multiple queries in same conversation"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          ;; Turn 1
          q1  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "What is 2+2?" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id q1 :blocks [] :duration-ms 0
                                          :vars [{:name "TURN_USER_REQUEST" :value "What is 2+2?" :code ";; SYSTEM"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "Simple math" :code ";; SYSTEM"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "4" :code ";; SYSTEM"}]})
          ;; Turn 2
          q2  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "And 3+3?" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id q2 :blocks [] :duration-ms 0
                                          :vars [{:name "TURN_USER_REQUEST" :value "And 3+3?" :code ";; SYSTEM"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "Another simple one" :code ";; SYSTEM"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "6" :code ";; SYSTEM"}]})
          reg (vis/db-latest-var-registry s cid)]
      ;; Each system var should have version 1 (v0 from turn 1, v1 from turn 2)
      (expect (= "And 3+3?" (:value (get reg 'TURN_USER_REQUEST))))
      (expect (= 1 (:version (get reg 'TURN_USER_REQUEST))))
      (expect (= "Another simple one" (:value (get reg 'ITERATION_PREVIOUS_REASONING))))
      (expect (= 1 (:version (get reg 'ITERATION_PREVIOUS_REASONING))))
      (expect (= "6" (:value (get reg 'CONVERSATION_PREVIOUS_ANSWER))))
      (expect (= 1 (:version (get reg 'CONVERSATION_PREVIOUS_ANSWER))))
      ;; Full history for CONVERSATION_PREVIOUS_ANSWER shows both turns
      (let [h (vis/db-var-history s cid 'CONVERSATION_PREVIOUS_ANSWER)]
        (expect (= 2 (count h)))
        (expect (= ["4" "6"] (mapv :value h)))))))

;; =============================================================================
;; Answer lifecycle
;; =============================================================================

(defdescribe answer-lifecycle-test
  (it "conversation_turn_state metadata stores answer on update"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "2+2?" :status :running})]
      ;; Before update — no answer in metadata
      (let [q (first (vis/db-list-conversation-turns s cid))]
        (expect (= :running (:status q)))
        (expect (nil? (:answer q))))
      ;; After update — answer present
      (vis/db-update-conversation-turn! s qid {:answer "4" :status :success :iteration-count 1 :duration-ms 500})
      (let [q   (first (vis/db-list-conversation-turns s cid))
            raw (first (raw-query s {:select [:metadata] :from :conversation_turn_state}))]
        (expect (= :done (:status q)))
        (expect (= "4" (:answer q)))
        ;; Verify it's in the JSON metadata, not pr-str'd
        (expect (clojure.string/includes? (:metadata raw) "\"answer\":\"4\"")))))

  (it "CONVERSATION_PREVIOUS_ANSWER var tracks across turns"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          ;; Turn 1: answer is 4
          q1  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "2+2?" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id q1 :blocks [{:code "(+ 2 2)" :result 4}]
                                          :duration-ms 100 :answer "4"
                                          :vars [{:name "CONVERSATION_PREVIOUS_ANSWER" :value "4" :code ";; SYSTEM"}]})
          _   (vis/db-update-conversation-turn! s q1 {:answer "4" :status :success :iteration-count 1})
          ;; Turn 2: answer changes to 6
          q2  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "3+3?" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id q2 :blocks [{:code "(+ 3 3)" :result 6}]
                                          :duration-ms 80 :answer "6"
                                          :vars [{:name "CONVERSATION_PREVIOUS_ANSWER" :value "6" :code ";; SYSTEM"}]})
          _   (vis/db-update-conversation-turn! s q2 {:answer "6" :status :success :iteration-count 1})
          ;; Turn 3: answer changes to 10
          q3  (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "5+5?" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id q3 :blocks [{:code "(+ 5 5)" :result 10}]
                                          :duration-ms 60 :answer "10"
                                          :vars [{:name "CONVERSATION_PREVIOUS_ANSWER" :value "10" :code ";; SYSTEM"}]})
          _   (vis/db-update-conversation-turn! s q3 {:answer "10" :status :success :iteration-count 1})]
      ;; Latest registry shows final answer
      (let [reg (vis/db-latest-var-registry s cid)]
        (expect (= "10" (:value (get reg 'CONVERSATION_PREVIOUS_ANSWER))))
        (expect (= 2 (:version (get reg 'CONVERSATION_PREVIOUS_ANSWER)))))
      ;; Full history shows all 3 answers in order
      (let [h (vis/db-var-history s cid 'CONVERSATION_PREVIOUS_ANSWER)]
        (expect (= 3 (count h)))
        (expect (= ["4" "6" "10"] (mapv :value h)))
        (expect (= [0 1 2] (mapv :version h))))
      ;; Each conversation_turn_state has its own answer in metadata
      (let [states (raw-query s {:select [:qs.user_request :qst.metadata]
                                 :from [[:conversation_turn_soul :qs]]
                                 :join [[:conversation_turn_state :qst] [:= :qst.conversation_turn_soul_id :qs.id]]
                                 :order-by [[:qs.created_at :asc]]})]
        (expect (= 3 (count states)))
        (expect (clojure.string/includes? (:metadata (nth states 0)) "\"4\""))
        (expect (clojure.string/includes? (:metadata (nth states 1)) "\"6\""))
        (expect (clojure.string/includes? (:metadata (nth states 2)) "\"10\""))))))

;; =============================================================================
;; Restore — dependency chains, topological order, sandbox reconstruction
;; =============================================================================

(defdescribe restore-test
  (it "restores vars in topological order — no dependencies"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "x" :value 42 :code "(def x 42)"}
                                                 {:name "y" :value "hello" :code "(def y \"hello\")"}
                                                 {:name "z" :value [1 2 3] :code "(def z [1 2 3])"}]})
          restored (vis/db-restore-blocks s cid)]
      (expect (= 3 (count restored)))
      (expect (= #{"x" "y" "z"} (set (map :name restored))))
      ;; All have data values
      (expect (= 42 (:result (first (filter #(= "x" (:name %)) restored)))))
      (expect (= "hello" (:result (first (filter #(= "y" (:name %)) restored)))))
      (expect (= [1 2 3] (:result (first (filter #(= "z" (:name %)) restored)))))
      ;; No dependencies
      (expect (every? #(empty? (:depends-on %)) restored))))

  (it "restores fn vars with {:vis/ref :expr}"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "double-it" :value (fn [x] (* x 2))
                                                  :code "(defn double-it [x] (* x 2))"}]})
          restored (vis/db-restore-blocks s cid)
          entry    (first restored)]
      (expect (= "double-it" (:name entry)))
      (expect (= {:vis/ref :expr} (:result entry)))
      (expect (= "(defn double-it [x] (* x 2))" (:expr entry)))))

  (it "linear dependency chain A → B → C restored in correct order"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          ;; Iteration 1: define base-rate
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "base-rate" :value 0.05 :code "(def base-rate 0.05)"}]})
          ;; Iteration 2: define calc-interest (depends on base-rate)
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "calc-interest" :value (fn [p] p)
                                                     :code "(defn calc-interest [principal] (* principal base-rate))"}]})
          ;; Iteration 3: define monthly-payment (depends on calc-interest)
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "monthly-payment" :value (fn [p] p)
                                                     :code "(defn monthly-payment [principal] (/ (calc-interest principal) 12))"}]})
          ;; Now wire the dependencies
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul
                                 :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; base-rate → calc-interest
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "calc-interest")
                                   :upstream-soul-id   (soul-by "base-rate")})
      ;; calc-interest → monthly-payment
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "monthly-payment")
                                   :upstream-soul-id   (soul-by "calc-interest")})
      (let [restored (vis/db-restore-blocks s cid)
            names    (mapv :name restored)]
        (expect (= 3 (count restored)))
        ;; base-rate MUST come before calc-interest, calc-interest before monthly-payment
        (expect (< (.indexOf names "base-rate") (.indexOf names "calc-interest")))
        (expect (< (.indexOf names "calc-interest") (.indexOf names "monthly-payment")))
        ;; base-rate has data, the fns have :vis/ref :expr
        (expect (= 0.05 (:result (first restored))))
        (expect (= {:vis/ref :expr} (:result (second restored))))
        (expect (= {:vis/ref :expr} (:result (nth restored 2))))
        ;; Dependency metadata is correct
        (let [calc (first (filter #(= "calc-interest" (:name %)) restored))]
          (expect (= [(soul-by "base-rate")] (:depends-on calc)))
          (expect (= [(soul-by "monthly-payment")] (:depended-by calc)))))))

  (it "diamond dependency: D depends on B and C, both depend on A"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "config" :value {:rate 0.1} :code "(def config {:rate 0.1})"}
                                                    {:name "tax-fn" :value (fn [x] x) :code "(defn tax-fn [amount] (* amount (:rate config)))"}
                                                    {:name "fee-fn" :value (fn [x] x) :code "(defn fee-fn [amount] (+ 10 (* amount (:rate config))))"}
                                                    {:name "total-fn" :value (fn [x] x) :code "(defn total-fn [amount] (+ (tax-fn amount) (fee-fn amount)))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; config → tax-fn, config → fee-fn
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "tax-fn")
                                   :upstream-soul-id   (soul-by "config")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "fee-fn")
                                   :upstream-soul-id   (soul-by "config")})
      ;; tax-fn → total-fn, fee-fn → total-fn
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "total-fn")
                                   :upstream-soul-id   (soul-by "tax-fn")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "total-fn")
                                   :upstream-soul-id   (soul-by "fee-fn")})
      (let [restored (vis/db-restore-blocks s cid)
            names    (mapv :name restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 4 (count restored)))
        ;; config must come before tax-fn AND fee-fn
        (expect (< (idx "config") (idx "tax-fn")))
        (expect (< (idx "config") (idx "fee-fn")))
        ;; tax-fn and fee-fn must come before total-fn
        (expect (< (idx "tax-fn") (idx "total-fn")))
        (expect (< (idx "fee-fn") (idx "total-fn")))
        ;; config is data, rest are fn refs
        (expect (= {:rate 0.1} (:result (nth restored (idx "config")))))
        (expect (= {:vis/ref :expr} (:result (nth restored (idx "total-fn")))))
        ;; total-fn depends on both tax-fn and fee-fn
        (let [total (nth restored (idx "total-fn"))]
          (expect (= 2 (count (:depends-on total))))
          (expect (= #{(soul-by "tax-fn") (soul-by "fee-fn")} (set (:depends-on total))))))))

  (it "deep chain: 5 levels deep, each depending on previous"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          var-names ["level-0" "level-1" "level-2" "level-3" "level-4"]
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars (mapv (fn [n] {:name n :value (fn [x] x)
                                                                  :code (str "(defn " n " [x] x)")}) var-names)})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; Chain: level-0 → level-1 → level-2 → level-3 → level-4
      (doseq [i (range 4)]
        (vis/db-store-dependency! s {:conversation-state-id state-id
                                     :downstream-soul-id (soul-by (var-names (inc i)))
                                     :upstream-soul-id   (soul-by (var-names i))}))
      (let [restored (vis/db-restore-blocks s cid)
            names    (mapv :name restored)]
        (expect (= 5 (count restored)))
        ;; Strict order: level-0, level-1, level-2, level-3, level-4
        (expect (= var-names names)))))

  (it "mixed data + fn vars with system vars, all restored correctly"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "analyze data" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "TURN_USER_REQUEST" :value "analyze data" :code ";; SYSTEM"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "step 1" :code ";; SYSTEM"}
                                                 {:name "dataset" :value [{:x 1 :y 2} {:x 3 :y 4}]
                                                  :code "(def dataset [{:x 1 :y 2} {:x 3 :y 4}])"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "step 2" :code ";; SYSTEM"}
                                                 {:name "summarize" :value (fn [ds] ds)
                                                  :code "(defn summarize [ds] (map :x ds))"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "ITERATION_PREVIOUS_REASONING" :value "step 3" :code ";; SYSTEM"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "[1 3]" :code ";; SYSTEM"}
                                                 {:name "result" :value [1 3]
                                                  :code "(def result (summarize dataset))"}]})
          ;; Wire: dataset → summarize (it reads dataset), dataset + summarize → result
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "summarize")
                                   :upstream-soul-id   (soul-by "dataset")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "result")
                                   :upstream-soul-id   (soul-by "dataset")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "result")
                                   :upstream-soul-id   (soul-by "summarize")})
      (let [restored (vis/db-restore-blocks s cid)
            names    (mapv :name restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)]
        ;; All 6 vars present (3 system + 3 user)
        (expect (= 6 (count restored)))
        ;; System vars have data values, latest versions
        (expect (= "analyze data" (:result (by-name "TURN_USER_REQUEST"))))
        (expect (= "step 3" (:result (by-name "ITERATION_PREVIOUS_REASONING"))))
        (expect (= "[1 3]" (:result (by-name "CONVERSATION_PREVIOUS_ANSWER"))))
        ;; dataset is data
        (expect (= [{:x 1 :y 2} {:x 3 :y 4}] (:result (by-name "dataset"))))
        ;; summarize is fn ref
        (expect (= {:vis/ref :expr} (:result (by-name "summarize"))))
        ;; result is data (was computed)
        (expect (= [1 3] (:result (by-name "result"))))
        ;; Topological order: dataset before summarize, both before result
        (expect (< (idx "dataset") (idx "summarize")))
        (expect (< (idx "dataset") (idx "result")))
        (expect (< (idx "summarize") (idx "result")))))))

;; =============================================================================
;; Sophisticated dependency patterns
;; =============================================================================

(defdescribe advanced-dependency-test
  (it "higher-order fn: make-adder returns a fn, result bound as var"
    ;; (defn make-adder [n] (fn [x] (+ x n)))
    ;; (def add-10 (make-adder 10))
    ;; (def result (add-10 5))  => 15
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "make-adder" :value (fn [n] (fn [x] (+ x n)))
                                                     :code "(defn make-adder [n] (fn [x] (+ x n)))"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "add-10" :value (fn [x] (+ x 10))
                                                     :code "(def add-10 (make-adder 10))"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "result" :value 15
                                                     :code "(def result (add-10 5))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; make-adder -> add-10 -> result
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "add-10")
                                   :upstream-soul-id   (soul-by "make-adder")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "result")
                                   :upstream-soul-id   (soul-by "add-10")})
      (let [restored (vis/db-restore-blocks s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            names    (mapv :name restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 3 (count restored)))
        ;; Topological: make-adder < add-10 < result
        (expect (< (idx "make-adder") (idx "add-10")))
        (expect (< (idx "add-10") (idx "result")))
        ;; make-adder is a fn -> ref
        (expect (= {:vis/ref :expr} (:result (by-name "make-adder"))))
        ;; add-10 is ALSO a fn (returned by make-adder) -> ref
        (expect (= {:vis/ref :expr} (:result (by-name "add-10"))))
        ;; result is data (15)
        (expect (= 15 (:result (by-name "result")))))))

  (it "closure chain: factory -> instance -> bound literal via eval"
    ;; (def config {:multiplier 3})
    ;; (defn make-scaler [] (let [m (:multiplier config)] (fn [x] (* x m))))
    ;; (def scale (make-scaler))
    ;; (def scaled-data (mapv scale [1 2 3 4 5]))
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "config" :value {:multiplier 3}
                                                     :code "(def config {:multiplier 3})"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "make-scaler" :value (fn [] (fn [x] (* x 3)))
                                                     :code "(defn make-scaler [] (let [m (:multiplier config)] (fn [x] (* x m))))"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "scale" :value (fn [x] (* x 3))
                                                     :code "(def scale (make-scaler))"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "scaled-data" :value [3 6 9 12 15]
                                                     :code "(def scaled-data (mapv scale [1 2 3 4 5]))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; config -> make-scaler -> scale -> scaled-data
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "make-scaler")
                                   :upstream-soul-id   (soul-by "config")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "scale")
                                   :upstream-soul-id   (soul-by "make-scaler")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "scaled-data")
                                   :upstream-soul-id   (soul-by "scale")})
      (let [restored (vis/db-restore-blocks s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 4 (count restored)))
        ;; Strict topological order
        (expect (< (idx "config") (idx "make-scaler")))
        (expect (< (idx "make-scaler") (idx "scale")))
        (expect (< (idx "scale") (idx "scaled-data")))
        ;; config = data, make-scaler = fn ref, scale = fn ref, scaled-data = data
        (expect (= {:multiplier 3} (:result (by-name "config"))))
        (expect (= {:vis/ref :expr} (:result (by-name "make-scaler"))))
        (expect (= {:vis/ref :expr} (:result (by-name "scale"))))
        (expect (= [3 6 9 12 15] (:result (by-name "scaled-data")))))))

  (it "multi-version var with dependency: changing upstream propagates ref"
    ;; Iter 1: (def base 10)
    ;; Iter 2: (defn compute [x] (+ x base))
    ;; Iter 3: (def base 20)  -- base changes!
    ;; Iter 4: (def answer (compute 5)) -- should use new base
    ;; At restore time, base=20 (latest version), compute has :vis/ref :expr
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "base" :value 10 :code "(def base 10)"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "compute" :value (fn [x] (+ x 10))
                                                     :code "(defn compute [x] (+ x base))"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "base" :value 20 :code "(def base 20)"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars [{:name "answer" :value 25
                                                     :code "(def answer (compute 5))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; base -> compute, compute -> answer, base -> answer (transitive)
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "compute")
                                   :upstream-soul-id   (soul-by "base")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "answer")
                                   :upstream-soul-id   (soul-by "compute")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "answer")
                                   :upstream-soul-id   (soul-by "base")})
      (let [restored (vis/db-restore-blocks s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 3 (count restored)))
        ;; base LATEST is 20 (version 1), not 10
        (expect (= 20 (:result (by-name "base"))))
        (expect (= 1 (:version (by-name "base"))))
        ;; compute is fn ref, needs re-eval with new base
        (expect (= {:vis/ref :expr} (:result (by-name "compute"))))
        (expect (= "(defn compute [x] (+ x base))" (:expr (by-name "compute"))))
        ;; answer is data
        (expect (= 25 (:result (by-name "answer"))))
        ;; Order: base before compute before answer
        (expect (< (idx "base") (idx "compute")))
        (expect (< (idx "compute") (idx "answer"))))))

  (it "wide fan-out: one config feeds 5 independent fns"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          qid    (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          fn-names ["fn-a" "fn-b" "fn-c" "fn-d" "fn-e"]
          _      (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                             :vars (into [{:name "shared-cfg" :value {:k 1}
                                                           :code "(def shared-cfg {:k 1})"}]
                                                     (mapv (fn [n] {:name n :value (fn [x] x)
                                                                    :code (str "(defn " n " [x] (+ x (:k shared-cfg)))")}) fn-names))})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; shared-cfg -> each fn
      (doseq [n fn-names]
        (vis/db-store-dependency! s {:conversation-state-id state-id
                                     :downstream-soul-id (soul-by n)
                                     :upstream-soul-id   (soul-by "shared-cfg")}))
      (let [restored (vis/db-restore-blocks s cid)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 6 (count restored)))
        ;; shared-cfg must be first (all fns depend on it)
        (expect (= 0 (idx "shared-cfg")))
        ;; All fns come after
        (doseq [n fn-names]
          (expect (< (idx "shared-cfg") (idx n)))))))

  (it "cross-turn dependency: var from turn 1 used by fn in turn 2"
    (let [s      (h/store)
          cid    (vis/db-store-conversation! s {:channel :tui})
          ;; Turn 1: define data
          q1     (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "load data" :status :done})
          _      (vis/db-store-iteration! s {:conversation-turn-id q1 :blocks [] :duration-ms 0
                                             :vars [{:name "raw-data" :value [10 20 30]
                                                     :code "(def raw-data [10 20 30])"}]})
          ;; Turn 2: define fn + compute result using data from turn 1
          q2     (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "process it" :status :done})
          _      (vis/db-store-iteration! s {:conversation-turn-id q2 :blocks [] :duration-ms 0
                                             :vars [{:name "avg-fn" :value (fn [xs] (/ (reduce + xs) (count xs)))
                                                     :code "(defn avg-fn [xs] (/ (reduce + xs) (count xs)))"}]})
          _      (vis/db-store-iteration! s {:conversation-turn-id q2 :blocks [] :duration-ms 0
                                             :vars [{:name "average" :value 20
                                                     :code "(def average (avg-fn raw-data))"}]})
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)]
      ;; raw-data -> avg-fn (reads it), raw-data -> average, avg-fn -> average
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "avg-fn")
                                   :upstream-soul-id   (soul-by "raw-data")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "average")
                                   :upstream-soul-id   (soul-by "raw-data")})
      (vis/db-store-dependency! s {:conversation-state-id state-id
                                   :downstream-soul-id (soul-by "average")
                                   :upstream-soul-id   (soul-by "avg-fn")})
      (let [restored (vis/db-restore-blocks s cid)
            by-name  (into {} (map (fn [e] [(:name e) e])) restored)
            idx      (into {} (map-indexed (fn [i e] [(:name e) i])) restored)]
        (expect (= 3 (count restored)))
        ;; raw-data from turn 1 is still data
        (expect (= [10 20 30] (:result (by-name "raw-data"))))
        ;; avg-fn is fn ref
        (expect (= {:vis/ref :expr} (:result (by-name "avg-fn"))))
        ;; average is computed data
        (expect (= 20 (:result (by-name "average"))))
        ;; Order: raw-data before avg-fn before average
        (expect (< (idx "raw-data") (idx "avg-fn")))
        (expect (< (idx "avg-fn") (idx "average")))))))

;; =============================================================================
;; Lazy seq / infinite range safety
;; =============================================================================

(defdescribe lazy-seq-safety-test
  (it "infinite range → {:vis/ref :expr} — never realized"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "nums" :value (range) :code "(def nums (range))"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      ;; Must not hang!
      (expect (= {:vis/ref :expr} (:value v)))
      (expect (= "(def nums (range))" (:code v)))))

  (it "infinite repeat → {:vis/ref :expr}"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "ones" :value (repeat 1) :code "(def ones (repeat 1))"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      (expect (= {:vis/ref :expr} (:value v)))))

  (it "iterate → {:vis/ref :expr}"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "nats" :value (iterate inc 0)
                                                  :code "(def nats (iterate inc 0))"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      (expect (= {:vis/ref :expr} (:value v)))))

  (it "small lazy seq (map inc [1 2 3]) → {:vis/ref :expr} — lazy is lazy"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "small" :value (map inc [1 2 3])
                                                  :code "(def small (map inc [1 2 3]))"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      ;; Even small lazy seqs are refs — they're computations, not data
      (expect (= {:vis/ref :expr} (:value v)))))

  (it "realized vector is stored as data"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "v" :value [1 2 3 4 5]
                                                  :code "(def v [1 2 3 4 5])"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      (expect (= [1 2 3 4 5] (:value v)))))

  (it "realized list is stored as data"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "l" :value '(1 2 3)
                                                  :code "(def l '(1 2 3))"}]})
          v   (first (vis/db-list-iteration-vars s iid))]
      (expect (= '(1 2 3) (:value v)))))

  (it "lazy seq inside a map → map stored, lazy value becomes ref"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          iid (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "mixed" :value {:data [1 2 3]
                                                                        :lazy (map inc [10 20])
                                                                        :infinite (range)}
                                                  :code "(def mixed {...})"}]})
          m   (:value (first (vis/db-list-iteration-vars s iid)))]
      (expect (map? m))
      (expect (= [1 2 3] (:data m)))
      ;; Both lazy seqs → ref, regardless of size
      (expect (= {:vis/ref :expr} (:lazy m)))
      (expect (= {:vis/ref :expr} (:infinite m)))))

  (it "lazy seq as expression result → ref"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :running})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid
                                          :blocks [{:code "(range)" :result (range)}
                                                   {:code "(vec (range 5))" :result [0 1 2 3 4]}]
                                          :duration-ms 5})
          iteration (first (vis/db-list-conversation-turn-iterations s qid))
          execs (vis/db-list-iteration-blocks s (:id iteration))]
      ;; (range) → ref
      (expect (= {:vis/ref :expr} (:result (first execs))))
      ;; (vec (range 5)) → realized vector, stored as data
      (expect (= [0 1 2 3 4] (:result (second execs)))))))
;; =============================================================================
;; Integration: store → wipe sandbox → restore → use
;; =============================================================================

(defdescribe restore-integration-test
  (it "data var: store, wipe, restore, read back"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          ;; Create SCI sandbox, def a var
          {:keys [sci-ctx]} (env/create-sci-context nil)
          _   (sci/eval-string+ sci-ctx "(def data [10 20 30])" {:ns (sci/find-ns sci-ctx 'sandbox)})
          ;; Store it
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "data" :value [10 20 30] :code "(def data [10 20 30])"}]})
          ;; Wipe: fresh sandbox (simulates disconnect)
          {:keys [sci-ctx]} (env/create-sci-context nil)]
      ;; Verify var is gone
      (expect (try (sci/eval-string+ sci-ctx "data" {:ns (sci/find-ns sci-ctx 'sandbox)}) false
                (catch Exception _ true)))
      ;; Restore
      (let [results (env/restore-sandbox! sci-ctx s cid)]
        (expect (= 1 (count results)))
        (expect (true? (:success? (first results))))
        (expect (= :data (:restored-via (first results)))))
      ;; Now the var works
      (expect (= [10 20 30] (:val (sci/eval-string+ sci-ctx "data" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "function var: store, wipe, restore via eval, call it"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          ;; Store a fn (result will be {:vis/ref :expr})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "double-it" :value (fn [x] (* x 2))
                                                  :code "(defn double-it [x] (* x 2))"}]})
          ;; Fresh sandbox
          {:keys [sci-ctx]} (env/create-sci-context nil)]
      ;; Restore
      (let [results (env/restore-sandbox! sci-ctx s cid)]
        (expect (= 1 (count results)))
        (expect (= :eval (:restored-via (first results))))
        (expect (true? (:success? (first results)))))
      ;; Call the restored function
      (expect (= 42 (:val (sci/eval-string+ sci-ctx "(double-it 21)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "dependency chain: data → fn → result, all restored in order"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          ;; Store chain: rate → calc → answer
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "rate" :value 0.1 :code "(def rate 0.1)"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "calc" :value (fn [x] (* x 0.1))
                                                  :code "(defn calc [amount] (* amount rate))"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "answer" :value 100.0
                                                  :code "(def answer (calc 1000))"}]})
          ;; Wire dependencies
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)
          _   (vis/db-store-dependency! s {:conversation-state-id state-id
                                           :downstream-soul-id (soul-by "calc")
                                           :upstream-soul-id   (soul-by "rate")})
          _   (vis/db-store-dependency! s {:conversation-state-id state-id
                                           :downstream-soul-id (soul-by "answer")
                                           :upstream-soul-id   (soul-by "calc")})
          _   (vis/db-store-dependency! s {:conversation-state-id state-id
                                           :downstream-soul-id (soul-by "answer")
                                           :upstream-soul-id   (soul-by "rate")})
          ;; Fresh sandbox
          {:keys [sci-ctx]} (env/create-sci-context nil)]
      ;; Restore
      (let [results (env/restore-sandbox! sci-ctx s cid)
            by-name (into {} (map (fn [r] [(:name r) r])) results)]
        ;; rate restored as data, calc as eval, answer as data
        (expect (= :data (:restored-via (by-name "rate"))))
        (expect (= :eval (:restored-via (by-name "calc"))))
        (expect (= :data (:restored-via (by-name "answer"))))
        (expect (every? :success? results)))
      ;; All work in the sandbox
      (expect (= 0.1 (:val (sci/eval-string+ sci-ctx "rate" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      (expect (= 50.0 (:val (sci/eval-string+ sci-ctx "(calc 500)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      (expect (= 100.0 (:val (sci/eval-string+ sci-ctx "answer" {:ns (sci/find-ns sci-ctx 'sandbox)}))))))

  (it "higher-order fn factory restores, derived closure is unavailable until recreated intentionally"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "make-adder" :value (fn [n] (fn [x] (+ x n)))
                                                  :code "(defn make-adder [n] (fn [x] (+ x n)))"}]})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "add-5" :value (fn [x] (+ x 5))
                                                  :code "(def add-5 (make-adder 5))"}]})
          ;; Wire: make-adder → add-5
          state-id (first (map :id (raw-query s {:select [:id] :from :conversation_state})))
          souls    (raw-query s {:select [:id :name] :from :expression_soul :where [:= :kind "var"]})
          soul-by  (into {} (map (fn [r] [(:name r) (:id r)])) souls)
          _   (vis/db-store-dependency! s {:conversation-state-id state-id
                                           :downstream-soul-id (soul-by "add-5")
                                           :upstream-soul-id   (soul-by "make-adder")})
          ;; Fresh sandbox
          {:keys [sci-ctx]} (env/create-sci-context nil)]
      (let [results (env/restore-sandbox! sci-ctx s cid)
            by-name (into {} (map (fn [r] [(:name r) r])) results)]
        (expect (= :eval (:restored-via (by-name "make-adder"))))
        (expect (= :unavailable (:restored-via (by-name "add-5"))))
        (expect (= :unsafe-restore (:reason (by-name "add-5")))))
      ;; make-adder works (safe defn source restored via eval)
      (expect (= 15 (:val (sci/eval-string+ sci-ctx "((make-adder 10) 5)" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      ;; add-5 was an effectful/closure-producing def result; recreate intentionally.
      (expect (try (sci/eval-string+ sci-ctx "(add-5 7)" {:ns (sci/find-ns sci-ctx 'sandbox)})
                false
                (catch Exception _ true)))))

  (it "system vars are restored: TURN_USER_REQUEST, ITERATION_PREVIOUS_REASONING, CONVERSATION_PREVIOUS_ANSWER"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})
          qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          _   (vis/db-store-iteration! s {:conversation-turn-id qid :blocks [] :duration-ms 0
                                          :vars [{:name "TURN_USER_REQUEST" :value "What is 2+2?" :code ";; SYSTEM var"}
                                                 {:name "ITERATION_PREVIOUS_REASONING" :value "Simple math" :code ";; SYSTEM var"}
                                                 {:name "CONVERSATION_PREVIOUS_ANSWER" :value "4" :code ";; SYSTEM var"}]})
          {:keys [sci-ctx]} (env/create-sci-context nil)]
      (let [results (env/restore-sandbox! sci-ctx s cid)
            by-name (into {} (map (fn [r] [(:name r) r])) results)]
        ;; SYSTEM vars with ;; SYSTEM var code are data-restored
        (expect (= :data (:restored-via (by-name "TURN_USER_REQUEST"))))
        (expect (= :data (:restored-via (by-name "ITERATION_PREVIOUS_REASONING"))))
        (expect (= :data (:restored-via (by-name "CONVERSATION_PREVIOUS_ANSWER"))))
        (expect (every? :success? results)))
      (expect (= "What is 2+2?" (:val (sci/eval-string+ sci-ctx "TURN_USER_REQUEST" {:ns (sci/find-ns sci-ctx 'sandbox)}))))
      (expect (= "4" (:val (sci/eval-string+ sci-ctx "CONVERSATION_PREVIOUS_ANSWER" {:ns (sci/find-ns sci-ctx 'sandbox)})))))))

;; =============================================================================
;; Log
;; =============================================================================

(defdescribe log-test
  (it "inserts into log table with FK scope"
    (let [s   (h/store)
          cid (vis/db-store-conversation! s {:channel :tui})]
      (vis/db-log! s {:level :info :event "test.event" :data "{\"k\":1}"
                      :conversation-soul-id cid})
      (expect (= 1 (raw-count s :log)))
      (let [row (first (raw-query s {:select [:*] :from :log}))]
        (expect (= "info" (:level row)))
        (expect (= "test.event" (:event row)))
        (expect (= (str cid) (:conversation_soul_id row)))))))

;; ─── from auto_archive_sqlite_test.clj ───

(h/use-mem-store!)

(defn- sci-var
  "Create a SCI var with optional :doc metadata."
  ([sym value]
   (sci/new-var sym value))
  ([sym value doc]
   (sci/new-var sym value {:doc doc})))

(defn- make-sandbox
  "Build a sandbox-map {symbol -> sci-var} from a seq of
   [sym value] or [sym value doc] triples."
  [entries]
  (into {}
    (map (fn [[sym value & [doc]]]
           [sym (if doc (sci-var sym value doc) (sci-var sym value))]))
    entries))

(defn- make-sci-ctx
  "Create a minimal SCI context with a sandbox namespace containing `entries`.
   Returns the sci-ctx map (with :env atom)."
  [entries]
  (let [sandbox-map (make-sandbox entries)
        env-atom    (atom {:namespaces {'sandbox sandbox-map}})]
    {:env env-atom}))

(defn- sandbox-syms
  "Return the set of symbols currently in the SCI sandbox."
  [sci-ctx]
  (set (keys (get-in @(:env sci-ctx) [:namespaces 'sandbox]))))

(defdescribe auto-archive-hot-symbols-test

  (it "archives live user symbols down to the post-answer target and bumps the var-index revision"
    (let [s       (h/store)
          cid     (vis/db-store-conversation! s {:channel :tui})
          qid     (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          entries (mapv (fn [i]
                          [(symbol (format "v%02d" i)) i])
                    (range 82))
          _       (doseq [[sym value] entries]
                    (vis/db-store-iteration! s {:conversation-turn-id qid
                                                :blocks []
                                                :duration-ms 0
                                                :vars [{:name (name sym)
                                                        :value value
                                                        :code (str "(def " (name sym) " " value ")")}]})
                    (Thread/sleep 1))
          sci-ctx (make-sci-ctx entries)
          via     (atom {:current-revision 0})
          rlm-env {:db-info         s
                   :conversation-id cid
                   :sci-ctx         sci-ctx
                   :initial-ns-keys #{}
                   :var-index-atom  via}]
      (#'lp/auto-archive-hot-symbols! rlm-env)
      (expect (= 80 (count (sandbox-syms sci-ctx))))
      (expect (not (contains? (sandbox-syms sci-ctx) 'v00)))
      (expect (not (contains? (sandbox-syms sci-ctx) 'v01)))
      (expect (contains? (sandbox-syms sci-ctx) 'v81))
      (expect (= 1 (:current-revision @via)))))

  (it "does not archive docstring-protected symbols automatically"
    (let [s       (h/store)
          cid     (vis/db-store-conversation! s {:channel :tui})
          qid     (vis/db-store-conversation-turn! s {:parent-conversation-id cid :user-request "x" :status :done})
          entries (into [['protected 0 "Durable state"]]
                    (mapv (fn [i]
                            [(symbol (format "v%02d" i)) i])
                      (range 81)))
          _       (doseq [[sym value] (map #(take 2 %) entries)]
                    (vis/db-store-iteration! s {:conversation-turn-id qid
                                                :blocks []
                                                :duration-ms 0
                                                :vars [{:name (name sym)
                                                        :value value
                                                        :code (str "(def " (name sym) " " value ")")}]})
                    (Thread/sleep 1))
          sci-ctx (make-sci-ctx entries)
          via     (atom {:current-revision 0})
          rlm-env {:db-info         s
                   :conversation-id cid
                   :sci-ctx         sci-ctx
                   :initial-ns-keys #{}
                   :var-index-atom  via}]
      (#'lp/auto-archive-hot-symbols! rlm-env)
      (expect (contains? (sandbox-syms sci-ctx) 'protected))
      (expect (= 80 (count (sandbox-syms sci-ctx)))))))

;; ─── (Removed) plan_slot_test.clj content
;;
;; The plan_state / breadcrumb / plan_diff iteration columns and every
;; helper that fed them (prompt/load-effective-plan,
;; prompt/load-breadcrumb-chain, prompt/format-loop-nudge,
;; spec/compute-plan-diff, spec/plan-edit-distance,
;; spec/validate-plan-state) were deleted in the
;; "Drastically simplify the agent" cull (commit cad5f7d) and the
;; columns were dropped from V1__schema.sql in the follow-up sweep.
;; The agent now relies on <journal> + <var_index> only; no projection
;; layer carries plan/breadcrumb data. Tests that targeted any of the
;; above had nothing live to assert against and were orphaned.
;; ───

;; =============================================================================
;; SYSTEM_VAR_NAMES — fixed registry of UPPERCASE constants
;; =============================================================================

(defdescribe system-var-registry-test
  (it "SYSTEM_VAR_NAMES contains exactly the documented fourteen SYSTEM vars"
    (expect (= '#{TURN_USER_REQUEST
                  TURN_CONVERSATION_TURN_ID
                  TURN_CONVERSATION_SOUL_ID
                  TURN_CONVERSATION_STATE_ID
                  TURN_SYSTEM_PROMPT
                  TURN_ACTIVE_EXTENSIONS
                  TURN_ACCESSIBLE_SKILLS
                  ITERATION_ID
                  ITERATION_PREVIOUS_REASONING
                  CONVERSATION_ID
                  CONVERSATION_SOUL_ID
                  CONVERSATION_STATE_ID
                  CONVERSATION_TITLE
                  CONVERSATION_PREVIOUS_ANSWER}
              @(requiring-resolve 'com.blockether.vis.core/SYSTEM_VAR_NAMES))))

  (it "system-var-sym? is true for registered names, false otherwise"
    (let [system-var-sym? (requiring-resolve
                            'com.blockether.vis.core/system-var-sym?)]
      (expect (true?  (system-var-sym? 'TURN_USER_REQUEST)))
      (expect (true?  (system-var-sym? 'TURN_CONVERSATION_TURN_ID)))
      (expect (true?  (system-var-sym? 'TURN_CONVERSATION_SOUL_ID)))
      (expect (true?  (system-var-sym? 'TURN_CONVERSATION_STATE_ID)))
      (expect (true?  (system-var-sym? 'TURN_SYSTEM_PROMPT)))
      (expect (true?  (system-var-sym? 'TURN_ACTIVE_EXTENSIONS)))
      (expect (true?  (system-var-sym? 'TURN_ACCESSIBLE_SKILLS)))
      (expect (true?  (system-var-sym? 'ITERATION_ID)))
      (expect (true?  (system-var-sym? 'ITERATION_PREVIOUS_REASONING)))
      (expect (true?  (system-var-sym? 'CONVERSATION_ID)))
      (expect (true?  (system-var-sym? 'CONVERSATION_SOUL_ID)))
      (expect (true?  (system-var-sym? 'CONVERSATION_STATE_ID)))
      (expect (true?  (system-var-sym? 'CONVERSATION_TITLE)))
      (expect (true?  (system-var-sym? 'CONVERSATION_PREVIOUS_ANSWER)))
      (expect (false? (system-var-sym? 'CONFIG)))
      (expect (false? (system-var-sym? 'foo))))))

;; =============================================================================
;; (Removed) projection-test
;;
;; This block tested prompt/build-iteration-context against keys
;; (:plan-state, :breadcrumbs, :recent-thought, :system-vars,
;;  :prior-turn, :loop-nudges) that were removed in the same
;; simplification cull. The current build-iteration-context only
;; emits <journal> + <var_index> and is exercised end-to-end by
;; the agent loop tests; an isolated unit test was not preserved.
;; =============================================================================
