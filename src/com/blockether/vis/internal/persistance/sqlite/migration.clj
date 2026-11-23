(ns com.blockether.vis.internal.persistance.sqlite.migration
  "Flyway-backed schema migration runner.

   Lives in the SQLite extension because:

     1. The dialect-specific Flyway driver (`flyway-database-nc-sqlite`)
        is required to recognize `jdbc:sqlite:` URLs - already
        declared in this extension's deps.edn.
     2. Flyway is the only backend-side concern using
        `flyway-core`; making it a per-backend dep keeps the root
        package free of the migration toolchain.
     3. The previous arrangement shipped a generic `migrate!` from
        `com.blockether.vis.sdk`, but it had exactly one caller
        - this extension. Other backends will ship their own
        migration entry point in their own jar.

   Public API:

     `(migrate! datasource locations)` - apply every Flyway
     migration found at the given classpath `locations` to the
     supplied `DataSource`. Returns the datasource for thread-style
     chaining.

   `:baseline-on-migrate true` so existing databases without a
   `flyway_schema_history` table get one on first run. `:mixed true`
   so SQL files with mixed transactional + DDL statements work under
   SQLite.

   GraalVM native-image note: Flyway discovers migrations by LISTING the
   classpath location directory, which native-image does not support (it can
   `getResource` a specific file but not enumerate a dir). So `build.clj` writes
   an `_index.edn` of filenames next to each migration dir, and here we feed
   Flyway an explicit `ResourceProvider` built from those exact paths, paired
   with an empty Java migration `ClassProvider` for our SQL-only schema. Both
   providers are required to bypass scanning. On the JVM (no index) we fall
   back to Flyway's normal location scanning."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [javax.sql DataSource]
           [java.io File]
           [java.nio.charset StandardCharsets]
           [org.flywaydb.core.api ClassProvider Location ResourceProvider]
           [org.flywaydb.core.internal.resource.classpath ClassPathResource]))

(defn- strip-classpath
  [^String loc]
  (if (str/starts-with? loc "classpath:") (subs loc (count "classpath:")) loc))

(defn- index-resource-provider
  "A `ResourceProvider` serving the migrations named in each location's
   build-generated `_index.edn`. Returns nil when no index exists (JVM/dev),
   so callers fall back to Flyway's directory scanning."
  ^ResourceProvider [locations]
  (let [cl
        (or (.getContextClassLoader (Thread/currentThread)) (.getClassLoader ResourceProvider))

        res
        (vec
          (for [loc
                locations

                :let [base
                      (strip-classpath loc)

                      idx
                      (io/resource (str base "/_index.edn"))]
                :when idx
                fname
                (edn/read-string (slurp idx))]

            (ClassPathResource. (Location. loc) (str base "/" fname) cl StandardCharsets/UTF_8)))]

    (when (seq res)
      (reify
        ResourceProvider
          (getResource [_ name]
            (some (fn [^ClassPathResource r]
                    (when (or (= name (.getFilename r)) (= name (.getRelativePath r))) r))
                  res))
          (getResources [_ prefix suffixes]
            (filterv (fn [^ClassPathResource r]
                       (let [fname (.getFilename r)]
                         (and (str/starts-with? fname (or prefix ""))
                              (boolean (some #(str/ends-with? fname %) suffixes)))))
              res))))))

(defn- repairable-validation-error?
  "True when Flyway rejected the applied migration metadata during validation.
   The canonical V1 is intentionally edited in place, and historical V2+ rows
   can remain after those migrations are folded back into V1. `repair` is the
   safe Flyway operation for both cases: it realigns checksums and marks removed
   migrations as deleted without touching application tables or their rows."
  [^Throwable e]
  (boolean (some (fn [^Throwable t]
                   (let [^String message (or (ex-message t) "")]
                     (or (str/includes? message "Migration checksum mismatch")
                         (str/includes? message "Migrations have failed validation")
                         (str/includes? message
                                        "Detected applied migration not resolved locally"))))
                 (take 16
                       (take-while some?
                                   (iterate (fn [^Throwable t]
                                              (.getCause t))
                                            e))))))

(defn- migration-filenames
  "Names of the `.sql` files at one classpath `base`. Prefers the build-generated
   `_index.edn` (the only thing that works in the native image) and falls back to
   listing the directory on the JVM."
  [^String base]
  (if-let [idx (io/resource (str base "/_index.edn"))]
    (vec (edn/read-string (slurp idx)))
    (when-let [url (io/resource base)]
      (when (= "file" (.getProtocol url))
        (->> (.listFiles (io/file (.toURI url)))
             (map (fn [^java.io.File f]
                    (.getName f)))
             (filter (fn [^String n]
                       (str/ends-with? n ".sql")))
             (sort)
             (vec))))))

(defn- migration-sql-texts
  "The SQL bodies of every migration shipped at `locations`, in file order."
  [locations]
  (vec (for [loc
             locations

             :let [base
                   (strip-classpath loc)]
             fname
             (or (migration-filenames base) [])

             :let [res
                   (io/resource (str base "/" fname))]
             :when res]

         (slurp res))))

(defn- strip-sql-comments [^String sql] (str/replace sql #"--[^\n]*" ""))

(defn- closing-paren
  "Index of the `)` closing the `(` at index `open`, or nil."
  [^String s ^long open]
  (loop [i
         (inc open)

         depth
         1

         q
         ;; NUL = "not inside a quoted literal"; a real char keeps the loop primitive.
         \u0000]

    (when (< i (.length s))
      (let [c (.charAt s i)]
        (cond (not= q \u0000) (recur (inc i) depth (if (= c q) \u0000 q))
              (or (= c \') (= c \")) (recur (inc i) depth c)
              (= c \() (recur (inc i) (inc depth) \u0000)
              (= c \)) (if (= depth 1) i (recur (inc i) (dec depth) \u0000))
              :else (recur (inc i) depth \u0000))))))

(defn- split-top-level
  "Split a `CREATE TABLE` body at the commas that separate its definitions,
   ignoring commas nested in parens (CHECK, composite keys) or quotes."
  [^String body]
  (loop [i
         0

         depth
         0

         q
         \u0000

         start
         0

         acc
         []]

    (if (>= i (.length body))
      (conj acc (subs body start))
      (let [c (.charAt body i)]
        (cond (not= q \u0000) (recur (inc i) depth (if (= c q) \u0000 q) start acc)
              (or (= c \') (= c \")) (recur (inc i) depth c start acc)
              (= c \() (recur (inc i) (inc depth) \u0000 start acc)
              (= c \)) (recur (inc i) (dec depth) \u0000 start acc)
              (and (= c \,) (zero? depth))
              (recur (inc i) depth \u0000 (inc i) (conj acc (subs body start i)))
              :else (recur (inc i) depth \u0000 start acc))))))

(def ^:private table-constraint-heads
  "Leading words of a TABLE-level constraint, i.e. not a column definition."
  #{"constraint" "primary" "unique" "check" "foreign" "exclude"})

(defn- column-definition
  "`{:name ... :sql ...}` for one member of a `CREATE TABLE` body, or nil when
   that member is a table-level constraint rather than a column."
  [^String part]
  (let [text
        (str/trim (str/replace part #"\s+" " "))

        head
        (first (str/split text #"[\s(]" 2))]

    (when (and (seq text)
               (not (contains? table-constraint-heads (str/lower-case (or head ""))))
               (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (or head "")))
      {:name head :sql text})))

(defn- canonical-columns
  "Table name -> ordered column definitions, parsed out of the canonical SQL."
  [^String sql]
  (let [s
        (strip-sql-comments sql)

        m
        (re-matcher
          #"(?i)create\s+table\s+(?:if\s+not\s+exists\s+)?[\"`\[]?([A-Za-z0-9_]+)[\"`\]]?\s*\("
          s)]

    (loop [acc {}]
      (if-not (.find m)
        acc
        (let [table (.group m 1)
              open (dec (.end m))
              close (closing-paren s open)]

          (recur (if close
                   (assoc acc
                     table (vec (keep column-definition
                                      (split-top-level (subs s (inc open) close)))))
                   acc)))))))

(defn- addable-column?
  "Select columns SQLite can add: no PRIMARY KEY / UNIQUE, and NOT NULL needs
   a DEFAULT. With foreign keys enabled, REFERENCES must be nullable with an
   implicit or explicit NULL default."
  [{:keys [^String sql]}]
  (let [u
        (str/upper-case sql)

        not-null?
        (re-find #"\bNOT\s+NULL\b" u)

        default?
        (re-find #"\bDEFAULT\b" u)]

    (and (not (re-find #"\bPRIMARY\s+KEY\b" u))
         (not (re-find #"\bUNIQUE\b" u))
         (or (not (re-find #"\bREFERENCES\b" u))
             (and (not not-null?) (or (not default?) (boolean (re-find #"\bDEFAULT\s+NULL\b" u)))))
         (or (not not-null?) (boolean default?)))))

(defn- existing-columns
  "Lower-cased column names of `table`, or an empty set when it does not exist."
  [^java.sql.Connection conn ^String table]
  (with-open [st
              (.createStatement conn)

              rs
              (.executeQuery st (str "PRAGMA table_info(" table ")"))]

    (loop [acc #{}]
      (if (.next rs) (recur (conj acc (str/lower-case (.getString rs "name")))) acc))))

(defn- stored-columns
  "The column definitions `table` actually carries, parsed out of the store's own
   `CREATE TABLE` text by the very parser the canonical file goes through, so the
   two sides are comparable. Empty when the table does not exist."
  [^java.sql.Connection conn ^String table]
  (with-open [st (.prepareStatement
                   conn
                   "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ?")]
    (.setString st 1 table)
    (with-open [rs (.executeQuery st)]
      (if (.next rs) (get (canonical-columns (or (.getString rs "sql") "")) table []) []))))

(def ^:private column-fills
  "The value of a column for the rows of a store from before that column, as
   `{[table column] expression}`. The expression reads the other columns of the row.

   `session_agent.worked_ms` is the time of the tasks that ended: each task in `runs`, then
   the current task when it has an end. `session_agent.started_at` is the hand-off of the
   current task, because an older store kept no time of its first iteration."
  {["session_agent" "started_at"] (str
                                    "CASE WHEN status IN ('waiting', 'queued', 'blocked') THEN NULL"
                                    " ELSE coalesce(resumed_at, created_at) END")
   ["session_agent" "worked_ms"]
   (str "coalesce((SELECT sum(max(0, json_extract(run.value, '$.end')"
        " - json_extract(run.value, '$.start')))"
        " FROM json_each(coalesce(session_agent.runs, '[]')) AS run), 0)"
        " + CASE WHEN finished_at IS NULL THEN 0"
        " ELSE max(0, finished_at - coalesce(resumed_at, created_at)) END")})

(defn column-fill
  "The `column-fills` expression of `column` in `table`, or nil. An import of rows from before the
   column uses it too."
  [table column]
  (get column-fills [(str/lower-case (str table)) (str/lower-case (str column))]))

(defn- reconcile-canonical-columns!
  "Additively realign a database that was created by an EARLIER copy of the
   canonical V1 with the V1 shipped now.

   V1 is intentionally edited in place, so Flyway `repair` fixes the recorded
   checksum but never re-runs the file: an older store keeps its old columns and
   the next insert dies with `no such column`. Rather than adding a V2 file, we
   diff each `CREATE TABLE` in the shipped SQL against `PRAGMA table_info` and
   `ADD COLUMN` whatever is missing, using the column's own DDL text from the SQL
   file - no schema DDL is written in Clojure. Fresh databases match already, so
   this is a no-op for them. Nullable foreign keys retain their REFERENCES
   constraints; keys, foreign keys with non-NULL defaults, and NOT NULL without
   DEFAULT are left to a new schema generation.

   A column with a `column-fills` entry gets its values in the transaction that adds it, so
   an interrupted open leaves the store without the column and the next open does both again."
  [^DataSource ds locations]
  (let [tables (reduce merge {} (map canonical-columns (migration-sql-texts locations)))]
    (when (seq tables)
      (with-open [conn (.getConnection ds)]
        (doseq [[table cols] tables
                :let [have (existing-columns conn table)]
                :when (seq have)
                col cols
                :when (and (not (contains? have (str/lower-case ^String (:name col))))
                           (addable-column? col))]

          ;; Best effort: a racing process may have added it already, and a
          ;; definition SQLite still refuses must surface at its real use site
          ;; rather than making the whole store unopenable.
          (let [fill (column-fill table (:name col))
                auto (.getAutoCommit conn)]

            (try (.setAutoCommit conn false)
                 (with-open [st (.createStatement conn)]
                   (.executeUpdate st (str "ALTER TABLE " table " ADD COLUMN " (:sql col)))
                   (when fill
                     (.executeUpdate st (str "UPDATE " table " SET " (:name col) " = " fill))))
                 (.commit conn)
                 (catch Throwable _ (try (.rollback conn) (catch Throwable _ nil)))
                 (finally (try (.setAutoCommit conn auto) (catch Throwable _ nil))))))))))

(def ^:private retired-columns
  "Columns DELETED from the canonical V1 after stores already existed, as
   `[table column]`. Named one by one on purpose: the additive pass's mirror
   image — drop whatever the shipped SQL no longer lists — would let a single
   downgrade delete the newer version's data. Append only: the entry count is part
   of `schema-generation`.

   `llm_assistant_message` held the provider's raw assistant envelope so
   preserved thinking could survive a restart. It never was replayed from disk
   (a reseeded cross-turn iteration carries `:preserved-thinking/replay? false`)
   and the signature inside it is worthless to any other provider or to a later
   day, so the column was write-only: 471 MB of a measured 2.4 GB store.

   Cost, measured on that store: 3.7 s for the table rewrite, a transient WAL
   the size of the table, and 372 MB handed back to the freelist — SQLite reuses
   those pages, and only a `VACUUM` shrinks the file itself.

   `session_soul.last_opened_at` held the last time a person opened a session.
   Recency follows the newest sent message alone, so opening a session must not
   move it.

   `council_entry.kind` classified each message as a complaint, coordination or
   information. Council messages have no kind any more.

   `session_state.system_prompt` held the core prompt text from when the session
   was created. Each environment build makes the prompt again, so nothing read the
   copy back, and it was out of date after each configuration change.

   `session_soul.goal_history` held the earlier goals of a session. The transcript
   holds each goal and the replies about it, so the copy was not necessary.

   The `workspace` columns `repo_root`, `fork_ms`, `apply_fork_ms`, `filesystem_roots`,
   `workspace_kind`, `workspace_backend`, `workspace_mechanism` and
   `parent_workspace_id` described a draft: an isolated copy of a checkout. Vis has no
   drafts any more, so a workspace is only the directory in `root`."
  [["session_turn_iteration" "llm_assistant_message"] ["session_attachment" "classification"]
   ["session_attachment" "activity_anchor"] ["session_soul" "last_opened_at"]
   ["council_entry" "kind"] ["session_state" "system_prompt"] ["session_soul" "goal_history"]
   ["workspace" "repo_root"] ["workspace" "fork_ms"] ["workspace" "apply_fork_ms"]
   ["workspace" "filesystem_roots"] ["workspace" "workspace_kind"] ["workspace" "workspace_backend"]
   ["workspace" "workspace_mechanism"] ["workspace" "parent_workspace_id"]])

(def ^:private janitor-retired-columns
  "Columns DELETED from the canonical V1 that a janitor store rewrite drops, not the
   open, as `[table column]`: the DROP builds a large table again. Append only: the
   entry count is part of `schema-generation`, so an older Vis refuses the store as
   soon as a newer Vis opened it.

   `activity_invocation.search_text` held a plain copy of each Activity presentation:
   2.4 GB of a 17 GB store. Search matches the payloads of one history."
  [["activity_invocation" "search_text"]])

(def ^:private retired-values
  "Values a narrowed CHECK no longer admits, as `[table column old new]`.

   They are rewritten BEFORE the column is realigned: the realignment copies every
   value into the narrower column, and one refused value rolls it all back.

   `council_entry.source = 'autocomplain'` marked the complaint that the host
   published itself after a failed `py3`. The host wrote it, so it
   reads as `host` now that Council has no complaints."
  [["council_entry" "source" "autocomplain" "host"]])

(def ^:private retired-rows
  "Rows DELETED before the column that identifies them is retired, as
   `[table column predicate]`.

   A retired column takes its rows' identity with it: once
   `session_attachment.classification` is gone, nothing can tell the host's own
   Activity receipts from the artifacts a person actually made. Activity is no
   longer an attachment at all — it rides the iteration's form — so those rows are
   the obsolete rail itself, and leaving them behind would fill every historical
   gallery with `.live.ndjson` files no surface can open."
  [["session_attachment" "classification" "classification = 'activity'"]])

(def ^:private retired-copies
  "Values COPIED into another column of the same row before their column is retired,
   as `[table column target predicate]`.

   `workspace.repo_root` held the checkout that a draft was a copy of, and
   `workspace.root` held the copy. Without drafts, the session of each such row works
   in the checkout again. The copy stays on disk."
  [["workspace" "repo_root" "root" "workspace_kind = 'draft'"]])

(def ^:private schema-generation
  "The generation of the store shape that this Vis leaves: one step for each entry in
   `retired-columns`, `janitor-retired-columns`, `retired-values`, `retired-rows` and
   `retired-copies`. Those lists
   only grow, never remove an entry, so a newer Vis always has a higher generation. An
   older Vis cannot run against a store that a newer Vis removed columns or values from
   (#455)."
  (+ (count retired-columns)
     (count janitor-retired-columns)
     (count retired-values)
     (count retired-rows)
     (count retired-copies)))

(defn- stored-generation
  "The generation that `conn`'s store records, or 0 for a store from before the record."
  ^long [^java.sql.Connection conn]
  (if (seq (existing-columns conn "vis_schema"))
    (with-open [st
                (.createStatement conn)

                rs
                (.executeQuery st "SELECT generation FROM vis_schema WHERE id = 1")]

      (if (.next rs) (.getLong rs 1) 0))
    0))

(defn newer-store-error
  "An error that names the cause when a newer Vis has migrated the store of
   `connectable` (a `DataSource` or an open `Connection`), else nil. `cause` is the
   failure that made the caller ask, or nil."
  [connectable cause]
  (let [stored (if (instance? java.sql.Connection connectable)
                 (stored-generation connectable)
                 (with-open [conn (.getConnection ^DataSource connectable)]
                   (stored-generation conn)))]
    (when (> stored (long schema-generation))
      (ex-info (str "The Vis database was migrated by a newer Vis (schema generation "
                    stored
                    ", this Vis has "
                    schema-generation
                    "). Upgrade Vis, or restart the newer Vis.")
               {:vis/user-error true
                :type :persistance/newer-schema
                :stored-generation stored
                :generation schema-generation}
               cause))))

(defn- record-generation!
  "Record this Vis' generation in the store. A higher recorded generation stays."
  [^DataSource ds]
  (with-open [conn
              (.getConnection ds)

              st
              (.prepareStatement
                conn
                (str "INSERT INTO vis_schema (id, generation, updated_at) VALUES (1, ?, ?)"
                     " ON CONFLICT (id) DO UPDATE SET generation = excluded.generation,"
                     " updated_at = excluded.updated_at"
                     " WHERE excluded.generation > vis_schema.generation"))]

    (.setLong st 1 (long schema-generation))
    (.setLong st 2 (System/currentTimeMillis))
    (.executeUpdate st)))

(defn- word-end
  "Index just past the identifier that starts at `i`."
  ^long [^String s ^long i ^long n]
  (loop [j i]
    (if (and (< j n)
             (let [ch (.charAt s j)]
               (or (Character/isLetterOrDigit ch) (= ch \_))))
      (recur (inc j))
      j)))

(defn- split-statements
  "The canonical DDL split into whole statements. A `;` ends one only at block
   depth zero: `BEGIN` and `CASE` open a block a trigger body closes with `END`,
   so every semicolon inside a trigger stays with its trigger."
  [^String sql]
  (let [^String s
        (strip-sql-comments sql)

        n
        (long (.length s))]

    (loop [i
           0

           q
           \u0000

           depth
           0

           start
           0

           acc
           []]

      (if (>= i n)
        (into [] (comp (map str/trim) (filter seq)) (conj acc (subs s start)))
        (let [c (.charAt s i)]
          (cond (not= q \u0000) (recur (inc i) (if (= c q) \u0000 q) depth start acc)
                (or (= c \') (= c \")) (recur (inc i) c depth start acc)
                (and (= c \;) (zero? depth))
                (recur (inc i) q depth (inc i) (conj acc (subs s start i)))
                (or (Character/isLetter c) (= c \_)) (let [e (word-end s i n)
                                                           w (str/upper-case (subs s i e))]

                                                       (recur e
                                                              q
                                                              (long (case w
                                                                      ("BEGIN" "CASE")
                                                                      (inc depth)

                                                                      "END"
                                                                      (max 0 (dec depth))

                                                                      depth))
                                                              start
                                                              acc))
                :else (recur (inc i) q depth start acc)))))))

(def ^:private create-object-re
  "Head of a `CREATE` statement: its object KIND and the name it defines."
  #"(?is)^create\s+(?:unique\s+)?(?:virtual\s+)?(table|trigger|view|index)\s+(?:if\s+not\s+exists\s+)?[\"`\[]?([A-Za-z0-9_]+)")

(defn- canonical-objects
  "Every object the shipped SQL creates, IN FILE ORDER, as
   `{:kind :name :sql}`. Recreating from this vector replays the file's own
   dependency order — an FTS index before the triggers that write to it."
  [sql-texts]
  (into []
        (comp (mapcat split-statements)
              (keep (fn [^String stmt]
                      (when-let [m (re-find create-object-re stmt)]
                        {:kind (str/lower-case (nth m 1)) :name (nth m 2) :sql stmt}))))
        sql-texts))

(defn- dependent-objects
  "Indexes, views, triggers and external-content FTS tables that use this column.
   Never drop ordinary tables: a same-named column or a comment is not a dependency."
  [^java.sql.Connection conn ^String table ^String column]
  (let [column-re
        (re-pattern (str "(?i)\\b" (java.util.regex.Pattern/quote column) "\\b"))

        table-re
        (re-pattern (str "(?i)\\b" (java.util.regex.Pattern/quote table) "\\b"))

        content-re
        (re-pattern
          (str "(?i)\\bcontent\\s*=\\s*['\"]" (java.util.regex.Pattern/quote table) "['\"]"))]

    (with-open
      [st
       (.prepareStatement
         conn
         (str
           "SELECT type, name, tbl_name, sql FROM sqlite_master"
           " WHERE type IN ('table','trigger','view','index') AND name <> ? AND sql IS NOT NULL"))]
      (.setString st 1 table)
      (with-open [rs (.executeQuery st)]
        (loop [acc []]
          (if (.next rs)
            (let [kind (.getString rs "type")
                  definition (strip-sql-comments (.getString rs "sql"))
                  dependent? (and (re-find column-re definition)
                                  (case kind
                                    "table"
                                    (and (re-find #"(?i)^CREATE\s+VIRTUAL\s+TABLE" definition)
                                         (re-find content-re definition))

                                    "index"
                                    (= table (.getString rs "tbl_name"))

                                    ("trigger" "view")
                                    (re-find table-re definition)

                                    false))]

              (recur (cond-> acc
                       dependent?
                       (conj {:kind kind :name (.getString rs "name")}))))
            acc))))))

(defn- execute-ddl!
  [^java.sql.Connection conn ^String sql]
  (with-open [st (.createStatement conn)]
    (.executeUpdate st sql)))

(def ^:private derived-table-fills
  "Statements that fill derived tables from their source rows."
  {"session_turn_stats"
   "INSERT INTO session_turn_stats (session_soul_id, turn_count, answer_count, last_status, latest_turn_at)
    WITH sessions AS MATERIALIZED (SELECT id, session_soul_id FROM session_state), t AS (
      SELECT ss.session_soul_id AS sid, ts.request_kind AS kind, ts.created_at AS created_at,
             ts.position AS position, ts.id AS turn_id, f.id AS id, f.status AS status, MAX(f.version) AS version
      FROM session_turn_soul ts JOIN sessions ss ON ss.id = ts.session_state_id
      LEFT JOIN session_turn_state f ON f.session_turn_soul_id = ts.id GROUP BY ts.id
    )
    SELECT sid, COUNT(*) AS n,
           SUM(CASE WHEN kind = 'user' AND status IN ('done','error','interrupted') THEN 1 ELSE 0 END) AS answers,
           json_extract(json_group_array(status ORDER BY created_at DESC, position DESC, turn_id)
             FILTER (WHERE kind = 'user' AND status IS NOT NULL), '$[0]') AS last_status,
           MAX(CASE WHEN f.status <> 'running' THEN
             (SELECT MAX(i.created_at) FROM session_turn_iteration i WHERE i.session_turn_state_id = f.id)
           END) AS latest
    FROM t f GROUP BY sid"})

(defn- install-missing-canonical-tables!
  "Install newly added V1 tables and their indexes from the canonical DDL, atomically.
   Existing tables are left to the additive-column pass; no rows or objects are replaced."
  [^DataSource ds locations]
  (let [objects (canonical-objects (migration-sql-texts locations))]
    (with-open [conn (.getConnection ds)]
      (let [missing (into #{}
                          (comp (filter #(= "table" (:kind %)))
                                (remove #(contains? derived-table-fills (:name %)))
                                (filter #(empty? (existing-columns conn (:name %))))
                                (map :name))
                          objects)
            additions (filter (fn [{:keys [kind name sql]}]
                                (or (and (= "table" kind) (contains? missing name))
                                    (and (= "index" kind)
                                         (contains? missing
                                                    (second (re-find
                                                              #"(?i)\bon\s+[\"`\[]?([A-Za-z0-9_]+)"
                                                              sql))))))
                              objects)]

        (when (seq additions)
          (.setAutoCommit conn false)
          (try (doseq [{:keys [sql]} additions]
                 (execute-ddl! conn sql))
               (.commit conn)
               (catch Throwable error (.rollback conn) (throw error))
               (finally (.setAutoCommit conn true))))))))

(defn- schema-sql-key
  [^String sql]
  (mapv (fn [^String token]
          (if (= \' (.charAt token 0)) token (str/lower-case token)))
        (re-seq #"'(?:''|[^'])*'|[\w]+|[^\s]" sql)))

(defn- derived-schema-match?
  [^java.sql.Connection conn objects]
  (with-open [st (.prepareStatement conn
                                    "SELECT sql FROM sqlite_master WHERE type = ? AND name = ?")]
    (every? (fn [{:keys [kind name sql]}]
              (.setString st 1 kind)
              (.setString st 2 name)
              (with-open [rs (.executeQuery st)]
                (and (.next rs)
                     (or (= "table" kind)
                         (= (schema-sql-key sql) (schema-sql-key (.getString rs "sql")))))))
            objects)))

(defn- install-derived-tables!
  "Install or repair each derived table, its triggers, and its data in one transaction.
   An older Vis can drop a trigger during column repair. Restore it and fill the table
   again so writes made without the trigger are included. A current store needs no write."
  [^DataSource ds locations]
  (let [objects (canonical-objects (migration-sql-texts locations))]
    (with-open [conn (.getConnection ds)]
      (doseq [[^String table fill] derived-table-fills
              :let [ddl (some #(when (and (= "table" (:kind %)) (= table (:name %))) %) objects)
                    trigger-prefix (str "trg_" table "_")
                    triggers (filterv #(and (= "trigger" (:kind %))
                                            (str/starts-with? (:name %) trigger-prefix))
                               objects)
                    schema (into [ddl] triggers)]
              :when (and ddl (not (derived-schema-match? conn schema)))]

        (with-open [transaction (.createStatement conn)]
          (.executeUpdate transaction "BEGIN IMMEDIATE")
          (try (when-not (derived-schema-match? conn schema)
                 (when (empty? (existing-columns conn table)) (execute-ddl! conn (:sql ddl)))
                 (doseq [{:keys [name]} triggers]
                   (execute-ddl! conn (str "DROP TRIGGER IF EXISTS " name)))
                 (execute-ddl! conn (str "DELETE FROM " table))
                 (execute-ddl! conn fill)
                 (doseq [{:keys [sql]} triggers]
                   (execute-ddl! conn sql)))
               (.executeUpdate transaction "COMMIT")
               (catch Throwable error (.executeUpdate transaction "ROLLBACK") (throw error))))))))

(defn- existing-index-names
  "Lower-cased names of every index a store already holds."
  [^java.sql.Connection conn]
  (with-open [st
              (.createStatement conn)

              rs
              (.executeQuery st "SELECT name FROM sqlite_master WHERE type = 'index'")]

    (loop [acc #{}]
      (if (.next rs) (recur (conj acc (str/lower-case (.getString rs "name")))) acc))))

(defn- install-missing-canonical-indexes!
  "Create the canonical indexes a store does not carry yet.

   `install-missing-canonical-tables!` brings the indexes of a table it installed
   itself, and nothing else installs one: an index ADDED to a table that already
   exists would never appear, and the store would keep answering the query that
   index was written for with a full scan. Runs after the column passes, because
   a new index may name a newly added column. Per index and best effort - one
   SQLite refuses leaves every other one installed."
  [^DataSource ds locations]
  (let [objects (canonical-objects (migration-sql-texts locations))]
    (with-open [conn (.getConnection ds)]
      (let [held (existing-index-names conn)]
        (doseq [{:keys [kind name sql]} objects
                :when (and (= "index" kind) (not (contains? held (str/lower-case name))))]

          (try (execute-ddl! conn sql) (catch Throwable _ nil)))))))

(defn- restoring-dependents!
  "Run `alter!` between DROPPING every schema object whose DDL names `column`
   and recreating the ones the canonical SQL still defines, each recreated FTS5
   index `rebuild`-ed so it holds the surviving columns.

   The drop is what makes the alteration legal at all — SQLite refuses to touch
   a column a trigger reads — and the recreate is what keeps search working
   afterwards. Both column repairs need exactly this, so neither owns it."
  [^java.sql.Connection conn objects ^String table ^String column alter!]
  (let [deps
        (dependent-objects conn table column)

        dep-names
        (into #{} (map (comp str/lower-case :name)) deps)

        recreate
        (filterv #(contains? dep-names (str/lower-case ^String (:name %))) objects)]

    (doseq [{:keys [kind name]} deps]
      (execute-ddl! conn (str "DROP " (str/upper-case kind) " IF EXISTS " name)))
    (alter!)
    (doseq [{:keys [sql]} recreate]
      (execute-ddl! conn sql))
    (doseq [{:keys [kind name sql]}
            recreate

            :when (and (= "table" kind) (re-find #"(?i)using\s+fts5" ^String sql))]

      (execute-ddl! conn (str "INSERT INTO " name "(" name ") VALUES('rebuild')")))))

(defn- retire-column!
  "Drop `column` from `table` and restore what had to go with it. One
   transaction — a store ends up either fully retired or exactly as it started,
   never with its search triggers missing."
  [^java.sql.Connection conn objects ^String table ^String column]
  (restoring-dependents! conn
                         objects
                         table
                         column
                         #(execute-ddl! conn (str "ALTER TABLE " table " DROP COLUMN " column))))

(defn- drop-retired-columns!
  "Drop every `retired-columns` entry a store still carries. Best effort and
   once per store: a column SQLite refuses to drop is rolled back to dead but
   harmless rather than making the database unopenable."
  [^DataSource ds locations]
  (let [objects (canonical-objects (migration-sql-texts locations))]
    (with-open [conn (.getConnection ds)]
      (doseq [[^String table ^String column] retired-columns
              :when (contains? (existing-columns conn table) (str/lower-case column))]

        (let [auto (.getAutoCommit conn)]
          (try (.setAutoCommit conn false)
               (retire-column! conn objects table column)
               (.commit conn)
               (catch Throwable _ (try (.rollback conn) (catch Throwable _ nil)))
               (finally (try (.setAutoCommit conn auto) (catch Throwable _ nil)))))))))

(defn- delete-retired-rows!
  "Delete every `retired-rows` entry a store still carries.

   Runs BEFORE the columns are dropped, because the predicate is written in terms
   of a column retirement is about to remove. Best effort: a store that already
   dropped it has nothing left to identify."
  [^DataSource ds]
  (with-open [conn (.getConnection ds)]
    (doseq [[^String table ^String column ^String predicate] retired-rows
            :when (contains? (existing-columns conn table) (str/lower-case column))]

      (try (with-open [st (.createStatement conn)]
             (.executeUpdate st (str "DELETE FROM " table " WHERE " predicate)))
           (catch Throwable _ nil)))))

(defn- copy-retired-values!
  "Copy every `retired-copies` entry a store still carries.

   Runs BEFORE the columns are dropped. Best effort: a store that already dropped
   the column has nothing left to copy."
  [^DataSource ds]
  (with-open [conn (.getConnection ds)]
    (doseq [[^String table ^String column ^String target ^String predicate] retired-copies
            :when (contains? (existing-columns conn table) (str/lower-case column))]

      (try (with-open [st (.createStatement conn)]
             (.executeUpdate st
                             (str "UPDATE " table " SET " target " = " column " WHERE " predicate)))
           (catch Throwable _ nil)))))

(defn- rewrite-retired-values!
  "Rewrite every `retired-values` entry a store still holds. Best effort: a store
   without the column has nothing to rewrite."
  [^DataSource ds]
  (with-open [conn (.getConnection ds)]
    (doseq [[^String table ^String column ^String old ^String new] retired-values
            :when (contains? (existing-columns conn table) (str/lower-case column))]

      (try (with-open [st (.prepareStatement
                            conn
                            (str "UPDATE " table " SET " column " = ? WHERE " column " = ?"))]
             (.setString st 1 new)
             (.setString st 2 old)
             (.executeUpdate st))
           (catch Throwable _ nil)))))

(defn- column-names
  "The column names of `table` in their stored order, generated and hidden columns too."
  [^java.sql.Connection conn ^String table]
  (with-open [st
              (.createStatement conn)

              rs
              (.executeQuery st (str "PRAGMA table_xinfo(" table ")"))]

    (loop [acc []]
      (if (.next rs) (recur (conj acc (.getString rs "name"))) acc))))

(defn- hidden-columns?
  "True when `table` has a generated or hidden column: an INSERT cannot copy it."
  [^java.sql.Connection conn ^String table]
  (with-open [st (.prepareStatement conn "SELECT 1 FROM pragma_table_xinfo(?) WHERE hidden <> 0")]
    (.setString st 1 table)
    (with-open [rs (.executeQuery st)] (.next rs))))

(defn- table-objects
  "The store's own SQL of each index and trigger of `table`. Automatic indexes have none."
  [^java.sql.Connection conn ^String table]
  (with-open [st (.prepareStatement
                   conn
                   (str "SELECT sql FROM sqlite_master"
                        " WHERE type IN ('index', 'trigger') AND tbl_name = ? AND sql IS NOT NULL"))]
    (.setString st 1 table)
    (with-open [rs (.executeQuery st)]
      (loop [acc []]
        (if (.next rs) (recur (conj acc (.getString rs 1))) acc)))))

(defn- pragma-long
  ^long [^java.sql.Connection conn ^String pragma]
  (with-open [st
              (.createStatement conn)

              rs
              (.executeQuery st (str "PRAGMA " pragma))]

    (if (.next rs) (.getLong rs 1) 0)))

(defn- index-shape
  "Each index of `table` as `[unique origin partial columns]`, without its name, sorted.
   Automatic indexes of UNIQUE and PRIMARY KEY constraints are in it too."
  [^java.sql.Connection conn ^String table]
  (with-open [st (.prepareStatement
                   conn
                   (str "SELECT il.\"unique\", il.origin, il.partial,"
                        " (SELECT group_concat(ii.name, ',') FROM pragma_index_info(il.name) ii)"
                        " FROM pragma_index_list(?) il"))]
    (.setString st 1 table)
    (with-open [rs (.executeQuery st)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc [(.getLong rs 1) (.getString rs 2) (.getLong rs 3) (.getString rs 4)]))
          (sort-by pr-str acc))))))

(defn- reorder-space?
  "Check space for a table and its indexes without reading their pages. Use the size
   of the whole database as an upper estimate. A memory database has no disk file."
  [^java.sql.Connection conn]
  (with-open [st
              (.createStatement conn)

              rs
              (.executeQuery st "PRAGMA database_list")]

    (loop []

      (if (.next rs)
        (if (= "main" (.getString rs "name"))
          (let [path (.getString rs "file")]
            (or (str/blank? path)
                (>= (.getUsableSpace (.getParentFile (File. path)))
                    (* (pragma-long conn "main.page_count") (pragma-long conn "main.page_size")))))
          (recur))
        false))))

(defn reorder-table!
  "Give `table` the column order of the canonical SQL. Answer true when the table was
   built again, false when no change is needed or possible, and `:low-disk` when
   there is not enough space for the copy.

   SQLite reads a column that comes after a large BLOB only through all the overflow
   pages of that BLOB, so the canonical SQL puts a large BLOB last. A store that got
   its columns in an older order keeps that order, because SQLite cannot move a
   column: only a new table can change it.

   The new table gets each row, each index and each trigger of the old one. A column
   that the canonical SQL does not name keeps its stored definition and comes last.
   One transaction, so a stopped rebuild leaves the table as it was. A table that the
   copy cannot keep exactly stays as it is: a column without a definition that the
   parser reads, a generated column, or a constraint that only the old table has. Foreign-key
   checks are off on this connection during the copy: the rows are those that the
   store holds already, and a check would refuse an old row whose parent is gone."
  [^DataSource ds locations ^String table]
  (let [texts
        (migration-sql-texts (if (string? locations) [locations] locations))

        canonical
        (mapv (comp str/lower-case :name)
              (get (reduce merge {} (map canonical-columns texts)) table))

        ddl
        (some #(when (and (= "table" (:kind %)) (= table (:name %))) (:sql %))
              (canonical-objects texts))]

    (when-not (and ddl (seq canonical))
      (throw (ex-info (str "The canonical SQL has no table " table)
                      {:type :persistance/unknown-table :table table})))
    (with-open [conn (.getConnection ds)]
      (let [fk (pragma-long conn "foreign_keys")]
        (execute-ddl! conn "PRAGMA foreign_keys = OFF")
        (try (execute-ddl! conn "BEGIN IMMEDIATE")
             ;; Read the table under the write lock: a column or an index that another
             ;; connection adds before it must come into the new table too.
             (try (let [names
                        (column-names conn table)

                        stored
                        (mapv str/lower-case names)

                        known
                        (set canonical)

                        extras
                        (remove #(contains? known (str/lower-case ^String (:name %)))
                                (stored-columns conn table))

                        rebuild?
                        (and (not= (filterv (set stored) canonical) (filterv known stored))
                             (not (hidden-columns? conn table))
                             ;; Each column must have a definition to copy.
                             (= (set stored)
                                (into (set (filter known stored))
                                      (map (comp str/lower-case :name))
                                      extras)))

                        deferred?
                        (and rebuild? (not (reorder-space? conn)))

                        shape
                        (when (and rebuild? (not deferred?)) (index-shape conn table))]

                    (when (and rebuild? (not deferred?))
                      (let [aside
                            (str table "_vis_reorder")

                            objects
                            (table-objects conn table)

                            columns
                            (str/join ", " (map #(str \" (str/replace % "\"" "\"\"") \") names))]

                        (execute-ddl! conn (str "DROP TABLE IF EXISTS " aside))
                        (execute-ddl! conn
                                      (str/replace-first
                                        ddl
                                        (re-pattern (str "(?i)(create\\s+table\\s+(?:if\\s+not\\s+exists\\s+)?)[\"`\\[]?"
                                                       table
                                                       "[\"`\\]]?"))
                                        (str "$1" aside)))
                        (doseq [{:keys [sql]} extras]
                          (execute-ddl! conn (str "ALTER TABLE " aside " ADD COLUMN " sql)))
                        (execute-ddl! conn
                                      (str "INSERT INTO " aside " (" columns ") SELECT " columns " FROM " table))
                        (execute-ddl! conn (str "DROP TABLE " table))
                        (execute-ddl! conn (str "ALTER TABLE " aside " RENAME TO " table))
                        (doseq [sql objects]
                          (execute-ddl! conn sql))))
                    (cond deferred?
                          (do (execute-ddl! conn "ROLLBACK") :low-disk)

                          (or (not rebuild?)
                              (and (= (frequencies stored)
                                      (frequencies (map str/lower-case (column-names conn table))))
                                   (= shape (index-shape conn table))))
                          (do (execute-ddl! conn "COMMIT") rebuild?)

                          ;; A constraint that the copy did not keep: keep the old table.
                          :else (do (execute-ddl! conn "ROLLBACK") false)))
                  (catch Throwable error (execute-ddl! conn "ROLLBACK") (throw error)))
             (finally (execute-ddl! conn (str "PRAGMA foreign_keys = " fk))))))))

(defn drop-column!
  "Drop `column` from `table` in one transaction, and build again each schema object
   of the canonical SQL that names it. Answer true when the column was dropped, false
   when the table has no such column, and `:low-disk` when there is no space for the
   copy of the table that SQLite writes."
  [^DataSource ds locations ^String table ^String column]
  (let [objects (canonical-objects (migration-sql-texts (if (string? locations) [locations] locations)))]
    (with-open [conn (.getConnection ds)]
      (cond (not (contains? (existing-columns conn table) (str/lower-case column))) false
            (not (reorder-space? conn)) :low-disk
            :else (let [auto (.getAutoCommit conn)]
                    (try (.setAutoCommit conn false)
                         (retire-column! conn objects table column)
                         (.commit conn)
                         true
                         (catch Throwable e
                           (try (.rollback conn) (catch Throwable _ nil))
                           (throw e))
                         (finally (.setAutoCommit conn auto))))))))

(defn- drifted-columns
  "Columns whose definition in the STORE no longer reads like the canonical
   file's, as the canonical definition plus its `:table`.

   The additive pass only ever sees a column that is MISSING. A column whose
   text CHANGED is present, so nothing realigned it: when
   `prompt_cache_continuity` gained a `cache-context-changed` case in its CHECK,
   every store created before that edit kept the narrower constraint and the
   first write of the new value died with SQLITE_CONSTRAINT_CHECK, taking the
   turn with it. Both sides go through the same parse and compare
   case-insensitively, so formatting alone never provokes a rewrite."
  [^java.sql.Connection conn tables]
  (vec
    (for [[table cols]
          tables

          :let [have
                (into {}
                      (map (juxt (comp str/lower-case :name) (comp str/lower-case :sql)))
                      (stored-columns conn table))]
          :when (seq have)
          col
          cols

          :let [stored
                (get have (str/lower-case ^String (:name col)))]
          :when (and stored (not= stored (str/lower-case ^String (:sql col))) (addable-column? col))]

      (assoc col :table table))))

(defn- realign-column!
  "Give `table` the canonical definition of `col` while keeping the values it
   holds: the old column is renamed aside, the canonical one added, every value
   copied across and the old one dropped. SQLite has no ALTER COLUMN, and a whole
   table rebuild would have to re-derive constraints the file already spells out.

   A value the new definition REFUSES rolls the transaction back and leaves the
   store as it was: dropping data to satisfy a narrowed constraint is a decision
   for a retirement entry, not for a repair that runs on every open."
  [^java.sql.Connection conn objects ^String table {:keys [^String name ^String sql]}]
  (let [aside (str name "_vis_superseded")]
    (restoring-dependents!
      conn
      objects
      table
      name
      (fn []
        (execute-ddl! conn (str "ALTER TABLE " table " RENAME COLUMN " name " TO " aside))
        (execute-ddl! conn (str "ALTER TABLE " table " ADD COLUMN " sql))
        (execute-ddl! conn (str "UPDATE " table " SET " name " = " aside))
        (execute-ddl! conn (str "ALTER TABLE " table " DROP COLUMN " aside))))))

(defn- realign-drifted-columns!
  "Rewrite every column a store carries with an older canonical V1's definition.
   Best effort and per column: one repair SQLite refuses rolls back alone, so the
   rest of the store still realigns and the database stays openable."
  [^DataSource ds locations]
  (let [texts
        (migration-sql-texts locations)

        tables
        (reduce merge {} (map canonical-columns texts))

        objects
        (canonical-objects texts)]

    (when (seq tables)
      (with-open [conn (.getConnection ds)]
        (doseq [col (drifted-columns conn tables)]
          (let [auto (.getAutoCommit conn)]
            (try (.setAutoCommit conn false)
                 (realign-column! conn objects (:table col) col)
                 (.commit conn)
                 (catch Throwable _ (try (.rollback conn) (catch Throwable _ nil)))
                 (finally (try (.setAutoCommit conn auto) (catch Throwable _ nil))))))))))

(defn migrate!
  "Install the single canonical V1 schema.

   Applied checksum drift and history left by migrations later consolidated
   into V1 self-heal through Flyway `repair`, then migration is retried. Repair
   changes only `flyway_schema_history`; persisted Vis rows and schema objects
   are preserved. Databases created by an older V1 are then topped up additively
   by `reconcile-canonical-columns!`, given every index the canonical file has
   gained since (`install-missing-canonical-indexes!`), realigned by
   `realign-drifted-columns!` wherever a column's own definition changed, and
   stripped of `retired-columns`, so one canonical migration keeps serving stores
   that already exist.

   A store that a newer Vis migrated is refused with `newer-store-error`, and
   nothing in it changes. With `:shared? true`, another process has the store open,
   maybe an older Vis that still uses the retired columns and values. The retired
   values, rows, copies and columns then stay, and so does the recorded generation: a
   later open that has the store alone removes them (#455)."
  ([ds locations] (migrate! ds locations {}))
  ([^DataSource ds locations {:keys [shared?]}]
   (let [locs
         (cond (string? locations) [locations]
               (sequential? locations) (vec locations)
               :else (throw (ex-info "locations must be a string or coll of strings"
                                     {:type :persistance/invalid-migration-locations
                                      :got (type locations)})))

         rp
         (index-resource-provider locs)

         ^org.flywaydb.core.api.configuration.FluentConfiguration cfg
         (cond-> (-> (org.flywaydb.core.Flyway/configure)
                     (.dataSource ds)
                     (.locations ^"[Ljava.lang.String;" (into-array String locs))
                     (.baselineOnMigrate true)
                     (.baselineVersion "0")
                     (.mixed true))
           rp
           (-> (.resourceProvider rp)
               (.javaMigrationClassProvider (reify
                                              ClassProvider
                                                (getClasses [_] [])))))]

     (when-let [newer (newer-store-error ds nil)]
       (throw newer))
     (let [^org.flywaydb.core.Flyway flyway (.load cfg)]
       (try (.migrate flyway)
            (catch Throwable e
              (if (repairable-validation-error? e)
                (do (.repair flyway) (.migrate flyway))
                (throw e)))))
     (install-missing-canonical-tables! ds locs)
     (reconcile-canonical-columns! ds locs)
     (when-not shared? (rewrite-retired-values! ds))
     (realign-drifted-columns! ds locs)
     (install-missing-canonical-indexes! ds locs)
     (when-not shared?
       (delete-retired-rows! ds)
       (copy-retired-values! ds)
       (drop-retired-columns! ds locs)
       (record-generation! ds))
     (install-derived-tables! ds locs)
     ds)))
