(ns com.blockether.vis.ext.channel-tui.tabs
  "Per-place tab-set persistence, shared by EVERY TUI running there.

   The set of open tabs is the thing you want back when you return to a
   place: launch vis in a directory, restore the tabs you had there. The
   sessions themselves already persist in the DB (keyed by session id); this
   only records, per launch directory, WHICH sessions are open as tabs, their
   order, and which one was active.

   Stored as a small EDN sidecar under `~/.vis/tabs/<place>.edn` — no schema,
   no migration, entirely owned by this channel. Shape:

     {:active  \"<session-id>\"    ; the focused tab (or nil)
      :sessions [{:id \"<session-id>\" :root \"/abs/project-root\"} …]
                                   ; MERGED tab set across every live TUI in
                                   ; this place, left to right; :root is the
                                   ; tab's PROJECT root (absent when unknown)
      :clients {\"<pid>\" {:active … :sessions […] :at <ms>} …}}
                                   ; one entry per TUI process that saved here

   MULTI-TUI SYNC: several TUIs can run in the same place at once, and each
   used to clobber the sidecar with only its own tabs (last writer wins).
   `save!` now MERGES instead of overwriting (see `merge-clients`): this
   process replaces only ITS `:clients` entry, keeps every sibling whose pid
   is still alive, and prunes dead pids — so a crashed/exited TUI stops
   pinning tabs while a live sibling's tabs survive our save. The top-level
   `:active`/`:sessions` pair is the merged, legacy-compatible view the
   restore path reads. Merging is read-modify-write without a file lock; a
   racing save can momentarily drop a sibling's entry, which that sibling's
   next save restores — best effort by design, like the IO handling below.

   Legacy snapshots stored plain id strings in `:sessions` (and carried no
   `:clients`); readers go through `snapshot-session-ids`, which tolerates
   both shapes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.lang ProcessHandle]))

(defn- place-key
  "Stable filename for the current launch directory (`user.dir`): sanitized
   basename + an unsigned hash of the canonical path, so two same-named dirs
   in different locations never share a sidecar."
  []
  (let [dir
        (.getCanonicalPath (io/file (System/getProperty "user.dir")))

        base
        (-> (.getName (io/file dir))
            (str/replace #"[^A-Za-z0-9._-]+" "_"))

        base
        (if (str/blank? base) "root" base)

        h
        (Long/toUnsignedString (Integer/toUnsignedLong (hash dir)) 36)]

    (str base "-" h)))

(defn- tabs-file
  ^java.io.File []
  (io/file (System/getProperty "user.home") ".vis" "tabs" (str (place-key) ".edn")))

(defn- entry-id
  "Session id of one `:sessions` entry, tolerating BOTH shapes: the current
   `{:id … :root …}` maps and legacy plain id strings. Nil when malformed."
  [e]
  (cond (map? e) (some-> (:id e)
                         str
                         not-empty)
        (string? e) (not-empty e)
        :else nil))

(defn snapshot-session-ids
  "Ordered session ids from a snapshot's `:sessions`, tolerating BOTH entry
   shapes: the current `{:id … :root …}` maps and legacy plain id strings."
  [snapshot]
  (vec (keep entry-id (:sessions snapshot))))

(defn- client-id
  "This TUI process's identity in the sidecar's `:clients` — its pid."
  []
  (str (.pid (ProcessHandle/current))))

(defn- pid-alive?
  "True when `pid-str` names a process still running on this machine."
  [pid-str]
  (try (let [^java.util.Optional oph
             (ProcessHandle/of (Long/parseLong (str pid-str)))

             ^ProcessHandle ph
             (.orElse oph nil)]

         (boolean (some-> ph
                          .isAlive)))
       (catch Throwable _ false)))

(defn merge-clients
  "Merge THIS process's tab `snapshot` ({:active sid :sessions […]}) into the
   sidecar `disk` may already hold, as client `client`. Pure — liveness is
   injected via `alive?` (client-id → boolean) so multi-TUI scenarios are
   unit-testable without real processes.

   - our `client` entry is replaced wholesale (tabs closed HERE disappear),
   - sibling clients are kept only while `alive?` holds (a crashed or exited
     TUI stops pinning tabs),
   - the merged top-level `:sessions` lists OUR tabs first (order and
     `:active` win), then each surviving sibling's tabs (oldest save first),
     deduped by session id,
   - a legacy sidecar without `:clients` contributes nothing — exactly the
     pre-sync overwrite semantics (a dead process's tabs self-heal away)."
  [disk client snapshot now-ms alive?]
  (let [client
        (str client)

        siblings
        (into {}
              (filter (fn [[cid m]]
                        (and (map? m) (not= (str cid) client) (alive? cid))))
              (:clients disk))

        own
        (vec (:sessions snapshot))

        foreign
        (->> siblings
             (sort-by (fn [[_ m]]
                        (:at m 0)))
             (mapcat (fn [[_ m]]
                       (:sessions m)))
             (reduce (fn [[seen acc] e]
                       (let [id (entry-id e)]
                         (if (or (nil? id) (contains? seen id))
                           [seen acc]
                           [(conj seen id) (conj acc e)])))
                     [(set (keep entry-id own)) []])
             second)]

    {:active (:active snapshot)
     :sessions (into own foreign)
     :clients (assoc siblings client (assoc snapshot :at now-ms))}))

(defn read-snapshot
  "Read this place's persisted tab snapshot, or nil when absent/unreadable.
   Returns only the well-formed shape: a map with a vector `:sessions` (the
   merged multi-TUI view; per-client entries ride under `:clients`)."
  ([] (read-snapshot (tabs-file)))
  ([^java.io.File f]
   (try (when (.exists f)
          (let [m (edn/read-string (slurp f))]
            (when (and (map? m) (vector? (:sessions m))) m)))
        (catch Throwable _ nil))))

(defn save!
  "Persist `snapshot` ({:active sid :sessions [{:id sid :root root} …]}) for
   this place, MERGED with every live sibling TUI's entry instead of
   overwriting it (see `merge-clients`). Best effort — IO failures are
   swallowed so a read-only home never breaks the TUI."
  ([snapshot] (save! snapshot (tabs-file)))
  ([snapshot ^java.io.File f]
   (try (let [merged (merge-clients (read-snapshot f)
                                    (client-id)
                                    snapshot
                                    (System/currentTimeMillis)
                                    pid-alive?)]
          (io/make-parents f)
          (spit f (pr-str merged)))
        (catch Throwable _ nil))))
