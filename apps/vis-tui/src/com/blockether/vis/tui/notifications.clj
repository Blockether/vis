(ns com.blockether.vis.tui.notifications
  "Ephemeral in-app notifications.

   A single in-memory pub-sub every surface in this app can use to raise a
   transient signal - \"copied to clipboard\", \"tests passed\", \"provider
   switched\" - without embedding it in the answer body or polluting the log.

   Surface:

     (notify! text)
     (notify! text :level :info|:success|:warn|:error
                   :ttl-ms <long>|nil :title <string> :id <string> :source <string>)
     (notifications)              ;; vec of currently-active entries
     (dismiss! id)                ;; force-clear by id
     (clear-expired!)             ;; prune; called on every read
     (watch! key (fn [vec] ...))    ;; cross-channel reactivity
     (unwatch! key)

   Entry shape:
     {:id          <uuid>
      :text        <string>
      :level       :info | :success | :warn | :error
      :title       <string> | nil     ;; one bold toast line
      :source      <string> | nil     ;; the extension that sent it
      :created-at  <inst>
      :until       <epoch-ms> | nil   ;; nil = sticky / manual dismiss

   A push with the `:id` of an active entry replaces that entry. The store keeps
   at most `MAX_ENTRIES`, and it wakes the watchers when an entry expires, so a
   toast leaves the screen on time also while the user is idle.

   Levels are advisory metadata for channels: TUI uses them for
   colour, the CLI could prefix `[notice]` /
   `[warn]`. The host stores them but never interprets them.

   Why a flat module instead of a generic event bus: notifications
   are a single, narrow concern. A 50-line atom + watcher map serves
   it without introducing a generic pub-sub abstraction nobody asked
   for. If we ever grow more event types, this becomes one consumer
   of a richer system."
  (:require [taoensso.telemere :as tel]
            [com.blockether.vis.tui.util :as util]))

;; Defaults

(def ^:private DEFAULT_TTL_MS
  "Lifespan by level when the caller passes no `:ttl-ms`. A warning or an
   error stays longer, because the user must read and act on it."
  {:info 4000 :success 4000 :warn 8000 :error 8000})

(def ^:private MAX_ENTRIES
  "The most entries the store keeps. The oldest goes first; a toast stack
   never shows more than a few."
  5)

(def ^:private VALID_LEVELS #{:info :success :warn :error})

;; State

(defonce ^:private notifications-atom
  ;; Vec ordered by insertion (newest last). Vec instead of map so
  ;; channels iterating to render the most-recent-first don't have
  ;; to re-sort on every paint.
  (atom []))

(defonce ^:private watchers-atom
  ;; {key (fn [vec] ...)}. Watchers fire AFTER every successful state
  ;; mutation (push / dismiss / clear-expired) with the new vec.
  ;; Errors inside watchers are swallowed + logged so a misbehaving
  ;; channel can never poison the pub-sub for everyone else.
  (atom {}))

;; Internal helpers

(defn- expired?
  "True when `entry` has an `:until` deadline that has passed.
   Nil `:until` means sticky - never expires automatically."
  [entry]
  (when-let [until (:until entry)]
    (< (long until) (util/now-ms))))

(defn- prune "Drop every expired entry from `coll`." [coll] (filterv (complement expired?) coll))

(defn- fire-watchers!
  "Call every registered watcher with the new vec. Errors are
   swallowed and logged, never propagated."
  [snapshot]
  (doseq [[k f] @watchers-atom]
    (try (f snapshot)
         (catch Throwable t
           (tel/log! {:level :error :id ::watcher-error :data {:key k :error (ex-message t)}}
                     (str "Notifications watcher " k " threw: " (ex-message t)))))))

;; Public API

(defn clear-expired!
  "Drop every entry whose `:until` has passed. Returns the new vec.
   Called implicitly on every `notifications` read so callers never
   see stale entries."
  []
  (let [pruned (swap! notifications-atom prune)]
    pruned))

(defonce ^:private expiry-executor
  (delay (java.util.concurrent.Executors/newSingleThreadScheduledExecutor
           (reify
             java.util.concurrent.ThreadFactory
               (newThread [_ runnable]
                 (doto (Thread. ^Runnable runnable "vis-tui-notification-expiry")
                   (.setDaemon true)))))))

(defn- expire!
  "Drop expired entries and fire the watchers when one went away."
  []
  (let [[before after] (swap-vals! notifications-atom prune)]
    (when (not= (count before) (count after)) (fire-watchers! after))))

(defn- schedule-expiry!
  "Run `expire!` just after `until`, so the screen drops the entry on time."
  [until]
  (let [delay-ms (max 0 (+ 20 (- (long until) (util/now-ms))))]
    (.schedule ^java.util.concurrent.ScheduledExecutorService @expiry-executor
               ^Runnable expire!
               (long delay-ms)
               java.util.concurrent.TimeUnit/MILLISECONDS)))

(defn- push-entry
  "`coll` with `entry` as its newest entry. An entry with the same id goes away first.
   Expired entries and the oldest beyond `MAX_ENTRIES` go away too."
  [coll entry]
  (let [kept
        (filterv #(not= (:id entry) (:id %)) (prune coll))

        added
        (conj kept entry)]

    (subvec added (max 0 (- (count added) MAX_ENTRIES)))))

(defn notifications
  "Vec of currently-active notifications, oldest first. Implicitly
   prunes expired entries before returning so a paint loop reading
   this never has to re-check `:until` deadlines."
  []
  (clear-expired!))

(defn notify!
  "Push a new notification. Returns the entry's id (uuid string) so
   the caller can later `dismiss!` it manually.

   Options:
     :level   one of #{:info :success :warn :error} - default :info.
              Channels use the level for visual treatment (color,
              emoji prefix, etc.).
     :ttl-ms  lifespan in ms. Default by level, see `DEFAULT_TTL_MS`.
              nil = sticky / no auto expiry; the notification stays
              until `dismiss!`d.
     :title   one bold line above the text.
     :id      the entry identity. A push with an active id replaces it.
     :source  the extension that sent the notification.

   The notification is appended to the in-memory vec and every
   registered watcher is fired with the new full vec."
  [text & {:keys [level title source] :or {level :info} :as opts}]
  (when-not (string? text)
    (throw (ex-info "notify! text must be a String" {:type :vis/notify-bad-text :text text})))
  (when-not (VALID_LEVELS level)
    (throw (ex-info (str "notify! level must be one of " VALID_LEVELS)
                    {:type :vis/notify-bad-level :level level})))
  (let [id
        (or (some-> (:id opts)
                    str
                    not-empty)
            (str (java.util.UUID/randomUUID)))

        now
        (util/now-ms)

        ttl-ms
        (if (contains? opts :ttl-ms) (:ttl-ms opts) (DEFAULT_TTL_MS level))

        until
        (when ttl-ms (+ now (long ttl-ms)))

        entry
        (cond-> {:id id :text text :level level :created-at now :until until}
          (not-empty title)
          (assoc :title title)

          (not-empty source)
          (assoc :source source))

        snapshot
        (swap! notifications-atom push-entry entry)]

    (when until (schedule-expiry! until))
    (fire-watchers! snapshot)
    id))

(defn dismiss!
  "Drop the entry with the given id, regardless of its `:until`
   deadline. Returns true when an entry was actually removed.
   Watchers fire when the value changed."
  [id]
  (let [removed?
        (volatile! false)

        next-vec
        (swap! notifications-atom (fn [coll]
                                    (let [filtered (filterv #(not= id (:id %)) coll)]
                                      (vreset! removed? (not= (count coll) (count filtered)))
                                      filtered)))]

    (when @removed? (fire-watchers! next-vec))
    @removed?))

(defn dismiss-all!
  "Drop every notification. Returns the new (empty) vec."
  []
  (let [next-vec (swap! notifications-atom (fn [_]
                                             []))]
    (fire-watchers! next-vec)
    next-vec))

;; Watcher API

(defn watch!
  "Register a watcher under `key`. The fn `f` is called with the
   full active-notifications vec on every push / dismiss / clear.
   Replacing an existing watcher under the same key is fine -
   typical pattern is `(watch! :tui-screen render-bump)`.

   Watchers run synchronously on the mutating thread, AFTER the
   atom swap. They MUST be cheap (microseconds) - anything heavier
   should bounce work onto a future."
  [key f]
  (when-not (ifn? f)
    (throw (ex-info "watch! f must be ifn" {:type :vis/notify-bad-watcher :key key})))
  (swap! watchers-atom assoc key f)
  nil)

(defn unwatch!
  "Remove the watcher registered under `key`. Returns true when one
   was actually removed."
  [key]
  (let [removed? (volatile! false)]
    (swap! watchers-atom (fn [m]
                           (vreset! removed? (contains? m key))
                           (dissoc m key)))
    @removed?))
