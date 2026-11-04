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
      :session-id  <string> | nil     ;; nil = global, else only that session shows it
      :created-at  <epoch-ms>
      :ttl-ms      <long> | nil       ;; nil = sticky / manual dismiss
      :left-ms     <long> | nil       ;; time left when the clock last stopped
      :since       <epoch-ms> | nil}  ;; the clock runs from here, while the toast shows

   The entries are in two lanes. The `:global` lane has the entries of no session. The
   `:session` lane has the entries of the session in front. The screen shows each lane as
   its own card, one entry at a time as a page (`pager`, `show!`, `page!`). Only the shown
   entry of each lane counts down, so a toast that waits behind another page, or for its
   session to come in front, keeps all its time. A push with the `:id` of an active entry replaces
   that entry. While a clock runs, the store wakes the watchers every `TICK_MS`, so the
   countdown bar moves and a toast leaves on time also while the user is idle.

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
  "The most entries the store keeps. The oldest goes first. Session entries wait
   here until their session comes in front, so the store keeps more than one page."
  20)

(def ^:private TICK_MS "How often a running clock wakes the watchers." 250)

(def ^:private VALID_LEVELS #{:info :success :warn :error})

;; State

(defonce ^:private store
  ;; :entries is ordered by insertion (newest last). :pages maps each lane to the id
  ;; of its shown entry; a lane without an id shows its newest entry. :session is the
  ;; session that the screen showed last, so a click handler pages the list that the user sees.
  (atom {:entries [] :pages {} :session nil}))

(defonce ^:private watchers-atom
  ;; {key (fn [vec] ...)}. Watchers fire AFTER every successful state
  ;; mutation (push / dismiss / page / tick) with the new entries vec.
  ;; Errors inside watchers are swallowed + logged so a misbehaving
  ;; channel can never poison the pub-sub for everyone else.
  (atom {}))

;; Pure model

(defn remaining-ms
  "The milliseconds that `entry` still shows at `now`, or nil for a sticky entry."
  [{:keys [left-ms since]} now]
  (when left-ms (max 0 (- (long left-ms) (if since (- (long now) (long since)) 0)))))

(defn- expired? [entry now] (= 0 (remaining-ms entry now)))

(defn- prune
  "`state` without the entries whose time ran out at `now`."
  [state now]
  (update state
          :entries
          (fn [entries]
            (filterv #(not (expired? % now)) entries))))

(defn- shown-to?
  "True when session `sid` shows `entry`: a global entry, or an entry of `sid`."
  [entry sid]
  (let [owner (:session-id entry)]
    (or (nil? owner) (= owner sid))))

(defn lane
  "The card of `entry`: `:global` for an entry of no session, else `:session`."
  [entry]
  (if (:session-id entry) :session :global))

(defn- visible
  "The entries of `lane` that session `sid` shows, newest first."
  [entries sid lane-key]
  (filterv #(and (= lane-key (lane %)) (shown-to? % sid)) (rseq (vec entries))))

(defn- index-of [entries id] (first (keep-indexed #(when (= id (:id %2)) %1) entries)))

(defn pager
  "The page of `lane` that session `sid` shows at `now`: `{:entry :index :total
   :remaining-ms}`, or nil. Index 0 is the newest entry. The shown entry of the lane keeps
   its page while it is visible."
  [{:keys [entries pages]} sid lane-key now]
  (let [shown
        (visible entries sid lane-key)

        index
        (or (index-of shown (get pages lane-key)) 0)]

    (when-let [entry (get shown index)]
      {:entry entry :index index :total (count shown) :remaining-ms (remaining-ms entry now)})))

(defn- run-only
  "`state` with the clocks of the entries in set `ids` running and every other clock
   stopped at `now`."
  [state ids now]
  (update state
          :entries
          (fn [entries]
            (mapv (fn [{:keys [since left-ms] :as entry}]
                    (cond (contains? ids (:id entry))
                          (if (or since (nil? left-ms)) entry (assoc entry :since now))
                          since (-> entry
                                    (assoc :left-ms (remaining-ms entry now))
                                    (dissoc :since))
                          :else entry))
                  entries))))

(defn- push-entry
  "`state` with `entry` as its newest entry. An entry with the same id goes away first.
   Expired entries and the oldest beyond `MAX_ENTRIES` go away too. A new entry that the
   shown session sees becomes the shown page of its lane."
  [state entry now]
  (let [state
        (prune state now)

        added
        (conj (filterv #(not= (:id entry) (:id %)) (:entries state)) entry)]

    (cond-> (assoc state :entries (subvec added (max 0 (- (count added) (long MAX_ENTRIES)))))
      (shown-to? entry (:session state))
      (assoc-in [:pages (lane entry)] (:id entry)))))

(defn- drop-entry
  "`state` without entry `id`. When it was the shown page, the entry at the same page shows."
  [{:keys [entries session] :as state} id now]
  (let [lane-key
        (some->> entries
                 (some #(when (= id (:id %)) %))
                 lane)

        current
        (some-> (pager state session lane-key now)
                :entry
                :id)

        index
        (index-of (visible entries session lane-key) id)

        state
        (update state
                :entries
                (fn [entries]
                  (filterv #(not= id (:id %)) entries)))]

    (if (and index (= id current))
      (let [rest (visible (:entries state) session lane-key)]
        (assoc-in state [:pages lane-key] (:id (get rest (min (long index) (dec (count rest)))))))
      state)))

;; Watchers and clock

(defn- fire-watchers!
  "Call every registered watcher with the new vec. Errors are
   swallowed and logged, never propagated."
  [snapshot]
  (doseq [[k f] @watchers-atom]
    (try (f snapshot)
         (catch Throwable t
           (tel/log! {:level :error :id ::watcher-error :data {:key k :error (ex-message t)}}
                     (str "Notifications watcher " k " threw: " (ex-message t)))))))

(defonce ^:private clock-executor
  (delay (java.util.concurrent.Executors/newSingleThreadScheduledExecutor
           (reify
             java.util.concurrent.ThreadFactory
               (newThread [_ runnable]
                 (doto (Thread. ^Runnable runnable "vis-tui-notification-clock")
                   (.setDaemon true)))))))

;; The repeating tick while a clock runs, or nil. Start and stop lock this atom,
;; so a clock that starts while the tick stops always gets a new tick.
(defonce ^:private ticker (atom nil))

(defn- running? [state] (boolean (some :since (:entries state))))

(declare tick!)

(defn- ensure-ticker!
  "Start the repeating tick when a clock runs and no tick is scheduled."
  []
  (locking ticker
    (when (and (nil? @ticker) (running? @store))
      (reset! ticker (.scheduleAtFixedRate ^java.util.concurrent.ScheduledExecutorService
                                           @clock-executor
                                           ^Runnable tick!
                                           (long TICK_MS)
                                           (long TICK_MS)
                                           java.util.concurrent.TimeUnit/MILLISECONDS)))))

(defn- tick!
  "Drop expired entries and wake the watchers. Stop the tick when no clock runs."
  []
  (try (let [after (swap! store prune (util/now-ms))]
         (locking ticker
           (when-not (running? @store)
             (some-> ^java.util.concurrent.ScheduledFuture @ticker
                     (.cancel false))
             (reset! ticker nil)))
         (fire-watchers! (:entries after)))
       (catch Throwable t
         (tel/log! {:level :error :id ::tick-failed :data {:error (ex-message t)}}))))

;; Public API

(defn clear-expired!
  "Drop every entry whose time ran out. Returns the new vec."
  []
  (:entries (swap! store prune (util/now-ms))))

(defn notifications
  "Vec of currently-active notifications, oldest first. Implicitly
   prunes expired entries before returning."
  []
  (clear-expired!))

(defn- pages
  "The page of each lane that session `sid` shows at `now`: `{:global :session}`."
  [state sid now]
  {:global (pager state sid :global now) :session (pager state sid :session now)})

(defn show!
  "The pages that session `sid` shows now, `{:global :session}`, each as `pager` gives
   it. Only the clocks of these pages run, so a toast counts down only while it is on
   screen. Each paint calls it."
  [sid]
  (let [now
        (util/now-ms)

        state
        (swap! store (fn [state]
                       (let [state (assoc (prune state now) :session sid)]
                         (run-only state
                                   (into #{} (keep (comp :id :entry)) (vals (pages state sid now)))
                                   now))))]

    (ensure-ticker!)
    (pages state sid now)))

(defn page!
  "Show the next page (`delta` 1) or the previous page (-1) of the lane of entry `id`, in
   the session that the screen shows. The pages wrap. Returns the new page index of that
   lane, or nil when the entry is gone."
  [id delta]
  (let [now
        (util/now-ms)

        lane-of
        (fn [entries]
          (some #(when (= id (:id %)) (lane %)) entries))

        state
        (swap! store (fn [{:keys [entries session] :as state}]
                       (let [lane-key
                             (lane-of entries)

                             shown
                             (visible entries session lane-key)

                             n
                             (count shown)]

                         (if (or (nil? lane-key) (< n 2))
                           state
                           (let [i (long (:index (pager state session lane-key now)))]
                             (assoc-in state
                               [:pages lane-key]
                               (:id (get shown (mod (+ i (long delta)) n)))))))))]

    (fire-watchers! (:entries state))
    (when-let [lane-key (lane-of (:entries state))]
      (:index (pager state (:session state) lane-key now)))))

(defn notify!
  "Push a new notification. Returns the entry's id (uuid string) so
   the caller can later `dismiss!` it manually.

   Options:
     :level       one of #{:info :success :warn :error} - default :info.
                  Channels use the level for visual treatment (color,
                  emoji prefix, etc.).
     :ttl-ms      lifespan in ms on screen. Default by level, see `DEFAULT_TTL_MS`.
                  nil = sticky / no auto expiry; the notification stays
                  until `dismiss!`d.
     :title       one bold line above the text.
     :id          the entry identity. A push with an active id replaces it.
     :source      the extension that sent the notification.
     :session-id  the only session that shows it. Without it, every session shows it.

   The notification is appended to the in-memory vec and every
   registered watcher is fired with the new full vec."
  [text & {:keys [level title source session-id] :or {level :info} :as opts}]
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

        entry
        (cond-> {:id id :text text :level level :created-at now :ttl-ms ttl-ms :left-ms ttl-ms}
          (not-empty title)
          (assoc :title title)

          (not-empty source)
          (assoc :source source)

          (not-empty (some-> session-id
                             str))
          (assoc :session-id (str session-id)))

        state
        (swap! store push-entry entry now)]

    (fire-watchers! (:entries state))
    id))

(defn dismiss!
  "Drop the entry with the given id, regardless of its time. Returns true when
   an entry was actually removed. Watchers fire when the value changed."
  [id]
  (let [[before after]
        (swap-vals! store drop-entry id (util/now-ms))

        removed?
        (not= (count (:entries before)) (count (:entries after)))]

    (when removed? (fire-watchers! (:entries after)))
    removed?))

(defn dismiss-all!
  "Drop every notification. Returns the new (empty) vec."
  []
  (let [state (swap! store assoc :entries [] :pages {})]
    (fire-watchers! (:entries state))
    (:entries state)))

;; Watcher API

(defn watch!
  "Register a watcher under `key`. The fn `f` is called with the
   full active-notifications vec on every push / dismiss / page / tick.
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
