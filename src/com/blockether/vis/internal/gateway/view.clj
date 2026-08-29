(ns com.blockether.vis.internal.gateway.view
  "Gateway transport for the shared View lifecycle.

   The engine publishes `:view/open`, `:view/patch` and `:view/close` envelopes on
   every selected channel. This namespace projects the `:app` copy into the matching
   `view.open`, `view.patch` and `view.close` session events, preserving `:kind` so
   clients choose capability policy without guessing from event names.

   An `:input` View blocks while a `:live` View streams coalesced patches on
   [[live-flush-ms]]. Operator intent for either kind enters through one [[action!]]
   and one kind-independent REST resource; the engine owns the closed action
   vocabulary and capability checks."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as channel-events]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.view :as view]
            [com.blockether.vis.internal.view.sink :as sink]
            [com.blockether.vis.internal.view.spec :as view-spec])
  (:import [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(set! *warn-on-reflection* true)

(def channel-id "Channel the companion app is served on." :app)

(def ^:private listener-id ::gateway)

(def view-open-event
  "Session event that mounts either View kind with its complete starting document."
  "view.open")

(def view-patch-event
  "Session event carrying accepted operations against an open View."
  "view.patch")

(def view-close-event "Session event that ends either View kind with its result." "view.close")

(defn- session-of
  "Session id a request view / close event belongs to, as a string, or nil."
  [m]
  (some-> (:session-id m)
          str
          str/trim
          not-empty))

(defn view-of
  "Open View `view-id` when it belongs to session `sid`, as a kind/document
   descriptor, else nil. This is the ownership gate for the shared action route."
  [sid view-id]
  (when-let [{:keys [view] :as descriptor} (view/open-view (str view-id))]
    (when (= (str sid) (session-of view)) descriptor)))

(defn input-views
  "Input Views session `sid` is BLOCKED on right now, oldest first."
  [sid]
  (let [sid (str sid)]
    (filterv #(= sid (session-of %)) (view/pending-requests))))

(defn input-view-of
  "Input View `view-id` when it is pending in `sid`, else nil."
  [sid view-id]
  (let [{:keys [kind view]} (view-of sid view-id)]
    (when (= :input kind) view)))

(defn action!
  "Apply one closed operator action to open View `view-id`. Kind policy and all
   validation stay in the engine, so every remote surface receives the same verdict."
  [view-id action]
  (view/action! (str view-id) action))

;; --- Live views (a run the human WATCHES) ---

(def ^:private live-flush-ms
  "How long a view's patches are held before they are published as ONE session
   event.

   Not an optimization: `state/append-event!` is a DURABLE publish, so an
   extension draining a build log would park its own thread on the journal
   writer once per line, and the cross-process journal is force-truncated past
   16 MB mid-turn. Nothing is lost by waiting: the engine already accepted and
   RECORDED every patch in the view's sink file before this bridge saw it, and a
   client that misses a frame repairs from [[live-view-of]]."
  100)

(def ^:private live-merge-cap
  "The most items one coalesced `append` may carry — the engine's own
   `:max-patch-lines`. A merged frame stays the size of a patch the engine would
   have accepted, so a client's reducer is never handed something the producer
   could not have sent."
  500)

(defonce ^:private buffered (atom {}))

(defonce ^:private flusher
  (delay (Executors/newSingleThreadScheduledExecutor
           (reify
             ThreadFactory
               (newThread [_ runnable]
                 (doto (Thread. ^Runnable runnable "vis-live-view-flush") (.setDaemon true)))))))

(defn- op-node
  "The node an op addresses. `add-node` names it inside the spec it carries."
  [op]
  (or (:node-id op) (:id (:node-spec op))))

(defn- append-key
  "The one payload key an `append` carries — `:lines` for a log, the keyed
   collection for everything else."
  [op]
  (first (filter #(contains? op %) [:lines :rows :stats :steps :links])))

(defn- coalesce
  "Fold `op` into `ops`, merging it into the last operation with the same owner
   when two updates in one phone frame supersede each other.

   `set` MERGES on the engine's side, so two sets on one node are one set with
   the later keys winning; `append` upserts by id, so two appends are one append
   with the items still in order. `set-activity` is already a complete bounded
   snapshot, so only its latest replacement belongs on the wire. Everything
   structural — `add-node`, `remove-node`, `clear`, `remove` — is kept exactly as
   it stands: it decides what the node IS, and a surface that never saw it would
   paint a view the record does not have. Nothing is ever reordered: a node op
   merges only into the last op on its OWN node."
  [ops op]
  (let [node
        (op-node op)

        at
        (cond (= :set-activity (:op op)) (last (keep-indexed (fn [i earlier]
                                                               (when (= :set-activity (:op earlier))
                                                                 i))
                                                             ops))
              node (last (keep-indexed (fn [i earlier]
                                         (when (= node (op-node earlier)) i))
                                       ops)))

        prior
        (when (some? at) (nth ops at))

        k
        (append-key op)]

    (cond (nil? prior) (conj ops op)
          (= :set-activity (:op op) (:op prior)) (assoc ops at op)
          (= :set (:op op) (:op prior)) (assoc ops at (merge prior op))
          (and (= :append (:op op) (:op prior))
               (some? k)
               (= k (append-key prior))
               (<= (+ (count (get prior k)) (count (get op k))) (long live-merge-cap)))
          (assoc ops at (update prior k into (get op k)))
          :else (conj ops op))))

(defn- publish-patches!
  "Publish one coalesced frame. `first_seq`..`seq` is the range of engine
   patches it carries, so a client can tell a COALESCED jump from a real gap: a
   frame that does not continue the one before it is a resync, not loss."
  [frame]
  (when (seq (:ops frame))
    (state/append-event! (:session-id frame)
                         view-patch-event
                         {:kind :live
                          :view-id (:view-id frame)
                          :first-seq (:first-seq frame)
                          :patch {:view-id (:view-id frame) :seq (:seq frame) :ops (:ops frame)}})))

(defn- flush-view!
  "Publish and forget whatever `view-id` has buffered."
  [view-id]
  (let [[old _] (swap-vals! buffered dissoc view-id)]
    (some-> (get old view-id)
            publish-patches!)))

(defn flush-live-patches!
  "Publish every buffered patch NOW. The tick decides when this happens in a
   running gateway; a caller that needs the stream settled — a test, a shutdown —
   asks for it."
  []
  (let [[old _] (swap-vals! buffered empty)]
    (run! publish-patches! (vals old))
    nil))

(defn- buffer-patch!
  "Hold one accepted patch for `view-id` and make sure a flush is coming."
  [sid view-id patch]
  (let [fresh?
        (nil? (get @buffered view-id))

        _
        (swap! buffered update
          view-id
          (fn [frame]
            (-> (or frame {:session-id sid :view-id view-id :first-seq (:seq patch) :ops []})
                (assoc :seq (:seq patch))
                (update :ops #(reduce coalesce % (:ops patch))))))]

    (when fresh?
      (.schedule ^ScheduledExecutorService @flusher
                 ^Runnable
                 (fn []
                   (flush-view! view-id))
                 (long live-flush-ms)
                 TimeUnit/MILLISECONDS))
    nil))

(defn live-views
  "The live views open in session `sid` right now, oldest first — the resync a
   client reads after joining late, waking up or losing its stream."
  [sid]
  (let [sid (str sid)]
    (filterv #(= sid (session-of %)) (view/live-views))))

(defn live-view-of
  "Live view `view-id` when it belongs to `sid`, else nil. Every REST answer goes
   through this: a view id from another session must not be readable from here."
  [sid view-id]
  (let [{:keys [kind view]} (view-of sid view-id)]
    (when (= :live kind) view)))

(def ^:private live-log-page
  "Lines one log page answers: a WINDOW, the same amount a surface holds live
   ([[view-spec/log-defaults]]). It is both the default and the ceiling — a client
   asking for more than a window is asking for the file, and the record IS the
   file."
  (:window-lines view-spec/log-defaults))

(defn live-log-range
  "`limit` lines of log node `node-id` in view `view-id` of session `sid`, from
   0-based `from`, read from the view's RECORD. Both may be nil: the page policy
   lives HERE, so the REST route and any other caller page the same way.

   The picture carries only the node's window, so this is how a phone scrolls
   back through output whose patches it never received. It reads the file, not
   the registry: a view that has already closed still answers, which is what
   makes a finished run's log readable at all."
  [sid view-id node-id from limit]
  (sink/log-range (sink/view-file (str sid) (str view-id))
                  node-id
                  (max 0 (long (or from 0)))
                  (min (long live-log-page) (max 1 (long (or limit live-log-page))))))


(defn on-channel-event!
  "Project one canonical `:app` View event into the session journal."
  [event]
  (case (:op event)
    :view/open
    (when-let [sid (session-of event)]
      (state/append-event! sid
                           view-open-event
                           {:kind (:kind event) :view-id (:view-id event) :view (:view event)}))

    :view/patch
    (when (and (= :live (:kind event)) (session-of event))
      (buffer-patch! (session-of event) (:view-id event) (:patch event)))

    :view/close
    (when-let [sid (session-of event)]
      ;; An ending never overtakes buffered live work.
      (when (= :live (:kind event)) (flush-view! (:view-id event)))
      (state/append-event! sid
                           view-close-event
                           {:kind (:kind event) :view-id (:view-id event) :result (:result event)}))

    nil)
  nil)

(defn install!
  "Subscribe the gateway to `:app` View lifecycle events. Idempotent."
  []
  (channel-events/add-channel-event-listener! channel-id listener-id on-channel-event!)
  nil)

(defn uninstall!
  "Drop the subscription installed by [[install!]]. Anything a view still has
   buffered is published first: a gateway going away must not swallow patches
   the engine already accepted."
  []
  (channel-events/remove-channel-event-listener! channel-id listener-id)
  (flush-live-patches!)
  nil)
