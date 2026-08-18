(ns com.blockether.vis.internal.gateway.human-input
  "Gateway half of the typed human-input pause AND of the live view.

   `com.blockether.vis.internal.human-input` BLOCKS the extension thread and
   publishes `:human-input/request` / `:human-input/close` on every channel the
   request names — `[:tui :app]` by default. The TUI draws its dialog from the
   `:tui` channel; this namespace serves the `:app` channel and is the reason a
   companion-app operator ever learns a run is waiting on them:

     - the request becomes a `human_input.request` SESSION event, so it rides
       SSE live, sits in the replay ring for a client that connects later, and
       reaches the push tap (which alerts the phone);
     - the close becomes `human_input.close`, so every client drops the form
       the moment ANY surface answers — TUI, app or a timeout;
     - [[pending]] / [[submit!]] / [[cancel!]] back the REST routes so the app
       can answer a request it finds already open.

   A LIVE VIEW is the same interaction inverted: nothing blocks, the human only
   watches work move and may stop it. Its three events cross here the same way
   (`human_input.live.open` / `.patch` / `.close`), with one difference that is
   about the phone and not about the engine: patches are COALESCED on a tick
   ([[live-flush-ms]]) before they are published, because a run that streams a
   build log would otherwise pay a durable journal write per line.
   [[live-views]] / [[live-view-of]] / [[live-log-range]] / [[interrupt-live!]]
   back the REST routes, so a client that joined mid-flight reads the picture
   back instead of replaying a stream it never saw.

   Every request names the session it parks — `human-input/request!` refuses one
   that does not — so a run waiting on a human is always a run the app can see.
   A live view names its session for the same reason."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as channel-events]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.human-input :as human-input]
            [com.blockether.vis.internal.human-input.live-sink :as live-sink]
            [com.blockether.vis.internal.human-input.spec :as hi-spec])
  (:import [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(set! *warn-on-reflection* true)

(def channel-id "Channel the companion app is served on." :app)

(def ^:private listener-id ::gateway)

(def live-open-event
  "Session event a live view is DECLARED with. Carries the whole materialized
   view, so a client that has this frame needs nothing else to start painting."
  "human_input.live.open")

(def live-patch-event
  "Session event carrying accepted operations against an open view."
  "human_input.live.patch")

(def live-close-event
  "Session event a live view ENDS with, verdict and all."
  "human_input.live.close")

(defn- session-of
  "Session id a request view / close event belongs to, as a string, or nil."
  [m]
  (some-> (:session-id m)
          str
          str/trim
          not-empty))

(defn pending
  "Pending human-input request views for session `sid`, oldest first. These are
   the requests this session is BLOCKED on right now."
  [sid]
  (let [sid (str sid)]
    (filterv #(= sid (session-of %)) (human-input/pending-requests))))

(defn request-of
  "The pending request `request-id` when it belongs to `sid`, else nil. Every
   REST answer goes through this: a request id from another session (or an
   already-settled one) must not be answerable."
  [sid request-id]
  (let [view (human-input/pending-request (str request-id))]
    (when (and view (= (str sid) (session-of view))) view)))

(defn submit!
  "Answer `request-id` with a raw `field id -> value` map. Returns
   `{:is-accepted true}`, or `{:is-accepted false :errors {field-id message}}`
   when a value fails validation — the request stays pending so the operator
   can fix it."
  [request-id values]
  (human-input/submit! (str request-id) values))

(defn cancel!
  "Cancel `request-id`. Returns true when it was still pending and dismissable:
   the engine refuses a request declared `is_cancellable false`, so the app is
   held to exactly the rule the TUI dialog paints."
  [request-id]
  (human-input/cancel! (str request-id) "cancelled"))

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
  "Fold `op` into `ops`, merging it into the last op that touched the SAME node
   when the two say the same thing twice.

   `set` MERGES on the engine's side, so two sets on one node are one set with
   the later keys winning; `append` upserts by id, so two appends are one append
   with the items still in order. Everything structural — `add-node`,
   `remove-node`, `clear`, `remove` — is kept exactly as it stands: it decides
   what the node IS, and a surface that never saw it would paint a view the
   record does not have. Nothing is ever reordered: an op merges only into the
   last op on its OWN node, so ops on different nodes keep their order and a
   `clear` between two appends still cuts the log."
  [ops op]
  (let
    [node
     (op-node op)

     at
     (when node
       (last (keep-indexed (fn [i earlier]
                             (when (= node (op-node earlier)) i))
                           ops)))

     prior
     (when at (nth ops at))

     k
     (append-key op)]

    (cond (nil? prior) (conj ops op)
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
                         live-patch-event
                         {:view-id (:view-id frame)
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
  (let
    [fresh?
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
    (filterv #(= sid (session-of %)) (human-input/live-views))))

(defn live-view-of
  "Live view `view-id` when it belongs to `sid`, else nil. Every REST answer goes
   through this: a view id from another session must not be readable or
   stoppable from here."
  [sid view-id]
  (let [view (human-input/live-view (str view-id))]
    (when (and view (= (str sid) (session-of view))) view)))

(def ^:private live-log-page
  "Lines one log page answers: a WINDOW, the same amount a surface holds live
   ([[hi-spec/log-defaults]]). It is both the default and the ceiling — a client
   asking for more than a window is asking for the file, and the record IS the
   file."
  (:window-lines hi-spec/log-defaults))

(defn live-log-range
  "`limit` lines of log node `node-id` in view `view-id` of session `sid`, from
   0-based `from`, read from the view's RECORD. Both may be nil: the page policy
   lives HERE, so the REST route and any other caller page the same way.

   The picture carries only the node's window, so this is how a phone scrolls
   back through output whose patches it never received. It reads the file, not
   the registry: a view that has already closed still answers, which is what
   makes a finished run's log readable at all."
  [sid view-id node-id from limit]
  (live-sink/log-range (live-sink/view-file (str sid) (str view-id))
                       node-id
                       (max 0 (long (or from 0)))
                       (min (long live-log-page) (max 1 (long (or limit live-log-page))))))

(defn interrupt-live!
  "Stop live view `view-id` from the app, with `note` — the comment the person
   leaves with the stop, when they leave one. Returns the verdict, or nil when the
   view had already ended: a view is ALWAYS stoppable, exactly as Escape always
   stops the one the TUI is painting."
  ([view-id] (interrupt-live! view-id nil))
  ([view-id note] (human-input/interrupt-live! (str view-id) note)))

(defn on-channel-event!
  "Translate one `:app` channel event into a session event. Unknown ops are
   ignored — the channel bus is shared.

   Every request names a session: `human-input/request!` refuses one that does
   not, so the app is always told which run is parked. A live view names its
   session the same way, and its three events cross as the same kind of ordinary
   journal event: they ride SSE live, sit in the replay ring for a client that
   connects later, and reach a client in ANOTHER process — which is what lets a
   second terminal paint the very same view."
  [event]
  (case (:op event)
    :human-input/request
    (when-let [sid (session-of (:request event))]
      (state/append-event! sid "human_input.request" {:request (:request event)}))

    :human-input/close
    ;; `:session-id` rides on the close event itself: by the time it is
    ;; published the request is already out of the pending registry.
    (when-let [sid (session-of event)]
      (state/append-event! sid
                           "human_input.close"
                           {:request-id (:request-id event) :reason (:reason event)}))

    :human-input/live-open
    (when-let [sid (session-of event)]
      (state/append-event! sid live-open-event {:view-id (:view-id event) :view (:view event)}))

    :human-input/live-patch
    (when-let [sid (session-of event)]
      (buffer-patch! sid (:view-id event) (:patch event)))

    :human-input/live-close
    (when-let [sid (session-of event)]
      ;; An ending must never overtake the work it ends: whatever this view
      ;; still holds is published first, in the order the engine accepted it.
      (flush-view! (:view-id event))
      (state/append-event! sid
                           live-close-event
                           {:view-id (:view-id event) :result (:result event)}))

    nil)
  nil)

(defn install!
  "Subscribe the gateway to `:app` human-input events. Idempotent — the bus
   replaces a listener registered under the same id."
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
