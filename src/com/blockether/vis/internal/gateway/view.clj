(ns com.blockether.vis.internal.gateway.view
  "Gateway transport for the shared View lifecycle.

   The engine publishes `:view/open`, `:view/patch` and `:view/close` envelopes on
   every selected channel. This namespace projects the `:app` copy into the matching
   `view.open`, `view.patch` and `view.close` session events, preserving `:kind` so
   clients choose capability policy without guessing from event names.

   An `:input` View blocks while a `:live` View streams coalesced patches on
   [[live-flush-ms]]. Operator intent for either kind enters through one [[action!]]
   and one kind-independent REST resource; the engine owns the closed action
   vocabulary and capability checks.

   A `:view/send` envelope carries a Send of an app of the python_execution sandbox.
   It becomes one bounded notification turn of the session that owns the app.

   An input View belongs only to the session that raises it. A managed subagent
   never raises one: the engine refuses its request, because its leader answers
   for it. Each input View carries an `:origin` that names the session which asks."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.channel.events :as channel-events]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.view.core :as view]
            [com.blockether.vis.internal.view.sink :as sink]
            [com.blockether.vis.contract.view :as view-spec]
            [com.blockether.vis.contract.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(set! *warn-on-reflection* true)

(def channel-id "Channel the companion app is served on." :app)

(def ^:private listener-id ::gateway)

(defn- session-of
  "Session id a request view / close event belongs to, as a string, or nil."
  [m]
  (some-> (:session-id m)
          str
          str/trim
          not-empty))

(defn- session-link
  "Title and parent session of `sid` as `{:title … :parent-id …}`, or nil when the
   store does not know the session. A lookup failure answers nil: origin data is
   help for the reader, never a reason to lose the form."
  [sid]
  (try (when-let [db (lp/db-info)]
         (let [record (lp/by-id sid)
               parent (some-> (agents/info db sid)
                              :parent_id
                              str
                              str/trim
                              not-empty)]

           (when (or record parent)
             (cond-> {}
               (some-> (:title record)
                       str/trim
                       not-empty)
               (assoc :title (str/trim (:title record)))

               parent
               (assoc :parent-id parent)))))
       (catch Throwable _ nil)))

(defn- origin-of
  "The `:origin` stamp of an input View raised in session `sid`."
  [sid]
  (let [{:keys [title parent-id]}
        (session-link sid)

        parent-title
        (when parent-id (:title (session-link parent-id)))]

    (cond-> {:session-id sid}
      title
      (assoc :title title)

      parent-id
      (assoc :parent-session-id parent-id)

      parent-title
      (assoc :parent-title parent-title))))

(defn- with-origin
  "Input View `view` with its `:origin` stamp."
  [view]
  (if-let [sid (session-of view)]
    (assoc view :origin (origin-of sid))
    view))

(defn person-may-answer?
  "Whether the person may be asked in session `sid`. This is the ask gate of the
   engine: a managed subagent may not ask, because its leader answers for it."
  [sid]
  (agents/person-answers? (lp/db-info) (str sid)))

(defn view-of
  "Open View `view-id` when session `sid` may act on it, as a kind/document
   descriptor, else nil. This is the ownership gate for the shared action route.
   A View belongs only to its session."
  [sid view-id]
  (or (when-let [{:keys [view] :as descriptor} (view/open-view (str view-id))]
        (when (= (str sid) (session-of view)) descriptor))
      ;; A closed app stays readable. After a restart, its record answers.
      (when-let [app (view/app-view sid view-id)]
        {:kind :live :view app})))

(defn input-views
  "Input Views session `sid` is BLOCKED on right now, oldest first, each with its
   `:origin`."
  [sid]
  (let [sid (str sid)]
    (into [] (comp (filter #(= sid (session-of %))) (map with-origin)) (view/pending-requests))))

(defn input-view-of
  "Input View `view-id` when it is pending in `sid`, else nil."
  [sid view-id]
  (let [{:keys [kind view]} (view-of sid view-id)]
    (when (= :input kind) view)))

(defn action!
  "Apply one closed operator action to open View `view-id`. Kind policy and all
   validation stay in the engine, so every remote surface receives the same verdict.
   The decoded action's own keys become engine keywords here, once; the `values`
   it submits stay keyed by field name."
  [view-id action]
  (view/action! (str view-id)
                (cond-> action
                  (map? action)
                  (update-keys wire/engine-key))))

;; --- Live views (a run the human WATCHES) ---

(def ^:private live-flush-ms
  "How long a view's patches are held before they are published as ONE session
   event.

   Not an optimization: `state/append-event!` is a DURABLE publish, so an
   extension draining a build log would park its own thread on the journal
   writer once per line, and the cross-process journal is force-truncated past
   16 MB mid-turn. Nothing is lost by waiting: the engine already accepted and
   RECORDED every patch in the view's sink file before this bridge saw it, and a
   client that misses a frame repairs from [[live-view-of]].

   An APP frame is not durable at all. An app such as a game patches on every
   tick, and its record and [[live-view-of]] already keep its current picture, so
   a copy in the session journal only fills the replay of the transcript. Every
   app surface reads that picture when it opens and again after a gap. The frame
   skips the cross-process journal too: only the process that holds the app can
   serve its picture, so no other process can apply the frame."
  100)

(def ^:private live-merge-cap
  "The most items one coalesced `append` may carry — the engine's own
   `:max-patch-lines`. A merged frame stays the size of a patch the engine would
   have accepted, so a client's reducer is never handed something the producer
   could not have sent."
  500)

(defonce ^:private buffered (atom {}))

(defonce ^:private flusher
  (delay (Executors/newSingleThreadScheduledExecutor (util/daemon-thread-factory
                                                       "vis-live-view-flush"))))

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
   with the items still in order. Everything
   structural — `add-node`, `remove-node`, `clear`, `remove` — is kept exactly as
   it stands: it decides what the node IS, and a surface that never saw it would
   paint a view the record does not have. Nothing is ever reordered: a node op
   merges only into the last op on its OWN node."
  [ops op]
  (let [node
        (op-node op)

        at
        (when node
          (last (keep-indexed (fn [i earlier]
                                (when (= node (op-node earlier)) i))
                              ops)))

        prior
        (when (some? at) (nth ops at))

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
  (when (and (:session-id frame) (seq (:ops frame)))
    (state/append-event! (:session-id frame)
                         gateway-contract/view-patch-event
                         {:kind :live
                          :view-id (:view-id frame)
                          :first-seq (:first-seq frame)
                          :patch {:view-id (:view-id frame) :seq (:seq frame) :ops (:ops frame)}}
                         {:store? (not (:is-app frame)) :mirror? (not (:is-app frame))})))

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
  "Hold one accepted patch for `view-id` and make sure a flush is coming. The
   frame of an app is published live only."
  [sid view-id patch app?]
  ;; The swap that STARTS a frame schedules its flush. A freshness read taken
  ;; before the swap could miss a flush that emptied the buffer in between, and
  ;; then no later patch scheduled one: the view froze while its record grew.
  (let [[old _]
        (swap-vals!
          buffered
          update
          view-id
          (fn [frame]
            (-> (or frame
                    {:session-id sid :view-id view-id :first-seq (:seq patch) :ops [] :is-app app?})
                (assoc :seq (:seq patch))
                (update :ops #(reduce coalesce % (:ops patch))))))]
    (when (nil? (get old view-id))
      (.schedule ^ScheduledExecutorService @flusher
                 ^Runnable
                 (fn []
                   (flush-view! view-id))
                 (long live-flush-ms)
                 TimeUnit/MILLISECONDS))
    nil))

(defn live-views
  "The live views of session `sid`, oldest first — the resync a client reads after
   joining late, waking up or losing its stream. A `placement` (`inline`, `panel`
   or `attachment`) keeps only the views it names. An app stays in the list after
   it closes, until it is archived. With `archived?`, the list holds only the
   archived apps."
  ([sid] (live-views sid nil false))
  ([sid placement] (live-views sid placement false))
  ([sid placement archived?]
   (let [sid
         (str sid)

         placement
         (some-> placement
                 name
                 keyword)

         apps
         (filter #(= (boolean archived?) (boolean (:is-archived %))) (view/session-apps sid))

         views
         (when-not archived? (filter #(= sid (session-of %)) (view/live-views)))]

     (->> (concat views apps)
          (filter #(or (nil? placement) (= placement (:placement %))))
          (sort-by :created-at)
          vec))))

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
  "Read one bounded page of a live or closed log. `query` is a case-insensitive
   literal substring; `from` counts matches, not source lines. Nil paging values
   use the log-window policy, which is also the maximum response size."
  [sid view-id node-id from limit & [query]]
  (sink/log-range (sink/view-file (str sid) (str view-id))
                  node-id
                  (max 0 (long (or from 0)))
                  (min (long live-log-page) (max 1 (long (or limit live-log-page))))
                  query))

;; --- App Sends (a person hands work back to the agent) ---

(def ^:private send-title-chars
  "The most characters of an app title that a Send notification quotes."
  80)

(defn- counted [n noun] (str n " " noun (when-not (= 1 n) "s")))

(defn- send-summary
  "The app title and the size of Send `event`, such as `\"Review\": 2 comments, 1 value`."
  [{:keys [title submission]}]
  (let [title
        (str/trim (str title))

        limit
        (long send-title-chars)]

    (str "\"" (if (> (count title) limit) (str (subs title 0 (dec limit)) "…") title)
         "\": " (counted (count (:comments submission)) "comment")
         ", " (counted (count (:values submission)) "value"))))

(defn- send-request
  "The bounded notification that Send `event` puts into the context of the agent:
   the app, the receipt and the calls that read and answer it, never the review."
  [{:keys [view-id submission] :as event}]
  (let [id (:id submission)]
    (str "App Send: a person sent the app "
         (send-summary event)
         " (view_id "
         view-id
         ", receipt "
         id
         "). Read it with app = attach({\"view_id\": \""
         view-id
         "\", \"media_type\": \""
         app/media-type
         "\"}) and app.submission(\""
         id
         "\"). Answer with app.reply(node_id, body, submission_id=\""
         id
         "\"), or move the receipt with app.set_status(\""
         id
         "\", status, note=...).")))

(defn- deliver-send!
  "Queue one notification turn for Send `event` in the session of its sandbox app,
   then mark the receipt delivered. The session queue applies: a busy session runs
   the turn later. Only an accepted receipt is offered, so a Send that the owner
   already read is not offered twice. A refusal is logged, and the receipt stays
   accepted for a later offer."
  [{:keys [view-id submission] :as event}]
  (let [sid
        (session-of event)

        submission-id
        (:id submission)]

    (try (when (and sid
                    submission-id
                    (= "accepted"
                       (some-> (view/app-view sid view-id)
                               (app/find-submission submission-id)
                               :status)))
           (let [result (state/submit-turn!
                          sid
                          {:request (send-request event)
                           :display-request (str "Sent from the app " (send-summary event))
                           :idempotency-key (str "app-send:" view-id ":" submission-id)})]
             (if (:turn result)
               (view/mark-send-delivered! view-id submission-id)
               (tel/log! {:level :warn
                          :id ::send-not-delivered
                          :data {:session-id sid
                                 :view-id view-id
                                 :submission-id submission-id
                                 :error (:error result)
                                 :message (:message result)}
                          :msg
                          "The session did not take an app Send; its receipt stays accepted"}))))
         (catch Throwable t
           (tel/log!
             {:level :warn
              :id ::send-delivery-failed
              :data
              {:session-id sid :view-id view-id :submission-id submission-id :error (ex-message t)}
              :msg "Vis could not give an app Send to its session"})))))

(defn on-channel-event!
  "Project one canonical `:app` View event into the session journal. A Send of a
   sandbox app becomes a notification turn of its session."
  [event]
  (case (:op event)
    :view/open
    (let [sid (session-of event)]
      (cond (and sid (= :input (:kind event)))
            (state/append-event! sid
                                 gateway-contract/view-open-event
                                 {:kind :input
                                  :view-id (:view-id event)
                                  :view (assoc (:view event) :origin (origin-of sid))})
            sid (state/append-event!
                  sid
                  gateway-contract/view-open-event
                  {:kind (:kind event) :view-id (:view-id event) :view (:view event)})))

    :view/patch
    (when (= :live (:kind event))
      (buffer-patch! (session-of event) (:view-id event) (:patch event) (boolean (:is-app event))))

    :view/close
    (let [sid (session-of event)]
      ;; An ending never overtakes buffered live work.
      (when (= :live (:kind event)) (flush-view! (:view-id event)))
      (when sid
        (state/append-event!
          sid
          gateway-contract/view-close-event
          {:kind (:kind event) :view-id (:view-id event) :result (:result event)})))

    :view/send
    (deliver-send! event)

    nil)
  nil)

(defn install!
  "Subscribe the gateway to `:app` View lifecycle events. Idempotent."
  []
  (channel-events/add-channel-event-listener! channel-id listener-id on-channel-event!)
  ;; The saved pictures of owned views come back as stale views.
  (view/restore-owned!)
  nil)

(defn uninstall!
  "Drop the subscription installed by [[install!]]. Anything a view still has
   buffered is published first: a gateway going away must not swallow patches
   the engine already accepted."
  []
  (channel-events/remove-channel-event-listener! channel-id listener-id)
  (flush-live-patches!)
  (view/flush-owned!)
  nil)
