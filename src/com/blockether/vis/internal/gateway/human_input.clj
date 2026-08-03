(ns com.blockether.vis.internal.gateway.human-input
  "Gateway half of the typed human-input pause.

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

   A request raised outside a gateway session carries no `:session-id`. It is
   still published (the TUI shows it) and silently skipped here — a session
   event with no session has nowhere to go."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as channel-events]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.human-input :as human-input]))

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

(defn on-channel-event!
  "Translate one `:app` channel event into a session event. Unknown ops are
   ignored — the channel bus is shared."
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

    nil)
  nil)

(defn install!
  "Subscribe the gateway to `:app` human-input events. Idempotent — the bus
   replaces a listener registered under the same id."
  []
  (channel-events/add-channel-event-listener! channel-id listener-id on-channel-event!)
  nil)

(defn uninstall!
  "Drop the subscription installed by [[install!]]."
  []
  (channel-events/remove-channel-event-listener! channel-id listener-id)
  nil)
