(ns com.blockether.vis.internal.channel-events
  "Process-local channel event bus.

   Extensions use this to talk to mounted channels without depending on their
   implementation namespaces. Channels subscribe while running and translate
   events into their local state/events. No listener failure may take down the
   publisher or sibling listeners."
  (:require [taoensso.telemere :as tel]))

;; {channel-id {listener-id listener-fn}}. Channel id is a keyword such as
;; :tui. Listener id is any stable value owned by the subscriber.
(defonce ^:private listeners (atom {}))

(defn add-channel-event-listener!
  "Subscribe `listener-fn` to events for `channel-id`.

   Listener receives the event map exactly as published. Re-registering the
   same `listener-id` for a channel replaces the old listener. Returns
   `listener-id`."
  [channel-id listener-id listener-fn]
  (when-not (keyword? channel-id)
    (throw (ex-info "channel-id must be a keyword"
                    {:type :vis/channel-event-bad-channel-id :channel-id channel-id})))
  (when-not (ifn? listener-fn)
    (throw (ex-info "listener-fn must be invokable"
                    {:type :vis/channel-event-bad-listener :listener-id listener-id})))
  (swap! listeners assoc-in [channel-id listener-id] listener-fn)
  listener-id)

(defn remove-channel-event-listener!
  "Remove a listener registered with [[add-channel-event-listener!]]. Returns
   nil."
  [channel-id listener-id]
  (swap! listeners update channel-id dissoc listener-id)
  nil)

(defn channel-event-listeners
  "Return a snapshot of listener ids registered for `channel-id`. Diagnostic
   helper; callers must not rely on ordering."
  [channel-id]
  (vec (keys (get @listeners channel-id))))

(defn publish-channel-event!
  "Publish `event` to currently-running subscribers of `channel-id`.

   The event is assoc'ed with `:channel/id` when absent. Returns the number of
   listeners invoked. Listener exceptions are caught and logged."
  [channel-id event]
  (when-not (keyword? channel-id)
    (throw (ex-info "channel-id must be a keyword"
                    {:type :vis/channel-event-bad-channel-id :channel-id channel-id})))
  (when-not (map? event)
    (throw (ex-info "channel event must be a map"
                    {:type :vis/channel-event-bad-event :event event})))
  (let
    [snapshot
     (get @listeners channel-id)

     event
     (cond-> event
       (nil? (:channel/id event))
       (assoc :channel/id channel-id))]

    (doseq [[listener-id listener-fn] snapshot]
      (try (listener-fn event)
           (catch Throwable t
             (tel/log! {:level :warn
                        :id ::listener-failed
                        :data {:channel-id channel-id
                               :listener-id listener-id
                               :event-op (:op event)
                               :error (ex-message t)}
                        :msg "Channel event listener failed"}))))
    (count snapshot)))
