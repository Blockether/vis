(ns com.blockether.vis.internal.loop.cache-warmer
  "Prompt-cache keepalive for one session. After a real provider request, it plans
   a warm request shortly before the provider cache TTL ends. A long tool run, or a
   short pause in `idle` mode, then keeps the cache. A warm runs only when its
   expected saving beats its cost."
  (:require [com.blockether.vis.internal.loop.accounting :as accounting]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.util.concurrent Executors ScheduledExecutorService ScheduledFuture TimeUnit)))

(def ^:private TTL_MARGIN_MS
  "A warm reaches the provider at least this long before the cache expires."
  10000)

(def ^:private MIN_DELAY_MS 1000)

(def ^:private RUNNING_WINDOW_MS
  "Warms stop this long after the last real request of a running turn."
  (* 60 60 1000))

(def ^:private IDLE_WINDOW_MS
  "In `idle` mode, warms stop this long after the turn ends."
  (* 30 60 1000))

(def ^:private MIN_SAVINGS_USD "A warm runs only when it saves at least this much." 0.05)

(def ^:private IDLE_REUSE_PROBABILITY
  "Chance that a new request arrives inside the cache TTL after a turn ends."
  0.15)

(defn warm-delay-ms
  "Returns the wait from a request to its warm in milliseconds: 90% of `ttl-ms`,
   but at least 10 seconds before the cache expires. Returns nil when `ttl-ms` is
   nil or leaves no room."
  [ttl-ms]
  (when (and ttl-ms (> (long ttl-ms) (long TTL_MARGIN_MS)))
    (max (long MIN_DELAY_MS)
         (long (Math/floor (min (* 0.9 (double ttl-ms))
                                (double (- (long ttl-ms) (long TTL_MARGIN_MS)))))))))

(defn late-limit-ms
  "Returns how late a timer can fire and still warm: half of the time between the
   planned warm and the cache expiry. A later timer, for example after the computer
   slept, finds the cache gone."
  [ttl-ms delay-ms]
  (quot (- (long ttl-ms) (long delay-ms)) 2))

(defn warm-savings-usd
  "Returns the expected saving of one warm in USD, or nil for an unpriced route. The
   warm reads `:prompt-tokens` from the cache and writes one output token. Without
   it, the next request writes the prompt to the cache again, with probability
   `reuse-probability`. `:pricing`, `:model` and `:provider` price the tokens."
  [{:keys [pricing model provider prompt-tokens]} reuse-probability]
  (when (and pricing (pos? (long (or prompt-tokens 0))))
    (let [cost
          (fn [cache-read cache-write output]
            (:cost-usd (accounting/response-cost pricing
                                                 {:input-tokens prompt-tokens
                                                  :output-tokens output
                                                  :input-tokens-details {:regular 0
                                                                         :cache-read cache-read
                                                                         :cache-write cache-write}}
                                                 model
                                                 provider)))

          hit
          (cost prompt-tokens 0 0)

          miss
          (cost 0 prompt-tokens 0)

          warm
          (cost prompt-tokens 0 1)]

      (when (and hit miss warm)
        (- (* (double reuse-probability) (max 0.0 (- (double miss) (double hit))))
           (double warm))))))

(defonce ^:private scheduler
  (delay (Executors/newSingleThreadScheduledExecutor (util/daemon-thread-factory
                                                       "vis-prompt-cache-warm"))))

(defn- schedule-on-executor
  "Runs `task` on its own thread after `delay-ms`, so a slow warm never holds the
   shared timer thread. Returns a function that cancels the timer."
  [delay-ms task]
  (let [^ScheduledExecutorService executor
        @scheduler

        ^Runnable start
        #(future-call task)

        ^ScheduledFuture timer
        (.schedule executor start (long delay-ms) TimeUnit/MILLISECONDS)]

    #(.cancel timer false)))

(defn create
  "Returns the prompt-cache warmer of one session. Tests can pass `:clock`, a
   function that returns epoch milliseconds, and `:schedule`, a function of a delay
   and a task that returns a cancel function."
  ([] (create {}))
  ([{:keys [clock schedule]}]
   (atom {:generation 0
          :clock (or clock #(System/currentTimeMillis))
          :schedule (or schedule schedule-on-executor)
          :pending-costs []})))

(defn- disarm!
  "Starts a new generation, so a scheduled or running warm becomes stale, and
   cancels the timer. With `forget?`, it also drops the request that warms replay."
  [warmer forget?]
  (let [[old] (swap-vals! warmer
                          (fn [state]
                            (cond-> (-> state
                                        (update :generation inc)
                                        (dissoc :timer))
                              forget?
                              (dissoc :request :phase :until-ms))))]
    (when-let [cancel-timer (:timer old)]
      (cancel-timer))))

(defn- current? [warmer generation] (= generation (:generation @warmer)))

(defn- send-warm!
  "Sends one warm and keeps its cost for the next turn accounting. Returns
   `started-ms` when the warm succeeded and its generation is still current."
  [warmer generation started-ms request savings]
  (let [result
        (try ((:warm! request) #(not (current? warmer generation)))
             (catch Exception e
               (tel/log! {:level (if (current? warmer generation) :info :debug)
                          :id ::warm-failed
                          :data {:provider (:provider request)
                                 :model (:model request)
                                 :error (ex-message e)
                                 :type (:type (ex-data e))}}
                         "Prompt-cache warm failed")
               nil))

        api-usage
        (:api-usage result)

        cost
        (when api-usage
          (accounting/response-cost (:pricing request)
                                    api-usage
                                    (:model request)
                                    (:provider request)))]

    (when cost (swap! warmer update :pending-costs conj cost))
    (when result
      (tel/log! {:level :info
                 :id ::warmed
                 :data {:provider (:provider request)
                        :model (:model request)
                        :prompt-tokens (:prompt-tokens request)
                        :cache-read (get-in api-usage [:input-tokens-details :cache-read])
                        :savings-usd savings
                        :cost-usd (:cost-usd cost)}}
                "Prompt cache warmed"))
    (when (and result (current? warmer generation)) started-ms)))

(defn- skip-reason
  "Returns why the warm due at `due-ms` must not run at `now`, or nil."
  [{:keys [request phase until-ms]} now due-ms savings]
  (let [ttl-ms
        (:ttl-ms request)

        mode
        ((:mode request))]

    (cond (not (contains? #{"running" "idle"} mode)) :disabled
          (and (= :idle phase) (not= "idle" mode)) :disabled
          (> (long now) (+ (long due-ms) (long (late-limit-ms ttl-ms (warm-delay-ms ttl-ms)))))
          :late
          (> (long now) (long until-ms)) :window-ended
          (nil? savings) :unpriced
          (< (double savings) (double MIN_SAVINGS_USD)) :not-worth)))

(defn- fire!
  "Sends the warm of `generation` when it is still current, on time, inside its
   window and worth its cost. Returns the warm start time that the next warm counts
   from, or nil to stop."
  [warmer generation due-ms]
  (let [{:keys [clock request phase] :as state} @warmer]
    (when (= generation (:generation state))
      (swap! warmer (fn [s]
                      (if (= generation (:generation s)) (dissoc s :timer) s)))
      (let [now (long (clock))
            savings (warm-savings-usd request (if (= :idle phase) IDLE_REUSE_PROBABILITY 1.0))]

        (if-let [reason (skip-reason state now due-ms savings)]
          (tel/log! {:level :debug
                     :id ::warm-skipped
                     :data {:reason reason
                            :provider (:provider request)
                            :model (:model request)
                            :savings-usd savings}}
                    "Prompt-cache warm skipped")
          (send-warm! warmer generation now request savings))))))

(defn- arm!
  "Plans the next warm of `generation`: one warm delay after `from-ms`. A timer for
   a stale generation is cancelled at once."
  [warmer generation from-ms]
  (let [{:keys [clock schedule request]}
        @warmer

        due-ms
        (+ (long from-ms) (long (warm-delay-ms (:ttl-ms request))))

        wait-ms
        (max 0 (- due-ms (long (clock))))

        cancel-timer
        (schedule wait-ms
                  #(when-let [started-ms (fire! warmer generation due-ms)] (arm! warmer
                                                                                 generation
                                                                                 started-ms)))

        [old]
        (swap-vals!
          warmer
          (fn [state]
            (if (= generation (:generation state)) (assoc state :timer cancel-timer) state)))]

    (when-not (= generation (:generation old)) (cancel-timer))))

(defn request-started!
  "Stops a planned or running warm, because a real provider request starts."
  [warmer]
  (when warmer (disarm! warmer false)))

(defn request-finished!
  "Plans warms after a successful real request. `request` holds:
     :mode          - function that returns the `prompt_cache_keepalive` value
     :ttl-ms        - cache TTL of the serving route, or nil for no warm
     :prompt-tokens - input tokens of the request, cached ones included
     :pricing :provider :model - price the warm, see `warm-savings-usd`
     :warm!         - function of a cancel predicate that sends the warm"
  [warmer {:keys [mode ttl-ms] :as request}]
  (when warmer
    (disarm! warmer true)
    (when (and (contains? #{"running" "idle"} (mode)) (warm-delay-ms ttl-ms))
      (let [now
            (long ((:clock @warmer)))

            state
            (swap! warmer assoc
              :request request
              :phase :running
              :until-ms (+ now (long RUNNING_WINDOW_MS)))]

        (arm! warmer (:generation state) now)))))

(defn turn-settled!
  "Ends the running phase when a turn ends. In `idle` mode, warms continue for 30
   minutes with a lower reuse chance. In other modes, they stop."
  [warmer]
  (when warmer
    (let [{:keys [request clock]} @warmer]
      (if (and request (= "idle" ((:mode request))))
        (swap! warmer assoc :phase :idle :until-ms (+ (long (clock)) (long IDLE_WINDOW_MS)))
        (disarm! warmer true)))))

(defn stop! "Stops all warms of a closing session." [warmer] (when warmer (disarm! warmer true)))

(defn drain-costs!
  "Adds the costs of finished warms to a turn's `accounting-atom`."
  [warmer accounting-atom]
  (when warmer
    (let [[old] (swap-vals! warmer assoc :pending-costs [])]
      (doseq [cost (:pending-costs old)]
        (swap! accounting-atom accounting/add-cost cost)))))
