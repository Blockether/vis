(ns com.blockether.vis.internal.speech.residency
  "The speech models that this process holds in memory (#448).

   A loader reports each model that it loads with `loaded!`: its size, a function
   that tells if a request uses it, and a function that releases it. A request that
   uses a model calls `used!`. One background thread then:

     - releases a model that nothing used for the idle time
       (setting `speech_model_idle_unload`), and
     - releases the least recently used models while the loaded models need more
       than the memory limit (setting `speech_model_memory`).

   It never releases a model that a request uses, and it never releases the most
   recently used model for the limit, so one model that is larger than the limit
   still works. The next request loads a released model again. `/healthz` shows
   the list from `status`."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(set! *warn-on-reflection* true)

(def memory-toggle "speech_model_memory")

(def idle-toggle "speech_model_idle_unload")

(def ^:const sweep-seconds "How often the background thread looks for idle models." 30)

;; id -> {:id :token :bytes :loaded-at :used-at :in-use? :release! :keep-toggle}
(defonce ^:private models* (atom {}))

(def ^:dynamic *registry*
  "The registry that this thread reads and writes. Tests bind a fresh atom."
  models*)

(defn choice-bytes
  "Bytes of a `speech_model_memory` choice such as \"4 GB\". nil means no limit."
  [choice]
  (when-let [[_ n] (re-matches #"(\d+) GB" (str choice))]
    (* (long (parse-long n)) 1024 1024 1024)))

(defn choice-ms
  "Milliseconds of a `speech_model_idle_unload` choice such as \"5 minutes\". nil
   means never."
  [choice]
  (when-let [[_ n] (re-matches #"(\d+) minutes?" (str choice))]
    (* (long (parse-long n)) 60 1000)))

(defn memory-limit [] (choice-bytes (toggles/value-of memory-toggle)))

(defn idle-ms [] (choice-ms (toggles/value-of idle-toggle)))

(defn file-bytes
  "The total size of the regular files in `paths`. A directory counts all of its
   files. A missing path counts zero."
  [paths]
  (reduce +
          0
          (for [p
                paths

                ^File f
                (file-seq (io/file p))

                :when (.isFile f)]

            (.length f))))

(declare request-sweep!)

(defn loaded!
  "Record that model `id` is in memory now. `entry` has:

     :token        the loaded object; `released!` compares it
     :bytes        the size of the model
     :release!     frees the model; returns false when a request uses it
     :in-use?      optional; true while a request uses the model
     :keep-toggle  optional; a boolean setting that keeps the model when it is idle

   The background thread applies the memory limit soon after."
  [id entry]
  (let [now (System/currentTimeMillis)]
    (swap! *registry* assoc
      id
      (merge {:in-use? (constantly false)} entry {:id id :loaded-at now :used-at now})))
  (when (identical? *registry* models*) (request-sweep!))
  nil)

(defn used!
  "Record that a request uses model `id` now."
  [id]
  (let [now (System/currentTimeMillis)]
    (swap! *registry* (fn [m]
                        (if (contains? m id) (assoc-in m [id :used-at] now) m))))
  nil)

(defn released!
  "Remove model `id` when `token` is still the loaded object."
  [id token]
  (swap! *registry* (fn [m]
                      (if (identical? token (get-in m [id :token])) (dissoc m id) m)))
  nil)

(defn victims
  "The models to release now, as `[entry reason]`. First the idle models, then the
   least recently used models while the rest need more than `limit` bytes. Never a
   model in use, and never the most recently used model for the limit. `limit` or
   `idle-ms` nil turns that rule off."
  [models now limit idle-ms]
  (let [entries
        (->> (vals models)
             (map #(assoc % :busy? (boolean ((:in-use? %)))))
             (sort-by :used-at))

        idle?
        (fn [{:keys [busy? used-at keep-toggle]}]
          (and idle-ms
               (not busy?)
               (>= (- (long now) (long used-at)) (long idle-ms))
               (not (and keep-toggle (toggles/enabled? keep-toggle)))))

        idle
        (filter idle? entries)

        kept
        (remove idle? entries)]

    ;; `butlast`: the most recently used model is never a victim of the limit.
    (loop [out
           (mapv #(vector % :idle) idle)

           total
           (reduce + 0 (map :bytes kept))

           [e & more]
           (butlast kept)]

      (cond (or (nil? limit) (<= (long total) (long limit)) (nil? e)) out
            (:busy? e) (recur out total more)
            :else (recur (conj out [e :limit]) (- (long total) (long (:bytes e))) more)))))

(defn sweep!
  "Release the models that `victims` names. Returns the released ids. `opts` may
   give `:now`, `:limit` and `:idle-ms`; the settings give the missing ones. A model
   that a request started to use after the check stays."
  ([] (sweep! {}))
  ([opts]
   (let [now
         (get opts :now (System/currentTimeMillis))

         limit
         (if (contains? opts :limit) (:limit opts) (memory-limit))

         idle
         (if (contains? opts :idle-ms) (:idle-ms opts) (idle-ms))]

     (vec
       (for [[{:keys [id token bytes release!]} reason]
             (victims @*registry* now limit idle)

             :when (let [freed (try (boolean (release!))
                                    (catch Throwable t
                                      (tel/log! {:level :warn
                                                 :id ::release-failed
                                                 :data {:model id :error (ex-message t)}})
                                      false))]
                     (when freed
                       (released! id token)
                       (tel/log! {:level :info
                                  :id ::model-released
                                  :data {:model id :bytes bytes :reason reason}
                                  :msg "released a speech model"}))
                     freed)]

         id)))))

(defn- sweep-safely!
  []
  (try (sweep!)
       (catch Throwable t
         (tel/log! {:level :warn :id ::sweep-failed :data {:error (ex-message t)}}))))

;; Created at the first load, never at build time: native-image initializes this
;; namespace when it builds the binary.
(defonce ^:private sweeper
  (delay (doto (Executors/newSingleThreadScheduledExecutor
                 (reify
                   ThreadFactory
                     (newThread [_ r]
                       (doto (Thread. ^Runnable r "vis-speech-residency") (.setDaemon true)))))
           (.scheduleWithFixedDelay ^Runnable sweep-safely!
                                    sweep-seconds
                                    sweep-seconds
                                    TimeUnit/SECONDS))))

(defn- request-sweep! [] (.execute ^ScheduledExecutorService @sweeper ^Runnable sweep-safely!))

(defn status
  "The loaded speech models for `/healthz`, most recently used first."
  []
  (let [now
        (System/currentTimeMillis)

        models
        (vals @*registry*)

        limit
        (memory-limit)

        idle
        (idle-ms)]

    {:models (->> models
                  (sort-by :used-at >)
                  (mapv (fn [{:keys [id bytes used-at in-use?]}]
                          {:id id
                           :bytes bytes
                           :idle_seconds (quot (- now (long used-at)) 1000)
                           :in_use (boolean (in-use?))})))
     :bytes (reduce + 0 (map :bytes models))
     :limit_bytes limit
     :idle_unload_seconds (some-> idle
                                  (quot 1000))}))
