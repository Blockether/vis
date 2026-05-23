(ns com.blockether.vis.ext.language-clojure.nrepl-client
  "Thin, observable nREPL client for `clj/eval`.

   Connection model:
     * One `nrepl.core/connect` socket per `[host port]` key, cached
       on a `defonce` atom so we survive `(require :reload)` during
       development.
     * On every `eval!` we open a fresh `client-session` against the
       cached connection. Sessions are *not* shared across calls so a
       caller's `*1`/`*e`/dynamic var binding never leaks across
       Vis tool invocations.
     * Stale / closed sockets are detected (`IOException` / `nil`
       message stream) and the entry is evicted; the next call
       re-dials.

   Returned shape (success):
     {:value      \"42\"          ; pr-str of the LAST form's value, or nil
      :values     [\"1\" \"42\"]   ; pr-str of every emitted value
      :out        \"hello\\n\"     ; stdout aggregated
      :err        \"\"             ; stderr aggregated
      :ns         \"user\"         ; final *ns* name
      :status     #{\"done\"}      ; nREPL status set
      :ex         nil              ; exception class name, when status :ex
      :root-ex    nil              ; root exception class name
      :ms         12               ; wall-clock duration
      :port       7888
      :timed-out? false}

   Failure paths throw `ex-info` with `:type :clj/nrepl-*` so the
   Vis tool wrapper can surface a clean error to the model."
  (:require
   [nrepl.core :as nrepl]
   [nrepl.transport :as transport])
  (:import
   (java.io IOException)))

;; ---------------------------------------------------------------------------
;; Connection cache
;; ---------------------------------------------------------------------------

(defonce ^:private connections
  ;; { [host port] -> {:conn <nrepl-connection> :opened-at ms} }
  (atom {}))

(defn- key-of [host port] [(or host "localhost") (long port)])

(defn- open!
  "Open a fresh nREPL connection. Wraps `connect` failures into a
   structured ex-info so callers can present a clean message."
  [host port timeout-ms]
  (try
    (nrepl/connect :host (or host "localhost")
      :port (int port)
      :transport-fn transport/bencode
      ;; transport-level read timeout; eval timeout is enforced
      ;; separately via combined-response-fn below.
      :timeout (long timeout-ms))
    (catch Throwable t
      (throw (ex-info (str "nREPL connect failed on " (or host "localhost") ":" port
                        " — is the REPL running? Try (clj/ports).")
               {:type :clj/nrepl-connect-failed
                :host (or host "localhost")
                :port port
                :cause (.getMessage t)})))))

(defn- evict! [host port]
  (swap! connections dissoc (key-of host port)))

(defn- connection-for
  "Get a cached connection or open a new one. The cached value is a
   raw `nrepl.transport/FnTransport` (an `AutoCloseable`); we keep it
   alive across calls."
  [host port timeout-ms]
  (let [k (key-of host port)]
    (or (get-in @connections [k :conn])
      (let [c (open! host port timeout-ms)]
        (swap! connections assoc k {:conn c :opened-at (System/currentTimeMillis)})
        c))))

(defn close-all!
  "Close every cached connection. Idempotent. Useful from
   tests / doctor / shutdown hooks."
  []
  (let [m @connections]
    (reset! connections {})
    (doseq [[_ {:keys [conn]}] m]
      (try (.close ^java.io.Closeable conn) (catch Throwable _ nil)))))

;; ---------------------------------------------------------------------------
;; eval
;; ---------------------------------------------------------------------------

(defn- combine
  "Walk an nREPL response seq and reduce it to the public result map.
   Stops on first `:status` containing \"done\" or after deadline."
  [responses deadline]
  (loop [rs       responses
         values   []
         out-acc  (StringBuilder.)
         err-acc  (StringBuilder.)
         ns*      nil
         status   #{}
         ex       nil
         root-ex  nil]
    (cond
      (> (System/currentTimeMillis) deadline)
      {:timed-out? true
       :value      (peek values)
       :values     values
       :out        (.toString out-acc)
       :err        (.toString err-acc)
       :ns         ns*
       :status     (conj status "timeout")
       :ex         ex
       :root-ex    root-ex}

      (empty? rs)
      {:timed-out? false
       :value      (peek values)
       :values     values
       :out        (.toString out-acc)
       :err        (.toString err-acc)
       :ns         ns*
       :status     status
       :ex         ex
       :root-ex    root-ex}

      :else
      (let [{:strs [value out err ns ex* root-ex*]
             :keys [status]
             :as   msg} (first rs)
            v   value
            o   out
            e   err
            n   ns
            s   status
            ex2 (or (get msg "ex") ex*)
            rx2 (or (get msg "root-ex") root-ex*)]
        (when o (.append out-acc ^String o))
        (when e (.append err-acc ^String e))
        (let [new-status (into status (or s []))
              done?      (contains? new-status "done")]
          (if done?
            (recur nil
              (cond-> values v (conj v))
              out-acc err-acc (or n ns*) new-status (or ex2 ex) (or rx2 root-ex))
            (recur (next rs)
              (cond-> values v (conj v))
              out-acc err-acc (or n ns*) new-status (or ex2 ex) (or rx2 root-ex))))))))

(defn eval!
  "Evaluate `code` in the nREPL at `host:port`. Opts:
     :host        defaults to \"localhost\"
     :ns          starting namespace, e.g. \"user\"
     :timeout-ms  default 30000

   Always returns a map (see ns docstring). Connection failures
   throw `:clj/nrepl-connect-failed`; everything else (eval error,
   timeout) is reported inside the returned map so the model can
   read it as data."
  [{:keys [host port code ns timeout-ms]
    :or   {host "localhost" timeout-ms 30000}}]
  (when-not (pos? (or port 0))
    (throw (ex-info "eval! requires a positive :port"
             {:type :clj/nrepl-bad-args :port port})))
  (when-not (string? code)
    (throw (ex-info "eval! requires a :code string"
             {:type :clj/nrepl-bad-args :code code})))
  (let [start    (System/currentTimeMillis)
        deadline (+ start (long timeout-ms))]
    (try
      (let [conn     (connection-for host port timeout-ms)
            client   (nrepl/client conn timeout-ms)
            session  (nrepl/client-session client)
            req      (cond-> {:op "eval" :code code}
                       (string? ns) (assoc :ns ns))
            responses (session req)
            combined (combine responses deadline)
            elapsed  (- (System/currentTimeMillis) start)]
        (assoc combined :ms elapsed :port (int port) :host host))
      (catch IOException ioe
        (evict! host port)
        (throw (ex-info (str "nREPL socket error on " host ":" port " — connection evicted, retry.")
                 {:type :clj/nrepl-io
                  :host host :port port
                  :cause (.getMessage ioe)})))
      (catch Throwable t
        (if (= :clj/nrepl-connect-failed (:type (ex-data t)))
          (throw t)
          (throw (ex-info (str "nREPL eval failed: " (.getMessage t))
                   {:type :clj/nrepl-eval-failed
                    :host host :port port
                    :cause (.getMessage t)})))))))
