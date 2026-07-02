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
      :timed_out false}

   Failure paths throw `ex-info` with `:type :clj/nrepl-*` so the
   Vis tool wrapper can surface a clean error to the model."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
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
                           " — is the REPL running? Check ctx for nREPL state or call clj_repl().")
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

(defn- terminal-error-output?
  "True when nREPL has already emitted a terminal eval/source error on *err* but
   may not send the final done status. Returning immediately avoids burning the
   whole eval timeout for broken source files or missing test dependencies."
  [s]
  (boolean (re-find #"(?m)^(Syntax|Execution|Compiler) error" (str s))))

(defn- combine
  "Walk an nREPL response seq and reduce it to the public result map.
   Stops on first :status containing done, after deadline, or on terminal errors
   that nREPL reports on *err* without a final done message."
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
      {:timed_out true
       :value      (peek values)
       :values     values
       :out        (.toString out-acc)
       :err        (.toString err-acc)
       :ns         ns*
       :status     (conj status "timeout")
       :ex         ex
       :root-ex    root-ex}

      (empty? rs)
      {:timed_out false
       :value      (peek values)
       :values     values
       :out        (.toString out-acc)
       :err        (.toString err-acc)
       :ns         ns*
       :status     status
       :ex         ex
       :root-ex    root-ex}

      :else
      ;; nREPL keys are *strings* over bencode; some clients keywordize.
      ;; Read both shapes so we don't silently lose value / status / out
      ;; depending on transport wiring.
      (let [msg (first rs)
            mg  (fn [k] (or (get msg k) (get msg (keyword k))))
            v   (mg "value")
            o   (mg "out")
            e   (mg "err")
            n   (mg "ns")
            s   (mg "status")
            ex2 (mg "ex")
            rx2 (mg "root-ex")]
        (when o (.append out-acc ^String o))
        (when e (.append err-acc ^String e))
        (let [new-status   (into status (cond
                                          (nil? s)    []
                                          (string? s) [s]
                                          (coll? s)   (map str s)
                                          :else       [(str s)]))
              done?        (contains? new-status "done")
              terminal-error? (terminal-error-output? (.toString err-acc))
              values'      (cond-> values v (conj v))
              ns''         (or n ns*)
              ex''         (or ex2 ex)
              rx''         (or rx2 root-ex)]
          (if (or done? terminal-error?)
            ;; Drain no further. Once "done" arrived the eval is complete; for
            ;; terminal errors nREPL may never send done, and waiting for it
            ;; turns a useful syntax error into a timeout.
            {:timed_out false
             :value      (peek values')
             :values     values'
             :out        (.toString out-acc)
             :err        (.toString err-acc)
             :ns         ns''
             :status     (cond-> new-status terminal-error? (conj "eval-error"))
             :ex         ex''
             :root-ex    rx''}
            (recur (next rs) values' out-acc err-acc ns'' new-status ex'' rx'')))))))

;; ---------------------------------------------------------------------------
;; error enrichment — a beautiful, structured stacktrace on the eval-error path
;; ---------------------------------------------------------------------------

(defn- eval-error?
  "True when a combined eval result carries a runtime/eval error worth enriching."
  [combined]
  (boolean (or (contains? (:status combined) "eval-error")
               (:ex combined)
               (:root-ex combined))))

(defn- sget
  "Read an nREPL response key that may arrive as a string or a keyword."
  [m k]
  (or (get m k) (get m (keyword k))))

(defn- frame-flags [f]
  (set (map str (sget f "flags"))))

(defn- cider-frame->line
  "`user/handle-request  (handler.clj:42)` from a cider-nrepl frame map."
  [f]
  (str (sget f "name") "  (" (sget f "file")
       (when-let [l (sget f "line")] (str ":" l)) ")"))

(defn- pick-frames
  "Drop cider's `dup`/`tooling` noise frames (the same ones its own UI hides by
   default), keep the rest in order, cap the depth."
  [frames limit]
  (->> frames
       (remove #(let [fl (frame-flags %)] (or (fl "dup") (fl "tooling"))))
       (map cider-frame->line)
       (take limit)
       vec))

(defn- simple-class [cls]
  (let [s (str cls)
        i (.lastIndexOf s ".")]
    (if (neg? i) s (subs s (inc i)))))

(defn- cause->headline [c]
  (let [msg (str (:message c))]
    (str (simple-class (:class c)) (when (seq msg) (str ": " msg)))))

(defn- stacktrace->result
  "Shape cider-nrepl cause messages into `{:error_message :error_data :trace}`.
   The root (innermost) cause carries the real throw site + message; earlier
   causes are threaded into the headline as a `← caused by` chain."
  [causes]
  (let [causes (vec causes)
        root   (or (last (filter #(seq (sget % "stacktrace")) causes))
                   (last causes))
        heads  (map #(cause->headline {:class (sget % "class") :message (sget % "message")})
                    causes)]
    {:error_message (str/join "\n  ← caused by " heads)
     :error_data    (some-> (sget root "data") not-empty)
     :trace         (pick-frames (sget root "stacktrace") 16)}))

(defn- collect-op
  "Drive an op on `session` to `done`, returning `{:status #{..} :msgs [..]}`.
   Bails on the deadline; never blocks past it."
  [session op deadline]
  (loop [rs     (session {:op op})
         msgs   []
         status #{}]
    (if (or (empty? rs) (> (System/currentTimeMillis) deadline))
      {:status status :msgs msgs}
      (let [msg (first rs)
            s   (sget msg "status")
            st  (into status (cond (nil? s) [] (string? s) [s]
                                   (coll? s) (map str s) :else [(str s)]))
            msgs' (cond-> msgs (sget msg "class") (conj msg))]
        (if (contains? st "done")
          {:status st :msgs msgs'}
          (recur (next rs) msgs' st))))))

(def ^:private e-fetch-code
  "Portable fallback (no cider-nrepl): read `*e` in-session and hand back a
   readable `{:error_message :error_data :trace}` map — demunged user frames,
   JVM/nREPL plumbing filtered out."
  (str "(when-let [e *e]"
       "  {:error_message (str (.getSimpleName (class e))"
       "                       (let [m (.getMessage e)] (when (seq (str m)) (str \": \" m))))"
       "   :error_data (when (instance? clojure.lang.IExceptionInfo e) (pr-str (ex-data e)))"
       "   :trace (let [fs (map (fn [^StackTraceElement el]"
       "                          [(.getClassName el) (.getFileName el) (.getLineNumber el)])"
       "                        (.getStackTrace e))"
       "                fmt (fn [xs] (->> xs (map (fn [[c f l]] (str (clojure.lang.Compiler/demunge c) \"  (\" f \":\" l \")\"))) distinct vec))"
       "                usr (remove (fn [[c _ _]] (re-find #\"^(java\\.|jdk\\.|sun\\.|nrepl\\.|clojure\\.lang\\.|clojure\\.main|clojure\\.core)\" c)) fs)]"
       "            (vec (take 16 (fmt (if (seq usr) usr fs)))))})"))

(defn- drain-to-done!
  "Realize the tail of an in-flight response seq up to ITS `done`, bounded by
   `deadline`. nREPL's session stream is a single ordered channel (not demuxed by
   message id), so a follow-up op would otherwise read the previous eval's
   leftover messages. Returns true once the stream is clean (done reached)."
  [responses deadline]
  (loop [rs responses]
    (cond
      (> (System/currentTimeMillis) deadline) false
      (empty? rs)                             true
      :else
      (let [s   (sget (first rs) "status")
            st  (cond (nil? s) #{} (string? s) #{s}
                      (coll? s) (set (map str s)) :else #{(str s)})]
        (if (contains? st "done") true (recur (next rs)))))))

(defn- fetch-stacktrace!
  "Guarded second round-trip on the SAME `session` (so `*e` is bound) to enrich an
   eval error with a structured message + trace. First DRAINS the errored eval's
   `responses` to done — combine may have early-bailed on the terminal-error line,
   and the shared session stream must be clean before another op. Prefers
   cider-nrepl's stacktrace op (rich `flags` per frame); falls back to a portable
   `*e` self-fetch when that middleware is absent. Never throws — returns nil on
   any failure so the base result is untouched."
  [session responses]
  (let [deadline (+ (System/currentTimeMillis) 3000)]
    (try
      (when (drain-to-done! responses deadline)
        (or
         (some (fn [op]
                 (let [{:keys [status msgs]} (collect-op session op deadline)]
                   (when (and (seq msgs) (not (contains? status "unknown-op")))
                     (stacktrace->result msgs))))
               ["stacktrace" "analyze-last-stacktrace"])
         (let [r (combine (session {:op "eval" :code e-fetch-code}) deadline)
               v (:value r)]
           (when (and (string? v) (not= "nil" v))
             (let [m (try (edn/read-string v) (catch Throwable _ nil))]
               (when (map? m) (not-empty m)))))))
      (catch Throwable _ nil))))

(defn eval!
  "Evaluate `code` in the nREPL at `host:port`. Opts:
     :host        defaults to \"localhost\"
     :ns          starting namespace, e.g. \"user\"
     :timeout-ms  default 30000
     :pretty?     when true, ask nREPL's print middleware to pretty-print the
                  value(s) SERVER-SIDE via `nrepl.util.print/pprint` — so the
                  live object is formatted where it lives (handles unreadable
                  objects / lazy seqs) and `:value`/`:values` come back as
                  multi-line, indented text. Output stays valid EDN.
     :print-margin  right-margin columns for pretty printing (default 100)

   Always returns a map (see ns docstring). Connection failures
   throw `:clj/nrepl-connect-failed`; everything else (eval error,
   timeout) is reported inside the returned map so the model can
   read it as data."
  [{:keys [host port code ns timeout-ms pretty? print-margin]
    :or   {host "localhost" timeout-ms 30000 print-margin 100}}]
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
                       (string? ns) (assoc :ns ns)
                       pretty?      (assoc :nrepl.middleware.print/print "nrepl.util.print/pprint"
                                           :nrepl.middleware.print/options {:right-margin print-margin}))
            responses (session req)
            combined (combine responses deadline)
            combined (if (eval-error? combined)
                       (merge combined (fetch-stacktrace! session responses))
                       combined)
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

;; ---------------------------------------------------------------------------
;; probe — cheap liveness check (no code execution)
;; ---------------------------------------------------------------------------

(defn- describe-versions
  "Pull a flat `{:clojure :clojurescript :nrepl :java}` version-string map out
   of an nREPL `describe` `:versions` reply. nREPL keys arrive as keywords or
   strings depending on transport wiring, so read both shapes. Returns nil
   when no recognisable version is present."
  [versions]
  (when (map? versions)
    (let [vget (fn [k] (or (get versions k) (get versions (name k))))
          vstr (fn [m] (when (map? m)
                         (or (get m :version-string) (get m "version-string"))))
          out  (into {}
                     (keep (fn [k] (when-let [s (vstr (vget k))] [k s])))
                     [:clojure :clojurescript :nrepl :java])]
      (not-empty out))))

(defn- detect-dialect
  "Best-effort clj vs cljs classification from describe `:versions` + `:ops`.
   JVM nREPLs report a `:clojure` version; cljs-capable servers (shadow-cljs,
   piggieback, self-hosted) surface a clojurescript version or cljs/shadow
   ops. Returns `:cljs`, `:clj`, or `:unknown`."
  [versions ops]
  (let [opset    (set (map name (cond
                                  (map? ops)  (keys ops)
                                  (coll? ops) ops
                                  :else       nil)))
        cljs-op? (boolean (some #(re-find #"(?i)cljs|shadow|piggieback" %) opset))]
    (cond
      (:clojurescript versions) :cljs
      cljs-op?                   :cljs
      (:clojure versions)        :clj
      :else                      :unknown)))

(defn- server-cwd
  "Best-effort working directory of the server JVM via a single
   `(System/getProperty \"user.dir\")` eval. Returns a path string or nil
   (e.g. a non-JVM cljs runtime where `System` is undefined). Never throws."
  [host port timeout-ms]
  (try
    (let [r (eval! {:host host :port port
                    :code "(System/getProperty \"user.dir\")"
                    :timeout-ms timeout-ms})
          v (:value r)]
      (when (string? v)
        (let [parsed (try (edn/read-string v) (catch Throwable _ nil))]
          (when (and (string? parsed) (seq parsed)) parsed))))
    (catch Throwable _ nil)))

(defn probe!
  "Best-effort liveness probe for the nREPL at `host:port`. Sends a single
   `describe` op (no code execution) under a SHORT timeout and classifies:

     {:status :up :versions {:clojure ..} :dialect :clj|:cljs :cwd \"/path\"}
                         — connected and the server answered describe.
     {:status :unresponsive}
                         — socket opened but no clean describe reply in budget.
     {:status :down}     — could not connect (stale `.nrepl-port`, dead proc).

   `:dialect` (clj vs cljs) is read from the describe metadata; `:cwd` is the
   server JVM's working directory via one tiny eval (nil when unavailable).
   Both are best-effort and only present when `:up`.

   Never throws. Reuses the cached connection (warming the same pool
   `eval!` uses). The short `:timeout-ms` (default 100) is passed to
   `nrepl/client`, which bounds each response read regardless of the
   cached connection's transport timeout."
  [{:keys [host port timeout-ms] :or {host "localhost" timeout-ms 100}}]
  (if-not (pos? (or port 0))
    {:status :down}
    (try
      (let [conn     (connection-for host port timeout-ms)
            client   (nrepl/client conn timeout-ms)
            session  (nrepl/client-session client)
            deadline (+ (System/currentTimeMillis) (long timeout-ms))
            responses (session {:op "describe"})
            up       (fn [versions ops]
                       (cond-> {:status   :up
                                :versions (or versions {})
                                :dialect  (detect-dialect (or versions {}) ops)}
                         true (as-> m
                                    (if-let [cwd (server-cwd host port timeout-ms)]
                                      (assoc m :cwd cwd)
                                      m))))]
        (loop [rs       responses
               versions nil
               ops      nil
               done?    false]
          (cond
            done?
            (up versions ops)

            (empty? rs)
            (if versions (up versions ops) {:status :unresponsive})

            (> (System/currentTimeMillis) deadline)
            (if versions (up versions ops) {:status :unresponsive})

            :else
            (let [msg (first rs)
                  mg  (fn [k] (or (get msg k) (get msg (keyword k))))
                  v   (describe-versions (mg "versions"))
                  o   (mg "ops")
                  s   (mg "status")
                  st  (cond
                        (nil? s)    #{}
                        (string? s) #{s}
                        (coll? s)   (set (map str s))
                        :else       #{(str s)})]
              (recur (next rs)
                     (or v versions)
                     (or o ops)
                     (contains? st "done"))))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :clj/nrepl-connect-failed (:type (ex-data e)))
          {:status :down}
          {:status :unresponsive}))
      (catch IOException _
        (evict! host port)
        {:status :down})
      (catch Throwable _
        {:status :unresponsive}))))
