(ns com.blockether.vis.ext.language-clojure.nrepl-client
  "Thin, observable nREPL client for `clj/eval`.

   Connection model:
     * One `nrepl.core/connect` socket per `[host port]` key, cached
       on a `defonce` atom so we survive `(require :reload)` during
       development.
     * ONE long-lived nREPL session per connection, cloned lazily on
       first use and cached beside the socket — then REUSED by every
       `eval!`. This is how Cider/Calva/every editor drives nREPL: no
       per-eval `clone`/`close` round-trip on the hot path (those were
       what blew the native-tool timeout budget under JVM load), nothing
       to leak, and session-local state — `*1`/`*2`/`*3`/`*e` and dynamic
       `set!`s — PERSISTS across calls like a real REPL (`(def …)` was
       already global; now the whole session is).
     * Stale / closed sockets are detected (`IOException` / `nil`
       message stream) and the entry is evicted — closing the socket and
       dropping the cached session — so the next call re-dials + re-clones.

   Returned shape (success) — STRING keys (crosses the strings-only boundary
   as a tool `:result`; enrichment adds \"error_message\"/\"error_data\"/\"trace\"):
     {\"value\"      \"42\"          ; pr-str of the LAST form's value, or nil
      \"values\"     [\"1\" \"42\"]   ; pr-str of every emitted value
      \"out\"        \"hello\\n\"     ; stdout aggregated
      \"err\"        \"\"             ; stderr aggregated
      \"ns\"         \"user\"         ; final *ns* name
      \"status\"     #{\"done\"}      ; nREPL status set (strings)
      \"ex\"         nil              ; exception class name, when status :ex
      \"root_ex\"    nil              ; root exception class name
      \"ms\"         12               ; wall-clock duration
      \"port\"       7888
      \"timed_out\" false}

   Failure paths throw `ex-info` with `:type :clj/nrepl-*` so the
   Vis tool wrapper can surface a clean error to the model."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [nrepl.core :as nrepl]
            [nrepl.transport :as transport])
  (:import (java.io IOException)
           (java.net InetSocketAddress Socket)))

;; ---------------------------------------------------------------------------
;; Connection cache
;; ---------------------------------------------------------------------------

(defonce ^:private connections
  ;; { [host port] -> {:conn <conn> :session <sid> :health-session <sid> :opened-at ms} }
  (atom {}))

(defn- key-of [host port] [(or host "localhost") (long port)])

(defonce ^:private conn-locks (atom {}))

(defn- conn-lock
  "Per-`[host port]` monitor that SERIALIZES all transport I/O on that socket.
   nREPL's bencode transport is a single ordered byte stream with NO per-message
   demux: two `nrepl/client` readers racing on one socket steal each other's
   replies — an `eval!` receives another op's value, or waits forever for a
   `done` a concurrent reader (a per-turn health probe, a `gather`ed eval)
   already consumed, then spuriously times out and evicts the connection. Every
   op that drives this connection's transport takes the lock so reads never
   interleave. A JVM monitor is REENTRANT, so nested drives on one thread
   (probe! -> server-cwd -> eval!) never self-deadlock."
  [host port]
  (let [k (key-of host port)]
    (or (get @conn-locks k) (get (swap! conn-locks update k #(or % (Object.))) k))))

(defn- open!
  "Open a fresh nREPL connection with a BOUNDED connect phase.

   `nrepl.core/connect` dials with the blocking `(Socket. host port)`
   constructor — no connect timeout (and its option map has no `:timeout`
   key at all, so passing one is a silent no-op). A SYN that is silently
   dropped instead of refused would hang the caller for the OS default
   (tens of seconds), blowing every deadline above. So we open the socket
   ourselves with an explicit `.connect` timeout and hand it to the bencode
   transport — exactly what `nrepl.core/connect` does internally, minus the
   unbounded dial. Read timeouts are enforced per-response by `nrepl/client`;
   eval deadlines separately via combined-response-fn.

   Wraps failures into a structured ex-info so callers can present a clean
   message."
  [host port timeout-ms]
  (try
    (let
      [^String h (or host "localhost")
       sock (doto (Socket.)
              (.connect (InetSocketAddress. h (int port))
                        (int (max 1 (long (or timeout-ms 1000))))))]
      (transport/bencode sock))
    (catch Throwable t
      (throw
        (ex-info
          (str
            "nREPL connect failed on "
            (or host "localhost")
            ":"
            port
            " — is the REPL running? Check session[\"resources\"][\"repls\"][\"clojure\"] or call repl_start(\"clojure\").")
          {:type :clj/nrepl-connect-failed
           :host (or host "localhost")
           :port port
           :cause (.getMessage t)})))))

(defn- evict!
  "Drop the cached connection (and its long-lived session) for `[host port]`,
   CLOSING the socket so the server promptly reaps the session's executor
   thread — nothing is left to leak. The next call re-dials and re-clones."
  [host port]
  (let
    [k
     (key-of host port)

     conn
     (get-in @connections [k :conn])]

    (swap! connections dissoc k)
    (when conn (try (.close ^java.io.Closeable conn) (catch Throwable _ nil)))))

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

(defn- session-id-for
  "Get (or lazily clone) the ONE long-lived nREPL session id for this
   connection, cached beside the socket. Cloned once via `new-session` on
   first use, then REUSED by every `eval!` so session-local state — `*1`/`*e`
   and dynamic `set!`s — persists, and so the hot path carries no per-eval
   `clone`/`close` round-trip (those round-trips, under JVM load, are what
   blew the native-tool timeout budget). Dropped when the connection evicts."
  [client host port]
  (let [k (key-of host port)]
    (or (get-in @connections [k :session])
        (let [sid (nrepl/new-session client)]
          (swap! connections assoc-in [k :session] sid)
          sid))))

(defn- health-session-id-for
  "Get (or lazily clone) a DEDICATED health-probe session for this connection,
   cached beside the socket and REUSED by every `health-check!`. Kept SEPARATE
   from the user `:session` so the per-turn `(+ 1 1)` liveness eval never
   clobbers the user session's `*1`/`*2`/`*3`. Dropped when the connection
   evicts."
  [client host port]
  (let [k (key-of host port)]
    (or (get-in @connections [k :health-session])
        (let [sid (nrepl/new-session client)]
          (swap! connections assoc-in [k :health-session] sid)
          sid))))

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
  (loop
    [rs
     responses

     values
     []

     out-acc
     (StringBuilder.)

     err-acc
     (StringBuilder.)

     ns*
     nil

     status
     #{}

     ex
     nil

     root-ex
     nil]

    (cond
      (> (System/currentTimeMillis) (long deadline)) {"timed_out" true
                                                      "value" (peek values)
                                                      "values" values
                                                      "out" (.toString out-acc)
                                                      "err" (.toString err-acc)
                                                      "ns" ns*
                                                      "status" (conj status "timeout")
                                                      "ex" ex
                                                      "root_ex" root-ex}
      ;; The response seq ended WITHOUT a `done`. nREPL only yields nil (ending
      ;; the seq) when the client's response-timeout elapses waiting for the next
      ;; message — or the socket dropped — so reaching here is a genuine eval
      ;; TIMEOUT, never a clean finish (a completed eval returns on `done` above).
      ;; Report it as one (matching the `> deadline` branch) so run_tests /
      ;; repl_eval surface an actionable timeout instead of an opaque nil `value`
      ;; that downstream reads as "could not parse test result".
      (empty? rs) {"timed_out" true
                   "value" (peek values)
                   "values" values
                   "out" (.toString out-acc)
                   "err" (.toString err-acc)
                   "ns" ns*
                   "status" (conj (vec status) "timeout")
                   "ex" ex
                   "root_ex" root-ex}
      :else
      ;; nREPL keys are *strings* over bencode; some clients keywordize.
      ;; Read both shapes so we don't silently lose value / status / out
      ;; depending on transport wiring. (This `mg` reads the nREPL WIRE, not
      ;; the strings-only Clojure->Python boundary — leave it dual-shape.)
      (let
        [msg
         (first rs)

         mg
         (fn [k]
           (or (get msg k) (get msg (keyword k))))

         v
         (mg "value")

         o
         (mg "out")

         e
         (mg "err")

         n
         (mg "ns")

         s
         (mg "status")

         ex2
         (mg "ex")

         rx2
         (mg "root-ex")]

        (when o (.append out-acc ^String o))
        (when e (.append err-acc ^String e))
        (let
          [new-status
           (into status
                 (cond (nil? s) []
                       (string? s) [s]
                       (coll? s) (map str s)
                       :else [(str s)]))

           done?
           (contains? new-status "done")

           terminal-error?
           (terminal-error-output? (.toString err-acc))

           eval-error?
           (contains? new-status "eval-error")

           values'
           (cond-> values
             v
             (conj v))

           ns''
           (or n ns*)

           ex''
           (or ex2 ex)

           rx''
           (or rx2 root-ex)]

          (if (or done? terminal-error? eval-error?)
            ;; Drain no further. Once "done" arrived the eval is complete; and the
            ;; moment nREPL reports an "eval-error" status (or prints a terminal
            ;; error on *err* without ever sending done) the eval has finished —
            ;; waiting on a straggling done just turns a real error into a
            ;; full-budget timeout. There is nothing to wait for once it errored.
            {"timed_out" false
             "value" (peek values')
             "values" values'
             "out" (.toString out-acc)
             "err" (.toString err-acc)
             "ns" ns''
             "status" (cond-> new-status
                        terminal-error?
                        (conj "eval-error"))
             "ex" ex''
             "root_ex" rx''}
            (recur (next rs) values' out-acc err-acc ns'' new-status ex'' rx'')))))))

;; ---------------------------------------------------------------------------
;; error enrichment — a beautiful, structured stacktrace on the eval-error path
;; ---------------------------------------------------------------------------

(defn- eval-error?
  "True when a combined eval result carries a runtime/eval error worth enriching.
   Reads the string-keyed combined map (post strings-only boundary shape)."
  [combined]
  (boolean (or (contains? (get combined "status") "eval-error")
               (get combined "ex")
               (get combined "root_ex"))))

(defn- sget
  "Read an nREPL response key that may arrive as a string or a keyword."
  [m k]
  (or (get m k) (get m (keyword k))))

(defn- frame-flags [f] (set (map str (sget f "flags"))))

(defn- cider-frame->line
  "`user/handle-request  (handler.clj:42)` from a cider-nrepl frame map."
  [f]
  (str (sget f "name")
       "  ("
       (sget f "file")
       (when-let [l (sget f "line")]
         (str ":" l))
       ")"))

(defn- pick-frames
  "Drop cider's `dup`/`tooling` noise frames (the same ones its own UI hides by
   default), keep the rest in order, cap the depth."
  [frames limit]
  (->> frames
       (remove #(let
                  [fl
                   (frame-flags %)]

                  (or (fl "dup") (fl "tooling"))))
       (map cider-frame->line)
       (take limit)
       vec))

(defn- simple-class
  [cls]
  (let
    [s
     (str cls)

     i
     (.lastIndexOf s ".")]

    (if (neg? i) s (subs s (inc i)))))

(defn- cause->headline
  [c]
  (let [msg (str (:message c))]
    (str (simple-class (:class c)) (when (seq msg) (str ": " msg)))))

(defn- stacktrace->result
  "Shape cider-nrepl cause messages into `{:error_message :error_data :trace}`.
   The root (innermost) cause carries the real throw site + message; earlier
   causes are threaded into the headline as a `← caused by` chain."
  [causes]
  (let
    [causes
     (vec causes)

     root
     (or (last (filter #(seq (sget % "stacktrace")) causes)) (last causes))

     heads
     (map #(cause->headline {:class (sget % "class") :message (sget % "message")}) causes)]

    {"error_message" (str/join "\n  ← caused by " heads)
     "error_data" (some-> (sget root "data")
                          not-empty)
     "trace" (pick-frames (sget root "stacktrace") 16)}))

(defn- collect-op
  "Drive an op on `session` to `done`, returning `{:status #{..} :msgs [..]}`.
   Bails on the deadline; never blocks past it."
  [session op deadline]
  (loop
    [rs
     (session {:op op})

     msgs
     []

     status
     #{}]

    (if (or (empty? rs) (> (System/currentTimeMillis) (long deadline)))
      {:status status :msgs msgs}
      (let
        [msg
         (first rs)

         s
         (sget msg "status")

         st
         (into status
               (cond (nil? s) []
                     (string? s) [s]
                     (coll? s) (map str s)
                     :else [(str s)]))

         msgs'
         (cond-> msgs
           (sget msg "class")
           (conj msg))]

        (if (contains? st "done") {:status st :msgs msgs'} (recur (next rs) msgs' st))))))

(def ^:private e-fetch-code
  "Portable fallback (no cider-nrepl): read `*e` in-session and hand back a
   readable `{\"error_message\" \"error_data\" \"trace\"}` map — demunged user
   frames, JVM/nREPL plumbing filtered out. STRING keys so the parsed map merges
   cleanly into the string-keyed eval result (crosses the strings-only boundary).
   Walks the WHOLE cause chain: a compile error's `*e` is a `CompilerException`
   wrapper whose bland `Syntax error compiling at …` message + all-`Compiler.java`
   frames hide the real fault (`Unable to resolve symbol …`) in its cause — so the
   headline threads every cause (`← caused by`) and the trace comes from the ROOT.
   A COMPILE error (phase `:read-source`/`:macroexpansion`/`:compile-syntax-check`/…)
   is headline-only: its `(line:col)` message + `← caused by` already name the fault
   and its location, so both `trace` AND `error_data` are dropped (the lone
   `user/eval…` frame + `#:clojure.error{…}` phase map are pure noise). A RUNTIME
   error keeps its USER frames + `ex-data` (an all-plumbing root yields an empty
   trace)."
  (str
    "(when-let [e *e]"
    "  (let [causes (take-while some? (iterate (fn [^Throwable t] (.getCause t)) e))"
    "        head (fn [^Throwable t] (str (.getSimpleName (class t)) (let [m (.getMessage t)] (when (seq (str m)) (str \": \" m)))))"
    "        root (last causes)"
    "        de (some (fn [t] (when (and (instance? clojure.lang.IExceptionInfo t) (seq (ex-data t))) t)) causes)"
    "        edata (when de (ex-data de))"
    "        compile? (contains? #{:read-source :macro-syntax-check :macroexpansion :compile-syntax-check :compilation} (:clojure.error/phase edata))"
    "        fs (map (fn [^StackTraceElement el]"
    "                  [(.getClassName el) (.getFileName el) (.getLineNumber el)])"
    "                (.getStackTrace ^Throwable root))"
    "        fmt (fn [xs] (->> xs (map (fn [[c f l]] (str (clojure.lang.Compiler/demunge c) \"  (\" f \":\" l \")\"))) distinct vec))"
    "        usr (remove (fn [[c _ _]] (re-find #\"^(java\\.|jdk\\.|sun\\.|nrepl\\.|clojure\\.lang\\.|clojure\\.main|clojure\\.core)\" c)) fs)]"
    "    {\"error_message\" (clojure.string/join \"\\n  ← caused by \" (map head causes))"
    "     \"error_data\" (when (and edata (not compile?)) (pr-str edata))"
    "     \"trace\" (if compile? [] (vec (take 16 (fmt usr))))}))"))

(defn- drain-to-done!
  "Realize the tail of an in-flight response seq up to ITS `done`, bounded by
   `deadline`. nREPL's session stream is a single ordered channel (not demuxed by
   message id), so a follow-up op would otherwise read the previous eval's
   leftover messages. Returns true once the stream is clean (done reached)."
  [responses deadline]
  (loop [rs responses]
    (cond (> (System/currentTimeMillis) (long deadline)) false
          (empty? rs) true
          :else (let
                  [s (sget (first rs) "status")
                   st (cond (nil? s) #{}
                            (string? s) #{s}
                            (coll? s) (set (map str s))
                            :else #{(str s)})]

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
    (try (when (drain-to-done! responses deadline)
           (or (some (fn [op]
                       (let [{:keys [status msgs]} (collect-op session op deadline)]
                         (when (and (seq msgs) (not (contains? status "unknown-op")))
                           (stacktrace->result msgs))))
                     ["stacktrace" "analyze-last-stacktrace"])
               (let
                 [r (combine (session {:op "eval" :code e-fetch-code}) deadline)
                  v (get r "value")]

                 (when (and (string? v) (not= "nil" v))
                   (let [m (try (edn/read-string v) (catch Throwable _ nil))]
                     (when (map? m) (not-empty m)))))))
         (catch Throwable _ nil))))

(defn- form-spans
  "Start positions of every TOP-LEVEL form in `code` as `[line column end-line]`
   (all 1-based), using only stdlib readers — no dependency. Skips whitespace,
   commas, and line comments before each form; stops at EOF or the first form
   that fails to read. Lets `error-location` name the failing top-level form by
   index (the count of nREPL `values` emitted before the error) even when the
   JVM stack carries no usable source position."
  [code]
  (let
    [rdr
     (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. code))

     eof
     (Object.)

     skip-lead!
     (fn []
       (loop []

         (let [ch (.read rdr)]
           (cond (= ch -1) nil
                 (Character/isWhitespace ch) (recur)
                 (= (char ch) \,) (recur)
                 (= (char ch) \;) (do (loop []

                                        (let [c (.read rdr)]
                                          (when (and (not= c -1) (not= (char c) \newline))
                                            (recur))))
                                      (recur))
                 :else (.unread rdr ch)))))]

    (loop [acc []]
      (skip-lead!)
      (let
        [line (.getLineNumber rdr)
         col (.getColumnNumber rdr)
         form (try (read {:eof eof :read-cond :allow} rdr) (catch Throwable _ eof))
         end-line (.getLineNumber rdr)]

        (if (identical? form eof) acc (recur (conj acc [line col end-line])))))))

(def ^:private synthetic-dispatch-re
  "Munged names Clojure gives synthetic protocol/interface dispatch fns: their
   compiled source line is the `defprotocol`, never the call site."
  #"\$fn\$G\b|\$G__|/G--|G__\d")

(defn- synthetic-dispatch?
  "True when the frame carrying the error's source position is a synthetic
   protocol-dispatch fn — its `(REPL:n)`/`NO_SOURCE_FILE:n` line points at the
   protocol definition, so it must NOT override the value-count form position."
  [combined]
  (let
    [err-frame
     (some #(when (re-find #"\(REPL:" %) %) (str/split-lines (str (get combined "err"))))

     top-trace
     (first (get combined "trace"))]

    (boolean (or (and err-frame (re-find synthetic-dispatch-re err-frame))
                 (and top-trace
                      (re-find #"NO_SOURCE_FILE" (str top-trace))
                      (re-find synthetic-dispatch-re (str top-trace)))))))

(defn- error-location
  "Best-effort [line column] (1-based) of the failing form WITHIN the evaluated
   `code`. Priority: the compiler `error_data` position
   (`:clojure.error/line`/`:column`); then the `err` headline's `(REPL:L[:C])`
   marker or the top `NO_SOURCE_FILE` trace frame — but ONLY when that frame is
   real code, not a synthetic protocol-dispatch fn; then, for failures the JVM
   leaves unlocated or misattributes to a synthetic frame (macro-expansion
   arity, protocol method on a non-implementing type), the START of the failing
   TOP-LEVEL form, named by counting the per-form `values` nREPL emitted before
   the error. nil when no in-code position is recoverable (deep library code)."
  [combined code]
  (let
    [ed
     (get combined "error_data")

     m
     (when (string? ed) (try (edn/read-string ed) (catch Throwable _ nil)))

     edl
     (when (and (map? m) (integer? (:clojure.error/line m)))
       [(:clojure.error/line m) (:clojure.error/column m)])

     [_ ml mc]
     (re-find #"\(REPL:(\d+)(?::(\d+))?\)" (str (get combined "err")))

     marker
     (when ml [(Long/parseLong ml) (when mc (Long/parseLong mc))])

     trace-line
     (some (fn [frame]
             (when-let [[_ l] (re-find #"NO_SOURCE_FILE:(\d+)" (str frame))]
               (Long/parseLong l)))
           (get combined "trace"))

     synthetic?
     (synthetic-dispatch? combined)

     spans
     (when (string? code) (form-spans code))

     idx
     (count (get combined "values"))

     [fl fc]
     (when (and spans (< idx (count spans))) (nth spans idx))]

    (cond edl edl
          (and marker (not synthetic?)) marker
          (and trace-line (not synthetic?)) [trace-line nil]
          fl [fl fc]
          marker marker
          trace-line [trace-line nil])))

(defn- error-headline
  "The human message line for the caret: clojure.main's `err` message line (the
   one AFTER the `Execution/Syntax error …` header), falling back to the tail of
   `error_message` (`SimpleClass: msg` → `msg`)."
  [combined]
  (let [ls (remove str/blank? (str/split-lines (str (get combined "err"))))]
    (or (second ls)
        (let
          [em (str (get combined "error_message"))
           i (str/index-of em ": ")]

          (if i (subs em (+ (long i) 2)) em)))))

(defn- render-context
  "babashka-style source Context: a ±2-line window of `code` around `line`, each
   row numbered, with a `^--- <message>` caret under the failing position
   (`column` when known, else the line's first non-blank char). nil when `line`
   is outside `code`."
  [code line column message]
  (let
    [lines
     ;; Detab (one space per tab) so 1 char == 1 display column: the caret
     ;; padding is space-based, and the compiler counts a tab as one column,
     ;; so this keeps both the reported column and the caret aligned.
     (mapv #(str/replace % "\t" " ") (str/split-lines code))

     n
     (count lines)]

    (when (and (integer? line) (pos? (long line)) (<= (long line) n))
      (let
        [lo
         (max 1 (- (long line) 2))

         hi
         (min n (+ (long line) 2))

         pad
         (count (str hi))

         src
         (nth lines (dec (long line)))

         col
         (or (when (and (integer? column) (pos? (long column))) column)
             (inc (count (take-while #(Character/isWhitespace ^char %) src))))

         rows
         (mapcat (fn [i]
                   (let [row (str (format (str "%" pad "d") i) ": " (nth lines (dec (long i))))]
                     (if (= i line)
                       [row (str (apply str (repeat (+ pad 2 (dec (long col))) \space)) "^--- " message)]
                       [row])))
                 (range lo (inc (long hi))))]

        (str/join "\n" rows)))))

(defn- error-context
  "babashka-style `context` string for an eval error: the failing source line(s)
   from `code` with a caret at the reported position. nil when no in-code
   location is recoverable, so the base result is untouched."
  [combined code]
  (when (string? code)
    (when-let [[line column] (error-location combined code)]
      (render-context code line column (error-headline combined)))))

(defn- interrupt!
  "Best-effort `interrupt` op on `session` so a TIMED-OUT eval's server-side
   thread is actually stopped rather than left running. An abandoned eval keeps
   the Clojure compile/RT lock, so the NEXT eval (a fresh cloned session) blocks
   on that lock and burns its whole budget too — the multi-minute cascade. With
   no `:interrupt-id` nREPL interrupts the session's currently-running eval,
   which is exactly the one we cloned this session for. Bounded and swallows
   everything so it is safe from the timeout path / a `finally`."
  [session]
  (try (collect-op session "interrupt" (+ (System/currentTimeMillis) 1000))
       (catch Throwable _ nil)))

(defn- close-session!
  "Send a `close` op so the nREPL server reaps this session's executor thread.
   nREPL sessions are NOT reclaimed when their socket closes — only an explicit
   `close` op (or a full server shutdown) drops one — so when we deliberately
   evict a connection after a TIMED-OUT eval we close its long-lived session
   here first; otherwise that single orphaned `nREPL-session-*` thread lingers
   on the server until it stops. NOT used on the happy path: a reused session is
   closed only when its connection is torn down. Best-effort and bounded."
  [session]
  (try (collect-op session "close" (+ (System/currentTimeMillis) 2000)) (catch Throwable _ nil)))

(def ^:private synthetic-eval-frame-re #"^[^\s]+/eval\d+(?:[/$][^\s]+)?(?:\s|$)")

(defn- visible-trace
  "Remove compiler-generated `ns/evalNNN` wrapper frames from a public trace.
   Named user functions remain visible. The raw trace is retained until Context
   location has been resolved because its NO_SOURCE_FILE line can locate the
   submitted form."
  [trace]
  (->> trace
       (remove #(re-find synthetic-eval-frame-re (str %)))
       vec))

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
    :or {host "localhost" timeout-ms 30000 print-margin 100}}]
  (when-not (pos? (long (or port 0)))
    (throw (ex-info "eval! requires a positive :port" {:type :clj/nrepl-bad-args :port port})))
  (when-not (string? code)
    (throw (ex-info "eval! requires a :code string" {:type :clj/nrepl-bad-args :code code})))
  (let
    [start
     (System/currentTimeMillis)

     deadline
     (+ start (long timeout-ms))]

    (letfn
      [(attempt []
         #_{:clj-kondo/ignore [:locking-suspicious-lock]}
         (locking (conn-lock host port)
           (let
             [conn
              (connection-for host port timeout-ms)

              client
              (nrepl/client conn timeout-ms)

              session
              (nrepl/client-session client :session (session-id-for client host port))

              req
              (cond-> {:op "eval" :code code}
                (string? ns)
                (assoc :ns ns)

                pretty?
                (assoc :nrepl.middleware.print/print
                  "nrepl.util.print/pprint" :nrepl.middleware.print/options
                  {:right-margin print-margin}))

              responses
              (session req)

              combined
              (combine responses deadline)

              combined
              (if (eval-error? combined)
                (let
                  [raw-enriched
                   (merge combined (fetch-stacktrace! session responses))

                   ;; Resolve source Context before hiding generated eval frames:
                   ;; their NO_SOURCE_FILE position can identify the submitted form.
                   ctx
                   (error-context raw-enriched code)

                   enriched
                   (update raw-enriched "trace" visible-trace)]

                  (cond-> enriched
                    ctx
                    (assoc "context" ctx)))
                combined)

              elapsed
              (- (System/currentTimeMillis) start)

              res
              (assoc combined
                "ms" elapsed
                "port" (int port)
                "host" host)]

             ;; A timed-out eval leaves a possibly-desynced keep-alive socket
             ;; (background reader parked, late messages pending). Evict it so the
             ;; NEXT eval reconnects + re-clones fresh instead of inheriting the
             ;; wedge — the cascade that historically stalled run_tests past its
             ;; budget. Interrupt the abandoned server-side eval first (frees the
             ;; compile/RT lock so the NEXT eval can't wedge on it), then CLOSE its
             ;; session — a socket close does NOT reap an nREPL session, so a dropped
             ;; one would otherwise linger as a parked thread until the server stops.
             (when (get res "timed_out")
               (interrupt! session)
               (close-session! session)
               (evict! host port))
             res)))]
      (try (try (attempt)
                (catch IOException _ioe
                  ;; A cached keep-alive socket the server has since reaped fails the FIRST
                  ;; write with an IOException. Drop the dead entry and reconnect ONCE with a
                  ;; fresh connection so a transient eviction self-heals here — instead of
                  ;; bubbling a "retry." error the caller has to notice and re-issue by hand
                  ;; (which stalled run_tests / repl_eval).
                  (evict! host port)
                  (attempt)))
           (catch IOException ioe
             ;; The reconnect also failed on I/O — the socket is genuinely down.
             (evict! host port)
             (throw (ex-info
                      (str "nREPL socket error on " host ":" port " — connection evicted, retry.")
                      {:type :clj/nrepl-io :host host :port port :cause (.getMessage ioe)})))
           (catch Throwable t
             (if (= :clj/nrepl-connect-failed (:type (ex-data t)))
               (throw t)
               (throw (ex-info (str "nREPL eval failed: " (.getMessage t))
                               {:type :clj/nrepl-eval-failed
                                :host host
                                :port port
                                :cause (.getMessage t)}))))))))

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
    (let
      [vget
       (fn [k]
         (or (get versions k) (get versions (name k))))

       vstr
       (fn [m]
         (when (map? m) (or (get m :version-string) (get m "version-string"))))

       out
       (into {}
             (keep (fn [k]
                     (when-let [s (vstr (vget k))]
                       [k s])))
             [:clojure :clojurescript :nrepl :java])]

      (not-empty out))))

(defn- detect-dialect
  "Best-effort clj vs cljs classification from describe `:versions` + `:ops`.
   JVM nREPLs report a `:clojure` version; cljs-capable servers (shadow-cljs,
   piggieback, self-hosted) surface a clojurescript version or cljs/shadow
   ops. Returns `:cljs`, `:clj`, or `:unknown`."
  [versions ops]
  (let
    [opset
     (set (map name
               (cond (map? ops) (keys ops)
                     (coll? ops) ops
                     :else nil)))

     cljs-op?
     (boolean (some #(re-find #"(?i)cljs|shadow|piggieback" %) opset))]

    (cond (:clojurescript versions) :cljs
          cljs-op? :cljs
          (:clojure versions) :clj
          :else :unknown)))

(defn- server-cwd
  "Best-effort working directory of the server JVM via a single
   `(System/getProperty \"user.dir\")` eval. Returns a path string or nil
   (e.g. a non-JVM cljs runtime where `System` is undefined). Never throws."
  [host port timeout-ms]
  (try
    (let
      [r
       (eval!
         {:host host :port port :code "(System/getProperty \"user.dir\")" :timeout-ms timeout-ms})

       v
       (get r "value")]

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
  (if-not (pos? (long (or port 0)))
    {:status :down}
    (try #_{:clj-kondo/ignore [:locking-suspicious-lock]}
         (locking (conn-lock host port)
           (let
             [conn
              (connection-for host port timeout-ms)

              client
              (nrepl/client conn timeout-ms)

              deadline
              (+ (System/currentTimeMillis) (long timeout-ms))

              responses
              (nrepl/message client {:op "describe"})

              up
              (fn [versions ops]
                (cond->
                  {:status :up
                   :versions (or versions {})
                   :dialect (detect-dialect (or versions {}) ops)}
                  true
                  (as-> m (if-let [cwd (server-cwd host port timeout-ms)]
                            (assoc m :cwd cwd)
                            m))))]

             (loop
               [rs
                responses

                versions
                nil

                ops
                nil

                done?
                false]

               (cond done? (up versions ops)
                     (empty? rs) (if versions (up versions ops) {:status :unresponsive})
                     (> (System/currentTimeMillis) deadline)
                     (if versions (up versions ops) {:status :unresponsive})
                     :else (let
                             [msg
                              (first rs)

                              mg
                              (fn [k]
                                (or (get msg k) (get msg (keyword k))))

                              v
                              (describe-versions (mg "versions"))

                              o
                              (mg "ops")

                              s
                              (mg "status")

                              st
                              (cond (nil? s) #{}
                                    (string? s) #{s}
                                    (coll? s) (set (map str s))
                                    :else #{(str s)})]

                             (recur (next rs) (or v versions) (or o ops) (contains? st "done")))))))
         (catch clojure.lang.ExceptionInfo e
           (if (= :clj/nrepl-connect-failed (:type (ex-data e)))
             {:status :down}
             {:status :unresponsive}))
         (catch IOException _ (evict! host port) {:status :down})
         (catch Throwable _ {:status :unresponsive}))))

(def health-form
  "The canonical liveness eval: cheap, pure, and an unmistakable `\"2\"` result."
  "(+ 1 1)")

(defn health-check!
  "REAL eval-based health check — the strong signal `probe!`'s `describe` can't
   give. Runs `(+ 1 1)` through a DEDICATED health session on the cached
   connection under a SHORT timeout and confirms it returns `\"2\"`. `describe`
   only proves the accept loop answers; this proves the JVM actually EVALUATES
   code, so a wedged eval executor / blocked compile lock is caught even while
   `describe` still replies. Returns:

     {:status :up           :ms N}
       — evaluated `\"2\"` cleanly; the eval path is healthy.
     {:status :unresponsive :form \"(+ 1 1)\" :ms N :hint \"...\"}
       — connected but the eval TIMED OUT or didn't return `\"2\"`: the REPL is
         wedged and should be killed & restarted (or reprobed).
     {:status :down         :form \"(+ 1 1)\"}
       — could not connect (stale `.nrepl-port` / dead process).

   Uses a SEPARATE session so it never clobbers the user session's
   `*1`/`*2`/`*3`. Never throws. On a hard socket error the connection is evicted
   so the next call reconnects fresh."
  [{:keys [host port timeout-ms] :or {host "localhost" timeout-ms 2000}}]
  (if-not (pos? (long (or port 0)))
    {:status :down :form health-form}
    (let
      [start
       (System/currentTimeMillis)

       deadline
       (+ start (long timeout-ms))]

      (try
        #_{:clj-kondo/ignore [:locking-suspicious-lock]}
        (locking (conn-lock host port)
          (let
            [conn
             (connection-for host port timeout-ms)

             client
             (nrepl/client conn timeout-ms)

             session
             (nrepl/client-session client :session (health-session-id-for client host port))

             responses
             (session {:op "eval" :code health-form})

             combined
             (combine responses deadline)

             ms
             (- (System/currentTimeMillis) start)]

            (cond
              (get combined "timed_out")
              {:status :unresponsive
               :form health-form
               :ms ms
               :hint
               (str "nREPL eval timed out after "
                    timeout-ms
                    "ms on "
                    health-form
                    " — the REPL is UNRESPONSIVE. Kill & restart it (F4 / repl_start) or reprobe.")}
              (= "2" (str/trim (str (get combined "value")))) {:status :up :ms ms}
              :else
              {:status :unresponsive
               :form health-form
               :ms ms
               :hint
               (str
                 "nREPL returned "
                 (pr-str (get combined "value"))
                 " for "
                 health-form
                 " (expected \"2\") — the REPL is UNHEALTHY. Kill & restart it (F4 / repl_start).")})))
        (catch clojure.lang.ExceptionInfo e
          (if (= :clj/nrepl-connect-failed (:type (ex-data e)))
            {:status :down :form health-form}
            {:status :unresponsive
             :form health-form
             :hint (str "nREPL connection error on "
                        health-form
                        " — the REPL is UNRESPONSIVE. Kill & restart it (F4 / repl_start).")}))
        (catch IOException _ (evict! host port) {:status :down :form health-form})
        (catch Throwable _
          {:status :unresponsive
           :form health-form
           :hint (str "nREPL health check failed unexpectedly on "
                      health-form
                      " — the REPL may be UNRESPONSIVE. Kill & restart it (F4 / repl_start).")})))))
