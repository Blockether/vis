(ns com.blockether.vis.internal.credential-command
  "Command-backed provider credentials — the `api_key_command` config key.

   A static `api_key` (or a `${NAME}` reference to one) is only good for a
   long-lived secret. Short-lived SSO/gateway tokens come from a credential
   HELPER instead: a small program that prints a fresh token on stdout. This
   namespace runs that helper for a provider and hands the trimmed stdout back
   as the API key.

   Three contracts hold this together:

     - **No shell, ever.** The configured value is a structured argv that is
       passed to `ProcessBuilder` verbatim. It is never joined, never split on
       whitespace, and never handed to `sh -c`, so a token containing shell
       metacharacters — or a config written by someone else — cannot become
       command injection.
     - **The credential is write-once, in memory.** Resolved stdout is returned
       to the caller and cached HERE. It is never persisted (nothing writes it
       back into `:api-key`), never logged, and never placed in an error
       message. Every diagnostic this namespace produces is built from argv[0],
       the exit code, and the helper's stderr — never its stdout.
     - **Bounded and single-flight.** One helper invocation per provider at a
       time, bounded by `timeout-ms`, with a successful token cached for
       `success-ttl-ms` and a failure remembered for `failure-ttl-ms`. A
       long-running gateway must not fork a token helper per turn, and an
       interactive helper must never be launched twice concurrently.

   `resolve!` never throws: callers use its `:error` to render a provider as
   unavailable (`providers/provider-status`, `doctor`) or to drop it from the
   router build, exactly as an unresolved `${NAME}` is handled today."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [taoensso.telemere :as tel])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent TimeUnit)))

(def ^:private timeout-ms
  "Upper bound on ONE helper invocation. Matched to config's boot token timeout:
   `->svar-provider` resolves credentials while the router builds, and the router
   builds on the startup path, so a hung helper must not stall first paint."
  15000)

(def ^:private success-ttl-ms
  "How long a resolved token is reused before the helper runs again. Short-lived
   tokens usually live far longer than this; the point is to bound helper forks
   on a long-running gateway, not to track real expiry. A 401 invalidates the
   entry immediately via `invalidate!`, which is what actually drives refresh."
  300000)

(def ^:private failure-ttl-ms
  "How long a FAILURE is remembered. Non-zero so a broken/missing helper is not
   re-forked on every status read, frame, or routing decision; short so fixing
   the helper (installing it, logging in) takes effect without a restart."
  10000)

(def ^:private max-output-bytes
  "Cap on captured stdout/stderr. A helper that streams unboundedly must not be
   able to exhaust this process' heap."
  65536)

(def ^:private stderr-excerpt-chars
  "Cap on how much helper stderr is quoted into a diagnostic."
  200)

(defn argv
  "Normalize an `api_key_command` config value into a non-empty argv vector, or
   nil when it is absent or malformed.

   A bare string is ONE argv element — it is deliberately NOT shell-split. The
   whole point of the structured form is that arguments are unambiguous, and
   silently splitting `foo --env 'a b'` would reintroduce exactly the quoting
   guesswork this key exists to avoid. Every element must be a non-blank string;
   anything else makes the value invalid rather than partially usable."
  [v]
  (let
    [parts (cond (string? v) [v]
                 (sequential? v) (vec v)
                 :else nil)]
    (when (and (seq parts) (every? #(and (string? %) (not (str/blank? %))) parts)) parts)))

(defn- read-stream
  "Drain `is` fully (so the child never blocks on a full pipe) but retain at most
   `max-output-bytes` of it."
  ^String [^InputStream is]
  (let
    [buf
     (byte-array 8192)

     out
     (ByteArrayOutputStream.)]

    (loop [kept 0]
      (let [n (.read is buf)]
        (if (neg? n)
          (String. (.toByteArray out) StandardCharsets/UTF_8)
          (let
            [room (- (long max-output-bytes) kept)
             take-n (min n room)]

            (when (pos? take-n) (.write out buf 0 take-n))
            (recur (+ kept take-n))))))))

(defn- stderr-excerpt
  "The helper's own error text, first non-blank line, length-capped, for a
   diagnostic. stderr is the helper's DIAGNOSTIC channel; the credential travels
   on stdout and is never read here."
  [err]
  (when-let
    [line (some->> (str/split-lines (or err ""))
                   (map str/trim)
                   (remove str/blank?)
                   first)]
    (let
      [trimmed (if (> (count line) (long stderr-excerpt-chars))
                 (str (subs line 0 stderr-excerpt-chars) "…")
                 line)]
      (str ": " trimmed))))

(defn- exec-argv
  "Run `av` with NO shell and return `{:token trimmed-stdout}` or
   `{:error non-secret-message}`. Never throws."
  [av]
  (let [exe (first av)]
    (try (let [proc (.start (ProcessBuilder. ^java.util.List av))]
           ;; A helper that reads stdin (a prompt, a pipe check) must see EOF
           ;; instead of blocking forever against a pipe nobody writes to.
           (.close (.getOutputStream proc))
           (let
             [out-f (future (read-stream (.getInputStream proc)))
              err-f (future (read-stream (.getErrorStream proc)))]

             (if-not (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)
               (do (.destroyForcibly proc)
                   (future-cancel out-f)
                   (future-cancel err-f)
                   {:error (str "`" exe "` timed out after " timeout-ms "ms")})
               (let
                 [exit (.exitValue proc)
                  out (str/trim (str (deref out-f 2000 "")))
                  err (str (deref err-f 2000 ""))]

                 (cond (not (zero? exit)) {:error
                                           (str "`" exe "` exited " exit (stderr-excerpt err))}
                       ;; An empty stdout is a FAILURE, not an empty credential: a
                       ;; blank api-key would otherwise sail on and surface as an
                       ;; unexplained 401 on the first real turn.
                       (str/blank? out) {:error (str "`" exe "` produced no credential on stdout")}
                       :else {:token out})))))
         (catch java.io.IOException e
           ;; `ProcessBuilder.start` throws this when the executable is absent or
           ;; not executable — by far the most common real-world failure.
           {:error (str "cannot run `" exe "`: " (ex-message e))})
         (catch Throwable t
           (cancellation/preserve-interrupt! t)
           {:error (str "`" exe "` failed: " (ex-message t))}))))

(defonce ^:private cache
  ;; provider-id -> {:token|:error _ :at epoch-ms :argv av}
  ;; Holds a live credential: it is read back by `resolve!` only, and no writer
  ;; in this process copies it onto a provider map that could reach disk.
  (atom {}))

(defonce ^:private locks
  ;; provider-id -> monitor object. Single-flight is PER PROVIDER so an
  ;; interactive helper (a vault/SSO token printer, `op read`) can never be
  ;; concurrently, while two different providers still resolve in parallel.
  (atom {}))

(defn- lock-for
  ^Object [pid]
  (or (get @locks pid) (get (swap! locks update pid #(or % (Object.))) pid)))

(defn- fresh-entry
  "The cached entry for `pid` when it is still valid for argv `av`, else nil. A
   changed argv (an edited config, a different `--env`) invalidates by identity."
  [pid av]
  (let [{:keys [at token] :as e} (get @cache pid)]
    (when (and e
               (= av (:argv e))
               (< (- (System/currentTimeMillis) (long at))
                  (long (if token success-ttl-ms failure-ttl-ms))))
      e)))

(defn peek-token
  "Cached result for `pid` WITHOUT running anything: `{:token _}`, `{:error _}`,
   or nil when nothing valid is cached.

   This is the read for synchronous UI paths (`initial-provider-status`), which
   must never fork a subprocess to paint a frame — an unknown verdict there
   correctly renders as `loading`."
  [pid v]
  (when-let [av (argv v)]
    (some-> (fresh-entry pid av)
            (select-keys [:token :error]))))

(defn resolve!
  "Resolve provider `pid`'s credential from config value `v`, cached and
   single-flight. Returns `{:token s}` or `{:error non-secret-message}`; returns
   nil when `v` configures no command at all. Never throws.

   BLOCKS for up to `timeout-ms` on a cold miss, so callers on a paint path want
   `peek-token` instead."
  [pid v]
  (if-let [av (argv v)]
    (let [lock (lock-for pid)]
      (or (some-> (fresh-entry pid av)
                  (select-keys [:token :error]))
          (locking lock
            ;; Re-check inside the monitor: the thread that waited here may have
            ;; been waiting for exactly the invocation that just populated it.
            (or (some-> (fresh-entry pid av)
                        (select-keys [:token :error]))
                (let [res (exec-argv av)]
                  (swap! cache assoc
                    pid
                    (assoc res
                      :at (System/currentTimeMillis)
                      :argv av))
                  (when-let [err (:error res)]
                    (tel/log! {:level :warn
                               :id ::credential-command-failed
                               :data {:provider pid :error err}}
                              (str "provider " pid " credential command failed: " err)))
                  res)))))
    (when (some? v) {:error "api_key_command must be a non-empty list of non-blank strings"})))

(defn invalidate!
  "Forget `pid`'s cached credential so the next `resolve!` re-runs the helper.

   This is the refresh trigger: a 401 means the short-lived token the router
   baked in has expired, and the only way to learn the new one is to ask the
   helper again. Dropping the entry (rather than re-running here) keeps refresh
   lazy, single-flight, and on the thread that actually needs the token."
  [pid]
  (swap! cache dissoc pid)
  nil)

(defn reset-cache!
  "Drop every cached credential. For `/reload` and for tests."
  []
  (reset! cache {})
  nil)
