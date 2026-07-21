(ns com.blockether.vis.internal.egress-proxy
  "Gateway-hosted loopback EGRESS PROXY — the one door a jailed shell child may use
   to reach the network. Paired with the OS jail's *net-off-except-loopback* wall
   (`process-jail`), it turns vis.yml `:network` (allowed/denied domains + verb/path
   `:rules`) into REAL enforcement for shell children — `curl`, `wget`, a script,
   `subprocess` — not a cooperative env-var hint.

   How the wall + door fit together:
     - The jail denies ALL sockets except TCP to `127.0.0.1:<this-port>`, so a raw
       `socket()` / `/dev/tcp` in a script has nowhere to go — the kernel, not
       politeness, forces every byte through here.
     - `http_proxy`/`https_proxy`/`ALL_PROXY` point the child's HTTP clients at this
       port, so ordinary tools proxy voluntarily; the wall covers the rest.

   What it enforces (no MITM — no new deps, no CA):
     - Plain HTTP  (absolute-form proxy request): FULL host + method + path, because
       the request line is cleartext. GET-not-POST works here.
     - HTTPS (`CONNECT host:443`): HOST allow/deny only — method + path are inside the
       TLS the proxy does not terminate. That verb fidelity is the documented ceiling;
       closing it needs a TLS-terminating (MITM) proxy + an injected CA (future).

   The policy is a plain VALUE (per session), read fresh per connection via `policy-fn`
   so `/reload` + config edits take effect with no restart. One request per upstream
   connection (we force `Connection: close`), so HTTP keep-alive can't smuggle a second,
   unfiltered verb onto an already-approved socket."
  (:require [clojure.string :as str])
  (:import (java.io InputStream OutputStream)
           (java.net InetAddress InetSocketAddress ServerSocket Socket URI)
           (java.util.concurrent Executors ExecutorService ThreadFactory)
           (java.util.concurrent.atomic AtomicLong)))

;; ============================================================================
;; Policy: normalize vis.yml :network into a matcher, then decide per request.
;; Mirrors env_python/normalize-network-rules (the interpreter method-guard) so the
;; shell proxy and the Python guard read ONE config with the SAME semantics.
;; ============================================================================

(defn- nm
  "Trim a keyword/string to a non-blank string, or nil."
  [x]
  (some-> (if (keyword? x) (name x) (str x))
          str/trim
          not-empty))

(defn- host-key
  "Canonical host: lower-cased, trailing dots stripped."
  [x]
  (some-> (nm x)
          str/lower-case
          (str/replace #"\.+$" "")))

(defn- access->methods
  "Expand an `:access` preset to a method set (upper-cased)."
  [a]
  (case
    (some-> (nm a)
            str/lower-case)
    ("read-only" "readonly" "ro")
    #{"GET" "HEAD" "OPTIONS"}

    ("read-write" "readwrite" "rw" "full" "all")
    #{"*"}

    ("none" "deny" "closed")
    #{}

    #{}))

(defn- methods-of
  [ms]
  (into #{}
        (keep #(some-> (nm %)
                       str/upper-case))
        (if (coll? ms) ms [ms])))

(defn- allow-of
  [al]
  (vec (keep (fn [a]
               (when-let
                 [m (some-> (nm (:method a))
                            str/upper-case)]
                 {"method" m "path" (or (nm (:path a)) "*")}))
             al)))

(defn normalize-rules
  "Normalize the `:rules` list into a vector of
   `{\"host\" h \"methods\" #{UPPER…} \"allow\" [{\"method\" M \"path\" P}…]}`."
  [{:keys [rules]}]
  (let
    [add
     (fn [m h ms al]
       (cond-> m
         (and h (or (seq ms) (seq al)))
         (update h
                 (fn [e]
                   (-> (or e {:methods #{} :allow []})
                       (update :methods into ms)
                       (update :allow into al))))))

     acc
     (reduce (fn [m r]
               (add m
                    (host-key (:host r))
                    (into (access->methods (:access r)) (methods-of (:methods r)))
                    (allow-of (:allow r))))
             acc
             rules)]

    (mapv (fn [[h {:keys [methods allow]}]]
            {"host" h "methods" methods "allow" allow})
          acc)))

(defn compile-policy
  "Compile raw network-opts `{:allowed-domains :denied-domains :rules}`
   into the value the proxy consults: canonical domain lists + normalized rules. Returns
   nil when there is nothing to enforce (no domain restriction and no rules) — the caller
   then need not route through the proxy at all."
  [{:keys [allowed-domains denied-domains] :as net}]
  (let
    [allowed
     (vec (keep host-key allowed-domains))

     denied
     (vec (keep host-key denied-domains))

     rules
     (normalize-rules net)

     restrict-domains?
     (or (seq denied) (and (seq allowed) (not (some #{"*"} allowed))))]

    (when (or restrict-domains? (seq rules))
      {:allowed-domains allowed :denied-domains denied :rules rules})))

(defn- host-matches?
  "Does domain pattern `p` match host `h`? `*` = any; `*.x` = x + subdomains; a bare
   `x` = x + its subdomains (suffix match)."
  [p h]
  (let
    [p
     (host-key p)

     h
     (host-key h)]

    (cond (nil? p) false
          (= p "*") true
          (str/starts-with? p "*.") (let [base (subs p 2)]
                                      (or (= h base) (str/ends-with? h (str "." base))))
          :else (or (= h p) (str/ends-with? h (str "." p))))))

(defn- host-ok?
  "Host-level allow/deny (denied wins; empty/`*` allow-list ⇒ all)."
  [{:keys [allowed-domains denied-domains]} host]
  (cond (some #(host-matches? % host) denied-domains) false
        (or (empty? allowed-domains) (some #{"*"} allowed-domains)) true
        :else (boolean (some #(host-matches? % host) allowed-domains))))

(defn- glob->re
  "fnmatch-ish glob → regex; `*`/`**` match anything (incl. `/`), `?` one char."
  [g]
  (let [g (str g)]
    (re-pattern (str "^"
                     (-> g
                         (str/replace #"[.\\+^$(){}\[\]|]" "\\\\$0")
                         (str/replace "?" ".")
                         (str/replace "*" ".*"))
                     "$"))))

(defn- path-matches?
  [pat path]
  (let [pat (or pat "*")]
    (or (= pat "*") (= pat "**") (boolean (re-matches (glob->re pat) (str path))))))

(defn- rule-for-host
  "The most specific rule whose host matches (longest host string wins, so an exact
   host beats a `*` default)."
  [rules host]
  (->> rules
       (filter #(host-matches? (get % "host") host))
       (sort-by #(count (str (get % "host"))) >)
       first))

(defn- method-path-ok?
  [rule method path]
  (let
    [methods
     (get rule "methods")

     allow
     (get rule "allow")]

    (or (contains? methods "*")
        (contains? methods method)
        (boolean (some (fn [a]
                         (and (or (= (get a "method") "*") (= (get a "method") method))
                              (path-matches? (get a "path") path)))
                       allow)))))

(defn decide
  "Decide a request. `method`/`path` nil ⇒ HTTPS CONNECT (host-only). Returns
   `{:allow? bool :reason str}`."
  [policy method host path]
  (cond (nil? policy) {:allow? true}
        (not (host-ok? policy host)) {:allow? false :reason (str "host not permitted: " host)}
        (nil? method) {:allow? true} ; CONNECT: host-only
        :else (let
                [m
                 (str/upper-case (str method))

                 rule
                 (rule-for-host (:rules policy) host)]

                (if (or (nil? rule) (method-path-ok? rule m path))
                  {:allow? true}
                  {:allow? false
                   :reason (str "method " m " " path " not allowed for host " host)}))))

;; ============================================================================
;; Wire protocol helpers
;; ============================================================================

(defn- read-line-bytes
  "Read one CRLF-terminated line from `in` as a String (sans CRLF). nil at EOF."
  [^InputStream in]
  (let [sb (StringBuilder.)]
    (loop [prev -1]
      (let [b (.read in)]
        (cond (neg? b) (when (pos? (.length sb)) (.toString sb))
              (and (= b 10) (= prev 13)) (do (.setLength sb (max 0 (dec (.length sb))))
                                             (.toString sb))
              :else (do (.append sb (char b)) (recur b)))))))

(defn- read-headers
  "Read header lines up to (and consuming) the blank line. Returns a vector of raw
   header strings (order preserved)."
  [^InputStream in]
  (loop [acc []]
    (let [l (read-line-bytes in)]
      (if (or (nil? l) (= l "")) acc (recur (conj acc l))))))

(defn- header-name [h] (str/lower-case (str/trim (first (str/split h #":" 2)))))


(defn- splice
  "Full-duplex byte relay between two sockets on `pool`; returns when either side EOFs."
  [^ExecutorService pool ^Socket a ^Socket b]
  (let
    [copy
     (fn [^Socket from ^Socket to]
       (try (let
              [in
               (.getInputStream from)

               out
               (.getOutputStream to)

               buf
               (byte-array 16384)]

              (loop []

                (let [n (.read in buf)]
                  (when (pos? n) (.write out buf 0 n) (.flush out) (recur)))))
            (catch Throwable _ nil)
            (finally (try (.shutdownOutput to) (catch Throwable _ nil)))))

     f
     (.submit pool
              ^Runnable
              (fn []
                (copy a b)))]

    (copy b a)
    (try (.get f) (catch Throwable _ nil))))

(defn- write-str [^OutputStream out ^String s] (.write out (.getBytes s "ISO-8859-1")) (.flush out))

(defn- deny-response
  [^OutputStream out reason]
  (write-str out
             (str "HTTP/1.1 403 Forbidden\r\n"
                  "Content-Type: text/plain\r\n"
                  "Connection: close\r\n"
                  "Content-Length: "
                  (count (str reason "\n"))
                  "\r\n"
                  "\r\n"
                  reason
                  "\n")))

;; ============================================================================
;; Connection handling
;; ============================================================================

(defn- parse-request-line
  "\"METHOD TARGET VERSION\" → [method target version] (or nil)."
  [line]
  (let [parts (str/split (str line) #"\s+")]
    (when (= 3 (count parts)) parts)))

(defn- handle-connect
  "HTTPS CONNECT host:port — host-level policy, then a raw byte tunnel (verb opaque)."
  [^ExecutorService pool ^Socket client ^OutputStream cout target policy on-log]
  (let
    [[host port-s]
     (str/split (str target) #":" 2)

     port
     (try (Integer/parseInt (str/trim (or port-s "443"))) (catch Exception _ 443))

     {:keys [allow? reason]}
     (decide policy nil host nil)]

    (if-not allow?
      (do (on-log {:phase :connect :host host :allow? false :reason reason})
          (deny-response cout reason))
      (let [upstream (Socket.)]
        (on-log {:phase :connect :host host :port port :allow? true})
        (try (.connect upstream (InetSocketAddress. ^String host (int port)) 15000)
             (write-str cout "HTTP/1.1 200 Connection Established\r\n\r\n")
             (splice pool client upstream)
             (catch Throwable t
               (try (deny-response cout (str "upstream error: " (.getMessage t)))
                    (catch Throwable _ nil)))
             (finally (try (.close upstream) (catch Throwable _ nil))))))))

(defn- handle-http
  "Plain HTTP absolute-form proxy request — full host+method+path, then forward the one
   request (Connection: close) and relay the response."
  [^ExecutorService pool ^Socket client ^OutputStream cout method target version headers policy
   on-log]
  (let
    [uri
     (try (URI. (str target)) (catch Throwable _ nil))

     host
     (some-> ^URI uri
             .getHost)

     port
     (let [p (int (if uri (.getPort ^URI uri) -1))]
       (if (pos? p) p 80))

     path
     (let
       [p
        (.getRawPath ^URI uri)

        q
        (.getRawQuery ^URI uri)]

       (str (if (str/blank? p) "/" p) (when q (str "?" q))))

     {:keys [allow? reason]}
     (decide policy method host path)]

    (cond (nil? host) (deny-response cout "malformed proxy request")
          (not allow?)
          (do (on-log
                {:phase :http :method method :host host :path path :allow? false :reason reason})
              (deny-response cout reason))
          :else (let [upstream (Socket.)]
                  (on-log {:phase :http :method method :host host :path path :allow? true})
                  (try (.connect upstream (InetSocketAddress. ^String host (int port)) 15000)
                       (let
                         [uout (.getOutputStream upstream)
                          ;; origin-form request line + headers, minus hop-by-hop/proxy headers,
                          ;; forcing single-request semantics so keep-alive can't smuggle a verb.
                          kept (remove (fn [h]
                                         (#{"proxy-connection" "connection" "keep-alive"
                                            "proxy-authorization"}
                                          (header-name h)))
                                 headers)
                          req (str method
                                   " "
                                   path
                                   " "
                                   version
                                   "\r\n"
                                   (str/join "\r\n" kept)
                                   (when (seq kept) "\r\n")
                                   "Connection: close\r\n\r\n")]

                         (write-str uout req)
                         ;; body (client→upstream) + response (upstream→client), then done.
                         (splice pool client upstream))
                       (catch Throwable t
                         (try (deny-response cout (str "upstream error: " (.getMessage t)))
                              (catch Throwable _ nil)))
                       (finally (try (.close upstream) (catch Throwable _ nil))))))))

(defn- handle-client
  [^ExecutorService pool ^Socket client policy-fn on-log]
  (try (.setSoTimeout client 30000)
       (let
         [cin
          (.getInputStream client)

          cout
          (.getOutputStream client)

          line
          (read-line-bytes cin)

          rl
          (parse-request-line line)

          policy
          (try (policy-fn) (catch Throwable _ nil))]

         (if-not rl
           nil
           (let [[method target version] rl]
             (if (= (str/upper-case method) "CONNECT")
               (do (read-headers cin) ; consume CONNECT headers
                   (handle-connect pool client cout target policy on-log))
               (let [headers (read-headers cin)]
                 (handle-http pool client cout method target version headers policy on-log))))))
       (catch Throwable _ nil)
       (finally (try (.close client) (catch Throwable _ nil)))))

;; ============================================================================
;; Lifecycle
;; ============================================================================

(def ^:private thread-seq (AtomicLong. 0))

(defn- daemon-factory
  []
  (reify
    ThreadFactory
      (newThread [_ r]
        (doto (Thread. r (str "vis-egress-" (.getAndIncrement ^AtomicLong thread-seq)))
          (.setDaemon true)))))

(defn start!
  "Start a loopback egress proxy. `policy-fn` is a 0-arg fn returning the current
   compiled policy (see `compile-policy`) or nil (⇒ allow all). Binds `127.0.0.1` on an
   ephemeral port. Returns `{:port <int> :stop! (fn [])}`. Idempotent stop.

   `:on-log` (optional) receives a decision map per request for audit."
  [{:keys [policy-fn on-log]}]
  (let
    [on-log
     (or on-log
         (fn [_]
           nil))

     server
     (doto (ServerSocket.)
       (.setReuseAddress true)
       (.bind (InetSocketAddress. (InetAddress/getByName "127.0.0.1") 0)))

     port
     (.getLocalPort server)

     pool
     (Executors/newCachedThreadPool (daemon-factory))

     running
     (atom true)

     accept
     (fn []
       (while @running
         (let [client (try (.accept server) (catch Throwable _ nil))]
           (when client
             (.submit pool
                      ^Runnable
                      (fn []
                        (handle-client pool client (or policy-fn (constantly nil)) on-log)))))))]

    (doto (Thread. ^Runnable accept "vis-egress-accept") (.setDaemon true) (.start))
    {:port port
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil))
              (try (.shutdownNow pool) (catch Throwable _ nil))
              nil)}))
