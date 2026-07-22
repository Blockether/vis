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
     - HTTPS (`CONNECT host:443`): HOST allow/deny always; FULL method + path too when
       a MITM capability is supplied and the policy asks for it (`:mitm?`) — the proxy
       terminates the child's TLS with an ephemeral per-host leaf cert (see
       `internal.tls-mitm`), reads the real verb/path, then re-encrypts to the real
       upstream (whose real cert it still validates). Without MITM it is a raw byte
       tunnel (verb opaque) — the documented CONNECT-only ceiling.

   The policy is a plain VALUE (per session), read fresh per connection via `policy-fn`
   so `/reload` + config edits take effect with no restart. One request per upstream
   connection (we force `Connection: close`), so HTTP keep-alive can't smuggle a second,
   unfiltered verb onto an already-approved socket."
  (:require [clojure.string :as str])
  (:import (java.io InputStream OutputStream)
           (java.net InetAddress InetSocketAddress ServerSocket Socket URI)
           (java.util.concurrent Executors ExecutorService ThreadFactory)
           (java.util.concurrent.atomic AtomicLong)
           (javax.net.ssl SSLContext SSLSocket SSLSocketFactory)))

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
             {}
             rules)]

    (mapv (fn [[h {:keys [methods allow]}]]
            {"host" h "methods" methods "allow" allow})
          acc)))


(defn compile-policy
  "Compile raw network-opts `{:allowed-domains :denied-domains :rules}`
   into the value the proxy consults: canonical domain lists + normalized rules. Returns
   nil when there is nothing to enforce (no domain restriction and no rules) — the caller
   then need not route through the proxy at all."
  [{:keys [allowed-domains denied-domains exclude-domains] :as net}]
  (let
    [allowed
     (vec (keep host-key allowed-domains))

     denied
     (vec (keep host-key denied-domains))

     excluded
     (vec (keep host-key exclude-domains))

     rules
     (normalize-rules net)

     allow-private
     (boolean (:allow-private net))

     restrict-domains?
     (or (seq denied) (and (seq allowed) (not (some #{"*"} allowed))))]

    ;; `:exclude-domains` alone never forces enforcement — it only tells an already-
    ;; enforced policy to tunnel those hosts opaquely instead of terminating their TLS.
    ;; `:allow-private` DOES force a policy value so the SSRF floor can read the opt-in.
    (when (or restrict-domains? (seq rules) allow-private)
      {:allowed-domains allowed
       :denied-domains denied
       :exclude-domains excluded
       :rules rules
       :allow-private? allow-private})))

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
        (:deny-all? policy) {:allow? false :reason (:reason policy "vis: egress denied")}
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

(defn mitm-excluded?
  "True when `host` matches the policy's `:exclude-domains` — a client MITM cannot
   serve (cert-pinned tools, mTLS upstreams, Go/`gh` on macOS that ignores the CA
   env). Such a host stays HOST-allowlisted but its TLS is tunnelled opaquely, so
   verb/path is unenforced there. nil policy / empty list ⇒ never excluded."
  [policy host]
  (boolean (some #(host-matches? % host) (:exclude-domains policy))))

;; ============================================================================
;; SSRF deny-floor. The proxy runs UNJAILED with full host network, so it is a
;; confused deputy: a jailed child (whose only door is this proxy) could ask it
;; to fetch the host's OWN trust plane — loopback services, the gateway's control
;; port, cloud metadata (169.254.169.254 → IAM creds), the internal LAN. This
;; floor is INDEPENDENT of the domain allowlist: `allowed-domains ["*"]` means
;; "any PUBLIC host," never the box itself. It applies even when the policy is
;; nil (the friendly default-allow-public posture).
;; ============================================================================

(defn- cgnat-address?
  "100.64.0.0/10 — carrier-grade-NAT / shared address space (RFC 6598)."
  [^bytes b]
  (and (= 4 (alength b)) (= 100 (bit-and (aget b 0) 0xff)) (<= 64 (bit-and (aget b 1) 0xff) 127)))

(defn- ula-address?
  "fc00::/7 — IPv6 unique-local (RFC 4193)."
  [^bytes b]
  (and (= 16 (alength b)) (= 0xfc (bit-and (aget b 0) 0xfe))))

(defn- non-loopback-always-blocked?
  "Link-local (incl. metadata 169.254.169.254 and fe80::/10), wildcard/any-local, and
   multicast — the host's own trust plane. Blocked ALWAYS: never a legitimate egress
   target, not even via `:allow-private` or a reserved-port carve-out."
  [^InetAddress a]
  (or (.isLinkLocalAddress a) (.isAnyLocalAddress a) (.isMulticastAddress a)))

(defn- private-address?
  "Overridable deny (opt in via policy `:allow-private`): RFC1918 site-local, CGNAT
   100.64/10, IPv6 ULA fc00::/7 — the internal LAN, reachable from the gateway but
   off-limits to a jailed child by default."
  [^InetAddress a]
  (let [b (.getAddress a)]
    (or (.isSiteLocalAddress a) (cgnat-address? b) (ula-address? b))))

(defn- loopback-allowed?
  "Loopback egress is ALLOWED by default: the agent runs on the user's OWN machine,
   so reaching local dev servers (127.0.0.1:3000, a local DB) is expected. The ONLY
   loopback targets denied are the RESERVED gateway ports (the shared proxy itself +
   the control-plane API), non-overridably — a jailed child can never reach vis's own
   control plane. (Cloud metadata / link-local / any-local are a SEPARATE always-on
   floor; the internal LAN stays behind `:allow-private`.)"
  [policy port]
  (not (contains? (set (:reserved-loopback-ports policy)) port)))

(defn safe-upstream-address
  "Resolve `host` and validate EVERY resolved address against the SSRF deny-floor, then
   return `{:addr InetAddress}` to dial — the validated IP LITERAL, so the later connect
   can't be re-resolved to an internal address (DNS-rebind TOCTOU). `{:blocked reason}`
   when any resolved address is off-limits (or resolution fails).

   `policy` supplies `:allow-private?` (opts into RFC1918/CGNAT/ULA) and
   `:reserved-loopback-ports` (gateway ports that stay blocked). Link-local / metadata /
   any-local / multicast stay blocked ALWAYS; loopback is ALLOWED by default EXCEPT the
   reserved gateway/proxy ports; the internal LAN needs `:allow-private?`. nil host is blocked."
  [host port policy]
  (let
    [allow-private?
     (:allow-private? policy)

     addrs
     (when host (try (vec (InetAddress/getAllByName (str host))) (catch Throwable _ nil)))

     loopback?
     (fn [^InetAddress a]
       (.isLoopbackAddress a))]

    (cond (empty? addrs) {:blocked (str "cannot resolve host: " host)}
          (some non-loopback-always-blocked? addrs)
          {:blocked (str "blocked internal address for host: " host)}
          (some loopback? addrs) (if (loopback-allowed? policy port)
                                   {:addr (first (filter loopback? addrs))}
                                   {:blocked (str "blocked loopback address for host: " host)})
          (and (not allow-private?) (some private-address? addrs))
          {:blocked (str "blocked private address for host: " host)}
          :else {:addr (first addrs)})))

;; ── Tier-2 request filters (turn 46) ─────────────────────────────────────────
;; Registered fns that see the FULL request (method/host/path/headers) at the
;; proxy boundary — for HTTPS this is POST-DECRYPTION — and may DENY it. The
;; extension escape valve ABOVE the declarative `:rules` policy: trusted
;; extension code deciding over untrusted egress. Global (like op-hooks), keyed
;; by owner so an extension's filters die with it. FAIL-CLOSED: a filter that
;; throws DENIES (a security filter must never fail open).
(defonce ^:private network-filters (atom {})) ; owner -> [ (fn [ctx] decision) ]

(defn register-network-filter!
  "Register a Tier-2 network filter `f` `(fn [ctx] -> decision)` under `owner`.
   Fires at BOTH phases of an exchange: on the way OUT `ctx` is the request
   `{:phase :method :host :path :headers}`, on the way BACK it is the upstream
   response `{:phase :method :host :path :status :headers}` (`:phase`
   distinguishes them; headers a lower-cased name->value map). `f` returns nil /
   `{:allow? true}` to allow, or `{:allow? false :reason s}` (or a `vis.block`
   marker map) to DENY — a denied response yields a 403 so the child never sees
   the body. Only fires on MITM'd HTTPS + plain HTTP (a tunnelled/excluded host
   is encrypted and opaque)."
  [owner f]
  (when (and owner (ifn? f)) (swap! network-filters update owner (fnil conj []) f))
  owner)

(defn unregister-network-filters-for-owner!
  "Remove every network filter registered by `owner` (extension teardown)."
  [owner]
  (when owner (swap! network-filters dissoc owner))
  nil)

(defn registered-network-filters
  "All currently registered network-filter fns, across owners."
  []
  (into [] cat (vals @network-filters)))

(defn- headers->map
  "Raw header lines `[\"Name: value\" …]` -> a lower-cased name->value map."
  [headers]
  (reduce (fn [m ^String h]
            (let [i (.indexOf h ":")]
              (if (pos? i)
                (assoc m (str/lower-case (str/trim (subs h 0 i))) (str/trim (subs h (inc i))))
                m)))
          {}
          headers))

(defn- filter-decision
  "Interpret ONE filter's return value into `{:allow? bool :reason str}`.
   nil / `{:allow? true}` ⇒ allow; a `block` marker or `:allow?`/`allow` false
   ⇒ deny; any other value ⇒ allow."
  [res]
  (cond (nil? res) {:allow? true}
        (map? res) (let
                     [marker
                      (or (get res "marker") (get res :marker))

                      allow
                      (if (contains? res :allow?) (:allow? res) (get res "allow"))

                      reason
                      (or (get res :reason) (get res "reason"))]

                     (if (or (= "block" (str marker)) (false? allow))
                       {:allow? false :reason (str (or reason "blocked by filter"))}
                       {:allow? true}))
        (false? res) {:allow? false :reason "blocked by filter"}
        :else {:allow? true}))

(defn run-filters
  "Run `filters` (seq of `(fn [req])`) over request `req`; FIRST DENY WINS.
   FAIL-CLOSED: a filter that throws denies. Returns `{:allow? bool :reason str}`."
  [filters req]
  (reduce (fn [_ f]
            (let
              [d (try (filter-decision (f req))
                      (catch Throwable t
                        {:allow? false :reason (str "filter error: " (.getMessage t))}))]
              (if (:allow? d) {:allow? true} (reduced d))))
          {:allow? true}
          filters))

(defn apply-network-filters
  "Run every registered network filter over `ctx` (a request or response map).
   `{:allow? true}` when none deny."
  [ctx]
  (run-filters (registered-network-filters) ctx))

(defn decide+filter
  "Tier-1 `decide` then, only if allowed, the Tier-2 registered request filters.
   `req` = `{:phase :method :host :path :headers}`."
  [policy {:keys [method host path] :as req}]
  (let [d (decide policy method host path)]
    (if (:allow? d) (apply-network-filters req) d)))


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

(defn- proxy-auth-token
  "The session token a jailed child sends as `Proxy-Authorization: Basic
   base64(<token>:)` (the token rides the proxy URL userinfo). nil when absent or
   malformed — the caller then fails closed on an unattributable request."
  [headers]
  (some (fn [^String h]
          (when (= "proxy-authorization" (header-name h))
            (let [v (str/trim (subs h (inc (.indexOf h ":"))))]
              (when (str/starts-with? (str/lower-case v) "basic ")
                (try (let
                       [decoded (String. (.decode (java.util.Base64/getDecoder)
                                                  ^String (str/trim (subs v 6))))
                        user (first (str/split decoded #":" 2))]

                       (when-not (str/blank? user) user))
                     (catch Throwable _ nil))))))
        headers))


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

(defn- copy-until-eof
  "Copy `in`→`out` until EOF, flushing each chunk; swallow IO errors (peer closed)."
  [^InputStream in ^OutputStream out]
  (let [buf (byte-array 16384)]
    (try (loop []

           (let [n (.read in buf)]
             (when (pos? n) (.write out buf 0 n) (.flush out) (recur))))
         (catch Throwable _ nil))))

(defn- status-code
  "Parse the numeric status out of a response status line \"HTTP/1.1 200 OK\"."
  [line]
  (when line
    (let [parts (str/split (str/trim (str line)) #"\s+")]
      (when (>= (count parts) 2) (try (Integer/parseInt (nth parts 1)) (catch Throwable _ nil))))))

(defn- relay-with-response-filter
  "Forward the child's remaining request body to `upstream` (async) while reading
   the upstream RESPONSE status line + headers, running the registered response
   filters, and — only if allowed — writing them back to `client` followed by a
   streamed body. A denial replaces the response with a 403; the child never sees
   the upstream body. `req` is the originating request context (host/method/path/
   phase). Used only when response filters are registered; callers otherwise use
   the cheaper full-duplex `splice`."
  [^ExecutorService pool ^Socket client ^Socket upstream req on-log]
  (let
    [cin
     (.getInputStream client)

     cout
     (.getOutputStream client)

     uin
     (.getInputStream upstream)

     uout
     (.getOutputStream upstream)

     ;; Push any remaining request body child→upstream concurrently, so reading
     ;; the response on this thread can't deadlock against a body still in flight.
     _body
     (.submit pool
              ^Runnable
              (fn []
                (copy-until-eof cin uout)
                (try (.shutdownOutput ^Socket upstream) (catch Throwable _ nil))))

     status-line
     (read-line-bytes uin)]

    (when status-line
      (let
        [resp-headers
         (read-headers uin)

         resp
         (assoc req
           :phase (keyword (str (name (:phase req)) "-response"))
           :status (status-code status-line)
           :headers (headers->map resp-headers))

         {:keys [allow? reason]}
         (apply-network-filters resp)]

        (if-not allow?
          (do (on-log (assoc resp
                        :allow? false
                        :reason reason))
              (deny-response cout reason))
          (do (write-str cout
                         (str status-line
                              "\r\n"
                              (str/join "\r\n" resp-headers)
                              (when (seq resp-headers) "\r\n")
                              "\r\n"))
              (copy-until-eof uin cout)))))))

;; ============================================================================
;; Connection handling
;; ============================================================================

(defn- parse-request-line
  "\"METHOD TARGET VERSION\" → [method target version] (or nil)."
  [line]
  (let [parts (str/split (str line) #"\s+")]
    (when (= 3 (count parts)) parts)))

(defn- raw-tunnel
  "CONNECT fallback (no MITM): a raw byte tunnel to `host:port` — verb/path opaque."
  [^ExecutorService pool ^Socket client ^OutputStream cout ^InetAddress addr ^String host port
   on-log]
  (let [upstream (Socket.)]
    (on-log {:phase :connect :host host :port port :allow? true})
    (try (.connect upstream (InetSocketAddress. ^InetAddress addr (int port)) 15000)
         (write-str cout "HTTP/1.1 200 Connection Established\r\n\r\n")
         (splice pool client upstream)
         (catch Throwable t
           (try (deny-response cout (str "upstream error: " (.getMessage t)))
                (catch Throwable _ nil)))
         (finally (try (.close upstream) (catch Throwable _ nil))))))

(defn- mitm-intercept
  "CONNECT with a MITM capability: terminate the child's TLS with a per-host leaf,
   read the real method+path, apply the policy, then re-encrypt to the real
   upstream (system-validated) and relay. One request per connection (Connection:
   close) so keep-alive can't smuggle a second, unfiltered verb."
  [^ExecutorService pool ^Socket client ^OutputStream cout ^InetAddress addr ^String host port
   policy cap on-log]
  (write-str cout "HTTP/1.1 200 Connection Established\r\n\r\n")
  (let
    [^SSLContext ctx
     ((:ctx-for cap) host)

     ^SSLSocket ssl-client
     (.createSocket (.getSocketFactory ctx) ^Socket client host (int port) true)]

    (try
      (.setUseClientMode ssl-client false)
      (.startHandshake ssl-client)
      (let
        [cin
         (.getInputStream ssl-client)

         scout
         (.getOutputStream ssl-client)

         rl
         (parse-request-line (read-line-bytes cin))]

        (when rl
          (let
            [[method target version]
             rl

             headers
             (read-headers cin)

             path
             (str target)

             req
             {:phase :https :method method :host host :path path :headers (headers->map headers)}

             {:keys [allow? reason]}
             (decide+filter policy req)]

            (if-not allow?
              (do
                (on-log
                  {:phase :https :method method :host host :path path :allow? false :reason reason})
                (deny-response scout reason))
              (let
                [^SSLSocketFactory usf
                 (:upstream-factory cap)

                 ;; Dial the SSRF-validated IP literal, then layer TLS using the
                 ;; real `host` for SNI + cert verification — so a rebind can't
                 ;; swap in an internal address between resolve and connect.
                 ^Socket plain
                 (doto (Socket.) (.connect (InetSocketAddress. ^InetAddress addr (int port)) 15000))

                 ^SSLSocket upstream
                 (.createSocket usf plain host (int port) true)]

                (on-log {:phase :https :method method :host host :path path :allow? true})
                (try (.startHandshake upstream)
                     (let
                       [uout
                        (.getOutputStream upstream)

                        kept
                        (remove (fn [h]
                                  (#{"proxy-connection" "connection" "keep-alive"
                                     "proxy-authorization"}
                                   (header-name h)))
                          headers)

                        req
                        (str method
                             " "
                             path
                             " "
                             version
                             "\r\n"
                             (str/join "\r\n" kept)
                             (when (seq kept) "\r\n")
                             "Connection: close\r\n\r\n")]

                       (write-str uout req)
                       (if (seq (registered-network-filters))
                         (relay-with-response-filter
                           pool
                           ssl-client
                           upstream
                           {:phase :https :method method :host host :path path}
                           on-log)
                         (splice pool ssl-client upstream)))
                     (catch Throwable t
                       (try (deny-response scout (str "upstream error: " (.getMessage t)))
                            (catch Throwable _ nil)))
                     (finally (try (.close upstream) (catch Throwable _ nil)))))))))
      (catch Throwable _ nil)
      (finally (try (.close ssl-client) (catch Throwable _ nil))))))

(defn- handle-connect
  "HTTPS CONNECT host:port — host-level allow/deny first, then either a MITM
   inspection (when a capability is supplied and the policy asks, `:mitm?`) for
   full verb/path, or a raw byte tunnel (verb opaque)."
  [^ExecutorService pool ^Socket client ^OutputStream cout target policy mitm on-log]
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
      ;; Host allowed by policy — now the SSRF floor: resolve + reject the host's
      ;; own trust plane (loopback / metadata / internal LAN), dial the validated IP.
      (let [{:keys [addr blocked]} (safe-upstream-address host port policy)]
        (if blocked
          (do (on-log {:phase :connect :host host :allow? false :reason blocked})
              (deny-response cout blocked))
          (let
            [cap (when (and mitm (:mitm? policy) (not (mitm-excluded? policy host)))
                   (try (mitm) (catch Throwable _ nil)))]
            (if cap
              (mitm-intercept pool client cout addr host port policy cap on-log)
              (raw-tunnel pool client cout addr host port on-log))))))))

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

     req
     {:phase :http :method method :host host :path path :headers (headers->map headers)}

     {:keys [allow? reason]}
     (decide+filter policy req)

     ;; SSRF floor (only worth resolving once the host is policy-allowed).
     {:keys [addr blocked]}
     (when (and host allow?) (safe-upstream-address host port policy))]

    (cond (nil? host) (deny-response cout "malformed proxy request")
          (not allow?)
          (do (on-log
                {:phase :http :method method :host host :path path :allow? false :reason reason})
              (deny-response cout reason))
          blocked
          (do (on-log
                {:phase :http :method method :host host :path path :allow? false :reason blocked})
              (deny-response cout blocked))
          :else (let [upstream (Socket.)]
                  (on-log {:phase :http :method method :host host :path path :allow? true})
                  (try (.connect upstream (InetSocketAddress. ^InetAddress addr (int port)) 15000)
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
                         (if (seq (registered-network-filters))
                           (relay-with-response-filter
                             pool
                             client
                             upstream
                             {:phase :http :method method :host host :path path}
                             on-log)
                           (splice pool client upstream)))
                       (catch Throwable t
                         (try (deny-response cout (str "upstream error: " (.getMessage t)))
                              (catch Throwable _ nil)))
                       (finally (try (.close upstream) (catch Throwable _ nil))))))))

(defn- handle-client
  [^ExecutorService pool ^Socket client policy-fn mitm on-log]
  (try (.setSoTimeout client 30000)
       (let
         [cin
          (.getInputStream client)

          cout
          (.getOutputStream client)

          line
          (read-line-bytes cin)

          rl
          (parse-request-line line)]

         (if-not rl
           nil
           (let
             [[method target version]
              rl

              ;; Read headers ONCE, up front, for both CONNECT and plain HTTP:
              ;; the session token lives in `Proxy-Authorization`, so the policy
              ;; can only be resolved AFTER the headers are in hand.
              headers
              (read-headers cin)

              token
              (proxy-auth-token headers)

              policy
              (try (policy-fn token) (catch Throwable _ nil))]

             (if (= (str/upper-case method) "CONNECT")
               (handle-connect pool client cout target policy mitm on-log)
               (handle-http pool client cout method target version headers policy on-log)))))
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
  "Start a loopback egress proxy. `policy-fn` is a 1-arg fn `(fn [token] policy)` — it
   receives the per-connection session TOKEN (from `Proxy-Authorization`, nil when
   absent) and returns the compiled policy (see `compile-policy`), nil (⇒ allow all),
   or a `:deny-all?` sentinel (fail-closed). Binds `127.0.0.1` on an
   ephemeral port. Returns `{:port <int> :stop! (fn [])}`. Idempotent stop.

   `:on-log` (optional) receives a decision map per request for audit.

   `:mitm` (optional) a 0-arg fn returning a `tls-mitm/create!` capability (or nil).
   When present AND the per-connection policy carries `:mitm?`, HTTPS CONNECT is
   TLS-terminated so method+path are enforced; otherwise CONNECT is a raw tunnel."
  [{:keys [policy-fn on-log mitm]}]
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
             (.submit
               pool
               ^Runnable
               (fn []
                 (handle-client pool client (or policy-fn (constantly nil)) mitm on-log)))))))]

    (doto (Thread. ^Runnable accept "vis-egress-accept") (.setDaemon true) (.start))
    {:port port
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil))
              (try (.shutdownNow pool) (catch Throwable _ nil))
              nil)}))
