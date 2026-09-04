(ns com.blockether.vis.internal.process-jail
  "OS-level process CONTAINMENT — the 'jail' — that wraps the shell executors'
   argv so an allowed child is physically confined to the session workspace roots
   and, when network is off, cannot open a socket. This is a real containment
   boundary — not a cooperative name/argv check, which can be walked around since
   argv[0] is `bash` and the real binary hides inside the `-lc` string; the jail
   constrains what the child can DO once it runs, regardless of what a script
   inside it tries (curl, python -c, /dev/tcp — all hit the same wall).

   POLICY, NOT GUARDS. The jail is driven by a declarative *policy* compiled from
   vis.yml + the LIVE session roots, not by hand-written guard functions. The
   policy is a plain VALUE passed per spawn (never a process-global singleton, so
   many concurrent sessions in one gateway never stomp each other). Its shape:

     {:roots-fn     (fn [] [root-strings])  ; live session RW roots, re-read/spawn
      :net-enabled? <bool>                  ; whole shell-child network on/off
      :allow-read-write [<path> …]           ; full read+write grant
      :deny-write       [<path> …]           ; protect within writable (deny wins)
      :allow-read       [<path> …]           ; additional read-only paths
      :deny-read        [<path> …]           ; protect a read region (deny wins)
      :deny-exec        [<path> …]           ; readable but never executable
      :keychain?        <bool>               ; the OS credential store is reachable
      :inbound-ports    [<int> …]            ; ports a child may ACCEPT on from
                                             ; other hosts (loopback is always open)
      :env-values       {<NAME> <value>}     ; RESOLVED project env (`.env` +
                                             ; `environment:`) with ONE call's own
                                             ; `env` delta merged on, per spawn
      :env-removals     #{<NAME> …}          ; names THAT call asked to UNSET
      :inherit-host-env? <bool>}             ; `jail.environment: inherit` — the child
                                             ; also keeps the operator's ambient env

   The filesystem model mirrors Anthropic's sandbox-runtime:
     - WRITE is allow-only: denied everywhere except the session roots + tmp +
       `:allow-read-write`; `:deny-write` wins.
     - READ is default-deny here (workspace-focused, stronger than srt's
       read-everywhere default): system code/config + RW paths + `:allow-read`
       are readable; `:deny-read` wins.

   This namespace owns WHAT a session's child may do: it turns the session's
   configuration, live roots, proxy endpoint and call environment into one
   platform-neutral policy value and the complete child environment. HOW the
   operating system enforces that value belongs to `com.blockether/vis-python-runtime`
   (`spawn-process!` with `:policy`): the per-platform enforcement, the
   already-confined marker and the refusal on a host that cannot enforce all live
   there, beside the process launcher, so no enforcement text is assembled here."
  (:require [clojure.string :as str]
            [com.blockether.vis-python-runtime :as python-runtime]
            [com.blockether.vis.internal.python-runtime :as vis-python-runtime]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.util :as util])
  (:import (java.util HashMap)))

(defn supported?
  "True when this host can confine a child at all."
  []
  (nil? (python-runtime/jail-unsupported-reason)))

(defn unenforceable-reason
  "Nil when `supported?`, else the runtime's explanation of the platform gap."
  []
  (python-runtime/jail-unsupported-reason))

(defn- enforcing?
  "True when `policy` will actually confine the child: enabled, and either this
   process is itself already confined (the child inherits) or the host can
   confine. Decides whether the child gets the scrubbed environment."
  [policy]
  (boolean (and policy (not (:disabled? policy)) (or (python-runtime/jailed?) (supported?)))))

(defn- inbound-ports
  "Distinct integers in the legal TCP range; nil, junk and out-of-range values are
   dropped so a bad config value can never widen the policy."
  [ports]
  (->> ports
       (keep (fn [p]
               (let [n (cond (integer? p) (long p)
                             (string? p) (parse-long (str/trim p)))]
                 (when (and n (<= 1 n 65535)) n))))
       distinct
       vec))

(defn runtime-policy
  "The platform-neutral confinement VALUE the runtime compiles, from a session
   policy: the LIVE session roots via `:roots-fn` plus `:allow-read-write` are
   read-write, `:allow-read` read-only, the deny lists win, egress is the session
   proxy when one is up, otherwise open or off with `:net-enabled?`, and inbound
   is the managed listener port plus `:inbound-ports`. Called per spawn, so each
   child gets the CURRENT live roots without re-reading model-writable config."
  [{:keys [roots-fn net-enabled? allow-read-write allow-read deny-write deny-read deny-exec
           proxy-port loopback-port keychain?]
    :as policy}]
  (let [session-roots (when roots-fn (try (roots-fn) (catch Throwable _ nil)))]
    {:read-write (vec (concat session-roots allow-read-write))
     :read-only (vec allow-read)
     :deny-write (vec deny-write)
     :deny-read (vec deny-read)
     :deny-exec (vec deny-exec)
     :network (cond proxy-port {:proxy proxy-port}
                    net-enabled? :open
                    :else :off)
     :inbound (inbound-ports (cons loopback-port (:inbound-ports policy)))
     :keychain? (boolean keychain?)}))

(def ^:private keychain-denial-markers
  "How the macOS Security framework reports a lookup it could not complete. The
   first two are exactly what a DENIED credential-store lookup produces inside the jail."
  ["SecKeychainSearchCreateFromAttributes" "SecKeychainItemCopyContent"
   "errSecInteractionNotAllowed" "User interaction is not allowed"
   "could not be found in the keychain"])

(defn keychain-denial?
  "True when captured output shows a macOS Keychain lookup that failed the way a
   denied Mach lookup fails. Pure text test; `keychain-denial-hint` decides
   whether confinement is the explanation. ONE `output` string, because a command
   runs under a pty where stdout and stderr are one stream."
  [output]
  (let [text (str output)]
    (boolean (some #(str/includes? text %) keychain-denial-markers))))

(defn keychain-denial-hint
  "One actionable line when a command's output shows a Keychain lookup THIS jail
   denied, else nil. Silent when the jail is off (`:disabled?`) or the keychain
   is already granted (`:keychain?`) — the failure is then a real Keychain miss
   and naming the sandbox would send the caller the wrong way."
  [{:keys [disabled? keychain?]} output]
  (when (and (not disabled?) (not keychain?) (keychain-denial? output))
    (str "Keychain lookup blocked by the sandbox: a confined child cannot reach the OS"
         " credential store by default. Set `jail.keychain: true` in config to grant it.")))


(defn- java-proxy-options
  [{:keys [proxy-port java-trust-store java-trust-store-password java-proxy? loopback-port]}]
  (when (and java-proxy? proxy-port)
    (str (when loopback-port
           ;; Keep the nREPL listener on AF_INET; its launcher binds 127.0.0.1 and
           ;; the jail admits inbound traffic only on `loopback-port`.
           "-Djava.net.preferIPv4Stack=true ")
         "-Dhttp.proxyHost=127.0.0.1"
         " -Dhttp.proxyPort="
         proxy-port
         " -Dhttps.proxyHost=127.0.0.1"
         " -Dhttps.proxyPort=" proxy-port
         ;; Empty means even loopback HTTP destinations use the policy proxy. The
         ;; nREPL transport itself is a raw socket and is unaffected by this JVM option.
         " -Dhttp.nonProxyHosts=" (when java-trust-store
                                    (str " -Djavax.net.ssl.trustStore="
                                         java-trust-store
                                         " -Djavax.net.ssl.trustStoreType=PKCS12"
                                         " -Djavax.net.ssl.trustStorePassword="
                                         java-trust-store-password)))))

(defn proxy-env
  "Environment additions for a child of an enabled policy. When a gateway proxy
   endpoint is present, common proxy and CA variables cover curl/git/Python/Bun/
   etc.; managed JVM children additionally receive proxy plus ephemeral
   truststore properties through JAVA_TOOL_OPTIONS. The already-confined marker
   is not ours: the runtime stamps it on every child it confines."
  [policy]
  (if (:disabled? policy)
    {}
    (let [jail-env {}]
      (if-let [port (:proxy-port policy)]
        (let [token (:proxy-token policy)
              url (str "http://" (when token (str token "@")) "127.0.0.1:" port)
              ;; SOCKS5 shares the SAME loopback port (multiplexed by first byte).
              ;; `ALL_PROXY` is the fallback for non-HTTP schemes (ssh/git+ssh/db/raw
              ;; TCP) — it points at the SOCKS lane, while `http(s)_proxy` keep the
              ;; HTTP proxy so HTTPS verb/path MITM is preserved for web traffic.
              socks-url (str "socks5h://" (when token (str token "@")) "127.0.0.1:" port)
              ca (:ca-file policy)
              java-opts (java-proxy-options policy)]

          (cond-> (merge jail-env
                         {"http_proxy" url
                          "https_proxy" url
                          "all_proxy" socks-url
                          "HTTP_PROXY" url
                          "HTTPS_PROXY" url
                          "ALL_PROXY" socks-url
                          "no_proxy" ""
                          "NO_PROXY" ""})
            ca
            (merge {"CURL_CA_BUNDLE" ca
                    "SSL_CERT_FILE" ca
                    "REQUESTS_CA_BUNDLE" ca
                    "NODE_EXTRA_CA_CERTS" ca
                    "GIT_SSL_CAINFO" ca
                    "PIP_CERT" ca
                    "AWS_CA_BUNDLE" ca
                    "CARGO_HTTP_CAINFO" ca
                    "DENO_CERT" ca})

            java-opts
            (assoc "JAVA_TOOL_OPTIONS"
              (str (when-let [existing (not-empty (System/getenv "JAVA_TOOL_OPTIONS"))]
                     (str existing " "))
                   java-opts))))
        jail-env))))

(def ^:private env-passthrough-names
  "Non-secret operator vars a confined child may inherit under the default
   `jail.environment: declared`. Everything else in the operator environment —
   API keys, tokens, credentials — is dropped; a variable a child legitimately
   needs is DECLARED in `environment:` and arrives as a resolved value
   (`:env-values`), not by widening this list. `jail.environment: inherit`
   (`:inherit-host-env?`) hands the child the ambient environment WHOLE instead —
   the operator's explicit choice to give up ambient secrecy, and the only thing
   that widens this."
  #{"PATH" "HOME" "USER" "LOGNAME" "SHELL" "LANG" "LANGUAGE" "TERM" "TERMINFO" "TZ" "TMPDIR" "PWD"
    "HOSTNAME" "COLORTERM" "DISPLAY"})

(def ^:private env-passthrough-prefixes ["LC_"])

;; ── Variables that hijack the PRE-POLICY child bootstrap ───────────────────────
;;
;; `libvisjail` creates the child and installs its kernel policy before the requested
;; command starts. Loader and runtime variables still affect native setup before the
;; command reaches `main`, and can redirect code loading outside the policy's intended
;; executable graph. They therefore never cross this trust boundary.
;;
;; These names are therefore refused UNCONDITIONALLY — declaring one under
;; `environment:` cannot re-enable it, because the whole point of a declaration
;; is to hand a variable to the CONFINED child, and these never reach it: they
;; are consumed earlier, outside the jail. Declaring `LD_PRELOAD` for a sandbox
;; is not a use case, it is the exploit.

(def ^:private pre-exec-hijack-names
  "Exact names that run attacker code during another program's startup."
  #{"GCONV_PATH" "LOCPATH" "NLSPATH" "HOSTALIASES" "RESOLV_HOST_CONF" "BASH_ENV" "ENV" "SHELLOPTS"
    "BASHOPTS" "IFS"})

(def ^:private pre-exec-hijack-prefixes
  "Prefix families with the same power. `LD_*`/`DYLD_*` are the dynamic loader's
   own injection hooks (`LD_PRELOAD`, `LD_AUDIT`, `LD_LIBRARY_PATH`,
   `DYLD_INSERT_LIBRARIES`); `PERL*` configures the detacher itself (`PERL5OPT`
   loads arbitrary modules); `BASH_FUNC_*` smuggles shell functions. Matched by
   prefix on purpose — enumerating each name is a losing game."
  ["LD_" "DYLD_" "PERL" "BASH_FUNC_"])

(defn- pre-exec-hijack?
  "True when `k` can execute code in the unconfined detacher/enforcer hops."
  [^String k]
  (or (contains? pre-exec-hijack-names k)
      (boolean (some #(str/starts-with? k %) pre-exec-hijack-prefixes))))

(defn- env-passthrough?
  [^String k]
  (and (not (pre-exec-hijack? k))
       (or (contains? env-passthrough-names k)
           (boolean (some #(str/starts-with? k %) env-passthrough-prefixes)))))

(defn declared-env
  "The policy's RESOLVED project environment, as string pairs: the workspace's
   `.env`/`.env.local` with the operator's `environment:` declarations on top
   (`config/child-environment-values`). Every value already came from the source
   that produced it, so the jail hands them over verbatim — a confined child can
   read `.env` out of the workspace it was granted anyway, so withholding those
   values would confine nothing. A [[pre-exec-hijack?]] name is dropped even
   here, whether it came from a declaration or from a project `.env`: it would
   run code in the UNCONFINED detacher/enforcer hops, so nothing can buy it
   back."
  [policy]
  (into {}
        (keep (fn [[k v]]
                (let [k (str k)]
                  (when (and (not (str/blank? k)) (not (pre-exec-hijack? k)) (some? v))
                    [k (str v)]))))
        (:env-values policy)))

(defn jailed-child-env
  "The COMPLETE environment for a confined child: an allowlist of non-secret
   operator variables, plus the policy's resolved project environment (the
   workspace's `.env` plus the `environment:` declarations), plus this session's
   proxy + CA variables. Every API key / token / credential the operator happens
   to have exported is DROPPED — an AMBIENT variable a child needs is named in
   `environment:` — and so is every [[pre-exec-hijack?]] name, which would run
   code in the unconfined launcher before the jail exists.

   `jail.environment: inherit` (`:inherit-host-env?`) replaces the allowlist with
   the operator's WHOLE ambient environment, secrets included: filesystem,
   network, exec and Mach confinement are unchanged, but ambient secrecy is
   given up on purpose. The [[pre-exec-hijack?]] scrub still applies — that one
   is not confinement of the child, it is the jail's own installation.

   Returns nil when the policy is not enforcing — the caller keeps the parent
   environment and merges [[child-env-additions]] instead (unjailed
   platforms/`jail.enabled: false`), so non-confined behavior is unchanged."
  [policy]
  (when (enforcing? policy)
    (let [inherited
          (into {}
                (if (:inherit-host-env? policy)
                  (map identity)
                  (filter (fn [[k _]]
                            (env-passthrough? k))))
                (System/getenv))

          ;; A name ONE call asked to unset is simply never built into the map —
          ;; a confined child's environment is assembled here from nothing, so
          ;; there is nothing to remove later.
          removals
          (set (:env-removals policy))]

      ;; Total: the scrub also covers the declared + proxy additions, so no later
      ;; edit to either can reintroduce a pre-exec hijack name.
      (into {}
            (remove (fn [[k _]]
                      (or (pre-exec-hijack? k) (contains? removals k))))
            (merge inherited (declared-env policy) (proxy-env policy))))))

(defn child-env-additions
  "What an UNCONFINED child gets ON TOP of the inherited host environment: the
   resolved project environment (workspace `.env` + `environment:` declarations)
   plus this session's proxy + CA variables. They apply whether or not the jail
   is enabled — the project says where a variable comes from, and the jail only
   decides what ELSE a child may keep."
  [policy]
  (merge (declared-env policy) (proxy-env policy)))
(defn process-environment
  "Build the COMPLETE child environment for `policy`, then overlay trusted
   host-owned `extra`. Confined children start from the scrubbed allowlist;
   disabled policies preserve the ambient environment and apply removals."
  ([policy] (process-environment policy nil))
  ([policy extra]
   (let [^HashMap environment (if-let [full (jailed-child-env policy)]
                                (HashMap. ^java.util.Map full)
                                (doto (HashMap. ^java.util.Map (System/getenv))
                                  (.putAll ^java.util.Map (child-env-additions policy))))]
     (doseq [k (:env-removals policy)]
       (.remove environment ^String k))
     (when (seq extra) (.putAll environment ^java.util.Map extra))
     environment)))

(defn spawn!
  "Spawn `argv` through the runtime-owned process boundary. An enabled policy
   becomes a [[runtime-policy]] value the runtime enforces (it refuses the spawn
   on a host that cannot); a disabled policy, or a process that is itself already
   confined, still gets libvisjail's detached process group, PTY and lifecycle
   implementation with no second layer.

   Options: `:directory`, an exact `:environment` or trusted
   `:extra-environment`, `:pty?`, `:merge-stderr?`, `:rows`, and `:columns`."
  ([argv directory policy] (spawn! argv directory policy nil))
  ([argv directory policy {:keys [environment extra-environment pty? merge-stderr? rows columns]}]
   (let [confined? (and policy (not (:disabled? policy)) (not (python-runtime/jailed?)))]
     ;; The spawn itself is the runtime library's (`Jail/spawn`), so the cdylib
     ;; has to be resolvable HERE — in the process doing the spawning, which is
     ;; not necessarily one that ever started an interpreter. Ensuring is a no-op
     ;; once it resolves.
     (try (vis-python-runtime/ensure-library!) (catch Throwable _ nil))
     (python-runtime/spawn-process! (mapv str argv)
                                    {:environment
                                     (or environment (process-environment policy extra-environment))
                                     :directory (some-> directory
                                                        str)
                                     :policy (when confined? (runtime-policy policy))
                                     :pty? (boolean pty?)
                                     :merge-stderr? (boolean merge-stderr?)
                                     :rows (int (or rows 0))
                                     :columns (int (or columns 0))}))))

;; ── ONE call's own environment ──────────────────────────────────────────────
;; `environment:` says what EVERY child of Vis gets. A verb that SPAWNS also
;; carries what THIS child gets on top, and it carries it as an ARGUMENT of the
;; call — `shell(cmd, {"env": …})`, `repl_start({"env": …})` — never as
;; an ambient binding wrapped around a block. The record of the call is what
;; says which variables the child ran with: a scope opened three lines earlier
;; is gone the moment that block is folded, and independent spawns awaited
;; together would turn "which one saw the variable" into a question about
;; coroutines instead of about arguments.

(def ^:private call-env-name-pattern
  "A POSIX environment variable name — nothing else can be exported at all."
  #"[A-Za-z_][A-Za-z0-9_]*")

(def ^:private call-env-source-keys
  "Declaration keys ONE call's value may name — `environment:`'s own vocabulary,
   resolved by its own funnel (`config/inline-environment-value`)."
  ["env" "dotenv" "keychain" "command" "literal"])

(defn- refuse-call-env
  "Refuse the spawn and NAME the key. A per-call delta is the author's own line
   of code, so a variable dropped quietly here would be debugged as a bug in the
   program that never received it — the opposite of the `environment:` scrub,
   which filters a standing declaration nobody is watching."
  [name reason]
  (throw (ex-info (str "env " name ": " reason) {:type ::call-env-refused :name name})))

(defn- call-env-value
  "ONE delta value: nil to unset, a literal as its string, or a source map
   resolved exactly as the `environment:` block resolves the same shape."
  [k v]
  (cond (nil? v) nil
        (string? v) v
        (or (number? v) (boolean? v)) (str v)
        (map? v) (let [entry
                       (into {}
                             (map (fn [[ek ev]]
                                    [(if (keyword? ek) (name ek) (str ek)) ev]))
                             v)

                       source
                       (some (fn [sk]
                               (when (some? (get entry sk)) sk))
                             call-env-source-keys)]

                   (when-not source
                     (refuse-call-env
                       k
                       (str "a map value must name its source — "
                            (str/join ", " (map #(str "{\"" % "\": …}") call-env-source-keys))
                            " — got "
                            (pr-str v)
                            ".")))
                   (when (= "command" source)
                     (let [command (get entry source)]
                       (when-not (and (sequential? command)
                                      (seq command)
                                      (every? util/non-blank-string? command))
                         (refuse-call-env
                           k
                           (str "command source must be a non-empty argv list of non-blank strings"
                                (if (string? command)
                                  ", not a shell string."
                                  (str " — got " (pr-str command) ".")))))))
                   (or (config/inline-environment-value k entry)
                       (refuse-call-env k (str "the " source ": source resolved to no value."))))
        :else (refuse-call-env k
                               (str "value must be a literal (string, number, boolean), a source"
                                    " map, or null to unset — got "
                                    (pr-str v)
                                    "."))))

(defn call-env-values
  "Resolve ONE call's `env` delta into `{NAME value-or-nil}`, where nil means
   UNSET that name for this child. A DELTA: it is merged over the project
   environment (`config/child-environment-values`), never a replacement for it,
   so a workspace `.env` still reaches a child whose call names one variable.

   A value is either a LITERAL (string/number/boolean) or a SOURCE map — the
   same `{env|dotenv|keychain|command|literal}` shape `environment:` declares. That
   split is not style: this map is an ARGUMENT, so a literal is written into the
   session journal and the transcript for good. Literals are for SWITCHES
   (`NODE_ENV`, `RUST_LOG`, `PYTHONHASHSEED`); a secret names its source and
   only the child ever sees the value.

   Every refusal is LOUD and names the key: a name that is not a variable name,
   a [[pre-exec-hijack?]] name (which the jail would drop anyway, silently), a
   map naming no source, and a source that produced nothing — an explicit
   request for ONE variable that resolved to nothing is an error here, not the
   quiet `:unset` a standing declaration is allowed."
  [env]
  (cond (nil? env) {}
        (not (map? env))
        (throw (ex-info (str "env must be a map of NAME → value (a literal, or"
                             " {\"env\"|\"dotenv\"|\"keychain\"|\"command\"|\"literal\": …}), got "
                             (pr-str env))
                        {:type ::call-env-refused}))
        :else (into {}
                    (map (fn [[k v]]
                           (let [k (str/trim (str (if (keyword? k) (name k) k)))]
                             (when-not (re-matches call-env-name-pattern k)
                               (refuse-call-env (pr-str k) "not an environment variable name."))
                             (when (pre-exec-hijack? k)
                               (refuse-call-env
                                 k
                                 (str "runs code in the unconfined detacher/enforcer hops,"
                                      " before the jail exists — no call may set it.")))
                             [k (call-env-value k v)])))
                    env)))

(defn with-call-env
  "`policy` with ONE call's resolved delta merged over its project environment.
   Names set to nil become `:env-removals`: a confined child's environment is
   built from nothing so they are simply never added, while an UNCONFINED child
   inherits this process' environment and must have them removed before the
   complete map crosses the native spawn boundary.

   A nil policy is a spawn with no jail at all, and the delta still applies —
   a caller cannot lose its own variables by running where the jail is off."
  [policy overrides]
  (if (empty? overrides)
    policy
    (let [policy
          (or policy {:disabled? true})

          base
          (into {}
                (map (fn [[k v]]
                       [(str k) v]))
                (:env-values policy))

          removals
          (into #{} (comp (filter (comp nil? val)) (map key)) overrides)

          sets
          (into {} (remove (comp nil? val)) overrides)]

      (assoc policy
        :env-values (merge (apply dissoc base removals) sets)
        :env-removals (into (set (:env-removals policy)) removals)))))

(defn env-fingerprint
  "`{NAME \"<digest>\"}` for one resolved delta — its SHAPE without its values.
   This is what a status prints and what a REUSED process is compared against,
   and both of those are read by a model and written to a log, so the value
   itself can never appear: a name set from a keychain must compare equal to
   itself and to nothing else. An unset name fingerprints as \"unset\"."
  [values]
  (into (sorted-map)
        (map (fn [[k v]]
               [(str k) (if (nil? v) "unset" (subs (util/sha256-hex (str v)) 0 12))]))
        values))
(defn env-difference
  "Variable NAMES whose value differs between the env a live process is running
   with and the one a new start asked for. Both sides are FINGERPRINTS, so this
   compares digests and answers names — the only thing either side may keep."
  [running requested]
  (vec (sort (into #{}
                   (remove (fn [k]
                             (= (get running k) (get requested k))))
                   (concat (keys running) (keys requested))))))

(defn env-mismatch-refusal
  "`{:message :differing}` when a REPL is already running with an env OTHER than
   the one this start named, else nil. Every language pack answers this same
   refusal, because `repl_start` must mean ONE thing across languages: a live
   REPL is reused, never silently replaced, and an env it was not started with
   is a different REPL. Names and digests only — a value never reaches it."
  [id running requested]
  (when-let [differing (seq (env-difference running requested))]
    {:differing (vec differing)
     :message (str "repl_start for " id
                   " is already running with a different env (" (str/join ", " differing)
                   "). There is no restart:"
                   " repl_stop that REPL, then start it with this env.")}))

;; ── Standard language-process jail contract ────────────────────────────────
;; Language packs spawn managed REPLs and project test runners through
;; `session-process-spawn!`, which resolves one live per-session policy atomically
;; and crosses the one libvisjail boundary.

(def ^:private repl-toolchain-read-dirs
  "Installed language runtimes/toolchains a managed process may READ to boot. They
   remain read-only: a REPL may execute them but cannot replace its JVM/Python/Bun."
  ["~/.sdkman" "~/.asdf" "~/.jenv" "~/.pyenv" "~/.local/bin" "~/.local/share/mise"])

(def ^:private language-process-runtime-dirs
  "Vis-owned runtime state managed language processes may read and write. REPL
   managers write lifecycle logs here, including when exercised from a jailed
   project test runner."
  ["~/.vis/logs"])

(defn language-process-policy
  "Derive a managed-language jail policy from a session's base policy. It keeps
   filesystem confinement, adds read-only runtime installations plus Vis-owned log
   directories, and replaces the shell proxy endpoint with this session's
   attributed language-process endpoint.

   Dependency caches enter through the shared `workspace.filesystem` catalog and
   are already present on the base policy's read/write roots; this pass only adds
   the read-only language runtimes/toolchains plus Vis-owned log directories.

   Direct network access is disabled. CA-aware runtimes receive the combined PEM
   bundle, while JVM children also receive an ephemeral PKCS12 truststore.
   `loopback-port` permits only the managed nREPL's selected listener port."
  [base loopback-port]
  (cond (nil? base) nil
        (:disabled? base) base
        :else (-> base
                  (update :allow-read-write #(vec (concat % language-process-runtime-dirs)))
                  (update :allow-read #(vec (concat % repl-toolchain-read-dirs)))
                  ;; Managed REPL/test children get ONLY their own nREPL
                  ;; loopback port; the shell dev-server inbound allowlist is
                  ;; not theirs to inherit (least privilege).
                  (dissoc :inbound-ports)
                  (assoc :net-enabled? false
                         :proxy-port (:repl-proxy-port base)
                         :proxy-token nil
                         :ca-file (:repl-ca-file base)
                         :java-proxy? true
                         :loopback-port loopback-port))))

(defn repl-policy
  "Derive the managed-nREPL variant for its selected loopback listener port."
  [base loopback-port]
  (language-process-policy base loopback-port))

(defonce ^:private session-jail-policies (atom {}))

(defn register-session-jail!
  "Register (or replace) this session's live base jail-policy function."
  [session-id policy-fn]
  (when session-id (swap! session-jail-policies assoc session-id policy-fn)))

(defn prepare-session-jail!
  "Bind the language surface's live session env to the managed-process contract.
   Missing session identity or policy fails closed before a language handler can
   start a REPL or project test process. Safe and idempotent per dispatch."
  [{:keys [session-id jail-policy-fn]}]
  (when-not session-id
    (throw (ex-info "Managed language process denied: session id is unavailable"
                    {:type ::session-jail-missing})))
  (when-not jail-policy-fn
    (throw (ex-info "Managed language process denied: session jail policy is unavailable"
                    {:type ::session-jail-missing :session-id session-id})))
  (register-session-jail! session-id jail-policy-fn)
  session-id)

(defn unregister-session-jail!
  "Drop this session's registered jail policy (loop dispose)."
  [session-id]
  (when session-id (swap! session-jail-policies dissoc session-id)))

(defn- session-base-policy!
  [session-id]
  (let [policy-fn (get @session-jail-policies session-id)]
    (when-not policy-fn
      (throw (ex-info "Managed language process denied: session jail is not registered"
                      {:type ::session-jail-missing :session-id session-id})))
    (let [policy (try (policy-fn)
                      (catch Throwable t
                        (throw (ex-info
                                 "Managed language process denied: session jail policy failed"
                                 {:type ::session-jail-failed :session-id session-id}
                                 t))))]
      (when-not policy
        (throw (ex-info "Managed language process denied: session jail policy is unavailable"
                        {:type ::session-jail-missing :session-id session-id})))
      policy)))

(defn session-process-spawn!
  "THE managed-language launch contract. Resolve `session-id` atomically, derive
   its REPL/test policy, merge this call's environment delta, and spawn through
   [[spawn!]]. Unknown, disposed, or failing sessions are denied before spawn.

   Options additionally accept `:loopback-port`, `:env`, and every [[spawn!]]
   option. The returned value is a `java.lang.Process`."
  ([session-id argv directory] (session-process-spawn! session-id argv directory nil))
  ([session-id argv directory {:keys [loopback-port env] :as opts}]
   (let [policy (-> (session-base-policy! session-id)
                    (language-process-policy loopback-port)
                    (with-call-env (call-env-values env)))]
     (spawn! argv directory policy (dissoc opts :loopback-port :env)))))
