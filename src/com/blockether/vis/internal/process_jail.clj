(ns com.blockether.vis.internal.process-jail
  "OS-level process CONTAINMENT — the 'jail' — that wraps the shell executors'
   argv so an allowed child is physically confined to the session workspace roots
   and, when network is off, cannot open a socket. This is real containment, a
   real containment boundary — not a cooperative name/argv check, which can be
   walked around since argv[0] is `bash` and the real binary hides inside the
   `-lc` string;
   the jail constrains what the child can DO once it runs, regardless of what a
   script inside it tries (curl, python -c, /dev/tcp — all hit the same wall).

   POLICY, NOT GUARDS. The jail is driven by a declarative *policy* compiled from
   vis.yml + the LIVE session roots, not by hand-written guard functions. The
   policy is a plain VALUE passed per spawn (never a process-global singleton, so
   many concurrent sessions in one gateway never stomp each other). Its shape:

     {:roots-fn     (fn [] [root-strings])  ; live session RW roots, re-read/spawn
      :net-enabled? <bool>                  ; whole shell-child network on/off
      :allow-read-write [<path> …]           ; concise full read+write grant
      :allow-write      [<path> …]           ; legacy writable paths (also readable)
      :deny-write       [<path> …]           ; protect within writable (deny wins)
      :allow-read       [<path> …]           ; additional read-only paths
      :deny-read        [<path> …]           ; protect a read region (deny wins)
      :mach-services    [<name> …]           ; macOS Mach services a child may look up
      :inbound-ports    [<int> …]            ; extra local ports a child may ACCEPT on
                                             ; (bind is local-only; accept is port-gated)
      :env-values       {<NAME> <value>}}    ; RESOLVED `environment:` declarations,
                                             ; attached per spawn by the policy builder

   The filesystem model mirrors Anthropic's sandbox-runtime:
     - WRITE is allow-only: denied everywhere except the session roots + tmp +
       `:allow-read-write` + `:allow-write`; `:deny-write` wins.
     - READ is default-deny here (workspace-focused, stronger than srt's
       read-everywhere default): system code/config + RW paths + `:allow-read`
       are readable; `:deny-read` wins.

   `wrap-argv` compiles the policy, per spawn, into the OS enforcement primitive:

     - macOS  : a Seatbelt profile handed to the system `sandbox-exec -p` — ships
                with the OS, ZERO install. IMPLEMENTED + verified.
     - Linux  : bubblewrap (`bwrap`) mount + network namespaces. IMPLEMENTED (fs
                confinement + net-off wall); filtered egress via the proxy still
                needs seccomp, so a proxy-restricted net is denied ENTIRELY (safe)
                rather than left open. `supported?` is true only when `bwrap` is
                installed; otherwise `unenforceable-reason` explains the gap so a
                requested jail fails LOUD instead of silently passing the child.
     - other  : unsupported — callers keep the cooperative gate as the floor.

   Two locks learned from the kernel, baked in here:
     1. sandbox-exec matches RESOLVED real paths, so every root is realpath'd
        before templating (`/tmp` -> `/private/tmp`, else the rule never matches).
     2. a default-deny profile MUST `(import \"system.sb\")` or dyld/sysctl startup
        reads are denied and every binary aborts before `main`."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths])
  (:import (java.io File)
           (java.nio.file LinkOption Paths)
           (java.util.concurrent TimeUnit)))

(def ^:private link-opts (make-array LinkOption 0))

(defn- real-path
  "Canonical real-path string of `s`, or nil when it can't be resolved. sandbox-exec
   matches on RESOLVED paths, so roots MUST pass through here before templating."
  [s]
  (let [s (paths/expand-home s)]
    (when-not (str/blank? (str s))
      (try (.toString (.toRealPath (Paths/get (str s) (make-array String 0)) link-opts))
           (catch Throwable _ nil)))))

(defn- deny-path
  "Resolve a DENY target: prefer its real path, else fall back to the raw expanded
   string. Deny lists must fail safe — a not-yet-existing secret still denies."
  [s]
  (or (real-path s)
      (let [e (paths/expand-home s)]
        (when-not (str/blank? (str e)) (str e)))))

(defn- os-kind
  []
  (let [n (str/lower-case (str (System/getProperty "os.name")))]
    (cond (str/includes? n "mac") :macos
          (str/includes? n "linux") :linux
          :else :other)))

(def ^:private macos-sandbox-exec "/usr/bin/sandbox-exec")

;; Linux enforcement uses bubblewrap (`bwrap`) as the argv-prefix wrapper — the same
;; shape as macOS `sandbox-exec`. It is a setuid/unprivileged-userns helper that
;; builds a mount + PID + (optionally) network namespace for the child before exec.
;; Detected at its standard install locations; nil when absent (Linux then reports
;; the jail as UNENFORCEABLE rather than silently passing the child through).
(def ^:private linux-bwrap
  (some (fn [p]
          (when (.canExecute (File. ^String p)) p))
        ["/usr/bin/bwrap" "/bin/bwrap" "/usr/local/bin/bwrap"]))

;; Linux FILTERED EGRESS uses pasta (from the passt project) -- a userspace,
;; unprivileged TCP/IP stack. bwrap `--unshare-net` gives the child an isolated
;; network namespace whose loopback is its OWN, so the gateway proxy on the host's
;; `127.0.0.1:<port>` is unreachable -- filtered egress would die entirely. pasta
;; bridges ONLY that one port back to the host: `pasta -T <proxy-port> -t none
;; -u none -U none` forwards the child's `127.0.0.1:<proxy-port>` to the host
;; loopback and NOTHING else -- no default route (internet unreachable), no
;; gateway map (host control plane / other loopback ports unreachable). Exact
;; parity with the macOS "only the proxy port" Seatbelt rule, verified on a real
;; Linux kernel. nil when absent -- filtered egress then degrades to NO egress
;; (`--unshare-net`, safe) with a loud one-time warning. Same install class as
;; bwrap (`apt-get install passt`); works on WSL2 (real kernel) too.
(def ^:private linux-pasta
  (some (fn [p]
          (when (.canExecute (File. ^String p)) p))
        ["/usr/bin/pasta" "/bin/pasta" "/usr/local/bin/pasta"]))

;; WSL detection. WSL2 runs a REAL Linux kernel -- namespaces, bwrap and pasta all
;; work, so it is treated as ordinary Linux. WSL1 is a syscall-translation shim
;; with NO real kernel namespaces, so bwrap cannot enforce anything; it is reported
;; UNENFORCEABLE (fail loud) rather than silently passing children through. The
;; kernel `osrelease` distinguishes them: WSL2 = `...-microsoft-standard-WSL2`,
;; WSL1 = `...-Microsoft` (no `WSL2` marker).
(defn- linux-osrelease [] (try (slurp "/proc/sys/kernel/osrelease") (catch Throwable _ "")))

(defn- wsl1?
  "True only on WSL1 (a `microsoft` kernel WITHOUT the `WSL2` real-kernel marker)."
  []
  (let [r (str/lower-case (linux-osrelease))]
    (and (str/includes? r "microsoft") (not (str/includes? r "wsl2")))))

(defn supported?
  "True when the current OS can ENFORCE a jail. macOS: Seatbelt via the system
   `sandbox-exec`. Linux: bubblewrap (`bwrap`) namespaces. Other OSes: not
   supported -- callers keep the cooperative admission gate as the floor and
   `unenforceable-reason` explains why so `jail.enabled: true` never silently no-ops."
  []
  (case (os-kind)
    :macos
    (.canExecute (File. ^String macos-sandbox-exec))

    :linux
    (and (boolean linux-bwrap) (not (wsl1?)))

    false))

(defn unenforceable-reason
  "Nil when `supported?`, else a human string explaining why the jail CANNOT be
   enforced on this host -- so a requested `jail.enabled: true` fails LOUD instead of
   passing the child through unconfined. Distinguishes 'wrong OS' from 'right OS,
   enforcer binary missing' (the actionable case: install the tool)."
  []
  (when-not (supported?)
    (case (os-kind)
      :macos
      "macOS jail needs /usr/bin/sandbox-exec, which is not executable here"

      :linux
      (cond
        (wsl1?)
        "WSL1 has no real Linux kernel namespaces; the jail needs WSL2 (run `wsl --set-version <distro> 2`)"
        :else
        "Linux jail needs bubblewrap (`bwrap`); install it (e.g. `apt-get install bubblewrap`)")

      "the OS process jail is not available on this operating system")))

(defn- inherited-jail?
  "True inside a child already confined by this process-jail contract. Seatbelt
   restrictions are inherited across exec, and macOS rejects a second
   `sandbox-exec` application, so descendants must not wrap themselves again."
  []
  (= "1" (System/getenv "VIS_SEATBELT_ACTIVE")))

;; Read paths every Mach-O binary + dyld needs to even reach main(), plus the
;; standard package prefixes real tools live in (Homebrew/MacPorts/local) and
;; the shared system config dir (TLS CAs, resolv.conf/hosts — needed once net is
;; on). All read-only + world-readable code/config: no user secrets, and writes
;; here stay denied (only the session roots are RW). A Homebrew-linked `bash`
;; that can't read its own libreadline aborts before main, so this must be broad
;; enough to actually launch tools.
(def ^:private macos-system-read-roots
  ["/usr" "/bin" "/sbin" "/System" "/Library" "/private/var/db/dyld" "/private/var/select"
   "/private/etc" "/opt/homebrew" "/usr/local" "/opt/local"])

;; Directory-EXISTENCE probes (home lookup, dyld, cwd resolution) need metadata on
;; the traversal ancestors of granted roots. These are `literal` (the directory
;; itself, revealing only that it exists) — never `subpath` — so a confined child
;; can stat `$HOME` and `/Users` but cannot read the size/mtime of
;; `~/.ssh/id_ed25519` and other secrets beneath them. This scoped metadata grant
;; replaces a former global `(allow file-read-metadata)` that leaked file existence
;; + size + mtime for every path on the host.
(def ^:private macos-metadata-literals
  ;; A `delay`, never an eager read: `native-image` initializes this namespace at
  ;; BUILD time, so `user.home`/`java.io.tmpdir` would be the BUILDER's.
  (delay (into ["/" "/Users" "/Volumes" "/private" "/opt" "/etc" "/var" "/tmp" "/home"]
               (remove nil?)
               [(System/getProperty "user.home") (System/getProperty "java.io.tmpdir")])))

;; Linux read-only system roots a confined child needs to launch a real toolchain
;; (dynamic loader, shared libs, shell, and -- once net is on -- TLS CAs + resolver
;; config under /etc). All world-readable code/config, bind-mounted read-only; no
;; user secrets (those live under $HOME, which is NOT bound unless it is a session
;; root). `*-try` variants tolerate a path missing on a given distro.
(def ^:private linux-system-read-roots
  ["/usr" "/bin" "/sbin" "/lib" "/lib64" "/lib32" "/etc" "/opt" "/nix" "/run" "/var/lib"])

(defn- sbpl-quote
  [s]
  (str "\""
       (-> (str s)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\""))
       "\""))

(defn- subpaths [roots] (str/join (map #(str "(subpath " (sbpl-quote %) ")") roots)))

(defn- ancestor-dirs
  "Every ancestor directory of `p`, from its parent up to `/`, as absolute
   strings. Canonicalizing a path under a granted root lstats/readlinks each
   component on the way DOWN, so every ancestor needs `file-read-metadata` even
   though only the root itself carries `file-read*`/`file-write*`. Without this a
   confined child cannot `getCanonicalPath` a file it just created under a root
   whose ancestors aren't otherwise granted — most notably the darwin per-user
   temp dir (`/private/var/folders/<hash>/T`), whose `/private/var`,
   `/private/var/folders`, ... chain is granted nowhere else."
  [p]
  (loop
    [cur
     (when-let [s (not-empty p)]
       (.getParentFile (java.io.File. ^String s)))

     acc
     []]

    (if cur
      (recur (.getParentFile ^java.io.File cur) (conj acc (.getPath ^java.io.File cur)))
      acc)))

(defn macos-profile
  "Compile a Seatbelt (SBPL) profile string from a RESOLVED policy map
   `{:rw [..] :ro [..] :deny-write [..] :deny-read [..] :net-enabled? <bool>}`
   (all paths already canonical). Rules are emitted in Seatbelt's LAST-match-wins
   order: allow reads (system + rw + ro), allow writes (rw), then the deny carve-
   outs so `:deny-write`/`:deny-read` win over the allows. One-line string for
   `sandbox-exec -p`."
  ^String
  [{:keys [rw ro deny-write deny-read deny-exec net-enabled? proxy-port loopback-port inbound-ports
           mach-services]}]
  (let
    [rw
     (->> rw
          (keep real-path)
          distinct
          vec)

     ro
     (->> ro
          (keep real-path)
          distinct
          vec)

     dw
     (->> deny-write
          (keep deny-path)
          distinct
          vec)

     dr
     (->> deny-read
          (keep deny-path)
          distinct
          vec)

     dex
     (->> deny-exec
          (keep deny-path)
          distinct
          vec)]

    (str
      "(version 1)"
      "(import \"system.sb\")"
      "(deny default)"
      "(allow process-fork process-exec)"
      "(allow sysctl-read)"
      ;; GraalVM Native Image uses a named POSIX semaphore for signal delivery on
      ;; macOS. Without this narrow IPC permission, tools such as spel and bb abort
      ;; during VM startup before their main function runs.
      "(allow ipc-posix-sem)"
      ;; Seatbelt denies EVERY Mach lookup by default, which is what breaks macOS
      ;; Keychain reads (`security`, `gh auth token`, `git credential-osxkeychain`)
      ;; inside the jail. Only the services the operator granted are opened.
      (when-let [ms (seq (distinct (remove str/blank? (filter string? mach-services))))]
        (str "(allow mach-lookup"
             (apply str (map #(str "(global-name " (sbpl-quote %) ")") ms))
             ")"))
      "(allow file-read-metadata"
      (apply str
        (map #(str "(literal " (sbpl-quote %) ")")
             (distinct (concat @macos-metadata-literals (mapcat ancestor-dirs (concat rw ro))))))
      (subpaths (concat macos-system-read-roots rw ro))
      ")"
      "(allow file-read*"
      (subpaths macos-system-read-roots)
      "(literal \"/dev/null\")(literal \"/dev/zero\")(literal \"/dev/random\")(literal \"/dev/urandom\"))"
      (when (seq ro) (str "(allow file-read*" (subpaths ro) ")"))
      "(allow file-read* file-write*"
      "(literal \"/dev/null\")(literal \"/dev/tty\")(literal \"/dev/stdout\")(literal \"/dev/stderr\")"
      (subpaths rw)
      ")"
      (when (seq dw) (str "(deny file-write*" (subpaths dw) ")"))
      (when (seq dr) (str "(deny file-read*" (subpaths dr) ")"))
      ;; Block EXECUTION of specific binaries (`jail.deny-exec`). Overrides the
      ;; blanket `(allow process-fork process-exec)` above; a plain file-read deny
      ;; does NOT stop exec on macOS (the kernel maps a signed/allowed binary
      ;; without a file-read* check), so this is the real command block.
      (when (seq dex) (str "(deny process-exec*" (subpaths dex) ")"))
      ;; Network: a proxy endpoint is the sole outbound destination. A managed
      ;; nREPL — and any explicitly allowlisted dev/server port — additionally
      ;; needs to bind a server socket. Seatbelt's `network-bind` accepts the
      ;; address class but not a reliable host:port constraint, so bind is limited
      ;; to local IP sockets while inbound traffic is restricted, port by port, to
      ;; the preselected nREPL port plus each `:inbound-ports` entry. Binding is
      ;; broad (any local port); ACCEPTING a connection is the gated capability.
      (let
        [inbound
         (->> (cons loopback-port inbound-ports)
              (remove nil?)
              distinct)

         server-rules
         (when (seq inbound)
           (str "(allow network-bind (local ip))"
                (apply str
                  (map (fn [p]
                         (str "(allow network-inbound (local ip \"*:" p "\"))"))
                       inbound))))]

        (cond proxy-port (str "(deny network*)"
                              server-rules
                              "(allow network-outbound (remote ip \"localhost:"
                              proxy-port
                              "\"))")
              net-enabled? "(allow network*)"
              :else (str "(deny network*)" server-rules))))))

(defn compile-policy
  "Resolve a raw jail policy VALUE into the canonical map `macos-profile` consumes:
   read the LIVE session roots via `:roots-fn`, add the always-writable temp dirs,
   fold in the environment-snapshotted `:allow-read-write`/`:allow-write`/
   `:allow-read`/`:deny-write`/`:deny-read` paths (home-expanded + realpath'd).
   Called per spawn, so each child gets the CURRENT live roots without re-reading
   model-writable project config. `:allow-read-write` is the concise equivalent of
   granting the same path through both legacy allow lists."
  [{:keys [roots-fn net-enabled? allow-read-write allow-write allow-read deny-write deny-read
           deny-exec proxy-port loopback-port inbound-ports mach-services]}]
  (let
    [session-roots
     (when roots-fn (try (roots-fn) (catch Throwable _ nil)))

     tmps
     [(System/getProperty "java.io.tmpdir") "/tmp"]

     rw
     (->> (concat session-roots tmps allow-read-write allow-write)
          (keep real-path)
          distinct
          vec)

     ro
     (->> (concat allow-read-write allow-read)
          (keep real-path)
          distinct
          vec)

     ;; Extra inbound ports are sanitized to distinct integers in the legal TCP
     ;; range; anything else (nil, junk, out-of-range) is dropped so a bad config
     ;; value can never widen the profile or corrupt the emitted SBPL.
     inbound-ports
     (->> inbound-ports
          (keep (fn [p]
                  (let
                    [n (cond (integer? p) (long p)
                             (string? p) (parse-long (str/trim p)))]
                    (when (and n (<= 1 n 65535)) n))))
          distinct
          vec)]

    {:rw rw
     :ro ro
     :deny-write (vec deny-write)
     :deny-read (vec deny-read)
     :deny-exec (vec deny-exec)
     :net-enabled? (boolean net-enabled?)
     :proxy-port proxy-port
     :loopback-port loopback-port
     :inbound-ports inbound-ports
     :mach-services
     (into [] (comp (filter string?) (remove str/blank?) (distinct)) mach-services)}))

(def ^:private keychain-denial-markers
  "How the macOS Security framework reports a lookup it could not complete. The
   first two are exactly what a DENIED Mach lookup produces under Seatbelt."
  ["SecKeychainSearchCreateFromAttributes" "SecKeychainItemCopyContent"
   "errSecInteractionNotAllowed" "User interaction is not allowed"
   "could not be found in the keychain"])

(defn keychain-denial?
  "True when captured output shows a macOS Keychain lookup that failed the way a
   denied Mach lookup fails. Pure text test; `keychain-denial-hint` decides
   whether confinement is the explanation."
  [out err]
  (let [text (str out "\n" err)]
    (boolean (some #(str/includes? text %) keychain-denial-markers))))

(defn keychain-denial-hint
  "One actionable line when a command's output shows a Keychain lookup THIS jail
   denied, else nil. Silent when the jail is off (`:disabled?`) or the keychain
   services are already granted — the failure is then a real Keychain miss and
   naming the sandbox would send the caller the wrong way."
  [{:keys [disabled? mach-services]} out err]
  (when (and (not disabled?)
             (not (some #{"com.apple.SecurityServer"} mach-services))
             (keychain-denial? out err))
    (str "Keychain lookup blocked by the sandbox: Seatbelt denies every Mach lookup by default."
         " Set jail.mach_services.keychain: true in config to grant com.apple.SecurityServer,"
         " com.apple.ocspd and com.apple.trustd.agent plus read access to the keychain"
         " databases.")))

(defn linux-bwrap-args
  "Compile the bubblewrap flag vector (ending in `--`) from a RESOLVED policy map
   (the same shape `macos-profile` consumes). Filesystem: read-only bind the system
   toolchain roots + `:ro`, read-write bind `:rw` (session roots + tmp), then
   re-bind `:deny-write` read-only and mask `:deny-read` (empty tmpfs for dirs,
   /dev/null for files) -- later binds win, so the denies override the allows. Only
   bound paths exist inside the child, so everything else on the host (e.g. `~/.ssh`)
   is simply absent. `:deny-exec` binaries are masked with /dev/null (a char device),
   so `execve` fails EACCES -- the Linux equivalent of macOS's `(deny process-exec*)`;
   it is bound AFTER the allow binds so a denied binary inside an allowed `:ro` root
   is still blocked. Network: for FILTERED egress (`:proxy-port` set) pasta wraps
   the bwrap child, giving it a private net namespace that reaches ONLY the host
   gateway proxy port (see `linux-pasta`) -- exact macOS parity; bwrap then SHARES
   that namespace (no `--unshare-net`). Without pasta, filtered egress degrades to
   `--unshare-net` (no egress at all, safe). Net-off also `--unshare-net`; an
   explicitly-open network (`net-enabled?` with no proxy, e.g. a managed nREPL)
   shares the host network namespace."
  ^java.util.List
  [{:keys [rw ro deny-write deny-read deny-exec net-enabled? proxy-port loopback-port]}]
  (let
    [rw
     (->> rw
          (keep real-path)
          distinct
          vec)

     ro
     ;; System roots are bound at their LITERAL path (not real-path'd): on merged-usr
     ;; distros `/lib`,`/lib64`,`/bin`,`/sbin` are symlinks into `/usr`, and the ELF
     ;; interpreter is hardcoded (`/lib64/ld-linux-x86-64.so.2`, `/lib/ld-linux-aarch64.so.1`).
     ;; Canonicalizing them collapses the loader mount point so EVERY binary fails to
     ;; exec (ENOENT on its interpreter). `--ro-bind-try` tolerates any absent on a distro.
     ;; User `:ro` allow-read paths stay canonicalized for dedup/symlink safety.
     (->> (concat linux-system-read-roots (keep real-path ro))
          distinct
          vec)

     dw
     (->> deny-write
          (keep deny-path)
          distinct
          vec)

     dex
     (->> deny-exec
          (keep deny-path)
          distinct
          vec)

     ro-flags
     (mapcat (fn [p]
               ["--ro-bind-try" p p])
             ro)

     rw-flags
     (mapcat (fn [p]
               ["--bind-try" p p])
             rw)

     dw-flags
     (mapcat (fn [p]
               ["--ro-bind-try" p p])
             dw)

     dr-flags
     (mapcat (fn [p]
               (let [rp (deny-path p)]
                 (cond (nil? rp) nil
                       (.isDirectory (File. ^String rp)) ["--tmpfs" rp]
                       :else ["--ro-bind-try" "/dev/null" rp])))
             (distinct deny-read))

     ;; Mask each denied binary with /dev/null (a char device): `execve` on it fails
     ;; (exit 126). Bound AFTER the allow binds so it wins over a binary inside an
     ;; allowed `:ro` root -- the Linux equivalent of macOS `(deny process-exec*)`.
     ;; On merged-usr distros the same binary is reachable via BOTH `/usr/bin/<n>` and
     ;; `/bin/<n>` (distinct bwrap mounts), so masking only the canonical path leaves
     ;; the PATH alias runnable -- mask every EXISTING bin-dir alias of the basename.
     ;; `--ro-bind-try` aborts if the destination is absent on a read-only bind, so the
     ;; alias set is filtered to files that actually exist on the host.
     dex-flags
     (mapcat (fn [p]
               (let [n (.getName (File. ^String p))]
                 (->> (cons p
                            (map #(str % "/" n)
                                 ["/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"
                                  "/usr/local/sbin"]))
                      (filter #(.exists (File. ^String %)))
                      distinct
                      (mapcat (fn [t]
                                ["--ro-bind-try" "/dev/null" t])))))
             dex)

     ;; Network. proxy-port set = FILTERED egress: when pasta is present it gives
     ;; the child a private net ns reaching ONLY the host proxy port, and bwrap
     ;; SHARES that ns (no --unshare-net). Without pasta, filtered egress degrades
     ;; to a full no-egress wall rather than leaving the child open. net-off and
     ;; the no-pasta fallback both --unshare-net; an explicitly-open network
     ;; (net-enabled? with no proxy, e.g. a managed nREPL) shares the host ns.
     pasta?
     (boolean (and proxy-port linux-pasta))

     net
     (cond pasta? []
           proxy-port ["--unshare-net"]
           net-enabled? []
           :else ["--unshare-net"])

     bwrap-args
     ;; ABSOLUTE enforcer path — the very binary `supported?` validated. A bare name
     ;; would be resolved by PATH at exec time (and, under the detach prefix, by the
     ;; CHILD's scrubbed PATH), so a shadowing shim earlier on PATH could replace the
     ;; jail with an arbitrary program, and a PATH without it would fail the launch.
     (vec (concat [(or linux-bwrap "bwrap") "--die-with-parent" "--proc" "/proc" "--dev" "/dev"]
                  ro-flags
                  rw-flags
                  dw-flags
                  dr-flags
                  dex-flags
                  net
                  ["--"]))]

    (if pasta?
      ;; pasta wraps bwrap: `-T <port>` forwards the child's loopback proxy port to
      ;; the host (egress); `-t <loopback-port>` forwards the host INBOUND to the
      ;; child's loopback port so vis can attach to a managed nREPL bound inside the
      ;; ns (else `-t none`); `-u none -U none` disable UDP. So the ONLY reachable
      ;; destination is the gateway proxy, and the only inbound is the nREPL port.
      (into (vec (concat [(or linux-pasta "pasta") "-T" (str proxy-port)]
                         (if loopback-port ["-t" (str loopback-port)] ["-t" "none"])
                         ["-u" "none" "-U" "none" "--"]))
            bwrap-args)
      bwrap-args)))

(defonce ^:private unenforceable-warned (atom false))

(defn- warn-unenforceable!
  "Emit ONE loud stderr line when a jail was requested (`jail.enabled: true` -> an enabled
   policy) but this host cannot enforce it, so the operator learns the child is
   running UNCONFINED instead of the failure being silent. Deduped per process."
  [reason]
  (when (compare-and-set! unenforceable-warned false true)
    (binding [*out* *err*]
      (println (str "vis WARNING: the jail is enabled but CANNOT be enforced on this host -- "
                    reason
                    ". Shell/REPL children run UNCONFINED (full host access). "
                    "See `vis-docs sandbox`.")))))

(defonce ^:private no-pasta-warned (atom false))

(defn- warn-no-pasta!
  "Emit ONE loud stderr line on Linux when FILTERED egress was requested (a proxy
   policy) but pasta is absent, so the operator learns the child has NO network
   (egress denied entirely) instead of the degradation being silent. `passt`
   provides pasta. Deduped per process."
  []
  (when (compare-and-set! no-pasta-warned false true)
    (binding [*out* *err*]
      (println
        (str "vis WARNING: Linux filtered egress needs pasta (from `passt`) but it is "
             "not installed -- jailed shell/REPL children get NO network (egress denied "
             "entirely). Install it (e.g. `apt-get install passt`). See `vis-docs sandbox`.")))))

(defn wrap-argv
  "Given the base executor argv and a jail POLICY value, return the argv to spawn.
   macOS wraps the first managed process with `sandbox-exec` (descendants inherit
   the kernel policy and are left unwrapped, since Seatbelt rejects a second
   application). Linux wraps with bubblewrap (`bwrap`) namespaces. When a jail is
   requested but the host cannot enforce it, the child is passed through UNWRAPPED
   but a loud one-time warning fires -- `jail.enabled: true` never silently no-ops."
  [argv policy]
  (let [wanted? (and policy (not (:disabled? policy)))]
    (if (and wanted? (supported?) (not (inherited-jail?)))
      (case (os-kind)
        :macos
        ;; Absolute path (see `bwrap-args`): never a PATH lookup for the enforcer.
        (into [macos-sandbox-exec "-p" (macos-profile (compile-policy policy))] argv)

        :linux
        (let [pol (compile-policy policy)]
          (when (and (:proxy-port pol) (not linux-pasta)) (warn-no-pasta!))
          (into (linux-bwrap-args pol) argv))

        argv)
      (do (when (and wanted? (not (inherited-jail?)))
            (when-let [reason (unenforceable-reason)]
              (warn-unenforceable! reason)))
          argv))))

(defn- path-executable
  "Absolute path of `exe` on THIS process' PATH, or nil. Resolved from the daemon's
   own environment because a jailed child's env is REPLACED by the allowlist, so a
   bare program name may no longer resolve at exec time."
  ^String [^String exe]
  (some (fn [dir]
          (let [f (File. ^String dir ^String exe)]
            (when (and (.isFile f) (.canExecute f)) (.getPath f))))
        (remove str/blank? (str/split (str (System/getenv "PATH")) #":"))))

(defn- execs-in-place?
  "Probe a detacher: run `prefix … /bin/sh -c 'exit 77'` and demand 77 back.
   A detacher is only usable when it `exec`s the command IN PLACE — util-linux
   `setsid` forks when it cannot become a session leader and the parent exits 0,
   which would silently replace every command's exit status with a lie. False on
   a platform without `/bin/sh` (Windows), which just leaves the prefix off."
  [prefix]
  (try (let
         [p (.start (doto (ProcessBuilder. ^java.util.List
                                           (into (vec prefix) ["/bin/sh" "-c" "exit 77"]))
                      (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                      (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)))]
         (try (and (.waitFor p 5 TimeUnit/SECONDS) (= 77 (.exitValue p)))
              (finally (try (.close (.getOutputStream p)) (catch Throwable _ nil))
                       (when (.isAlive p) (.destroyForcibly p)))))
       (catch Throwable _ false)))

(def ^:private detach-argv
  "argv PREFIX that runs a child in its OWN process group, or `[]`.

   The daemon is its own process-group leader and a plain `ProcessBuilder` child
   inherits that group plus the controlling terminal (measured: child pgid ==
   gateway pid). So a tool that signals its OWN group — `kill 0`, `kill -- -$$`,
   a harness or build script tearing down its children, Ctrl-C in the tab a
   foreground daemon was started from — delivered that SIGINT/SIGTERM to the
   GATEWAY as well: the shutdown hook drained and cancelled every other session's
   live turn, and the log made it look like a deliberate stop.

   Each candidate `exec`s in place (verified once by [[execs-in-place?]]), so the
   pid `ProcessBuilder` hands back is still the real command: exit status, stdio
   pipes and every `ProcessHandle` kill path are unchanged. Empty when no
   candidate exists — the child then shares the group exactly as before."
  (delay (or (some (fn [prefix]
                     (when (and prefix (execs-in-place? prefix)) prefix))
                   [(when-let [perl (path-executable "perl")]
                      [perl "-e" "setpgrp(0,0); exec { $ARGV[0] } @ARGV or die $!" "--"])
                    (when-let [setsid (path-executable "setsid")]
                      [setsid])])
             [])))

(defn detached-argv
  "`argv` prefixed so the spawned child LEADS ITS OWN process group instead of
   inheriting the daemon's — the containment that keeps a child's group-directed
   signal (or the terminal's) away from the gateway JVM. Unchanged argv on a
   platform with no in-place detacher.

   Applies OUTSIDE [[wrap-argv]]: the detacher only `setpgid`s and `exec`s, so
   everything that actually RUNS is still inside the jail."
  [argv]
  (into (vec @detach-argv) argv))

(defn- java-proxy-options
  [{:keys [proxy-port java-trust-store java-trust-store-password java-proxy? loopback-port]}]
  (when (and java-proxy? proxy-port)
    (str (when loopback-port
           ;; Keep the nREPL listener on AF_INET; its launcher binds 127.0.0.1 and
           ;; the Seatbelt profile admits inbound traffic only on `loopback-port`.
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
  "Environment additions for a confined child. `VIS_SEATBELT_ACTIVE` records that
   the kernel policy is already inherited, preventing invalid nested
   `sandbox-exec` calls. When a gateway proxy endpoint is present, common proxy and
   CA variables cover curl/git/Python/Bun/etc.; managed JVM children additionally
   receive proxy + ephemeral truststore properties through JAVA_TOOL_OPTIONS."
  [policy]
  (if (:disabled? policy)
    {}
    (let
      [jail-env
       (if (and policy (or (inherited-jail?) (supported?))) {"VIS_SEATBELT_ACTIVE" "1"} {})]
      (if-let [port (:proxy-port policy)]
        (let
          [token (:proxy-token policy)
           url (str "http://" (when token (str token "@")) "127.0.0.1:" port)
           ;; SOCKS5 shares the SAME loopback port (multiplexed by first byte).
           ;; `ALL_PROXY` is the fallback for non-HTTP schemes (ssh/git+ssh/db/raw
           ;; TCP) — it points at the SOCKS lane, while `http(s)_proxy` keep the
           ;; HTTP proxy so HTTPS verb/path MITM is preserved for web traffic.
           socks-url (str "socks5h://" (when token (str token "@")) "127.0.0.1:" port)
           ca (:ca-file policy)
           java-opts (java-proxy-options policy)]

          (cond->
            (merge jail-env
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
  "Non-secret operator vars a confined child may inherit. Everything else in the
   operator environment — API keys, tokens, credentials — is dropped; a variable
   a child legitimately needs is DECLARED in `environment:` and arrives as a
   resolved value (`:env-values`), not by widening this list."
  #{"PATH" "HOME" "USER" "LOGNAME" "SHELL" "LANG" "LANGUAGE" "TERM" "TERMINFO" "TZ" "TMPDIR" "PWD"
    "HOSTNAME" "COLORTERM" "DISPLAY"})

(def ^:private env-passthrough-prefixes ["LC_"])

;; ── Variables that hijack the PRE-JAIL exec chain ──────────────────────────────
;;
;; The spawned argv is not the command: it is `perl -e 'setpgrp; exec …'` (the
;; detacher, see `detach-argv`) exec'ing `sandbox-exec`/`bwrap`, which only THEN
;; installs the kernel policy. Every one of those hops runs with the CHILD's
;; environment while still UNCONFINED, so a variable that makes a program run
;; code at startup — before it reaches `main` — is a complete jail bypass.
;;
;; Measured on macOS: with `PERL5OPT=-Mevil PERL5LIB=…` in the child env, the
;; detacher ran the module and wrote a file in `$HOME` that the very same jailed
;; argv was denied (`WRITE-DENIED`, no file) without it.
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
   code in the unconfined detacher/enforcer before the jail exists.

   Returns nil when the policy is not enforcing — the caller keeps the parent
   environment and merges [[child-env-additions]] instead (unjailed
   platforms/`jail.enabled: false`), so non-confined behavior is unchanged."
  [policy]
  (when (and policy (not (:disabled? policy)) (or (inherited-jail?) (supported?)))
    (let
      [inherited (into {}
                       (filter (fn [[k _]]
                                 (env-passthrough? k)))
                       (System/getenv))]
      ;; Total: the scrub also covers the declared + proxy additions, so no later
      ;; edit to either can reintroduce a pre-exec hijack name.
      (into {}
            (remove (fn [[k _]]
                      (pre-exec-hijack? k)))
            (merge inherited (declared-env policy) (proxy-env policy))))))

(defn child-env-additions
  "What an UNCONFINED child gets ON TOP of the inherited host environment: the
   resolved project environment (workspace `.env` + `environment:` declarations)
   plus this session's proxy + CA variables. They apply whether or not the jail
   is enabled — the project says where a variable comes from, and the jail only
   decides what ELSE a child may keep."
  [policy]
  (merge (declared-env policy) (proxy-env policy)))

;; ── Standard language-process jail contract ────────────────────────────────
;; Language packs spawn managed REPLs and project test runners via raw
;; ProcessBuilder calls outside the shell executors. Every such spawn obtains its
;; argv + proxy environment from `session-process-launch`, which resolves one live
;; per-session policy atomically and fails closed when the session is unavailable.

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
                  (update :allow-write #(vec (concat % language-process-runtime-dirs)))
                  (update :allow-read
                          #(vec (concat % repl-toolchain-read-dirs language-process-runtime-dirs)))
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
    (let
      [policy (try (policy-fn)
                   (catch Throwable t
                     (throw (ex-info "Managed language process denied: session jail policy failed"
                                     {:type ::session-jail-failed :session-id session-id}
                                     t))))]
      (when-not policy
        (throw (ex-info "Managed language process denied: session jail policy is unavailable"
                        {:type ::session-jail-missing :session-id session-id})))
      policy)))

(defn session-process-launch
  "THE language-surface launch contract. Atomically resolve `session-id`'s policy
   and return `{:argv ... :env ...}` for one managed language subprocess.

   `:loopback-port` is reserved for a managed nREPL's preselected local listener.
   Unknown, nil, disposed, or failing sessions are denied before spawn; the
   contract never silently returns a naked argv. On an OS without a supported
   enforcer, `:argv` keeps its jail shape unchanged (the platform capability gap is
   explicit), while the policy lookup still must succeed.

   `:argv` is additionally [[detached-argv]]-prefixed: a managed REPL/interpreter
   is long-lived, third-party and often runs test harnesses that signal their own
   process group, and it used to inherit the DAEMON's group (measured: managed
   nREPL pgid == gateway pid). One `kill 0` inside it took the gateway down with
   it, cancelling every other session's live turn."
  ([session-id argv] (session-process-launch session-id argv nil))
  ([session-id argv {:keys [loopback-port]}]
   (let
     [policy
      (language-process-policy (session-base-policy! session-id) loopback-port)

      full
      (jailed-child-env policy)]

     ;; Confined child ⇒ FULL scrubbed env replaces the operator's (secrets dropped);
     ;; unenforced platform ⇒ additions merged onto the inherited env (unchanged).
     (if full
       {:argv (detached-argv (wrap-argv argv policy)) :env full :replace-env? true}
       {:argv (detached-argv (wrap-argv argv policy))
        :env (child-env-additions policy)
        :replace-env? false}))))
