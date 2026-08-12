(ns com.blockether.vis.internal.process-jail-test
  "The OS process jail: the SBPL compiler is asserted as pure data, and — on a
   macOS host that can actually enforce Seatbelt — a real wrapped `bash` proves
   containment end to end (workspace RW allowed; outside-read, write-outside,
   deny-write carve-outs, and network all denied). The policy is a PER-SESSION
   VALUE threaded into `wrap-argv`, never a process-global singleton."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer
             [deftest is testing thrown? thrown-with-msg?]]
            [com.blockether.vis.internal.process-jail :as pj]))

(deftest macos-profile-compiler
  (testing "net OFF denies sockets; net ON allows them"
    (is (str/includes? (pj/macos-profile {:rw [] :net-enabled? false}) "(deny network*)"))
    (is (str/includes? (pj/macos-profile {:rw [] :net-enabled? true}) "(allow network*)")))
  (testing "default-deny base with the dyld/system import (else binaries abort)"
    (let [p (pj/macos-profile {:rw [] :net-enabled? false})]
      (is (str/includes? p "(deny default)"))
      (is (str/includes? p "(import \"system.sb\")"))))
  (testing "native-image signal delivery may open its POSIX semaphore"
    (is (str/includes? (pj/macos-profile {:rw [] :net-enabled? false}) "(allow ipc-posix-sem)")))
  (testing "resolvable RW roots become subpath rules on their REAL path"
    (let
      [dir
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-" (System/nanoTime)))
         (.mkdirs))

       real
       (.getCanonicalPath dir)

       p
       (pj/macos-profile {:rw [(.getPath dir)] :net-enabled? false})]

      (is (str/includes? p (str "(subpath \"" real "\")"))
          "rule must template the canonical path, not the raw /tmp path")
      (.delete dir)))
  (testing "deny-write / deny-read emit deny rules AFTER the allows (last-match-wins)"
    (let
      [dir
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-fs-" (System/nanoTime)))
         (.mkdirs))

       sub
       (doto (io/file dir "protected") (.mkdirs))

       realsub
       (.getCanonicalPath sub)

       p
       (pj/macos-profile {:rw [(.getPath dir)] :deny-write [(.getPath sub)] :net-enabled? false})

       allow-idx
       (str/index-of p (str "(subpath \"" (.getCanonicalPath dir) "\")"))

       deny-idx
       (str/last-index-of p (str "(deny file-write*(subpath \"" realsub "\")"))]

      (is (str/includes? p (str "(deny file-write*(subpath \"" realsub "\")")))
      (is (and allow-idx deny-idx (< allow-idx deny-idx))
          "the deny-write carve-out must come after the RW allow so it wins")
      (io/delete-file sub true)
      (io/delete-file dir true))))

(deftest inbound-ports-gate-accept
  (testing "no inbound ports: a jailed shell child cannot accept on any port"
    (let [p (pj/macos-profile {:rw [] :net-enabled? false})]
      (is (str/includes? p "(deny network*)"))
      (is (not (str/includes? p "network-inbound"))
          "without an allowlist there is no inbound rule at all")))
  (testing "allowlisted ports emit one port-gated inbound rule each; bind is local-only"
    (let [p (pj/macos-profile {:rw [] :net-enabled? false :inbound-ports [5273 4200]})]
      (is (str/includes? p "(allow network-bind (local ip))"))
      (is (str/includes? p "(allow network-inbound (local ip \"*:5273\"))"))
      (is (str/includes? p "(allow network-inbound (local ip \"*:4200\"))"))
      (is (not (str/includes? p "network-outbound"))
          "an inbound allowlist never grants outbound egress")))
  (testing "the managed nREPL loopback port and inbound ports coexist, de-duplicated"
    (let
      [p (pj/macos-profile
           {:rw [] :net-enabled? false :loopback-port 5273 :inbound-ports [5273 6000]})]
      (is (= 2 (count (re-seq #"network-inbound" p)))
          "duplicate loopback/inbound port collapses to one rule")
      (is (str/includes? p "(allow network-inbound (local ip \"*:6000\"))"))))
  (testing "compile-policy sanitizes to distinct legal integers, dropping junk/out-of-range"
    (let
      [resolved (pj/compile-policy {:roots-fn (constantly [])
                                    :inbound-ports [5273 "4200" 0 70000 "nope" 5273 nil]})]
      (is (= [5273 4200] (:inbound-ports resolved))))))

(deftest compile-policy-resolves-live-roots
  (testing "session roots-fn + tmp become the RW set, allow-read → :ro"
    (let
      [dir
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-cp-" (System/nanoTime)))
         (.mkdirs))

       resolved
       (pj/compile-policy
         {:roots-fn (constantly [(.getPath dir)]) :net-enabled? true :allow-read []})

       canon
       (.getCanonicalPath dir)]

      (is (contains? (set (:rw resolved)) canon) "workspace root is writable")
      (is (some #(str/includes? % "tmp") (:rw resolved)) "tmp dirs are always writable")
      (is (true? (:net-enabled? resolved)))
      (io/delete-file dir true))))

(deftest compile-policy-supports-concise-read-write-grants
  (testing "allow-read-write grants the path through both canonical access sets"
    (let
      [dir
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-rw-" (System/nanoTime)))
         (.mkdirs))

       canon
       (.getCanonicalPath dir)

       resolved
       (pj/compile-policy {:roots-fn (constantly []) :allow-read-write [(.getPath dir)]})]

      (try (is (contains? (set (:rw resolved)) canon))
           (is (contains? (set (:ro resolved)) canon))
           (finally (io/delete-file dir true))))))

(deftest wrap-argv-is-off-by-default
  (testing "nil policy => argv passes through untouched"
    (is (= ["bash" "-lc" "echo hi"] (pj/wrap-argv ["bash" "-lc" "echo hi"] nil)))))

(defn- run-jailed
  [argv]
  (let
    [pb
     (doto (ProcessBuilder. ^java.util.List argv) (.redirectErrorStream true))

     p
     (.start pb)

     out
     (slurp (.getInputStream p))]

    {:exit (.waitFor p) :out out}))

(defn- linux? [] (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "linux"))

(defn- basename
  "Last path segment of an argv head. The jail emits the ABSOLUTE enforcer binary,
   so shape assertions compare the program NAME, never a PATH-resolved bare word."
  [^String s]
  (.getName (io/file s)))

(def ^:private linux-sandbox-applicable?
  ;; Hosted Linux runners can expose bwrap but deny the namespace syscalls it
  ;; needs (for example, by blocking loopback setup or uid-map writes). Cache an
  ;; actual net-off launch so real E2E checks run whenever enforcement is viable
  ;; and do not mistake a present binary for an available kernel capability.
  (delay (when (and (linux?) (pj/supported?))
           (try (zero? (:exit (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc" "true"]
                                                        {:roots-fn (constantly [])
                                                         :net-enabled? false}))))
                (catch Throwable _ false)))))

(defn- sandbox-applicable?
  "True only when this JVM may apply a fresh, working OS jail.

   Managed test JVMs already inherit Seatbelt (`VIS_SEATBELT_ACTIVE=1`), and a
   nested profile cannot strengthen or replace that kernel policy. On Linux,
   bubblewrap additionally needs namespace privileges that some hosted runners
   deny despite shipping the executable."
  []
  (and (pj/supported?)
       (not= "1" (System/getenv "VIS_SEATBELT_ACTIVE"))
       (or (not (linux?)) @linux-sandbox-applicable?)))

(deftest macos-e2e-runner-contract
  (when (= "1" (System/getenv "VIS_REQUIRE_MACOS_SANDBOX_E2E"))
    (is (pj/supported?) "required macOS E2E runner must provide sandbox-exec")
    (is (sandbox-applicable?)
        "required macOS E2E runner must be an unconfined host JVM, not a managed jailed child")))

(deftest real-containment
  ;; sandbox-exec cannot apply a nested profile from an already Seatbelt-confined
  ;; test JVM, so execute this OS integration check only when a probe can apply one.
  (when (and (pj/supported?) (sandbox-applicable?))
    (let
      [ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-ws-" (System/nanoTime)))
         (.mkdirs))

       protected
       (doto (io/file ws "protected") (.mkdirs))

       secret
       (io/file ws "secret.txt")

       wsc
       (.getCanonicalPath ws)

       protc
       (.getCanonicalPath protected)

       policy
       {:roots-fn (constantly [(.getPath ws)])
        :net-enabled? false
        :deny-write [(.getPath protected)]
        :deny-read [(.getPath secret)]}]

      (spit (io/file ws "inside.txt") "workspace-ok")
      (spit secret "TOP-SECRET")
      (try (testing "reads + writes inside the workspace succeed"
             (let
               [r (run-jailed
                    (pj/wrap-argv
                      ["bash" "--noprofile" "--norc" "-lc"
                       (str "cat " wsc "/inside.txt && echo x > " wsc "/w.txt && echo WROTE")]
                      policy))]
               (is (zero? (:exit r)))
               (is (str/includes? (:out r) "workspace-ok"))
               (is (str/includes? (:out r) "WROTE"))))
           (testing "deny-write protects a subtree inside an otherwise writable root"
             (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                        (str "echo nope > " protc "/blocked.txt 2>&1")]
                                       policy))
             (is (not (.exists (io/file protected "blocked.txt")))))
           (testing "deny-read protects a file inside an otherwise readable root"
             (let
               [r (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                             (str "cat " (.getCanonicalPath secret) " 2>&1")]
                                            policy))]
               (is (not (str/includes? (:out r) "TOP-SECRET")))))
           (testing "network is denied when the policy is net-off"
             (let
               [r (run-jailed
                    (pj/wrap-argv
                      ["bash" "--noprofile" "--norc" "-lc"
                       "curl -sS --max-time 4 https://example.com -o /dev/null && echo GOTNET"]
                      policy))]
               (is (not (str/includes? (:out r) "GOTNET")))))
           (testing "deny-exec blocks execution of a binary while a sibling still runs"
             (let
               [blocked
                (io/file ws "blocked-bin")

                allowed
                (io/file ws "allowed-bin")]

               (io/copy (io/file "/bin/date") blocked)
               (io/copy (io/file "/bin/date") allowed)
               (.setExecutable blocked true)
               (.setExecutable allowed true)
               (let
                 [pol
                  (assoc policy :deny-exec [(.getPath blocked)])

                  rb
                  (run-jailed (pj/wrap-argv [(.getCanonicalPath blocked) "+%Y"] pol))

                  ra
                  (run-jailed (pj/wrap-argv [(.getCanonicalPath allowed) "+%Y"] pol))]

                 (is (not (zero? (:exit rb))) "deny-exec must block the named binary")
                 (is (zero? (:exit ra)) "a sibling binary still executes"))
               (io/delete-file blocked true)
               (io/delete-file allowed true)))
           (finally (io/delete-file (io/file ws "inside.txt") true)
                    (io/delete-file (io/file ws "w.txt") true)
                    (io/delete-file secret true)
                    (io/delete-file protected true)
                    (io/delete-file ws true))))))

(deftest proxy-env-vars
  (testing "a confined child is marked even when it has no proxy endpoint"
    (let [expected (if (pj/supported?) {"VIS_SEATBELT_ACTIVE" "1"} {})]
      (is (= expected (pj/proxy-env {})))
      (is (= expected (pj/proxy-env {:net-enabled? true})))))
  (testing ":proxy-port sets both-case proxy vars, and NO CA vars without a :ca-file"
    (let
      [e
       (pj/proxy-env {:proxy-port 4321})

       url
       "http://127.0.0.1:4321"

       socks
       "socks5h://127.0.0.1:4321"]

      ;; http(s) keep the HTTP proxy (MITM verb/path); all_proxy = the SOCKS lane
      ;; for non-HTTP schemes (ssh/git+ssh/db/raw TCP) on the same loopback port.
      (doseq [k ["http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY"]]
        (is (= url (get e k)) k))
      (doseq [k ["all_proxy" "ALL_PROXY"]]
        (is (= socks (get e k)) k))
      (when (pj/supported?) (is (= "1" (get e "VIS_SEATBELT_ACTIVE"))))
      (is (not (contains? e "CURL_CA_BUNDLE")))
      (is (not (contains? e "SSL_CERT_FILE")))))
  (testing ":proxy-token rides the proxy URL userinfo (session attribution)"
    (let
      [e
       (pj/proxy-env {:proxy-port 4321 :proxy-token "tok-123"})

       url
       "http://tok-123@127.0.0.1:4321"

       socks
       "socks5h://tok-123@127.0.0.1:4321"]

      (doseq [k ["http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY"]]
        (is (= url (get e k)) k))
      (doseq [k ["all_proxy" "ALL_PROXY"]]
        (is (= socks (get e k)) k))))
  (testing "with a :ca-file EVERY common CA-trust var points at the ephemeral CA PEM"
    ;; The MITM tier mints per-host leaves off an ephemeral CA; each runtime reads a
    ;; different trust var, so the full set (sandbox-runtime's nine) must be covered
    ;; or that runtime silently fails the handshake instead of trusting the proxy.
    (let
      [ca
       "/tmp/vis-ca.pem"

       e
       (pj/proxy-env {:proxy-port 4321 :ca-file ca})]

      (doseq
        [v ["CURL_CA_BUNDLE" "SSL_CERT_FILE" "REQUESTS_CA_BUNDLE" "NODE_EXTRA_CA_CERTS"
            "GIT_SSL_CAINFO" "PIP_CERT" "AWS_CA_BUNDLE" "CARGO_HTTP_CAINFO" "DENO_CERT"]]
        (is (= ca (get e v)) (str v " must point at the CA PEM"))))))

(deftest repl-jail-contract
  (testing "language policy preserves the network wall and adds toolchain access"
    (let
      [base
       {:roots-fn (constantly ["/tmp"])
        :net-enabled? false
        :proxy-port 999
        :proxy-token "shell-token"
        :repl-proxy-port 1000
        :repl-ca-file "/repl-ca.pem"
        :java-trust-store "/repl-ca.p12"
        :java-trust-store-password "secret"
        :ca-file "/shell-ca.pem"
        :allow-write ["/w"]
        :allow-read ["/r"]}

       rp
       (pj/repl-policy base 54321)

       tool
       (pj/language-process-policy base nil)]

      (is (false? (:net-enabled? rp)))
      (is (= 1000 (:proxy-port rp)))
      (is (nil? (:proxy-token rp)))
      (is (= "/repl-ca.pem" (:ca-file rp)))
      (is (= 54321 (:loopback-port rp)))
      (is (nil? (:loopback-port tool)))
      (is (not (some #{"~/.m2"} (:allow-write rp)))) ; caches are opt-in, no default
      (is (some #{"~/.vis/logs"} (:allow-write rp)))
      (is (some #{"~/.vis/logs"} (:allow-read tool)))
      (is (some #{"/w"} (:allow-write rp)))))
  (testing "unknown and disposed sessions fail closed before spawn"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"session jail is not registered"
                          (pj/session-process-launch "no-such-session" ["clojure" "-M"])))
    (is (thrown? clojure.lang.ExceptionInfo (pj/session-process-launch nil ["python3"]))))
  (testing "one atomic contract returns jailed argv plus session-attributed proxy env"
    (pj/register-session-jail! "t-sid"
                               (constantly {:roots-fn (constantly ["/tmp"])
                                            :net-enabled? false
                                            :repl-proxy-port 999
                                            :repl-ca-file "/tmp/repl-ca.pem"
                                            :java-trust-store "/tmp/repl-ca.p12"
                                            :java-trust-store-password "secret"}))
    (try
      (let
        [{:keys [argv env]}
         (pj/session-process-launch "t-sid" ["clojure" "-M:x"] {:loopback-port 54321})

         ;; Every managed process is spawned into its OWN process group, so the jail
         ;; wrapper starts AFTER that prefix. A managed REPL used to sit in the
         ;; DAEMON's group, and one `kill 0` from a test harness inside it stopped
         ;; the gateway, cancelling every other session's live turn.
         detach
         (vec @@#'pj/detach-argv)

         jailed
         (vec (drop (count detach) argv))]

        (is (= detach (vec (take (count detach) argv)))
            "the launch contract must hand back a process-group-detached argv")
        (is (= "http://127.0.0.1:999" (get env "HTTPS_PROXY")))
        (is (= "/tmp/repl-ca.pem" (get env "SSL_CERT_FILE")))
        (is (re-find #"-Dhttps\.proxyPort=999" (get env "JAVA_TOOL_OPTIONS")))
        (is (re-find #"-Djava\.net\.preferIPv4Stack=true" (get env "JAVA_TOOL_OPTIONS")))
        (is (re-find #"-Djavax\.net\.ssl\.trustStore=/tmp/repl-ca\.p12"
                     (get env "JAVA_TOOL_OPTIONS")))
        (cond (and (linux?) (pj/supported?))
              ;; Linux: proxy-port present => pasta lane wraps bwrap; the managed
              ;; nREPL's loopback port is forwarded INBOUND (`-t <port>`) so vis attaches.
              (do (is (= "pasta" (basename (first jailed))) "linux repl jail must wrap with pasta")
                  (let
                    [av
                     jailed

                     ti
                     (.indexOf ^java.util.List av "-t")]

                    (is (and (pos? ti) (= "54321" (nth av (inc ti))))
                        "the nREPL loopback port must be pasta -t forwarded inbound")))
              (sandbox-applicable?)
              (do (is (= "/usr/bin/sandbox-exec" (first jailed))
                      "the macOS enforcer is the absolute system binary, never a PATH lookup")
                  (is (not (re-find #"\(allow network\*\)" (nth jailed 2))))
                  (is (re-find #"network-bind \(local ip\)" (nth jailed 2)))
                  (is (str/includes? (nth jailed 2) "network-inbound (local ip \"*:54321\")"))
                  (is (re-find #"localhost:999" (nth jailed 2))))
              :else (is (= ["clojure" "-M:x"] jailed))))
      (finally (pj/unregister-session-jail! "t-sid")))))

(deftest env-scrub-allowlist
  (testing
    "a confined child inherits ONLY the non-secret allowlist plus the RESOLVED
            `environment:` declarations; every operator secret is dropped and the
            proxy/CA additions are present"
    (let
      [policy
       {:roots-fn (fn []
                    [(System/getProperty "java.io.tmpdir")])
        :net-enabled? false
        ;; The value, not just the name: this is where a `dotenv:`/`keychain:`
        ;; declaration reaches the child. `jail.env` could never carry one.
        :env-values {"MY_DECLARED_TOKEN" "from-dotenv"}}

       env
       (pj/jailed-child-env policy)

       real
       (into {} (System/getenv))

       secretish
       (filter #(re-find #"(?i)key|token|secret|password" %) (keys real))]

      (is (map? env))
      (is (contains? env "PATH"))
      (is (contains? env "HOME"))
      (is (= "1" (get env "VIS_SEATBELT_ACTIVE")))
      (is (= "from-dotenv" (get env "MY_DECLARED_TOKEN"))
          "a declared variable reaches the confined child with its resolved value")
      (is (empty? (filter env (remove #{"MY_DECLARED_TOKEN"} secretish)))
          "no UNDECLARED API key / token / secret / password var may reach a jailed child")))
  (testing "an unconfined child gets the same declarations as plain additions"
    (is (= {"MY_DECLARED_TOKEN" "from-dotenv"}
           (pj/child-env-additions {:disabled? true
                                    :env-values {"MY_DECLARED_TOKEN" "from-dotenv"
                                                 "BLANK_NAME" nil}}))))
  (testing "nil when the policy is not enforcing (disabled / nil) — caller inherits"
    (is (nil? (pj/jailed-child-env nil)))
    (is (nil? (pj/jailed-child-env {:disabled? true
                                    :roots-fn (fn []
                                                ["/x"])})))))

(deftest jail-environment-inherit-mode
  (testing
    "`jail.environment: inherit` (`:inherit-host-env?`) keeps the operator's
            ambient environment in a confined child, while the default keeps only the
            allowlist — everything else about the jail is unchanged"
    (let
      [ambient
       (into {} (System/getenv))

       ;; A real ambient name the default mode must drop: not on the passthrough
       ;; allowlist, not a pre-exec hijack name.
       outsider
       (first (remove #(or (#'pj/env-passthrough? %) (#'pj/pre-exec-hijack? %)) (keys ambient)))

       policy
       {:roots-fn (constantly [])
        :net-enabled? false
        :env-values {"MY_DECLARED_TOKEN" "from-dotenv"}}

       declared-env
       (pj/jailed-child-env policy)

       inherited-env
       (pj/jailed-child-env (assoc policy :inherit-host-env? true))]

      (is (some? outsider) "the host must export something outside the allowlist to test with")
      (is (not (contains? declared-env outsider))
          "the DEFAULT mode drops every ambient variable that is not a non-secret basic")
      (is (= (get ambient outsider) (get inherited-env outsider))
          "`inherit` hands the confined child the ambient value verbatim")
      (is (= "from-dotenv" (get inherited-env "MY_DECLARED_TOKEN"))
          "the project's own environment still applies on top under `inherit`")
      (is (= "1" (get inherited-env "VIS_SEATBELT_ACTIVE"))
          "`inherit` is an ENVIRONMENT mode only: the child is still confined")))
  (testing "a pre-exec hijack name is refused under `inherit` too — that scrub is the jail itself"
    (let
      [env (pj/jailed-child-env {:roots-fn (constantly [])
                                 :net-enabled? false
                                 :inherit-host-env? true
                                 :env-values {"LD_PRELOAD" "/tmp/x.so" "PERL5OPT" "-Mevil"}})]
      (is (empty? (filter #'pj/pre-exec-hijack? (keys env)))))))

(deftest metadata-scoped-to-roots
  (testing
    "file-read-metadata is scoped: no global grant; ancestors are literals,
            granted roots are subpaths, and $HOME is NOT recursively exposed"
    (let
      [p (pj/macos-profile (pj/compile-policy {:roots-fn (fn []
                                                           ["/tmp"])
                                               :net-enabled? false}))]
      (is (nil? (re-find #"\(allow file-read-metadata\)" p))
          "the former GLOBAL metadata grant (the leak) must be gone")
      (is (str/includes? p "file-read-metadata(literal \"/\")"))
      (is (str/includes? p (str "(literal \"" (System/getProperty "user.home") "\")")))
      (is (not (re-find #"file-read-metadata[^\n]*subpath \"[^\"]*\.ssh" p))
          "metadata must not recurse into ~/.ssh and other home secrets")))
  (testing
    "a granted root's ancestor directories are metadata literals so a
            confined child can canonicalize (lstat every component of) a path it
            creates under, e.g. the darwin per-user temp dir (/private/var/folders/..)"
    (let
      [dir
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-anc-" (System/nanoTime)))
         (.mkdirs))

       real
       (.getCanonicalPath dir)

       p
       (pj/macos-profile (pj/compile-policy {:roots-fn (fn []
                                                         [(.getPath dir)])
                                             :net-enabled? false}))

       ancestors
       (loop
         [f
          (.getParentFile (io/file real))

          acc
          []]

         (if f (recur (.getParentFile f) (conj acc (.getPath f))) acc))]

      (try
        ;; Every resolved ancestor — /private/var, /private/var/folders, <hash>, … —
        ;; must carry a metadata literal; without the full chain getCanonicalPath
        ;; EPERMs on the first ungranted component.
        (doseq [anc ancestors]
          (is (str/includes? p (str "(literal \"" anc "\")"))
              (str "ancestor not granted metadata: " anc)))
        (finally (.delete dir))))))

(deftest linux-bwrap-compiler
  ;; Pure argv compilation — runs on EVERY OS (incl. macOS + Linux CI), no kernel
  ;; needed. Asserts the bubblewrap flag vector the Linux jail hands the executor.
  (let
    [root
     (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-lx-" (System/nanoTime)))
       (.mkdirs))

     prot
     (doto (io/file root "protected") (.mkdirs))

     rp
     (.getCanonicalPath root)

     pp
     (.getCanonicalPath prot)

     base
     {:rw [(.getPath root)] :ro [] :deny-write [(.getPath prot)] :deny-read [(.getPath prot)]}

     off
     (pj/linux-bwrap-args (assoc base :net-enabled? false))

     open
     (pj/linux-bwrap-args (assoc base :net-enabled? true))

     dex
     (pj/linux-bwrap-args (assoc base
                            :net-enabled? false
                            :deny-exec ["/bin/sh"]))

     pairs
     (partition 2 1 off)]

    (try
      (testing "argv shape: starts with bwrap, ends with the -- separator"
        (is (= "bwrap" (basename (first off))))
        (is (= "--" (last off))))
      (testing "session root is bind-mounted read-write" (is (some #(= % ["--bind-try" rp]) pairs)))
      (testing "system toolchain roots are read-only bind-mounted (else nothing launches)"
        (is (some #(= % ["--ro-bind-try" "/usr"]) pairs)))
      (testing "deny-write is re-bound read-only AFTER the rw bind (deny wins)"
        (is (some #(= % ["--ro-bind-try" pp]) pairs))
        (let
          [ai
           (.indexOf ^java.util.List off rp)

           di
           (.lastIndexOf ^java.util.List off pp)]

          (is (and (pos? ai) (pos? di) (< ai di)))))
      (testing "deny-read is masked with an empty tmpfs" (is (some #(= % ["--tmpfs" pp]) pairs)))
      (testing "net OFF gets the --unshare-net kernel wall (safe)"
        (is (some #{"--unshare-net"} off)))
      (testing "filtered egress (proxy-port): pasta lane vs no-pasta fallback"
        (let
          [no-pasta
           (with-redefs [pj/linux-pasta nil]
             (pj/linux-bwrap-args (assoc base
                                    :net-enabled? true
                                    :proxy-port 51000)))

           pasta
           (with-redefs [pj/linux-pasta "/usr/bin/pasta"]
             (pj/linux-bwrap-args (assoc base
                                    :net-enabled? true
                                    :proxy-port 51000)))]

          (is (some #{"--unshare-net"} no-pasta)
              "no pasta => filtered egress degrades to the no-egress wall (safe)")
          (is (not= "pasta" (basename (first no-pasta))))
          (is (= ["/usr/bin/pasta" "--quiet" "--log-file"] (take 3 pasta))
              "pasta wraps bwrap by ABSOLUTE path and says nothing on the child's stdio")
          ;; pasta prefixes the argv, so anything it prints IS the command's output:
          ;; --quiet drops the informational half and the log file takes the rest.
          (is (str/includes? (nth pasta 3) "/logs/pasta-")
              "pasta's own diagnostics belong in this process' log directory")
          (is (= ["-T" "51000" "-t" "none" "-u" "none" "-U" "none" "--"] (subvec (vec pasta) 4 13))
              "forwarding ONLY the proxy port")
          (is (= "bwrap" (basename (nth pasta 13))) "pasta hands off to bwrap")
          (is (nil? (some #{"--unshare-net"} pasta))
              "pasta provides the restricted ns; bwrap shares it (no --unshare-net)")
          (is (some #(= % ["--bind-try" rp]) (partition 2 1 pasta))
              "the filesystem jail still applies inside the pasta lane")))
      (testing "an explicitly-open network shares the host namespace (no --unshare-net)"
        (is (nil? (some #{"--unshare-net"} open))))
      (testing
        "deny-exec masks the binary with /dev/null so execve fails (macOS process-exec* parity)"
        (is (some (fn [[a _]]
                    (= "/dev/null" a))
                  (partition 2 1 dex))
            "deny-exec must emit a /dev/null mask bind for the binary")
        (is (not (some (fn [[a _]]
                         (= "/dev/null" a))
                       (partition 2 1 off)))
            "no /dev/null mask without deny-exec (deny-read here is a dir => tmpfs)"))
      (finally (io/delete-file prot true) (io/delete-file root true)))))

(deftest unenforceable-fails-loud
  ;; A requested jail on a host that cannot enforce it must NOT silently pass the
  ;; child through pretending safety: `wrap-argv` returns argv UNWRAPPED, and
  ;; `unenforceable-reason` explains why (so callers can warn loudly).
  (with-redefs [pj/supported? (constantly false)]
    (is (= ["bash" "-lc" "echo hi"]
           (pj/wrap-argv ["bash" "-lc" "echo hi"] {:roots-fn (constantly [])}))
        "unsupported host => passthrough, never a fake-jailed argv")
    (is (some? (pj/unenforceable-reason))
        "unsupported host must give a reason, so `jail.enabled: true` is not a silent no-op")))

(deftest wsl-detection
  ;; WSL2 runs a real kernel (bwrap + pasta work) => treated as ordinary Linux.
  ;; WSL1 has NO real namespaces => must be reported unenforceable (fail loud),
  ;; never silently passed through. The kernel osrelease is the discriminator.
  (testing "WSL1 kernel (a `Microsoft` build with NO `WSL2` marker) is detected"
    (with-redefs [pj/linux-osrelease (constantly "4.4.0-19041-Microsoft")]
      (is (true? (#'pj/wsl1?)))))
  (testing "WSL2 real-kernel build is NOT flagged WSL1"
    (with-redefs [pj/linux-osrelease (constantly "5.15.153.1-microsoft-standard-WSL2")]
      (is (false? (#'pj/wsl1?)))))
  (testing "native Linux is NOT flagged WSL1"
    (with-redefs [pj/linux-osrelease (constantly "6.8.0-52-generic")]
      (is (false? (#'pj/wsl1?))))))

(deftest linux-e2e-runner-contract
  ;; Local/foreign hosts may legitimately skip kernel E2E. The Linux CI job opts
  ;; into this contract, so its conditional E2E cases can NEVER become vacuous.
  (when (= "1" (System/getenv "VIS_REQUIRE_LINUX_SANDBOX_E2E"))
    (is (linux?) "required Linux E2E runner must be Linux")
    (is (pj/supported?) "required Linux E2E runner must have bubblewrap installed")
    (is (sandbox-applicable?) "required Linux E2E runner must permit a real bubblewrap namespace")
    (is (some #(.canExecute (io/file ^String %))
              ["/usr/bin/pasta" "/bin/pasta" "/usr/local/bin/pasta"])
        "required Linux E2E runner must provide pasta from the passt package")))

(deftest linux-real-containment
  ;; Real bubblewrap enforcement — runs ONLY on a Linux host with bwrap (i.e. the
  ;; ubuntu CI job). Proves a wrapped bash reads its workspace but CANNOT read a
  ;; secret outside the bound roots (which simply does not exist inside the jail).
  (when (and (linux?) (sandbox-applicable?))
    (let
      [ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-bwrap-ws-" (System/nanoTime)))
         (.mkdirs))

       wsc
       (.getCanonicalPath ws)

       _
       (spit (io/file ws "ok.txt") "WORKSPACE-OK")

       secret
       (io/file (System/getProperty "user.home") (str ".vis-bwrap-secret-" (System/nanoTime)))

       _
       (spit secret "TOP-SECRET-DATA")

       sc
       (.getCanonicalPath secret)

       policy
       {:roots-fn (constantly [wsc]) :net-enabled? false :deny-exec ["/bin/ls"]}

       argv
       (pj/wrap-argv ["bash" "-lc"
                      (str "cat " wsc
                           "/ok.txt; echo ---; cat " sc
                           " 2>&1 || true"
                           "; echo ===; ls / >/dev/null 2>&1 && echo LS-RAN || echo LS-BLOCKED")]
                     policy)]

      (try (is (= "bwrap" (basename (first argv))) "linux jail must bwrap-wrap the child")
           (let [{:keys [out]} (run-jailed argv)]
             (is (str/includes? out "WORKSPACE-OK")
                 "workspace file must be readable inside the jail")
             (is (not (str/includes? out "TOP-SECRET-DATA"))
                 "a secret outside the bound roots must be absent inside the jail")
             (is (and (str/includes? out "LS-BLOCKED") (not (str/includes? out "LS-RAN")))
                 "deny-exec must block execve of the masked binary (macOS process-exec* parity)"))
           (finally (io/delete-file (io/file ws "ok.txt") true)
                    (io/delete-file ws true)
                    (io/delete-file secret true))))))

(defn- pasta-present?
  []
  (some #(.canExecute (io/file ^String %)) ["/usr/bin/pasta" "/bin/pasta" "/usr/local/bin/pasta"]))

(deftest linux-pasta-filtered-egress
  ;; Real pasta+bwrap FILTERED egress — runs ONLY on a Linux host with bwrap AND
  ;; pasta (the ubuntu CI job installs `passt`). Proves a jailed child reaches ONLY
  ;; the gateway proxy port (via pasta `-T`), while a sibling loopback port (the
  ;; would-be control plane) and the public internet are both unreachable. This is
  ;; the Linux equivalent of the macOS "only the proxy port" Seatbelt rule.
  (when (and (linux?) (sandbox-applicable?) (pasta-present?))
    (let
      [proxy-srv
       (java.net.ServerSocket. 0 16 (java.net.InetAddress/getByName "127.0.0.1"))

       ctrl-srv
       (java.net.ServerSocket. 0 16 (java.net.InetAddress/getByName "127.0.0.1"))

       proxy-port
       (.getLocalPort proxy-srv)

       ctrl-port
       (.getLocalPort ctrl-srv)

       ;; both servers send a marker byte-string immediately on accept, then close
       accept!
       (fn [^java.net.ServerSocket ss ^String marker]
         (future (try (loop []

                        (let [s (.accept ss)]
                          (doto (.getOutputStream s) (.write (.getBytes marker)) (.flush))
                          (.close s))
                        (recur))
                      (catch Throwable _ nil))))

       _
       (accept! proxy-srv "PROXY-OK")

       _
       (accept! ctrl-srv "CTRL-OK")

       ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-pasta-ws-" (System/nanoTime)))
         (.mkdirs))

       wsc
       (.getCanonicalPath ws)

       probe
       (str "P=$(timeout 4 bash -c 'exec 3<>/dev/tcp/127.0.0.1/"
            proxy-port
            " && head -c8 <&3' 2>/dev/null); echo \"proxy=[$P]\"; "
            "timeout 4 bash -c 'exec 3<>/dev/tcp/127.0.0.1/"
            ctrl-port
            " && head -c8 <&3' 2>/dev/null && echo CTRL-REACHED || echo CTRL-BLOCKED; "
            "timeout 4 bash -c 'exec 3<>/dev/tcp/1.1.1.1/443' 2>/dev/null "
            "&& echo NET-REACHED || echo NET-BLOCKED")

       policy
       {:roots-fn (constantly [wsc]) :net-enabled? true :proxy-port proxy-port}

       argv
       (pj/wrap-argv ["bash" "-lc" probe] policy)]

      (try (is (= "pasta" (basename (first argv))) "filtered egress must wrap the child with pasta")
           (let [{:keys [out]} (run-jailed argv)]
             (is (str/includes? out "PROXY-OK")
                 "the child must reach the gateway proxy port through pasta's -T forward")
             (is (str/includes? out "CTRL-BLOCKED")
                 "a sibling loopback port (control plane) must be unreachable")
             (is (str/includes? out "NET-BLOCKED")
                 "the public internet must be unreachable (proxy-only egress)"))
           (finally (.close proxy-srv) (.close ctrl-srv) (io/delete-file ws true))))))

(deftest detached-argv-gives-a-child-its-own-process-group
  (testing "the prefix leaves the command intact"
    (let [detach (vec @@#'pj/detach-argv)]
      (is (= ["python3" "-i"] (vec (drop (count detach) (pj/detached-argv ["python3" "-i"])))))
      (is (= detach (vec (take (count detach) (pj/detached-argv ["python3" "-i"])))))))
  (testing "a spawned child LEADS its own group, and the detacher exec's in place"
    ;; Non-vacuous: the same command spawned WITHOUT the prefix inherits this JVM's
    ;; group — which is exactly how a child's `kill 0` used to reach the gateway.
    (let
      [pgid-of
       (fn [argv]
         (let
           [p
            (.start (ProcessBuilder. ^java.util.List
                                     (into
                                       (vec argv)
                                       ["/bin/sh" "-c"
                                        "echo \"$$ $(ps -o pgid= -p $$ | tr -d ' ')\"; exit 7"])))

            out
            (slurp (.getInputStream p))

            code
            (.waitFor p)]

           (into (vec (str/split (str/trim out) #"\s+")) [code])))

       detach
       (vec @@#'pj/detach-argv)]

      (let [[pid pgid] (pgid-of [])]
        (is (not= pid pgid) "a plain spawn inherits our group"))
      (when (seq detach)
        (let [[pid pgid code] (pgid-of (pj/detached-argv []))]
          (is (= pid pgid) "a detached child must lead its own process group")
          (is (= 7 code) "an exec'ing detacher preserves the command's own exit status"))))))

(defn- run-with-env
  "Spawn `argv` exactly as the launch contract does: merged stdio, and — when the
   policy is enforcing — the scrubbed environment REPLACING the operator's."
  [argv env]
  (let [pb (doto (ProcessBuilder. ^java.util.List (vec argv)) (.redirectErrorStream true))]
    (when env
      (let [^java.util.Map e (.environment pb)]
        (.clear e)
        (.putAll e ^java.util.Map env)))
    (let
      [^Process p (.start pb)
       out (slurp (.getInputStream p))]

      {:exit (.waitFor p) :out out})))

(defn- host-pgid
  "Process-group id of `pid`, read from THIS (unconfined) JVM — a jailed child
   cannot exec `ps` itself."
  [pid]
  (let
    [^Process p
     (.start (doto (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" (str "ps -o pgid= -p " pid)])
               (.redirectErrorStream true)))

     out
     (str/trim (slurp (.getInputStream p)))]

    (.waitFor p)
    out))

(deftest enforcer-is-an-absolute-validated-binary
  ;; The jail wrapper is exec'd THROUGH the process-group detach prefix, so a bare
  ;; program name would be resolved by PATH at exec time — the CHILD's scrubbed
  ;; PATH. A shim earlier on PATH could then stand in for the enforcer, and a PATH
  ;; without it would break every launch. Emit the exact binary `supported?`
  ;; validated, by absolute path, so no lookup happens at all.
  (testing "linux bwrap/pasta are the discovered install locations"
    (with-redefs
      [pj/linux-bwrap
       "/usr/local/bin/bwrap"

       pj/linux-pasta
       "/usr/bin/pasta"]

      (let [av (pj/linux-bwrap-args {:rw [] :net-enabled? true :proxy-port 51000})]
        (is (= "/usr/bin/pasta" (first av)))
        (is (= "/usr/local/bin/bwrap" (nth av 13))))))
  (when (and (not (linux?)) (sandbox-applicable?))
    (testing "macOS wraps with /usr/bin/sandbox-exec itself"
      (let [av (pj/wrap-argv ["bash" "-lc" "true"] {:roots-fn (constantly []) :net-enabled? false})]
        (is (= "/usr/bin/sandbox-exec" (first av)))
        (is (.canExecute (io/file ^String (first av))))))))

(deftest a-detached-child-is-still-fully-jailed
  ;; Containment and detachment are applied TOGETHER by the launch contract:
  ;; `detached-argv(wrap-argv(argv))` plus the scrubbed env. The prefix only
  ;; setpgid()s and exec's the enforcer, so everything that actually runs must
  ;; still be confined — asserted here on the composed argv, not on either half.
  (when (sandbox-applicable?)
    (let
      [ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-det-" (System/nanoTime)))
         (.mkdirs))

       wsc
       (.getCanonicalPath ws)

       secret
       (io/file (System/getProperty "user.home") (str ".vis-jail-det-secret-" (System/nanoTime)))

       escape
       (io/file (System/getProperty "user.home") (str ".vis-jail-det-escape-" (System/nanoTime)))

       policy
       {:roots-fn (constantly [wsc]) :net-enabled? false}

       env
       (pj/jailed-child-env policy)

       script
       (str
         "echo in > "
         wsc
         "/inside.txt && echo WROTE-INSIDE; "
         "(echo out > "
         (.getCanonicalPath escape)
         ") 2>/dev/null && echo ESCAPED || echo WRITE-DENIED; "
         "cat "
         (.getCanonicalPath secret)
         " 2>/dev/null || echo READ-DENIED; "
         "curl -sS --max-time 4 https://example.com -o /dev/null 2>/dev/null && echo GOTNET || echo NET-DENIED; "
         "exit 9")

       argv
       (pj/detached-argv (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc" script] policy))]

      (spit secret "TOP-SECRET-DATA")
      (try (testing "the jail still confines a detached child"
             (let [{:keys [exit out]} (run-with-env argv env)]
               (is (str/includes? out "WROTE-INSIDE") "the session root stays writable")
               (is (str/includes? out "WRITE-DENIED") "a write outside the roots is refused")
               (is (not (.exists escape)) "and it must not land on disk either")
               (is (not (str/includes? out "TOP-SECRET-DATA"))
                   "a file outside the roots stays unreadable")
               (is (str/includes? out "NET-DENIED") "net-off is enforced")
               (is (= 9 exit)
                   "the detacher exec's in place: the command's own exit status survives")))
           (testing "and that same jailed child leads its OWN process group"
             ;; Non-vacuous: the identical jailed argv spawned WITHOUT the prefix
             ;; sits in this JVM's group — how a child's `kill 0` reached the daemon.
             (when (seq @@#'pj/detach-argv)
               (let
                 [sleeper
                  (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc" "sleep 5"] policy)

                  start
                  (fn [av]
                    (let
                      [pb
                       (doto (ProcessBuilder. ^java.util.List (vec av)) (.redirectErrorStream true))

                       ^java.util.Map e
                       (.environment pb)]

                      (.clear e)
                      (.putAll e ^java.util.Map env)
                      (.start pb)))

                  ^Process detached
                  (start (pj/detached-argv sleeper))

                  ^Process plain
                  (start sleeper)]

                 (try (Thread/sleep 400)
                      (is (= (str (.pid detached)) (host-pgid (.pid detached)))
                          "a detached+jailed child must lead its own process group")
                      (is (= (host-pgid (.pid (java.lang.ProcessHandle/current)))
                             (host-pgid (.pid plain)))
                          "without the prefix the same jailed child sits in OUR group")
                      (finally (.destroyForcibly detached) (.destroyForcibly plain))))))
           (finally (io/delete-file (io/file ws "inside.txt") true)
                    (io/delete-file ws true)
                    (io/delete-file secret true)
                    (io/delete-file escape true))))))

(deftest a-path-shim-cannot-hijack-the-enforcer
  ;; End-to-end consequence of naming the enforcer absolutely: a program with the
  ;; enforcer's NAME, first on the child's PATH, must never be the thing that runs.
  ;; The detach prefix exec's by PATH lookup, and the jailed child's environment is
  ;; the scrubbed one, so a bare name here would hand the whole jail to the shim.
  (when (sandbox-applicable?)
    (let
      [tmp
       (System/getProperty "java.io.tmpdir")

       ws
       (doto (io/file tmp (str "vis-jail-hijack-ws-" (System/nanoTime))) (.mkdirs))

       policy
       {:roots-fn (constantly [(.getCanonicalPath ws)]) :net-enabled? false}

       jailed
       (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc" "echo RAN-JAILED"] policy)

       enforcer
       (basename (first jailed))

       shim-dir
       (doto (io/file tmp (str "vis-jail-shim-" (System/nanoTime))) (.mkdirs))

       shim
       (io/file shim-dir enforcer)

       env
       (assoc (pj/jailed-child-env policy)
         "PATH" (str (.getCanonicalPath shim-dir) ":/usr/bin:/bin"))]

      (spit shim "#!/bin/sh\necho PWNED-JAIL-BYPASSED\nexec \"$@\"\n")
      (.setExecutable shim true)
      (try (testing "non-vacuity: a bare name on this PATH really does resolve to the shim"
             (let
               [{:keys [out]} (run-with-env ["/bin/sh" "-c" (str enforcer " /bin/echo control")]
                                            env)]
               (is (str/includes? out "PWNED-JAIL-BYPASSED"))))
           (testing "yet the launch argv still runs the validated enforcer"
             (let [{:keys [out]} (run-with-env (pj/detached-argv jailed) env)]
               (is (not (str/includes? out "PWNED-JAIL-BYPASSED"))
                   "the enforcer must never be resolved through the child's PATH")
               (is (str/includes? out "RAN-JAILED") "and the jailed command still runs")))
           (finally (io/delete-file shim true)
                    (io/delete-file shim-dir true)
                    (io/delete-file ws true))))))

(deftest pre-exec-hijack-vars-are-refused-even-when-opted-in
  ;; The jail is not installed by the process the daemon spawns: `detached-argv`
  ;; puts `perl -e 'setpgrp; exec …'` in front, and that perl — plus `sandbox-exec`
  ;; / `bwrap` themselves — run UNCONFINED with the child's environment. Any
  ;; variable that makes one of those hops execute code at startup is a full jail
  ;; bypass, so the allowlist must refuse it ahead of every opt-in path.
  (testing "no startup-code variable passes, not even as an `environment:` declaration"
    (doseq
      [k ["PERL5OPT" "PERL5LIB" "PERLLIB" "PERL5DB" "LD_PRELOAD" "LD_AUDIT" "LD_LIBRARY_PATH"
          "DYLD_INSERT_LIBRARIES" "DYLD_LIBRARY_PATH" "BASH_ENV" "BASH_FUNC_x%%" "ENV" "SHELLOPTS"
          "IFS" "GCONV_PATH" "LOCPATH" "NLSPATH" "HOSTALIASES"]]
      (is (#'pj/pre-exec-hijack? k) (str k " must be recognised as a pre-exec hijack"))
      (is (not (#'pj/env-passthrough? k)) (str k " must not pass through the ambient scrub"))
      (is (empty? (pj/declared-env {:env-values {k "payload"}}))
          (str k " must not pass through even when `environment:` declares it"))))
  (testing "and the ordinary allowlist is untouched"
    (is (#'pj/env-passthrough? "PATH"))
    (is (#'pj/env-passthrough? "LC_ALL"))
    (is (not (#'pj/env-passthrough? "AWS_SECRET_ACCESS_KEY"))))
  (testing "the produced environment carries none of them"
    (when-let
      [env (pj/jailed-child-env {:roots-fn (constantly [])
                                 :net-enabled? false
                                 :env-values {"PERL5OPT" "-Mevil" "LD_PRELOAD" "/tmp/x.so"}})]
      (is (empty? (filter #'pj/pre-exec-hijack? (keys env)))))))

(deftest a-pre-jail-env-hijack-cannot-escape-the-jail
  ;; End-to-end consequence, measured against a directory the jail is told to
  ;; DENY writes to: code running inside the jail cannot touch it, so the marker
  ;; file appearing there means something ran BEFORE the sandbox was installed.
  (when (and (sandbox-applicable?)
             (= "perl" (basename (or (first (pj/detached-argv ["/bin/sh"])) ""))))
    (let
      [tmp
       (System/getProperty "java.io.tmpdir")

       ws
       (doto (io/file tmp (str "vis-jail-hijack-env-ws-" (System/nanoTime))) (.mkdirs))

       outside
       (doto (io/file ws "no-write-here") (.mkdirs))

       marker
       (io/file outside "ESCAPED")

       lib
       (doto (io/file ws "perllib") (.mkdirs))

       policy
       {:roots-fn (constantly [(.getCanonicalPath ws)])
        :net-enabled? false
        :deny-write [(.getCanonicalPath outside)]}

       argv
       (pj/detached-argv (pj/wrap-argv ["/bin/sh" "-c" "echo RAN-JAILED"] policy))

       clean
       (pj/jailed-child-env policy)

       hostile
       (assoc clean
         "PERL5OPT" "-Mvishijack"
         "PERL5LIB" (.getCanonicalPath lib))]

      (spit (io/file lib "vishijack.pm")
            (str "package vishijack; sub import { system('/usr/bin/touch','"
                 (.getCanonicalPath marker)
                 "'); } 1;\n"))
      (try (testing "the jailed command itself cannot write there"
             (let
               [{:keys [out]} (run-with-env
                                (pj/detached-argv
                                  (pj/wrap-argv
                                    ["/bin/sh" "-c"
                                     (str "touch '"
                                          (.getCanonicalPath marker)
                                          "' 2>/dev/null && echo WROTE || echo WRITE-DENIED")]
                                    policy))
                                clean)]
               (is (str/includes? out "WRITE-DENIED"))
               (is (not (.exists marker)))))
           (testing "non-vacuity: left in the environment, the var really does escape the jail"
             (.delete marker)
             (run-with-env argv hostile)
             (is (.exists marker)
                 "the unconfined detacher must be the thing this test defends against"))
           (testing "so the environment the launch contract builds must drop it"
             (.delete marker)
             (let
               [{:keys [out]} (run-with-env argv
                                            (into {}
                                                  (remove (fn [[k _]]
                                                            (#'pj/pre-exec-hijack? k)))
                                                  hostile))]
               (is (not (.exists marker)) "no code may run before the jail is installed")
               (is (str/includes? out "RAN-JAILED") "and the jailed command still runs")))
           (finally (io/delete-file marker true)
                    (io/delete-file (io/file lib "vishijack.pm") true)
                    (io/delete-file lib true)
                    (io/delete-file outside true)
                    (io/delete-file ws true))))))

;; ── macOS Mach services (#90) ────────────────────────────────────────────────
;; Seatbelt denies EVERY Mach lookup by default, which is what makes `security`,
;; `gh auth token` and `git credential-osxkeychain` fail inside the jail with an
;; opaque Security-framework message.

(deftest macos-profile-mach-services
  (testing "no grant => no mach-lookup rule at all"
    (is (not (str/includes? (pj/macos-profile {:rw [] :net-enabled? false}) "mach-lookup"))))
  (testing "granted services become one global-name each inside a single allow"
    (let
      [p (pj/macos-profile {:rw []
                            :net-enabled? false
                            :mach-services ["com.apple.SecurityServer" "com.apple.ocspd"]})]
      (is (str/includes? p "(allow mach-lookup"))
      (is (str/includes? p "(global-name \"com.apple.SecurityServer\")"))
      (is (str/includes? p "(global-name \"com.apple.ocspd\")"))))
  (testing "compile-policy sanitizes: strings only, blanks and duplicates dropped"
    (is (= ["com.apple.SecurityServer"]
           (:mach-services (pj/compile-policy {:roots-fn (constantly [])
                                               :mach-services ["com.apple.SecurityServer"
                                                               "com.apple.SecurityServer" "" nil
                                                               7]}))))
    (is (= [] (:mach-services (pj/compile-policy {:roots-fn (constantly [])}))))))

(deftest keychain-denial-hint-explains-a-denied-lookup
  (testing "a Security-framework failure under a live jail names the config key"
    (let
      [hint (pj/keychain-denial-hint
              {:disabled? false :mach-services []}
              "SecKeychainSearchCreateFromAttributes: parameters passed are not valid")]
      (is (str/includes? hint "jail.mach_services.keychain"))
      (is (str/includes? hint "com.apple.SecurityServer"))))
  (testing "silent when the jail is off or the keychain services are already granted"
    (is (nil? (pj/keychain-denial-hint {:disabled? true}
                                       "SecKeychainSearchCreateFromAttributes: nope")))
    (is (nil? (pj/keychain-denial-hint {:disabled? false
                                        :mach-services ["com.apple.SecurityServer"]}
                                       "SecKeychainSearchCreateFromAttributes: nope"))))
  (testing "unrelated output is never annotated"
    (is (false? (pj/keychain-denial? "hello world")))
    (is (nil? (pj/keychain-denial-hint {:disabled? false :mach-services []} "hello world")))))
