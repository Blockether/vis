(ns com.blockether.vis.internal.process-jail-test
  "The OS process jail: the SBPL compiler is asserted as pure data, and — on a
   macOS host that can actually enforce Seatbelt — a real wrapped `bash` proves
   containment end to end (workspace RW allowed; outside-read, write-outside,
   deny-write carve-outs, and network all denied). The policy is a PER-SESSION
   VALUE threaded into `wrap-argv`, never a process-global singleton."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

(defn- sandbox-applicable?
  "True only when this JVM may apply a fresh Seatbelt profile.

   Managed test JVMs already inherit Seatbelt (`VIS_SEATBELT_ACTIVE=1`), and a
   nested profile cannot strengthen or replace that kernel policy. macOS CI and
   an ordinary host JVM run the real enforcement branch; inherited runs exercise
   the pure compiler/launch contract without pretending nested enforcement ran."
  []
  (and (pj/supported?) (not= "1" (System/getenv "VIS_SEATBELT_ACTIVE"))))

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
    (try (let
           [{:keys [argv env]}
            (pj/session-process-launch "t-sid" ["clojure" "-M:x"] {:loopback-port 54321})]
           (is (= "http://127.0.0.1:999" (get env "HTTPS_PROXY")))
           (is (= "/tmp/repl-ca.pem" (get env "SSL_CERT_FILE")))
           (is (re-find #"-Dhttps\.proxyPort=999" (get env "JAVA_TOOL_OPTIONS")))
           (is (re-find #"-Djava\.net\.preferIPv4Stack=true" (get env "JAVA_TOOL_OPTIONS")))
           (is (re-find #"-Djavax\.net\.ssl\.trustStore=/tmp/repl-ca\.p12"
                        (get env "JAVA_TOOL_OPTIONS")))
           (if (sandbox-applicable?)
             (do (is (= "sandbox-exec" (first argv)))
                 (is (not (re-find #"\(allow network\*\)" (nth argv 2))))
                 (is (re-find #"network-bind \(local ip\)" (nth argv 2)))
                 (is (str/includes? (nth argv 2) "network-inbound (local ip \"*:54321\")"))
                 (is (re-find #"localhost:999" (nth argv 2))))
             (is (= ["clojure" "-M:x"] argv))))
         (finally (pj/unregister-session-jail! "t-sid")))))

(deftest env-scrub-allowlist
  (testing
    "a confined child inherits ONLY the allowlist + jail.env opt-ins; every
            operator secret is dropped, proxy/CA additions are present"
    (let
      [policy
       {:roots-fn (fn []
                    [(System/getProperty "java.io.tmpdir")])
        :net-enabled? false
        :env-passthrough ["MY_OPT_IN"]}

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
      (is (empty? (filter env secretish))
          "no API key / token / secret / password var may reach a jailed child")))
  (testing "nil when the policy is not enforcing (disabled / nil) — caller inherits"
    (is (nil? (pj/jailed-child-env nil)))
    (is (nil? (pj/jailed-child-env {:disabled? true
                                    :roots-fn (fn []
                                                ["/x"])})))))

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

(defn- linux? [] (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "linux"))

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

     prox
     (pj/linux-bwrap-args (assoc base
                            :net-enabled? true
                            :proxy-port 51000))

     open
     (pj/linux-bwrap-args (assoc base :net-enabled? true))

     pairs
     (partition 2 1 off)]

    (try (testing "argv shape: starts with bwrap, ends with the -- separator"
           (is (= "bwrap" (first off)))
           (is (= "--" (last off))))
         (testing "session root is bind-mounted read-write"
           (is (some #(= % ["--bind-try" rp]) pairs)))
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
         (testing "net OFF and proxy-restricted both get the --unshare-net kernel wall (safe)"
           (is (some #{"--unshare-net"} off))
           (is (some #{"--unshare-net"} prox)))
         (testing "an explicitly-open network shares the host namespace (no --unshare-net)"
           (is (nil? (some #{"--unshare-net"} open))))
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
        "unsupported host must give a reason, so `sandbox: true` is not a silent no-op")))

(deftest linux-e2e-runner-contract
  (when (= "1" (System/getenv "VIS_REQUIRE_LINUX_SANDBOX_E2E"))
    (is (and (linux?) (pj/supported?))
        "required Linux E2E runner must be Linux with bubblewrap installed")))

(deftest linux-real-containment
  ;; Real bubblewrap enforcement — runs ONLY on a Linux host with bwrap (i.e. the
  ;; ubuntu CI job). Proves a wrapped bash reads its workspace but CANNOT read a
  ;; secret outside the bound roots (which simply does not exist inside the jail).
  (when (and (linux?) (pj/supported?))
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
       {:roots-fn (constantly [wsc]) :net-enabled? false}

       argv
       (pj/wrap-argv ["bash" "-lc" (str "cat " wsc "/ok.txt; echo ---; cat " sc " 2>&1 || true")]
                     policy)]

      (try (is (= "bwrap" (first argv)) "linux jail must bwrap-wrap the child")
           (let [{:keys [out]} (run-jailed argv)]
             (is (str/includes? out "WORKSPACE-OK")
                 "workspace file must be readable inside the jail")
             (is (not (str/includes? out "TOP-SECRET-DATA"))
                 "a secret outside the bound roots must be absent inside the jail"))
           (finally (io/delete-file (io/file ws "ok.txt") true)
                    (io/delete-file ws true)
                    (io/delete-file secret true))))))