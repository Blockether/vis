(ns com.blockether.vis.internal.process-jail-test
  "The OS process jail: the SBPL compiler is asserted as pure data, and — on a
   macOS host that can actually enforce Seatbelt — a real wrapped `bash` proves
   containment end to end (workspace RW allowed; outside-read, write-outside,
   deny-write carve-outs, and network all denied). The policy is a PER-SESSION
   VALUE compiled at the `libvisjail` spawn boundary, never process-global."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer
             [deftest is testing thrown? thrown-with-msg?]]
            [com.blockether.vis.internal.config :as config]
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
    (let [dir
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
    (let [dir
          (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "vis-jail-fs-" (System/nanoTime)))
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
    (let [p (pj/macos-profile
              {:rw [] :net-enabled? false :loopback-port 5273 :inbound-ports [5273 6000]})]
      (is (= 2 (count (re-seq #"network-inbound" p)))
          "duplicate loopback/inbound port collapses to one rule")
      (is (str/includes? p "(allow network-inbound (local ip \"*:6000\"))"))))
  (testing "compile-policy sanitizes to distinct legal integers, dropping junk/out-of-range"
    (let [resolved (pj/compile-policy {:roots-fn (constantly [])
                                       :inbound-ports [5273 "4200" 0 70000 "nope" 5273 nil]})]
      (is (= [5273 4200] (:inbound-ports resolved))))))

(deftest compile-policy-resolves-live-roots
  (testing "session roots-fn + tmp become the RW set, allow-read → :ro"
    (let [dir
          (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "vis-jail-cp-" (System/nanoTime)))
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
    (let [dir
          (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "vis-jail-rw-" (System/nanoTime)))
            (.mkdirs))

          canon
          (.getCanonicalPath dir)

          resolved
          (pj/compile-policy {:roots-fn (constantly []) :allow-read-write [(.getPath dir)]})]

      (try (is (contains? (set (:rw resolved)) canon))
           (is (contains? (set (:ro resolved)) canon))
           (finally (io/delete-file dir true))))))

(defn- run-process
  [argv dir policy]
  (let [^Process process
        (pj/spawn! argv dir policy {:merge-stderr? true})

        out
        (future (slurp (.getInputStream process)))]

    {:exit (.waitFor process) :out @out :pid (.pid process)}))

(defn- sandbox-applicable?
  []
  (and (pj/supported?) (not= "1" (System/getenv "VIS_SEATBELT_ACTIVE"))))

(deftest native-spawn-is-off-by-default
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "visjail-direct-" (System/nanoTime)))
              (.mkdirs))]
    (try (is (= {:exit 0 :out "direct"}
                (select-keys (run-process ["/bin/sh" "-c" "printf direct"] dir nil) [:exit :out])))
         (finally (io/delete-file dir true)))))

(deftest real-native-containment
  (when (sandbox-applicable?)
    (let [root
          (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "visjail-real-" (System/nanoTime)))
            (.mkdirs))

          protected
          (doto (io/file root "protected") (.mkdirs))

          secret
          (io/file protected "secret.txt")

          outside
          (io/file (System/getProperty "user.home") (str ".visjail-denied-" (System/nanoTime)))

          policy
          {:roots-fn (constantly [(.getPath root)])
           :net-enabled? false
           :deny-write [(.getPath protected)]
           :deny-read [(.getPath secret)]}]

      (try (spit secret "secret")
           (let [inside
                 (run-process ["/bin/sh" "-c"
                               (str "printf ok > "
                                    (.getPath (io/file root "ok.txt"))
                                    "; printf escaped > "
                                    (.getPath outside)
                                    " 2>/dev/null || true")]
                              root
                              policy)

                 denied-read
                 (run-process ["/bin/sh" "-c" (str "cat " (.getPath secret))] root policy)]

             (is (zero? (:exit inside)))
             (is (= "ok" (slurp (io/file root "ok.txt"))))
             (is (not (.exists outside)))
             (is (not (zero? (:exit denied-read)))))
           (finally (when (.exists outside) (io/delete-file outside true))
                    (doseq [file (reverse (file-seq root))]
                      (io/delete-file file true)))))))
(deftest proxy-env-vars
  (testing "a confined child is marked even when it has no proxy endpoint"
    (let [expected (if (pj/supported?) {"VIS_SEATBELT_ACTIVE" "1"} {})]
      (is (= expected (pj/proxy-env {})))
      (is (= expected (pj/proxy-env {:net-enabled? true})))))
  (testing ":proxy-port sets both-case proxy vars, and NO CA vars without a :ca-file"
    (let [e
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
    (let [e
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
    (let [ca
          "/tmp/vis-ca.pem"

          e
          (pj/proxy-env {:proxy-port 4321 :ca-file ca})]

      (doseq [v ["CURL_CA_BUNDLE" "SSL_CERT_FILE" "REQUESTS_CA_BUNDLE" "NODE_EXTRA_CA_CERTS"
                 "GIT_SSL_CAINFO" "PIP_CERT" "AWS_CA_BUNDLE" "CARGO_HTTP_CAINFO" "DENO_CERT"]]
        (is (= ca (get e v)) (str v " must point at the CA PEM"))))))

(deftest repl-jail-contract
  (testing "language policy preserves the wall and adds toolchain access"
    (let [base
          {:roots-fn (constantly ["/tmp"])
           :net-enabled? false
           :repl-proxy-port 1000
           :repl-ca-file "/repl-ca.pem"
           :allow-write ["/w"]
           :allow-read ["/r"]}

          policy
          (pj/repl-policy base 54321)]

      (is (false? (:net-enabled? policy)))
      (is (= 1000 (:proxy-port policy)))
      (is (= 54321 (:loopback-port policy)))
      (is (some #{"~/.vis/logs"} (:allow-write policy)))
      (is (some #{"/w"} (:allow-write policy)))))
  (testing "unknown and disposed sessions fail before native spawn"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"session jail is not registered"
                          (pj/session-process-spawn! "no-such-session" ["/bin/true"] "/tmp")))
    (is (thrown? clojure.lang.ExceptionInfo (pj/session-process-spawn! nil ["/bin/true"] "/tmp"))))
  (testing "one contract resolves policy, environment and process lifecycle"
    (pj/register-session-jail! "t-sid"
                               (constantly {:roots-fn (constantly ["/tmp"])
                                            :net-enabled? false
                                            :repl-proxy-port nil
                                            :env-values {"VIS_TEST_VALUE" "managed"}}))
    (try (let [^Process process (pj/session-process-spawn! "t-sid" ["/bin/sh" "-c"
                                                                    "printf %s \"$VIS_TEST_VALUE\""]
                                                           "/tmp" {:merge-stderr? true})]
           (is (= 0 (.waitFor process)))
           (is (= "managed" (slurp (.getInputStream process))))
           (is (pos? (.pid process))))
         (finally (pj/unregister-session-jail! "t-sid")))))
(deftest env-scrub-allowlist
  (testing
    "a confined child inherits ONLY the non-secret allowlist plus the RESOLVED
            `environment:` declarations; every operator secret is dropped and the
            proxy/CA additions are present"
    (let [policy
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
    (let [ambient
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
    (let [env (pj/jailed-child-env {:roots-fn (constantly [])
                                    :net-enabled? false
                                    :inherit-host-env? true
                                    :env-values {"LD_PRELOAD" "/tmp/x.so" "PERL5OPT" "-Mevil"}})]
      (is (empty? (filter #'pj/pre-exec-hijack? (keys env)))))))

(deftest metadata-scoped-to-roots
  (testing
    "file-read-metadata is scoped: no global grant; ancestors are literals,
            granted roots are subpaths, and $HOME is NOT recursively exposed"
    (let [p (pj/macos-profile (pj/compile-policy {:roots-fn (fn []
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
    (let [dir
          (doto (io/file (System/getProperty "java.io.tmpdir")
                         (str "vis-jail-anc-" (System/nanoTime)))
            (.mkdirs))

          real
          (.getCanonicalPath dir)

          p
          (pj/macos-profile (pj/compile-policy {:roots-fn (fn []
                                                            [(.getPath dir)])
                                                :net-enabled? false}))

          ancestors
          (loop [f
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
  (let [root
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

    (try (testing "argv shape: contains only flags and ends with the command separator"
           (is (= "--die-with-parent" (first off)))
           (is (= "--" (last off))))
         (testing "session root is bind-mounted read-write"
           (is (some #(= % ["--bind-try" rp]) pairs)))
         (testing "system toolchain roots are read-only bind-mounted (else nothing launches)"
           (is (some #(= % ["--ro-bind-try" "/usr"]) pairs)))
         (testing "deny-write is re-bound read-only AFTER the rw bind (deny wins)"
           (is (some #(= % ["--ro-bind-try" pp]) pairs))
           (let [ai
                 (.indexOf ^java.util.List off rp)

                 di
                 (.lastIndexOf ^java.util.List off pp)]

             (is (and (pos? ai) (pos? di) (< ai di)))))
         (testing "deny-read is masked with an empty tmpfs" (is (some #(= % ["--tmpfs" pp]) pairs)))
         (testing "net OFF gets the --unshare-net kernel wall (safe)"
           (is (some #{"--unshare-net"} off)))
         (testing "filtered egress delegates the private network namespace to libvisjail"
           (let [filtered (pj/linux-bwrap-args (assoc base
                                                 :net-enabled? true
                                                 :proxy-port 51000))]
             (is (nil? (some #{"--unshare-net"} filtered)))))
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

(deftest enabled-policy-fails-closed-without-libvisjail
  (with-redefs [pj/supported?
                (constantly false)

                pj/unenforceable-reason
                (constantly "runtime missing")]

    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"runtime missing"
          (pj/spawn! ["/bin/true"] "/tmp" {:roots-fn (constantly ["/tmp"]) :net-enabled? false})))))
(deftest wsl-detection
  ;; WSL2 runs a real kernel with namespaces, so it is treated as ordinary Linux.
  ;; WSL1 has no real namespaces and must be reported unenforceable, never silently
  ;; passed through. The kernel osrelease is the discriminator.
  (testing "WSL1 kernel (a `Microsoft` build with NO `WSL2` marker) is detected"
    (with-redefs [pj/linux-osrelease (constantly "4.4.0-19041-Microsoft")]
      (is (true? (#'pj/wsl1?)))))
  (testing "WSL2 real-kernel build is NOT flagged WSL1"
    (with-redefs [pj/linux-osrelease (constantly "5.15.153.1-microsoft-standard-WSL2")]
      (is (false? (#'pj/wsl1?)))))
  (testing "native Linux is NOT flagged WSL1"
    (with-redefs [pj/linux-osrelease (constantly "6.8.0-52-generic")]
      (is (false? (#'pj/wsl1?))))))

(deftest macos-profile-mach-services
  (testing "no grant => no mach-lookup rule at all"
    (is (not (str/includes? (pj/macos-profile {:rw [] :net-enabled? false}) "mach-lookup"))))
  (testing "granted services become one global-name each inside a single allow"
    (let [p (pj/macos-profile {:rw []
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
    (let [hint (pj/keychain-denial-hint
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

;; ONE call's own `env`: the delta a SPAWNING verb carries as an ARGUMENT, over
;; the project environment. Ambient scope is what this contract refuses — the
;; record of the call is what says which variables the child ran with.
(deftest one-calls-own-env-delta
  (testing "literals become strings, null unsets, and a source map resolves through `environment:`"
    (is (= {"NODE_ENV" "test" "PORT" "8080" "DEBUG" "true" "GONE" nil}
           (pj/call-env-values {"NODE_ENV" "test" "PORT" 8080 "DEBUG" true "GONE" nil})))
    (is (= {} (pj/call-env-values nil)))
    (is (= {"TOKEN" "from-the-host"}
           (binding [config/*extension-getenv* (constantly "from-the-host")]
             (pj/call-env-values {"TOKEN" {"env" "SOME_HOST_NAME"}}))))
    ;; Issue #156: `environment:`'s own `{literal: …}` spelling resolves here too.
    (is (= {"VIS_MANAGED" "true"} (pj/call-env-values {"VIS_MANAGED" {"literal" "true"}}))))
  (testing "every refusal NAMES the key — a per-call delta is the author's own line of code"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"env DYLD_INSERT_LIBRARIES"
                          (pj/call-env-values {"DYLD_INSERT_LIBRARIES" "/tmp/x.dylib"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"env BASH_ENV"
                          (pj/call-env-values {"BASH_ENV" "/tmp/rc"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not an environment variable name"
                          (pj/call-env-values {"not a name" "x"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"must name its source"
                          (pj/call-env-values {"TOKEN" {"vault" "prod"}})))
    (is
      (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"env SSH_PASSWORD: command source must be a non-empty argv list of non-blank strings, not a shell string"
        (pj/call-env-values {"SSH_PASSWORD" {"command"
                                             "security find-generic-password -w -s server"}})))
    ;; A standing `environment:` declaration that resolves to nothing is simply
    ;; unset; ONE call that asked for that variable is an error instead.
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"resolved to no value"
                          (binding [config/*extension-getenv* (constantly nil)]
                            (pj/call-env-values {"TOKEN" {"env" "NOTHING_EXPORTS_THIS"}}))))
    (is (thrown? clojure.lang.ExceptionInfo (pj/call-env-values {"A" ["not" "a" "value"]})))
    (is (thrown? clojure.lang.ExceptionInfo (pj/call-env-values "NODE_ENV=test"))))
  (testing "the delta MERGES over the project environment and records what it unset"
    (let [policy (pj/with-call-env {:disabled? true
                                    :env-values {"KEEP" "1" "OVER" "project" "DROP" "2"}}
                                   (pj/call-env-values {"OVER" "call" "NEW" "3" "DROP" nil}))]
      (is (= {"KEEP" "1" "OVER" "call" "NEW" "3"} (:env-values policy)))
      (is (= #{"DROP"} (:env-removals policy)))
      (is (= {"KEEP" "1" "OVER" "call" "NEW" "3"} (pj/child-env-additions policy)))
      ;; No policy at all is still a spawn, and it still gets its own variables.
      (is (= {"NEW" "3"} (:env-values (pj/with-call-env nil {"NEW" "3"}))))
      (is (= {:disabled? true} (pj/with-call-env {:disabled? true} {})))))
  (testing "a confined child's environment is BUILT, so an unset name is never in it"
    (when-let [full (pj/jailed-child-env {:roots-fn (constantly [])
                                          :net-enabled? false
                                          :env-values {"KEEP" "1"}
                                          :env-removals #{"DROP" "PATH"}})]
      (is (= "1" (get full "KEEP")))
      (is (nil? (get full "DROP")))
      ;; PATH is on the inherit allowlist: the removal outranks it.
      (is (nil? (get full "PATH")))))
  (testing "a fingerprint carries the SHAPE of a delta and never a value"
    (let [fp (pj/env-fingerprint {"TOKEN" "s3cret-value" "GONE" nil})]
      (is (= #{"TOKEN" "GONE"} (set (keys fp))))
      (is (not (str/includes? (pr-str fp) "s3cret-value")))
      (is (= "unset" (get fp "GONE")))
      (is (= fp (pj/env-fingerprint {"TOKEN" "s3cret-value" "GONE" nil})))
      (is (not= fp (pj/env-fingerprint {"TOKEN" "another-value" "GONE" nil})))))
  ;; Regression, issue #repl-consistency: only the Clojure pack compared a REUSED
  ;; REPL's env, so `repl_start` meant "reuse or refuse" in one language and
  ;; "silently kill and respawn" in the others. The refusal is minted HERE now,
  ;; so every pack answers the same words.
  (testing "one refusal, shared: a live REPL's env is compared by NAME, never by value"
    (let [running
          (pj/env-fingerprint {"TZ" "UTC" "TOKEN" "s3cret-value"})

          same
          (pj/env-fingerprint {"TZ" "UTC" "TOKEN" "s3cret-value"})

          other
          (pj/env-fingerprint {"TZ" "UTC" "TOKEN" "other-value"})

          refusal
          (pj/env-mismatch-refusal "pyrepl:~/proj" running other)]

      (is (empty? (pj/env-difference running same)))
      (is (nil? (pj/env-mismatch-refusal "pyrepl:~/proj" running same)))
      (is (= ["TOKEN"] (:differing refusal)))
      (is (str/includes? (:message refusal) "pyrepl:~/proj"))
      (is (str/includes? (:message refusal) "repl_stop"))
      ;; names and digests only — no value reaches the message
      (is (not (str/includes? (:message refusal) "s3cret-value")))
      (is (not (str/includes? (:message refusal) "other-value")))
      ;; an ADDED or DROPPED name differs too
      (is (= ["EXTRA"] (pj/env-difference running (assoc running "EXTRA" "d")))))))
