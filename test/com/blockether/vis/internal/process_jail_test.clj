(ns com.blockether.vis.internal.process-jail-test
  "The OS process jail as Vis owns it: a session's configuration, live roots and
   proxy endpoint become ONE platform-neutral policy VALUE plus the complete
   child environment, and — on a host that can enforce — a real wrapped `bash`
   proves containment end to end. HOW that value is enforced is the runtime's
   (`com.blockether/vis-python-runtime`); nothing here spells an enforcement
   dialect, and a scan keeps it that way."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer
             [deftest is testing thrown? thrown-with-msg?]]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.process-jail :as pj]))

(deftest runtime-policy-value
  (testing "live roots + read-write grants are read-write, read-only stays read-only"
    (let [p (pj/runtime-policy {:roots-fn (constantly ["/ws" "/ws2"])
                                :allow-read-write ["/cache"]
                                :allow-read ["/ro"]
                                :deny-write ["/ws/protected"]
                                :deny-read ["/ws/secret"]
                                :deny-exec ["/usr/bin/curl"]})]
      (is (= ["/ws" "/ws2" "/cache"] (:read-write p)))
      (is (= ["/ro"] (:read-only p)))
      (is (= ["/ws/protected"] (:deny-write p)))
      (is (= ["/ws/secret"] (:deny-read p)))
      (is (= ["/usr/bin/curl"] (:deny-exec p)))
      (is (false? (:keychain? p)))))
  (testing "a failing roots-fn grants nothing rather than everything"
    (is (= [] (:read-write (pj/runtime-policy {:roots-fn #(throw (ex-info "boom" {}))})))))
  (testing "egress: the session proxy when one is up, else open or off"
    (is (= {:proxy 4321} (:network (pj/runtime-policy {:proxy-port 4321 :net-enabled? true}))))
    (is (= :open (:network (pj/runtime-policy {:net-enabled? true}))))
    (is (= :off (:network (pj/runtime-policy {:net-enabled? false})))))
  (testing "inbound is the managed listener port plus the configured ports, sanitized"
    (is (= [54321 5273 4200]
           (:inbound (pj/runtime-policy {:loopback-port 54321
                                         :inbound-ports [5273 "4200" 5273 nil "junk" 0 70000]}))))
    (is (= [] (:inbound (pj/runtime-policy {})))))
  (testing "the keychain grant is a boolean"
    (is (true? (:keychain? (pj/runtime-policy {:keychain? true}))))))

(def ^:private enforcement-tokens
  "Words that only an enforcement dialect uses. Vis states policy; the runtime
   compiles it — a hit here means enforcement text leaked back into this repo."
  ["Seatbelt" "SBPL" "sandbox-exec" "(deny default)" "bwrap" "bubblewrap" "--unshare" "mach-lookup"
   "VIS_SEATBELT_ACTIVE"])

(deftest vis-states-policy-and-never-compiles-enforcement
  (doseq [file
          (->> (file-seq (io/file "src"))
               (filter #(str/ends-with? (.getName ^java.io.File %) ".clj")))

          :let [text
                (slurp file)]
          token
          enforcement-tokens]

    (is (not (str/includes? text token)) (str (.getPath ^java.io.File file) " mentions " token))))

(defn- run-process
  [argv dir policy]
  (let [^Process process
        (pj/spawn! argv dir policy {:merge-stderr? true})

        out
        (future (slurp (.getInputStream process)))]

    {:exit (.waitFor process) :out @out :pid (.pid process)}))

(defn- sandbox-applicable? [] (and (pj/supported?) (not (runtime/jailed?))))

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
  (testing "no proxy endpoint, no additions — the confinement marker is the runtime's"
    (is (= {} (pj/proxy-env {})))
    (is (= {} (pj/proxy-env {:net-enabled? true}))))
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
           :allow-read-write ["/w"]
           :allow-read ["/r"]}

          policy
          (pj/repl-policy base 54321)]

      (is (false? (:net-enabled? policy)))
      (is (= 1000 (:proxy-port policy)))
      (is (= 54321 (:loopback-port policy)))
      (is (some #{"~/.vis/logs"} (:allow-read-write policy)))
      (is (some #{"/w"} (:allow-read-write policy)))
      (is (some #{"~/.sdkman"} (:allow-read policy)))
      (is (nil? (:inbound-ports policy)) "the shell dev-server ports are not inherited")))
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
          "the project's own environment still applies on top under `inherit`")))
  (testing "a pre-exec hijack name is refused under `inherit` too — that scrub is the jail itself"
    (let [env (pj/jailed-child-env {:roots-fn (constantly [])
                                    :net-enabled? false
                                    :inherit-host-env? true
                                    :env-values {"LD_PRELOAD" "/tmp/x.so" "PERL5OPT" "-Mevil"}})]
      (is (empty? (filter #'pj/pre-exec-hijack? (keys env)))))))

(deftest keychain-denial-hint-explains-a-denied-lookup
  (testing "a Security-framework failure under a live jail names the config key"
    (let [hint (pj/keychain-denial-hint
                 {:disabled? false :keychain? false}
                 "SecKeychainSearchCreateFromAttributes: parameters passed are not valid")]
      (is (str/includes? hint "jail.keychain: true"))))
  (testing "silent when the jail is off or the keychain is already granted"
    (is (nil? (pj/keychain-denial-hint {:disabled? true}
                                       "SecKeychainSearchCreateFromAttributes: nope")))
    (is (nil? (pj/keychain-denial-hint {:disabled? false :keychain? true}
                                       "SecKeychainSearchCreateFromAttributes: nope"))))
  (testing "unrelated output is never annotated"
    (is (false? (pj/keychain-denial? "hello world")))
    (is (nil? (pj/keychain-denial-hint {:disabled? false :keychain? false} "hello world")))))

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
