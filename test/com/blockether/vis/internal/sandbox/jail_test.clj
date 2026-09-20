(ns com.blockether.vis.internal.sandbox.jail-test
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
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.sandbox.jail :as pj]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]))

(deftest automatic-java-process-boundary
  (let [snapshot
        (security-policy/snapshot {"jail" {"enabled" true}})

        java-home
        (.getCanonicalPath (io/file (System/getProperty "java.home")))

        denied
        (str java-home "/private")

        base
        (assoc (:process-jail snapshot)
          :deny-read [denied]
          :deny-write [java-home]
          :net-enabled? false)]

    (with-redefs [runtime/jailed?
                  (constantly false)

                  runtime/spawn-process!
                  (fn [_ options]
                    options)]

      (let [sent (:policy (pj/spawn! ["/bin/true"] nil base {:environment {}}))]
        (is (some #{java-home} (:read-only sent)))
        (is (not (some #{java-home} (:read-write sent))))
        (is (= [denied] (:deny-read sent)))
        (is (= [java-home] (:deny-write sent)))))))

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
  (testing "deny rules travel beside the paths the snapshot expanded (#263)"
    (let [p (pj/runtime-policy {:deny-read ["/ws/a/.env"]
                                :deny-read-rules ["/ws/**/.env"]
                                :deny-write ["/ws/vendor"]
                                :deny-write-rules ["/ws/vendor"]})]
      (is (= ["/ws/a/.env" "/ws/**/.env"] (:deny-read p)))
      (is (= ["/ws/vendor"] (:deny-write p)))))
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
    (is (true? (:keychain? (pj/runtime-policy {:keychain? true})))))
  (testing "a Python worker keeps session policy and adds only its bootstrap doors"
    (let [policy (pj/python-worker-policy {:roots-fn (constantly ["/ws"])
                                           :net-enabled? true
                                           :proxy-port 4321
                                           :worker-proxy-port 4322
                                           :proxy-token "sess"
                                           :keychain? true}
                                          "/run"
                                          "/run/control.sock"
                                          ["/java" "/classpath"])]
      (is (= ["/run"] (:allow-read-write policy)))
      (is (= ["/java" "/classpath"] (:allow-read policy)))
      (is (= ["/run/control.sock"] (:unix-connect policy)))
      (is (= {"all_proxy" "http://sess@127.0.0.1:4322" "ALL_PROXY" "http://sess@127.0.0.1:4322"}
             (select-keys (pj/proxy-env policy) ["all_proxy" "ALL_PROXY"])))
      (is (= {:proxy 4322} (:network (pj/runtime-policy policy))))
      (is (= "sess" (:proxy-token policy)))
      (is (true? (:keychain? policy))))))

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

(defn- pattern-enforcing-host?
  "True where the OS takes a deny PATTERN as written, so a file created after the
   snapshot is denied as soon as it exists. Seatbelt compiles the pattern into the
   profile; bubblewrap binds mount points and has nothing to bind for a glob — the
   platform split `jail.md` documents under \"Deny specific files\"."
  []
  (str/starts-with? (System/getProperty "os.name") "Mac"))

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
      (is (nil? (get full "PATH"))))))

(deftest configured-deny-rules-refuse-host-tools
  ;; #263: the host file tools run in THIS process, which no sandbox confines, so
  ;; they ask the policy directly for the rules the children get from the kernel.
  (let [env {:security-policy {:process-jail {:deny-read-rules ["/ws/.env" "/ws/**/.env"]
                                              :deny-write-rules ["/ws/vendor"]}}}]
    (testing "a read rule closes the file it names and every file a glob matches"
      (is (str/includes? (pj/deny-refusal env "file-read" "/ws/.env") "jail.filesystem.deny_read"))
      (is (some? (pj/deny-refusal env "file-read" "/ws/service/.env")))
      ;; Reading is how a rewrite starts: a file this session may not read is not
      ;; one it may patch either.
      (is (some? (pj/deny-refusal env "file-write" "/ws/.env"))))
    (testing "the rest of the root stays open, and a near-miss name is not a match"
      (is (nil? (pj/deny-refusal env "file-read" "/ws/src/core.clj")))
      (is (nil? (pj/deny-refusal env "file-read" "/ws/.envrc"))))
    (testing "a write rule names a directory and closes its subtree for writes only"
      (is (str/includes? (pj/deny-refusal env "file-write" "/ws/vendor/lib/a.js")
                         "jail.filesystem.deny_write"))
      (is (nil? (pj/deny-refusal env "file-read" "/ws/vendor/lib/a.js"))))
    (testing "no policy in the environment, no refusal"
      (is (nil? (pj/deny-refusal {} "file-read" "/ws/.env"))))))

(deftest deny-rules-survive-a-disabled-jail
  ;; `jail.enabled` is the OS toggle; the #263 deny rules are configuration of their
  ;; own. A disabled jail keeps them — the host file tools still refuse — instead of
  ;; making the whole vis.yml invalid.
  (let [root
        (.getCanonicalPath (doto (io/file (System/getProperty "java.io.tmpdir")
                                          (str "visdeny-off-" (System/nanoTime)))
                             (.mkdirs)))

        snapshot
        (security-policy/snapshot {"workspace" {"filesystem" [{"id" "project" "path" root}]}
                                   "jail" {"enabled" false
                                           "filesystem" {"deny_read" [".env" "**/.env"]
                                                         "deny_write" ["vendor"]}}}
                                  {:base-dir root})

        env
        {:security-policy snapshot}]

    (try (is (false? (:jail-enabled snapshot)))
         (is (true? (:disabled? (:process-jail snapshot))))
         (is (str/includes? (pj/deny-refusal env "file-read" (str root "/.env"))
                            "jail.filesystem.deny_read"))
         (is (some? (pj/deny-refusal env "file-read" (str root "/service/.env"))))
         (is (some? (pj/deny-refusal env "file-write" (str root "/vendor/lib.js"))))
         (is (nil? (pj/deny-refusal env "file-read" (str root "/README.md"))))
         (finally (io/delete-file (io/file root) true)))))

(deftest configured-deny-read-reaches-every-child
  ;; #263: `cat` is not the only reader. The rule has to reach the shell child,
  ;; the language REPL and the Python worker, or it protects only the built-in
  ;; tools while `open(".env")` still succeeds.
  (let [root
        (.getCanonicalPath (doto (io/file (System/getProperty "java.io.tmpdir")
                                          (str "visdeny-" (System/nanoTime)))
                             (.mkdirs)))

        secret
        (io/file root ".env")

        nested
        (io/file root "service" ".env")

        readable
        (io/file root "README.md")]

    (try (.mkdirs (io/file root "service"))
         (spit secret "TOKEN=secret")
         (spit nested "TOKEN=secret")
         (spit readable "ok")
         ;; The snapshot expands the glob, so it is built AFTER the files exist.
         (let [policy
               (assoc (:process-jail (security-policy/snapshot
                                       {"workspace" {"filesystem" [{"id" "project" "path" root}]}
                                        "jail" {"enabled" true
                                                "filesystem" {"allow" ["project"]
                                                              "deny_read" [".env" "**/.env"]}}}
                                       {:base-dir root}))
                 :roots-fn (constantly [root])
                 :net-enabled? false)

               denied
               [(.getPath secret) (.getPath nested)]]

           (testing "every managed child is handed the expanded paths and the rules"
             (with-redefs [runtime/jailed?
                           (constantly false)

                           runtime/spawn-process!
                           (fn [_ options]
                             options)]

               (doseq [child [policy
                              (pj/python-worker-policy policy "/run" "/run/control.sock" [])]]
                 (let [sent (:deny-read (:policy
                                          (pj/spawn! ["/bin/true"] nil child {:environment {}})))]
                   (is (every? (set sent) denied))
                   ;; #263: the rule travels as written too. Expanding it names only the
                   ;; files that existed when the snapshot was built.
                   (is (some #(str/includes? % "**/.env") sent))))))
           (testing "and a host that can enforce stops the child itself"
             (when (sandbox-applicable?)
               (let [read-file
                     (fn [^java.io.File file]
                       (run-process ["/bin/sh" "-c" (str "cat " (.getPath file))]
                                    (io/file root)
                                    policy))

                     late
                     (io/file root "late" ".env")]

                 (is (not (zero? (:exit (read-file secret)))))
                 (is (not (zero? (:exit (read-file nested)))))
                 (is (= {:exit 0 :out "ok"} (select-keys (read-file readable) [:exit :out])))
                 ;; #263: this one appears AFTER the policy was built, so only the rule
                 ;; itself can cover it — reading it here is the leak the issue reported.
                 ;; Only a pattern-enforcing host answers for it; elsewhere the child keeps
                 ;; exactly the paths the snapshot expanded, and Vis' own tools hold the rule.
                 (when (pattern-enforcing-host?)
                   (.mkdirs (io/file root "late"))
                   (spit late "TOKEN=secret")
                   (let [late-read (read-file late)]
                     (is (not (zero? (:exit late-read))))
                     (is (not (str/includes? (:out late-read) "TOKEN=secret")))))))))
         (finally (doseq [file (reverse (file-seq (io/file root)))]
                    (io/delete-file file true))))))
