(ns com.blockether.vis.internal.sandbox.jail-test
  "The OS process jail as Vis owns it: a session's configuration, live roots and
   proxy endpoint become ONE platform-neutral policy VALUE plus the complete
   child environment, and — on a host that can enforce — a real wrapped `bash`
   proves containment end to end. HOW that value is enforced is the runtime's
   (`com.blockether/vis-python-runtime`); nothing here spells an enforcement
   dialect, and a scan keeps it that way."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it throws-with-msg? throws?]]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.sandbox.jail :as pj]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]))

(defdescribe automatic-java-process-boundary
             (it "automatic java process boundary"
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
                       (expect (some #{java-home} (:read-only sent)))
                       (expect (not (some #{java-home} (:read-write sent))))
                       (expect (= [denied] (:deny-read sent)))
                       (expect (= [java-home] (:deny-write sent))))))))

(defdescribe
  runtime-policy-value
  (it "live roots + read-write grants are read-write, read-only stays read-only"
      (let [p (pj/runtime-policy {:roots-fn (constantly ["/ws" "/ws2"])
                                  :allow-read-write ["/cache"]
                                  :allow-read ["/ro"]
                                  :deny-write ["/ws/protected"]
                                  :deny-read ["/ws/secret"]
                                  :deny-exec ["/usr/bin/curl"]})]
        (expect (= ["/ws" "/ws2" "/cache"] (:read-write p)))
        (expect (= ["/ro"] (:read-only p)))
        (expect (= ["/ws/protected"] (:deny-write p)))
        (expect (= ["/ws/secret"] (:deny-read p)))
        (expect (= ["/usr/bin/curl"] (:deny-exec p)))
        (expect (false? (:keychain? p)))))
  (it "deny rules travel beside the paths the snapshot expanded (#263)"
      (let [p (pj/runtime-policy {:deny-read ["/ws/a/.env"]
                                  :deny-read-rules ["/ws/**/.env"]
                                  :deny-write ["/ws/vendor"]
                                  :deny-write-rules ["/ws/vendor"]})]
        (expect (= ["/ws/a/.env" "/ws/**/.env"] (:deny-read p)))
        (expect (= ["/ws/vendor"] (:deny-write p)))))
  (it "a failing roots-fn grants nothing rather than everything"
      (expect (= [] (:read-write (pj/runtime-policy {:roots-fn #(throw (ex-info "boom" {}))})))))
  (it "egress: the session proxy when one is up, else open or off"
      (expect (= {:proxy 4321}
                 (:network (pj/runtime-policy {:proxy-port 4321 :net-enabled? true}))))
      (expect (= :open (:network (pj/runtime-policy {:net-enabled? true}))))
      (expect (= :off (:network (pj/runtime-policy {:net-enabled? false})))))
  (it "inbound is the managed listener port plus the configured ports, sanitized"
      (expect (= [54321 5273 4200]
                 (:inbound (pj/runtime-policy {:loopback-port 54321
                                               :inbound-ports [5273 "4200" 5273 nil "junk" 0
                                                               70000]}))))
      (expect (= [] (:inbound (pj/runtime-policy {})))))
  (it "the keychain grant is a boolean"
      (expect (true? (:keychain? (pj/runtime-policy {:keychain? true})))))
  (it "a Python worker keeps session policy and adds only its bootstrap doors"
      (let [policy (pj/python-worker-policy {:roots-fn (constantly ["/ws"])
                                             :net-enabled? true
                                             :proxy-port 4321
                                             :worker-proxy-port 4322
                                             :proxy-token "sess"
                                             :keychain? true}
                                            "/run"
                                            "/run/control.sock"
                                            ["/java" "/classpath"])]
        (expect (= ["/run"] (:allow-read-write policy)))
        (expect (= ["/java" "/classpath"] (:allow-read policy)))
        (expect (= ["/run/control.sock"] (:unix-connect policy)))
        (expect (= {"all_proxy" "http://sess:vis@127.0.0.1:4322"
                    "ALL_PROXY" "http://sess:vis@127.0.0.1:4322"}
                   (select-keys (pj/proxy-env policy) ["all_proxy" "ALL_PROXY"])))
        (expect (= {:proxy 4322} (:network (pj/runtime-policy policy))))
        (expect (= "sess" (:proxy-token policy)))
        (expect (true? (:keychain? policy))))))

(def ^:private enforcement-tokens
  "Words that only an enforcement dialect uses. Vis states policy; the runtime
   compiles it — a hit here means enforcement text leaked back into this repo."
  ["Seatbelt" "SBPL" "sandbox-exec" "(deny default)" "bwrap" "bubblewrap" "--unshare" "mach-lookup"
   "VIS_SEATBELT_ACTIVE"])

(defdescribe vis-states-policy-and-never-compiles-enforcement
             (it "vis states policy and never compiles enforcement"
                 (doseq [file
                         (->> (file-seq (io/file "src"))
                              (filter #(str/ends-with? (.getName ^java.io.File %) ".clj")))

                         :let [text
                               (slurp file)]
                         token
                         enforcement-tokens]

                   (expect (not (str/includes? text token))
                           (str (.getPath ^java.io.File file) " mentions " token)))))

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

(defdescribe native-spawn-is-off-by-default
             (it "native spawn is off by default"
                 (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                                          (str "visjail-direct-" (System/nanoTime)))
                             (.mkdirs))]
                   (try (expect (= {:exit 0 :out "direct"}
                                   (select-keys
                                     (run-process ["/bin/sh" "-c" "printf direct"] dir nil)
                                     [:exit :out])))
                        (finally (io/delete-file dir true))))))

(defdescribe
  real-native-containment
  (it
    "real native containment"
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

               (expect (zero? (:exit inside)))
               (expect (= "ok" (slurp (io/file root "ok.txt"))))
               (expect (not (.exists outside)))
               (expect (not (zero? (:exit denied-read)))))
             (finally (when (.exists outside) (io/delete-file outside true))
                      (doseq [file (reverse (file-seq root))]
                        (io/delete-file file true))))))))

(defdescribe proxy-env-vars
             (it "no proxy endpoint, no additions — the confinement marker is the runtime's"
                 (expect (= {} (pj/proxy-env {})))
                 (expect (= {} (pj/proxy-env {:net-enabled? true}))))
             (it ":proxy-port sets both-case proxy vars, and NO CA vars without a :ca-file"
                 (let [e
                       (pj/proxy-env {:proxy-port 4321})

                       url
                       "http://127.0.0.1:4321"

                       socks
                       "socks5h://127.0.0.1:4321"]

                   ;; http(s) keep the HTTP proxy (MITM verb/path); all_proxy = the SOCKS lane
                   ;; for non-HTTP schemes (ssh/git+ssh/db/raw TCP) on the same loopback port.
                   (doseq [k ["http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY"]]
                     (expect (= url (get e k)) k))
                   (doseq [k ["all_proxy" "ALL_PROXY"]]
                     (expect (= socks (get e k)) k))
                   ;; Without this, Node's built-in fetch (undici) ignores the proxy variables.
                   (expect (= "1" (get e "NODE_USE_ENV_PROXY")))
                   (expect (not (contains? e "CURL_CA_BUNDLE")))
                   (expect (not (contains? e "SSL_CERT_FILE")))))
             (it ":proxy-token rides the proxy URL userinfo as user, with a filler password"
                 (let [e
                       (pj/proxy-env {:proxy-port 4321 :proxy-token "tok-123"})

                       url
                       "http://tok-123:vis@127.0.0.1:4321"

                       socks
                       "socks5h://tok-123:vis@127.0.0.1:4321"]

                   (doseq [k ["http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY"]]
                     (expect (= url (get e k)) k))
                   (doseq [k ["all_proxy" "ALL_PROXY"]]
                     (expect (= socks (get e k)) k))))
             (it "with a :ca-file EVERY common CA-trust var points at the ephemeral CA PEM"
                 ;; The MITM tier mints per-host leaves off an ephemeral CA; each runtime reads a
                 ;; different trust var, so the full set (sandbox-runtime's nine) must be covered
                 ;; or that runtime silently fails the handshake instead of trusting the proxy.
                 (let [ca
                       "/tmp/vis-ca.pem"

                       e
                       (pj/proxy-env {:proxy-port 4321 :ca-file ca})]

                   (doseq [v ["CURL_CA_BUNDLE" "SSL_CERT_FILE" "REQUESTS_CA_BUNDLE"
                              "NODE_EXTRA_CA_CERTS" "GIT_SSL_CAINFO" "PIP_CERT" "AWS_CA_BUNDLE"
                              "CARGO_HTTP_CAINFO" "DENO_CERT"]]
                     (expect (= ca (get e v)) (str v " must point at the CA PEM"))))))

(defdescribe
  env-scrub-allowlist
  (it
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

      (expect (map? env))
      (expect (contains? env "PATH"))
      (expect (contains? env "HOME"))
      (expect (= "from-dotenv" (get env "MY_DECLARED_TOKEN"))
              "a declared variable reaches the confined child with its resolved value")
      (expect (empty? (filter env (remove #{"MY_DECLARED_TOKEN"} secretish)))
              "no UNDECLARED API key / token / secret / password var may reach a jailed child")))
  (it "an unconfined child gets the same declarations as plain additions"
      (expect (= {"MY_DECLARED_TOKEN" "from-dotenv"}
                 (pj/child-env-additions {:disabled? true
                                          :env-values {"MY_DECLARED_TOKEN" "from-dotenv"
                                                       "BLANK_NAME" nil}}))))
  (it "nil when the policy is not enforcing (disabled / nil) — caller inherits"
      (expect (nil? (pj/jailed-child-env nil)))
      (expect (nil? (pj/jailed-child-env {:disabled? true
                                          :roots-fn (fn []
                                                      ["/x"])})))))

(defdescribe
  jail-environment-inherit-mode
  (it
    "`jail.environment: inherit` (`:inherit-host-env?`) keeps the operator's
            ambient environment in a confined child, while the default keeps only the
            allowlist — everything else about the jail is unchanged"
    (let [ambient
          (into {} (System/getenv))

          ;; A real ambient name the default mode must drop: not on the passthrough
          ;; allowlist, not a pre-exec hijack name, not dropped in every mode.
          outsider
          (first (remove #(or (#'pj/env-passthrough? %)
                              (#'pj/pre-exec-hijack? %)
                              (contains? @#'pj/ambient-env-drops %))
                   (keys ambient)))

          policy
          {:roots-fn (constantly [])
           :net-enabled? false
           :env-values {"MY_DECLARED_TOKEN" "from-dotenv"}}

          declared-env
          (pj/jailed-child-env policy)

          inherited-env
          (pj/jailed-child-env (assoc policy :inherit-host-env? true))]

      (expect (some? outsider) "the host must export something outside the allowlist to test with")
      (expect (not (contains? declared-env outsider))
              "the DEFAULT mode drops every ambient variable that is not a non-secret basic")
      (expect (= (get ambient outsider) (get inherited-env outsider))
              "`inherit` hands the confined child the ambient value verbatim")
      (expect (= "from-dotenv" (get inherited-env "MY_DECLARED_TOKEN"))
              "the project's own environment still applies on top under `inherit`")))
  (it "a pre-exec hijack name is refused under `inherit` too — that scrub is the jail itself"
      (let [env (pj/jailed-child-env {:roots-fn (constantly [])
                                      :net-enabled? false
                                      :inherit-host-env? true
                                      :env-values {"LD_PRELOAD" "/tmp/x.so" "PERL5OPT" "-Mevil"}})]
        (expect (empty? (filter #'pj/pre-exec-hijack? (keys env)))))))

(defdescribe
  ambient-node-options-stays-out-of-children
  ;; Regression for a user report: the gateway inherited NODE_OPTIONS from the terminal
  ;; that started it. The value named a `--require` preload in a temp folder, and the
  ;; terminal tool deleted that file. Every Node command in the sandbox then failed.
  (it "drops the ambient NODE_OPTIONS from unconfined and `inherit` children"
      (let [drop-ambient
            @#'pj/ambient-env

            host
            {"PATH" "/usr/bin" "NODE_OPTIONS" "--require=/gone/preload.cjs"}]

        (with-redefs [pj/ambient-env (fn ([] (drop-ambient host)) ([env] (drop-ambient env)))]
          (let [unconfined (pj/process-environment {:disabled? true})
                inherited (pj/jailed-child-env {:roots-fn (constantly [])
                                                :net-enabled? false
                                                :inherit-host-env? true})]

            (expect (= "/usr/bin" (get unconfined "PATH")))
            (expect (not (contains? unconfined "NODE_OPTIONS")))
            (expect (= "/usr/bin" (get inherited "PATH")))
            (expect (not (contains? inherited "NODE_OPTIONS")))))))
  (it "keeps the NODE_OPTIONS that the project declares"
      (let [env (pj/process-environment {:disabled? true
                                         :env-values {"NODE_OPTIONS" "--max-old-space-size=2048"}})]
        (expect (= "--max-old-space-size=2048" (get env "NODE_OPTIONS"))))))

(defdescribe
  keychain-denial-hint-explains-a-denied-lookup
  (it "a Security-framework failure under a live jail names the config key"
      (let [hint (pj/keychain-denial-hint
                   {:disabled? false :keychain? false}
                   "SecKeychainSearchCreateFromAttributes: parameters passed are not valid")]
        (expect (str/includes? hint "jail.keychain: true"))))
  (it "silent when the jail is off or the keychain is already granted"
      (expect (nil? (pj/keychain-denial-hint {:disabled? true}
                                             "SecKeychainSearchCreateFromAttributes: nope")))
      (expect (nil? (pj/keychain-denial-hint {:disabled? false :keychain? true}
                                             "SecKeychainSearchCreateFromAttributes: nope"))))
  (it "unrelated output is never annotated"
      (expect (false? (pj/keychain-denial? "hello world")))
      (expect (nil? (pj/keychain-denial-hint {:disabled? false :keychain? false} "hello world")))))

;; ONE call's own `env`: the delta a SPAWNING verb carries as an ARGUMENT, over
;; the project environment. Ambient scope is what this contract refuses — the
;; record of the call is what says which variables the child ran with.
(defdescribe
  one-calls-own-env-delta
  (it "literals become strings, null unsets, and a source map resolves through `environment:`"
      (expect (= {"NODE_ENV" "test" "PORT" "8080" "DEBUG" "true" "GONE" nil}
                 (pj/call-env-values {"NODE_ENV" "test" "PORT" 8080 "DEBUG" true "GONE" nil})))
      (expect (= {} (pj/call-env-values nil)))
      (expect (= {"TOKEN" "from-the-host"}
                 (binding [config/*extension-getenv* (constantly "from-the-host")]
                   (pj/call-env-values {"TOKEN" {"env" "SOME_HOST_NAME"}}))))
      ;; Issue #156: `environment:`'s own `{literal: …}` spelling resolves here too.
      (expect (= {"VIS_MANAGED" "true"} (pj/call-env-values {"VIS_MANAGED" {"literal" "true"}}))))
  (it
    "every refusal NAMES the key — a per-call delta is the author's own line of code"
    (expect (throws-with-msg? clojure.lang.ExceptionInfo
                              #"env DYLD_INSERT_LIBRARIES"
                              #(pj/call-env-values {"DYLD_INSERT_LIBRARIES" "/tmp/x.dylib"})))
    (expect (throws-with-msg? clojure.lang.ExceptionInfo
                              #"env BASH_ENV"
                              #(pj/call-env-values {"BASH_ENV" "/tmp/rc"})))
    (expect (throws-with-msg? clojure.lang.ExceptionInfo
                              #"not an environment variable name"
                              #(pj/call-env-values {"not a name" "x"})))
    (expect (throws-with-msg? clojure.lang.ExceptionInfo
                              #"must name its source"
                              #(pj/call-env-values {"TOKEN" {"vault" "prod"}})))
    (expect
      (throws-with-msg?
        clojure.lang.ExceptionInfo
        #"env SSH_PASSWORD: command source must be a non-empty argv list of non-blank strings, not a shell string"
        #(pj/call-env-values {"SSH_PASSWORD" {"command"
                                              "security find-generic-password -w -s server"}})))
    ;; A standing `environment:` declaration that resolves to nothing is simply
    ;; unset; ONE call that asked for that variable is an error instead.
    (expect (throws-with-msg? clojure.lang.ExceptionInfo
                              #"resolved to no value"
                              #(binding [config/*extension-getenv* (constantly nil)]
                                 (pj/call-env-values {"TOKEN" {"env" "NOTHING_EXPORTS_THIS"}}))))
    (expect (throws? clojure.lang.ExceptionInfo #(pj/call-env-values {"A" ["not" "a" "value"]})))
    (expect (throws? clojure.lang.ExceptionInfo #(pj/call-env-values "NODE_ENV=test"))))
  (it "the delta MERGES over the project environment and records what it unset"
      (let [policy (pj/with-call-env {:disabled? true
                                      :env-values {"KEEP" "1" "OVER" "project" "DROP" "2"}}
                                     (pj/call-env-values {"OVER" "call" "NEW" "3" "DROP" nil}))]
        (expect (= {"KEEP" "1" "OVER" "call" "NEW" "3"} (:env-values policy)))
        (expect (= #{"DROP"} (:env-removals policy)))
        (expect (= {"KEEP" "1" "OVER" "call" "NEW" "3"} (pj/child-env-additions policy)))
        ;; No policy at all is still a spawn, and it still gets its own variables.
        (expect (= {"NEW" "3"} (:env-values (pj/with-call-env nil {"NEW" "3"}))))
        (expect (= {:disabled? true} (pj/with-call-env {:disabled? true} {})))))
  (it "a confined child's environment is BUILT, so an unset name is never in it"
      (when-let [full (pj/jailed-child-env {:roots-fn (constantly [])
                                            :net-enabled? false
                                            :env-values {"KEEP" "1"}
                                            :env-removals #{"DROP" "PATH"}})]
        (expect (= "1" (get full "KEEP")))
        (expect (nil? (get full "DROP")))
        ;; PATH is on the inherit allowlist: the removal outranks it.
        (expect (nil? (get full "PATH"))))))

(defdescribe configured-deny-rules-refuse-host-tools
             (it "configured deny rules refuse host tools"
                 ;; #263: the host file tools run in THIS process, which no sandbox confines, so
                 ;; they ask the policy directly for the rules the children get from the kernel.
                 (let [env {:security-policy {:process-jail {:deny-read-rules ["/ws/.env"
                                                                               "/ws/**/.env"]
                                                             :deny-write-rules ["/ws/vendor"]}}}]
                   ;; a read rule closes the file it names and every file a glob matches
                   (expect (str/includes? (pj/deny-refusal env "file-read" "/ws/.env")
                                          "jail.filesystem.deny_read"))
                   (expect (some? (pj/deny-refusal env "file-read" "/ws/service/.env")))
                   ;; Reading is how a rewrite starts: a file this session may not read is not
                   ;; one it may patch either.
                   (expect (some? (pj/deny-refusal env "file-write" "/ws/.env")))
                   ;; the rest of the root stays open, and a near-miss name is not a match
                   (expect (nil? (pj/deny-refusal env "file-read" "/ws/src/core.clj")))
                   (expect (nil? (pj/deny-refusal env "file-read" "/ws/.envrc")))
                   ;; a write rule names a directory and closes its subtree for writes only
                   (expect (str/includes? (pj/deny-refusal env "file-write" "/ws/vendor/lib/a.js")
                                          "jail.filesystem.deny_write"))
                   (expect (nil? (pj/deny-refusal env "file-read" "/ws/vendor/lib/a.js")))
                   ;; no policy in the environment, no refusal
                   (expect (nil? (pj/deny-refusal {} "file-read" "/ws/.env"))))))

(defdescribe
  deny-rules-survive-a-disabled-jail
  (it "deny rules survive a disabled jail"
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

        (try (expect (false? (:jail-enabled snapshot)))
             (expect (true? (:disabled? (:process-jail snapshot))))
             (expect (str/includes? (pj/deny-refusal env "file-read" (str root "/.env"))
                                    "jail.filesystem.deny_read"))
             (expect (some? (pj/deny-refusal env "file-read" (str root "/service/.env"))))
             (expect (some? (pj/deny-refusal env "file-write" (str root "/vendor/lib.js"))))
             (expect (nil? (pj/deny-refusal env "file-read" (str root "/README.md"))))
             (finally (io/delete-file (io/file root) true))))))

(defdescribe
  configured-deny-read-reaches-every-child
  (it "configured deny read reaches every child"
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

        (try
          (.mkdirs (io/file root "service"))
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

            ;; every managed child is handed the expanded paths and the rules
            (with-redefs [runtime/jailed?
                          (constantly false)

                          runtime/spawn-process!
                          (fn [_ options]
                            options)]

              (doseq [child [policy (pj/python-worker-policy policy "/run" "/run/control.sock" [])]]
                (let [sent (:deny-read (:policy
                                         (pj/spawn! ["/bin/true"] nil child {:environment {}})))]
                  (expect (every? (set sent) denied))
                  ;; #263: the rule travels as written too. Expanding it names only the
                  ;; files that existed when the snapshot was built.
                  (expect (some #(str/includes? % "**/.env") sent)))))
            ;; and a host that can enforce stops the child itself
            (when (sandbox-applicable?)
              (let [read-file
                    (fn [^java.io.File file]
                      (run-process ["/bin/sh" "-c" (str "cat " (.getPath file))]
                                   (io/file root)
                                   policy))

                    late
                    (io/file root "late" ".env")]

                (expect (not (zero? (:exit (read-file secret)))))
                (expect (not (zero? (:exit (read-file nested)))))
                (expect (= {:exit 0 :out "ok"} (select-keys (read-file readable) [:exit :out])))
                ;; #263: this one appears AFTER the policy was built, so only the rule
                ;; itself can cover it — reading it here is the leak the issue reported.
                ;; Only a pattern-enforcing host answers for it; elsewhere the child keeps
                ;; exactly the paths the snapshot expanded, and Vis' own tools hold the rule.
                (when (pattern-enforcing-host?)
                  (.mkdirs (io/file root "late"))
                  (spit late "TOKEN=secret")
                  (let [late-read (read-file late)]
                    (expect (not (zero? (:exit late-read))))
                    (expect (not (str/includes? (:out late-read) "TOKEN=secret"))))))))
          (finally (doseq [file (reverse (file-seq (io/file root)))]
                     (io/delete-file file true)))))))
