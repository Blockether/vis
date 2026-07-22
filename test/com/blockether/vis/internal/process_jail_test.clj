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
  "False when this test JVM already runs inside Seatbelt, which rejects nested
   sandbox_apply with EPERM even though sandbox-exec is installed."
  []
  (zero? (:exit (run-jailed (pj/wrap-argv ["/usr/bin/true"]
                                          {:roots-fn (constantly ["/tmp"]) :net-enabled? false})))))

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
       "http://127.0.0.1:4321"]

      (doseq [k ["http_proxy" "https_proxy" "all_proxy" "HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY"]]
        (is (= url (get e k)) k))
      (when (pj/supported?) (is (= "1" (get e "VIS_SEATBELT_ACTIVE"))))
      (is (not (contains? e "CURL_CA_BUNDLE")))
      (is (not (contains? e "SSL_CERT_FILE")))))
  (testing ":proxy-token rides the proxy URL userinfo (session attribution)"
    (let
      [e
       (pj/proxy-env {:proxy-port 4321 :proxy-token "tok-123"})

       url
       "http://tok-123@127.0.0.1:4321"]

      (doseq [k ["http_proxy" "https_proxy" "all_proxy" "HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY"]]
        (is (= url (get e k)) k))))
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
      (is (some #{"~/.m2"} (:allow-write rp)))
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
           (if (pj/supported?)
             (do (is (= "sandbox-exec" (first argv)))
                 (is (not (re-find #"\(allow network\*\)" (nth argv 2))))
                 (is (re-find #"network-bind \(local ip\)" (nth argv 2)))
                 (is (str/includes? (nth argv 2) "network-inbound (local ip \"*:54321\")"))
                 (is (re-find #"localhost:999" (nth argv 2))))
             (is (= ["clojure" "-M:x"] argv))))
         (finally (pj/unregister-session-jail! "t-sid")))))