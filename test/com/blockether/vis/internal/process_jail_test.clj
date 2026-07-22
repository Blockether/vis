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

(deftest real-containment
  ;; The threat model is a secret OUTSIDE every allowed root (e.g. ~/.ssh). Temp
  ;; dirs are INTENTIONALLY writable roots (mirrors the Python sandbox), so the
  ;; secret + escape target live under $HOME, which the jail never allows.
  (when (pj/supported?)
    (let
      [home
       (System/getProperty "user.home")

       ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-ws-" (System/nanoTime)))
         (.mkdirs))

       protected
       (doto (io/file ws "protected") (.mkdirs))

       secret
       (io/file home (str ".vis-jail-secret-" (System/nanoTime) ".txt"))

       escape
       (io/file home (str ".vis-jail-escape-" (System/nanoTime) ".txt"))

       wsc
       (.getCanonicalPath ws)

       protc
       (.getCanonicalPath protected)

       policy
       {:roots-fn (constantly [(.getPath ws)])
        :net-enabled? false
        :deny-write [(.getPath protected)]}]

      (spit (io/file ws "inside.txt") "workspace-ok")
      (spit secret "TOP-SECRET")
      (try (testing "reads + writes inside the workspace succeed"
             (let
               [r (run-jailed
                    (pj/wrap-argv
                      ["bash" "--noprofile" "--norc" "-lc"
                       (str "cat " wsc "/inside.txt" " && echo x > " wsc "/w.txt && echo WROTE")]
                      policy))]
               (is (zero? (:exit r)))
               (is (str/includes? (:out r) "workspace-ok"))
               (is (str/includes? (:out r) "WROTE"))))
           (testing "a deny-write subtree INSIDE the workspace is protected (carve-out wins)"
             (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                        (str "echo nope > " protc "/blocked.txt 2>&1")]
                                       policy))
             (is (not (.exists (io/file protected "blocked.txt")))))
           (testing "reading a secret OUTSIDE every root ($HOME) is denied"
             (let
               [r (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                             (str "cat " (.getCanonicalPath secret) " 2>&1")]
                                            policy))]
               (is (not (str/includes? (:out r) "TOP-SECRET")))))
           (testing "writing outside every root ($HOME) is denied"
             (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                        (str "echo escaped > " (.getCanonicalPath escape) " 2>&1")]
                                       policy))
             (is (not (.exists escape))))
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
                    (io/delete-file protected true)
                    (io/delete-file ws true)
                    (io/delete-file secret true)
                    (io/delete-file escape true))))))

(deftest proxy-env-vars
  (testing "no :proxy-port ⇒ no env additions (jail leaves egress untouched)"
    (is (= {} (pj/proxy-env {})))
    (is (= {} (pj/proxy-env {:net-enabled? true}))))
  (testing ":proxy-port sets both-case proxy vars, and NO CA vars without a :ca-file"
    (let
      [e
       (pj/proxy-env {:proxy-port 4321})

       url
       "http://127.0.0.1:4321"]

      (doseq [k ["http_proxy" "https_proxy" "all_proxy" "HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY"]]
        (is (= url (get e k)) k))
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