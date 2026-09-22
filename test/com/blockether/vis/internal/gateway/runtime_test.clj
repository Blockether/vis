(ns com.blockether.vis.internal.gateway.runtime-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.runtime :as protocol]))

(defn- client-var [sym] (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(deftest handshake-wire-roundtrip-test
  (testing "canonical string-keyed handshakes parse into engine keys"
    (is (= {:protocol 3 :min-client 2 :min-gateway 1 :version "1.2.3" :build "abc123def456"}
           (contract/wire->handshake {"protocol" 3
                                      "min_client" "2"
                                      "min_gateway" 1.0
                                      "version" "1.2.3"
                                      "build" "abc123def456"}))))
  (testing "an old peer with no handshake is rejected"
    (is (= {:protocol nil :min-client nil :min-gateway nil :version nil :build nil}
           (contract/wire->handshake {})))
    (let [verdict (protocol/client-verdict "vis-test" nil)]
      (is (= "unknown" (:reason verdict)))
      (is (= "gateway" (:upgrade verdict)))
      (is (false? (:is-compatible verdict))))))

(deftest compatibility-verdict-test
  ;; The three numbers move together here: a release that only ever speaks its
  ;; own contract refuses the half that is behind, in whichever direction it is
  ;; behind, instead of serving a shape neither side maintains.
  (testing "this release serves only the protocol it speaks"
    (is (= 13 contract/protocol-version))
    (is (= 13 contract/minimum-client-protocol))
    (is (= 13 contract/minimum-gateway-protocol)))
  (testing "a gateway rejects an explicitly too-old client"
    (let [verdict
          (contract/verdict
            {:gateway-protocol 2 :gateway-min-client 2 :client-protocol 1 :client-min-gateway 1})]
      (is (false? (:is-compatible verdict)))
      (is (= "client-too-old" (:reason verdict)))
      (is (= "client" (:upgrade verdict)))))
  (testing "a client rejects an explicitly too-old gateway"
    (let [verdict
          (contract/verdict
            {:gateway-protocol 1 :gateway-min-client 1 :client-protocol 2 :client-min-gateway 2})]
      (is (false? (:is-compatible verdict)))
      (is (= "gateway-too-old" (:reason verdict)))
      (is (= "gateway" (:upgrade verdict)))))
  (testing "an unversioned client is rejected"
    (let [verdict (protocol/gateway-verdict {:headers {}})]
      (is (false? (:is-compatible verdict)))
      (is (= "unknown" (:reason verdict)))
      (is (= "client" (:upgrade verdict)))))
  (testing "current peers agree"
    (is (= "ok"
           (:reason (contract/verdict {:gateway-protocol 2
                                       :gateway-min-client 2
                                       :client-protocol 2
                                       :client-min-gateway 2}))))))

(deftest runtime-handshake-adds-only-runtime-identity-test
  (let [identity
        (atom nil)

        result
        (with-redefs [protocol/release-version
                      (constantly "1.2.3")

                      protocol/build-id
                      (constantly "abc123def456")

                      contract/handshake
                      (fn [runtime-identity]
                        (reset! identity runtime-identity)
                        :contract-handshake)]

          (protocol/handshake))]

    (is (= :contract-handshake result))
    (is (= {:version "1.2.3" :build "abc123def456"} @identity))))

(deftest client-records-the-nested-health-handshake-test
  (let [handshake-atom
        @(client-var 'gateway-handshake*)

        previous
        @handshake-atom

        ;; A gateway demanding MORE than this build speaks — the point of the
        ;; case, so the floor has to stay ahead of `protocol-version`.
        body
        {"status" "ok"
         "protocol" {"protocol" 14 "min_client" 14 "min_gateway" 2 "version" "14.0.0"}}]

    (try (is (= body ((client-var 'note-handshake!) body)))
         (is (= {:protocol 14 :min-client 14 :min-gateway 2 :version "14.0.0" :build nil}
                @handshake-atom))
         (is (= "client-too-old" (:reason (client/compatibility))))
         (finally (reset! handshake-atom previous)))))

(deftest release-version-is-loaded-once-test
  ;; JVM SDK profiling found repeated classpath probes for immutable build metadata.
  (let [expected
        (protocol/release-version)

        resource
        io/resource

        lookups
        (atom 0)]

    (with-redefs [io/resource (fn [& args]
                                (swap! lookups inc)
                                (apply resource args))]
      (dotimes [_ 10]
        (is (= expected (protocol/release-version)))))
    (is (zero? @lookups) "version reads after the first one must not probe the classpath")))

;; A dev build has no release to be ordered by, so its commit is what says whether a
;; daemon is running the code in front of you.
(deftest build-identity-supersedes-an-unorderable-version-test
  (testing "a version order decides first, in both directions"
    (is (true? (protocol/superseded? {:our-version "0.1.40" :their-version "0.1.39"})))
    (is (false? (protocol/superseded? {:our-version "0.1.39"
                                       :their-version "0.1.40"
                                       :our-build "aaa111aaa111"
                                       :their-build "bbb222bbb222"}))
        "a newer daemon is never pulled back to this build's commit"))
  (testing "where the versions carry no order, the commit does"
    (is (true? (protocol/superseded? {:our-version "dev"
                                      :their-version "dev"
                                      :our-build "aaa111aaa111"
                                      :their-build "bbb222bbb222"})))
    (is (false? (protocol/superseded? {:our-version "dev"
                                       :their-version "dev"
                                       :our-build "aaa111aaa111"
                                       :their-build "aaa111aaa111"})))
    (is (true? (protocol/superseded? {:our-version "0.1.40"
                                      :their-version "0.1.40"
                                      :our-build "aaa111aaa111"
                                      :their-build "bbb222bbb222"}))
        "one VIS_VERSION built twice is still two builds"))
  (testing "a build nobody could name is no evidence at all"
    (is (false? (protocol/superseded?
                  {:our-version "dev" :their-version "dev" :our-build "aaa111aaa111"})))
    (is (false? (protocol/superseded?
                  {:our-version "dev" :their-version "dev" :their-build "bbb222bbb222"})))
    (is (false? (protocol/superseded? {:our-version "dev" :their-version "dev"})))))

(deftest build-id-is-one-value-per-process-test
  (testing "a source run identifies itself by HEAD, with no git process to pay for"
    (let [id (protocol/build-id)]
      (is (string? id))
      (is (re-matches #"[0-9a-f]{40}(-dirty)?" (protocol/release-sha)))
      (is (= id (#'protocol/short-commit (protocol/release-sha))))
      ;; The COMMIT is the whole identity: no working-tree marker, so the id never
      ;; depends on which classpath this run holds or on a walk over the checkout.
      (is (re-matches #"[0-9a-f]{12}(-dirty)?" id))
      (is (= (protocol/release-sha)
             (#'protocol/head-sha (#'protocol/git-dir (#'protocol/checkout-root))))
          "a dev run says exactly what HEAD says, edited worktree or not")
      (is (identical? id (protocol/build-id))
          "computed once: a daemon must advertise the code it LOADED, not today's disk")))
  (testing "the handshake carries it, so the probe every attach already pays for answers it"
    (is (= (protocol/build-id) (:build (protocol/handshake)))))
  (testing "a native image and a source checkout name one commit the same way"
    (let [short-commit #'protocol/short-commit]
      (is (= "bcc0c8208350" (short-commit "bcc0c8208350bd0e9e6c1a5a6f4d3c2b1a098765")))
      (is (= "bcc0c8208350" (short-commit "bcc0c8208350")))
      (is (= "bcc0c8208350-dirty" (short-commit "bcc0c8208350bd0e9e6c1a5a6f4d3c2b1a098765-dirty")))
      (is (nil? (short-commit "unknown"))
          "a build that could not read its own commit has no identity to compare"))))

(defn- release-skew-copy
  "The human copy for two halves that speak the SAME protocol and differ only in
   release - the pair the verdict calls compatible."
  [gateway-version client-version]
  (protocol/explain (contract/verdict {:gateway-protocol contract/protocol-version
                                       :gateway-min-client contract/minimum-client-protocol
                                       :gateway-version gateway-version
                                       :client-protocol contract/protocol-version
                                       :client-min-gateway contract/minimum-gateway-protocol
                                       :client-version client-version
                                       :client-name "vis-tui"})))

;; Regression: a client and a gateway on different releases of one protocol were told
;; "Versions match", which is how somebody running the older half learned nothing at
;; all about the newer Vis already serving them.
(deftest compatible-release-skew-copy-test
  (testing "a newer gateway is announced to the client that is behind"
    (let [{:keys [title summary remedy]} (release-skew-copy "0.2.22" "0.2.21")]
      (is (= "A newer Vis is available" title))
      (is (str/includes? summary "0.2.22"))
      (is (str/includes? summary "0.2.21"))
      (is (str/includes? (str/join " " remedy) "vis-agent update"))))
  (testing "a newer client is told which half is behind instead"
    (let [{:keys [title remedy]} (release-skew-copy "0.2.21" "0.2.22")]
      (is (= "The gateway runs an older Vis" title))
      (is (str/includes? (str/join " " remedy) "vis-agent gateway stop"))))
  (testing "one release on both halves still matches"
    (is (= "Versions match" (:title (release-skew-copy "0.2.22" "0.2.22"))))
    (is (= "Versions match" (:title (release-skew-copy "dev" "0.2.22"))))))
