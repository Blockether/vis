(ns com.blockether.vis.internal.gateway.protocol-test
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.protocol :as protocol]))

(defn- client-var [sym] (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(deftest handshake-wire-roundtrip-test
  (testing "canonical string-keyed handshakes parse into engine keys"
    (is (= {:protocol 3 :min-client 2 :min-gateway 1 :version "1.2.3" :build "abc123def456"}
           (protocol/wire->handshake {"protocol" 3
                                      "min_client" "2"
                                      "min_gateway" 1.0
                                      "version" "1.2.3"
                                      "build" "abc123def456"}))))
  (testing "an old peer with no handshake is rejected"
    (is (= {:protocol nil :min-client nil :min-gateway nil :version nil :build nil}
           (protocol/wire->handshake {})))
    (let [verdict (protocol/client-verdict "vis-test" nil)]
      (is (= "unknown" (:reason verdict)))
      (is (= "gateway" (:upgrade verdict)))
      (is (false? (:is-compatible verdict))))))

(deftest compatibility-verdict-test
  (testing "this release serves only the current wire protocol"
    (is (= 2 protocol/protocol-version))
    (is (= 2 protocol/min-client-protocol))
    (is (= 2 protocol/min-gateway-protocol)))
  (testing "a gateway rejects an explicitly too-old client"
    (let [verdict
          (protocol/verdict
            {:gateway-protocol 2 :gateway-min-client 2 :client-protocol 1 :client-min-gateway 1})]
      (is (false? (:is-compatible verdict)))
      (is (= "client-too-old" (:reason verdict)))
      (is (= "client" (:upgrade verdict)))))
  (testing "a client rejects an explicitly too-old gateway"
    (let [verdict
          (protocol/verdict
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
           (:reason (protocol/verdict {:gateway-protocol 2
                                       :gateway-min-client 2
                                       :client-protocol 2
                                       :client-min-gateway 2}))))))

(deftest client-records-the-nested-health-handshake-test
  (let [handshake-atom
        @(client-var 'gateway-handshake*)

        previous
        @handshake-atom

        body
        {"status" "ok" "protocol" {"protocol" 3 "min_client" 3 "min_gateway" 2 "version" "3.0.0"}}]

    (try (is (= body ((client-var 'note-handshake!) body)))
         (is (= {:protocol 3 :min-client 3 :min-gateway 2 :version "3.0.0" :build nil}
                @handshake-atom))
         (is (= "client-too-old" (:reason (client/compatibility))))
         (finally (reset! handshake-atom previous)))))

;; The order that decides whether a running daemon is stale after `vis-agent update`.
(deftest release-version-ordering-test
  (testing "a release is only ever picked up forward"
    (is (true? (protocol/newer-release? "0.1.40" "0.1.39")))
    (is (false? (protocol/newer-release? "0.1.39" "0.1.40")))
    (is (false? (protocol/newer-release? "0.1.40" "0.1.40"))))
  (testing "segments compare as numbers, not as text"
    (is (true? (protocol/newer-release? "0.1.10" "0.1.9")))
    (is (true? (protocol/newer-release? "0.2.0" "0.1.99"))))
  (testing "a shorter version is zero-padded, never ranked by its length"
    (is (true? (protocol/newer-release? "0.2" "0.1.9")))
    (is (false? (protocol/newer-release? "0.1" "0.1.0")))
    (is (false? (protocol/newer-release? "0.1.0" "0.1"))))
  (testing "a prerelease ranks with the release it precedes"
    (is (true? (protocol/newer-release? "0.1.40-rc1" "0.1.39")))
    (is (false? (protocol/newer-release? "0.1.40" "0.1.40-rc1"))))
  (testing "a build with no ordered version is neither newer nor older"
    (is (false? (protocol/newer-release? "dev" "0.1.39")))
    (is (false? (protocol/newer-release? "0.1.40" "dev")))
    (is (false? (protocol/newer-release? "0.1.40" nil)))
    (is (false? (protocol/newer-release? nil nil)))))

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
      ;; The COMMIT is the whole identity: no working-tree marker, so the id never
      ;; depends on which classpath this run holds or on a walk over the checkout.
      (is (re-matches #"[0-9a-f]{12}(-dirty)?" id))
      (is (= id (#'protocol/checkout-build-id (#'protocol/checkout-root)))
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
