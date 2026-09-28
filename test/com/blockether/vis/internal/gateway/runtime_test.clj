(ns com.blockether.vis.internal.gateway.runtime-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.runtime :as protocol]))

(defn- client-var [sym] (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(defdescribe
  handshake-wire-roundtrip-test
  (it "canonical string-keyed handshakes parse into engine keys"
      (expect (= {:protocol 3 :min-client 2 :min-gateway 1 :version "1.2.3" :build "abc123def456"}
                 (contract/wire->handshake {"protocol" 3
                                            "min_client" "2"
                                            "min_gateway" 1.0
                                            "version" "1.2.3"
                                            "build" "abc123def456"}))))
  (it "an old peer with no handshake is rejected"
      (expect (= {:protocol nil :min-client nil :min-gateway nil :version nil :build nil}
                 (contract/wire->handshake {})))
      (let [verdict (protocol/client-verdict "vis-test" nil)]
        (expect (= "unknown" (:reason verdict)))
        (expect (= "gateway" (:upgrade verdict)))
        (expect (false? (:is-compatible verdict))))))

(defdescribe
  compatibility-verdict-test
  ;; The three numbers move together here: a release that only ever speaks its
  ;; own contract refuses the half that is behind, in whichever direction it is
  ;; behind, instead of serving a shape neither side maintains.
  (it "this release serves only the protocol it speaks"
      (expect (= 13 contract/protocol-version))
      (expect (= 13 contract/minimum-client-protocol))
      (expect (= 13 contract/minimum-gateway-protocol)))
  (it "a gateway rejects an explicitly too-old client"
      (let [verdict
            (contract/verdict
              {:gateway-protocol 2 :gateway-min-client 2 :client-protocol 1 :client-min-gateway 1})]
        (expect (false? (:is-compatible verdict)))
        (expect (= "client-too-old" (:reason verdict)))
        (expect (= "client" (:upgrade verdict)))))
  (it "a client rejects an explicitly too-old gateway"
      (let [verdict
            (contract/verdict
              {:gateway-protocol 1 :gateway-min-client 1 :client-protocol 2 :client-min-gateway 2})]
        (expect (false? (:is-compatible verdict)))
        (expect (= "gateway-too-old" (:reason verdict)))
        (expect (= "gateway" (:upgrade verdict)))))
  (it "an unversioned client is rejected"
      (let [verdict (protocol/gateway-verdict {:headers {}})]
        (expect (false? (:is-compatible verdict)))
        (expect (= "unknown" (:reason verdict)))
        (expect (= "client" (:upgrade verdict)))))
  (it "current peers agree"
      (expect (= "ok"
                 (:reason (contract/verdict {:gateway-protocol 2
                                             :gateway-min-client 2
                                             :client-protocol 2
                                             :client-min-gateway 2}))))))

(defdescribe runtime-handshake-adds-only-runtime-identity-test
             (it "runtime handshake adds only runtime identity"
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

                   (expect (= :contract-handshake result))
                   (expect (= {:version "1.2.3" :build "abc123def456"} @identity)))))

(defdescribe
  client-records-the-nested-health-handshake-test
  (it "client records the nested health handshake"
      (let [handshake-atom
            @(client-var 'gateway-handshake*)

            previous
            @handshake-atom

            ;; A gateway demanding MORE than this build speaks — the point of the
            ;; case, so the floor has to stay ahead of `protocol-version`.
            body
            {"status" "ok"
             "protocol" {"protocol" 14 "min_client" 14 "min_gateway" 2 "version" "14.0.0"}}]

        (try (expect (= body ((client-var 'note-handshake!) body)))
             (expect (= {:protocol 14 :min-client 14 :min-gateway 2 :version "14.0.0" :build nil}
                        @handshake-atom))
             (expect (= "client-too-old" (:reason (client/compatibility))))
             (finally (reset! handshake-atom previous))))))

(defdescribe release-version-is-loaded-once-test
             (it "release version is loaded once"
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
                       (expect (= expected (protocol/release-version)))))
                   (expect (zero? @lookups)
                           "version reads after the first one must not probe the classpath"))))

;; A dev build has no release to be ordered by, so its commit is what says whether a
;; daemon is running the code in front of you.
(defdescribe
  build-identity-supersedes-an-unorderable-version-test
  (it "a version order decides first, in both directions"
      (expect (true? (protocol/superseded? {:our-version "0.1.40" :their-version "0.1.39"})))
      (expect (false? (protocol/superseded? {:our-version "0.1.39"
                                             :their-version "0.1.40"
                                             :our-build "aaa111aaa111"
                                             :their-build "bbb222bbb222"}))
              "a newer daemon is never pulled back to this build's commit"))
  (it "where the versions carry no order, the commit does"
      (expect (true? (protocol/superseded? {:our-version "dev"
                                            :their-version "dev"
                                            :our-build "aaa111aaa111"
                                            :their-build "bbb222bbb222"})))
      (expect (false? (protocol/superseded? {:our-version "dev"
                                             :their-version "dev"
                                             :our-build "aaa111aaa111"
                                             :their-build "aaa111aaa111"})))
      (expect (true? (protocol/superseded? {:our-version "0.1.40"
                                            :their-version "0.1.40"
                                            :our-build "aaa111aaa111"
                                            :their-build "bbb222bbb222"}))
              "one VIS_VERSION built twice is still two builds"))
  (it "a build nobody could name is no evidence at all"
      (expect (false? (protocol/superseded?
                        {:our-version "dev" :their-version "dev" :our-build "aaa111aaa111"})))
      (expect (false? (protocol/superseded?
                        {:our-version "dev" :their-version "dev" :their-build "bbb222bbb222"})))
      (expect (false? (protocol/superseded? {:our-version "dev" :their-version "dev"})))))

(defdescribe
  build-id-is-one-value-per-process-test
  (it "a source run identifies itself by HEAD, with no git process to pay for"
      (let [id (protocol/build-id)]
        (expect (string? id))
        (expect (re-matches #"[0-9a-f]{40}(-dirty)?" (protocol/release-sha)))
        (expect (= id (#'protocol/short-commit (protocol/release-sha))))
        ;; The COMMIT is the whole identity: no working-tree marker, so the id never
        ;; depends on which classpath this run holds or on a walk over the checkout.
        (expect (re-matches #"[0-9a-f]{12}(-dirty)?" id))
        (expect (= (protocol/release-sha)
                   (#'protocol/head-sha (#'protocol/git-dir (#'protocol/checkout-root))))
                "a dev run says exactly what HEAD says, edited worktree or not")
        (expect (identical? id (protocol/build-id))
                "computed once: a daemon must advertise the code it LOADED, not today's disk")))
  (it "the handshake carries it, so the probe every attach already pays for answers it"
      (expect (= (protocol/build-id) (:build (protocol/handshake)))))
  (it "a native image and a source checkout name one commit the same way"
      (let [short-commit #'protocol/short-commit]
        (expect (= "bcc0c8208350" (short-commit "bcc0c8208350bd0e9e6c1a5a6f4d3c2b1a098765")))
        (expect (= "bcc0c8208350" (short-commit "bcc0c8208350")))
        (expect (= "bcc0c8208350-dirty"
                   (short-commit "bcc0c8208350bd0e9e6c1a5a6f4d3c2b1a098765-dirty")))
        (expect (nil? (short-commit "unknown"))
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
(defdescribe compatible-release-skew-copy-test
             (it "a newer gateway is announced to the client that is behind"
                 (let [{:keys [title summary remedy]} (release-skew-copy "0.2.22" "0.2.21")]
                   (expect (= "A newer Vis is available" title))
                   (expect (str/includes? summary "0.2.22"))
                   (expect (str/includes? summary "0.2.21"))
                   (expect (str/includes? (str/join " " remedy) "vis-agent update"))))
             (it "a newer client is told which half is behind instead"
                 (let [{:keys [title remedy]} (release-skew-copy "0.2.21" "0.2.22")]
                   (expect (= "The gateway runs an older Vis" title))
                   (expect (str/includes? (str/join " " remedy) "vis-agent gateway stop"))))
             (it "one release on both halves still matches"
                 (expect (= "Versions match" (:title (release-skew-copy "0.2.22" "0.2.22"))))
                 (expect (= "Versions match" (:title (release-skew-copy "dev" "0.2.22"))))))
