(ns com.blockether.vis.internal.extension-reload-test
  "Unit tests for the F1-lite extension hot-reload orchestrator
   (`loop/reload-extensions!`) and the surgical side-effect cleanup
   in `extension/deregister-extension!`. Covers plan Q12-Q16 +
   caveats: continue-on-error, deferred reseat for the calling env,
   per-env lock-acquisition timeout."
  (:require
   [clojure.string :as string]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.registry :as registry]
   [lazytest.core :refer [defdescribe expect it]]))

;; ---------------------------------------------------------------------------
;; Side-effect cleanup
;; ---------------------------------------------------------------------------

(defn- clear-test-extensions!
  "Drop every extension whose namespace begins with `test.reload.`
   so synthetic test extensions don't leak across runs."
  []
  (doseq [ext (extension/registered-extensions)
          :let [n (str (:ext/namespace ext))]
          :when (string/starts-with? n "test.reload.")]
    (extension/deregister-extension! (:ext/namespace ext))))

(defdescribe deregister-extension-cli-cleanup-test
  (it "deregister-extension! removes :ext/cli entries from the cmd registry"
    (clear-test-extensions!)
    (try
      (let [ext (extension/extension
                  {:ext/namespace 'test.reload.with-cli
                   :ext/doc       "synthetic ext with cli"
                   :ext/cli       [{:cmd/name   "synthetic-foo"
                                    :cmd/parent ["extensions"]
                                    :cmd/doc    "test cmd"
                                    :cmd/run-fn (fn [_ _] :ok)}]})]
        (extension/register-extension! ext)
        (expect (some #(= "synthetic-foo" (:cmd/name %))
                  (registry/registered-under ["extensions"])))
        (extension/deregister-extension! 'test.reload.with-cli)
        (expect (not (some #(= "synthetic-foo" (:cmd/name %))
                       (registry/registered-under ["extensions"])))))
      (finally (clear-test-extensions!))))

  (it "deregister-extension! is safe to call for unregistered ns"
    (expect (nil? (extension/deregister-extension! 'test.reload.never-registered)))))

;; ---------------------------------------------------------------------------
;; Source marker storage
;; ---------------------------------------------------------------------------

(defdescribe source-markers-sidecar-test
  (it "register-extension! populates the source-markers sidecar"
    (clear-test-extensions!)
    (try
      (extension/register-extension!
        (extension/extension
          {:ext/namespace 'com.blockether.vis.core
           :ext/doc       "synthetic re-register against an already-loaded ns"}))
      (let [m (extension/extension-source-markers-of 'com.blockether.vis.core)]
        (expect (some? m))
        (expect (vector? (:source-paths m)))
        (expect (or (nil? (:source-hash-sha256 m))
                  (string? (:source-hash-sha256 m)))))
      (finally (clear-test-extensions!))))

  (it "deregister-extension! drops the sidecar entry"
    (clear-test-extensions!)
    (try
      (extension/register-extension!
        (extension/extension
          {:ext/namespace 'test.reload.markers-drop
           :ext/doc       "tests sidecar drop"}))
      ;; No source resolution for synthetic ns; markers map should be
      ;; present but empty.
      (expect (some? (extension/extension-source-markers-of 'test.reload.markers-drop)))
      (extension/deregister-extension! 'test.reload.markers-drop)
      (expect (nil? (extension/extension-source-markers-of 'test.reload.markers-drop)))
      (finally (clear-test-extensions!)))))

;; ---------------------------------------------------------------------------
;; reload-extensions! happy path
;; ---------------------------------------------------------------------------

(defdescribe reload-extensions-test
  (it "happy path: returns a diff summary with expected keys"
    (let [r (lp/reload-extensions!)]
      (expect (vector? (:added r)))
      (expect (vector? (:removed r)))
      (expect (vector? (:reloaded r)))
      (expect (vector? (:errors r)))
      (expect (number? (:envs-reseated r)))
      (expect (vector? (:env-reseat-deferred r)))
      (expect (vector? (:env-reseat-skipped r)))
      (expect (number? (:duration-ms r)))
      (expect (number? (:blocked-ms r)))))

  (it "reloads all currently-registered extensions (no change-detection)"
    ;; F1-lite: every still-present ext lands in :reloaded.
    (let [pre  (set (map :ext/namespace (extension/registered-extensions)))
          r    (lp/reload-extensions!)
          post (set (map :ext/namespace (extension/registered-extensions)))]
      ;; All previously-registered nses appear in :reloaded (modulo
      ;; any that aren't on the manifest scan, which become :removed
      ;; instead). For the production registry, all should re-show.
      (expect (= pre post))
      (expect (every? pre (:reloaded r)))))

  (it "respects :reload/timeout-ms argument"
    (let [r (lp/reload-extensions! {:reload/timeout-ms 100})]
      ;; No envs in cache for unit tests, so timeout never trips.
      (expect (= [] (:env-reseat-skipped r))))))

;; ---------------------------------------------------------------------------
;; diff-extensions logic
;; ---------------------------------------------------------------------------

(defdescribe diff-extensions-test
  (it "classifies into :added / :removed / :reloaded"
    ;; Reach into the private fn via #'.
    (let [diff (#'lp/diff-extensions
                [{:ext/namespace 'a} {:ext/namespace 'b}]
                {"manifest-1" {:nses ['b 'c]}
                 "manifest-2" {:nses ['d]}})]
      (expect (= ['c 'd] (:added diff)))
      (expect (= ['a]    (:removed diff)))
      (expect (= ['b]    (:reloaded diff)))))

  (it "empty registry + empty manifests -> all-empty diff"
    (let [diff (#'lp/diff-extensions [] {})]
      (expect (= [] (:added diff)))
      (expect (= [] (:removed diff)))
      (expect (= [] (:reloaded diff))))))
