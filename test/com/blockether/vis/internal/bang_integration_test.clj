(ns com.blockether.vis.internal.bang-integration-test
  "Engine loop integration of `!`/`!&` shell-sugar dispatch.

   Asserts that `run-turn!` short-circuits the LLM round-trip when the user
   message is a bang, runs the shell tool directly (honoring `:shell/enabled`),
   and persists one synthetic iteration whose form owns the command's stdout."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.gateway.resources :as resources]
            [com.blockether.vis.internal.foundation.shell-test :as shell-fixture]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]))

;; A bang runs the `shell` op, which is tagged by the foundation-core extension:
;; register it when no earlier test in this JVM has, so the namespace passes alone.
(set-ns-context! [(around-each [f]
                               (let [registered? (some #(= "foundation-core" (:ext/name %))
                                                       (extension/registered-extensions))]
                                 (when-not registered? (foundation/register!))
                                 (try (f)
                                      (finally (when-not registered?
                                                 (extension/deregister-extension!
                                                   "foundation-core"))))))])

(defn- with-store
  [f]
  (let [store (ps/db-open! :memory)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- bang-env
  ([store] (bang-env store "/tmp"))
  ([store root]
   (let [ws
         (persistance/db-workspace-insert! store {:repo-id "test" :root root :state :active})

         soul-id
         (persistance/db-store-session! store
                                        {:channel :tui :workspace-id (:id ws) :title "bang-test"})]

     {:extensions (atom [])
      :db-info store
      :session-id soul-id
      :channel :tui
      :workspace ws
      :workspace/id (:id ws)
      :workspace/root (:root ws)
      :ctx-atom (ctx-loop/make-ctx-atom soul-id)
      :turn-state-atom (ctx-loop/make-turn-state-atom)})))

(defdescribe parse-bang-test
             (it "recognizes ! (sync) and !& (background), rejecting non-bangs and bare markers"
                 (let [pb #'turn/parse-bang]
                   (expect (= {:kind :run :cmd "ls -la"} (pb "!ls -la")))
                   (expect (= :bg (:kind (pb "!&npm run dev"))))
                   (expect (= "npm run dev" (:cmd (pb "!&npm run dev"))))
                   (expect (some? (:id (pb "!&npm run dev"))))
                   (expect (= {:kind :run :cmd "echo hi"} (pb "  !echo hi ")))
                   (expect (nil? (pb "hello world")))
                   (expect (nil? (pb "/help")))
                   (expect (nil? (pb "!")))
                   (expect (nil? (pb "!&")))
                   (expect (nil? (pb "!& ")))
                   (expect (nil? (pb nil))))))

(defdescribe
  run-turn-bang-test
  (it
    "runs shell directly and persists stdout as the only command output"
    (with-store
      (fn [store]
        (let [env
              (bang-env store)

              called
              (atom 0)

              chunks
              (atom [])

              result
              (with-redefs [iteration/iteration-loop
                            (fn [& _]
                              (swap! called inc)
                              {:status :success})

                            toggles/enabled?
                            (fn [_]
                              true)]

                (turn/run-turn! env
                                "!echo hi-from-bang"
                                {:hooks {:on-chunk #(swap! chunks conj %)}}))

              turns
              (persistance/db-list-session-turns store (:session-id env))

              iters
              (persistance/db-list-session-turn-iterations store (:id (first turns)))

              form
              (first (:forms (first iters)))

              output
              (some #(when (= :form-result (:phase %)) %) @chunks)]

          ;; No LLM round-trip.
          (expect (= 0 @called))
          (expect (= :success (:status result)))
          (expect (= :complete (:prior-outcome result)))
          ;; The raw command is the persisted turn request.
          (expect (= "!echo hi-from-bang" (:user-request (first turns))))
          ;; One synthetic iteration and one canonical stdout carrying output plus status.
          (expect (= 1 (count turns)))
          (expect (= :user-shell (:tag form)))
          (expect (= "shell" (:vis/tool-name form)))
           (let [ctx (#'shell-fixture/py-ctx env)]
             (try (let [replay (ep/run-python-block ctx
                                (str (:src form) "\nprint(result.out.strip())"))]
                    (expect (nil? (:error replay)) (pr-str replay))
                    (expect (= "hi-from-bang\n" (:stdout replay))))
                  (finally (ep/dispose-python-context! ctx))))
          (expect (= "hi-from-bang\nexit 0" (:stdout form)))
          (expect (not (contains? form :result)))
          (expect (not (contains? form :result-summary)))
          ;; The same factual shape is streamed live; no answer prose duplicates it.
          (expect (= "hi-from-bang\nexit 0" (:stdout output)))
          (expect (not (contains? output :result-summary)))
          (expect (= "" (:answer result))))))))

;; Regression: a `!`/`!&` bang turn ran in the PROCESS cwd instead of the
;; session's own workspace root — `!ls -la` listed the wrong directory.
;; The bang path calls the shell var DIRECTLY, so it bypassed the one seam that
;; binds `workspace/*workspace-root*` from the environment.
(defdescribe run-turn-bang-workspace-cwd-test
             (it "runs the bang command in the session's workspace root, not the process cwd"
                 (with-store
                   (fn [store]
                     (let [root
                           (.getCanonicalPath
                             (.toFile (java.nio.file.Files/createTempDirectory
                                        "vis-bang-cwd"
                                        (into-array java.nio.file.attribute.FileAttribute []))))

                           env
                           (bang-env store root)

                           result
                           (with-redefs [toggles/enabled? (fn [_]
                                                            true)]
                             (turn/run-turn! env "!pwd" {}))

                           turns
                           (persistance/db-list-session-turns store (:session-id env))

                           iters
                           (persistance/db-list-session-turn-iterations store (:id (first turns)))

                           lines
                           (str/split-lines (:stdout (first (:forms (first iters)))))]

                       (expect (= :success (:status result)))
                       (expect (= [root "exit 0"] lines)))))))

(defdescribe run-turn-bang-background-replay-test
  (it "replays a background bang as a shell handle under the original alias"
      (with-store
        (fn [store]
          (let [env (bang-env store)
                cmd "!&printf '%s' 'quoted \"value\"'"
                result (with-redefs [toggles/enabled? (constantly true)]
                         (turn/run-turn! env cmd {}))
                [turn] (persistance/db-list-session-turns store (:session-id env))
                [iteration] (persistance/db-list-session-turn-iterations store (:id turn))
                [form] (:forms iteration)
                alias (re-find #"background-[a-f0-9]{8}" (:stdout form))
                ctx (#'shell-fixture/py-ctx env)]
            (try (let [replay (ep/run-python-block ctx
                                (str (:src form) "\nprint(sh.id)\nprint((await sh).out.strip())"))]
                   (expect (= :success (:status result)))
                   (expect (nil? (:error replay)) (pr-str replay))
                   (expect (= (str alias "\nquoted \"value\"\n") (:stdout replay))))
                 (finally (ep/dispose-python-context! ctx))))))))

(defdescribe run-turn-bang-timeout-replay-test
  (it "leaves the shell running when the saved foreground wait expires"
      (with-store
        (fn [store]
          (let [env (bang-env store)
                sid (:session-id env)
                ctx (#'shell-fixture/py-ctx env)]
            (try
              (with-redefs [toggles/enabled? (constantly true) shell/default-timeout-secs 1]
                (turn/run-turn! env "!sleep 3; echo finished" {}))
              (let [[turn] (persistance/db-list-session-turns store sid)
                    [iteration] (persistance/db-list-session-turn-iterations store (:id turn))
                    [form] (:forms iteration)
                    replay (ep/run-python-block ctx (str (:src form) "\nprint(sh.snapshot().status)"))]
                (expect (str/includes? (:stdout form) "Timed out after 1s"))
                (expect (nil? (:error replay)) (pr-str replay))
                (expect (= "running\n" (:stdout replay))))
              (finally (resources/stop-all! sid) (ep/dispose-python-context! ctx))))))))

(defdescribe run-turn-bang-disabled-test
             (it "refuses when the shell layer is OFF, without running the command"
                 (with-store (fn [store]
                               (let [env
                                     (bang-env store)

                                     result
                                     (with-redefs [iteration/iteration-loop
                                                   (fn [& _]
                                                     (throw (ex-info "should not run" {})))

                                                   toggles/enabled?
                                                   (fn [_]
                                                     false)]

                                       (turn/run-turn! env "!echo nope" {}))]

                                 (expect (= :success (:status result))))))))
