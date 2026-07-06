(ns com.blockether.vis.internal.bang-integration-test
  "Engine loop integration of `!`/`!&` shell-sugar dispatch.

   Asserts that `run-turn!` short-circuits the LLM round-trip when the user
   message is a bang, runs the shell tool directly (honoring `:shell/enabled`),
   and persists ONE synthetic iteration whose form carries the shell RESULT map
   at `:tag :user-shell` — so the op-card renders as the answer bubble and the
   result rides later prompts' prior-turn context like a real shell tool call."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.ext.persistance-sqlite.registrar]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.toggles :as toggles]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- bang-env
  [store]
  (let [ws
        (persistance/db-workspace-insert!
          store
          {:repo-id "test" :repo-root "/tmp" :root "/tmp" :state :active :fork-ms 0})

        soul-id
        (persistance/db-store-session!
          store
          {:channel :tui :workspace-id (:id ws) :title "bang-test" :system-prompt ""})]

    {:extensions (atom [])
     :db-info store
     :session-id soul-id
     :channel :tui
     :workspace ws
     :workspace/id (:id ws)
     :ctx-atom (ctx-loop/make-ctx-atom soul-id)
     :turn-state-atom (ctx-loop/make-turn-state-atom)}))

(defdescribe parse-bang-test
             (it "recognizes ! (sync) and !& (background), rejecting non-bangs and bare markers"
                 (let [pb #'lp/parse-bang]
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
    "runs shell directly, skips iteration-loop, persists a :user-shell result form"
    (with-store
      (fn [store]
        (let [env
              (bang-env store)

              called
              (atom 0)

              result
              (with-redefs [lp/iteration-loop
                            (fn [& _]
                              (swap! called inc)
                              {:status :success})

                            toggles/enabled?
                            (fn [_]
                              true)]

                (lp/run-turn! env "!echo hi-from-bang" {}))

              turns
              (persistance/db-list-session-turns store (:session-id env))

              iters
              (persistance/db-list-session-turn-iterations store (:id (first turns)))

              form
              (first (:forms (first iters)))]

          ;; No LLM round-trip.
          (expect (= 0 @called))
          (expect (= :success (:status result)))
          (expect (= :complete (:prior-outcome result)))
          ;; The op-card renders as the answer bubble.
          (expect (str/includes? (:answer result) "$ echo hi-from-bang"))
          (expect (str/includes? (:answer result) "hi-from-bang"))
          ;; The raw command is the persisted turn request.
          (expect (= "!echo hi-from-bang" (:user-request (first turns))))
          ;; ONE synthetic iteration with a native-shell form the model reads
          ;; back in prior-turn context, tagged so channels suppress the trace.
          (expect (= 1 (count turns)))
          (expect (= :user-shell (:tag form)))
          (expect (= "shell_run" (:vis/tool-name form)))
          (expect (= "hi-from-bang\n" (get (:result form) "stdout")))
          (expect (= 0 (get (:result form) "exit"))))))))

(defdescribe run-turn-bang-disabled-test
             (it "refuses when the shell layer is OFF, without running the command"
                 (with-store (fn [store]
                               (let [env
                                     (bang-env store)

                                     result
                                     (with-redefs [lp/iteration-loop
                                                   (fn [& _]
                                                     (throw (ex-info "should not run" {})))

                                                   toggles/enabled?
                                                   (fn [_]
                                                     false)]

                                       (lp/run-turn! env "!echo nope" {}))]

                                 (expect (= :success (:status result)))
                                 (expect (str/includes? (:answer result) "Shell layer is OFF")))))))
