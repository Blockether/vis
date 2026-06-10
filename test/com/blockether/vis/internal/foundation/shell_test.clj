(ns com.blockether.vis.internal.foundation.shell-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.foundation.shell :as shell]
   [com.blockether.vis.internal.resources :as resources]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

;; The impls are private (named for clarity inside the ns); reach them by var
;; so tests drive the real gate/render contract without the Python wrapper.
(def ^:private shell* @#'shell/shell-impl)
(def ^:private shell-bg* @#'shell/shell-bg-impl)
(def ^:private shell-logs* @#'shell/shell-logs-impl)
(def ^:private render-shell @#'shell/channel-render-shell)
(def ^:private render-bg @#'shell/channel-render-shell-bg)
(def ^:private render-logs @#'shell/channel-render-shell-logs)

(defn- threw?
  "lazytest has no `thrown?`; run `thunk` and report whether it threw."
  [thunk]
  (try (thunk) false (catch Throwable _ true)))

(defn- with-shell-on [f]
  (toggles/set-enabled! :vis/shell-tool true)
  (try (f) (finally (toggles/reset-to-default! :vis/shell-tool))))

(defn- poll
  "Re-run `thunk` until `pred` holds (default ~5s), returning the value."
  ([thunk pred] (poll thunk pred 50))
  ([thunk pred tries]
   (loop [i 0]
     (let [v (thunk)]
       (cond (pred v) v
         (>= i tries) (throw (ex-info "poll exhausted" {:last v}))
         :else (do (Thread/sleep 100) (recur (inc i))))))))

(defdescribe shell-toggle-gate-test
  (it "is OFF by default and short-circuits every call into a refusal envelope"
    (toggles/reset-to-default! :vis/shell-tool)
    (expect (false? (toggles/enabled? :vis/shell-tool)))
    (let [gate (@#'shell/shell-gate-before-fn :shell)
          out  (gate {:session-id "t"} identity ["echo hi"])]
      (expect (contains? out :result))
      (expect (extension/envelope-failure? (:result out)))
      (expect (= :shell-disabled (get-in out [:result :error :reason])))
      ;; hint must point the model at the USER-owned toggle, not a retry
      (expect (str/includes? (get-in out [:result :error :hint]) "USER"))))

  (it "injects env as the first arg when the toggle is ON"
    (with-shell-on
      (fn []
        (let [gate (@#'shell/shell-gate-before-fn :shell)
              out  (gate {:session-id "t"} identity ["echo hi"])]
          (expect (not (contains? out :result)))
          (expect (= [{:session-id "t"} "echo hi"] (:args out))))))))

(defdescribe shell-sync-test
  (it "returns stdout / stderr / exit / timed_out / duration_ms"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [r (:result (shell* {} "echo out; echo err 1>&2; exit 3"))]
            (expect (= :shell (:op r)))
            (expect (= "out\n" (:stdout r)))
            (expect (= "err\n" (:stderr r)))
            (expect (= 3 (:exit r)))
            (expect (false? (:timed_out r)))
            (expect (number? (:duration_ms r))))))))

  (it "treats a non-zero exit as a SUCCESS envelope (data, not a tool error)"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [env (shell* {} "exit 42")]
            (expect (extension/envelope-success? env))
            (expect (= 42 (:exit (:result env)))))))))

  (it "kills the process tree on timeout and reports timed_out with nil exit"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [t0 (System/currentTimeMillis)
                r  (:result (shell* {} "sleep 30" {:timeout_secs 1}))
                dt (- (System/currentTimeMillis) t0)]
            (expect (true? (:timed_out r)))
            (expect (nil? (:exit r)))
            (expect (< dt 15000)))))))

  (it "rejects a cwd that escapes the workspace root"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (expect (threw? #(shell* {} "pwd" {:cwd "../.."})))))))

  (it "accepts a float timeout but rejects a non-numeric one with a typed error"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          ;; GraalPy can hand over a Double; it must coerce, not throw
          (expect (extension/envelope-success? (shell* {} "true" {:timeout_secs 1.0})))
          (let [thrown (try (shell* {} "true" {:timeout_secs "30"}) nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
            (expect (= ::shell/bad-option (:type thrown)))))))))

(defdescribe shell-background-test
  (it "registers a session resource, tails logs, and stops cleanly via the registry"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [sid "shell-test-bg"
                env {:session-id sid}]
            (try
              (let [reg (:result (shell-bg* env "worker" "echo l1; echo l2; sleep 60"))]
                (expect (= "running" (:status reg)))
                (expect (pos? (:pid reg))))
              ;; resource is visible to the owning session
              (let [rs (resources/list-resources sid)]
                (expect (= 1 (count rs)))
                (expect (= "worker" (:id (first rs))))
                (expect (= :shell (:kind (first rs))))
                (expect (true? (:can-stop (first rs)))))
              ;; logs arrive in [seq, line] tuples
              (let [r (poll #(:result (shell-logs* env "worker"))
                        #(>= (count (:lines %)) 2))]
                (expect (= [1 "l1"] (vec (first (:lines r)))))
                (expect (= "running" (:status r))))
              ;; the registry stop path runs our stop-fn AND removes the resource
              (let [stop (resources/stop! sid "worker")]
                (expect (= :stopped (:result stop)))
                (expect (empty? (resources/list-resources sid)))
                ;; logs are gone after stop (buffer dropped)
                (expect (threw? #(shell-logs* env "worker"))))
              (finally (resources/stop-all! sid))))))))

  (it "keeps an exited process listed (status :exited) with readable logs + exit"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [sid "shell-test-exit"
                env {:session-id sid}]
            (try
              (shell-bg* env "quick" "echo done; exit 7")
              (let [r (poll #(:result (shell-logs* env "quick"))
                        #(= "exited" (:status %)))]
                (expect (= 7 (:exit r)))
                (expect (= [[1 "done"]] (mapv vec (:lines r)))))
              ;; NOT pruned — alive-fn keeps it until resource_stop
              (let [res (first (resources/list-resources sid))]
                (expect (some? res))
                (expect (= :exited (:status res))))
              (finally (resources/stop-all! sid))))))))

  (it "refuses a duplicate id while the first is still running"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [sid "shell-test-dup"
                env {:session-id sid}]
            (try
              (shell-bg* env "dup" "sleep 60")
              (expect (threw? #(shell-bg* env "dup" "sleep 60")))
              (finally (resources/stop-all! sid))))))))

  (it "surfaces cwd and uptime_ms in the logs payload"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [sid "shell-test-meta"
                env {:session-id sid}]
            (try
              (shell-bg* env "m" "sleep 60")
              (let [r (:result (shell-logs* env "m"))]
                (expect (string? (:cwd r)))
                (expect (>= (:uptime_ms r) 0)))
              (finally (resources/stop-all! sid))))))))

  (it "stops promptly even when the command double-forks a detached daemon"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let [sid "shell-test-nohup"
                env {:session-id sid}]
            (try
              ;; nohup ... & detaches a grandchild that escapes kill-tree's
              ;; reach; the stop must STILL return promptly (stream close
              ;; unblocks the pump) and drop the resource.
              (shell-bg* env "d" "nohup sleep 120 >/dev/null 2>&1 & echo spawned; sleep 60")
              (let [t0   (System/currentTimeMillis)
                    stop (resources/stop! sid "d")
                    dt   (- (System/currentTimeMillis) t0)]
                (expect (= :stopped (:result stop)))
                (expect (< dt 8000))
                (expect (empty? (resources/list-resources sid))))
              (finally (resources/stop-all! sid)))))))))

(defdescribe shell-render-contract-test
  (it "every renderer returns the {:summary :display} contract"
    (expect (extension/render-fn-result?
              (render-shell {:cmd "echo hi" :exit 0 :timed_out false
                             :timeout_secs 120 :stdout "hi\n" :stderr ""
                             :duration_ms 12})))
    (expect (extension/render-fn-result?
              (render-shell {:cmd "x" :exit nil :timed_out true
                             :timeout_secs 1 :stdout "" :stderr "boom"
                             :duration_ms 1000})))
    (expect (extension/render-fn-result?
              (render-bg {:id "w" :pid 4242 :cmd "npm run dev" :status "running"})))
    (expect (extension/render-fn-result?
              (render-logs {:id "w" :status "exited" :exit 0
                            :lines [[1 "a"] [2 "b"]] :shown_count 2
                            :line_count 2 :dropped 0})))))

(defdescribe shell-prompt-test
  (it "is empty when OFF and advertises the surface when ON"
    (toggles/reset-to-default! :vis/shell-tool)
    (expect (= "" (shell/shell-prompt {})))
    (with-shell-on
      (fn []
        (let [p (shell/shell-prompt {})]
          (expect (str/includes? p "shell_bg"))
          (expect (str/includes? p "resource_stop")))))))

(defdescribe shell-extension-wiring-test
  (it "exposes shell / shell-bg / shell-logs as built-in sandbox bindings"
    (let [ks (set (keys (extension/builtin-sandbox-bindings (constantly {}))))]
      (expect (contains? ks 'shell))
      (expect (contains? ks 'shell-bg))
      (expect (contains? ks 'shell-logs)))))
