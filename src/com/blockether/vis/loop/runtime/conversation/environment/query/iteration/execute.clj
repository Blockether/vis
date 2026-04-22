(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.execute
  "SCI code execution.

   Takes a validated code string and runs it in the SCI sandbox.
   Handles timeouts, stdout/stderr capture, and common-mistake lint.
   Form repair was removed — if edamame rejects the syntax, the error
   is returned directly."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.shared :as helpers]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.validate :as validate]
   [com.blockether.vis.loop.storage.schema :refer [*eval-timeout-ms* *rlm-ctx*
                                                    clamp-eval-timeout-ms]]
   [sci.core :as sci]))

(defn- detect-common-mistakes
  "Pre-exec lint: catches common Clojure mistakes BEFORE SCI eval.
   Returns nil if clean, or an error string with actionable fix."
  [code]
  (let [s (str/trim code)]
    (cond
      (re-find #"#\([^)]*#\(" s)
      "Nested #() is illegal in Clojure. Rewrite inner #() as (fn [...] ...)"
      :else nil)))

(defn- run-sci-code
  "Evaluate `code` in `sci-ctx` with captured stdout/stderr.
   Returns {:result :stdout :stderr :error} with writers already closed."
  [sci-ctx code & {:keys [sandbox-ns]}]
  (let [stdout-writer (java.io.StringWriter.)
        stderr-writer (java.io.StringWriter.)
        err-pw       (java.io.PrintWriter. stderr-writer true)
        exec-future (future
                      (try
                        (let [result (sci/binding [sci/out stdout-writer
                                                   sci/err err-pw]
                                       (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                         (:val (sci/eval-string+ sci-ctx code
                                                 (when ns {:ns ns})))))]
                          {:result result :stdout (str stdout-writer) :stderr (str stderr-writer) :error nil})
                        (catch Throwable e
                          {:result nil :stdout (str stdout-writer) :stderr (str stderr-writer)
                           :error (str (.getSimpleName (class e)) ": " (or (ex-message e) (str e)))})))
        timeout-ms (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             {:result nil :stdout "" :stderr "" :error (str (.getSimpleName (class e)) ": " (ex-message e))}))]
    (.close stdout-writer)
    (.close stderr-writer)
    (if (nil? execution-result)
      (do (future-cancel exec-future)
        {:result nil :stdout "" :stderr "" :error (str "Timeout (" (/ timeout-ms 1000) "s)") :timeout? true})
      execution-result)))

(defn execute-code
  "Execute a single code block in the SCI sandbox.
   Returns {:result :stdout :stderr :error :execution-time-ms :timeout?}."
  [{:keys [sci-ctx sandbox-ns]} code & {:keys [timeout-ms]}]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :execute-code})]
    (let [start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)]
      (if lint-error
        (do (helpers/rlm-debug! {:lint-error lint-error} "Pre-exec lint caught mistake")
          {:result nil :stdout "" :stderr "" :error lint-error
           :execution-time-ms 0 :timeout? false})
        (if-let [parse-error (validate/parse-clojure-syntax code)]
          ;; Syntax error — return directly (no form repair)
          (do (helpers/rlm-debug! {:parse-error parse-error} "Edamame pre-parse failed")
            {:result nil :stdout "" :stderr "" :error parse-error
             :execution-time-ms 0 :timeout? false})
          ;; Normal execution path
          (let [execution-result (if timeout-ms
                                   (binding [*eval-timeout-ms* (clamp-eval-timeout-ms timeout-ms)]
                                     (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                                   (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                execution-time (- (System/currentTimeMillis) start-time)]
            (if (:timeout? execution-result)
              (assoc execution-result :execution-time-ms execution-time :timeout? true)
              (assoc execution-result :execution-time-ms execution-time :timeout? false))))))))
