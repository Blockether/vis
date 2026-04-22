(ns com.blockether.vis.loop.runtime.conversation.environment.core
  "Environment orchestration: hook system, tool registration, execute-tool."
  (:require
   [com.blockether.vis.loop.tool :as sci-tool]
   [com.blockether.vis.loop.runtime.shared :as sci-shared]
   [com.blockether.vis.loop.runtime.tool-diagnostics :as tool-diag]
   [com.blockether.vis.loop.runtime.conversation.environment.base :as base]
   [sci.core :as sci]
   [taoensso.trove :as trove]))

;; Re-export base primitives so callers that alias environment.core as
;; rlm-tools can reach everything without a second require.
(def create-sci-context base/create-sci-context)
(def build-var-index base/build-var-index)
(def parse-rich-code base/parse-rich-code)
(def sci-update-binding! base/sci-update-binding!)
(def bump-var-index! base/bump-var-index!)
(def bind-and-bump! base/bind-and-bump!)

(declare execute-tool register-tool-def! wrap-tool-for-sci)

;; MAX_VAR_INDEX_ROWS was removed — every defined var shows up in
;; <var_index>. With the `code` column (bounded to
;; MAX_VAR_INDEX_CODE_CHARS per row) instead of raw value previews,
;; even dozens of vars stay cheap to render.
;; =============================================================================
;; Hook System v3 - per-tool + global hooks with policy/observation split
;; =============================================================================
;;
;; Per-tool hooks form three chains: :before / :wrap / :after.
;; Global hooks live on query-env! opts and are pure observers.

(def MAX_HOOK_DEPTH
  "Ceiling on :invoke recursion depth per top-level tool dispatch."
  8)

(def DEFAULT_QUERY_CTX
  "Fallback query context used by register-env-fn!'s immediate SCI flash
   when no query-env! call is active."
  {:hooks nil
   :iteration-atom nil
   :depth 0
   :parent-dispatch-id nil})

(defn- status-id
  [status]
  (when status
    (keyword "rlm.status" (name status))))

(defn- ensure-error-map
  [err default-id]
  (cond
    (nil? err) nil
    (map? err) (cond-> err
                 (not (:error-id err)) (assoc :error-id default-id))
    :else {:type :rlm/unknown-tool-error
           :error-id default-id
           :message (str err)}))

(defn- gen-anon-id
  [stage]
  (gensym (str "anon/" (name stage) "-")))

(defn- normalize-hook-entry
  [stage entry]
  (cond
    (fn? entry)
    {:id (gen-anon-id stage) :fn entry}

    (and (map? entry) (fn? (:fn entry)))
    (update entry :id #(or % (gen-anon-id stage)))

    :else
    (throw (ex-info (str "Invalid hook entry for :" (name stage) " - "
                      "must be a fn or {:id :fn} map")
             {:type :rlm/invalid-hook-entry :stage stage :entry entry}))))

(defn normalize-hooks
  "Coerce a hook spec for one stage into a canonical vec of {:id :fn} maps."
  [stage hooks]
  (cond
    (nil? hooks) []
    (fn? hooks) [(normalize-hook-entry stage hooks)]
    (map? hooks) [(normalize-hook-entry stage hooks)]
    (sequential? hooks) (mapv #(normalize-hook-entry stage %) hooks)
    :else
    (throw (ex-info (str "Invalid hooks shape for :" (name stage))
             {:type :rlm/invalid-hooks-shape :stage stage :hooks hooks}))))

(defn merge-hook-chain
  "Merge incoming hook entries into an existing chain by :id."
  [existing incoming]
  (let [incoming-by-id (into {} (map (juxt :id identity)) incoming)
        merged (mapv (fn [entry]
                       (if-let [updated (get incoming-by-id (:id entry))]
                         updated
                         entry))
                 existing)
        existing-ids (set (map :id existing))
        appended (vec (remove #(contains? existing-ids (:id %)) incoming))]
    (vec (concat merged appended))))

(defn- merge-tool-def-hooks
  [existing new-def]
  (let [new-before (normalize-hooks :before (:before new-def))
        new-after  (normalize-hooks :after  (:after  new-def))
        new-wrap   (normalize-hooks :wrap   (:wrap   new-def))
        old-hooks  (:hooks existing {})]
    (-> new-def
      (assoc :hooks {:before (merge-hook-chain (:before old-hooks []) new-before)
                     :after  (merge-hook-chain (:after old-hooks [])  new-after)
                     :wrap   (merge-hook-chain (:wrap old-hooks [])   new-wrap)})
      (dissoc :before :after :wrap))))

(defn- run-before-chain
  [hooks invocation]
  (reduce
    (fn [acc {:keys [id fn]}]
      (let [current-inv (assoc invocation :args (:args acc))]
        (try
          (let [ret (fn current-inv)]
            (cond
              (or (nil? ret) (= {} ret))
              acc

              (and (map? ret) (contains? ret :error))
              (reduced (assoc acc
                         :short-circuit {:result nil
                                         :error (ensure-error-map (:error ret)
                                                  :rlm.error/hook-before-returned-error)}
                         :skipped? true))

              (and (map? ret) (contains? ret :skip))
              (reduced (assoc acc
                         :short-circuit {:result (:skip ret) :error nil}
                         :skipped? true))

              (and (map? ret) (contains? ret :args))
              (assoc acc :args (:args ret))

              :else
              (do (trove/log! {:level :warn
                               :data {:stage :before :id id :ret ret}
                               :msg "Before hook returned unknown shape; ignoring"})
                acc)))
          (catch Throwable t
            (reduced (assoc acc
                       :short-circuit {:result nil
                                       :error (assoc (sci-shared/format-exception-short t)
                                                :error-id :rlm.error/hook-exception
                                                :stage :before :id id)}
                       :skipped? true))))))
    {:args (:args invocation) :short-circuit nil :skipped? false}
    hooks))

(defn- run-after-chain
  [hooks initial-outcome]
  (reduce
    (fn [outcome {:keys [id fn]}]
      (try
        (let [ret (fn outcome)]
          (cond
            (or (nil? ret) (= {} ret))
            outcome

            (and (map? ret) (or (contains? ret :result) (contains? ret :error)))
            (cond-> outcome
              (contains? ret :result) (assoc :result (:result ret))
              (contains? ret :error)  (assoc :error
                                        (ensure-error-map (:error ret)
                                          :rlm.error/hook-after-returned-error)))

            :else
            (do (trove/log! {:level :warn
                             :data {:stage :after :id id :ret ret}
                             :msg "After hook returned unknown shape; ignoring"})
              outcome)))
        (catch Throwable t
          (update outcome :hook-errors (fnil conj [])
            {:stage :after :id id
             :error (assoc (sci-shared/format-exception-short t)
                      :error-id :rlm.error/hook-exception)}))))
    initial-outcome
    hooks))

(defn- compose-wrap-chain
  [wraps base-handler]
  (reduce
    (fn [handler {:keys [fn]}]
      (fn handler))
    base-handler
    wraps))

(defn- make-invoke-fn
  [env tool-registry-atom parent-dispatch-id parent-query-ctx]
  (fn invoke [sym args]
    (let [next-depth (inc (:depth parent-query-ctx 0))]
      (when (>= next-depth MAX_HOOK_DEPTH)
        (throw (ex-info (str "Hook :invoke recursion limit reached ("
                          MAX_HOOK_DEPTH ")")
                 {:type :rlm/hook-recursion-limit
                  :depth next-depth
                  :sym sym}))))
    (let [tool-def (get @tool-registry-atom sym)]
      (when-not tool-def
        (throw (ex-info (str "Unknown tool for :invoke - " sym " not registered")
                 {:type :rlm/unknown-invoke-tool :sym sym})))
      (let [child-ctx (-> parent-query-ctx
                        (assoc :hooks nil
                          :depth (inc (:depth parent-query-ctx 0))
                          :parent-dispatch-id parent-dispatch-id))
            outcome (execute-tool env sym (:fn tool-def) (vec args)
                      {:tool-hooks (:hooks tool-def)
                       :tool-def tool-def
                       :tool-registry-atom tool-registry-atom
                       :query-ctx child-ctx})]
        (if (:error outcome)
          (throw (ex-info (get-in outcome [:error :message] "invoke failed")
                   {:type :rlm/invoke-error :outcome outcome}))
          (:result outcome))))))

(defn- apply-tool-formatter
  "Run the tool-def's :format-result-fn on `raw-result` and, when possible,
   attach `:rlm/format` + `:rlm/formatted` metadata so downstream consumers
   (LLM serializer, println override, var index) can skip recomputing.

   Contract:
   - Always returns `{:result value-maybe-with-meta :formatted string}`.
   - Formatter exceptions are swallowed: fallback to pr-str of the raw value
     so one misbehaving formatter can't nuke a tool call.
   - Non-IObj values (strings, numbers, keywords, booleans, nil) pass through
     unchanged — metadata isn't attachable. The serializer still receives
     `:formatted` on the outcome map and can use tool-sym lookup for primitives.
   - When `raw-result` already carries `:rlm/format` meta (e.g. a tool returned
     an already-formatted nested value), we don't overwrite it; the upstream
     formatter wins. This keeps composition sane."
  [tool-def raw-result]
  (let [fmt (:format-result-fn tool-def)
        formatted (if fmt
                    (try (fmt raw-result)
                      (catch Throwable _ (pr-str raw-result)))
                    (pr-str raw-result))
        existing-meta (when (instance? clojure.lang.IObj raw-result) (meta raw-result))
        already-formatted? (contains? existing-meta :rlm/format)
        result' (cond
                  (not (instance? clojure.lang.IObj raw-result)) raw-result
                  already-formatted? raw-result
                  :else (vary-meta raw-result assoc
                          :rlm/format (or fmt (constantly formatted))
                          :rlm/formatted formatted))]
    {:result result' :formatted formatted}))

(defn execute-tool
  "Core tool invocation pipeline. Runs :before chain -> :wrap middleware ->
   fn -> :after chain, firing global :on-tool-invoked / :on-tool-completed
   observers around the whole thing."
  [env sym user-fn args {:keys [tool-hooks tool-def tool-registry-atom query-ctx]}]
  (let [{:keys [hooks iteration-atom parent-dispatch-id]} (or query-ctx DEFAULT_QUERY_CTX)
        dispatch-id (str (sci-shared/->uuid))
        cancel-atom (:cancel-atom env)
        before-hooks (or (:before tool-hooks) [])
        after-hooks  (or (:after tool-hooks)  [])
        wrap-hooks   (or (:wrap tool-hooks)   [])
        invoke-fn (make-invoke-fn env tool-registry-atom dispatch-id
                    (or query-ctx DEFAULT_QUERY_CTX))
        cancel-fn (fn [] (when cancel-atom (reset! cancel-atom true)))
        current-iteration (if iteration-atom @iteration-atom 0)
        invocation {:sym sym
                    :args args
                    :iteration current-iteration
                    :env env
                    :dispatch-id dispatch-id
                    :parent-dispatch-id parent-dispatch-id
                    :invoke invoke-fn
                    :cancel! cancel-fn}]
    (when-let [g (:on-tool-invoked hooks)]
      (try (g invocation)
        (catch Throwable t
          (trove/log! {:level :warn
                       :data (sci-shared/format-exception-short t)
                       :msg ":on-tool-invoked observer threw; ignoring"}))))
    (let [{transformed-args :args short-circuit :short-circuit skipped? :skipped?}
          (run-before-chain before-hooks invocation)
          start-ns (System/nanoTime)
          {:keys [result error]}
          (cond
            short-circuit
            short-circuit

            :else
            (let [validate-input  (:validate-input-fn tool-def)
                  validate-output (:validate-output-fn tool-def)
                  rescue-fn       (:rescue-fn tool-def)
                  ;; Wrap validate-output in a local fn so we can reuse it
                  ;; for both the happy path and the rescued path without
                  ;; duplicating the three-branch coerce/default logic.
                  apply-validate-output
                  (fn [validated-inv raw-result]
                    (if validate-output
                      (let [ret (validate-output (assoc validated-inv :result raw-result))]
                        (if (and (map? ret) (contains? ret :result))
                          (:result ret)
                          raw-result))
                      raw-result))
                  base-handler (fn [inv]
                                 (try
                                   (let [validated-inv (if validate-input
                                                         (let [ret (validate-input inv)]
                                                           (if (and (map? ret) (contains? ret :args))
                                                             (assoc inv :args (:args ret))
                                                             inv))
                                                         inv)
                                         args          (:args validated-inv)
                                         ;; Isolate the tool-fn call so its
                                         ;; exceptions can be intercepted by
                                         ;; `:rescue-fn` without catching
                                         ;; validator or reducer errors.
                                         raw-result    (try
                                                         (apply user-fn args)
                                                         (catch Throwable t
                                                           (if rescue-fn
                                                             ;; Rescue handler: may return a value,
                                                             ;; return nil (treated as successful nil),
                                                             ;; or throw (original or replacement error).
                                                             (apply rescue-fn t args)
                                                             (throw t))))
                                         final-result  (apply-validate-output validated-inv raw-result)]
                                     {:result final-result :error nil})
                                   (catch Throwable t
                                     {:result nil
                                      :error (assoc (sci-shared/format-exception-short t)
                                               :error-id :rlm.error/tool-exception)})))
                  composed (compose-wrap-chain wrap-hooks base-handler)]
              (try
                (composed (assoc invocation :args transformed-args))
                (catch Throwable t
                  {:result nil
                   :error (assoc (sci-shared/format-exception-short t)
                            :error-id :rlm.error/wrap-exception)}))))
          duration-ms (double (/ (- (System/nanoTime) start-ns) 1e6))
          {formatted-result :result formatted-str :formatted}
          (if (or error (nil? tool-def))
            {:result result :formatted nil}
            (apply-tool-formatter tool-def result))
          initial-outcome (merge invocation
                            {:args transformed-args
                             :result formatted-result
                             :formatted formatted-str
                             :error (ensure-error-map error :rlm.error/tool-error)
                             :duration-ms duration-ms
                             :skipped? (boolean skipped?)
                             :hook-errors []})
          final-outcome (let [post-after (run-after-chain after-hooks initial-outcome)
                              status (cond
                                       (:error post-after) :error
                                       (:skipped? post-after) :skipped
                                       :else :success)]
                          (assoc post-after :status status :status-id (status-id status)))]
      (when-let [g (:on-tool-completed hooks)]
        (try (g final-outcome)
          (catch Throwable t
            (trove/log! {:level :warn
                         :data (sci-shared/format-exception-short t)
                         :msg ":on-tool-completed observer threw; ignoring"}))))
      ;; Record execution diagnostics
      (tool-diag/record-execution! sym (long (* duration-ms 1e6)) (boolean (:error final-outcome)))
      final-outcome)))

(defn wrap-tool-for-sci
  "Build the fn that gets bound into the SCI sandbox for a registered tool.
   Returns :result on success or throws an ex-info on :error."
  ([env sym user-fn tool-registry-atom]
   (wrap-tool-for-sci env sym user-fn tool-registry-atom DEFAULT_QUERY_CTX))
  ([env sym user-fn tool-registry-atom query-ctx]
   (fn wrapped-tool [& args]
     (let [outcome (execute-tool env sym user-fn (vec args)
                     {:tool-hooks (get-in @tool-registry-atom [sym :hooks])
                      :tool-def (get @tool-registry-atom sym)
                      :tool-registry-atom tool-registry-atom
                      :query-ctx (assoc query-ctx
                                   :depth 0
                                   :parent-dispatch-id nil)})]
       (if-let [err (:error outcome)]
         (throw (ex-info (or (:message err) "tool error") err))
         (:result outcome))))))

(defn list-tool-hooks
  "Return a map describing the hook chains registered for `sym`."
  [hook-registry-atom sym]
  (when-let [tool-def (get @hook-registry-atom sym)]
    (let [describe (fn [entries]
                     (vec (map-indexed
                            (fn [i {:keys [id fn]}]
                              {:id id
                               :position i
                               :fn-name (or (some-> fn class .getSimpleName) "fn")})
                            entries)))
          hooks (:hooks tool-def {})]
      {:before (describe (:before hooks []))
       :after  (describe (:after hooks []))
       :wrap   (describe (:wrap hooks []))})))

(defn list-registered-tools
  "Return a vec of {:sym :hook-counts} maps summarizing every registered tool."
  [hook-registry-atom]
  (vec
    (for [[sym tool-def] @hook-registry-atom]
      {:sym sym
       :hook-counts {:before (count (get-in tool-def [:hooks :before]))
                     :after  (count (get-in tool-def [:hooks :after]))
                     :wrap   (count (get-in tool-def [:hooks :wrap]))}})))

(defn unregister-hook!
  "Remove a hook entry by :id from a given stage. Returns true/false."
  [hook-registry-atom sym stage id]
  (let [removed? (atom false)]
    (swap! hook-registry-atom
      (fn [registry]
        (if-let [tool-def (get registry sym)]
          (let [old-entries (get-in tool-def [:hooks stage] [])
                new-entries (vec (remove #(= id (:id %)) old-entries))]
            (when (not= (count old-entries) (count new-entries))
              (reset! removed? true))
            (assoc-in registry [sym :hooks stage] new-entries))
          registry)))
    @removed?))

(defn register-tool-def!
  "Register or layer a tool-def in `hook-registry-atom`."
  [hook-registry-atom sym tool-def]
  (let [canonical (merge-tool-def-hooks
                    (get @hook-registry-atom sym {})
                    tool-def)]
    (sci-tool/maybe-assert-fn-tool-def! canonical)
    (swap! hook-registry-atom assoc sym canonical)
    canonical))
