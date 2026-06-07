(ns com.blockether.vis.ext.foundation-bridge.core
  "Bridge verification tools for Vis."
  (:refer-clojure :exclude [next])
  (:require [bridge.cli :as br-cli]
            [bridge.artifacts :as artifacts]
            [bridge.evidence :as evidence]
            [bridge.io :as bio]
            [bridge.next :as br-next]
            [bridge.policy :as policy]
            [bridge.profile :as br-profile]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-bridge.render :as br-render]
            [com.blockether.vis.internal.extension :as extension]))

(def ^:private default-profile-paths
  [".bridge/profile.edn"
   ".bridge/persistent/profile.edn"])

(defn- now-ms [] (System/currentTimeMillis))

(defn- workspace-root
  [env]
  (or (:workspace/root env)
    (System/getProperty "user.dir")))

(defn- normalize-opts
  [opts]
  (cond
    (nil? opts) {}
    (map? opts) opts
    :else (throw (ex-info "Bridge opts must be a map."
                   {:type :vis.bridge/invalid-opts
                    :opts opts}))))

(defn- ensure-vector
  [x]
  (cond
    (nil? x) []
    (vector? x) x
    (sequential? x) (vec x)
    :else [x]))

(defn- py-tool-name
  "Convert a `br/foo-bar` tool name to a Python snake_case function name."
  [tool]
  (-> (str tool)
    (str/replace "/" "_")
    (str/replace "-" "_")))

(defn- py-arg
  "Render a single arg as a Python literal (strings quoted, nil → None)."
  [x]
  (cond
    (nil? x)     "None"
    (string? x)  (str "\"" x "\"")
    (boolean? x) (if x "True" "False")
    :else        (str x)))

(defn- tool-call
  [tool args]
  {:tool tool
   :args (vec args)
   :call (str (py-tool-name tool)
           "("
           (str/join ", " (map py-arg args))
           ")")})

(defn- render-tool-call
  [{:keys [call]}]
  (or call "br_init()"))

(defn- action->extension-op
  [action]
  (when action
    {:kind :extension-op
     :summary (:summary action)
     :reason (:reason action)
     :required-evidence (vec (:required-evidence action))
     :op (tool-call "br/run-evidence" [(:evidence-id action)])}))

(defn- action-command
  [obligation]
  (first (:actions obligation)))

(defn- obligation->flat
  [obligation]
  (let [required-evidence (vec (:required-evidence obligation))]
    (cond-> (select-keys obligation [:kind :subject :artifact :summary :reason :state
                                     :required-evidence :actions])
      true (assoc :evidence-kinds required-evidence)
      (= 1 (count required-evidence)) (assoc :evidence-kind (first required-evidence))
      (seq (:actions obligation)) (assoc :command (action-command obligation)))))

(defn- evidence-receipts
  [status-result]
  (let [artifact-root (when-let [root (:artifact-root status-result)]
                        (if-let [profile-root (:profile-root status-result)]
                          (bio/resolve-path profile-root root)
                          root))]
    (->> (when artifact-root
           (artifacts/find-artifacts artifact-root))
      (filter #(= "evidence-run" (:artifact %)))
      (mapv (fn [artifact]
              {:id (or (:evidence-id artifact) (:id artifact))
               :artifact (:artifact artifact)
               :kind (some-> (:kind artifact) evidence/normalize-evidence-kind)
               :role (:role artifact)
               :subject (:subject artifact)
               :status (some-> (:evidence-status artifact) str)
               :execution-status (some-> (:execution-status artifact) str)
               :finished-at (:finished-at artifact)
               :command (:command artifact)})))))

(defn- status-summary
  [status-result]
  (let [required-obligations (vec (concat (:failed-obligations status-result)
                                    (:open-obligations status-result)))
        recommended-obligations (vec (:recommended-obligations status-result))
        receipts (:evidence-receipts status-result)]
    {:project (:project status-result)
     :status (:status status-result)
     :issue-count (:issue-count status-result)
     :required-obligation-count (count required-obligations)
     :recommended-obligation-count (count recommended-obligations)
     :receipt-count (count receipts)
     :completed-receipt-count (count (filter #(= "passed" (:status %)) receipts))
     :changed-files (:changed-files status-result)
     :next-action (:next-action status-result)}))

(defn- derived-next-action
  [status-result]
  (or (:next-action status-result)
    (some-> (first (:required-obligations status-result))
      :command)))

(defn- profile-discovery
  [root opts]
  (let [explicit-path (:profile opts)
        searched (mapv #(bio/resolve-path root %) default-profile-paths)
        discovered (or explicit-path
                     (some (fn [path]
                             (let [resolved (bio/resolve-path root path)]
                               (when (bio/exists? resolved)
                                 resolved)))
                       default-profile-paths))]
    {:workspace-root root
     :configured? (boolean discovered)
     :profile-path discovered
     :searched-paths searched
     :explicit-profile? (boolean explicit-path)}))

(defn- no-profile-result
  [{:keys [workspace-root searched-paths explicit-profile? profile-path]}]
  {:configured? false
   :workspace-root workspace-root
   :profile-path profile-path
   :searched-paths searched-paths
   :next-step {:kind :extension-op
               :op (tool-call "br/init" [])}
   :message (if explicit-profile?
              "Bridge profile path was provided but no profile was found there."
              "No Bridge profile is configured for this workspace.")})

(defn- no-profile-error
  [{:keys [workspace-root searched-paths explicit-profile? profile-path]}]
  {:message (if explicit-profile?
              "Bridge profile path was provided but no profile was found there."
              "No Bridge profile is configured for this workspace.")
   :hint (str "Initialize Bridge with bare `br_init()`, or pass `{\"profile\": \"/abs/path/to/.bridge/profile.edn\"}`. "
           "Workspace root: " workspace-root)
   :details {:profile-path profile-path
             :searched-paths searched-paths}})

(defn- bridge-path-sandbox-only-policy-error?
  [t]
  (let [errors (get-in (ex-data t) [:validation :errors])]
    (and (seq errors)
      (every? (fn [err]
                (and (= :unknown-field (:type err))
                  (= [:bridge-path-sandbox] (:path err))))
        errors))))

(defn- load-policy
  "Load Bridge policy while tolerating Vis-owned `:bridge-path-sandbox`.
   Upstream Bridge validates verification policy strictly and does not know
   this extension field yet; other validation failures still propagate."
  [policy-path]
  (try
    (policy/load-policy policy-path)
    (catch clojure.lang.ExceptionInfo t
      (if (bridge-path-sandbox-only-policy-error? t)
        (bio/read-data policy-path)
        (throw t)))))

(defn- load-profile+policy
  [env opts]
  (let [discovery (profile-discovery (workspace-root env) opts)]
    (when-not (:configured? discovery)
      (throw (ex-info "Bridge profile not configured."
               {:type :vis.bridge/profile-not-found
                :bridge/discovery discovery})))
    (let [profile-path* (:profile-path discovery)
          profile (br-profile/load-profile profile-path*)
          policy-path (or (:policy opts)
                        (:verification-policy-path profile)
                        (let [default-path (bio/resolve-path (:root-path profile)
                                             ".bridge/verification-policy.yaml")]
                          (when (bio/exists? default-path)
                            default-path)))
          policy (when (and policy-path (bio/exists? policy-path))
                   (load-policy policy-path))]
      {:profile profile
       :policy policy
       :profile-path profile-path*
       :policy-path policy-path
       :discovery discovery})))

(defn- normalize-path-fragment
  [path]
  (-> (str path)
    (str/replace (str (char 92)) "/")
    (str/replace #"^\./+" "")))

(defn- clean-path-prefix
  [path]
  (-> (normalize-path-fragment path)
    (str/replace #"^/+" "")
    (str/replace #"/+$" "")))

(defn- relative-to-workspace
  [workspace-root* path]
  (try
    (let [rel (normalize-path-fragment
                (bio/relativize-path workspace-root* path))]
      (when-not (or (= ".." rel)
                  (str/starts-with? rel "../"))
        rel))
    (catch Throwable _
      nil)))

(defn- prefixed-glob
  [prefix pattern]
  (let [prefix* (clean-path-prefix prefix)
        pattern* (-> (normalize-path-fragment pattern)
                   (str/replace #"^/+" ""))]
    (cond
      (str/blank? pattern*) nil
      (or (str/blank? prefix*) (= "." prefix*)) pattern*
      :else (str prefix* "/" pattern*))))

(defn- directory-glob
  [glob]
  (when glob
    (if (str/ends-with? glob "/")
      (str glob "**")
      glob)))

(defn- policy-pattern->workspace-glob
  [env profile pattern]
  (let [workspace-root* (workspace-root env)
        pattern* (normalize-path-fragment pattern)
        file (java.io.File. pattern*)]
    (directory-glob
      (if (.isAbsolute file)
        (relative-to-workspace workspace-root* pattern*)
        (let [profile-prefix (relative-to-workspace workspace-root*
                               (:root-path profile))]
          (when profile-prefix
            (prefixed-glob profile-prefix pattern*)))))))

(defn- protected-access
  [access]
  (case (cond
          (keyword? access) (name access)
          (some? access) (str access)
          :else nil)
    "read-only" :read-only
    "read-write" :read-write
    "none" :none
    (throw (ex-info "Invalid Bridge path sandbox access."
             {:type :vis.bridge/invalid-path-sandbox-access
              :access access}))))

(def ^:private protected-path-hint
  "Bridge policy protects this path; use the br/* tool surface instead of direct file IO.")

(defn- protected-path-hint-for-rule
  [rule]
  (let [reason (:reason rule)]
    (if (and (string? reason) (not (str/blank? reason)))
      reason
      protected-path-hint)))

(defn- bridge-sandbox-rule->protected-path
  [env profile sandbox rule]
  (when-let [glob (policy-pattern->workspace-glob env profile (:path-pattern rule))]
    {:glob glob
     :access (protected-access (or (:access rule) (:default-access sandbox)))
     :hint (protected-path-hint-for-rule rule)}))

(defn- bridge-protected-paths
  [env]
  (let [discovery (profile-discovery (workspace-root env) {})]
    (if-not (:configured? discovery)
      []
      (let [{:keys [profile policy]} (load-profile+policy env {})
            sandbox (:bridge-path-sandbox policy)]
        (if (and sandbox (:enforce? sandbox))
          (mapv identity
            (keep #(bridge-sandbox-rule->protected-path env profile sandbox %)
              (:rules sandbox)))
          [])))))

(defn- selected-opts
  [opts]
  (select-keys opts [:profile :policy :changed_files :subject :out_dir
                     :out :timeout_seconds :is_dry_run]))

(defn- tool-success
  [op started-at-ms result opts]
  (let [finished-at-ms (now-ms)]
    (extension/success
      {:op op
       :result result
       :metadata {:started-at-ms started-at-ms
                  :finished-at-ms finished-at-ms
                  :duration-ms (- finished-at-ms started-at-ms)
                  :opts (selected-opts opts)}})))

(defn- tool-failure
  [op started-at-ms {:keys [throwable error]} opts]
  (let [finished-at-ms (now-ms)]
    (extension/failure
      {:op op
       :result nil
       :throwable throwable
       :error error
       :metadata {:started-at-ms started-at-ms
                  :finished-at-ms finished-at-ms
                  :duration-ms (- finished-at-ms started-at-ms)
                  :opts (selected-opts opts)}})))

(defn- bridge-tool
  [op _env opts f]
  (let [started-at-ms (now-ms)
        opts* (normalize-opts opts)]
    (try
      (tool-success op started-at-ms (f opts*) opts*)
      (catch Throwable t
        (if-let [discovery (:bridge/discovery (ex-data t))]
          (tool-failure op started-at-ms {:error (no-profile-error discovery)} opts*)
          (tool-failure op started-at-ms {:throwable t} opts*))))))

(defn- bridge-check
  [env opts]
  (bridge-tool
    :br/check
    env
    opts
    (fn [opts]
      (let [{:keys [profile policy profile-path policy-path]} (load-profile+policy env opts)
            status (br-next/build-status profile {:changed-files (ensure-vector (:changed_files opts))
                                                  :policy policy})
            required-obligations (mapv obligation->flat
                                   (concat (:failed-obligations status)
                                     (:open-obligations status)))
            recommended-obligations (mapv obligation->flat (:recommended-obligations status))
            receipts (evidence-receipts status)
            with-flat (assoc status
                        :configured? true
                        :profile-path profile-path
                        :policy-path policy-path
                        :required-obligations required-obligations
                        :recommended-obligations recommended-obligations
                        :evidence-receipts receipts)
            flattened (assoc with-flat :next-action (derived-next-action with-flat))]
        (assoc flattened :status-summary (status-summary (assoc flattened :next-action (:next-action flattened))))))))

(defn- next-suggestion
  [action]
  (let [op-name (:op action)
        evidence-id (or (:evidence-id action)
                      (get-in action [:args :id]))]
    (cond
      (or (= "run-evidence" (:kind action))
        (= "bridge/run-evidence" op-name))
      (action->extension-op (assoc action :evidence-id evidence-id))

      :else nil)))

(defn- status-obligation->suggestion
  [obligation]
  (or (action->extension-op (:command obligation))
    (next-suggestion obligation)))

(defn- unconfigured-next-step
  []
  {:kind :extension-op
   :op (tool-call "br/init" [])})

(defn- next-result
  [status-result]
  (let [actions (keep status-obligation->suggestion
                  (or (:required-obligations status-result)
                    #_{:clj-kondo/ignore [:unresolved-var]}
                    (br-next/planned-actions status-result)))]
    {:configured? true
     :status (:status status-result)
     :issue-count (:issue-count status-result)
     :profile-path (:profile-path status-result)
     :changed-files (:changed-files status-result)
     :status-summary (status-summary status-result)
     :summary (select-keys status-result [:project :status :issue-count
                                          :open-obligations :failed-obligations
                                          :recommended-obligations :stale-artifacts
                                          :subject-problems :completed-obligations
                                          :completed-evidence :required-obligations
                                          :evidence-receipts :next-action
                                          :status-summary])
     :next-step (action->extension-op (:next-action status-result))
     :suggestions (vec actions)}))

(defn- bridge-hint
  [{:keys [environment]}]
  (let [env (or environment {})
        discovery (profile-discovery (workspace-root env) {})]
    (if-not (:configured? discovery)
      {:importance   :warn
       :title (str "Bridge is not configured for this workspace. "
                "Initialize it via bare " (render-tool-call (:op (unconfigured-next-step)))
                " before asking for Bridge status or evidence work. "
                "Then `task_set(\"vis.foundation/bridge\", {\"status\": \"done\"})`.")}
      (let [status-result (:result (bridge-check env {}))]
        (when (and status-result
                (pos? (long (or (:issue-count status-result) 0))))
          {:importance   :info
           :title (str "Bridge reports open verification work in this workspace. "
                    "Inspect the next suggested Bridge action via bare "
                    (render-tool-call (tool-call "br/next" []))
                    ". Do not execute evidence work from this hint unless verification is already in scope for the current task. "
                    "Then `task_set(\"vis.foundation/bridge\", {\"status\": \"done\"})`.")})))))

(defn init
  "Initialize Bridge in the current workspace. Optional opts: {:root path}. Returns existing configuration when Bridge is already set up."
  [env & [opts]]
  (bridge-tool
    :br/init
    env
    opts
    (fn [opts]
      (let [root (or (:root opts) (workspace-root env))
            discovery (profile-discovery root opts)]
        (if (:configured? discovery)
          {:configured? true
           :already-configured? true
           :created? false
           :workspace-root root
           :profile-path (:profile-path discovery)
           :created []
           :updated []
           :message "Bridge is already configured for this workspace."}
          (let [result (br-cli/command-init {:root root})
                refreshed (profile-discovery root opts)]
            {:configured? true
             :already-configured? false
             :created? true
             :workspace-root root
             :profile-path (:profile-path refreshed)
             :created (:created result)
             :updated (:updated result)
             :next-step {:kind :extension-op
                         :op (tool-call "br/check" [])}}))))))

(defn profile
  "Return the active Bridge project profile summary. Optional opts: {:profile path :policy path}."
  [env & [opts]]
  (bridge-tool
    :br/profile
    env
    opts
    (fn [opts]
      (let [discovery (profile-discovery (workspace-root env) opts)]
        (if-not (:configured? discovery)
          (no-profile-result discovery)
          (let [{:keys [profile policy profile-path policy-path]} (load-profile+policy env opts)]
            {:configured? true
             :summary (br-profile/profile-summary profile)
             :profile-path profile-path
             :policy-path policy-path
             :policy-loaded? (boolean policy)}))))))

(defn check
  "Run Bridge check for the workspace. Optional opts: {:profile path :policy path :changed_files [path ...]}."
  [env & [opts]]
  (let [opts* (normalize-opts opts)
        discovery (profile-discovery (workspace-root env) opts*)]
    (if-not (:configured? discovery)
      (tool-success :br/check (now-ms)
        (assoc (no-profile-result discovery)
          :status "unconfigured"
          :issue-count 1
          :next-step (unconfigured-next-step)
          :changed-files (ensure-vector (:changed_files opts*)))
        opts*)
      (bridge-check env opts*))))

(defn next
  "Return the next suggested Bridge action as Vis extension operations. Optional opts: {:profile path :policy path :changed_files [path ...]}."
  [env & [opts]]
  (let [opts* (normalize-opts opts)
        discovery (profile-discovery (workspace-root env) opts*)]
    (if-not (:configured? discovery)
      (tool-success :br/next (now-ms)
        (assoc (no-profile-result discovery)
          :status "unconfigured"
          :issue-count 1
          :next-step (unconfigured-next-step)
          :suggestions [(unconfigured-next-step)]
          :changed-files (ensure-vector (:changed_files opts*)))
        opts*)
      (tool-success :br/next (now-ms)
        (next-result (:result (bridge-check env opts*)))
        opts*))))

(defn list-evidence
  "List Bridge evidence commands configured by the active profile. Optional opts: {:profile path :policy path}."
  [env & [opts]]
  (bridge-tool
    :br/list-evidence
    env
    opts
    (fn [opts]
      (let [discovery (profile-discovery (workspace-root env) opts)]
        (if-not (:configured? discovery)
          (assoc (no-profile-result discovery) :commands [])
          (let [{:keys [profile profile-path]} (load-profile+policy env opts)]
            {:configured? true
             :profile-path profile-path
             :commands (evidence/list-commands profile)}))))))

(defn run-evidence
  "Run one configured Bridge evidence command and write its receipt. Args: evidence id string, optional opts {:profile path :subject s :out path :out_dir path :timeout_seconds n :is_dry_run true}."
  [env id & [opts]]
  (bridge-tool
    :br/run-evidence
    env
    opts
    (fn [opts]
      (let [discovery (profile-discovery (workspace-root env) opts)]
        (when-not (:configured? discovery)
          (throw (ex-info "Bridge profile not configured."
                   {:type :vis.bridge/profile-not-found
                    :bridge/discovery discovery})))
        (let [{:keys [profile profile-path]} (load-profile+policy env opts)]
          {:profile-path profile-path
           :result (evidence/run-command profile
                     (str id)
                     {:out-dir (:out_dir opts)
                      :out-path (:out opts)
                      :subject (:subject opts)
                      :timeout-seconds (:timeout_seconds opts)
                      :dry-run? (boolean (:is_dry_run opts))})})))))

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

;; Each `:render-fn` is a structured IR builder over the raw
;; `:result` map (see `render.clj`). The MODEL surface is the
;; unwrapped SCI return value — these renderers shape ONLY the
;; channel/TUI preview, never what the LLM reads.

(def bridge-symbols
  [(vis/symbol #'init {:before-fn inject-env
                       :tag :mutation
                       :render-fn br-render/render-init
                       :arglists '([] [opts])})
   (vis/symbol #'profile {:before-fn inject-env
                          :tag :observation
                          :render-fn br-render/render-profile
                          :arglists '([] [opts])})
   (vis/symbol #'check {:before-fn inject-env
                        :tag :observation
                        :render-fn br-render/render-check
                        :arglists '([] [opts])})
   (vis/symbol #'next {:before-fn inject-env
                       :tag :observation
                       :render-fn br-render/render-next
                       :arglists '([] [opts])})
   (vis/symbol #'list-evidence {:before-fn inject-env
                                :tag :observation
                                :render-fn br-render/render-list-evidence
                                :arglists '([] [opts])})
   (vis/symbol #'run-evidence {:before-fn inject-env
                               :tag :mutation
                               :render-fn br-render/render-run-evidence
                               :arglists '([id] [id opts])})])

(def bridge-prompt
  (str/join
    " "
    ["`br_*` Bridge verification tools:"
     "use `br_init()` to bootstrap Bridge in a new repo,"
     "use `br_check()` first when asked for Bridge status,"
     "use `br_next()` to inspect the immediate next action,"
     "use `br_list_evidence()` to inspect configured evidence commands,"
     "and use `br_run_evidence(id, opts?)` only when the configured command should actually run."
     "`br_run_evidence(id, {\"is_dry_run\": True})` previews the execution plan without writing a receipt."
     "When answering status questions, summarize the returned map instead of pasting it raw."
     "Prefer `status_summary`, `required_obligations`, `evidence_receipts`, and `next_action` when they are present."
     "Call out `status`, `issue_count`, open or failed obligations, and any evidence receipts that are already present."
     "Keep policy obligations and runnable evidence ids distinct: for example `unit-tests` is not the same thing as the runnable `unit` command."
     "Prefer the `br_next` suggestions over shell commands because they stay inside the Vis tool surface."]))

(def bridge-hooks
  [{:id :vis.bridge/next
    :doc "Hint the model about the next Bridge action when the workspace is unconfigured or Bridge has open evidence work."
    :phase :turn.iteration/start
    :fn bridge-hint}])

;; Tags carried INLINE on each `vis/symbol` opts map above;
;; register-extension! auto-populates the op registry.

;; =============================================================================
;; CLI surface -- `vis ext bridge <subcommand>`
;;
;; Mirrors the SCI alias (`br/init`, `br/check`, ...) so the binary
;; reflects the same operations the model sees inside iterations.
;; Every subcommand thin-wraps the matching tool fn with an empty
;; env (workspace-root defaults to `user.dir`), prints the resulting
;; map as EDN, and exits non-zero on tool failure or open Bridge
;; obligations.
;; =============================================================================

(defn- println-original!
  [s]
  (.println ^java.io.PrintStream System/out (str s)))

(defn- pprint-edn
  [v]
  (with-out-str (pprint/pprint v)))

(defn- cli-result-status
  "Translate a `bridge-tool` result map into a process exit code.
   `extension/failure` payloads expose `:status :error`; success
   payloads carry the underlying tool result under `:result`. We
   exit non-zero on failure or when the underlying tool reports any
   open Bridge issues."
  [result]
  (let [tool-result (:result result)]
    (cond
      (= :error (:status result))                       1
      (= "unconfigured" (:status tool-result))          1
      (pos? (long (or (:issue-count tool-result) 0)))   1
      :else                                             0)))

(defn- emit-result!
  "Print the tool result (EDN) and exit with the derived status."
  [result]
  (println-original! (pprint-edn result))
  (System/exit (cli-result-status result)))

(defn- parse-kv-opts
  "Parse a residual arg vector into a Bridge opts map. Supported
   flags: `--profile PATH`, `--policy PATH`, `--changed-file PATH`
   (repeatable), `--subject S`, `--out PATH`, `--out-dir PATH`,
   `--timeout-seconds N`, `--dry-run`. Unknown flags raise so the
   user sees a structural error instead of a silent drop."
  [residual]
  (loop [xs (vec residual)
         opts {}]
    (let [[head & tail] xs]
      (cond
        (nil? head) opts

        (= "--dry-run" head)
        (recur (vec tail) (assoc opts :is_dry_run true))

        (#{"--profile" "--policy" "--subject" "--out" "--out-dir"} head)
        (let [k (case head
                  "--profile" :profile
                  "--policy"  :policy
                  "--subject" :subject
                  "--out"     :out
                  "--out-dir" :out_dir)]
          (recur (vec (rest tail)) (assoc opts k (first tail))))

        (= "--changed-file" head)
        (recur (vec (rest tail))
          (update opts :changed_files (fnil conj []) (first tail)))

        (= "--timeout-seconds" head)
        (recur (vec (rest tail))
          (assoc opts :timeout_seconds (parse-long (str (first tail)))))

        (str/starts-with? (str head) "--")
        (throw (ex-info (str "Unknown bridge flag: " head)
                 {:flag head}))

        :else
        (throw (ex-info (str "Unexpected positional argument: " head)
                 {:arg head}))))))

(defn- cli-env [] {})

(defn- cli-init!
  [_parsed residual]
  (let [opts (parse-kv-opts residual)]
    (emit-result! (init (cli-env) opts))))

(defn- cli-profile!
  [_parsed residual]
  (emit-result! (profile (cli-env) (parse-kv-opts residual))))

(defn- cli-check!
  [_parsed residual]
  (emit-result! (check (cli-env) (parse-kv-opts residual))))

(defn- cli-next!
  [_parsed residual]
  (emit-result! (next (cli-env) (parse-kv-opts residual))))

(defn- cli-list-evidence!
  [_parsed residual]
  (emit-result! (list-evidence (cli-env) (parse-kv-opts residual))))

(defn- cli-run-evidence!
  [_parsed residual]
  (let [[id & rest-args] (vec residual)]
    (when (str/blank? (str id))
      (println-original! "Usage: vis ext bridge run-evidence <id> [--dry-run] [...flags]")
      (System/exit 1))
    (emit-result! (run-evidence (cli-env) id (parse-kv-opts rest-args)))))

(def ^:private bridge-cli
  [{:cmd/name      "bridge"
    :cmd/doc       "Bridge verification coordinator -- mirrors the `br/` SCI alias."
    :cmd/usage     "vis ext bridge <init|profile|check|next|list-evidence|run-evidence> [flags]"
    :cmd/subcommands
    [{:cmd/name  "init"
      :cmd/doc   "Bootstrap Bridge for this workspace (.bridge/profile.edn etc)."
      :cmd/usage "vis ext bridge init"
      :cmd/run-fn cli-init!}
     {:cmd/name  "profile"
      :cmd/doc   "Print the active Bridge profile summary."
      :cmd/usage "vis ext bridge profile [--profile PATH] [--policy PATH]"
      :cmd/run-fn cli-profile!}
     {:cmd/name  "check"
      :cmd/doc   "Run Bridge check for the workspace; exits non-zero on open obligations."
      :cmd/usage "vis ext bridge check [--changed-file PATH ...] [--profile PATH] [--policy PATH]"
      :cmd/run-fn cli-check!}
     {:cmd/name  "next"
      :cmd/doc   "Print the next suggested Bridge action(s)."
      :cmd/usage "vis ext bridge next [--changed-file PATH ...] [--profile PATH]"
      :cmd/run-fn cli-next!}
     {:cmd/name  "list-evidence"
      :cmd/doc   "List evidence commands configured by the active profile."
      :cmd/usage "vis ext bridge list-evidence [--profile PATH]"
      :cmd/run-fn cli-list-evidence!}
     {:cmd/name  "run-evidence"
      :cmd/doc   "Run a configured evidence command and write its receipt."
      :cmd/usage "vis ext bridge run-evidence <id> [--dry-run] [--subject S] [--out PATH] [--out-dir PATH] [--timeout-seconds N] [--profile PATH]"
      :cmd/examples ["vis ext bridge run-evidence unit --dry-run"
                     "vis ext bridge run-evidence unit --timeout-seconds 300"]
      :cmd/run-fn cli-run-evidence!}]}])

(def vis-extension
  (vis/extension
    {:ext/name "foundation-bridge"
     :ext/description "Bridge verification coordinator tools under `br/`."
     :ext/version "0.1.0"
     :ext/author "enajski"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/sci {:ext.sci/alias 'br
               :ext.sci/symbols bridge-symbols}
     :ext/cli bridge-cli
     :ext/hooks bridge-hooks
     :ext/kind "verification"
     :ext/protected-paths bridge-protected-paths
     :ext/prompt bridge-prompt}))

(vis/register-extension! vis-extension)
