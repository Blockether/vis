(ns vis.ext.bridge
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

(defn- tool-call
  [tool args]
  {:tool tool
   :args (vec args)
   :call (str "(" tool
           (when (seq args)
             (str " " (pr-str (first args))
               (apply str (map #(str " " (pr-str %)) (rest args)))))
           ")")})

(defn- render-tool-call
  [{:keys [call]}]
  (or call "(br/init)"))

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
   :hint (str "Initialize Bridge with bare `(br/init)`, or pass `{:profile \"/abs/path/to/.bridge/profile.edn\"}`. "
           "Workspace root: " workspace-root)
   :details {:profile-path profile-path
             :searched-paths searched-paths}})

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
                   (policy/load-policy policy-path))]
      {:profile profile
       :policy policy
       :profile-path profile-path*
       :policy-path policy-path
       :discovery discovery})))

(defn- selected-opts
  [opts]
  (select-keys opts [:profile :policy :changed-files :subject :out-dir
                     :out :timeout-seconds :dry-run?]))

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
  [op env opts f]
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
            status (br-next/build-status profile {:changed-files (ensure-vector (:changed-files opts))
                                                  :policy policy})]
        (let [required-obligations (mapv obligation->flat
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
              flattened (assoc status
                          :configured? true
                          :profile-path profile-path
                          :policy-path policy-path
                          :required-obligations required-obligations
                          :recommended-obligations recommended-obligations
                          :evidence-receipts receipts
                          :next-action (derived-next-action with-flat))]
          (assoc flattened :status-summary (status-summary (assoc flattened :next-action (:next-action flattened)))))))))

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
      {:importance :high
       :text (str "Bridge is not configured for this workspace. "
               "Initialize it via bare " (render-tool-call (:op (unconfigured-next-step)))
               " before asking for Bridge status or evidence work.")}
      (let [status-result (:result (bridge-check env {}))]
        (when (and status-result
                (pos? (long (or (:issue-count status-result) 0))))
          {:importance :low
           :text (str "Bridge reports open verification work in this workspace. "
                   "Inspect the next suggested Bridge action via bare "
                   (render-tool-call (tool-call "br/next" []))
                   ". Do not execute evidence work from this hint unless verification is already in scope for the current task.")})))))

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
  "Run Bridge check for the workspace. Optional opts: {:profile path :policy path :changed-files [path ...]}."
  [env & [opts]]
  (let [opts* (normalize-opts opts)
        discovery (profile-discovery (workspace-root env) opts*)]
    (if-not (:configured? discovery)
      (tool-success :br/check (now-ms)
        (assoc (no-profile-result discovery)
          :status "unconfigured"
          :issue-count 1
          :next-step (unconfigured-next-step)
          :changed-files (ensure-vector (:changed-files opts*)))
        opts*)
      (bridge-check env opts*))))

(defn next
  "Return the next suggested Bridge action as Vis extension operations. Optional opts: {:profile path :policy path :changed-files [path ...]}."
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
          :changed-files (ensure-vector (:changed-files opts*)))
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
  "Run one configured Bridge evidence command and write its receipt. Args: evidence id string, optional opts {:profile path :subject s :out path :out-dir path :timeout-seconds n :dry-run? true}."
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
                     {:out-dir (:out-dir opts)
                      :out-path (:out opts)
                      :subject (:subject opts)
                      :timeout-seconds (:timeout-seconds opts)
                      :dry-run? (boolean (:dry-run? opts))})})))))

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

(def bridge-symbols
  [(vis/symbol #'init {:before-fn inject-env
                       :render-fn vis/render-string
                       :arglists '([] [opts])})
   (vis/symbol #'profile {:before-fn inject-env
                          :render-fn vis/render-string
                          :arglists '([] [opts])})
   (vis/symbol #'check {:before-fn inject-env
                        :render-fn vis/render-string
                        :arglists '([] [opts])})
   (vis/symbol #'next {:before-fn inject-env
                       :render-fn vis/render-string
                       :arglists '([] [opts])})
   (vis/symbol #'list-evidence {:before-fn inject-env
                                :render-fn vis/render-string
                                :arglists '([] [opts])})
   (vis/symbol #'run-evidence {:before-fn inject-env
                               :render-fn vis/render-string
                               :arglists '([id] [id opts])})])

(def bridge-prompt
  (str/join
    " "
    ["`br/` Bridge verification tools:"
     "use `(br/init)` to bootstrap Bridge in a new repo,"
     "use `(br/check)` first when asked for Bridge status,"
     "use `(br/next)` to inspect the immediate next action,"
     "use `(br/list-evidence)` to inspect configured evidence commands,"
     "and use `(br/run-evidence id opts?)` only when the configured command should actually run."
     "`(br/run-evidence id {:dry-run? true})` previews the execution plan without writing a receipt."
     "When answering status questions, summarize the returned map instead of pasting it raw."
     "Prefer `:status-summary`, `:required-obligations`, `:evidence-receipts`, and `:next-action` when they are present."
     "Call out `:status`, `:issue-count`, open or failed obligations, and any evidence receipts that are already present."
     "Keep policy obligations and runnable evidence ids distinct: for example `unit-tests` is not the same thing as the runnable `unit` command."
     "Prefer the `br/next` suggestions over shell commands because they stay inside the Vis tool surface."]))

(def bridge-hooks
  [{:id :vis.bridge/next
    :doc "Hint the model about the next Bridge action when the workspace is unconfigured or Bridge has open evidence work."
    :phase :turn.iteration/start
    :fn bridge-hint}])

(doseq [[op tag] [[:br/init :op.tag/mutation]
                  [:br/profile :op.tag/observation]
                  [:br/check :op.tag/observation]
                  [:br/next :op.tag/observation]
                  [:br/list-evidence :op.tag/observation]
                  [:br/run-evidence :op.tag/mutation]]]
  (vis/register-op! op {:tag tag}))

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
        (recur (vec tail) (assoc opts :dry-run? true))

        (#{"--profile" "--policy" "--subject" "--out" "--out-dir"} head)
        (let [k (case head
                  "--profile" :profile
                  "--policy"  :policy
                  "--subject" :subject
                  "--out"     :out
                  "--out-dir" :out-dir)]
          (recur (vec (rest tail)) (assoc opts k (first tail))))

        (= "--changed-file" head)
        (recur (vec (rest tail))
          (update opts :changed-files (fnil conj []) (first tail)))

        (= "--timeout-seconds" head)
        (recur (vec (rest tail))
          (assoc opts :timeout-seconds (parse-long (str (first tail)))))

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
    {:ext/name "bridge"
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
     :ext/prompt bridge-prompt}))

(vis/register-extension! vis-extension)
