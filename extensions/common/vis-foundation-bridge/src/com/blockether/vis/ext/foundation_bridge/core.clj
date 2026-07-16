(ns com.blockether.vis.ext.foundation-bridge.core
  "Bridge verification tools for Vis.

   Consumes Bridge exclusively through its public library API
   (`bridge.api`); `br/check` returns Bridge's canonical status summary
   (`:summary-version` 1) plus the Vis envelope keys. This extension
   adds no flattening of its own — meaning lives in the kernel."
  (:refer-clojure :exclude [next])
  (:require [bridge.api :as br]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]))

(def ^:private default-profile-paths [".bridge/profile.edn" ".bridge/persistent/profile.edn"])

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- workspace-root [env] (or (:workspace/root env) (System/getProperty "user.dir")))

(defn- kw->snake
  "Keyword -> snake_case string, mirroring the Clojure->Python boundary
   (`env-python/kw->snake`): kebab -> snake, trailing `?`/`!` stripped,
   namespace folded with `_`."
  ^String [k]
  (-> (if (namespace k) (str (namespace k) "_" (name k)) (name k))
      (str/replace "-" "_")
      (str/replace #"[?!]$" "")))

(defn- boundary-key
  [k]
  (cond (string? k) k
        (keyword? k) (kw->snake k)
        (symbol? k) (kw->snake (keyword k))
        :else (str k)))

(defn- deep-stringify
  "Recursively rebuild a tool-result value into the STRINGS-ONLY shape the
   Clojure->Python boundary requires: map KEYS and keyword/symbol VALUES become
   snake_case strings (mirroring env-python/kw->snake), collections recurse.
   Bridge merges bridge.api's keyword-keyed status summary verbatim into its
   results, so this is the single source-stringification pass applied at each
   public tool-fn exit — internal builders (`bridge-check`, `next-result`, the
   `profile-discovery`/protected-path structures) stay idiomatic keyword Clojure
   because `next`/`bridge-hint`/the sandbox layer read them back by keyword."
  [x]
  (cond (map? x) (reduce-kv (fn [m k v]
                              (assoc m (boundary-key k) (deep-stringify v)))
                            {}
                            x)
        (or (vector? x) (seq? x) (set? x)) (mapv deep-stringify x)
        (keyword? x) (kw->snake x)
        (symbol? x) (kw->snake (keyword x))
        :else x))

(defn- stringify-result
  "Apply `deep-stringify` to a tool envelope's `:result` payload — the only part
   that crosses to Python. Envelope keys, `:metadata`, and `:error` stay
   internal keyword-keyed."
  [envelope]
  (if (map? (:result envelope)) (update envelope :result deep-stringify) envelope))

(defn- normalize-opts
  [opts]
  (cond (nil? opts) {}
        (map? opts) opts
        :else (throw (ex-info "Bridge opts must be a map."
                              {:type :vis.bridge/invalid-opts :opts opts}))))

(defn- ensure-vector
  [x]
  (cond (nil? x) []
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
  (cond (nil? x) "None"
        (string? x) (str "\"" x "\"")
        (boolean? x) (if x "True" "False")
        :else (str x)))

(defn- tool-call
  [tool args]
  {:tool tool
   :args (vec args)
   :call (str (py-tool-name tool) "(" (str/join ", " (map py-arg args)) ")")})

(defn- render-tool-call [{:keys [call]}] (or call "br_init()"))

(defn- action->extension-op
  [action]
  (when action
    {:kind :extension-op
     :summary (:summary action)
     :reason (:reason action)
     :required-evidence (vec (:required-evidence action))
     :op (tool-call "br/run-evidence" [(:evidence-id action)])}))

(defn- profile-discovery
  [root opts]
  (let [explicit-path
        (get opts "profile")

        searched
        (mapv #(br/resolve-path root %) default-profile-paths)

        discovered
        (or explicit-path
            (some (fn [path]
                    (let [resolved (br/resolve-path root path)]
                      (when (br/exists? resolved) resolved)))
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
   :next-step {:kind :extension-op :op (tool-call "br/init" [])}
   :message (if explicit-profile?
              "Bridge profile path was provided but no profile was found there."
              "No Bridge profile is configured for this workspace.")})

(defn- no-profile-error
  [{:keys [workspace-root searched-paths explicit-profile? profile-path]}]
  {:message (if explicit-profile?
              "Bridge profile path was provided but no profile was found there."
              "No Bridge profile is configured for this workspace.")
   :hint
   (str
     "Initialize Bridge with bare `br_init()`, or pass `{\"profile\": \"/abs/path/to/.bridge/profile.edn\"}`. "
     "Workspace root: "
     workspace-root)
   :details {:profile-path profile-path :searched-paths searched-paths}})

(defn- load-profile+policy
  [env opts]
  (let [discovery (profile-discovery (workspace-root env) opts)]
    (when-not (:configured? discovery)
      (throw (ex-info "Bridge profile not configured."
                      {:type :vis.bridge/profile-not-found :bridge/discovery discovery})))
    (let [profile-path* (:profile-path discovery)
          profile (br/load-profile profile-path*)
          policy-path (or (get opts "policy")
                          (:verification-policy-path profile)
                          (let [default-path (br/resolve-path (:root-path profile)
                                                              ".bridge/verification-policy.yaml")]
                            (when (br/exists? default-path) default-path)))
          policy (when (and policy-path (br/exists? policy-path)) (br/load-policy policy-path))]

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
  (try (let [rel (normalize-path-fragment (br/relativize-path workspace-root* path))]
         (when-not (or (= ".." rel) (str/starts-with? rel "../")) rel))
       (catch Throwable _ nil)))

(defn- prefixed-glob
  [prefix pattern]
  (let [prefix*
        (clean-path-prefix prefix)

        pattern*
        (-> (normalize-path-fragment pattern)
            (str/replace #"^/+" ""))]

    (cond (str/blank? pattern*) nil
          (or (str/blank? prefix*) (= "." prefix*)) pattern*
          :else (str prefix* "/" pattern*))))

(defn- directory-glob [glob] (when glob (if (str/ends-with? glob "/") (str glob "**") glob)))

(defn- policy-pattern->workspace-glob
  [env profile pattern]
  (let [workspace-root*
        (workspace-root env)

        ^String pattern*
        (normalize-path-fragment pattern)

        file
        (java.io.File. pattern*)]

    (directory-glob (if (.isAbsolute file)
                      (relative-to-workspace workspace-root* pattern*)
                      (let [profile-prefix (relative-to-workspace workspace-root*
                                                                  (:root-path profile))]
                        (when profile-prefix (prefixed-glob profile-prefix pattern*)))))))

(defn- protected-access
  [access]
  (case (cond (keyword? access) (name access)
              (some? access) (str access)
              :else nil)
    "read-only"
    :read-only

    "read-write"
    :read-write

    "none"
    :none

    (throw (ex-info "Invalid Bridge path sandbox access."
                    {:type :vis.bridge/invalid-path-sandbox-access :access access}))))

(def ^:private protected-path-hint
  "Bridge policy protects this path; use the br/* tool surface instead of direct file IO.")

(defn- protected-path-hint-for-rule
  [rule]
  (let [reason (:reason rule)]
    (if (and (string? reason) (not (str/blank? reason))) reason protected-path-hint)))

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
  (select-keys opts
               ["profile" "policy" "changed_files" "subject" "out_dir" "out" "timeout_seconds"
                "is_dry_run"]))

(defn- tool-success
  [op started-at-ms result opts]
  (let [finished-at-ms (now-ms)]
    (extension/success {:op op
                        :result result
                        :metadata {:started-at-ms started-at-ms
                                   :finished-at-ms finished-at-ms
                                   :duration-ms (- finished-at-ms (long started-at-ms))
                                   :opts (selected-opts opts)}})))

(defn- tool-failure
  [op started-at-ms {:keys [throwable error]} opts]
  (let [finished-at-ms (now-ms)]
    (extension/failure {:op op
                        :result nil
                        :throwable throwable
                        :error error
                        :metadata {:started-at-ms started-at-ms
                                   :finished-at-ms finished-at-ms
                                   :duration-ms (- finished-at-ms (long started-at-ms))
                                   :opts (selected-opts opts)}})))

(defn- bridge-tool
  [op _env opts f]
  (let [started-at-ms
        (now-ms)

        opts*
        (normalize-opts opts)]

    (try (tool-success op started-at-ms (f opts*) opts*)
         (catch Throwable t
           (if-let [discovery (:bridge/discovery (ex-data t))]
             (tool-failure op started-at-ms {:error (no-profile-error discovery)} opts*)
             (tool-failure op started-at-ms {:throwable t} opts*))))))

(defn- bridge-check
  "Run Bridge's check and return its canonical status summary
   (`:summary-version` 1, see bridge.summary) wrapped with the Vis
   envelope keys (`:configured?`, `:profile-path`, `:policy-path`)."
  [env opts]
  (bridge-tool :br/check
               env
               opts
               (fn [opts]
                 (let [{:keys [profile policy profile-path policy-path]}
                       (load-profile+policy env opts)

                       summary
                       (br/check profile
                                 {:changed-files (ensure-vector (get opts "changed_files"))
                                  :policy policy})]

                   (assoc summary
                     :configured? true
                     :profile-path profile-path
                     :policy-path policy-path)))))

(defn- next-suggestion
  [action]
  (let [op-name
        (:op action)

        evidence-id
        (or (:evidence-id action) (get-in action [:args :id]))]

    (cond (or (= "run-evidence" (:kind action)) (= "bridge/run-evidence" op-name))
          (action->extension-op (assoc action :evidence-id evidence-id))
          :else nil)))

(defn- status-obligation->suggestion
  [obligation]
  (or (action->extension-op (:command obligation)) (next-suggestion obligation)))

(defn- unconfigured-next-step [] {:kind :extension-op :op (tool-call "br/init" [])})

(defn- next-result
  [check-result]
  (let [actions (keep status-obligation->suggestion (:required-obligations check-result))]
    {:configured? true
     :project (:project check-result)
     :status (:status check-result)
     :issue-count (:issue-count check-result)
     :profile-path (:profile-path check-result)
     :changed-files (:changed-files check-result)
     :counts (:counts check-result)
     :summary (select-keys check-result
                           [:summary-version :project :status :issue-count :counts
                            :required-obligations :recommended-obligations :stale-artifacts
                            :subject-problems :evidence-receipts :next-action])
     :next-step (action->extension-op (:next-action check-result))
     :suggestions (vec actions)}))

(def ^:private ^:const hint-recheck-ms
  "Re-run the reflective Bridge check at most this often per workspace even when
   `.bridge/` looks unchanged, so working-tree drift (stale artifacts vs edited
   sources) still refreshes the nudge without paying the check every iteration."
  30000)

(defonce ^:private hint-cache
  ;; workspace-root -> {:fingerprint fp :at ms :hint hint}. Memoizes the
  ;; iteration-start check: `bridge-check` reaches Bridge through UNHINTED Java
  ;; interop (a full reflective `Class/getMethods` + `Method[]` copy per call —
  ;; the single biggest allocation source during a turn per the gateway JFR), and
  ;; the hint reran it on EVERY agent iteration. Keyed on a `.bridge/` fingerprint
  ;; so a receipt / profile / policy write invalidates it immediately.
  (atom {}))

(defn- bridge-dir-fingerprint
  "Cheap change-detector for `.bridge/` inputs: `[max-mtime total-size file-count]`
   across the tree. Any profile / policy / receipt write shifts it, so the memoized
   check re-runs exactly then. nil when `.bridge/` is absent."
  [root]
  (try (let [dir (java.io.File. (str root) ".bridge")]
         (when (.isDirectory dir)
           (loop [stack [dir]
                  mx 0
                  sz 0
                  n 0]

             (if-let [^java.io.File f (peek stack)]
               (let [stack (pop stack)]
                 (if (.isDirectory f)
                   (recur (into stack (or (seq (.listFiles f)) [])) mx sz n)
                   (recur stack (max mx (.lastModified f)) (+ sz (.length f)) (inc n))))
               [mx sz n]))))
       (catch Throwable _ nil)))

(defn- compute-bridge-hint
  "The uncached hint body: run Bridge's check and, only when it reports open work,
   build the :info nudge. Split out so `bridge-hint` can memoize the reflective
   check around it."
  [env]
  (let [status-result (:result (bridge-check env {}))]
    (when (and status-result (pos? (long (or (:issue-count status-result) 0))))
      {:importance :info
       :title
       (str
         "Bridge reports open verification work in this workspace. "
         "Inspect the next suggested Bridge action via bare "
         (render-tool-call (tool-call "br/next" []))
         ". Do not execute evidence work from this hint unless verification is already in scope for the current task.")})))

(defn- bridge-hint
  "Iteration-start hint. Emits nothing when Bridge is not configured —
   a workspace without Bridge is the normal state, not actionable work
   (the static extension prompt already documents `br_init()` for when
   Bridge is in scope). Only configured workspaces with open
   verification work get an :info hint.

   The underlying `bridge-check` reaches Bridge through reflective Java interop,
   so its result is MEMOIZED per workspace (see `hint-cache`) and recomputed only
   when `.bridge/` changes (a receipt / profile / policy write shifts the
   fingerprint) or the cached value ages past `hint-recheck-ms` — rather than
   running a fresh reflective check on every single agent iteration."
  [{:keys [environment]}]
  (let [env
        (or environment {})

        root
        (workspace-root env)

        discovery
        (profile-discovery root {})]

    (when (:configured? discovery)
      (let [fp
            (bridge-dir-fingerprint root)

            now
            (now-ms)

            cached
            (get @hint-cache root)]

        (if (and cached
                 (= (:fingerprint cached) fp)
                 (< (- now (long (:at cached))) hint-recheck-ms))
          (:hint cached)
          (let [hint (compute-bridge-hint env)]
            (swap! hint-cache assoc root {:fingerprint fp :at now :hint hint})
            hint))))))

(defn init
  "Bootstrap Bridge in this workspace. `await br_init()` (opts `{\"root\": path}`). Returns the existing config if already set up."
  [env & [opts]]
  (stringify-result
    (bridge-tool :br/init
                 env
                 opts
                 (fn [opts]
                   (let [root
                         (or (get opts "root") (workspace-root env))

                         discovery
                         (profile-discovery root opts)]

                     (if (:configured? discovery)
                       {:configured? true
                        :already-configured? true
                        :workspace-root root
                        :profile-path (some-> (:profile-path discovery)
                                              normalize-path-fragment)
                        :created []
                        :updated []
                        :message "Bridge is already configured for this workspace."}
                       (let [result
                             (br/init! {:root root})

                             refreshed
                             (profile-discovery root opts)]

                         {:configured? true
                          :already-configured? false
                          :workspace-root root
                          :profile-path (some-> (:profile-path refreshed)
                                                normalize-path-fragment)
                          :created (mapv normalize-path-fragment (:created result))
                          :updated (:updated result)
                          :next-step {:kind :extension-op :op (tool-call "br/check" [])}})))))))

(defn profile
  "Active Bridge project profile summary. `await br_profile()` (opts `{\"profile\": path, \"policy\": path}`)."
  [env & [opts]]
  (stringify-result (bridge-tool :br/profile
                                 env
                                 opts
                                 (fn [opts]
                                   (let [discovery (profile-discovery (workspace-root env) opts)]
                                     (if-not (:configured? discovery)
                                       (no-profile-result discovery)
                                       (let [{:keys [profile policy profile-path policy-path]}
                                             (load-profile+policy env opts)]
                                         {:configured? true
                                          :summary (br/profile-summary profile)
                                          :profile-path profile-path
                                          :policy-path policy-path
                                          :policy-loaded? (boolean policy)})))))))

(defn check
  "Run Bridge check. `await br_check({\"changed_files\": [path, ...]})` (also `\"profile\"`/`\"policy\"`). Returns `{\"status\", \"issue_count\", \"next_action\", ...}` (an unconfigured project returns `\"next_step\"` guidance instead) — summarize it, don't paste raw."
  [env & [opts]]
  (let [opts*
        (normalize-opts opts)

        discovery
        (profile-discovery (workspace-root env) opts*)]

    (stringify-result (if-not (:configured? discovery)
                        (tool-success :br/check
                                      (now-ms)
                                      (assoc (no-profile-result discovery)
                                        :status "unconfigured"
                                        :issue-count 1
                                        :next-step (unconfigured-next-step)
                                        :changed-files (ensure-vector (get opts* "changed_files")))
                                      opts*)
                        (bridge-check env opts*)))))

(defn next
  "Next suggested Bridge action(s). `await br_next({\"changed_files\": [path, ...]})`. Returns `{\"status\", \"suggestions\": [...], \"next_step\"}`."
  [env & [opts]]
  (let [opts*
        (normalize-opts opts)

        discovery
        (profile-discovery (workspace-root env) opts*)]

    (stringify-result
      (if-not (:configured? discovery)
        (tool-success :br/next
                      (now-ms)
                      (assoc (no-profile-result discovery)
                        :status "unconfigured"
                        :issue-count 1
                        :next-step (unconfigured-next-step)
                        :suggestions [(unconfigured-next-step)]
                        :changed-files (ensure-vector (get opts* "changed_files")))
                      opts*)
        (tool-success :br/next (now-ms) (next-result (:result (bridge-check env opts*))) opts*)))))

(defn list-evidence
  "List the active profile's evidence commands. `await br_list_evidence()`. Returns `{\"commands\": [...]}`."
  [env & [opts]]
  (stringify-result (bridge-tool :br/list-evidence
                                 env
                                 opts
                                 (fn [opts]
                                   (let [discovery (profile-discovery (workspace-root env) opts)]
                                     (if-not (:configured? discovery)
                                       (assoc (no-profile-result discovery) :commands [])
                                       (let [{:keys [profile profile-path]}
                                             (load-profile+policy env opts)]
                                         {:configured? true
                                          :profile-path profile-path
                                          :commands (br/list-commands profile)})))))))

(defn run-evidence
  "Run one evidence command and write its receipt. `await br_run_evidence(id, {\"subject\": s, \"out\": path, \"out_dir\": path, \"timeout_seconds\": n, \"is_dry_run\": True})`. `is_dry_run` previews the plan without writing."
  [env id & [opts]]
  (stringify-result
    (bridge-tool
      :br/run-evidence
      env
      opts
      (fn [opts]
        (let [discovery (profile-discovery (workspace-root env) opts)]
          (when-not (:configured? discovery)
            (throw (ex-info "Bridge profile not configured."
                            {:type :vis.bridge/profile-not-found :bridge/discovery discovery})))
          (let [{:keys [profile profile-path]} (load-profile+policy env opts)]
            {:profile-path profile-path
             :result (br/run-command profile
                                     (str id)
                                     {:out-dir (get opts "out_dir")
                                      :out-path (get opts "out")
                                      :subject (get opts "subject")
                                      :timeout-seconds (get opts "timeout_seconds")
                                      :dry-run? (boolean (get opts "is_dry_run"))})}))))))

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def bridge-symbols
  [(vis/symbol #'init {:before-fn inject-env :tag :mutation :arglists '([] [opts])})
   (vis/symbol #'profile {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'check {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'next {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'list-evidence {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'run-evidence {:before-fn inject-env :tag :mutation :arglists '([id] [id opts])})])

(def bridge-prompt
  (str/join
    " "
    ["`br_*` Bridge verification tools:" "use `br_init()` to bootstrap Bridge in a new repo,"
     "use `br_check()` first when asked for Bridge status,"
     "use `br_next()` to inspect the immediate next action,"
     "use `br_list_evidence()` to inspect configured evidence commands,"
     "and use `br_run_evidence(id, opts?)` only when the configured command should actually run."
     "`br_run_evidence(id, {\"is_dry_run\": True})` previews the execution plan without writing a receipt."
     "When answering status questions, summarize the returned map instead of pasting it raw; if a result is large, slice / shape it in python_execution rather than dumping it back."
     "Prefer `counts`, `required_obligations`, `evidence_receipts`, and `next_action` when they are present."
     "Call out `status`, `issue_count`, open or failed obligations, and any evidence receipts that are already present."
     "Keep policy obligations and runnable evidence ids distinct: for example `unit-tests` is not the same thing as the runnable `unit` command."
     "Prefer the `br_next` suggestions over shell commands because they stay inside the Vis tool surface."]))

(def bridge-hooks
  [{:id :vis.bridge/next
    :doc
    "Hint the model about the next Bridge action when a configured workspace has open evidence work. Silent when Bridge is not configured."
    :phase :turn.iteration/start
    :lifetime :turn
    :fn bridge-hint}])

;; Tags carried INLINE on each `vis/symbol` opts map above;
;; register-extension! auto-populates the op registry.

;; =============================================================================
;; CLI surface -- `vis ext bridge <subcommand>`
;;
;; Mirrors the `br/` tool alias (`br/init`, `br/check`, ...) so the binary
;; reflects the same operations the model sees inside iterations.
;; Every subcommand thin-wraps the matching tool fn with an empty
;; env (workspace-root defaults to `user.dir`), prints the resulting
;; map as EDN, and exits non-zero on tool failure or open Bridge
;; obligations.
;; =============================================================================

(defn- println-original! [s] (.println ^java.io.PrintStream System/out (str s)))

(defn- pprint-edn [v] (with-out-str (pprint/pprint v)))

(defn- cli-result-status
  "Translate a `bridge-tool` result map into a process exit code.
   `extension/failure` payloads expose `:status :error`; success
   payloads carry the underlying tool result under `:result`. We
   exit non-zero on failure or when the underlying tool reports any
   open Bridge issues."
  [result]
  (let [tool-result (:result result)]
    (cond (= :error (:status result)) 1
          (= "unconfigured" (get tool-result "status")) 1
          (pos? (long (or (get tool-result "issue_count") 0))) 1
          :else 0)))

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
  (loop [xs
         (vec residual)

         opts
         {}]

    (let [[head & tail] xs]
      (cond (nil? head) opts
            (= "--dry-run" head) (recur (vec tail) (assoc opts "is_dry_run" true))
            (#{"--profile" "--policy" "--subject" "--out" "--out-dir"} head)
            (let [k (case head
                      "--profile"
                      "profile"

                      "--policy"
                      "policy"

                      "--subject"
                      "subject"

                      "--out"
                      "out"

                      "--out-dir"
                      "out_dir")]
              (recur (vec (rest tail)) (assoc opts k (first tail))))
            (= "--changed-file" head)
            (recur (vec (rest tail)) (update opts "changed_files" (fnil conj []) (first tail)))
            (= "--timeout-seconds" head)
            (recur (vec (rest tail)) (assoc opts "timeout_seconds" (parse-long (str (first tail)))))
            (str/starts-with? (str head) "--") (throw (ex-info (str "Unknown bridge flag: " head)
                                                               {:flag head}))
            :else (throw (ex-info (str "Unexpected positional argument: " head) {:arg head}))))))

(defn- cli-env [] {})

(defn- cli-init!
  [_parsed residual]
  (let [opts (parse-kv-opts residual)]
    (emit-result! (init (cli-env) opts))))

(defn- cli-profile! [_parsed residual] (emit-result! (profile (cli-env) (parse-kv-opts residual))))

(defn- cli-check! [_parsed residual] (emit-result! (check (cli-env) (parse-kv-opts residual))))

(defn- cli-next! [_parsed residual] (emit-result! (next (cli-env) (parse-kv-opts residual))))

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
  [{:cmd/name "bridge"
    :cmd/doc "Bridge verification coordinator -- mirrors the `br/` tool alias."
    :cmd/usage "vis ext bridge <init|profile|check|next|list-evidence|run-evidence> [flags]"
    :cmd/subcommands
    [{:cmd/name "init"
      :cmd/doc "Bootstrap Bridge for this workspace (.bridge/profile.edn etc)."
      :cmd/usage "vis ext bridge init"
      :cmd/run-fn cli-init!}
     {:cmd/name "profile"
      :cmd/doc "Print the active Bridge profile summary."
      :cmd/usage "vis ext bridge profile [--profile PATH] [--policy PATH]"
      :cmd/run-fn cli-profile!}
     {:cmd/name "check"
      :cmd/doc "Run Bridge check for the workspace; exits non-zero on open obligations."
      :cmd/usage "vis ext bridge check [--changed-file PATH ...] [--profile PATH] [--policy PATH]"
      :cmd/run-fn cli-check!}
     {:cmd/name "next"
      :cmd/doc "Print the next suggested Bridge action(s)."
      :cmd/usage "vis ext bridge next [--changed-file PATH ...] [--profile PATH]"
      :cmd/run-fn cli-next!}
     {:cmd/name "list-evidence"
      :cmd/doc "List evidence commands configured by the active profile."
      :cmd/usage "vis ext bridge list-evidence [--profile PATH]"
      :cmd/run-fn cli-list-evidence!}
     {:cmd/name "run-evidence"
      :cmd/doc "Run a configured evidence command and write its receipt."
      :cmd/usage
      "vis ext bridge run-evidence <id> [--dry-run] [--subject S] [--out PATH] [--out-dir PATH] [--timeout-seconds N] [--profile PATH]"
      :cmd/examples ["vis ext bridge run-evidence unit --dry-run"
                     "vis ext bridge run-evidence unit --timeout-seconds 300"]
      :cmd/run-fn cli-run-evidence!}]}])

(def vis-extension
  (vis/extension {:ext/name "foundation-bridge"
                  :ext/description "Bridge verification coordinator tools under `br/`."
                  :ext/version "0.1.0"
                  :ext/author "enajski"
                  :ext/owner "vis"
                  :ext/license "Apache-2.0"
                  :ext/engine {:ext.engine/alias 'br :ext.engine/symbols bridge-symbols}
                  :ext/cli bridge-cli
                  :ext/hooks bridge-hooks
                  :ext/kind "verification"
                  :ext/protected-paths bridge-protected-paths
                  :ext/prompt-fn bridge-prompt}))

(vis/register-extension! vis-extension)
