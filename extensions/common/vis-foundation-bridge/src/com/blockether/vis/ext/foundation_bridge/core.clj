(ns com.blockether.vis.ext.foundation-bridge.core
  "Bridge verification tools for Vis.

   Consumes Bridge exclusively through its public library API
   (`bridge.api`); `br/check` returns Bridge's canonical status summary
   (`:summary-version` 1) plus the Vis envelope keys. This extension
   adds no flattening of its own — meaning lives in the kernel."
  (:require [bridge.api :as br]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]))

(def ^:private default-profile-paths
  ;; Bridge 0.2.x writes YAML profiles; keep the legacy `.edn` names as
  ;; fallbacks so older workspaces still resolve. Order = discovery priority.
  [".bridge/profile.yaml" ".bridge/persistent/profile.yaml" ".bridge/profile.edn"
   ".bridge/persistent/profile.edn"])

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
   public tool-fn exit — internal builders stay idiomatic keyword Clojure until
   the result crosses into the sandbox."
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
        (string? x) (pr-str x)
        (boolean? x) (if x "True" "False")
        (map? x) (str "{"
                      (str/join ", "
                                (map (fn [[k v]]
                                       (str (py-arg k) ": " (py-arg v)))
                                     x))
                      "}")
        (sequential? x) (str "[" (str/join ", " (map py-arg x)) "]")
        :else (str x)))

(defn- tool-call
  [tool args]
  {:tool tool
   :args (vec args)
   :call (str (py-tool-name tool) "(" (str/join ", " (map py-arg args)) ")")})

(defn- canonical-root [root] (br/resolve-path (or root ".") "."))

(defn- profile-at-root
  [root]
  (some (fn [path]
          (let [resolved (br/resolve-path root path)]
            (when (br/exists? resolved) resolved)))
        default-profile-paths))

(defn- profile-discovery-at-root
  [root]
  (let [root*
        (canonical-root root)

        profile-path
        (profile-at-root root*)]

    {:workspace-root root*
     :configured? (boolean profile-path)
     :profile-path profile-path
     :searched-paths (mapv #(br/resolve-path root* %) default-profile-paths)
     :explicit-profile? false}))

(defn- bridge-project-discovery
  [root]
  (let [root*
        (canonical-root root)

        inventory
        (vis/repository-inventory root*)

        repository-roots
        (mapv :root (:repositories inventory))

        candidate-roots
        (distinct (cons root* repository-roots))

        projects
        (->> candidate-roots
             (keep (fn [project-root]
                     (when-let [profile-path (profile-at-root project-root)]
                       {:root project-root :profile-path profile-path})))
             (sort-by :root)
             vec)

        active-project
        (some #(when (= root* (:root %)) %) projects)

        default-project
        (or active-project
            (when (and (not (:truncated? inventory)) (= 1 (count projects))) (first projects)))

        selection-ambiguous?
        (and (nil? default-project) (or (:truncated? inventory) (> (count projects) 1)))]

    {:workspace-root root*
     :projects projects
     :repository-roots repository-roots
     :active-root-repository? (boolean (some #{root*} repository-roots))
     :default-profile-path (:profile-path default-project)
     :discovery-truncated? (boolean (:truncated? inventory))
     :selection-ambiguous? selection-ambiguous?}))

(defn- profile-discovery
  [root opts]
  (let [root*
        (canonical-root root)

        explicit
        (get opts "profile")

        explicit-path
        (some->> explicit
                 (br/resolve-path root*))

        project-discovery
        (when-not explicit (bridge-project-discovery root*))

        profile-path
        (or explicit-path (:default-profile-path project-discovery))

        configured?
        (if explicit-path (br/exists? explicit-path) (boolean profile-path))]

    (merge {:workspace-root root*
            :configured? configured?
            :profile-path profile-path
            :searched-paths (mapv #(br/resolve-path root* %) default-profile-paths)
            :explicit-profile? (boolean explicit)}
           project-discovery)))

(defn- profile-selection-required?
  [discovery]
  (or (:explicit-profile? discovery) (:selection-ambiguous? discovery)))

(defn- throw-profile-selection!
  [discovery]
  (throw (ex-info "Bridge profile not selected."
                  {:type :vis.bridge/profile-not-found :bridge/discovery discovery})))

(defn- no-profile-result
  [{:keys [workspace-root searched-paths explicit-profile? profile-path repository-roots]}]
  (cond-> {:configured? false
           :workspace-root workspace-root
           :profile-path profile-path
           :searched-paths searched-paths
           :next-step {:kind :extension-op
                       :op (if (seq repository-roots)
                             (tool-call "br/init" [{"root" "/abs/path/to/project"}])
                             (tool-call "br/init" []))}
           :message (if explicit-profile?
                      "Bridge profile path was provided but no profile was found there."
                      "No Bridge profile is configured for this workspace.")}
    (seq repository-roots)
    (assoc :repository-roots repository-roots)))

(defn- no-profile-error
  [{:keys [workspace-root searched-paths explicit-profile? profile-path projects repository-roots
           discovery-truncated? selection-ambiguous?]}]
  (let
    [multiple?
     (> (count projects) 1)

     message
     (cond explicit-profile? "Bridge profile path was provided but no profile was found there."
           (and selection-ambiguous? discovery-truncated?)
           "Bridge project selection is ambiguous because repository discovery was truncated."
           multiple? "Multiple Bridge projects are configured; select one explicitly."
           :else "No Bridge profile is configured for this workspace.")

     hint
     (if selection-ambiguous?
       "Pass `{\"profile\": \"/abs/path/to/.bridge/profile.yaml\"}` to the br/* operation."
       (if (seq repository-roots)
         (str
           "Choose one repository and call `br_init({\"root\": \"/abs/path/to/project\"})`, or pass an explicit `profile`. "
           "Workspace root: "
           workspace-root)
         (str
           "Initialize Bridge with bare `br_init()`, or pass `{\"profile\": \"/abs/path/to/.bridge/profile.yaml\"}`. "
           "Workspace root: "
           workspace-root)))]

    {:message message
     :hint hint
     :details {:profile-path profile-path
               :searched-paths searched-paths
               :projects projects
               :discovery-truncated? discovery-truncated?}}))

(defn- load-profile+policy
  [env opts]
  (let [discovery (profile-discovery (workspace-root env) opts)]
    (when-not (:configured? discovery) (throw-profile-selection! discovery))
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
  (let [projects (->> (:projects (bridge-project-discovery (workspace-root env)))
                      (sort-by (fn [{:keys [root]}]
                                 [(- (.getNameCount (.toPath (java.io.File. root)))) root])))]
    (->> projects
         (mapcat (fn [{:keys [profile-path]}]
                   (let [{:keys [profile policy]} (load-profile+policy env {"profile" profile-path})
                         sandbox (:bridge-path-sandbox policy)]

                     (if (and sandbox (:enforce? sandbox))
                       (keep #(bridge-sandbox-rule->protected-path env profile sandbox %)
                             (:rules sandbox))
                       []))))
         vec)))

(defn- bridge-session-context
  [env]
  (let [{:keys [projects default-profile-path discovery-truncated?]} (bridge-project-discovery
                                                                       (workspace-root env))]
    (if (seq projects)
      {"session_env" {"bridge" (cond-> {"projects" (mapv (fn [{:keys [root profile-path]}]
                                                           {"root" root
                                                            "profile_path" profile-path})
                                                         projects)}
                                 default-profile-path
                                 (assoc "default_profile_path" default-profile-path)

                                 discovery-truncated?
                                 (assoc "discovery_truncated" true))}}
      {})))

(defn- selected-opts
  [opts]
  (select-keys opts
               ["root" "profile" "policy" "changed_files" "subject" "out_dir" "out"
                "timeout_seconds" "is_dry_run"]))

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

(defn- root-required-error
  [{:keys [workspace-root repository-roots discovery-truncated?]}]
  {:message
   "Bridge initialization needs an explicit project root in this multi-repository workspace."
   :hint "Call `br_init({\"root\": \"/abs/path/to/project\"})` for exactly one repository."
   :details {:workspace-root workspace-root
             :repository-roots repository-roots
             :discovery-truncated? discovery-truncated?}})

(defn- bridge-tool
  [op _env opts f]
  (let [started-at-ms
        (now-ms)

        opts*
        (normalize-opts opts)]

    (try (tool-success op started-at-ms (f opts*) opts*)
         (catch Throwable t
           (cond (:bridge/discovery (ex-data t))
                 (tool-failure op
                               started-at-ms
                               {:error (no-profile-error (:bridge/discovery (ex-data t)))}
                               opts*)
                 (:bridge/root-required (ex-data t))
                 (tool-failure op
                               started-at-ms
                               {:error (root-required-error (:bridge/root-required (ex-data t)))}
                               opts*)
                 :else (tool-failure op started-at-ms {:throwable t} opts*))))))

(defn- init-root
  [env opts]
  (let [workspace-root*
        (canonical-root (workspace-root env))

        explicit-root
        (get opts "root")

        root-provided?
        (contains? opts "root")]

    (if root-provided?
      (or (br/resolve-path workspace-root* explicit-root)
          (throw (ex-info "Bridge root must be a non-blank path."
                          {:type :vis.bridge/invalid-root :root explicit-root})))
      (let [{:keys [repository-roots active-root-repository? discovery-truncated?] :as discovery}
            (bridge-project-discovery workspace-root*)]
        (when (and (not active-root-repository?) (or (seq repository-roots) discovery-truncated?))
          (throw (ex-info "Explicit Bridge project root required."
                          {:type :vis.bridge/root-required :bridge/root-required discovery})))
        workspace-root*))))

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

(defn init
  "Bootstrap Bridge in this workspace. `await br_init()` (opts `{\"root\": path}`). Returns the existing config if already set up."
  [env & [opts]]
  (stringify-result
    (bridge-tool :br/init
                 env
                 opts
                 (fn [opts]
                   (let [root
                         (init-root env opts)

                         discovery
                         (profile-discovery-at-root root)]

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
                             (profile-discovery-at-root root)]

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
                                       (if (profile-selection-required? discovery)
                                         (throw-profile-selection! discovery)
                                         (no-profile-result discovery))
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
                        (if (profile-selection-required? discovery)
                          (bridge-tool :br/check
                                       env
                                       opts*
                                       (fn [_]
                                         (throw-profile-selection! discovery)))
                          (tool-success :br/check
                                        (now-ms)
                                        (assoc (no-profile-result discovery)
                                          :status "unconfigured"
                                          :issue-count 1
                                          :changed-files (ensure-vector (get opts*
                                                                             "changed_files")))
                                        opts*))
                        (bridge-check env opts*)))))

(defn list-evidence
  "List the active profile's evidence commands. `await br_list_evidence()`. Returns `{\"commands\": [...]}`."
  [env & [opts]]
  (stringify-result (bridge-tool :br/list-evidence
                                 env
                                 opts
                                 (fn [opts]
                                   (let [discovery (profile-discovery (workspace-root env) opts)]
                                     (if-not (:configured? discovery)
                                       (if (profile-selection-required? discovery)
                                         (throw-profile-selection! discovery)
                                         (assoc (no-profile-result discovery) :commands []))
                                       (let [{:keys [profile profile-path]}
                                             (load-profile+policy env opts)]
                                         {:configured? true
                                          :profile-path profile-path
                                          :commands (br/list-commands profile)})))))))

(defn run-evidence
  "Run one evidence command and write its receipt. `await br_run_evidence(id, {\"subject\": s, \"out\": path, \"out_dir\": path, \"timeout_seconds\": n, \"is_dry_run\": True})`. `is_dry_run` previews the plan without writing."
  [env id & [opts]]
  (stringify-result
    (bridge-tool :br/run-evidence
                 env
                 opts
                 (fn [opts]
                   (let [discovery (profile-discovery (workspace-root env) opts)]
                     (when-not (:configured? discovery) (throw-profile-selection! discovery))
                     (let [{:keys [profile profile-path]} (load-profile+policy env opts)]
                       {:profile-path profile-path
                        :result (br/run-command profile
                                                (str id)
                                                {:out-dir (get opts "out_dir")
                                                 :out-path (get opts "out")
                                                 :subject (get opts "subject")
                                                 :timeout-seconds (get opts "timeout_seconds")
                                                 :dry-run? (boolean (get opts
                                                                         "is_dry_run"))})}))))))

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def bridge-symbols
  [(vis/symbol #'init {:before-fn inject-env :tag :mutation :arglists '([] [opts])})
   (vis/symbol #'profile {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'check {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'list-evidence {:before-fn inject-env :tag :observation :arglists '([] [opts])})
   (vis/symbol #'run-evidence {:before-fn inject-env :tag :mutation :arglists '([id] [id opts])})])

(defn bridge-prompt
  [env]
  (let [{:keys [projects default-profile-path]} (bridge-project-discovery (workspace-root env))]
    (when (seq projects)
      (str
        (if (> (count projects) 1)
          (if default-profile-path
            "Bridge configured for multiple projects; bare br/* calls use the active-root profile, and other projects require `{\"profile\": path}`. "
            "Bridge configured for multiple projects; pass `{\"profile\": path}` to each br/* call. ")
          "Bridge configured. ")
        "Use `br_check()` for canonical status and `next_action`; "
        "use `br_run_evidence(...)` only when verification is in scope. "
        "Summarize selected fields; never dump the result. Use `doc(name)` for contracts."))))

;; Tags carried INLINE on each `vis/symbol` opts map above;
;; register-extension! auto-populates the op registry.

;; =============================================================================
;; CLI surface -- `vis extension bridge <subcommand>`
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
   `extension/failure` payloads expose `:success? false`; successful
   payloads carry the underlying tool result under `:result`. We
   exit non-zero on failure or when the underlying tool reports any
   open Bridge issues."
  [result]
  (let [tool-result (:result result)]
    (cond (false? (:success? result)) 1
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
   flags: `--root PATH`, `--profile PATH`, `--policy PATH`, `--changed-file PATH`
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
            (#{"--root" "--profile" "--policy" "--subject" "--out" "--out-dir"} head)
            (let [k (case head
                      "--root"
                      "root"

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

(defn- cli-list-evidence!
  [_parsed residual]
  (emit-result! (list-evidence (cli-env) (parse-kv-opts residual))))

(defn- cli-run-evidence!
  [_parsed residual]
  (let [[id & rest-args] (vec residual)]
    (when (str/blank? (str id))
      (println-original! "Usage: vis extension bridge run-evidence <id> [--dry-run] [...flags]")
      (System/exit 1))
    (emit-result! (run-evidence (cli-env) id (parse-kv-opts rest-args)))))

(def ^:private bridge-cli
  [{:cmd/name "bridge"
    :cmd/doc "Bridge verification coordinator -- mirrors the `br/` tool alias."
    :cmd/usage "vis extension bridge <init|profile|check|list-evidence|run-evidence> [flags]"
    :cmd/subcommands
    [{:cmd/name "init"
      :cmd/doc "Bootstrap Bridge for this workspace (.bridge/profile.yaml etc)."
      :cmd/usage "vis extension bridge init [--root PATH]"
      :cmd/run-fn cli-init!}
     {:cmd/name "profile"
      :cmd/doc "Print the active Bridge profile summary."
      :cmd/usage "vis extension bridge profile [--profile PATH] [--policy PATH]"
      :cmd/run-fn cli-profile!}
     {:cmd/name "check"
      :cmd/doc "Run Bridge check for the workspace; exits non-zero on open obligations."
      :cmd/usage
      "vis extension bridge check [--changed-file PATH ...] [--profile PATH] [--policy PATH]"
      :cmd/run-fn cli-check!}
     {:cmd/name "list-evidence"
      :cmd/doc "List evidence commands configured by the active profile."
      :cmd/usage "vis extension bridge list-evidence [--profile PATH]"
      :cmd/run-fn cli-list-evidence!}
     {:cmd/name "run-evidence"
      :cmd/doc "Run a configured evidence command and write its receipt."
      :cmd/usage
      "vis extension bridge run-evidence <id> [--dry-run] [--subject S] [--out PATH] [--out-dir PATH] [--timeout-seconds N] [--profile PATH]"
      :cmd/examples ["vis extension bridge run-evidence unit --dry-run"
                     "vis extension bridge run-evidence unit --timeout-seconds 300"]
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
                  :ext/kind "verification"
                  :ext/ctx-fn bridge-session-context
                  :ext/protected-paths bridge-protected-paths
                  :ext/prompt-fn bridge-prompt}))

(vis/register-extension! vis-extension)
