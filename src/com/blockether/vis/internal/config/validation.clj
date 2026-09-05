(ns com.blockether.vis.internal.config.validation
  "Validates raw configuration with the contract JSON Schema and derives runtime policy maps."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.config :as contract-config]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]))

(def ^:private sensitive-keys #{"api_key" "environment" "env" "headers" "llm_headers" "extra_body"})

(defn- redact
  [x]
  (cond (map? x) (into {}
                       (map (fn [[k v]]
                              [k (if (sensitive-keys k) "<redacted>" (redact v))]))
                       x)
        (vector? x) (mapv redact x)
        (set? x) (set (map redact x))
        :else x))

(def api-style-values contract-config/api-style-values)

(def api-style-aliases
  (into {}
        (map (fn [[alias normalized]]
               [alias (keyword normalized)]))
        contract-config/api-style-aliases))

(def workspace-access-values contract-config/workspace-access-values)

(def workspace-draft-values contract-config/workspace-draft-values)

(def workspace-os-values contract-config/workspace-os-values)

(defn normalize-api-style
  "Normalize a declared provider API style to the keyword used by the router."
  [value]
  (when (or (string? value) (keyword? value) (symbol? value))
    (get api-style-aliases (if (or (keyword? value) (symbol? value)) (name value) value))))

(defn explain-data [config] (contract-config/config-explain-data config))

(defn valid? [config] (contract-config/config-valid? config))

(defn providers-valid?
  [providers]
  (contract-config/definition-valid? "config" {"providers" providers}))

(defn environment-valid?
  [environment]
  (contract-config/definition-valid? "environment" environment))

(defn- pointer-segments
  [pointer]
  (if (str/blank? pointer)
    []
    (mapv #(-> %
               (str/replace "~1" "/")
               (str/replace "~0" "~"))
          (rest (str/split pointer #"/" -1)))))

(defn- error-path
  [{:keys [instanceLocation keyword params]}]
  (cond-> (pointer-segments instanceLocation)
    (= "additionalProperties" keyword)
    (conj (:additionalProperty params))

    (= "required" keyword)
    (conj (:missingProperty params))))

(defn- path-prefix? [prefix path] (= prefix (subvec path 0 (min (count prefix) (count path)))))

(defn- deepest-errors
  [errors]
  (let [with-path
        (mapv #(assoc % ::path (error-path %)) errors)

        enum-paths
        (into #{} (comp (filter #(= "enum" (:keyword %))) (map ::path)) with-path)

        enum-parents
        (into #{} (map pop) enum-paths)]

    (remove (fn [{path ::path keyword :keyword :as error}]
              (or (and (= "required" keyword) (contains? enum-parents (pop path)))
                  (and (= "const" keyword) (contains? enum-paths path))
                  (some (fn [{other-path ::path :as other}]
                          (and (not (identical? error other))
                               (< (count path) (count other-path))
                               (path-prefix? path other-path)))
                        with-path)))
      with-path)))

(defn- field-label
  [segments]
  (reduce (fn [label segment]
            (if (re-matches #"\d+" (str segment))
              (str label "[" segment "]")
              (str label (when (seq label) ".") segment)))
          ""
          segments))

(defn- edit-distance
  [a b]
  (let [b
        (vec b)

        n
        (count b)]

    (peek (reduce (fn [previous [i character]]
                    (reduce (fn [row j]
                              (conj row
                                    (min (inc (long (nth row j)))
                                         (inc (long (nth previous (inc (long j)))))
                                         (+ (long (nth previous j))
                                            (if (= character (nth b j)) 0 1)))))
                            [(inc (long i))]
                            (range n)))
                  (vec (range (inc (long n))))
                  (map-indexed vector a)))))

(defn- closest-key
  [key known]
  (let [[candidate distance]
        (first (sort-by second
                        (map (fn [name]
                               [name (edit-distance (str/lower-case key) (str/lower-case name))])
                             known)))]
    (when (and candidate (seq key) (<= (long distance) (max 1 (quot (count key) 3)))) candidate)))

(defn- error-definition
  [{:keys [absoluteKeywordLocation]}]
  (second (re-find #"#/\$defs/([^/]+)/additionalProperties$" (str absoluteKeywordLocation))))

(defn- format-schema-error
  [{:keys [keyword params error message] :as schema-error}]
  (let [path
        (error-path schema-error)

        label
        (or (not-empty (field-label path)) "config")]

    (case keyword
      "additionalProperties"
      (let [unknown
            (:additionalProperty params)

            known
            (contract-config/definition-property-names (error-definition schema-error))

            hint
            (when (and (string? unknown) (seq known)) (closest-key unknown known))]

        (str label
             ": unknown " (if (= 1 (count path)) "top-level config key" "key")
             " (config is closed)"
             (when hint (str " — did you mean \"" (field-label (conj (pop path) hint)) "\"?"))))

      "required"
      (str label ": required key is missing")

      "enum"
      (if (contains? #{"api_style" "compatibility"} (last path))
        (str label ": value is not a wire dialect - use one of " (str/join ", " api-style-values))
        (str label ": " error))

      (str label ": " (or error message "does not satisfy the configuration schema")))))

(defn explain-problems
  "Readable field errors produced from Skjema's JSON Schema diagnostics."
  [config]
  (cond (nil? config) []
        (not (map? config)) ["config: expected a YAML map with string keys"]
        :else (->> (:errors (explain-data config))
                   deepest-errors
                   (map format-schema-error)
                   distinct
                   vec)))

(def derived-machine-keys #{"vision_memory"})

(def project-scoped-config-keys #{"workspace" "jail" "environment"})

(defn without-project-scoped
  "Drop checkout-owned configuration before writing the machine store."
  [config]
  (if-not (map? config)
    [config #{}]
    (let [dropped (into #{} (filter #(contains? config %)) project-scoped-config-keys)]
      [(apply dissoc config dropped) dropped])))

(defn- error-top-level-key [schema-error] (first (error-path schema-error)))

(defn without-invalid-derived
  "Drop invalid machine-derived blocks so the next write can rebuild them."
  [config]
  (if (or (not (map? config)) (valid? config))
    [config #{}]
    (let [dropped (into #{}
                        (comp (map error-top-level-key)
                              (filter derived-machine-keys)
                              (filter #(contains? config %)))
                        (:errors (explain-data config)))]
      [(apply dissoc config dropped) dropped])))

(defn config-error-panel
  [fields source]
  (into ["" (str "  Invalid Vis configuration" (when source (str " in " source)) ":") ""]
        (concat (map #(str "  - " %) (or (seq fields) ["config: does not match the Vis contract"]))
                ["" "  Fix the entries above and run vis-agent again." ""])))

(defn assert-config!
  "Validate raw string-keyed configuration with the contract JSON Schema."
  ([config] (assert-config! config nil))
  ([config source]
   (if (valid? config)
     config
     (let [fields (explain-problems config)]
       (throw (ex-info (str "Invalid Vis configuration"
                            (when source (str " in " source))
                            (when (seq fields) (str ":\n  - " (str/join "\n  - " fields))))
                       {:type :vis/invalid-config
                        :vis/user-error true
                        :vis/panel (config-error-panel fields source)
                        :source source
                        :fields fields
                        :config (redact config)
                        :problems (mapv redact (:errors (explain-data config)))}))))))

(defn- rooted-path?
  [value]
  (and (util/non-blank-string? value)
       (or (str/starts-with? value "/") (= value "~") (str/starts-with? value "~/"))))

(defn- rooted-path-list? [value] (and (vector? value) (every? rooted-path? value)))

(defn- port? [value] (and (integer? value) (<= 1 value 65535)))

(defn- string-map?
  [value]
  (and (map? value) (every? string? (keys value)) (every? string? (vals value))))

(def process-jail-config-keys
  #{:disabled? :inherit-host-env? :allow-read-write :allow-read :deny-read :deny-write :deny-exec
    :no-search :inbound-ports :path-descriptions :keychain?})

(defn- process-jail-config?
  [policy]
  (and (map? policy)
       (every? process-jail-config-keys (keys policy))
       (boolean? (:disabled? policy))
       (boolean? (:inherit-host-env? policy))
       (every? rooted-path-list?
               ((juxt :allow-read-write :allow-read :deny-read :deny-write) policy))
       (rooted-path-list? (or (:no-search policy) []))
       (rooted-path-list? (or (:deny-exec policy) []))
       (vector? (:inbound-ports policy))
       (= (count (:inbound-ports policy)) (count (distinct (:inbound-ports policy))))
       (every? port? (:inbound-ports policy))
       (boolean? (:keychain? policy))
       (let [descriptions (:path-descriptions policy)]
         (or (nil? descriptions) (string-map? descriptions)))))

(defn assert-process-jail-config!
  "Validate and return the internal policy consumed by process-jail."
  [policy]
  (if (process-jail-config? policy)
    policy
    (throw (ex-info "Invalid process-jail configuration"
                    {:type :vis/invalid-process-jail-config :policy (redact policy)}))))

(defn- resolve-exec-denies
  "Resolve `jail.deny-exec` entries into absolute executable paths that the jail
   forbids from being EXECUTED (an exec deny that overrides the blanket exec
   allow — kernel-enforced, no leaky argv parsing).
   A bare name is looked up on every PATH directory (all matches denied); an
   absolute/home path is denied verbatim."
  [names]
  (let [dirs (some-> (System/getenv "PATH")
                     (str/split (re-pattern java.io.File/pathSeparator)))]
    (into []
          (comp (mapcat (fn [n]
                          (let [n (str n)]
                            (if (or (str/starts-with? n "/") (str/starts-with? n "~"))
                              [n]
                              (into []
                                    (comp (map #(str % java.io.File/separator n))
                                          (filter #(.canExecute (java.io.File. ^String %))))
                                    dirs)))))
                (distinct))
          names)))

(defn entry-read-only?
  "True when the catalog entry explicitly declares read-only access."
  [entry]
  (contains? #{"read-only" "readonly" "ro"}
             (some-> (get entry "access")
                     str/lower-case)))

(defn- entry-no-search?
  "Search visibility defaults to true; only an explicit `search: false` opts out."
  [entry]
  (false? (get entry "search")))

(defn entry-draft-policy
  "The catalog entry draft policy as a keyword. Defaults to `:shared`."
  [entry]
  (case (some-> (get entry "draft")
                str
                str/lower-case)
    "copy-only"
    :copy-only

    "copy-and-apply"
    :copy-and-apply

    "not-allowed"
    :not-allowed

    :shared))

(defn host-os
  "This host's `when.os` token: `macos`, `windows`, `wsl` (Linux under WSL),
   `linux`, or `unknown` when the platform can't be identified."
  []
  (let [n (str/lower-case (str (System/getProperty "os.name")))]
    (cond (str/includes? n "mac") "macos"
          (str/includes? n "win") "windows"
          (str/includes? n "linux") (if (try (str/includes? (str/lower-case (slurp "/proc/version"))
                                                            "microsoft")
                                             (catch Throwable _ false))
                                      "wsl"
                                      "linux")
          :else "unknown")))

(defn- path-present?
  [p]
  (boolean (when (and (string? p) (seq p))
             (.exists (java.io.File. ^String (paths/expand-home p))))))

(defn mount-env
  "Current host facts used to evaluate a `when` clause."
  []
  {:os (host-os) :exists? path-present?})

(defn- when-os-match?
  [declared os]
  (let [wanted (cond (string? declared) #{declared}
                     (coll? declared) (set (map str declared)))]
    (or (nil? wanted)
        (contains? wanted os)
        ;; A Linux condition also admits WSL.
        (and (= "wsl" os) (contains? wanted "linux")))))

(defn entry-mount-status
  "Why a catalog entry does or does not mount on this host:

   `:mounted`         — declared, present, admitted.
   `:os-mismatch`     — `when.os` names other platforms.
   `:when-absent`     — `when.exists` names a path that is not there.
   `:optional-absent` — `optional: true` and the root itself is missing.
   `:missing`         — admitted, but the root does not exist yet."
  ([entry] (entry-mount-status entry (mount-env)))
  ([entry {:keys [os exists?]}]
   (let [clause
         (get entry "when")

         exists?
         (or exists? path-present?)]

     (cond (not (when-os-match? (get clause "os") os)) :os-mismatch
           (and (contains? clause "exists") (not (exists? (get clause "exists")))) :when-absent
           (exists? (get entry "path")) :mounted
           (true? (get entry "optional")) :optional-absent
           :else :missing))))

(defn entry-mounted?
  "True when the entry belongs in THIS host's catalog."
  ([entry] (entry-mounted? entry (mount-env)))
  ([entry env] (contains? #{:mounted :missing} (entry-mount-status entry env))))

(defn applicable-entries
  "The catalog entries that apply to this host, in declaration order: a `when`
   that does not match and an `optional` root whose path is absent are dropped."
  ([entries] (applicable-entries entries (mount-env)))
  ([entries env] (into [] (filter #(entry-mounted? % env)) entries)))

(defn- os-clause-str [declared] (if (string? declared) declared (str/join ", " (map str declared))))

(defn workspace-mount-diagnostics
  "One message per declared root that did NOT mount as written: conditional roots
   the host skipped (`:info`) and admitted roots whose path is missing (`:warn`,
   or `:info` when `optional: true`). Empty when every root is present, so it
   doubles as the startup hint and the `doctor` check."
  ([config] (workspace-mount-diagnostics config (mount-env)))
  ([config env]
   (into
     []
     (keep
       (fn [entry]
         (let [id
               (get entry "id")

               path
               (get entry "path")

               base
               {:id id :path path}]

           (case (entry-mount-status entry env)
             :os-mismatch
             (assoc base
               :level :info
               :reason :os-mismatch
               :message (str "workspace root '"
                             id
                             "' is not mounted: when.os is "
                             (os-clause-str (get-in entry ["when" "os"]))
                             " and this host is "
                             (:os env)
                             ".")
               :remediation "Nothing to do — the root is meant for another platform.")

             :when-absent
             (assoc base
               :level :info
               :reason :when-absent
               :message (str "workspace root '"
                             id
                             "' is not mounted: when.exists path "
                             (get-in entry ["when" "exists"])
                             " does not exist.")
               :remediation "Create that path, or drop the when.exists clause.")

             :optional-absent
             (assoc base
               :level :info
               :reason :optional-absent
               :message
               (str "optional workspace root '" id "' is not mounted: " path " does not exist.")
               :remediation "Create the path to mount it, or leave it optional.")

             :missing
             (assoc base
               :level :warn
               :reason :missing
               :message (str "workspace root '" id "' points at " path ", which does not exist.")
               :remediation (str "Create it, mark the entry optional: true, or gate it with a "
                                 "when: clause."))

             nil))))
     (get-in config ["workspace" "filesystem"] []))))

(defn workspace-draft-policies
  "`{catalog-path -> policy}` for every declared root that opts OUT of the default
   `:shared` behaviour. Independent of `jail.filesystem.allow`: the policy governs
   draft isolation, which applies whether or not the OS jail is enabled. Roots
   this host does not mount never appear."
  ([config] (workspace-draft-policies config (mount-env)))
  ([config env]
   (assert-config! config)
   (into {}
         (keep (fn [entry]
                 (let [policy (entry-draft-policy entry)]
                   (when (not= :shared policy) [(get entry "path") policy]))))
         (applicable-entries (get-in config ["workspace" "filesystem"] []) env))))

(def vis-home-entry
  "The implicit session-state root, excluded from default search."
  {"id" "vis-home"
   "path" "~/.vis"
   "description"
   "Vis' own session folder — session DB, gateway event journals, state.yml, drafts, logs."
   "search" false})

(defn- same-root?
  "True when two catalog paths denote the same directory once `~` is expanded and
   a trailing separator dropped."
  [a b]
  (letfn [(norm [p]
            (some-> p
                    str
                    paths/expand-home
                    paths/unixify
                    (str/replace #"/+$" "")))]
    (= (norm a) (norm b))))

(defn- with-vis-home
  "Append the implicit `~/.vis` entry unless the catalog already declares that
   path — an explicit entry stays the operator's (its id, description and access
   win)."
  [entries]
  (if (some #(same-root? (get % "path") (get vis-home-entry "path")) entries)
    (vec entries)
    (conj (vec entries) vis-home-entry)))

(defn process-jail-config
  "Derive process-jail policy from schema-validated configuration.
   A disabled jail admits the full applicable workspace catalog; an enabled jail
   admits only named roots. The session-state root is always included."
  ([config] (process-jail-config config (mount-env)))
  ([config env]
   (assert-config! config)
   (let [jail
         (get config "jail" {})

         entries
         (applicable-entries (get-in config ["workspace" "filesystem"] []) env)

         by-id
         (reduce (fn [m e]
                   (assoc m (get e "id") e))
                 {}
                 entries)

         allowed
         ;; An enabled jail admits named roots; a disabled jail admits the catalog.
         (if (true? (get jail "enabled"))
           (let [declared
                 (into #{} (map #(get % "id")) (get-in config ["workspace" "filesystem"] []))]
             (into []
                   (keep (fn [id]
                           (or (get by-id id)
                               (when-not (contains? declared id)
                                 (throw
                                   (ex-info
                                     (str "jail.filesystem.allow references unknown workspace id: "
                                          id)
                                     {:type :vis/invalid-config :id id}))))))
                   (get-in jail ["filesystem" "allow"] [])))
           entries)

         allowed
         (with-vis-home allowed)

         descriptions
         (into {}
               (keep (fn [e]
                       (when-let [d (get e "description")]
                         [(get e "path") d])))
               allowed)

         read-only
         (into [] (comp (filter entry-read-only?) (map #(get % "path"))) allowed)

         no-search
         (into [] (comp (filter entry-no-search?) (map #(get % "path"))) allowed)]

     (assert-process-jail-config!
       {:disabled? (not (true? (get jail "enabled")))
        :inherit-host-env? (= "inherit" (get jail "environment"))
        :allow-read-write (into [] (comp (remove entry-read-only?) (map #(get % "path"))) allowed)
        :allow-read read-only
        :deny-read []
        :deny-write []
        :deny-exec (resolve-exec-denies (get jail "deny_exec"))
        :no-search no-search
        :inbound-ports (vec (get-in jail ["network" "inbound_ports"]))
        :keychain? (true? (get jail "keychain"))
        :path-descriptions descriptions}))))

(defn- network-allow->runtime
  [allow]
  (cond-> {:method (get allow "method")}
    (contains? allow "path")
    (assoc :path (get allow "path"))))

(defn- network-rule->runtime
  [rule]
  (cond-> {:host (get rule "host")}
    (contains? rule "access")
    (assoc :access (get rule "access"))

    (contains? rule "methods")
    (assoc :methods (get rule "methods"))

    (contains? rule "ports")
    (assoc :ports (get rule "ports"))

    (contains? rule "allow")
    (assoc :allow (mapv network-allow->runtime (get rule "allow")))))

(defn network-config
  "Derive the keyword-keyed internal egress policy from validated YAML config.
   Egress filtering is one facet of the process jail: `jail.enabled` is the single
   gate. When the jail is off the policy is empty (egress open); when on, the
   `jail.network` block (allowed/denied/exclude domains, `allow_private`, rules)
   is enforced alongside the filesystem and inbound-port confinement."
  [config]
  (assert-config! config)
  (let [jail
        (get config "jail" {})

        net
        (get jail "network" {})]

    (if-not (true? (get jail "enabled"))
      {}
      (cond-> {}
        (contains? net "allowed_domains")
        (assoc :allowed-domains (get net "allowed_domains"))

        (contains? net "denied_domains")
        (assoc :denied-domains (get net "denied_domains"))

        (contains? net "exclude_domains")
        (assoc :exclude-domains (get net "exclude_domains"))

        (contains? net "allow_private")
        (assoc :allow-private (get net "allow_private"))

        (contains? net "rules")
        (assoc :rules (mapv network-rule->runtime (get net "rules")))))))
