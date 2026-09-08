(ns com.blockether.vis.internal.sandbox.policy
  "Canonical immutable security-policy snapshots and their model-facing view.

   A snapshot is created once for a root environment, inherited unchanged by
   child environments, and replaced only by an explicit environment rebuild.
   Enforcement and context both derive from this value."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.validation :as config-validation]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io File]
           [java.nio.file Files LinkOption Path Paths]))

(def ^:private no-link-options (make-array LinkOption 0))

(defn- nearest-real-path
  "Resolve a configured path against `base-dir`, resolving every existing
   ancestor and preserving a missing tail. This snapshots symlink identity while
   still allowing a configured directory to be created after startup."
  [path base-dir home]
  (when-not (str/blank? (str path))
    (let [expanded
          (paths/expand-home path home)

          ^Path raw
          (Paths/get expanded (make-array String 0))

          ^Path absolute
          (.normalize (if (.isAbsolute raw)
                        raw
                        (.resolve (Paths/get (str base-dir) (make-array String 0)) raw)))]

      (loop [^Path ancestor
             absolute

             tail
             ()]

        (cond (nil? ancestor) (.toString absolute)
              (Files/exists ancestor no-link-options)
              (let [real (try (.toRealPath ancestor no-link-options)
                              (catch Throwable _ (.toAbsolutePath ancestor)))]
                (.toString (.normalize ^Path
                                       (reduce (fn [^Path p ^String segment]
                                                 (.resolve p segment))
                                               real
                                               tail))))
              :else (recur (.getParent ancestor) (cons (str (.getFileName ancestor)) tail)))))))

(defn home-relative
  "Render an absolute path under HOME as `~` / `~/…`; leave other paths absolute."
  ([path] (home-relative path (System/getProperty "user.home")))
  ([path home]
   (let [abbreviated (paths/abbreviate-home (some-> path
                                                    str
                                                    not-empty)
                                            home)]
     (if (= "~/" abbreviated) "~" abbreviated))))

(defn- resolve-paths
  [paths base-dir home]
  (vec (distinct (keep #(nearest-real-path % base-dir home) paths))))

(defn- stable-value
  [value]
  (cond (map? value) (into (sorted-map)
                           (map (fn [[k v]]
                                  [(if (keyword? k) (name k) (str k)) (stable-value v)]))
                           value)
        (set? value) (mapv stable-value (sort-by str value))
        (sequential? value) (mapv stable-value value)
        (keyword? value) (name value)
        :else value))

(defn- sha256 [value] (str "sha256:" (util/sha256-hex (pr-str (stable-value value)))))

(defn- project-path-name
  [entry resolved-path]
  (or (get entry "python_name")
      (let [path
            (Paths/get resolved-path (make-array String 0))

            stem
            (or (some-> path
                        .getFileName
                        str
                        not-empty)
                (get entry "id"))

            stem
            (-> stem
                (str/replace #"(.)([A-Z][a-z]+)" "$1_$2")
                (str/replace #"([a-z0-9])([A-Z])" "$1_$2")
                (str/replace #"[^A-Za-z0-9]+" "_")
                (str/replace #"^_+|_+$" "")
                str/lower-case)]

        (when (str/blank? stem)
          (throw (ex-info "Workspace project needs an explicit python_name"
                          {:type :vis/invalid-config
                           :problems
                           [(str "workspace.filesystem entry "
                                 (get entry "id")
                                 ": set python_name to a snake_case name ending in _path")]})))
        (str (when (re-find #"^[0-9]" stem) "project_") stem "_path"))))

(defn- project-paths
  "Snapshot registered project names without turning generic filesystem grants into projects."
  [config base-dir home]
  (reduce
    (fn [bindings entry]
      (if (and (false? (get entry "search")) (not (get entry "python_name")))
        bindings
        (let [path
              (nearest-real-path (get entry "path") base-dir home)

              alias
              (project-path-name entry path)]

          (when (or (= "project_root_path" alias)
                    (and (contains? bindings alias) (not= path (get bindings alias))))
            (throw
              (ex-info
                (str "Conflicting Python project path name: " alias)
                {:type :vis/invalid-config
                 :problems
                 [(str
                    "workspace.filesystem: "
                    alias
                    " is reserved or names more than one project; set a distinct python_name")]})))
          (assoc bindings alias path))))
    (sorted-map)
    (config-validation/admitted-workspace-entries config)))

(defn- java-installation-root
  "Recognize a real Java installation, never a launcher directory or manager root."
  [executable]
  (when-not (str/blank? executable)
    (try (let [path (Paths/get executable (make-array String 0))]
           (when (.isAbsolute path)
             (let [real (.toRealPath path no-link-options)
                   bin (.getParent real)
                   root (some-> bin
                                .getParent)]

               (when (and root
                          (= "java" (str (.getFileName real)))
                          (= "bin" (str (.getFileName bin)))
                          (Files/isExecutable real)
                          (Files/isRegularFile (.resolve root "release") no-link-options)
                          (Files/isDirectory (.resolve root "lib") no-link-options))
                 (str root)))))
         (catch Exception _ nil))))

(defn- java-read-roots
  "Snapshot the host JVM and Java selected by the host's JAVA_HOME/PATH.
   Inspect installation paths only; never execute launchers or scan other versions."
  ([]
   (java-read-roots (System/getProperty "java.home")
                    {"JAVA_HOME" (System/getenv "JAVA_HOME") "PATH" (System/getenv "PATH")}))
  ([java-home environment]
   (let [path-java (some (fn [directory]
                           (when-not (str/blank? directory)
                             (try (let [path (Paths/get directory (into-array String ["java"]))]
                                    (when (and (.isAbsolute path)
                                               (Files/isRegularFile path no-link-options)
                                               (Files/isExecutable path))
                                      (str path)))
                                  (catch Exception _ nil))))
                         (str/split (or (get environment "PATH") "")
                                    (re-pattern (java.util.regex.Pattern/quote
                                                  File/pathSeparator))))]
     (->> (conj (mapv #(when-not (str/blank? %) (str % "/bin/java"))
                      [java-home (get environment "JAVA_HOME")])
                path-java)
          (keep java-installation-root)
          distinct
          vec))))

(defn snapshot
  "Build the immutable canonical security policy from validated string-keyed
   configuration. Relative and home-relative paths become absolute; symlinks are
   resolved at this boundary. Recognized host Java installations are process-only
   read grants, excluded from default searches unless already explicitly granted."
  ([config] (snapshot config {}))
  ([config
    {:keys [base-dir home]
     :or {base-dir (System/getProperty "user.dir") home (System/getProperty "user.home")}}]
   (config-validation/assert-config! config)
   (let [jail
         (config-validation/process-jail-config config)

         network
         (config-validation/network-config config)

         path-keys
         [:allow-read-write :allow-read :deny-read :deny-write :no-search]

         jail
         (reduce (fn [policy key]
                   (update policy key resolve-paths base-dir home))
                 jail
                 path-keys)

         ;; (language caches now live in the workspace catalog and resolve through the
         ;;  path-keys reduce above; no separate cache-resolution pass.)
         jail
         (update jail
                 :path-descriptions
                 (fn [m]
                   (into {}
                         (keep (fn [[k v]]
                                 (when-let [rp (nearest-real-path k base-dir home)]
                                   [rp v])))
                         m)))

         jail
         (let [explicit
               (set (concat (:allow-read-write jail) (:allow-read jail)))

               roots
               (when-not (:disabled? jail) (remove explicit (java-read-roots)))]

           (-> jail
               (update :allow-read into roots)
               (update :no-search into roots)
               (update :path-descriptions
                       merge
                       (zipmap roots
                               (repeat
                                 "Java runtime installation (automatic, process read-only).")))))

         ;; Per-root DRAFT isolation policy, keyed by the SAME canonical path the
         ;; filesystem grants use. Independent of `:jail-enabled`: a drafted session
         ;; isolates catalog roots whether or not the OS jail confines them.
         draft-policies
         (into {}
               (keep (fn [[path policy]]
                       (when-let [rp (nearest-real-path path base-dir home)]
                         [rp policy])))
               (config-validation/workspace-draft-policies config))

         policy
         {:jail-enabled (not= false (get-in config ["jail" "enabled"]))
          :network network
          :process-jail jail
          :draft-policies draft-policies
          :project-paths (project-paths config base-dir home)}

         generation
         (sha256 policy)]

     (assoc policy
       :generation generation
       :base-dir (str base-dir)
       :home (str home)))))

(defn draft-policies
  "Canonical `{root-path -> draft policy}` for catalog roots that opt out of the
   default `:shared` isolation. Empty for a catalog that declares no `draft` key."
  [policy]
  (or (:draft-policies policy) {}))

(defn- host-filesystem-roots
  "Canonical host filesystem roots. With the jail disabled these represent
   unrestricted filesystem access."
  []
  (->> (java.io.File/listRoots)
       (keep (fn [^java.io.File root]
               (try (.getCanonicalPath root) (catch Throwable _ nil))))
       distinct
       vec))

(defn read-write-roots
  "Filesystem roots available read/write to common model tools. When the jail is
   disabled, every host filesystem root is available; otherwise this is the
   configured allowlist."
  [policy]
  (if (:jail-enabled policy)
    (vec (distinct (get-in policy [:process-jail :allow-read-write])))
    (host-filesystem-roots)))

(defn no-search-roots
  "Roots excluded from the DEFAULT grep sweep; explicit paths still reach
   them. With the jail disabled, host filesystem roots are excluded so granting
   unrestricted explicit access does not make an unscoped grep crawl the machine."
  [policy]
  (if (:jail-enabled policy)
    (vec (get-in policy [:process-jail :no-search]))
    (host-filesystem-roots)))

(defn access-view
  "Build the string-keyed model context from the exact enforcement snapshot.
   `workspace-roots` are the live session overlay; configured grants remain
   immutable. Paths under HOME render as `~/…` without changing enforcement."
  [policy workspace-roots]
  (let [home
        (:home policy)

        jail
        (:process-jail policy)

        network
        (:network policy)

        rw
        (->> (concat workspace-roots (read-write-roots policy))
             (keep identity)
             distinct
             (mapv #(home-relative % home)))

        ro
        (->> (:allow-read jail)
             distinct
             (mapv #(home-relative % home)))

        deny-read
        (mapv #(home-relative % home) (:deny-read jail))

        deny-write
        (mapv #(home-relative % home) (:deny-write jail))

        no-search
        (mapv #(home-relative % home) (no-search-roots policy))

        descriptions
        (into {}
              (map (fn [[k v]]
                     [(home-relative k home) v]))
              (:path-descriptions jail))

        ;; Only roots that opt OUT of the default `shared` isolation are worth
        ;; naming: a drafted session either works on a private copy of them or
        ;; cannot touch them at all.
        draft
        (into {}
              (keep (fn [[k v]]
                      (let [p (name v)]
                        (when-not (= "shared" p) [(home-relative k home) p]))))
              (draft-policies policy))]

    (cond-> {"generation" (:generation policy)
             "is_jailed" (boolean (:jail-enabled policy))
             "filesystem" (cond-> {"read_write" rw
                                   "process_read_only" ro
                                   "deny_read" deny-read
                                   "deny_write" deny-write
                                   "no_search" no-search
                                   "descriptions" descriptions}
                            (seq draft)
                            (assoc "draft" draft))
             "network" {"enabled" true
                        "allowed_domains" (vec (:allowed-domains network))
                        "denied_domains" (vec (:denied-domains network))
                        "exclude_domains" (vec (:exclude-domains network))
                        "allow_private" (boolean (:allow-private network))
                        "inbound_ports" (vec (:inbound-ports jail))}
             "changes_require" "reload"}
      (:config-error policy)
      (assoc "config_error" (:config-error policy)))))
