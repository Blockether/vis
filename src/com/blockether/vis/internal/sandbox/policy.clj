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
           [java.nio.file DirectoryStream FileSystems Files LinkOption Path PathMatcher Paths]
           [java.util ArrayList Collection]
           [java.util.concurrent ConcurrentHashMap ConcurrentLinkedQueue ForkJoinPool
            RecursiveAction]
           [java.util.concurrent.atomic AtomicBoolean AtomicReference]
           [java.util.function Function]))

(def ^:private no-link-options (make-array LinkOption 0))

(def ^:private nofollow-options (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

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

(defn extension-grant
  "Check one filesystem grant from an extension declaration and return
   `{:path :mode :reason}`, or throw. `grant` has the wire keys `path`, `mode` and
   `reason`. The path must be absolute after `~` expansion. The checks use its
   resolved form, but the grant keeps the path as declared, so the jail can follow
   its links. A grant must not cover the filesystem root, the home directory or
   `vis-home`: these hold the user's files and the Vis credentials."
  [ext-name grant {:keys [home vis-home]}]
  (let [{:strs [path mode reason]}
        grant

        refuse
        (fn [why]
          (throw (ex-info (str "Extension '" ext-name "' grant " (pr-str path) " " why ".")
                          {:type ::bad-grant :extension ext-name :path path})))]

    (when-not (util/non-blank-string? path) (refuse "needs a path"))
    (when-not (#{"read" "read_write"} mode) (refuse "needs mode \"read\" or \"read_write\""))
    (when-not (util/non-blank-string? reason) (refuse "needs a reason"))
    (let [^Path declared (Paths/get (paths/expand-home path home) (make-array String 0))]
      (when-not (.isAbsolute declared) (refuse "must be absolute or start with ~"))
      (let [^Path resolved (Paths/get (nearest-real-path (str declared) "/" home)
                                      (make-array String 0))
            covers? (fn [protected]
                      (when-not (str/blank? (str protected))
                        (.startsWith ^Path
                                     (Paths/get (nearest-real-path protected "/" home)
                                                (make-array String 0))
                                     resolved)))]

        (when (or (nil? (.getParent resolved)) (covers? home) (covers? vis-home))
          (refuse "covers the filesystem root, the home directory or the Vis home"))
        {:path (str (.normalize declared))
         :mode (if (= "read" mode) :read :read-write)
         :reason (str/trim reason)}))))

(defn- resolve-paths
  [paths base-dir home]
  (vec (distinct (keep #(nearest-real-path % base-dir home) paths))))

(def ^:private glob-characters "What makes a deny rule a PATTERN rather than one path." #"[*?\[{]")

(defn- glob-split
  "An absolute glob pattern as `[literal-ancestor-directory remainder]`."
  [pattern]
  (let [segments
        (str/split pattern #"/")

        literal
        (take-while #(not (re-find glob-characters %)) segments)

        root
        (str/join "/" literal)]

    [(if (str/blank? root) "/" root) (str/join "/" (drop (count literal) segments))]))

(def ^:private reliable-link-stores
  "File systems where a directory has 2 links plus one link for each subdirectory."
  #{"ext2" "ext3" "ext4" "xfs" "tmpfs"})

(defn- link-count-reliable?
  "True when the file system of `directory` counts its subdirectories in its link count."
  [^Path directory]
  (try (contains? reliable-link-stores (.type (Files/getFileStore directory)))
       (catch Exception _ false)))

(defn- subdirectory-count
  "The number of subdirectories of the directory `path`, or -1 when its file system does not
   give a reliable count. `reliable` caches the answer for each device. Nil when `path` is not
   a directory, or is a link."
  [^Path path ^ConcurrentHashMap reliable]
  (try (let [^java.util.Map attributes (Files/readAttributes path
                                                             "unix:mode,nlink,dev"
                                                             ^"[Ljava.nio.file.LinkOption;"
                                                             nofollow-options)]
         (when (= 0040000 (bit-and (long (.get attributes "mode")) 0170000))
           (let [links (long (.get attributes "nlink"))]
             (if (and (>= links 2)
                      (.computeIfAbsent reliable
                                        (.get attributes "dev")
                                        (reify
                                          Function
                                            (apply [_ _] (link-count-reliable? path)))))
               (- links 2)
               -1))))
       (catch UnsupportedOperationException _ (when (Files/isDirectory path nofollow-options) -1))
       (catch IllegalArgumentException _ (when (Files/isDirectory path nofollow-options) -1))
       (catch Exception _ nil)))

(defn- walk-entries
  "Call `visit` on each entry below `start`, down to `depth` levels, with parallel directory
   reads. The walk stops when `visit` returns a true value. Links are not followed and an
   unreadable directory is skipped. A file system with reliable link counts tells how many
   subdirectories a directory has. After the last one, the other entries need no status
   call. This halves a cold walk on Linux (#396)."
  [^Path start depth visit]
  (let [depth
        (long depth)

        stop
        (AtomicBoolean.)

        reliable
        (ConcurrentHashMap.)

        pool
        (ForkJoinPool.)

        task
        (fn task [^Path dir ^long level ^long subdirectories]
          (proxy [RecursiveAction] []
            (compute []
              (let [children (ArrayList.)]
                (when-not (.get stop)
                  (try (with-open [^DirectoryStream entries (Files/newDirectoryStream dir)]
                         (let [iterator (.iterator entries)]
                           (loop [remaining subdirectories]
                             (when (and (not (.get stop)) (.hasNext iterator))
                               (let [^Path entry (.next iterator)]
                                 (when (visit entry) (.set stop true))
                                 (if-let [links (when (and (< level depth) (not (zero? remaining)))
                                                  (subdirectory-count entry reliable))]
                                   (do (.add children (task entry (inc level) links))
                                       (recur (if (pos? remaining) (dec remaining) remaining)))
                                   (recur remaining)))))))
                       (catch Exception _ nil)))
                (RecursiveAction/invokeAll ^Collection children)))))]

    (try (.invoke pool ^RecursiveAction (task start 1 (or (subdirectory-count start reliable) -1)))
         (finally (.shutdown pool)))))

(defn- glob? [rule] (boolean (re-find glob-characters rule)))

(defn- glob-matches
  "`{pattern [path …]}`, sorted, for the absolute glob `patterns` that share the literal
   ancestor `root`, from ONE walk. Hidden files count. The walk stays at the top level when
   no pattern can cross a directory."
  [^String root patterns]
  (let [matchers
        (into []
              (keep (fn [pattern]
                      (when-let [matcher (try (.getPathMatcher (FileSystems/getDefault)
                                                               (str "glob:" pattern))
                                              (catch Throwable _ nil))]
                        [pattern matcher])))
              patterns)

        depth
        (if (some #(let [remainder (second (glob-split %))] (or (str/includes? remainder "/")
                                                                (str/includes? remainder "**")))
                  patterns)
          Long/MAX_VALUE
          1)

        ^Path start
        (Paths/get root (make-array String 0))

        found
        (into {}
              (map (fn [[pattern]]
                     [pattern (ConcurrentLinkedQueue.)]))
              matchers)]

    (when (and (seq matchers) (Files/isDirectory start no-link-options))
      (walk-entries start
                    depth
                    (fn [^Path path]
                      (doseq [[pattern ^PathMatcher matcher] matchers]
                        (when (.matches matcher path)
                          (.add ^ConcurrentLinkedQueue (found pattern) (.toString path)))))))
    (update-vals found #(vec (sort %)))))

(defn- expand-deny-lists
  "Expand each list of `lists`, `{key [pattern …]}`, to concrete paths. A literal rule is
   denied by SPELLING, whether or not that file exists yet. A glob names nothing that the
   kernel understands, so it expands to the files present now. Globs with the same literal
   ancestor share one walk, in all lists. Hidden files are included, because `.env` is the point."
  [lists base-dir home]
  (let [hits (into {}
                   (mapcat (fn [[root globs]]
                             (glob-matches root globs)))
                   (group-by (comp first glob-split) (distinct (filter glob? (mapcat val lists)))))]
    (update-vals lists
                 (fn [patterns]
                   (vec (distinct (mapcat (fn [pattern]
                                            (if (glob? pattern)
                                              (keep #(nearest-real-path % base-dir home)
                                                    (get hits pattern))
                                              [pattern]))
                                          patterns)))))))

(defn- literal-rules [patterns] (vec (distinct (remove glob? patterns))))

(defn- glob-matcher
  ^PathMatcher [pattern]
  (try (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern)) (catch Throwable _ nil)))

(defn- crosses-directories?
  [pattern]
  (let [remainder (second (glob-split pattern))]
    (or (str/includes? remainder "/") (str/includes? remainder "**"))))

(defn deny-overlap
  "The first path that a rule of `patterns` covers and that is `root`, an ancestor of `root` or a
   path below it, else nil. Literal rules and ancestors need no walk. Below `root`, a glob walks
   only its part of the tree inside `root`, and the walk stops at the first hit (#396)."
  [patterns root]
  (let [^Path selected
        (Paths/get (str root) (make-array String 0))

        under?
        (fn [^Path path ^Path parent]
          (.startsWith path parent))

        globs
        (keep (fn [pattern]
                (when-let [matcher (glob-matcher pattern)]
                  [pattern matcher]))
              (filter glob? patterns))

        walks
        (reduce (fn [walks [pattern matcher]]
                  (let [literal
                        (Paths/get ^String (first (glob-split pattern)) (make-array String 0))

                        crosses?
                        (crosses-directories? pattern)

                        start
                        (cond (under? literal selected) literal
                              (and crosses? (under? selected literal)) selected)]

                    (cond-> walks
                      start
                      (update start
                              (fn [[depth matchers]]
                                [(max (long (or depth 0)) (if crosses? Long/MAX_VALUE 1))
                                 (conj (or matchers []) matcher)])))))
                {}
                globs)]

    (or (some (fn [rule]
                (let [denied (Paths/get ^String rule (make-array String 0))]
                  (when (or (under? selected denied) (under? denied selected)) rule)))
              (literal-rules patterns))
        (some (fn [[_ ^PathMatcher matcher]]
                (loop [^Path candidate selected]
                  (cond (nil? candidate) nil
                        (.matches matcher candidate) (str candidate)
                        :else (recur (.getParent candidate)))))
              globs)
        (some (fn [[start [depth matchers]]]
                (let [found (AtomicReference.)]
                  (walk-entries start
                                depth
                                (fn [^Path path]
                                  (when (some #(.matches ^PathMatcher % path) matchers)
                                    (.compareAndSet found nil (str path)))))
                  (.get found)))
              walks))))

(defn- os-binds-deny-paths?
  "True when the OS jail of this host denies only concrete paths. The Linux jail binds
   paths. The macOS jail compiles each pattern itself."
  []
  (str/starts-with? (str/lower-case (str (System/getProperty "os.name"))) "linux"))

(def ^:private deny-walk-refresh-nanos
  "A shared deny walk older than this starts a new one in the background."
  (* 5 1000 1000 1000))

;; Shared deny walks, by deny lists: {lists {:paths … :started nanos :pending future}}.
(defonce ^:private deny-walks (atom {}))

(defn- deny-walk!
  "The shared walk entry of `lists`. Start a background walk when none runs and the last
   one started more than [[deny-walk-refresh-nanos]] ago."
  [lists]
  (locking deny-walks
    (let [{:keys [pending started] :as entry} (get @deny-walks lists)]
      (if (or pending
              (and started (< (- (System/nanoTime) (long started)) (long deny-walk-refresh-nanos))))
        entry
        (let [now (System/nanoTime)
              walk (future (try (let [paths (expand-deny-lists lists
                                                               (System/getProperty "user.dir")
                                                               (System/getProperty "user.home"))]
                                  (locking deny-walks
                                    (swap! deny-walks assoc lists {:paths paths :started now}))
                                  paths)
                                (catch Throwable t
                                  (locking deny-walks
                                    (swap! deny-walks update lists dissoc :pending))
                                  (throw t))))]

          (get (swap! deny-walks assoc-in [lists :pending] walk) lists))))))

(defn jail-deny-paths
  "Concrete paths for each list of `lists`, `{key [pattern …]}`, when the OS jail of this
   host denies only concrete paths, else nil. Sessions with the same rules share one walk. A
   walk older than 5 s is renewed in the background, so only the first call waits (#396)."
  [lists]
  (when (and (os-binds-deny-paths?) (some glob? (mapcat val lists)))
    (let [{:keys [paths pending]} (deny-walk! lists)]
      (or paths @pending))))

(defn- warm-jail-deny-paths!
  "Start the walk of [[jail-deny-paths]] for `lists` in the background, without a wait."
  [lists]
  (when (and (os-binds-deny-paths?) (some glob? (mapcat val lists))) (deny-walk! lists))
  nil)

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

         ;; A deny rule stays a PATTERN for the host tools, the OS jail and the diagnostics
         ;; view, which match it live. The snapshot never walks the tree (#396). A jail that
         ;; binds paths expands the globs at spawn, from one shared background walk.
         jail
         (-> jail
             (assoc :deny-read-rules (:deny-read jail)
                    :deny-write-rules (:deny-write jail))
             (update :deny-read literal-rules)
             (update :deny-write literal-rules))

         _
         (when-not (:disabled? jail)
           (warm-jail-deny-paths! {:deny-read (:deny-read-rules jail)
                                   :deny-write (:deny-write-rules jail)}))

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
         {:jail-enabled (true? (get-in config ["jail" "enabled"]))
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

(defn resolve-rules
  "Resolve path rules to canonical absolute patterns, like the configured deny rules. A
   relative rule resolves against `base-dir`; a missing tail stays, so a rule can name a
   file that does not exist yet."
  [rules base-dir home]
  (resolve-paths rules base-dir home))

(defn rule-root
  "The directory that an OS sandbox can grant for one write rule: a literal rule itself, or
   the literal ancestor directory of a glob."
  [rule]
  (if (re-find glob-characters rule) (first (glob-split rule)) rule))

(defn- add-rules
  [jail rules-key paths-key rules]
  (cond-> jail
    (seq rules)
    (-> (update rules-key #(vec (distinct (concat % rules))))
        (update paths-key #(vec (distinct (concat % (literal-rules rules))))))))

(defn outside-write-rules
  "The resolved write rules that `policy` cannot grant: with the jail on, a rule whose
   directory lies outside every writable root of `policy` and `extra-roots`. A disabled jail
   writes everywhere that no deny rule closes."
  [policy rules extra-roots]
  (let [ceiling
        (keep identity (concat (get-in policy [:process-jail :allow-read-write]) extra-roots))

        inside?
        (fn [rule]
          (let [path (Paths/get ^String (rule-root rule) (make-array String 0))]
            (some #(.startsWith path (Paths/get (str %) (make-array String 0))) ceiling)))]

    (if (:jail-enabled policy) (vec (remove inside? rules)) [])))

(defn restrict
  "Narrow `policy`, the parent's policy, with a subagent file grant. `files` holds the
   `:write`, `:read_only` and `:deny` rules. Each rule also covers files that are created
   later. `:write` limits writes to its rules: other writable roots stay readable only. A
   write rule that `policy` cannot grant grants nothing."
  [policy {:keys [write read_only deny]}]
  (if-not (or (seq write) (seq read_only) (seq deny))
    policy
    (let [base-dir
          (:base-dir policy)

          home
          (:home policy)

          requested
          (resolve-rules write base-dir home)

          write
          (vec (remove (set (outside-write-rules policy requested [base-dir])) requested))

          jail
          (-> (:process-jail policy)
              (add-rules :deny-read-rules :deny-read (resolve-rules deny base-dir home))
              (add-rules :deny-write-rules :deny-write (resolve-rules read_only base-dir home)))

          jail
          (cond-> jail
            (seq requested)
            (assoc :write-limited?
              true :write-rules
              write :allow-read
              (vec (distinct (concat (:allow-read jail) (:allow-read-write jail))))
              :allow-read-write
              (vec (distinct (map rule-root write)))))

          narrowed
          (assoc policy :process-jail jail)]

      (assoc narrowed :generation (sha256 (dissoc narrowed :generation :base-dir :home))))))

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
  "Filesystem roots available read/write to common model tools. With the jail
   enabled this is the configured allowlist. A disabled jail adds every host
   filesystem root, but the configured catalog keeps leading it: a named project
   root stays a root of its own, with its guidance and its default search scope."
  [policy]
  (let [configured (distinct (get-in policy [:process-jail :allow-read-write]))]
    (if (:jail-enabled policy)
      (vec configured)
      (vec (distinct (concat configured (host-filesystem-roots)))))))

(defn no-search-roots
  "Roots excluded from the DEFAULT grep sweep; explicit paths still reach
   them. Catalog `search: false` roots stay excluded with the jail on or off. With
   the jail disabled, host filesystem roots are also excluded so granting
   unrestricted explicit access does not make an unscoped grep crawl the machine."
  [policy]
  (let [configured (get-in policy [:process-jail :no-search])]
    (if (:jail-enabled policy)
      (vec configured)
      (vec (distinct (concat configured (host-filesystem-roots)))))))

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

        ;; A subagent file grant writes only its rules; its other roots stay readable.
        write-rules
        (some->> (seq (:write-rules jail))
                 (mapv #(home-relative % home)))

        ro
        (->> (:allow-read jail)
             distinct
             (mapv #(home-relative % home)))

        deny-read
        (mapv #(home-relative % home) (or (:deny-read-rules jail) (:deny-read jail)))

        deny-write
        (mapv #(home-relative % home) (or (:deny-write-rules jail) (:deny-write jail)))

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
             "filesystem" (cond-> {"read_write" (or write-rules rw)
                                   "process_read_only" ro
                                   "deny_read" deny-read
                                   "deny_write" deny-write
                                   "no_search" no-search
                                   "descriptions" descriptions}
                            write-rules
                            (assoc "read_only" rw)

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
