(ns com.blockether.vis.internal.security-policy
  "Canonical immutable security-policy snapshots and their model-facing view.

   A snapshot is created once for a root environment, inherited unchanged by
   child environments, and replaced only by an explicit environment rebuild.
   Enforcement and context both derive from this value."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config-spec :as config-spec])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path Paths]
           [java.security MessageDigest]))

(def ^:private no-link-options (make-array LinkOption 0))

(defn- expand-home
  [path home]
  (let [s (str path)]
    (cond (= s "~") home
          (str/starts-with? s "~/") (str home File/separator (subs s 2))
          :else s)))

(defn- nearest-real-path
  "Resolve a configured path against `base-dir`, resolving every existing
   ancestor and preserving a missing tail. This snapshots symlink identity while
   still allowing a configured directory to be created after startup."
  [path base-dir home]
  (when-not (str/blank? (str path))
    (let
      [expanded
       (expand-home path home)

       ^Path raw
       (Paths/get expanded (make-array String 0))

       ^Path absolute
       (.normalize (if (.isAbsolute raw)
                     raw
                     (.resolve (Paths/get (str base-dir) (make-array String 0)) raw)))]

      (loop
        [^Path ancestor
         absolute

         tail
         ()]

        (cond (nil? ancestor) (.toString absolute)
              (Files/exists ancestor no-link-options)
              (let
                [real (try (.toRealPath ancestor no-link-options)
                           (catch Throwable _ (.toAbsolutePath ancestor)))]
                (.toString (.normalize (reduce (fn [^Path p ^String segment]
                                                 (.resolve p segment))
                                               real
                                               tail))))
              :else (recur (.getParent ancestor) (cons (str (.getFileName ancestor)) tail)))))))

(defn home-relative
  "Render an absolute path under HOME as `~` / `~/…`; leave other paths absolute."
  ([path] (home-relative path (System/getProperty "user.home")))
  ([path home]
   (let
     [path
      (some-> path
              str
              not-empty)

      home
      (some-> home
              str
              not-empty)]

     (if-not (and path home)
       path
       (try (let
              [^Path p
               (.normalize (.toAbsolutePath (Paths/get path (make-array String 0))))

               ^Path h
               (.normalize (.toAbsolutePath (Paths/get home (make-array String 0))))]

              (cond (= p h) "~"
                    (.startsWith p h)
                    (str "~/" (str/replace (.toString (.relativize h p)) File/separator "/"))
                    :else path))
            (catch Throwable _ path))))))

(defn- resolve-paths
  [paths base-dir home]
  (vec (distinct (keep #(nearest-real-path % base-dir home) paths))))

(defn- resolve-cache-entry
  [entry base-dir home]
  (cond (string? entry) (nearest-real-path entry base-dir home)
        (map? entry)
        (when-let [path (nearest-real-path (or (:path entry) (get entry "path")) base-dir home)]
          (cond-> {:path path}
            (or (:access entry) (get entry "access"))
            (assoc :access (or (:access entry) (get entry "access")))))
        :else nil))

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

(defn- sha256
  [value]
  (let
    [digest (.digest (MessageDigest/getInstance "SHA-256")
                     (.getBytes (pr-str (stable-value value)) StandardCharsets/UTF_8))]
    (str "sha256:" (apply str (map #(format "%02x" (bit-and 0xff %)) digest)))))

(defn snapshot
  "Build the immutable canonical security policy from validated string-keyed
   configuration. Relative and home-relative paths become absolute; symlinks are
   resolved at this boundary."
  ([config] (snapshot config {}))
  ([config
    {:keys [base-dir home]
     :or {base-dir (System/getProperty "user.dir") home (System/getProperty "user.home")}}]
   (config-spec/assert-config! config)
   (let
     [jail
      (config-spec/process-jail-config config)

      network
      (config-spec/network-config config)

      path-keys
      [:allow-read-write :allow-read :allow-write :deny-read :deny-write]

      jail
      (reduce (fn [policy key]
                (update policy key resolve-paths base-dir home))
              jail
              path-keys)

      jail
      (update jail
              :language-cache-dirs
              #(into []
                     (keep (fn [entry]
                             (resolve-cache-entry entry base-dir home)))
                     %))

      jail
      (update jail
              :path-descriptions
              (fn [m]
                (into {}
                      (keep (fn [[k v]]
                              (when-let [rp (nearest-real-path k base-dir home)]
                                [rp v])))
                      m)))

      policy
      {:sandbox (not= false (get config "sandbox")) :network network :process-jail jail}

      generation
      (sha256 policy)]

     (assoc policy
       :generation generation
       :base-dir (str base-dir)
       :home (str home)))))

(defn read-write-roots
  "Configured roots available read/write to common model tools. `allow-write`
   remains readable under the process-jail contract, so it belongs here too."
  [policy]
  (vec (distinct (concat (get-in policy [:process-jail :allow-read-write])
                         (get-in policy [:process-jail :allow-write])))))

(defn- cache-view
  [entry home]
  (if (string? entry)
    {"path" (home-relative entry home) "access" "read_write"}
    {"path" (home-relative (:path entry) home)
     "access" (if (contains? #{"read-only" "readonly" "ro"}
                             (some-> (:access entry)
                                     name
                                     str/lower-case))
                "read_only"
                "read_write")}))

(defn access-view
  "Build the string-keyed model context from the exact enforcement snapshot.
   `workspace-roots` are the live session overlay; configured grants remain
   immutable. Paths under HOME render as `~/…` without changing enforcement."
  [policy workspace-roots]
  (let
    [home
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

     caches
     (mapv #(cache-view % home) (:language-cache-dirs jail))

     descriptions
     (into {}
           (map (fn [[k v]] [(home-relative k home) v]))
           (:path-descriptions jail))]

    {"generation" (:generation policy)
     "sandboxed" (boolean (:sandbox policy))
     "filesystem" {"read_write" rw
                   "process_read_only" ro
                   "deny_read" deny-read
                   "deny_write" deny-write
                   "process_only" {"language_caches" caches}
                   "descriptions" descriptions}
     "network" {"enabled" true
                "allowed_domains" (vec (:allowed-domains network))
                "denied_domains" (vec (:denied-domains network))
                "exclude_domains" (vec (:exclude-domains network))
                "allow_private" (boolean (:allow-private network))
                "inbound_ports" (vec (:inbound-ports jail))}
     "changes_require" "reload"}))
