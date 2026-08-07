(ns com.blockether.vis.internal.protected-paths
  "The ONE resolver for `:ext/protected-paths`.

   An extension DECLARES rules (`{:glob :access :hint}`) and every surface obeys
   them: the native file verbs (`foundation/editing/core`) and the Python
   sandbox's own filesystem (`sandbox-fs/confined-filesystem`). Both consult this
   namespace, so a path that is protected for `delete` is protected for
   `pathlib.Path.unlink` too.

   Precedence: first-match-wins WITHIN one extension, most-restrictive-wins
   ACROSS extensions (`:none` > `:read-only` > `:read-write`) — no extension can
   loosen another's boundary.

   A TARGET is `{:requested :resolved :absolute :kind}`, where `:resolved` is the
   workspace-relative, `/`-separated address the globs are written against."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension])
  (:import [java.nio.file FileSystems Paths]))

(def access-rank {:read-write 0 :read-only 1 :none 2})

(defn- nio-path ^java.nio.file.Path [s] (Paths/get (str s) (make-array String 0)))

(defn glob-matches?
  "Whether `glob` matches the relative path `rel` (or its last segment)."
  [glob rel]
  (let
    [matcher
     (.getPathMatcher (FileSystems/getDefault) (str "glob:" glob))

     rel
     (str/replace (str rel) (str (char 92)) "/")

     name
     (last (str/split rel #"/+"))]

    (boolean (some (fn [candidate]
                     (try (.matches matcher (nio-path candidate)) (catch Throwable _ false)))
                   (distinct [rel name])))))

(def ^:private glob-meta-chars #{\* \? \[ \] \{ \}})

(defn glob-static-prefix
  "The literal directory prefix of `glob`, before its first wildcard."
  [glob]
  (let
    [glob
     (str/replace (str glob) (str (char 92)) "/")

     idx
     (first (keep-indexed (fn [idx ch]
                            (when (contains? glob-meta-chars ch) idx))
                          glob))

     raw-prefix
     (if idx (subs glob 0 idx) glob)

     prefix
     (if (and idx (not (str/ends-with? raw-prefix "/")))
       (let [slash-idx (.lastIndexOf ^String raw-prefix "/")]
         (if (neg? slash-idx) "" (subs raw-prefix 0 slash-idx)))
       raw-prefix)

     prefix
     (str/replace prefix #"/+$" "")]

    (if (str/blank? prefix) "." prefix)))

(defn path-prefix?
  "Whether `ancestor` is `path` or one of its ancestors."
  [ancestor path]
  (let
    [ancestor
     (str/replace (str ancestor) (str (char 92)) "/")

     path
     (str/replace (str path) (str (char 92)) "/")]

    (or (= "." ancestor) (= ancestor path) (str/starts-with? path (str ancestor "/")))))

(defn composite-target?
  "Whether the target addresses a whole subtree (a directory, or a batch scope)."
  [{:keys [kind absolute]}]
  (or (= :dir kind) (and (= :path kind) absolute (.isDirectory (io/file absolute)))))

(defn rule-matches?
  [target rule]
  (or (glob-matches? (:glob rule) (:resolved target))
      (and (composite-target? target)
           (let
             [rel
              (:resolved target)

              prefix
              (glob-static-prefix (:glob rule))]

             (or (path-prefix? prefix rel)
                 (and (not= :read-write (:access rule)) (path-prefix? rel prefix)))))))

(defn- rules-by-extension
  [rules]
  (->> (map-indexed vector rules)
       (reduce (fn [groups [idx rule]]
                 (let [ext-name (:extension/name rule)]
                   (-> groups
                       (update-in [ext-name :idx] #(or % idx))
                       (update-in [ext-name :rules] (fnil conj []) rule))))
               {})
       vals
       (sort-by :idx)
       (mapv :rules)))

(defn- first-matching-rule
  [target rules]
  (some (fn [rule]
          (when (rule-matches? target rule) rule))
        rules))

(defn- more-restrictive-rule
  [best rule]
  (if (or (nil? best) (> (long (access-rank (:access rule))) (long (access-rank (:access best)))))
    rule
    best))

(defn resolve-access
  "The most restrictive rule matching `target`, or nil."
  [rules target]
  (reduce (fn [best extension-rules]
            (if-let [match (first-matching-rule target extension-rules)]
              (more-restrictive-rule best match)
              best))
          nil
          (rules-by-extension rules)))

(defn blocked-access?
  [access-intent access]
  (or (= :none access) (and (= :write access-intent) (= :read-only access))))

(defn- workspace-relative
  "`abs` addressed relative to `root`, `/`-separated, or nil when it is outside."
  [root abs]
  (let
    [root
     (str/replace (str/replace (str root) (str (char 92)) "/") #"/+$" "")

     abs
     (str/replace (str abs) (str (char 92)) "/")]

    (cond (str/blank? root) nil
          (= root abs) "."
          (str/starts-with? abs (str root "/")) (subs abs (inc (count root)))
          :else nil)))

(defn deny-fn
  "Build the predicate the Python sandbox's `confine!` consults.

   `env-thunk` yields the live environment (whose active extensions declare the
   rules); `root-thunk` yields the workspace root the globs are written against.
   Returns `(fn [operation abs-path] -> blocked rule | nil)`, where `operation`
   is the sandbox's own `file-read` / `file-write` name. Throws when the rule
   registry itself is broken — the caller fails CLOSED."
  [env-thunk root-thunk]
  (fn [operation abs-path]
    (when-let [rel (workspace-relative (root-thunk) abs-path)]
      (let
        [intent (if (str/ends-with? (str operation) "-write") :write :read)
         target {:requested (str abs-path) :resolved rel :absolute (str abs-path) :kind :path}
         rule (resolve-access (extension/active-protected-globs (env-thunk)) target)]

        (when (and rule (blocked-access? intent (:access rule))) rule)))))
