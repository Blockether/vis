(ns com.blockether.vis.ext.foundation-core.environment.digest
  "Slim per-iteration env digest for `:session/env`.

   Pi-style auto-pin: the heavy `v/snapshot` map (~1.5 KB of language
   byte counts, deps.edn listings, duplicated git info) only renders
   on explicit deep-dive. The model reads `:session/env` instead for
   the 80% case: cwd, OS, primary language, monorepo shape, active
   extension aliases.

   Each top-level slice (`:host`, `:project`, `:extensions`) is its
   own bounded map. Other extensions extend the section via their
   own `:ext/ctx` returning `{:session/env {their-key {…}}}`; deep
   merge in `extension/ctx-contributions` keeps everyone honest.

   Cheap: reads from the existing cached `snapshot` and the runtime
   active-extensions vec. Never throws; on failure returns nil so
   `combined-ctx` simply skips the section."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.environment.agents :as agents]
   [com.blockether.vis.ext.foundation-core.environment.core :as env]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.prompt :as prompt]))

(defn- normalize-os
  "Lift host/os-name into a stable keyword the model can dispatch on
   without parsing platform-specific banner strings."
  [^String os-name]
  (let [s (some-> os-name str/lower-case)]
    (cond
      (nil? s)                       :unknown
      (str/includes? s "mac")        :macos
      (str/includes? s "darwin")     :macos
      (str/includes? s "linux")      :linux
      (str/includes? s "windows")    :windows
      (str/includes? s "bsd")        :bsd
      :else                          :unknown)))

(defn- normalize-shell
  "Reduce $SHELL to a bare keyword. Unknown / empty shells fall back
   to :unknown so downstream pattern-matches never NPE."
  [^String shell]
  (let [base (some-> shell str/lower-case
               (str/split #"/")
               last
               not-empty)]
    (case base
      "zsh"    :zsh
      "bash"   :bash
      "fish"   :fish
      "sh"     :sh
      "pwsh"   :pwsh
      "cmd"    :cmd
      "powershell" :powershell
      (when base (keyword base)))))

(defn- host-digest
  "Reduce host snapshot to 4 stable keys. Time is ISO-8601 so the model
   can compare clock drift without re-parsing the long format."
  [{:keys [host]}]
  (when (map? host)
    (cond-> {:cwd   (:cwd host)
             :os    (normalize-os (:os-name host))
             :shell (normalize-shell (:shell host))}
      (:time host) (assoc :clock (:time host)))))

(defn- language-share
  "Project the languages list into a {keyword fraction} map. Caps at
   the 5 dominant languages and rounds fractions to 0.01 so the map
   stays under ~80 bytes regardless of repo size."
  [{:keys [languages]}]
  (when (and (map? languages) (pos? (long (or (:total-bytes languages) 0))))
    (let [total (double (:total-bytes languages))
          rows  (take 5 (:languages languages))]
      (->> rows
        (keep (fn [{:keys [language bytes]}]
                (when (and language (pos? (long (or bytes 0))))
                  [(keyword (str/lower-case (str language)))
                   (-> (/ (double bytes) total)
                     (* 100.0)
                     Math/round
                     (/ 100.0)
                     double)])))
        (into {})))))

(defn- project-digest
  "Reduce project-shape snapshot to 4 keys + AGENTS.md flag.

   `:kind` is one of :polylith / :monorepo / :single (derived from
   monorepo shape). `:primary-language` is keywordised from the
   language scan. `:extension-count` is the polylith brick count
   when applicable, nil otherwise."
  [snapshot agents-found?]
  (let [mono   (:monorepo snapshot)
        langs  (:languages snapshot)
        kind   (cond
                 (= (:shape mono) :polylith) :polylith
                 (some? (:shape mono))       :monorepo
                 :else                       :single)
        primary (some-> (:primary langs) str/lower-case keyword)
        shares  (language-share snapshot)
        brick-n (when (= kind :polylith)
                  (get-in mono [:totals :bricks]))]
    (cond-> {:kind kind :agents-md? (boolean agents-found?)}
      primary  (assoc :primary-language primary)
      shares   (assoc :language-share shares)
      brick-n  (assoc :extension-count (long brick-n)))))

(defn- extensions-digest
  "Slim model-facing summary of active vis extensions.

   `:active-count` is the count of currently-active extensions
   (`:ext/activation-fn` truthy). `:aliases` is a set of SCI aliases
   the model can actually call. Extensions without an alias are
   silently skipped from the set; their count still bumps total."
  [environment]
  (let [active (try (or (prompt/active-extensions environment) [])
                 (catch Throwable _ []))
        aliases (into (sorted-set) (keep extension/ext-alias-symbol) active)]
    {:active-count (count active)
     :aliases      aliases}))

(defn digest
  "Compose the full `:session/env` value: `{:host {…} :project {…}
   :extensions {…}}`. Each slice falls back to nil; the merge below
   drops nil sections so the model never sees an empty subtree.

   No-arg arity is used by callers (and tests) that want the digest
   without an environment (extensions slice is empty)."
  ([] (digest nil))
  ([environment]
   (let [snap    (try (env/snapshot) (catch Throwable _ nil))
         agents? (try (boolean (:found? (agents/instructions)))
                   (catch Throwable _ false))
         host    (host-digest snap)
         project (project-digest snap agents?)
         exts    (when environment (extensions-digest environment))]
     (cond-> {}
       host    (assoc :host host)
       project (assoc :project project)
       exts    (assoc :extensions exts)))))
