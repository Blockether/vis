(ns com.blockether.vis.channels.core
  "Cross-channel shared functions.

   This namespace provides the shared provider/config management layer
   used by TUI, web, CLI, and Telegram channels. Single source of truth
   for provider state — changes here are reflected everywhere."
  (:require [clojure.string :as str]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.extension :as ext]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core]
            [taoensso.telemere :as tel]))

;;; ── Provider state ─────────────────────────────────────────────────────────
;;
;; The active config is always in ~/.vis/config.edn (persisted) and mirrored
;; in this atom (in-memory for fast reads). Every mutation goes through
;; set-provider! which writes to disk AND updates the atom.

(defonce ^:private active-config (atom nil))

(defn current-config
  "Return the current provider config. Loads from disk on first call."
  []
  (or @active-config
    (let [cfg (config/load-config)]
      (reset! active-config cfg)
      cfg)))

(defn set-provider!
  "Set the single active provider config. Persists to disk and updates in-memory state.
   `provider` is a svar-native provider map {:id :base-url :api-key :models [...]}.
   Replaces any existing provider with the same :id, or adds if new."
  [provider]
  (let [cfg   (or (current-config) {:providers []})
        pid   (:id provider)
        provs (vec (:providers cfg))
        ;; Replace existing or append
        updated (let [idx (some (fn [[i p]] (when (= (:id p) pid) i))
                        (map-indexed vector provs))]
                  (if idx
                    (assoc provs idx provider)
                    (conj provs provider)))
        new-cfg {:providers updated}]
    (config/save-config! new-cfg)
    (reset! active-config new-cfg)
    ;; Rebuild the global router AND reseat it on every cached env.
    ;; Skipping the second step lets long-lived envs (TUI session,
    ;; Telegram bot) keep talking to the previous model even though
    ;; the singleton already advertises the new one.
    (try (let [r (query-core/rebuild-router! new-cfg)]
           (conversations/refresh-cached-routers! r))
      (catch Exception e
        (tel/log! {:level :warn :data {:error (ex-message e)}}
          "Failed to rebuild router after provider change")))
    new-cfg))

(defn remove-provider!
  "Remove a provider by :id. Persists to disk."
  [provider-id]
  (let [cfg     (or (current-config) {:providers []})
        updated (vec (remove #(= (:id %) provider-id) (:providers cfg)))
        new-cfg {:providers updated}]
    (config/save-config! new-cfg)
    (reset! active-config new-cfg)
    new-cfg))

(defn active-provider
  "Return the first (primary) provider from config, or nil."
  []
  (first (:providers (current-config))))

(defn active-model
  "Return the primary model name string, or nil."
  []
  (some-> (active-provider) :models first config/model-name))

(defn provider-ids
  "Return set of configured provider :id keywords."
  []
  (into #{} (map :id) (:providers (or (current-config) {:providers []}))))

(defn has-provider?
  "True if a provider with the given :id is already configured."
  [provider-id]
  (contains? (provider-ids) provider-id))

(defn reload-config!
  "Force reload config from disk."
  []
  (reset! active-config (config/load-config)))

;;; ── Lifecycle helpers ──────────────────────────────────────────────────────

(defn register-conversation-shutdown-hook!
  "Register a JVM shutdown hook that prints the conversation resume command.
   Safe to call multiple times — each call replaces the previous hook."
  [conversation-id]
  (let [hook (Thread. (fn []
                        (let [out config/original-stdout]
                          (.println out "")
                          (.println out (str "  vis chat --conversation-id " conversation-id))
                          (.println out "")
                          (.flush out))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

;;; ── Streaming helpers ─────────────────────────────────────────────────────
;;
;; Reusable across TUI, web, Telegram, CLI.
;; The on-chunk callback receives streaming chunks from svar's ask!:
;;   {:iteration N :thinking str :code [str] :done? bool}
;;
;; Thinking (reasoning) streams live as the LLM thinks.
;; Code/expressions arrive after the LLM finishes its response.
;; :done? true marks the final chunk of an iteration.

(defn make-progress-tracker
  "Create a progress tracker for streaming iteration chunks.
   Returns {:on-chunk fn, :get-timeline fn}.

   `on-update` is called (on-update timeline chunk) on every chunk.
   Timeline is a vec of chunk maps, deduplicated by iteration."
  ([] (make-progress-tracker nil))
  ([{:keys [on-update]}]
   (let [timeline (atom {})  ;; iteration-num → latest chunk
         as-vec   #(mapv val (sort-by key %))]
     {:on-chunk  (fn [chunk]
                  (let [iter (:iteration chunk)
                        tl   (swap! timeline assoc iter chunk)]
                    (when on-update
                      (on-update (as-vec tl) chunk))))
      :get-timeline #(as-vec @timeline)})))

(defn format-date
  "Format a java.util.Date as dd-MM-yyyy HH:mm in local timezone."
  [^java.util.Date d]
  (when d
    (.format (doto (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm")
               (.setTimeZone (java.util.TimeZone/getDefault)))
      d)))

(defn format-clojure
  "Pretty-print a Clojure code string using zprint.
   Falls back to the original string on any error."
  [code-str width]
  (try
    (let [formatted ((requiring-resolve 'zprint.core/zprint-str)
                      code-str width {:parse-string? true
                                      :style :community})]
      (if (clojure.string/blank? formatted) code-str (clojure.string/trimr formatted)))
    (catch Exception _ code-str)))

(defn format-duration
  "Format millisecond duration as human-readable. e.g. '2.3s', '1m 15s'.

  Always uses `Locale/US` so the decimal separator is a dot regardless
  of the JVM default locale (otherwise pl_PL etc. would print '7,3s').
  Coerces `ms` to a long up-front because callers routinely pass a
  double (svar's `elapsed-since` divides by `1e6`), and `(quot 67000.0
  60000)` is `1.0` — which would render as '1.0m 7.0s' instead of
  '1m 7s'."
  [ms]
  (when (and ms (pos? ms))
    (let [ms (long ms)]
      (cond
        (< ms 1000)  (str ms "ms")
        (< ms 60000) (String/format java.util.Locale/US "%.1fs"
                       (into-array Object [(double (/ ms 1000.0))]))
        :else        (let [m (quot ms 60000)
                           s (quot (mod ms 60000) 1000)]
                       (str m "m " s "s"))))))

;;; ── Extension CLI ─────────────────────────────────────────────────────────
;;
;; Extensions export CLI commands via :ext/cli.
;;
;; Command spec:
;;   {:cmd  "name"
;;    :doc  "Short description"
;;    :args [{:name "path" :kind :positional :type :string :required true  :doc "Input file"}
;;           {:name "count" :kind :positional :type :int   :required false :doc "How many"}
;;           {:name "--verbose" :kind :flag :type :boolean :doc "Verbose output"}
;;           {:name "--output"  :kind :flag :type :string  :doc "Output path"}]
;;    :fn   (fn [parsed-args] ...)}
;;
;; :kind :positional — matched by order, no -- prefix
;; :kind :flag       — matched by --name, boolean flags need no value
;;
;; `vis ext help` shows all commands.
;; `vis ext <cmd> --help` shows command-specific help with args.
;; `vis ext <cmd> [args...]` validates then dispatches.

(defn list-extensions
  "Return all registered extensions with their metadata."
  []
  (mapv (fn [e]
          {:namespace (str (:ext/namespace e))
           :doc       (:ext/doc e)
           :group     (:ext/group e)
           :version   (or (:ext/version e) "—")
           :cli-cmds  (str/join ", " (map :cmd (or (:ext/cli e) [])))})
    (ext/registered-extensions)))

(defn find-extension-cmd
  "Find an extension CLI command by name. Returns {:ext ext :cmd cmd-map} or nil."
  [cmd-name]
  (some (fn [e]
          (some (fn [cmd]
                  (when (= (:cmd cmd) cmd-name)
                    {:ext e :cmd cmd}))
            (:ext/cli e)))
    (ext/registered-extensions)))

(defn all-extension-cmds
  "Return a flat vec of {:cmd :doc :ext-ns :args} for every registered extension CLI command."
  []
  (into []
    (mapcat (fn [e]
              (map (fn [c] (assoc c :ext-ns (str (:ext/namespace e))))
                (or (:ext/cli e) []))))
    (ext/registered-extensions)))

;; ── Arg parsing & validation ───────────────────────────────────────────

(defn- flag-arg? [s] (str/starts-with? (str s) "--"))

(defn- coerce-arg [value type]
  (case (or type :string)
    :string  value
    :int     (if-let [n (parse-long value)] n
               (throw (ex-info (str "Expected integer, got: " value) {:value value})))
    :boolean (contains? #{"true" "1" "yes"} (str/lower-case (str value)))
    :file    value
    value))

(defn parse-ext-args
  "Parse CLI args against an arg spec. Returns a map of {arg-name value}.

   :kind :positional args are matched in declaration order.
   :kind :flag args are matched by --name. Boolean flags need no value."
  [arg-specs raw-args]
  (let [positional (vec (filter #(= :positional (:kind %)) arg-specs))
        flags      (into {} (map (fn [a] [(:name a) a]))
                     (filter #(= :flag (:kind %)) arg-specs))]
    (loop [args     (seq raw-args)
           pos-idx  0
           result   {}]
      (if-not args
        result
        (let [arg  (first args)
              more (next args)]
          (if (flag-arg? arg)
            ;; Flag
            (if-let [spec (get flags arg)]
              (if (= :boolean (:type spec))
                (recur more pos-idx (assoc result (:name spec) true))
                (recur (next more) pos-idx
                  (assoc result (:name spec) (coerce-arg (first more) (:type spec)))))
              (recur more pos-idx result))
            ;; Positional
            (if (< pos-idx (count positional))
              (let [spec (nth positional pos-idx)]
                (recur more (inc pos-idx)
                  (assoc result (:name spec) (coerce-arg arg (:type spec)))))
              (recur more pos-idx result))))))))

(defn validate-ext-args
  "Validate parsed args against spec. Returns nil on success, error string on failure."
  [arg-specs parsed]
  (let [required (filter :required arg-specs)
        missing  (remove #(contains? parsed (:name %)) required)]
    (when (seq missing)
      (str "Missing required argument(s): "
        (str/join ", " (map :name missing))))))

;; ── Help rendering ───────────────────────────────────────────────────

(defn- pad [s w]
  (let [s (str s)] (if (>= (count s) w) s (str s (apply str (repeat (- w (count s)) \space))))))

(defn format-cmd-help
  "Build help text for a single extension CLI command."
  [{:keys [cmd doc args ext-ns]}]
  (let [positional (filter #(= :positional (:kind %)) (or args []))
        flags      (filter #(= :flag (:kind %)) (or args []))
        usage-pos  (str/join " "
                     (map (fn [{:keys [name required]}]
                            (if required (str "<" name ">") (str "[" name "]")))
                       positional))
        usage-flags (when (seq flags) "[flags]")
        usage      (str/join " " (remove nil? [usage-pos usage-flags]))
        fmt-arg    (fn [{:keys [name type required doc]}]
                     (str "    " (pad name 20)
                       (pad (or (some-> type clojure.core/name) "string") 10)
                       (if required "required  " "optional  ")
                       (or doc "")))]
    (str "  vis ext " cmd (when (seq usage) (str " " usage))
      "\n\n  " (or doc "")
      (when ext-ns (str "\n  Extension: " ext-ns))
      (when (seq positional)
        (str "\n\n  Positional arguments:\n"
          (str/join "\n" (map fmt-arg positional))))
      (when (seq flags)
        (str "\n\n  Flags:\n"
          (str/join "\n" (map fmt-arg flags)))))))

(defn extension-help
  "Build help text for all extension CLI commands."
  []
  (let [cmds (all-extension-cmds)]
    (if (empty? cmds)
      "No extension commands available. Run 'vis extensions' to see registered extensions."
      (str "Extension commands:\n\n"
        (str/join "\n\n"
          (map (fn [{:keys [cmd doc ext-ns] :as c}]
                 (str "  vis ext " (pad cmd 20) (or doc "") "  (" ext-ns ")"))
            cmds))))))

;; ── Dispatch ───────────────────────────────────────────────────────

(defn run-extension-cmd!
  "Parse args, validate, and run an extension CLI command.
   Returns {:ok result} or {:error message}."
  [cmd-name raw-args]
  (if-let [{:keys [cmd]} (find-extension-cmd cmd-name)]
    (let [arg-specs (or (:args cmd) [])
          ;; --help on any command
          help?    (some #{"--help" "-h"} raw-args)]
      (if help?
        {:help (format-cmd-help (assoc cmd :ext-ns (:ext-ns cmd)))}
        (let [parsed (parse-ext-args arg-specs raw-args)
              err    (validate-ext-args arg-specs parsed)]
          (if err
            {:error (str err "\n\n" (format-cmd-help cmd))}
            {:ok ((:fn cmd) parsed)}))))
    {:error (str "Unknown command: " cmd-name "\n\n" (extension-help))}))
