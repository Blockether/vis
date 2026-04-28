(ns com.blockether.vis-cli.commandline.extensions
  "Extension CLI dispatcher.

   Extensions export CLI commands via `:ext/cli`. The CONTRACT (cmd
   spec, registry) lives in `com.blockether.vis-extension.commandline.base`;
   this namespace owns the CONSUMER side: argument parsing, validation,
   help rendering, and dispatch.

   Command spec:
       {:cmd  \"name\"
        :doc  \"Short description\"
        :args [{:name \"path\"     :kind :positional :type :string  :required true  :doc \"...\"}
               {:name \"count\"    :kind :positional :type :int     :required false :doc \"...\"}
               {:name \"--verbose\" :kind :flag      :type :boolean :doc \"...\"}
               {:name \"--output\"  :kind :flag      :type :string  :doc \"...\"}]
        :fn   (fn [parsed-args] ...)}

   `:kind :positional` — matched by order, no `--` prefix.
   `:kind :flag`       — matched by `--name`. Boolean flags need no value.

   Public commands wired by the dispatcher (`commandline.main`):
   - `vis ext help`            — list every extension command
   - `vis ext <cmd> --help`    — show command-specific help with args
   - `vis ext <cmd> [args...]` — validate then dispatch"
  (:require [clojure.string :as str]
            [com.blockether.vis-extension.extension :as ext]))

;;; ── Extension introspection ─────────────────────────────────────────────

(defn list-extensions
  "Return all registered extensions with their metadata (table rows)."
  []
  (mapv (fn [e]
          {:namespace (str (:ext/namespace e))
           :doc       (:ext/doc e)
           :group     (:ext/group e)
           :version   (or (:ext/version e) "—")
           :cli-cmds  (str/join ", " (map :cmd/name (or (:ext/cli e) [])))})
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

;;; ── Arg parsing & validation ───────────────────────────────────────────

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

;;; ── Help rendering ─────────────────────────────────────────────────────

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
          (map (fn [{:keys [cmd doc ext-ns]}]
                 (str "  vis ext " (pad cmd 20) (or doc "") "  (" ext-ns ")"))
            cmds))))))

;;; ── Dispatch ───────────────────────────────────────────────────────────

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
