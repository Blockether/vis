(ns com.blockether.vis.internal.automation.cli
  "`vis-agent automations` commands: list, show, create, run, pause, resume and delete
   automations through the gateway, so a setup script can manage them without the app."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.gateway.client :as gateway-client])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.util Date)))

(defn- refuse!
  "Stop the command with `message` as a user error, without a stack trace."
  [message]
  (throw (ex-info message {:type :vis.cli/automations :vis/user-error true})))

(defn- gateway-message
  [status json]
  (let [error (get json "error")]
    (or (when (map? error) (not-empty (str (get error "message"))))
        (when (string? error) (not-empty error))
        (str "The gateway answered HTTP " status))))

(defn- call!
  "Send one request to the gateway. Answers `{:text :json}` of a 2xx answer, else
   stops with the message of the gateway."
  ([method path] (call! method path nil))
  ([method path body]
   (let [{:keys [status] :as response}
         (gateway-client/request! method
                                  path
                                  (cond-> {}
                                    (some? body)
                                    (assoc :body body)))

         text
         (let [b (:body response)]
           (if (bytes? b) (String. ^bytes b StandardCharsets/UTF_8) (str b)))

         json
         (wire/parse-json text)]

     (when-not (<= 200 (long status) 299) (refuse! (gateway-message status json)))
     {:text text :json json})))

(defn- automation-path
  [id & more]
  (apply str "/v1/automations/" (URLEncoder/encode (str id) StandardCharsets/UTF_8) more))

(defn- automations [] (get-in (call! :get "/v1/automations") [:json "automations"]))

(defn- match-automations
  "Automations from `items` selected by `input`: an exact id wins, else every id with
   that prefix (case-insensitive)."
  [items input]
  (let [needle
        (str/lower-case (str/trim (str input)))

        aid
        #(str/lower-case (str (get % "id")))]

    (if (str/blank? needle)
      []
      (let [exact (filterv #(= needle (aid %)) items)]
        (if (seq exact) exact (filterv #(str/starts-with? (aid %) needle) items))))))

(defn- label [a] (str (get a "id") " (" (get a "name") ")"))

(defn- resolve!
  "The automation with the full id or unambiguous prefix `input`, or stop with why not."
  [input]
  (let [matches (match-automations (automations) input)]
    (case (count matches)
      1
      (first matches)

      0
      (refuse! (str "Automation not found: " input ". List them with: vis-agent automations list"))

      (refuse! (str "Ambiguous automation id: " input
                    " (" (count matches)
                    " matches)\n" (str/join "\n" (map #(str "  " (label %)) matches)))))))

(defn- owner-name [a] (or (get-in a ["owner" "extension"]) "-"))

(defn- state [a] (if (get a "enabled") "enabled" "paused"))

(defn- moment [ms] (if (number? ms) (fmt/format-date (Date. (long ms))) "-"))

(defn- list!
  [{:strs [json]} _]
  (config/init-cli!)
  (let [{:keys [text]}
        (call! :get "/v1/automations")

        items
        (get (wire/parse-json text) "automations")]

    (cond json (commandline/stdout! text)
          (empty? items) (commandline/stdout! "No automations.")
          :else (do (commandline/stdout! "\n  Automations\n")
                    (commandline/print-table!
                      [{:key :id :label "ID" :width 36 :align :left}
                       {:key :name :label "Name" :width 24 :align :left :grow? true}
                       {:key :state :label "State" :width 8 :align :left}
                       {:key :owner :label "Extension" :width 12 :align :left}
                       {:key :next :label "Next run" :width 16 :align :left}]
                      (mapv (fn [a]
                              {:id (str (get a "id"))
                               :name (str (get a "name"))
                               :state (state a)
                               :owner (owner-name a)
                               :next (moment (get a "next_run_at"))})
                            items))
                    (commandline/stdout! (str "\n  " (count items) " automation(s)\n"))))))

(defn- trigger-text
  [{:strs [kind seconds expression timezone at]}]
  (case kind
    "every"
    (str "every " seconds "s")

    "cron"
    (str "cron " expression (when timezone (str " " timezone)))

    "once"
    (str "once at " (moment at))

    (str kind)))

(defn- show!
  [parsed _]
  (config/init-cli!)
  (let [a (resolve! (get parsed "id"))]
    (if (get parsed "json")
      (commandline/stdout! (:text (call! :get (automation-path (get a "id")))))
      (doseq [[k v] [["ID" (get a "id")] ["Name" (get a "name")] ["State" (state a)]
                     ["Extension" (owner-name a)]
                     ["Triggers" (str/join ", " (map trigger-text (get a "triggers")))]
                     ["Target" (get-in a ["target" "mode"])]
                     ["Next run" (moment (get a "next_run_at"))]
                     ["Last run"
                      (if-let [run (get a "last_run")]
                        (str (get run "status") " " (moment (get run "created_at")))
                        "-")]]]
        (commandline/stdout! (str "  " k ": " v))))))

(defn- read-definition
  "The automation definition in the file at `source`, or on stdin for `-`."
  [source]
  (let [text
        (cond (= "-" source) (slurp *in*)
              (.isFile (io/file source)) (slurp (io/file source))
              :else (refuse! (str "No definition file at " source)))

        definition
        (wire/parse-json text)]

    (when-not (map? definition) (refuse! (str "The definition in " source " is not a JSON object")))
    definition))

(defn upsert!
  "Store `definition` once by its name. One automation of that name that no extension
   owns gets the fields; with none, a new automation is created. Answers
   `[:updated|:created automation]`."
  [definition]
  (let [same (filterv #(and (= (get definition "name") (get % "name")) (nil? (get % "owner")))
               (automations))]
    (case (count same)
      0
      [:created (:json (call! :post "/v1/automations" definition))]

      1
      [:updated (:json (call! :patch (automation-path (get (first same) "id")) definition))]

      (refuse! (str (count same)
                    " automations have the name \"" (get definition "name")
                    "\". Delete or rename all but one:\n"
                    (str/join "\n" (map #(str "  " (label %)) same)))))))

(defn- create!
  [parsed residual]
  (config/init-cli!)
  (let [source (or (get parsed "file") (first residual))]
    (when (str/blank? (str source))
      (refuse! "Name the definition file, or - for stdin: vis-agent automations create FILE"))
    (let [[outcome a] (upsert! (read-definition source))]
      (commandline/stdout!
        (if (= :created outcome) (str "Created " (get a "id")) (str "Updated " (get a "id")))))))

(defn- run-now!
  [parsed _]
  (config/init-cli!)
  (let [a
        (resolve! (get parsed "id"))

        run
        (:json (call! :post (automation-path (get a "id") "/run")))]

    (commandline/stdout!
      (str "Started run " (get run "id") " of " (label a) ": " (get run "status")))))

(defn- set-enabled!
  [enabled verb]
  (fn [parsed _]
    (config/init-cli!)
    (let [a (resolve! (get parsed "id"))]
      (call! :patch (automation-path (get a "id")) {"enabled" enabled})
      (commandline/stdout! (str verb " " (label a))))))

(defn- delete!
  [parsed _]
  (config/init-cli!)
  (let [a (resolve! (get parsed "id"))]
    (call! :delete (automation-path (get a "id")))
    (commandline/stdout! (str "Deleted " (label a)))))

(defn- automations!
  [parsed residual]
  (if (seq residual)
    (refuse!
      (str "Unknown automations command: " (first residual) ". Run: vis-agent automations --help"))
    (list! parsed residual)))

(def ^:private id-arg
  {:name "id"
   :kind :positional
   :type :string
   :required true
   :doc "Automation id (full UUID or unambiguous prefix)."})

(def ^:private json-arg
  {:name "json" :kind :flag :type :boolean :doc "Print the gateway JSON unchanged, for scripts."})

(def command
  {:cmd/name "automations"
   :cmd/doc "List, show, create, run, pause, resume or delete automations."
   :cmd/usage "vis-agent automations <list|show|create|run|pause|resume|delete> [...]"
   :cmd/args [json-arg]
   :cmd/examples ["vis-agent automations" "vis-agent automations create automation.json"
                  "vis-agent automations pause 9f2c1a44"]
   :cmd/subcommands #(registry/registered-under ["automations"])
   :cmd/run-fn automations!})

(defn- subcommand
  [cmd-name doc usage args run-fn]
  {:cmd/name cmd-name
   :cmd/parent ["automations"]
   :cmd/doc doc
   :cmd/usage usage
   :cmd/args args
   :cmd/examples [usage]
   :cmd/run-fn run-fn})

(def subcommands
  "Subcommands registered under `vis-agent automations`."
  [(subcommand "list"
               "List automations with their state and next run."
               "vis-agent automations list [--json]"
               [json-arg]
               list!)
   (subcommand "show"
               "Show one automation."
               "vis-agent automations show <ID> [--json]"
               [id-arg json-arg]
               show!)
   (subcommand "create"
               "Create an automation from a JSON definition, or update the one with the same name."
               "vis-agent automations create <FILE|->"
               [{:name "file"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "JSON definition file, or - for stdin."}]
               create!)
   (subcommand "run" "Run an automation now." "vis-agent automations run <ID>" [id-arg] run-now!)
   (subcommand "pause"
               "Pause an automation."
               "vis-agent automations pause <ID>"
               [id-arg]
               (set-enabled! false "Paused"))
   (subcommand "resume"
               "Resume a paused automation."
               "vis-agent automations resume <ID>"
               [id-arg]
               (set-enabled! true "Resumed"))
   (subcommand "delete"
               "Delete an automation and its runs."
               "vis-agent automations delete <ID>"
               [id-arg]
               delete!)])
