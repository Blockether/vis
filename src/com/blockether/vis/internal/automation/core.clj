(ns com.blockether.vis.internal.automation.core
  "Automation definitions: validation, storage and the wire shape.

   An automation joins triggers, one prompt, one target and delivery options.
   The runner owns execution; this namespace never starts a turn. Definitions
   keep the contract spelling: string keys in snake_case."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.automation.cron :as cron]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.util :as util])
  (:import (java.net URI URISyntaxException)
           (java.util Base64 UUID)))

(def ^:private schema (document/schema-document "automations"))

(defn limit
  "One value of the contract `x-vis-limits`."
  ^long [k]
  (long (get-in schema ["x-vis-limits" (name k)])))

(def schedule-kinds #{"cron" "every" "once"})

(defn- invalid! [message] (throw (ex-info message {:status 400 :code :invalid-automation})))

(defn not-found!
  [what id]
  (throw (ex-info (str what " not found") {:status 404 :code :not-found :id id})))

(defn- utf8-size ^long [^String text] (alength (util/utf8 text)))

(defn- branches
  "The `oneOf` branches that one schema error belongs to, outermost first."
  [error]
  (let [location
        (str (:keywordLocation error))

        matcher
        (re-matcher #"/oneOf/\d+" location)]

    (loop [found []]
      (if (.find matcher) (recur (conj found (subs location 0 (.end matcher)))) found))))

(defn- schema-message
  "One readable message for a schema failure. A `oneOf` branch whose constant
   does not match is not the branch that the caller meant, so its errors stay
   out. Without a matching branch, the message names the allowed constants."
  [explained]
  (let [errors
        (:errors explained)

        constants
        (filter #(= "const" (:keyword %)) errors)

        failed
        (set (mapcat branches constants))

        live
        (remove #(or (= "oneOf" (:keyword %)) (some failed (branches %))) errors)

        error
        (first (sort-by #(- (count (:instanceLocation %))) live))

        [where message]
        (if error
          [(:instanceLocation error) (:error error)]
          [(:instanceLocation (first constants))
           (str "use one of "
                (str/join ", " (distinct (map #(get-in % [:params :allowedValue]) constants))))])]

    (str "Automation is not valid"
         (when (seq where) (str " at " where))
         (when message (str ": " message)))))

(defn- check-schema!
  [definition value]
  (when-let [explained (document/explain-json "automations" definition value)]
    (invalid! (schema-message explained))))

(defn- callback-url!
  [url]
  (let [uri (try (URI. url) (catch URISyntaxException _ (invalid! "Callback URL is not valid")))]
    (when (str/blank? (.getHost ^URI uri)) (invalid! "Callback URL needs a host"))))

(defn next-fire
  "The first fire time of one schedule trigger strictly after `from`, or nil.
   `anchor` is the start of an interval schedule."
  [trigger anchor from]
  (case (get trigger "kind")
    "cron"
    (cron/next-fire (cron/parse (get trigger "expression"))
                    (cron/zone (get trigger "timezone"))
                    from)

    "every"
    (let [interval
          (* 1000 (long (get trigger "seconds")))

          steps
          (inc (quot (max 0 (- (long from) (long anchor))) interval))]

      (+ (long anchor) (* steps interval)))

    "once"
    (let [at (long (get trigger "at"))]
      (when (> at (long from)) at))

    nil))

(defn- check-trigger!
  [trigger now]
  (case (get trigger "kind")
    "cron"
    (try (when-not (next-fire trigger now now)
           (invalid! (str "Cron expression never fires: " (get trigger "expression"))))
         (catch clojure.lang.ExceptionInfo e
           (if (#{:invalid-cron :invalid-timezone} (:type (ex-data e)))
             (invalid! (ex-message e))
             (throw e))))

    nil))

(defn normalize
  "Fill the optional fields of a valid input with their defaults."
  [definition]
  (let [delivery (get definition "delivery")]
    (-> (select-keys definition
                     ["name" "enabled" "triggers" "prompt" "target" "delivery" "model"
                      "deliver_only"])
        (update "name" str/trim)
        (update "enabled" #(if (nil? %) true %))
        (assoc "delivery" {"push" (if (contains? delivery "push") (get delivery "push") true)
                           "callback" (get delivery "callback")})
        (update "model" identity)
        (update "deliver_only" boolean))))

(defn validate
  "Check one complete definition; answer it with defaults. Throws ex-info with
   `:status 400` and a readable message."
  [definition now]
  (check-schema! "automation_input" definition)
  (let [definition
        (normalize definition)

        triggers
        (get definition "triggers")]

    (when (> (utf8-size (get definition "prompt")) (limit :prompt_bytes))
      (invalid! (str "Prompt is larger than " (limit :prompt_bytes) " bytes")))
    (when (> (count (filter #(= "webhook" (get % "kind")) triggers)) 1)
      (invalid! "An automation can have only one webhook trigger"))
    (doseq [trigger triggers]
      (check-trigger! trigger now))
    (some-> (get-in definition ["delivery" "callback" "url"])
            callback-url!)
    definition))

(defn webhook-trigger
  [definition]
  (first (filter #(= "webhook" (get % "kind")) (get definition "triggers"))))

(defn next-run-at
  "The next scheduled fire time of an enabled automation after `now`, or nil."
  [{:keys [definition created_at]} now]
  (when (get definition "enabled")
    (let [fires (keep #(when (schedule-kinds (get % "kind"))
                         (try (next-fire % created_at now) (catch Exception _ nil)))
                      (get definition "triggers"))]
      (when (seq fires) (apply min fires)))))

(defn run->wire
  [run automation-name]
  {"id" (:id run)
   "automation_id" (:automation_id run)
   "automation_name" automation-name
   "trigger" (:trigger_kind run)
   "status" (:status run)
   "reason" (:reason run)
   "scheduled_at" (:scheduled_at run)
   "created_at" (:created_at run)
   "started_at" (:started_at run)
   "finished_at" (:finished_at run)
   "session_id" (:session_id run)
   "turn_id" (:turn_id run)
   "answer" (:answer run)
   "error" (:error run)
   "is_silent" (boolean (:is_silent run))})

(defn ->wire
  "The contract shape of one stored automation. Secrets never appear."
  [{:keys [id definition created_at updated_at webhook_secret callback_secret] :as row} last-run
   now]
  (merge definition
         {"id" id
          "created_at" created_at
          "updated_at" updated_at
          "next_run_at" (next-run-at row now)
          "webhook" (when (webhook-trigger definition) {"path" (str "/v1/hooks/" id)})
          "secrets" {"webhook" (some? webhook_secret) "callback" (some? callback_secret)}
          "last_run" (some-> last-run
                             (run->wire (get definition "name")))}))

(defn new-secret
  "A Standard Webhooks secret: `whsec_` and 32 random bytes in base64."
  []
  (str "whsec_" (.encodeToString (Base64/getEncoder) (util/random-bytes 32))))

(defn- last-run
  [db automation-id]
  (first (ps/db-automation-runs db {:automation-id automation-id :limit 1})))

(defn describe
  "The wire shape of one automation, or a 404 ex-info."
  [db id now]
  (if-let [row (ps/db-automation-get db id)]
    (->wire row (last-run db id) now)
    (not-found! "Automation" id)))

(defn list-all [db now] (mapv #(->wire % (last-run db (:id %)) now) (ps/db-automation-list db)))

(defn create!
  [db input now]
  (when (>= (count (ps/db-automation-list db)) (limit :automations))
    (throw (ex-info (str "An automation store holds at most " (limit :automations) " automations")
                    {:status 409 :code :automation-limit})))
  (let [definition
        (validate input now)

        id
        (str (UUID/randomUUID))]

    (ps/db-automation-put! db
                           {:id id
                            :name (get definition "name")
                            :enabled (get definition "enabled")
                            :definition definition
                            :created_at now
                            :updated_at now})
    (describe db id now)))

(defn update!
  "Replace the given top-level fields and validate the result as a whole."
  [db id patch now]
  (check-schema! "automation_patch" patch)
  (let [row
        (or (ps/db-automation-get db id) (not-found! "Automation" id))

        definition
        (validate (merge (:definition row) patch) now)]

    (ps/db-automation-put! db
                           (assoc row
                             :name (get definition "name")
                             :enabled (get definition "enabled")
                             :definition definition
                             :updated_at now))
    (describe db id now)))

(defn delete!
  [db id]
  (if (ps/db-automation-delete! db id) {"id" id "is_deleted" true} (not-found! "Automation" id)))

(defn rotate-secret!
  "Create or replace one secret and answer it once."
  [db id kind now]
  (check-schema! "secret_request" {"kind" kind})
  (let [row
        (or (ps/db-automation-get db id) (not-found! "Automation" id))

        secret
        (new-secret)]

    (ps/db-automation-put! db
                           (assoc row
                             (if (= "webhook" kind) :webhook_secret :callback_secret) secret
                             :updated_at now))
    {"kind" kind "secret" secret}))

(defn runs
  "Runs newest first, with the automation names."
  [db opts]
  (let [names (into {} (map (juxt :id :name)) (ps/db-automation-list db))]
    (mapv #(run->wire % (get names (:automation_id %) "Automation"))
          (ps/db-automation-runs db opts))))

(defn run
  [db run-id]
  (if-let [row (ps/db-automation-run db run-id)]
    (run->wire row (or (:name (ps/db-automation-get db (:automation_id row))) "Automation"))
    (not-found! "Automation run" run-id)))
