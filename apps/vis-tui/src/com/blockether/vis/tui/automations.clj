(ns com.blockether.vis.tui.automations
  "The Automations view: read the automations of the gateway, run one now, pause or resume it,
   read its runs, create a one-time secret and delete it. A person creates an automation in the
   chat or in the Companion app. A secret appears only in a dialog, never in the transcript."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(def ^:private timeout-ms 5000)

(defn- enc [s] (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn- call!
  "One gateway call: `{:body parsed}` for 200 or 201, else `{:error message}`."
  ([method path] (call! method path nil))
  ([method path body]
   (try (let [response
              (vis/request! method
                            path
                            (cond-> {:timeout-ms timeout-ms}
                              (some? body)
                              (assoc :body body)))

              parsed
              (try (wire/parse-json (:body response)) (catch Throwable _ nil))]

          (if (#{200 201} (:status response))
            {:body parsed}
            {:error (or (get-in parsed ["error" "message"])
                        (str "The gateway answered " (:status response)))}))
        (catch Throwable _ {:error "The gateway did not answer"}))))

(defn- automation-path [id & [suffix]] (str "/v1/automations/" (enc id) suffix))

(defn fetch "Every automation and the global switch." [] (call! :get "/v1/automations"))

(defn- runs [id] (call! :get (str "/v1/automations/runs?automation_id=" (enc id) "&limit=20")))

(defn- set-enabled! [id enabled?] (call! :patch (automation-path id) {"enabled" enabled?}))

(defn- run-now! [id] (call! :post (automation-path id "/run") {}))

(defn- secret! [id kind] (call! :post (automation-path id "/secrets") {"kind" kind}))

(defn- delete! [id] (call! :delete (automation-path id)))

;; Presentation

(defn time-label
  "Local date and minute of epoch milliseconds, or nil."
  ([ms] (time-label ms (ZoneId/systemDefault)))
  ([ms ^ZoneId zone]
   (when ms
     (.format (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm") zone)
              (Instant/ofEpochMilli (long ms))))))

(defn- duration-label
  [seconds]
  (let [s (long seconds)]
    (cond (zero? (rem s 86400)) (str (quot s 86400) " d")
          (zero? (rem s 3600)) (str (quot s 3600) " h")
          (zero? (rem s 60)) (str (quot s 60) " min")
          :else (str s " s"))))

(defn trigger-label
  "One trigger in a short phrase."
  [trigger]
  (case (get trigger "kind")
    "cron"
    (str/join " " (remove nil? ["cron" (get trigger "expression") (get trigger "timezone")]))

    "every"
    (str "every " (duration-label (get trigger "seconds")))

    "once"
    (str "once at " (time-label (get trigger "at")))

    "webhook"
    (str (get trigger "signature") " webhook")

    (str (get trigger "kind"))))

(defn- target-label
  [target]
  (case (get target "mode")
    "session"
    (str "the session `" (get target "session_id") "`")

    "new"
    "a new session for each run"

    "temporary"
    "a temporary session for each run"

    "unknown"))

(defn- delivery-label
  [delivery]
  (let [url (get-in delivery ["callback" "url"])]
    (str/join ", "
              (cond-> ["the session"]
                (get delivery "push")
                (conj "Push")

                url
                (conj (str "a callback to " url))))))

(defn- state-label [automation] (if (get automation "enabled") "On" "Paused"))

(defn rows
  "List items for the automations, with the state and the next run as the hint."
  [automations]
  (mapv (fn [automation]
          {:label (get automation "name")
           :hint (str/join " · "
                           (remove nil?
                             [(state-label automation)
                              (some->> (get automation "next_run_at")
                                       time-label
                                       (str "next "))]))
           :automation automation})
        automations))

(defn- webhook? [automation] (some #(= "webhook" (get % "kind")) (get automation "triggers")))

(defn actions
  "What a person can do with one automation."
  [automation]
  (let [secret-label
        (fn [kind]
          (str (if (get-in automation ["secrets" kind]) "Replace " "Create ") kind " secret"))]
    (cond-> [{:id :details :label "Details"} {:id :run :label "Run now"}
             {:id :toggle :label (if (get automation "enabled") "Pause" "Resume")}
             {:id :runs :label "Runs"}]
      (webhook? automation)
      (conj {:id :webhook-secret :label (secret-label "webhook")})

      (get-in automation ["delivery" "callback"])
      (conj {:id :callback-secret :label (secret-label "callback")})

      true
      (conj {:id :delete :label "Delete"}))))

(defn- one-line
  [s limit]
  (let [s (str/trim (str/replace (str s) #"\s+" " "))]
    (if (> (count s) (long limit)) (str (subs s 0 (dec (long limit))) "…") s)))

(defn detail-markdown
  "The whole definition of one automation."
  [automation]
  (let [last-run (get automation "last_run")]
    (str "- State: "
         (state-label automation)
         "\n- Triggers: "
         (str/join ", " (map trigger-label (get automation "triggers")))
         "\n- Target: "
         (target-label (get automation "target"))
         "\n- Delivery: "
         (delivery-label (get automation "delivery"))
         (when (get automation "deliver_only") "\n- Sends the prompt text without a model")
         "\n- Next run: "
         (or (time-label (get automation "next_run_at")) "none")
         "\n- Last run: "
         (if last-run
           (str (get last-run "status") " at " (time-label (get last-run "created_at")))
           "none")
         (when-let [url (get-in automation ["webhook" "url"])]
           (str "\n- Webhook URL: `" url "`"))
         (when-let [path (get-in automation ["webhook" "path"])]
           (str "\n- Webhook path: `" path "`"))
         "\n\n```text\n"
         (get automation "prompt")
         "\n```\n")))

(defn runs-markdown
  "Recent runs as a table, newest first."
  [runs]
  (if (empty? runs)
    "No runs yet."
    (str "| Created | Trigger | Status | Result |\n|---|---|---|---|\n"
         (str/join
           "\n"
           (for [run runs]
             (str "| "
                  (time-label (get run "created_at"))
                  " | "
                  (get run "trigger")
                  " | "
                  (get run "status")
                  " | "
                  (str/replace
                    (one-line (or (get run "error") (get run "answer") (get run "reason") "") 80)
                    "|"
                    "\\|")
                  " |")))
         "\n")))

(defn secret-markdown
  "The one-time view of a new secret."
  [kind secret automation copied?]
  (str "Vis shows this secret only once. Store it now.\n\n`"
       secret
       "`\n\n"
       (if (= "webhook" kind)
         (str "Sign each webhook to `"
              (or (get-in automation ["webhook" "url"]) (get-in automation ["webhook" "path"]))
              "` with this secret. The old secret no longer works.")
         "Check the `webhook-signature` header of each callback with this secret.")
       (when copied? "\n\nThe secret is on the clipboard.")))

;; View

(defn- report!
  [{:keys [error]} done]
  (if error (vis/notify! error :level :error) (vis/notify! done)))

(defn- create-secret!
  [screen automation kind]
  (when (or (not (get-in automation ["secrets" kind]))
            (dlg/confirm-dialog! screen
                                 (str "Replace the " kind " secret?")
                                 "The current secret stops working at once."))
    (let [{:keys [body error]} (secret! (get automation "id") kind)]
      (if error
        (vis/notify! error :level :error)
        (let [secret (get body "secret")]
          (dlg/markdown-viewer-dialog!
            screen
            (str (get automation "name") " · New " kind " secret")
            (secret-markdown kind secret automation (input/clipboard-copy! secret))))))))

(defn- act!
  [screen automation]
  (let [id
        (get automation "id")

        automation-name
        (get automation "name")]

    (when-let [{action :id} (dlg/select-dialog! screen automation-name (actions automation))]
      (case action
        :details
        (dlg/markdown-viewer-dialog! screen automation-name (detail-markdown automation))

        :run
        (report! (run-now! id) (str "Started " automation-name))

        :toggle
        (let [enabled? (get automation "enabled")]
          (report! (set-enabled! id (not enabled?))
                   (str (if enabled? "Paused " "Resumed ") automation-name)))

        :runs
        (let [{:keys [body error]} (runs id)]
          (if error
            (vis/notify! error :level :error)
            (dlg/markdown-viewer-dialog! screen
                                         (str automation-name " · Runs")
                                         (runs-markdown (get body "runs")))))

        :webhook-secret
        (create-secret! screen automation "webhook")

        :callback-secret
        (create-secret! screen automation "callback")

        :delete
        (when (dlg/confirm-dialog! screen
                                   (str "Delete " automation-name "?")
                                   "Vis deletes the automation and its run history.")
          (report! (delete! id) (str "Deleted " automation-name)))

        nil))))

(defn show!
  "Open the Automations view until the person closes the list."
  [screen]
  (loop []

    (let [{:keys [body error]} (fetch)]
      (if error
        (vis/notify! (str "Could not read automations: " error) :level :error)
        (let [automations (get body "automations")
              title (if (get body "is_enabled")
                      "Automations"
                      "Automations · Turn on Allow automations in Settings")
              items (if (seq automations)
                      (rows automations)
                      [{:label "No automations. Ask Vis in the chat to create one."}])
              choice (dlg/list-dialog!
                       screen
                       title
                       items
                       {:filter? (> (count items) 8) :enter-label "open" :height :content})]

          (when-let [automation (:automation choice)]
            (act! screen automation)
            (recur)))))))
