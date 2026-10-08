(ns com.blockether.vis.internal.decisions.openai
  "OpenAI Decisions API behind the one `/v1/systemone` contract.

   A model named `openai/<id>` runs on OpenAI instead of a local bundle. This
   namespace converts validated questions to OpenAI's request, then converts the
   ordered answers back to the local answer shapes. Only an OpenAI API key works:
   the Decisions API refuses ChatGPT (Codex) sign-in tokens."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.provider.service :as provider-service]))

(def ^:private prefix "openai/")

(def ^:private default-base-url "https://api.openai.com/v1")

(def known-models
  "OpenAI classifier models that the model catalog lists. Other `openai/` ids still pass through."
  ["gpt-6-luna"])

(def ^:private timeout-ms 60000)

(defn model?
  "True when `name` selects an OpenAI decision model."
  [name]
  (and (string? name) (str/starts-with? name prefix) (< (count prefix) (count name))))

(defn- non-blank [value] (when (and (string? value) (not (str/blank? value))) value))

(defn credentials
  "The OpenAI API key and base URL, or nil. Never the Codex sign-in token.

   The configured `openai` provider wins: its `api_key_command`, then its
   `api_key`. The `OPENAI_API_KEY` environment variable is the fallback."
  []
  (let [entry
        (try (some #(when (= :openai (:id %)) %) (provider-service/configured-providers-cached))
             (catch Throwable _ nil))

        api-key
        (or (non-blank (try (config/command-token :openai) (catch Throwable _ nil)))
            (when-not (some-> (:api-key entry)
                              (str/includes? "${"))
              (non-blank (:api-key entry)))
            (non-blank (System/getenv "OPENAI_API_KEY")))]

    (when api-key
      {:api-key api-key
       :base-url (str/replace (or (non-blank (:base-url entry)) default-base-url) #"/+$" "")})))

(defn models-status
  "Catalog rows for the known OpenAI models. Remote rows have no `installed` key."
  []
  (let [available (some? (credentials))]
    (mapv (fn [id]
            {"model_ref" (str prefix id)
             "provider" "openai"
             "residency" "remote"
             "available" available})
          known-models)))

(defn- fail!
  ([type message] (fail! type message {}))
  ([type message data] (throw (ex-info message (assoc data :type type)))))

(defn- described
  [base description]
  (if-let [text (non-blank description)]
    (assoc base "description" text)
    base))

(defn- level-text [value] (if (string? value) value (json/write-json-str value)))

(defn- request-question
  "One validated local question as an OpenAI question."
  [{:keys [id type instruction criteria]}]
  (case type
    "choice"
    {"type" "choice"
     "name" id
     "instructions" instruction
     "choices"
     (mapv (fn [[label description]]
             (described {"value" label}
                        (some-> description
                                level-text)))
           (if (instance? java.util.Map criteria) criteria (map vector criteria (repeat nil))))}

    "score"
    {"type" "score"
     "name" id
     "instructions" instruction
     "levels" (mapv (fn [level]
                      {"label" (level-text level)})
                    criteria)}

    "noul"
    (let [meaning
          (fn [label]
            (some-> (get criteria label)
                    level-text
                    non-blank))

          lines
          (remove nil?
            [instruction
             (some->> (meaning "true")
                      (str "True means: "))
             (some->> (meaning "false")
                      (str "False means: "))])]

      {"type" "predicate" "name" id "instructions" (str/join "\n" lines)})))

(defn request-body
  "The OpenAI `/v1/decisions` body for one model id, input text and validated questions."
  [model-id input items]
  {"model" model-id "input" input "questions" (mapv request-question items)})

(defn- round4 ^double [value] (/ (Math/round (* 10000.0 (double value))) 10000.0))

(defn- probability ^double [value] (round4 (double (or value 0.0))))

(defn- unexpected!
  [id]
  (fail! :decisions/unavailable (str "OpenAI returned an unexpected answer for " id)))

(defn- local-answer
  "Convert one ordered OpenAI answer to the local answer shape of `item`."
  [{:keys [id type criteria]} answer]
  (let [kind (get answer "type")]
    (cond (= "refusal" kind) {"type" "refusal"}
          (and (= "noul" type) (= "predicate" kind))
          (let [truth (probability (get answer "probability"))]
            {"type" "noul" "noul" truth "confidence" (round4 (max truth (- 1.0 truth)))})
          (and (= "choice" type) (= "choice" kind))
          {"type" "choice"
           "choice" (str (get answer "choice"))
           "probabilities" (into {}
                                 (map (fn [row]
                                        [(str (get row "value"))
                                         (probability (get row "probability"))]))
                                 (get answer "probabilities"))
           "confidence" (probability (get answer "confidence"))}
          (and (= "score" type) (= "score" kind))
          (let [labels (mapv level-text criteria)
                by-label (into {}
                               (map (fn [row]
                                      [(get row "label") (probability (get row "probability"))]))
                               (get answer "probabilities"))
                probabilities (mapv #(get by-label % 0.0) labels)
                indexes (map str (range (count labels)))]

            {"type" "score"
             "score" (round4 (reduce + (map-indexed #(* %1 %2) probabilities)))
             "legend" (zipmap indexes criteria)
             "probabilities" (zipmap indexes probabilities)
             "confidence" (probability (get answer "confidence"))})
          :else (unexpected! id))))

(defn response->result
  "The local decision result for OpenAI response `body`, keyed by question id in request order."
  [name items body]
  (let [answers (get body "answers")]
    (when-not (and (sequential? answers) (= (count answers) (count items)))
      (fail! :decisions/unavailable "OpenAI returned a different number of answers"))
    (let [usage (get body "usage")]
      {"model" (or (non-blank (get body "model")) (subs name (count prefix)))
       "routing" {"model" name "model_ref" name "provider" "openai"}
       "answers" (into {}
                       (map (fn [item answer]
                              (when-let [returned (get answer "name")]
                                (when-not (= returned (:id item)) (unexpected! (:id item))))
                              [(:id item) (local-answer item answer)])
                            items
                            answers))
       "usage" {"input_tokens" (long (or (get usage "input_tokens") 0))
                "output_tokens" (long (or (get usage "output_tokens") 0))}})))

(defn- upstream-message
  [body]
  (or (try (non-blank (get-in (json/read-json body) ["error" "message"])) (catch Exception _ nil))
      "no error message"))

(defn- post!
  [{:keys [api-key base-url]} body]
  (try (http/post (str base-url "/decisions")
                  {:headers {"Authorization" (str "Bearer " api-key)
                             "Content-Type" "application/json"
                             "Accept" "application/json"}
                   :body (json/write-json-str body)
                   :timeout timeout-ms
                   :throw false})
       (catch Exception e
         (fail! :decisions/unavailable (str "OpenAI decisions are unreachable: " (ex-message e))))))

(defn infer!
  "Answer validated questions with OpenAI model `name` (`openai/<id>`) and input text."
  [name input items]
  (let [model-id (subs name (count prefix))]
    (if (empty? items)
      {"model" model-id
       "routing" {"model" name "model_ref" name "provider" "openai"}
       "answers" {}
       "usage" {"input_tokens" 0 "output_tokens" 0}}
      (let [creds (or (credentials)
                      (fail!
                        :decisions/provider-unavailable
                        (str "OpenAI decision models need an OpenAI API key. Configure the openai "
                             "provider or set OPENAI_API_KEY. A ChatGPT (Codex) sign-in does not "
                             "work with the Decisions API.")))
            {:keys [status body]} (post! creds (request-body model-id input items))]

        (case (long status)
          200
          (response->result name
                            items
                            (try (json/read-json body)
                                 (catch Exception _
                                   (fail! :decisions/unavailable "OpenAI returned invalid JSON"))))

          (400 422)
          (fail! :decisions/invalid-request
                 (str "OpenAI rejected the decision request: " (upstream-message body)))

          (401 403)
          (fail! :decisions/provider-unavailable
                 (str "OpenAI rejected the configured API key: " (upstream-message body)))

          404
          (fail! :decisions/unknown-model
                 (str "Unknown OpenAI decision model: " model-id)
                 {:model name})

          (fail! :decisions/unavailable
                 (str "OpenAI decisions failed with HTTP " status
                      ": " (upstream-message body))))))))
