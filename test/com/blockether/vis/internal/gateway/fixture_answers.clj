;; Copyright (c) Blockether. All rights reserved.
(ns com.blockether.vis.internal.gateway.fixture-answers
  "Complete gateway answers for test stubs, read from the real answers in
   `fixtures/gateway-answers.json`. A stub that answers a partial map breaks the
   response contract, and the answer hook (`response-check`) refuses it (#487).
   Give only the fields that the test is about; the fixture gives the rest."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]))

(def ^:private answers
  (delay (get (wire/parse-json (slurp (io/resource "vis-contract/fixtures/gateway-answers.json")))
              "answers")))

(defn- answer
  "The fixture answer of the operation key `op`, such as `\"GET /v1/sessions/:sid/turns/:tid\"`."
  [op]
  (or (get @answers op) (throw (ex-info (str "No fixture answer for " op) {:operation op}))))

(defn soul
  "A complete wire soul with the fields of `overrides`."
  ([] (soul {}))
  ([overrides] (merge (answer "PUT /v1/sessions/:sid/group") (wire/canonical overrides))))

(defn session-row
  "A complete session list row: a soul with its unread fields, and `overrides`."
  ([] (session-row {}))
  ([overrides] (soul (merge {:is_unread false :unread_answers 0} overrides))))

(defn turn
  "A complete wire turn with the fields of `overrides`."
  ([] (turn {}))
  ([overrides] (merge (answer "GET /v1/sessions/:sid/turns/:tid") (wire/canonical overrides))))

(defn created-session
  "A complete `POST /v1/sessions` answer with the fields of `overrides`."
  ([] (created-session {}))
  ([overrides]
   (merge {"id" "s1"
           "channel" "api"
           "title" nil
           "goal" nil
           "external_id" nil
           "workspace_id" nil
           "group_id" nil
           "origin" nil}
          (wire/canonical overrides))))
