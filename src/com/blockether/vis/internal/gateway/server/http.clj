(ns com.blockether.vis.internal.gateway.server.http
  "Request parsing and JSON responses shared by the gateway route handlers."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util]))

;; Ring helpers

(defn json-response
  ([body] (json-response 200 body))
  ([status body]
   {:status status :headers {"Content-Type" "application/json"} :body (wire/json-str body)}))

(defn error-response
  [status type message & {:as extra}]
  (json-response status (gateway-contract/error-body type message extra)))

(defn session-404
  [sid-str]
  (error-response 404 :session-not-found "unknown session" :session_id (str sid-str)))

(defn group-404
  [gid-str]
  (error-response 404 :group-not-found "unknown session group" :group_id (str gid-str)))

(defn body-json
  [request]
  (some-> (:body request)
          slurp
          wire/parse-json))

(defn path-sid
  [request]
  (some-> (get-in request [:path-params :sid])
          parse-uuid))

(defn query-long
  "Long value of query param `k`, or nil when it is absent, blank or unparsable.

  Ring hands back a VECTOR when a param repeats (`?limit=1&limit=2`), so read the
  LAST value — a duplicated param is a client bug, not a ClassCastException.

  Range policy belongs to the caller: 0 and negatives come back as themselves."
  [request k]
  (let [v (get-in request [:query-params k])]
    (some-> (if (sequential? v) (last v) v)
            str
            str/trim
            not-empty
            parse-long)))

(defn query-str
  "Trimmed string value of query param `k`, or nil when it is absent or blank.
   Same duplicate-param rule as [[query-long]]: the LAST value wins."
  [request k]
  (let [v (get-in request [:query-params k])]
    (some-> (if (sequential? v) (last v) v)
            str
            str/trim
            not-empty)))

(def ^:private archived-views
  "The ONE archive vocabulary every list route of this gateway reads: `exclude`
   answers the active items, `include` answers both and `only` answers the
   archive alone - what a reveal asks for. A boolean could not say which of the
   three a reader meant, so the parameter names the VIEW instead."
  {"exclude" :exclude "include" :include "only" :only})

(defn query-archived
  "The archive view a request asks for, or `:invalid` when it names one this
   gateway does not have. Absent or blank is `:exclude`: a caller that never
   heard of the archive reads the active list."
  [request]
  (if-let [v (query-str request "archived")]
    (get archived-views (str/lower-case v) :invalid)
    :exclude))

(defn archived-400
  "The answer to an `archived` parameter naming a view that does not exist."
  []
  (error-response 400 :invalid-archived "archived must be exclude, include or only"))

(defn session-archived-409
  "The answer to a WRITE aimed at an archived session. The archive is READ-ONLY and
   the gateway is where that holds: the app and the TUI disable their controls, so a
   human never learns it from a refusal, but a stale screen or an SDK caller reaches
   these routes anyway."
  [sid]
  (error-response 409
                  :session-archived
                  "this session is archived and read-only - unarchive it to keep working"
                  :session_id (str sid)))

(defn session-busy-409
  "The answer to an archive aimed at work in flight: a running turn, a turn queued
   behind it, or an input prompt parked on the operator. `sid` names the session that
   is busy, which for a GROUP is the member holding the whole shelf up."
  [sid]
  (error-response 409
                  :session-busy "this session is still working - archive it once its turn is done"
                  :session_id (str sid)))

(defn weak-etag
  "WEAK conditional-GET validator over `parts`: SHA-256 of their canonical JSON.

   Weak because the callers hash CONTENT and drop per-request clock samples
   (`server_time_ms`) — two answers may differ in bytes while rendering
   identically, which is exactly the case a 304 exists for."
  ^String [parts]
  (str "W/\"" (subs (util/sha256-hex (wire/json-str parts)) 0 32) "\""))

(defn query-session-ids
  "A comma-separated list of session ids from one query parameter, as a set.

   Absent, blank and \"only commas\" all mean the empty set: an overlay a device
   does not have is not an error, it is simply no overlay."
  [request k]
  (into #{}
        (comp (map str/trim) (remove str/blank?))
        (str/split (str (get-in request [:query-params k])) #",")))
