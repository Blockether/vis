(ns com.blockether.vis.internal.gateway.server.transcripts
  "Transcript routes: a session's transcript as JSON, Markdown or HTML, its
   artifacts, and its activity pages and exports."
  (:require [com.blockether.vis.contract.activity :as activity-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.foundation.transcript :as transcript]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util]
            [ring.core.protocols :as ring-protocols])
  (:import [java.io OutputStream]))

(defn- transcript-handler
  "Transcript rows for a session, optionally WINDOWED: `?limit=` (window size,
  defaulting to the NEWEST rows) and `?offset=` (0-based start in the
  oldest-first list). Without them the whole transcript is returned, so an older
  client is unaffected. The window is sliced BEFORE hydration, so a page costs
  page-sized work instead of session-sized work, and it is ALSO capped in bytes
  (turn count does not bound bytes: one real session's newest 24 turns encode to
  9.5 MB). The reply's `offset` can therefore be HIGHER than the one asked for —
  page from the RETURNED offset, never from your own arithmetic.

  A window param that is PRESENT but unparsable is a 400, never a silent
  fallback: falling back would answer garbage with the whole (40 MB on a big
  session) transcript — the exact cost this endpoint exists to avoid. In-range
  policy is `state/transcript-page`'s: it clamps, so `?limit=0` honestly means
  zero rows."
  [request]
  (if-let [sid (http/path-sid request)]
    (let [given? (fn [k]
                   (some? (get-in request [:query-params k])))
          limit (http/query-long request "limit")
          offset (http/query-long request "offset")]

      (if (or (and (given? "limit") (nil? limit)) (and (given? "offset") (nil? offset)))
        (http/error-response 400 :invalid-window "limit and offset must be integers")
        (let [page (state/transcript-page sid {:limit limit :offset offset})]
          (http/json-response {:turns (:turns page)
                               :total (:total page)
                               :offset (:offset page)
                               :has-more (:has-more page)}))))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- session-artifacts-handler
  "GET /v1/sessions/:sid/artifacts — `{\"artifacts\": [descriptor, …]}` for the
   WHOLE session, oldest turn first, metadata only. A gallery asks this once
   instead of deriving itself from the transcript page it happens to hold, which
   listed only what the reader had already scrolled back to."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:artifacts (state/session-artifacts sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- transcript-md-handler
  "Render a session's user/assistant dialog as Markdown — the canonical
   `transcript->md :dialog` every surface (CLI, web, file export) renders
   through — served as text so a channel can DISPLAY it without re-implementing
   transcript rendering client-side."
  [request]
  (if-let [sid (http/path-sid request)]
    {:status 200
     :headers {"Content-Type" "text/markdown; charset=utf-8"}
     :body (str (transcript/transcript-md (lp/db-info) sid {:mode :dialog}))}
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- transcript-html-handler
  "Render a session's transcript as a STANDALONE HTML document — the canonical
   `transcript->html` every surface (CLI, web, file export) renders through, the
   HTML sibling of `transcript-md-handler`. `:dialog` when `?mode=dialog`, else
   the full forensic report."
  [request]
  (if-let [sid (http/path-sid request)]
    (let [mode (if (= "dialog" (get-in request [:query-params "mode"])) :dialog :full)]
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (str (transcript/transcript-html (lp/db-info) sid {:mode mode}))})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- activity-page-request
  [request export?]
  (let [params
        (:query-params request)

        integer-param
        (fn [key default]
          (if-let [value (get params key)]
            (or (parse-long value)
                (throw (ex-info "Activity page parameters must be integers"
                                {:type :activity/invalid-page})))
            default))

        sid
        (http/path-sid request)

        aid
        (some-> (get-in request [:path-params :aid])
                parse-uuid)

        page
        (when (and sid aid)
          (persistance/db-activity-page (lp/db-info)
                                        sid
                                        (str aid)
                                        (if export?
                                          {}
                                          {:after (integer-param "after" 0)
                                           :limit (integer-param "limit"
                                                                 activity-contract/page-row-limit)
                                           :q (get params "q")})))

        revision
        (integer-param "revision" nil)]

    (when (and page revision (not= revision (get-in page [:history :revision])))
      (throw (ex-info "Activity changed; reload the history and retry"
                      {:type :activity/revision-conflict})))
    {:sid sid :aid (str aid) :page page}))

(defn- activity-error-response
  [error]
  (case (:type (ex-data error))
    :activity/invalid-page
    (http/error-response 400 "invalid_activity_page" (ex-message error))

    :activity/revision-conflict
    (http/error-response 409 "activity_changed" (ex-message error))

    (throw error)))

(defn- activity-page-handler
  [request]
  (try
    (if-let [page (:page (activity-page-request request false))]
      (http/json-response page)
      (http/error-response 404 "activity_not_found" "Activity history not found in this session"))
    (catch clojure.lang.ExceptionInfo error (activity-error-response error))))

(defn- activity-export-handler
  "Stream complete redacted history one page at a time. Never mix revisions or build
   the full export in gateway memory; an interrupted export must be retried."
  [request]
  (try (let [{:keys [sid aid page]} (activity-page-request request true)]
         (if page
           {:status 200
            :headers {"Content-Type" "text/plain; charset=utf-8"
                      "Content-Disposition" (str "attachment; filename=\"activity-" aid ".txt\"")
                      "X-Vis-Activity-Revision" (str (get-in page [:history :revision]))}
            :body
            (reify
              ring-protocols/StreamableResponseBody
                (write-body-to-stream [_ _ output]
                  (let [revision (get-in page [:history :revision])]
                    (loop [current page
                           first? true]

                      (when-not (= revision (get-in current [:history :revision]))
                        (.write ^OutputStream output
                                (util/utf8
                                  "\n\nINCOMPLETE EXPORT: Activity changed. Reload and retry.\n"))
                        (throw (java.io.IOException. "Activity changed during export")))
                      (let [text (activity-contract/copy-text (activity-contract/from-wire
                                                                (wire/->wire current)))
                            ^String chunk (if first? text (subs text (count "ACTIVITY")))]

                        (.write ^OutputStream output (util/utf8 chunk)))
                      (.flush ^OutputStream output)
                      (when-let [after (get-in current [:history :next-after])]
                        (recur (persistance/db-activity-page (lp/db-info) sid aid {:after after})
                               false))))))}
           (http/error-response 404
                                "activity_not_found"
                                "Activity history not found in this session")))
       (catch clojure.lang.ExceptionInfo error (activity-error-response error))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/sessions/:sid/transcript"] transcript-handler
   [:get "/v1/sessions/:sid/artifacts"] session-artifacts-handler
   [:get "/v1/sessions/:sid/transcript.md"] transcript-md-handler
   [:get "/v1/sessions/:sid/transcript.html"] transcript-html-handler
   [:get "/v1/sessions/:sid/activity/:aid"] activity-page-handler
   [:get "/v1/sessions/:sid/activity/:aid/export"] activity-export-handler})
