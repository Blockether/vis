(ns com.blockether.vis.internal.gateway.server.sessions
  "Session routes: create, list and search sessions, and read or change one
   session's metadata, model, usage, context, forks, alerts, slash commands,
   suggestions and background resources."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.channel.file-picker :as file-picker]
            [com.blockether.vis.internal.channel.slash :as slash]
            [com.blockether.vis.internal.extension.client :as client-extensions]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.resources :as resources]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.view :as gw-view]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(def ^:private web-native-slashes
  [{:name "/help" :doc "Show the available slash commands."}
   {:name "/new-session" :doc "Create and open a new session. Optional text starts its first turn."}
   {:name "/sessions" :doc "Return to the session list."}])

(defn- slashes-handler
  "GET /v1/sessions/:sid/slashes[?channel=web|tui] — load Python extensions in
   the gateway on first demand, then resolve slash/template discovery in the active
   session workspace. The omitted channel defaults to Companion's web catalog."
  [request]
  (if-let [sid (http/path-sid request)]
    (if-let [info (state/session-workspace-info sid)]
      (let [root (or (get info "root") (:root info))
            requested (get-in request [:query-params "channel"])
            channel (case requested
                      nil
                      :web

                      "web"
                      :web

                      "tui"
                      :tui

                      nil)]

        (if channel
          (extension/with-context
            {:env {:session-id sid :workspace/root root}}
            (python-extensions/ensure-python-extensions-loaded!)
            (http/json-response
              {:commands (slash/slash-palette channel (when (= :web channel) web-native-slashes))}))
          (http/error-response 400 :invalid-request "channel must be web or tui")))
      (http/session-404 (get-in request [:path-params :sid])))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- create-session-handler
  "POST /v1/sessions — mint one session.

   An optional `group_id` STARTS it inside that session group, so a client that
   offered the verb ON a group never has to file the row a beat after it appears
   (BLO-167). An unknown group refuses the create instead of quietly making a
   loose session somewhere else in the project."
  [request]
  (let [body
        (http/body-json request)

        raw
        (get body "group_id")

        gid
        (some-> (not-empty (str raw))
                parse-uuid)]

    (cond (and (not (str/blank? (str raw))) (nil? gid))
          (http/error-response 400 :invalid-request "group_id must be a session group id")
          (and gid (nil? (state/get-session-group gid))) (http/group-404 (str raw))
          :else (http/json-response 201
                                    (state/create-session!
                                      (cond-> {:channel (some-> (get body "channel")
                                                                keyword)
                                               :title (get body "title")
                                               :external-id (get body "external_id")
                                               :workspace-id (get body "workspace_id")
                                               :root (get body "root")}
                                        gid
                                        (assoc :group-id gid)))))))

(defn- sessions-etag
  "Conditional-GET validator for a session-list ANSWER: SHA-256 over the rows
   MINUS `server_time_ms`, together with the window frame (total/limit/root and
   the cursor it was cut after) that identifies WHICH answer they are.

   `total` inside the validator is what lets an unchanged FIRST window prove the
   whole list: a session created or deleted anywhere changes the count, and a
   session that gains content ranks to the very top and so lands in this window.
   A client holding a full list can therefore stop after one 304 instead of
   revalidating every window behind it (see `GatewayClient.listSessions`).

   `server_time_ms` is a per-request clock sample (`state/soul` takes it fresh on
   every call), so hashing it would make every poll a guaranteed miss and no
   client could ever revalidate. WEAK (`W/`) for exactly that reason: the bytes
   may differ between two answers, the CONTENT a client renders cannot."
  ^String [payload]
  (http/weak-etag [(:total payload) (:limit payload) (:root payload) (:after payload)
                   (mapv #(dissoc % "server_time_ms") (:sessions payload))
                   ;; The parked strip and project totals ride the same HEAD answer,
                   ;; so both must ride its validator. Either can change while the
                   ;; visible session rows remain byte-for-byte identical.
                   (mapv #(dissoc % "server_time_ms") (:awaiting payload))
                   (some-> (:overview payload)
                           (dissoc :server_time_ms))]))

(def ^:private default-session-window
  "Rows a list read is answered with when it named neither a window nor a cut. Nothing
   paints a thousand rows at once, and building them cost ~825KB and ~450ms of gateway
   time per ask; a client that wants more walks `next_cursor`."
  20)

(defn- list-sessions-handler
  "GET /v1/sessions[?limit=&after=&root=&project_id=&id_prefix=&ids=&grouped=] — sessions in
   navigator order, WINDOWED, with a validator so a poller can revalidate instead of
   re-downloading.

   A read that names NO window and NO cut is answered with the head window
   (`default-session-window`), never the fleet: that answer was ~825KB built in ~450ms
   and no surface paints a thousand rows. A caller that wants the tail walks
   `next_cursor`; a caller that wants a NARROW answer names its cut (`project_id` for
   one project's tab set, `id_prefix` for the session a short id names, `ids` for the
   rows a picker's window does not hold) and is bounded by its own question. The reply
   carries `total` / `limit` / `next_cursor` / `has_more` and the gateway only decorates
   the rows it returns: the ordering is derived from cheap facts (see
   `state/list-sessions-page`), so a 100-row first page of a 448-session store costs
   roughly a fifth of the ~257ms full build and a fifth of its ~300KB — the app paints
   its first screen without waiting for the tail.

   The HEAD also carries `overview`: every project's stable counts from the same
   gateway snapshot. Rows and totals therefore land in one response and one React
   patch instead of racing two requests and repainting the list as pages arrive.
   Tail windows omit it; the head is always read first.

   A window is addressed by a CURSOR, never an offset: `after` is the
   `next_cursor` of the previous answer, which names the last ROW the client
   holds, so the page after it is the same page however much the fleet moved in
   between. Offsets indexed an ordering recomputed per request, which duplicated
   one row and dropped another whenever a turn landed mid-walk. A cursor that is
   PRESENT but unparsable is a 400, exactly like a bad `limit`, never a silent
   fallback to the head of the list.

   `dirty` names the sessions holding words the ASKING device has typed and not
   sent - the one part of the navigator list this process cannot see. They are
   listed even when otherwise empty and band above the rest, so the window a device
   walks is the list it paints, and its page count is the gateway's own.

   `awaiting` carries the sessions parked on an unanswered input View,
   complete and OUTSIDE the window, so a client pins them above a list that no
    longer reorders itself when a turn starts or ends.

   `grouped=aside` does the same for the sessions a human has FILED in a group:
   they leave the window and come back complete under `grouped`, while `total`,
   `next_cursor` and `has_more` describe the loose sessions the pager is walking.
   A group is a shelf the client paints whole, so paging it was hiding rows the
   human had just filed.

   `group_limit`/`group_offset` name the PAGE OF BANDS a client is painting — the
   same window `/v1/session-groups` cuts — and the shelves under `grouped` follow
   it. A project with a wall of groups is then a bounded first paint, and the two
   reads agree by construction instead of by timing. Without them every filed
   session comes back, as before.

   `group_id` cuts the same list to ONE group's sessions: with `archived=only` it
   answers the rows a human archived INSIDE a group that is still active, which is
   what a group's own reveal asks for.

   The companion refreshes this on a timer and the payload is BIG while the
   content is identical until a turn moves. With `If-None-Match` the steady state
   collapses to a 304 with no body: nothing transferred, nothing parsed, nothing
   reconciled, nothing repainted."
  [request]
  (let [given?
        (fn [k]
          (some? (get-in request [:query-params k])))

        limit
        (http/query-long request "limit")

        after
        (some-> (get-in request [:query-params "after"])
                str
                not-empty)

        ;; One PROJECT's window. The companion pages a project header in place, and a
        ;; page it slices locally is not a page: it needs the whole fleet downloaded
        ;; first. With `root` the gateway cuts the ordering to that project, so
        ;; `total`/`has_more` describe the project the pager is printing.
        ;;
        ;; A root that is PRESENT and BLANK is a cut of its own: the sessions no project
        ;; holds, which the overview counts under the same blank root and a client paints
        ;; as `No project`. Dropped to nil, that shelf was answered with the whole fleet -
        ;; one session in its header over 128 pages of every other project's work.
        root
        (when (given? "root") (str (get-in request [:query-params "root"])))

        ;; The TUI asks this list two narrow questions - ONE project's tab set, and the
        ;; session a short id names - so both are CUTS of the gateway's ordering here
        ;; instead of a fleet download the channel filters afterwards.
        project-id
        (some-> (get-in request [:query-params "project_id"])
                str
                not-empty)

        id-prefix
        (some-> (get-in request [:query-params "id_prefix"])
                str
                not-empty)

        ;; ONE group's sessions, so a group shelf can reveal its own archive
        ;; without the fleet being downloaded and sliced on the device.
        group-id
        (some-> (get-in request [:query-params "group_id"])
                str
                not-empty)

        dirty
        (http/query-session-ids request "dirty")

        ;; The rows a SET of ids names. Session search is ranked over the whole store, so
        ;; a hit can sit outside the window a picker holds; it asks for those rows here
        ;; instead of downloading the fleet to find one of them.
        ids
        (http/query-session-ids request "ids")

        ;; Which sessions stand BESIDE the window. `aside` lifts the filed rows out of
        ;; the page, so a group shelf is complete however deep its sessions sit.
        grouped
        (some-> (get-in request [:query-params "grouped"])
                str
                not-empty)

        ;; The page of BANDS the client is painting. The shelves `aside` answers
        ;; follow this window, so a wall of groups is not a wall of shelves on
        ;; the first paint.
        group-limit
        (http/query-long request "group_limit")

        group-offset
        (http/query-long request "group_offset")

        ;; A read that named neither a window nor a cut asked for every session in the
        ;; store. The head window is what it gets, and `next_cursor` carries anyone who
        ;; wants the rest.
        window-limit
        (if (or (given? "limit")
                (some? root)
                (seq project-id)
                (seq id-prefix)
                (seq ids)
                (seq group-id))
          limit
          default-session-window)

        ;; WHICH VIEW of the list: the active sessions (the default), the archive
        ;; alone - what a reveal asks for - or both.
        archived
        (http/query-archived request)

        ;; The two ways this read is refused before it costs anything: an archive
        ;; view this gateway does not have, and a window nobody can cut.
        refusal
        (cond (= :invalid archived) (http/archived-400)
              (or (and (given? "limit") (nil? limit))
                  (and (some? after) (nil? (state/parse-session-cursor after))))
              (http/error-response
                400
                :invalid-window
                "limit must be an integer and after must be a <band>:<key>:<id> cursor"))]

    (if refusal
      refusal
      (let [;; The bands that window names, resolved ONCE: the shelves beside the
            ;; window are cut to exactly the groups the client is painting. Only a
            ;; read that asked for a band window pays for this.
            group-ids
            (when (and (= "aside" grouped) (some? group-limit))
              (let [pid (or (some-> project-id
                                    parse-uuid)
                            (some-> (when (seq root) (state/get-project-by-root "local" root))
                                    (get "id")
                                    parse-uuid))]
                (when pid
                  (mapv #(get % "id")
                        (:groups (state/list-session-groups-page pid
                                                                 {:archived archived
                                                                  :limit group-limit
                                                                  :offset group-offset}))))))

            page
            (state/list-sessions-page :all
                                      {:limit window-limit
                                       :after after
                                       :root root
                                       :project-id project-id
                                       :id-prefix id-prefix
                                       :group-id group-id
                                       :group-ids group-ids
                                       :ids ids
                                       :dirty dirty
                                       :grouped grouped
                                       :archived archived})

            payload
            (cond-> {:sessions (:sessions page)
                     ;; Beside the window, never inside it: `state/list-sessions-page`.
                     :awaiting (:awaiting page)
                     :root root
                     :total (:total page)
                     :limit (:limit page)
                     ;; Echoed so the validator identifies WHICH window these rows are, and
                     ;; handed on so the next request is the client's own last row.
                     :after after
                     :next-cursor (:next-cursor page)
                     :has-more (:has-more page)}
              (seq grouped)
              (assoc :grouped (:grouped page))

              ;; One request owns the first paint. Recomputing this for every tail
              ;; window would add work while conveying the same fleet-wide fact.
              (nil? after)
              (assoc :overview (state/projects-overview :all dirty)))

            etag
            (sessions-etag payload)

            base
            {"ETag" etag "Cache-Control" "no-cache"}]

        (if (= etag (get-in request [:headers "if-none-match"]))
          {:status 304 :headers base :body nil}
          (update (http/json-response payload) :headers merge base))))))

(defn- search-sessions-handler
  "GET /v1/sessions/actions/search?q=&channel= — the answer to `q`, in the
   LIST's own order.

   The SERVER decides that order: `matches` carries every session whose title or
   transcript matches, FRESHEST first, each with the `rank` band it earned (title
   0, request 1, reply 2, thinking 3) and `is_in_title`. A running session is not
   lifted over that - a band that flips mid-turn moves results under the reader's
   finger. Searching narrows the session list without reshuffling it, so
   the dates a client paints only ever fall as it scans down; `rank` says WHERE
   the query hit. Clients PAINT this order and never re-derive one from the
   flags, so a third client cannot invent a fourth ordering. `session_ids`
   mirrors it."
  [request]
  (let [q
        (str (get-in request [:query-params "q"]))

        channel
        (or (some-> (get-in request [:query-params "channel"])
                    keyword)
            :all)

        ;; ONE search per request: `session_ids` is derived from the matches
        ;; instead of re-running the identical (previously full-table) scan.
        matches
        (state/search-session-matches channel q)]

    (http/json-response {:session_ids (mapv :session_id matches) :matches matches})))

(defn- soul-handler
  [request]
  (let [sid
        (http/path-sid request)

        include-queued?
        (= "queued" (get-in request [:query-params "include"]))]

    (if-let [soul (some-> sid
                          state/soul)]
      (http/json-response (cond-> soul
                            include-queued?
                            (assoc :queued_turns
                              (state/list-queued-turns sid) :queue-paused
                              (state/queue-paused-info sid))))
      (http/session-404 (get-in request [:path-params :sid])))))

(defn- patch-session-handler
  "PATCH /v1/sessions/:sid - star (`{is_favorite}`), archive (`{archived}`),
   rename (`{title}`) OR change project membership (`{project_id}`, null to
   remove from project). The star is checked first, then the archive, then
   membership."
  [request]
  (let [sid
        (http/path-sid request)

        body
        (http/body-json request)]

    (cond (not sid) (http/session-404 (get-in request [:path-params :sid]))
          ;; The star is a STATE the human sets, not an event: the request carries
          ;; the intent (`is_favorite`) and the soul that comes back carries the
          ;; `favorite_rank` the gateway allocated for it. Backend-owned, so the
          ;; other devices on this gateway see the same star without being told.
          (contains? body "is_favorite")
          (if-let [soul (state/set-favorite! sid (boolean (get body "is_favorite")))]
            (http/json-response soul)
            (http/session-404 (get-in request [:path-params :sid])))
          ;; The ARCHIVE is the same kind of state as the star above: the request
          ;; carries the intent, the soul that comes back carries the stamp this
          ;; gateway wrote, and every other device sees the session leave the list
          ;; without being told about this call.
          ;;
          ;; A session still WORKING cannot be put away: the turn would keep running with
          ;; nothing in any list naming it, and cancelling it behind a swipe verb would be
          ;; worse. Unarchiving is never refused.
          (contains? body "archived") (let [archived? (boolean (get body "archived"))]
                                        (if (and archived? (state/session-working? sid))
                                          (http/session-busy-409 sid)
                                          (if-let [soul (state/set-archived! sid archived?)]
                                            (http/json-response soul)
                                            (http/session-404 (get-in request
                                                                      [:path-params :sid])))))
          (contains? body "project_id") (if-let [soul (state/assign-project!
                                                        sid
                                                        (some-> (get body "project_id")
                                                                parse-uuid))]
                                          (http/json-response soul)
                                          (http/session-404 (get-in request [:path-params :sid])))
          (str/blank? (str (get body "title")))
          (http/error-response 400 :invalid-request "title must be a non-blank string")
          :else (if-let [soul (state/set-title! sid (get body "title"))]
                  (http/json-response soul)
                  (http/session-404 (get-in request [:path-params :sid]))))))

(defn- delete-session-handler
  [request]
  (when-let [sid (http/path-sid request)]
    (client-extensions/detach! sid)
    (state/close-session! sid))
  {:status 204 :headers {} :body nil})

(defn- release-session-handler
  "POST /v1/sessions/:sid/release — a client closed its VIEW of the session
   (TUI tab/exit). Releases the live runtime and stops the session's background
   resources; the persisted transcript stays resumable. Idempotent, 204 always
   (mirrors DELETE — releasing an unknown sid is a no-op, not an error)."
  [request]
  (some-> (http/path-sid request)
          state/release-session!)
  {:status 204 :headers {} :body nil})

(defn- mark-session-read-handler
  "PUT /v1/sessions/:sid/read {seen_answers} — how far this reader has read the
   conversation. An absent or null `seen_answers` means ALL of it, which is what a
   surface opening the session reports. The watermark never moves backwards, and
   the receipt carries the count the gateway now holds, so a caller repaints the
   row from the gateway's own answer instead of counting answers itself."
  [request]
  (let [sid-str
        (get-in request [:path-params :sid])

        sid
        (http/path-sid request)

        raw
        (get (http/body-json request) "seen_answers")

        text
        (str/trim (str raw))

        seen
        (cond (number? raw) (max 0 (long raw))
              (re-matches #"\d+" text) (parse-long text)
              :else nil)]

    (cond (or (nil? sid) (nil? (state/soul sid))) (http/session-404 sid-str)
          (and (some? raw) (nil? seen)) (http/error-response
                                          400
                                          :invalid-request
                                          "seen_answers must be a whole number of answers")
          :else (http/json-response (state/mark-session-read! sid {:seen-answers seen})))))

(defn- context-handler
  [request]
  (if-let [snapshot (some-> (http/path-sid request)
                            state/context-snapshot)]
    (http/json-response snapshot)
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- req-rid
  "Resource id from the request. It rides as the `rid` QUERY PARAM (not a path
   segment) because resource ids can embed absolute paths (e.g. an nREPL id
   `nrepl:/Users/.../ws`); an encoded `/` in a path segment trips Jetty's
   \"Ambiguous URI path separator\" 400."
  [request]
  (get-in request [:query-params "rid"]))

(defn- resources-handler
  "GET /v1/sessions/:sid/resources — the session's live vis-managed resources
   (background `shell` children, managed REPLs, MCP connections, …) FROM THE DAEMON's
   registry. An in-process client reads its own registry directly because
   it runs INSIDE the daemon, but the TUI and remote clients run in a DIFFERENT
   process from the one the agent's tools execute in; without this endpoint they
   read an empty local registry and never learn a background started."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:resources (resources/list-resources sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- resource-stop-handler
  "POST /v1/sessions/:sid/resources/stop?rid=… — run the resource's stop-fn in
   the daemon (the single canonical stop path) and unregister it."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response (resources/stop! sid (req-rid request)))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- resource-logs-handler
  "GET /v1/sessions/:sid/resources/logs?rid=… — captured output lines for a
   background via its logs-fn (nil when the resource has none)."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:lines (resources/logs sid (req-rid request))})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- session-model-handler
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:model (state/session-model-cached sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- set-session-model-handler
  "PATCH /v1/sessions/:sid/model {provider, model} — pin the session to a
   provider+model from THIS gateway's fleet (blank/omitted clears the pin).

   The provider is validated against the PICKER fleet because the gateway OWNS
   the fleet: a client that pins an id this gateway does not serve would silently
   degrade to the default route on every turn while every picker/footer kept
   rendering the phantom pick. Unknown id -> 400 `unknown-provider`.

   `picker-fleet` — not `configured-providers` — is the exact set the TUI picker
   and the companion's `/v1/router` dialog OFFER: it also carries providers that
   are AUTHENTICATED but not yet persisted into `:providers`, and the engine
   routes those too (the router build appends them). Validating against config
   alone answered 400 for a provider the user had just picked from the list.

   The MODEL name is deliberately NOT restricted to the configured names — the
   live catalog (`/v1/providers/:id/models`, the TUI's \"Show all models\")
   legitimately offers models that are not pinned in vis.yml."
  [request]
  (if-let [sid (http/path-sid request)]
    (let [{:strs [provider model]} (http/body-json request)
          pid (some-> provider
                      str
                      str/trim
                      not-empty)
          known (into #{} (map (comp name :id)) (providers/picker-fleet))]

      (cond
        ;; READ-ONLY: pinning a model is a write, and an archived session takes none.
        (state/session-archived? sid) (http/session-archived-409 sid)
        (and pid (not (contains? known pid)))
        (http/error-response 400
                             :unknown-provider
                             (str "provider " pid " is not configured on this gateway")
                             :provider_id pid)
        :else (do (state/set-session-model! sid pid model)
                  (http/json-response {:model (state/session-model sid)}))))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- usage-handler
  "GET the whole-session usage rollup (turns, iterations, tool calls, folds,
   token split, cache hit rate, cost). ON-DEMAND only: it decodes every
   iteration's tool-call BLOB to count tools, so it is never folded into
   `list-sessions`. `{\"usage\" nil}` for a session with no turns yet."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:usage (state/session-usage-info sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- fork-points-handler
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:turns (state/fork-points sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- fork-session-handler
  [request]
  (if-let [sid (http/path-sid request)]
    (let [{through "through_turn_id"} (http/body-json request)]
      (try (http/json-response {:session (state/fork-session! sid through)})
           (catch clojure.lang.ExceptionInfo e
             (http/error-response 409 (:type (ex-data e) :session/fork-failed) (ex-message e)))))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- session-alert-handler
  "GET /v1/sessions/:sid/alert - the banner this session would raise right now,
   `{\"title\": …, \"body\": …}`, worded by the one place that words alerts:
   [[push/answer-alert]] and [[push/question-alert]].

   The desktop app cannot be pushed to at all - its WKWebView has no
   `PushManager` - so it raises its own alerts from the fleet it already polls,
   and asks HERE what they say. `?reason=question` is a run parked on a human;
   anything else is its answer."
  [request]
  (let [sid
        (http/path-sid request)

        session
        (when sid (state/soul sid))]

    (if session
      (http/json-response (if (= "question" (get-in request [:query-params "reason"]))
                            (push/question-alert (first (gw-view/input-views sid)))
                            (push/answer-alert {:title (get session "title")
                                                :answer (state/newest-answer-text sid)})))
      (http/session-404 (get-in request [:path-params :sid])))))

(defn- suggest-handler
  "GET /v1/sessions/:sid/suggest?kind=file&q=&limit=20. Search the session's
   workspace, not the daemon's working directory. Returns wire rows
   {:name :size :age :status}; limit is clamped to 1–1000 so attachment pickers
   can request more candidates than the inline @ overlay. Trigger handling
   and attachment admission remain client-side."
  [request]
  (let [sid (http/path-sid request)]
    (if-not (and sid (state/soul sid))
      (http/session-404 (get-in request [:path-params :sid]))
      (let [kind (or (http/query-str request "kind") "file")
            q (or (http/query-str request "q") "")
            limit (max 1 (min 1000 (long (or (http/query-long request "limit") 20))))]

        (if (= "file" kind)
          (if-let [workspace-root (get (state/session-workspace-info sid) "root")]
            (binding [workspace/*workspace-root* workspace-root]
              (http/json-response (file-picker/suggest-file-rows q {:limit limit})))
            (http/error-response 409 :workspace-unavailable "Session workspace is unavailable."))
          (http/error-response 400 :invalid-request (str "unknown suggest kind: " kind)))))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/sessions"] list-sessions-handler
   [:post "/v1/sessions"] create-session-handler
   [:get "/v1/sessions/actions/search"] search-sessions-handler
   [:get "/v1/sessions/:sid"] soul-handler
   [:patch "/v1/sessions/:sid"] patch-session-handler
   [:delete "/v1/sessions/:sid"] delete-session-handler
   [:put "/v1/sessions/:sid/read"] mark-session-read-handler
   [:get "/v1/sessions/:sid/slashes"] slashes-handler
   [:post "/v1/sessions/:sid/release"] release-session-handler
   [:get "/v1/sessions/:sid/alert"] session-alert-handler
   [:get "/v1/sessions/:sid/context"] context-handler
   [:get "/v1/sessions/:sid/resources"] resources-handler
   [:post "/v1/sessions/:sid/resources/stop"] resource-stop-handler
   [:get "/v1/sessions/:sid/resources/logs"] resource-logs-handler
   [:get "/v1/sessions/:sid/model"] session-model-handler
   [:patch "/v1/sessions/:sid/model"] set-session-model-handler
   [:get "/v1/sessions/:sid/usage"] usage-handler
   [:get "/v1/sessions/:sid/forks"] fork-points-handler
   [:post "/v1/sessions/:sid/forks"] fork-session-handler
   [:get "/v1/sessions/:sid/suggest"] suggest-handler})
