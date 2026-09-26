(ns com.blockether.vis.internal.gateway.server.projects
  "Project and session-group routes."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]))

(defn- path-pid
  [request]
  (some-> (get-in request [:path-params :pid])
          parse-uuid))

(defn- project-404
  [pid-str]
  (http/error-response 404 :project-not-found "unknown project" :project_id (str pid-str)))

(defn- projects-overview-handler
  "GET /v1/projects/overview — every project this gateway holds with its own
   counts, and the gateway's totals beside them, in ONE answer.

   A client used to build this by downloading the fleet and tallying it locally,
   so project headers and their numbers only settled once every session window
   had landed — switching gateways repainted the header row page by page. The
   gateway already holds the facts; it answers with them (`state/projects-overview`).

   `dirty` is the same overlay the session list takes (see
   `list-sessions-handler`), so a header's count and the pages under it are one
   number: without it a device holding unsent words in an otherwise empty session
   would read a header that its own pages disagree with.

   Carries the same weak ETag contract as the session list, because a header row
   is polled on a timer and is identical between polls until a turn moves."
  [request]
  (let [payload
        (state/projects-overview :all (http/query-session-ids request "dirty"))

        etag
        (http/weak-etag [(:project_count payload) (:session_count payload) (:live_count payload)
                         (:awaiting_count payload) (:unread_count payload) (:projects payload)])

        base
        {"ETag" etag "Cache-Control" "no-cache"}]

    (if (= etag (get-in request [:headers "if-none-match"]))
      {:status 304 :headers base :body nil}
      (update (http/json-response payload) :headers merge base))))

(defn- list-projects-handler
  "GET /v1/projects[?owner=…&archived=exclude|include|only] — the owner's
   projects (projects are CROSS-CHANNEL), each with a live session_count."
  [request]
  (let [owner
        (not-empty (get-in request [:query-params "owner"]))

        archived
        (http/query-archived request)]

    (if (= :invalid archived)
      (http/archived-400)
      (http/json-response {:projects (state/list-projects (cond-> {:archived archived}
                                                            owner
                                                            (assoc :owner-id owner)))}))))

(defn- create-project-handler
  "POST /v1/projects {name, color?, owner_id?, root?} — create a (cross-channel) project."
  [request]
  (let [{:strs [name color owner_id root]} (http/body-json request)]
    (if (str/blank? (str name))
      (http/error-response 400 :invalid-request "name must be a non-blank string")
      (http/json-response 201
                          (state/create-project! (cond-> {:name name}
                                                   color
                                                   (assoc :color color)

                                                   (not (str/blank? (str root)))
                                                   (assoc :workspace-root root)

                                                   owner_id
                                                   (assoc :owner-id owner_id)))))))

(defn- ensure-project-for-root-handler
  "POST /v1/projects/actions/ensure {root, name?, owner_id?} — get-or-create the project
   bound to a canonical workspace root. A project IS a TUI tab set; this is the
   launch-dir -> project resolution. Idempotent (safe under concurrent TUIs)."
  [request]
  (let [{:strs [root name owner_id]} (http/body-json request)]
    (if (str/blank? (str root))
      (http/error-response 400 :invalid-request "root must be a non-blank string")
      (http/json-response
        (state/ensure-project-for-root! (or (not-empty owner_id) "local") root name)))))

(defn- get-project-handler
  [request]
  (let [pid-str (get-in request [:path-params :pid])]
    (if-let [p (some-> (path-pid request)
                       state/get-project)]
      (http/json-response p)
      (project-404 pid-str))))

(defn- patch-project-handler
  "PATCH /v1/projects/:pid {name?, color?, position?, archived?} — patch a project."
  [request]
  (let [pid-str
        (get-in request [:path-params :pid])

        pid
        (path-pid request)

        body
        (http/body-json request)

        opts
        (cond-> {}
          (contains? body "name")
          (assoc :name (get body "name"))

          (contains? body "color")
          (assoc :color (get body "color"))

          (contains? body "position")
          (assoc :position (get body "position"))

          (contains? body "archived")
          (assoc :archived? (boolean (get body "archived"))))]

    (cond (not pid) (project-404 pid-str)
          (and (contains? opts :name) (str/blank? (str (:name opts))))
          (http/error-response 400 :invalid-request "name must be a non-blank string")
          (empty? opts) (http/error-response 400 :invalid-request "no project fields to update")
          :else (if-let [p (state/update-project! pid opts)]
                  (http/json-response p)
                  (project-404 pid-str)))))

(defn- delete-project-handler
  "DELETE /v1/projects/:pid[?is_recursive=true] — by default member sessions
   scatter back to project-less (204, body-less).

   `is_recursive=true` DELETES every member session (and its draft clones) before
   dropping the project row, and answers 200 with `{project_id,
   deleted_session_ids, session_count}`: a client needs those ids to prune local
   state (rows, snapshots, unsent drafts) without racing a re-read."
  [request]
  (let [pid-str
        (get-in request [:path-params :pid])

        pid
        (path-pid request)]

    (if (= "true" (get-in request [:query-params "is_recursive"]))
      (if pid
        (http/json-response (state/delete-project! pid {:is-recursive true}))
        (project-404 pid-str))
      (do (some-> pid
                  state/delete-project!)
          {:status 204 :headers {} :body nil}))))

(defn- reorder-project-sessions-handler
  "PATCH /v1/projects/:pid/sessions {order:[sid…]} — persist the manual order of
   the sessions (TUI tabs) inside a project so they stay MOVABLE cross-channel.
   LOOSE sessions named in `order` are ADOPTED into the project atomically; guests
   owned by another project are never stolen."
  [request]
  (let [pid-str
        (get-in request [:path-params :pid])

        pid
        (path-pid request)

        order
        (->> (get (http/body-json request) "order")
             (keep #(some-> %
                            str
                            parse-uuid))
             vec)]

    (cond (not pid) (project-404 pid-str)
          (empty? order) (http/error-response 400
                                              :invalid-request
                                              "order must be a non-empty array of session ids")
          :else (let [count (state/reorder-project-sessions! pid order)]
                  (http/json-response {:project_id (str pid) :count count})))))

(defn- path-gid
  [request]
  (some-> (get-in request [:path-params :gid])
          parse-uuid))

(defn- list-session-groups-handler
  "GET /v1/session-groups?project=<pid>|root=<path>[&owner=…&archived=exclude|include|only]
                         [&limit=<n>&offset=<n>]
   — the groups inside ONE project, in the order the human put them in.

   `root` is accepted because a client can group its list by WORKSPACE ROOT
   without ever holding a project id (the companion does). A root with no project
   yet simply has no groups — a READ never creates one.

   An archived group leaves this list the way an archived session leaves the
   session list, and `archived=only` is the reveal that asks for the archive
   alone.

   `limit`/`offset` cut ONE page out of a wall of bands a human can keep growing,
   and `total`/`has_more` are what a pager over them prints, with `session_total`
   counting the sessions filed across the WHOLE wall. Their order is the human's own
   `position`, which no turn moves, so an offset names the same page
   tomorrow. Without `limit` the answer is every band, unchanged — what the TUI
   reads."
  [request]
  (let [asked
        (not-empty (get-in request [:query-params "project"]))

        root
        (not-empty (get-in request [:query-params "root"]))

        owner
        (or (not-empty (get-in request [:query-params "owner"])) "local")

        archived
        (http/query-archived request)

        ;; ONE page of bands: a client painting a wall of groups asks for its
        ;; window here instead of receiving the wall and cutting it on the device.
        limit
        (http/query-long request "limit")

        offset
        (http/query-long request "offset")

        pid
        (or (some-> asked
                    parse-uuid)
            (some-> (when root (state/get-project-by-root owner root))
                    (get "id")
                    parse-uuid))]

    (cond (= :invalid archived) (http/archived-400)
          (and (nil? asked) (nil? root))
          (http/error-response 400 :invalid-request "project or root is required")
          (nil? pid) (http/json-response {:project_id nil
                                          :groups []
                                          :total 0
                                          :session_total 0
                                          :limit limit
                                          :offset (max 0 (long (or offset 0)))
                                          :has-more false})
          :else (let [page (state/list-session-groups-page
                             pid
                             {:archived archived :limit limit :offset offset})]
                  (http/json-response {:project_id (str pid)
                                       :groups (:groups page)
                                       :total (:total page)
                                       :session_total (:session-total page)
                                       :limit (:limit page)
                                       :offset (:offset page)
                                       :has-more (:has-more page)})))))

(defn- create-session-group-handler
  "POST /v1/session-groups {name, color?, position?, project_id?|root?, owner_id?}
   — create a group inside one project. With `root` the project is get-or-created
   first, so a client that groups by workspace root never resolves an id itself."
  [request]
  (let [{:strs [name color position project_id root owner_id]}
        (http/body-json request)

        owner
        (or (not-empty (str owner_id)) "local")

        project
        (cond (not (str/blank? (str project_id))) (some-> (parse-uuid (str project_id))
                                                          state/get-project)
              (not (str/blank? (str root))) (state/ensure-project-for-root! owner root nil)
              :else nil)

        pid
        (some-> (get project "id")
                parse-uuid)]

    (cond (str/blank? (str name))
          (http/error-response 400 :invalid-request "name must be a non-blank string")
          (and (some? color) (not (gateway-contract/session-group-color? color)))
          (http/error-response 400
                               :invalid-request
                               "color must be one of the closed group palette tokens"
                               :colors gateway-contract/session-group-colors)
          (nil? pid)
          (http/error-response 400 :invalid-request "project_id or root must name a project")
          :else (if-let [group (try (state/create-session-group! pid
                                                                 (cond-> {:name name}
                                                                   color
                                                                   (assoc :color color)

                                                                   position
                                                                   (assoc :position position)))
                                    ;; the UNIQUE(project_id, name) index is the whole
                                    ;; check: a group is addressed by the name a human
                                    ;; typed, so a second one is a conflict, not a 500.
                                    (catch Exception _ nil))]
                  (http/json-response 201 group)
                  (http/error-response 409
                                       :group-exists
                                       "a group with that name already exists in this project"
                                       :name (str name))))))

(defn- patch-session-group-handler
  "PATCH /v1/session-groups/:gid {name?, color?, position?, archived?} — rename,
   recolour, reorder or archive a group. `color` is a palette TOKEN, never a hex
   string.

   Archiving a group takes its sessions out of sight WITH it and stamps none of
   them: the shelf is what the human filed them on, so unarchiving the group
   brings back exactly the rows it hid."
  [request]
  (let [gid-str
        (get-in request [:path-params :gid])

        gid
        (path-gid request)

        body
        (http/body-json request)

        opts
        (cond-> {}
          (contains? body "name")
          (assoc :name (get body "name"))

          (contains? body "color")
          (assoc :color (get body "color"))

          (contains? body "position")
          (assoc :position (get body "position"))

          (contains? body "archived")
          (assoc :archived? (boolean (get body "archived"))))]

    (cond (or (not gid) (nil? (state/get-session-group gid))) (http/group-404 gid-str)
          (and (contains? opts :name) (str/blank? (str (:name opts))))
          (http/error-response 400 :invalid-request "name must be a non-blank string")
          (and (contains? opts :color) (not (gateway-contract/session-group-color? (:color opts))))
          (http/error-response 400
                               :invalid-request
                               "color must be one of the closed group palette tokens"
                               :colors gateway-contract/session-group-colors)
          (empty? opts) (http/error-response 400 :invalid-request "no group fields to update")
          ;; Archiving the shelf archives everything standing on it, so ONE member still
          ;; working refuses the whole group - and the answer names it. Unarchiving is
          ;; never refused.
          :else
          (if-let [busy (when (:archived? opts) (state/busy-session-in-group gid))]
            (http/session-busy-409 busy)
            (if-let [group (try (state/update-session-group! gid opts) (catch Exception _ nil))]
              (http/json-response group)
              (http/error-response 409
                                   :group-exists
                                   "a group with that name already exists in this project"))))))

(defn- delete-session-group-handler
  "DELETE /v1/session-groups/:gid[?sessions=detach|delete] — drop a group and say
   what becomes of its members. `sessions=detach` (the default) leaves every
   session in the project, ungrouped; `sessions=delete` deletes them together with
   the group. Answers `{group_id, scattered_session_ids, deleted_session_ids,
   session_count}` so a client can prune local state without racing a re-read."
  [request]
  (let [gid-str
        (get-in request [:path-params :gid])

        gid
        (path-gid request)

        mode
        (or (not-empty (get-in request [:query-params "sessions"])) "detach")]

    (cond (or (not gid) (nil? (state/get-session-group gid))) (http/group-404 gid-str)
          (not (contains? #{"detach" "delete"} mode))
          (http/error-response 400 :invalid-request "sessions must be detach or delete")
          :else (http/json-response (if (= "delete" mode)
                                      (state/delete-session-group-with-sessions! gid)
                                      (state/delete-session-group! gid))))))

(defn- set-session-group-handler
  "PUT /v1/sessions/:sid/group {group_id} — file a session under a group; a null
   `group_id` leaves it ungrouped inside its project. Answers the refreshed soul,
   so the caller repaints the row from the gateway's own answer."
  [request]
  (let [sid-str
        (get-in request [:path-params :sid])

        sid
        (http/path-sid request)

        raw
        (get (http/body-json request) "group_id")

        gid
        (some-> (not-empty (str raw))
                parse-uuid)]

    (cond (or (nil? sid) (nil? (state/soul sid))) (http/session-404 sid-str)
          (and (not (str/blank? (str raw))) (nil? gid))
          (http/error-response 400 :invalid-request "group_id must be a session group id or null")
          (and gid (nil? (state/get-session-group gid))) (http/group-404 (str raw))
          :else (http/json-response (state/assign-session-group! sid gid)))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/projects"] list-projects-handler
   [:post "/v1/projects"] create-project-handler
   [:get "/v1/projects/overview"] projects-overview-handler
   [:post "/v1/projects/actions/ensure"] ensure-project-for-root-handler
   [:get "/v1/projects/:pid"] get-project-handler
   [:patch "/v1/projects/:pid"] patch-project-handler
   [:delete "/v1/projects/:pid"] delete-project-handler
   [:patch "/v1/projects/:pid/sessions"] reorder-project-sessions-handler
   [:get "/v1/session-groups"] list-session-groups-handler
   [:post "/v1/session-groups"] create-session-group-handler
   [:patch "/v1/session-groups/:gid"] patch-session-group-handler
   [:delete "/v1/session-groups/:gid"] delete-session-group-handler
   [:put "/v1/sessions/:sid/group"] set-session-group-handler})
