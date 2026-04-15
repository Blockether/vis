(ns com.blockether.vis.web.routes
  "HTTP routing — GET/POST handlers for sessions and queries."
  (:require [com.blockether.vis.conversations :as conv]
            [com.blockether.vis.web.presenter :as presenter]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]))

;; Deferred require to avoid circular dep — server loads first, routes reference it
(defn- srv-live [] @(requiring-resolve 'com.blockether.vis.web.server/live-status))
(defn- srv-get-session [id] ((requiring-resolve 'com.blockether.vis.web.server/get-session) id))
(defn- srv-create-session! [n] ((requiring-resolve 'com.blockether.vis.web.server/create-session!) n))
(defn- srv-delete-session! [id] ((requiring-resolve 'com.blockether.vis.web.server/delete-session!) id))
(defn- srv-sessions-list [] ((requiring-resolve 'com.blockether.vis.web.server/sessions-list)))
(defn- exec-submit! [sid q] ((requiring-resolve 'com.blockether.vis.web.executor/submit-query!) sid q))

;;; ── Helpers ────────────────────────────────────────────────────────────

(defn- valid-session-id? [id]
  (and id (re-matches #"[a-f0-9\-]{36}" id)))

(defn- parse-form-params [body]
  (into {} (map #(let [[k v] (str/split % #"=" 2)]
                   [(java.net.URLDecoder/decode (or k "") "UTF-8")
                    (java.net.URLDecoder/decode (or v "") "UTF-8")])
               (str/split body #"&"))))

;;; ── Handler ────────────────────────────────────────────────────────────

(defn handler [req]
  (let [uri  (:uri req)
        meth (:request-method req)]
    (cond
      ;; GET / → redirect to last session or create new
      (and (= meth :get) (= uri "/"))
      (let [slist (srv-sessions-list)]
        (if (seq slist)
          {:status 302 :headers {"Location" (str "/s/" (:id (last slist)))}}
          {:status 302 :headers {"Location" "/new"}}))

      ;; GET /new → create session and redirect
      (and (= meth :get) (= uri "/new"))
      (let [sess (srv-create-session! "New Chat")]
        {:status 302 :headers {"Location" (str "/s/" (:id sess))}})

      ;; GET /s/:id → show session (or delete, or poll)
      (and (= meth :get) (str/starts-with? uri "/s/"))
      (let [parts (str/split uri #"/")
            id    (nth parts 2 nil)
            id    (when (valid-session-id? id) id)]
        (cond
          (str/ends-with? uri "/context")
          (if-let [_sess (when id (srv-get-session id))]
            ;; Resolved late via requiring-resolve to dodge the server→routes cycle.
            (let [env          (conv/env-for id)
                  db-latest    (requiring-resolve 'com.blockether.vis.rlm.db/db-latest-var-registry)
                  conv-ref     (:conversation-ref env)
                  db-info      (:db-info env)
                  ;; Pull the persisted var registry straight from SQLite — this
                  ;; replaces the defunct @P workspace view. Each entry has
                  ;; {:value :code :query-id :query-ref :iteration-id :created-at}.
                  var-registry (try (when (and db-info conv-ref) (db-latest db-info conv-ref))
                                    (catch Exception _ nil))
                  vars         (when (seq var-registry)
                                 (->> var-registry
                                      (sort-by first)
                                      (mapv (fn [[sym {:keys [value code]}]]
                                              (let [s (pr-str value)]
                                                {:name  (str sym)
                                                 :value (if (> (count s) 200) (str (subs s 0 197) "...") s)
                                                 :code  code
                                                 :type  (cond
                                                          (nil? value) "nil"
                                                          (map? value) "map"
                                                          (vector? value) "vector"
                                                          (set? value) "set"
                                                          (sequential? value) "seq"
                                                          (string? value) "string"
                                                          (integer? value) "int"
                                                          (float? value) "float"
                                                          (boolean? value) "bool"
                                                          (keyword? value) "keyword"
                                                          :else (.getSimpleName (class value)))})))))]
              {:status 200
               :headers {"Content-Type" "application/json"}
               :body (json/write-json-str (cond-> {:context [] :learnings []}
                                            (seq vars) (assoc :variables vars)))})
            {:status 404 :headers {"Content-Type" "application/json"} :body "{\"error\":\"not found\"}"})

          (str/ends-with? uri "/delete")
          (if (and id ((requiring-resolve 'com.blockether.vis.web.executor/in-flight?) id))
            {:status 409 :headers {"Content-Type" "application/json"}
             :body "{\"error\":\"session has in-flight query\"}"}
            (do (when id (srv-delete-session! id))
                {:status 302 :headers {"Location" "/"}}))

          :else
          (if-let [sess (when id (srv-get-session id))]
            (let [qs     (:query-string req)
                  check  (when qs (some->> (re-find #"check=(\d+)" qs) second parse-long))
                  offset (when qs (some->> (re-find #"offset=(\d+)" qs) second parse-long))]
              (if check
                (let [ready?    (> (count (:messages sess)) check)
                      progress  (get @(srv-live) id)
                      inflight? ((requiring-resolve 'com.blockether.vis.web.executor/in-flight?) id)]
                  {:status 200
                   :headers {"Content-Type" "application/json"}
                   :body (json/write-json-str
                          (cond-> {:ready ready? :inflight inflight?}
                            (:current progress)    (assoc :status (:current progress))
                            (:iterations progress) (assoc :iterations (:iterations progress))))})
                {:status 200
                 :headers {"Content-Type" "text/html; charset=utf-8"}
                 :body (presenter/page id (srv-sessions-list) (:messages sess) {:offset (or offset presenter/page-size)})}))
            {:status 302 :headers {"Location" "/"}})))

      ;; POST /s/:id → async query
      (and (= meth :post) (str/starts-with? uri "/s/"))
      (let [id     (let [raw (nth (str/split uri #"/") 2 nil)]
                     (when (valid-session-id? raw) raw))
            body   (slurp (:body req))
            params (parse-form-params body)
            q      (str/trim (get params "q" ""))]
        (if (or (str/blank? id) (str/blank? q))
          {:status 400 :headers {"Content-Type" "application/json"} :body "{\"error\":\"missing session or query\"}"}
          (do
            (println (str "[" (Instant/now) "] [" (subs id 0 (min 8 (count id))) "] " q))
            (exec-submit! id q)
            {:status 200 :headers {"Content-Type" "application/json"} :body "{\"ok\":true}"})))

      ;; Static files from resources/public/
      (and (= meth :get) (or (str/starts-with? uri "/css/")
                              (str/starts-with? uri "/js/")))
      (let [resource (io/resource (str "public" uri))
            ext (cond (str/ends-with? uri ".css") "text/css"
                      (str/ends-with? uri ".js") "application/javascript"
                      :else "application/octet-stream")]
        (if resource
          {:status 200
           :headers {"Content-Type" (str ext "; charset=utf-8")
                     "Cache-Control" "no-cache, no-store, must-revalidate"}
           :body (slurp resource)}
          {:status 404 :body "Not found"}))

      :else {:status 404 :body "Not found"})))
