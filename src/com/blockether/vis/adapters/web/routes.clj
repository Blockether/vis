(ns com.blockether.vis.adapters.web.routes
  "HTTP routing — GET/POST handlers for conversations and queries."
  (:require [com.blockether.vis.adapters.web.conversations :as web-conversations]
            [com.blockether.vis.adapters.web.executor :as executor]
            [com.blockether.vis.adapters.web.presentation :as presentation]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]))

;;; ── Helpers ────────────────────────────────────────────────────────────

(defn- valid-conversation-id? [id]
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
      ;; GET / → redirect to last conversation or create new
      (and (= meth :get) (= uri "/"))
      (let [clist (web-conversations/conversations-list)]
        (if (seq clist)
          {:status 302 :headers {"Location" (str "/conversations/" (:id (last clist)))}}
          {:status 302 :headers {"Location" "/conversations/new"}}))

      ;; GET /conversations/new → create conversation and redirect
      (and (= meth :get) (= uri "/conversations/new"))
      (let [conv (web-conversations/create-conversation! "New Chat")]
        {:status 302 :headers {"Location" (str "/conversations/" (:id conv))}})

      ;; GET /conversations/:id → show conversation (or delete, or poll)
      (and (= meth :get) (str/starts-with? uri "/conversations/"))
      (let [parts (str/split uri #"/")
            id    (nth parts 2 nil)
            id    (when (valid-conversation-id? id) id)]
        (cond
          (str/ends-with? uri "/context")
          (if-let [payload (and id (web-conversations/context-payload id))]
            {:status 200
             :headers {"Content-Type" "application/json"}
             :body (json/write-json-str payload)}
            {:status 404 :headers {"Content-Type" "application/json"} :body "{\"error\":\"not found\"}"})

          (str/ends-with? uri "/delete")
          (if (and id (executor/in-flight? id))
            {:status 409 :headers {"Content-Type" "application/json"}
             :body "{\"error\":\"conversation has in-flight query\"}"}
            (do (when id (web-conversations/delete-conversation! id))
              {:status 302 :headers {"Location" "/"}}))

          :else
          (if-let [conv (when id (web-conversations/get-conversation id))]
            (let [qs     (:query-string req)
                  check  (when qs (some->> (re-find #"check=(\d+)" qs) second parse-long))
                  offset (when qs (some->> (re-find #"offset=(\d+)" qs) second parse-long))]
              (if check
                (let [ready?    (> (count (:messages conv)) check)
                      progress  (get @web-conversations/live-status id)
                      inflight? (executor/in-flight? id)]
                  {:status 200
                   :headers {"Content-Type" "application/json"}
                   :body (json/write-json-str
                           (cond-> {:ready ready? :inflight inflight?}
                             (:current progress)    (assoc :status (:current progress))
                             (:iterations progress) (assoc :iterations (:iterations progress))))})
                {:status 200
                 :headers {"Content-Type" "text/html; charset=utf-8"}
                 :body (presentation/page id (web-conversations/conversations-list) (:messages conv) {:offset (or offset presentation/page-size)})}))
            {:status 404
                 :headers {"Content-Type" "text/html; charset=utf-8"}
                 :body (presentation/not-found-page)})))

      ;; POST /conversations/:id → async query
      (and (= meth :post) (str/starts-with? uri "/conversations/"))
      (let [id     (let [raw (nth (str/split uri #"/") 2 nil)]
                     (when (valid-conversation-id? raw) raw))
            body   (slurp (:body req))
            params (parse-form-params body)
            q      (str/trim (get params "q" ""))]
        (if (or (str/blank? id) (str/blank? q))
          {:status 400 :headers {"Content-Type" "application/json"} :body "{\"error\":\"missing conversation or query\"}"}
          (do
            (println (str "[" (Instant/now) "] [" (subs id 0 (min 8 (count id))) "] " q))
            (executor/submit-query! id q)
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
