(ns com.blockether.vis.channels.web.routes
  "HTTP routing — GET/POST handlers for conversations and queries."
  (:require [com.blockether.vis.channels.web.conversations :as web-conversations]
            [com.blockether.vis.channels.web.dictation :as dictation]
            [com.blockether.vis.channels.web.executor :as executor]
            [com.blockether.vis.channels.web.presentation :as presentation]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.trove :as trove])
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

      ;; POST /conversations/:id/dictate → one-shot LLM cleanup of a raw
      ;; speech-to-text transcript. Reads JSON body {"text":"..."} and
      ;; returns {"text":"<refined>"}. NEVER creates a query or iteration
      ;; entity — pure refinement helper, not part of the conversation.
      (and (= meth :post) (str/ends-with? uri "/dictate"))
      (let [parts (str/split uri #"/")
            id    (when (= 4 (count parts)) (nth parts 2 nil))
            id    (when (valid-conversation-id? id) id)
            body  (try (slurp (:body req)) (catch Exception _ ""))
            payload (try (json/read-json body :key-fn keyword)
                         (catch Exception _ nil))
            raw   (str/trim (str (:text payload)))]
        (cond
          (nil? id)
          {:status 400 :headers {"Content-Type" "application/json"}
           :body "{\"error\":\"invalid conversation id\"}"}
          (str/blank? raw)
          {:status 400 :headers {"Content-Type" "application/json"}
           :body "{\"error\":\"missing text\"}"}
          :else
          (let [refined (dictation/cleanup-dictation id raw)]
            {:status 200
             :headers {"Content-Type" "application/json"}
             :body (json/write-json-str {:text refined})})))

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
            (trove/log! {:level :info :id ::submit-query
                         :data {:conversation-id (subs id 0 (min 8 (count id))) :query q}
                         :msg (str "query submitted: " q)})
            (executor/submit-query! id q)
            {:status 200 :headers {"Content-Type" "application/json"} :body "{\"ok\":true}"})))

      ;; Static files from resources/public/
      ;; Fonts are immutable — cache for a year. Third-party libs vendored
      ;; under /vendor/ are pinned to a specific version on disk so they
      ;; can also be cached aggressively (bump the file, not the URL, to
      ;; upgrade). Our own JS/CSS change frequently and are NOT versioned
      ;; in the URL, so the browser must revalidate every request;
      ;; otherwise a deploy never reaches the user until they hard-reload.
      (and (= meth :get) (or (str/starts-with? uri "/css/")
                            (str/starts-with? uri "/js/")
                            (str/starts-with? uri "/fonts/")
                            (str/starts-with? uri "/vendor/")))
      (let [resource (io/resource (str "public" uri))
            ext (cond (str/ends-with? uri ".css") "text/css"
                  (str/ends-with? uri ".js") "application/javascript"
                  (str/ends-with? uri ".ttf") "font/ttf"
                  (str/ends-with? uri ".woff2") "font/woff2"
                  (str/ends-with? uri ".woff") "font/woff"
                  :else "application/octet-stream")
            cache-control (cond
                            (str/starts-with? uri "/fonts/")
                            "public, max-age=31536000, immutable"
                            (str/starts-with? uri "/vendor/")
                            "public, max-age=604800"
                            :else
                            "no-cache, no-store, must-revalidate")]
        (if resource
          {:status 200
           :headers {"Content-Type" ext
                     "Cache-Control" cache-control}
           :body (io/input-stream resource)}
          {:status 404 :body "Not found"}))

      :else {:status 404 :body "Not found"})))
