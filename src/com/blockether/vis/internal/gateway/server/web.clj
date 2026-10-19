(ns com.blockether.vis.internal.gateway.server.web
  "Serve the prebuilt Companion web app from the gateway root, so `vis-agent web`
   needs no Node.js at runtime.

   Release installs unpack `vis-web.tar.gz` beside the native runtime, and the
   launcher exports that folder as `VIS_WEB_DIR`; a source checkout points it at
   `apps/vis-companion/dist-web`. The launcher names the folder even before it
   exists and the gateway looks it up on every request, so a build or install
   that lands after the gateway started is served without a restart. Until the
   folder holds an `index.html` the gateway serves its API alone.

   The bundle is public content, like `/docs`: it holds no secret, and the app
   authenticates its own API calls. Only GET and HEAD requests for a file that
   exists inside the folder are answered here; the API, the docs and every
   missing file fall through to the router unchanged.

   A web launch connects a fresh browser without a pasted token. `vis-agent web`
   asks its gateway for a one-time launch code that expires in [[LAUNCH_TTL_MS]].
   The code goes into the URL fragment, and the app exchanges it once for the
   gateway id and token. The long-lived token never enters a URL."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io File]))

(def ^:private content-types
  {"css" "text/css; charset=utf-8"
   "html" "text/html; charset=utf-8"
   "ico" "image/x-icon"
   "jpeg" "image/jpeg"
   "jpg" "image/jpeg"
   "js" "text/javascript; charset=utf-8"
   "json" "application/json"
   "map" "application/json"
   "mjs" "text/javascript; charset=utf-8"
   "png" "image/png"
   "svg" "image/svg+xml"
   "txt" "text/plain; charset=utf-8"
   "wasm" "application/wasm"
   "webmanifest" "application/manifest+json"
   "webp" "image/webp"
   "woff" "font/woff"
   "woff2" "font/woff2"})

(def ^:private bundle-path
  "Every name Vite emits: plain segments that never start with a dot, so `..`, a
   hidden file or a percent-encoded name never reaches the file system."
  #"(?:[A-Za-z0-9_-][A-Za-z0-9._-]*/)*[A-Za-z0-9_-][A-Za-z0-9._-]*")

(defn configured-dir
  "The web app folder the launcher names in `VIS_WEB_DIR`. It may not exist yet."
  []
  (System/getenv "VIS_WEB_DIR"))

(defn configured-root
  "The canonical web app folder named by `dir` ([[configured-dir]] by default), or
   nil when it is unset or holds no `index.html`."
  (^File [] (configured-root (configured-dir)))
  (^File [dir]
   (when-let [dir (some-> dir
                          str
                          str/trim
                          not-empty)]
     (let [root (.getCanonicalFile (io/file dir))]
       (when (.isFile (io/file root "index.html")) root)))))

(defn- app-uri?
  "True for a path the web app may answer. The API and docs namespaces are never
   shadowed, whatever the bundle contains."
  [^String uri]
  (and (str/starts-with? uri "/")
       (not (str/starts-with? uri "/v1/"))
       (not (str/starts-with? uri "/docs"))))

(defn- bundle-file
  "The regular file `uri` names inside `root`, or nil. `/` is the app itself."
  ^File [^File root ^String uri]
  (let [rel (if (= "/" uri) "index.html" (subs uri 1))]
    (when (re-matches bundle-path rel)
      (let [file (.getCanonicalFile (io/file root rel))]
        (when (and (.isFile file)
                   (str/starts-with? (.getPath file) (str (.getPath root) File/separator)))
          file)))))

(defn- file-response
  [request ^File file]
  (let [file-name
        (.getName file)

        dot
        (str/last-index-of file-name ".")

        extension
        (when dot (str/lower-case (subs file-name (inc (long dot)))))

        etag
        (str "W/\"" (.length file) "-" (.lastModified file) "\"")

        headers
        {"Content-Type" (get content-types extension "application/octet-stream")
         ;; Vite fingerprints everything under /assets/; the entry page, the
         ;; service worker and the logo keep their names across releases.
         "Cache-Control" (if (str/starts-with? (str (:uri request)) "/assets/")
                           "public, max-age=31536000, immutable"
                           "no-cache")
         "ETag" etag
         "X-Content-Type-Options" "nosniff"
         ;; The app drives an agent that runs code: no other page may frame it.
         "X-Frame-Options" "DENY"
         "Content-Security-Policy" "frame-ancestors 'none'"}]

    (cond (= etag (get-in request [:headers "if-none-match"])) {:status 304 :headers headers}
          (= :head (:request-method request))
          {:status 200 :headers (assoc headers "Content-Length" (str (.length file)))}
          :else
          {:status 200 :headers (assoc headers "Content-Length" (str (.length file))) :body file})))

(defn wrap-web
  "Answer GET and HEAD requests for the web app's own files from the folder `dir`
   names ([[configured-dir]] by default). [[configured-root]] resolves the folder
   per request, so a build or install that lands later is served at once. A blank
   `dir` returns `handler` unchanged."
  ([handler] (wrap-web handler (configured-dir)))
  ([handler dir]
   (if (str/blank? (some-> dir
                           str))
     handler
     (fn [request]
       (let [uri (str (:uri request))]
         (or (when (and (contains? #{:get :head} (:request-method request)) (app-uri? uri))
               (when-let [file (some-> (configured-root dir)
                                       (bundle-file uri))]
                 (file-response request file)))
             (handler request)))))))

(def LAUNCH_TTL_MS
  "How long a launch code stays redeemable. A browser that `vis-agent web` opens
   redeems it in seconds; a wrapper that opens the address itself gets minutes."
  (* 5 60 1000))

(defonce ^:private launches (atom {}))

(defn mint-launch!
  "Record a new one-time launch code and return `{:code :expires-at-ms}`. Expired
   codes are dropped on each call, so the store never grows past live launches."
  ([] (mint-launch! (util/now-ms)))
  ([^long now]
   (let [code
         (util/base64url (util/random-bytes 32))

         expires-at-ms
         (+ now (long LAUNCH_TTL_MS))]

     (swap! launches (fn [codes]
                       (assoc (into {}
                                    (filter (fn [[_ expiry]]
                                              (> (long expiry) now)))
                                    codes)
                         code expires-at-ms)))
     {:code code :expires-at-ms expires-at-ms})))

(defn redeem-launch!
  "True once for a code that [[mint-launch!]] returned and that has not expired.
   Any attempt consumes the code, so a second or a late attempt is false."
  ([code] (redeem-launch! code (util/now-ms)))
  ([code ^long now]
   (let [code
         (str code)

         [codes _]
         (swap-vals! launches dissoc code)]

     (boolean (some-> (get codes code)
                      long
                      (> now))))))

(defn- holds-gateway-secret?
  "True when `request` carries the gateway token as a bearer token or in the
   internal client's `X-Vis-Gateway-Secret` header."
  [request token]
  (let [header (fn [k]
                 (some-> (get-in request [:headers k])
                         str/trim))]
    (or (util/constant-time=? (str "Bearer " token) (header "authorization"))
        (util/constant-time=? token (header "x-vis-gateway-secret")))))

(defn- gateway-id
  "The id `/healthz` reports for this gateway."
  []
  (let [{:keys [db host port]} @instance/server-state]
    (instance/gateway-instance-id db host port)))

(defn- launch-handler
  "Mint a launch code. Only a holder of the gateway token may ask, also on a
   loopback gateway that does not require the token for its API: any page in a
   local browser can reach loopback, and the code grants the token."
  [request]
  (let [token (str (:token @instance/server-state))]
    (if (and (seq token) (holds-gateway-secret? request token))
      (let [{:keys [code expires-at-ms]} (mint-launch!)]
        (http/json-response {:code code :expires-at-ms expires-at-ms :gateway-id (gateway-id)}))
      (http/error-response 401 :unauthorized "a web launch needs the gateway token"))))

(defn- launch-code
  "The trimmed `code` of a redeem request body, or nil."
  [request]
  (let [body (try (http/body-json request) (catch Exception _ nil))]
    (when (map? body)
      (some-> (get body "code")
              str
              str/trim
              not-empty))))

(defn- redeem-handler
  "Exchange a launch code for the gateway id and token. The route is open: the
   code is the credential, and it works once."
  [request]
  (let [{:keys [token require-token?]}
        @instance/server-state

        code
        (launch-code request)]

    (if (and code (redeem-launch! code))
      (assoc-in (http/json-response
                  {:gateway-id (gateway-id) :token token :require-token (boolean require-token?)})
        [:headers "Cache-Control"]
        "no-store")
      (http/error-response
        404
        :launch-not-found
        "This launch link was already used or has expired. Run `vis-agent web` again."))))

(def handlers
  {[:post "/v1/web/launches"] launch-handler [:post "/v1/web/launches/redeem"] redeem-handler})
