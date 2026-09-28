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
   missing file fall through to the router unchanged."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
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
