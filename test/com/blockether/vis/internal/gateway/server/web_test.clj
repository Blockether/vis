(ns com.blockether.vis.internal.gateway.server.web-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.gateway.server.web :as web]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- bundle!
  "A temp folder holding a small web app next to a file that is not part of it."
  []
  (let [base
        (.toFile (Files/createTempDirectory "vis-web-test" (make-array FileAttribute 0)))

        root
        (io/file base "vis-web")]

    (doseq [[rel content] {"index.html" "<!doctype html><title>Vis</title>"
                           "sw.js" "self.addEventListener('push', () => {});"
                           "assets/index-abc123.js" "console.log('vis');"
                           "assets/font-abc123.woff2" "font"
                           ".env" "SECRET=1"}]
      (io/make-parents (io/file root rel))
      (spit (io/file root rel) content))
    (spit (io/file base "outside.txt") "not part of the app")
    {:base base :root root}))

(defn- delete-tree! [dir] (run! io/delete-file (reverse (file-seq dir))))

(def ^:private api-response {:status 404 :body "handled by the router"})

(defn- serve
  [root request]
  ((web/wrap-web (constantly api-response) root) (merge {:request-method :get} request)))

(defdescribe configured-root-needs-an-app-entry-page
             (it "configured root needs an app entry page"
                 (let [{:keys [base root]} (bundle!)]
                   (try (expect (= (.getCanonicalFile root) (web/configured-root (str root))))
                        (expect (= (.getCanonicalFile root)
                                   (web/configured-root (str "  " root "  "))))
                        ;; no folder, a blank value or a folder without index.html serves no app
                        (expect (nil? (web/configured-root nil)))
                        (expect (nil? (web/configured-root "")))
                        (expect (nil? (web/configured-root (str base))))
                        (expect (nil? (web/configured-root (str (io/file base "missing")))))
                        (finally (delete-tree! base))))))

(defdescribe
  serves-the-app-and-its-assets
  (it
    "serves the app and its assets"
    (let [{:keys [base root]}
          (bundle!)

          root
          (web/configured-root (str root))]

      (try
        ;; the gateway root is the app's entry page, revalidated on every load
        (let [response (serve root {:uri "/"})]
          (expect (= 200 (:status response)))
          (expect (= "<!doctype html><title>Vis</title>" (slurp (:body response))))
          (expect (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
          (expect (= "no-cache" (get-in response [:headers "Cache-Control"])))
          (expect (= "nosniff" (get-in response [:headers "X-Content-Type-Options"])))
          (expect (= "DENY" (get-in response [:headers "X-Frame-Options"])))
          (expect (= "frame-ancestors 'none'"
                     (get-in response [:headers "Content-Security-Policy"])))
          (expect (= "33" (get-in response [:headers "Content-Length"]))))
        ;; fingerprinted assets are cached for good; the service worker is not
        (let [asset
              (serve root {:uri "/assets/index-abc123.js"})

              font
              (serve root {:uri "/assets/font-abc123.woff2"})

              worker
              (serve root {:uri "/sw.js"})]

          (expect (= "text/javascript; charset=utf-8" (get-in asset [:headers "Content-Type"])))
          (expect (= "public, max-age=31536000, immutable"
                     (get-in asset [:headers "Cache-Control"])))
          (expect (= "font/woff2" (get-in font [:headers "Content-Type"])))
          (expect (= "text/javascript; charset=utf-8" (get-in worker [:headers "Content-Type"])))
          (expect (= "no-cache" (get-in worker [:headers "Cache-Control"]))))
        ;; HEAD answers the headers alone
        (let [response (serve root {:uri "/" :request-method :head})]
          (expect (= 200 (:status response)))
          (expect (= "33" (get-in response [:headers "Content-Length"])))
          (expect (nil? (:body response))))
        ;; an unchanged file revalidates with 304
        (let [etag
              (get-in (serve root {:uri "/"}) [:headers "ETag"])

              response
              (serve root {:uri "/" :headers {"if-none-match" etag}})]

          (expect (some? etag))
          (expect (= 304 (:status response)))
          (expect (nil? (:body response))))
        (finally (delete-tree! base))))))

(defdescribe everything-else-reaches-the-router
             (it "everything else reaches the router"
                 (let [{:keys [base root]}
                       (bundle!)

                       root
                       (web/configured-root (str root))]

                   (try (doseq [uri ["/missing.js" "/.env" "/../outside.txt" "/assets/../index.html"
                                     "/%2e%2e/outside.txt" "/assets/" "/assets" "/v1/sessions"
                                     "/docs" "/docs/index.html" "/healthz"]]
                          (expect (= api-response (serve root {:uri uri})) uri))
                        ;; only reads are answered
                        (expect (= api-response (serve root {:uri "/" :request-method :post})))
                        ;; a link inside the folder cannot reach a file outside it
                        (Files/createSymbolicLink (.toPath (io/file root "linked.txt"))
                                                  (.toPath (io/file base "outside.txt"))
                                                  (make-array FileAttribute 0))
                        (expect (= api-response (serve root {:uri "/linked.txt"})))
                        (finally (delete-tree! base))))))

;; A gateway started before `vis-agent web` builds the app, or before `vis-agent
;; update` installs it, must serve the app once it lands: restarting a gateway that
;; other sessions use is not an option.
(defdescribe a-bundle-that-lands-later-is-served-without-a-restart
             (it "a bundle that lands later is served without a restart"
                 (let [{:keys [base root]}
                       (bundle!)

                       index
                       (io/file root "index.html")

                       page
                       (slurp index)

                       handler
                       (web/wrap-web (constantly api-response) (str root))

                       get-app
                       #(handler {:request-method :get :uri "/"})]

                   (try (io/delete-file index)
                        ;; the router answers while the folder has no entry page
                        (expect (= api-response (get-app)))
                        (spit index page)
                        ;; the same handler serves the app once it lands
                        (expect (= 200 (:status (get-app))))
                        (expect (= (.getCanonicalFile index) (:body (get-app))))
                        (finally (delete-tree! base))))))

(defdescribe without-a-bundle-the-gateway-is-unchanged
             (it "without a bundle the gateway is unchanged"
                 (let [handler (constantly api-response)]
                   (expect (identical? handler (web/wrap-web handler nil))))))
