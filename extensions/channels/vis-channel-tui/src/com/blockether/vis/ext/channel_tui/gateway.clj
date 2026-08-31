(ns com.blockether.vis.ext.channel-tui.gateway
  "Gateway-owned browser transport for the Lanterna TUI.

   This is an ordinary route contribution inside Vis's existing Reitit/Ring app and
   Jetty 12 server; Lanterna owns no HTTP server or socket. Lanterna remains the sole
   layout engine. The first GET returns cells and media already rendered as HTML;
   subsequent paints are server-rendered fragments over the gateway's existing SSE
   transport. Browser JavaScript only applies those fragments and forwards input/resize
   forms. Companion carries the same routes through its authenticated parent while the
   terminal iframe remains credential-free. One process owns one TUI runtime, so every
   connected browser sees and controls the same terminal session."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.screen :as screen]
            [com.blockether.vis.ext.channel-tui.theme :as theme]
            [com.blockether.vis.internal.gateway.server :as gateway-server]
            [com.blockether.vis.internal.util :as util]
            [ring.core.protocols :as ring-protocols]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna.terminal.html HtmlTerminal HtmlTerminalRenderer
            HtmlTerminalRenderer$Frame]
           [java.io IOException OutputStream]
           [java.security MessageDigest]))

(def ^:private cookie-name "vis_tui_gateway")
(defonce ^:private runtime* (atom nil))

(defn- same-secret?
  [a b]
  (boolean (and (string? a) (string? b) (MessageDigest/isEqual (util/utf8 a) (util/utf8 b)))))

(defn- cookie-secret [request] (get-in request [:cookies cookie-name :value]))

(defn- bearer-secret
  [request]
  (some->> (get-in request [:headers "authorization"])
           str
           (re-matches #"(?i)Bearer\s+(.+)")
           second))

(defn- request-secret
  [request]
  (or (get-in request [:query-params "token"]) (cookie-secret request) (bearer-secret request)))

(defn- request-authed? [request token] (same-secret? token (request-secret request)))

(defn- new-terminal
  []
  (-> (HtmlTerminal/builder)
      (.title "Vis terminal")
      (.defaultForeground theme/text-fg)
      (.defaultBackground theme/terminal-bg)
      (.build)))

(defn- launch-terminal!
  []
  (let [id
        (random-uuid)

        terminal
        (new-terminal)]

    (reset! runtime* {:id id :terminal terminal})
    (let [task (vis/worker-future "tui-gateway"
                                  (fn []
                                    (try (screen/run-chat! {:html-terminal terminal})
                                         (catch Throwable error
                                           (tel/log! {:level :error
                                                      :id ::runtime-failed
                                                      :data {:error (ex-message error)}}
                                                     "Gateway TUI runtime stopped after an error."))
                                         (finally (try (.close terminal) (catch Throwable _ nil))
                                                  (swap! runtime* #(when (= id (:id %)) nil))))))]
      (swap! runtime* #(when (= id (:id %)) (assoc % :task task))))
    terminal))

(defn- ensure-terminal!
  []
  (locking runtime*
    (let [terminal (:terminal @runtime*)]
      (if (and terminal (not (.isClosed ^HtmlTerminal terminal))) terminal (launch-terminal!)))))

(defn stop!
  "Stop the gateway-owned TUI, if one is running."
  []
  (when-let [current (locking runtime*
                       (let [current @runtime*]
                         (reset! runtime* nil)
                         current))]
    (.close ^HtmlTerminal (:terminal current)))
  nil)

(def ^:private html-headers
  {"Content-Type" "text/html; charset=utf-8"
   "Cache-Control" "no-store"
   "Referrer-Policy" "no-referrer"
   "X-Content-Type-Options" "nosniff"
   "Content-Security-Policy"
   "default-src 'none'; connect-src 'self'; img-src data:; media-src data:; style-src 'unsafe-inline'; script-src 'unsafe-inline'; base-uri 'none'; frame-ancestors 'none'"})

(defn- secure-request?
  [request]
  (or (= :https (:scheme request))
      (= "https"
         (some-> (get-in request [:headers "x-forwarded-proto"])
                 str/lower-case))))

(defn- unauthorized-handler
  [_request]
  {:status 401
   :headers html-headers
   :body
   (str
     "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
     "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
     "<title>Vis terminal — authorization required</title></head>"
     "<body><main><h1>Authorization required</h1>"
     "<p>Open <code>/tui?token=&lt;gateway-token&gt;</code> once to start a secure browser session.</p>"
     "</main></body></html>")})

(defn- session-cookie-response
  [token request]
  {:status 303
   :headers {"Location" "/tui" "Cache-Control" "no-store"}
   :cookies {cookie-name {:value token
                          :path "/tui"
                          :http-only true
                          :same-site :strict
                          :secure (secure-request? request)}}
   :body ""})

(defn- page-handler
  [token request]
  (let [query-params
        (:query-params request)

        query-token
        (get query-params "token")]

    (cond (same-secret? token (cookie-secret request))
          {:status 200
           :headers html-headers
           :body (.renderLiveHtml ^HtmlTerminal (ensure-terminal!) "/tui")}
          (and (contains? query-params "token") (not (same-secret? token query-token)))
          (unauthorized-handler request)
          :else (session-cookie-response token request))))

(defn- browser-session-handler
  [token handler]
  (fn [request]
    (if (request-authed? request token) (handler request) (unauthorized-handler request))))

(defn- embed-handler
  [request]
  (let [bridge (util/non-blank (get-in request [:query-params "bridge"]))]
    (if (and bridge (<= (count bridge) 128))
      {:status 200
       :headers html-headers
       :body (.renderBridgeHtml ^HtmlTerminal (ensure-terminal!) bridge)}
      {:status 400
       :headers {"Content-Type" "text/plain; charset=utf-8" "Cache-Control" "no-store"}
       :body "bridge must be a non-blank identifier of at most 128 characters"})))

(defn- request-form [request] (or (:form-params request) (:params request) {}))

(defn- input-handler
  [request]
  (.submitBrowserInput ^HtmlTerminal (ensure-terminal!) (request-form request))
  {:status 204 :headers {"Cache-Control" "no-store"} :body ""})

(defn- form-long
  [form key]
  (try (some-> (get form key)
               str
               Long/parseLong)
       (catch NumberFormatException _ nil)))

(defn- resize-handler
  [request]
  (let [form
        (request-form request)

        columns
        (form-long form "cols")

        rows
        (form-long form "rows")]

    (if (and columns rows)
      (do (.resizeFromBrowser ^HtmlTerminal (ensure-terminal!) (int columns) (int rows))
          {:status 204 :headers {"Cache-Control" "no-store"} :body ""})
      {:status 400
       :headers {"Content-Type" "text/plain; charset=utf-8" "Cache-Control" "no-store"}
       :body "cols and rows must be integers"})))

(defn- frame-event
  [^HtmlTerminalRenderer$Frame frame]
  (str "id: "
       (.version frame)
       "\n"
       "event: frame\n"
       (->> (str/split (HtmlTerminalRenderer/renderFrame frame) #"\R" -1)
            (map #(str "data: " % "\n"))
            (apply str))
       "\n"))

(defn- last-event-id
  [request]
  (try (Long/parseLong (str (or (get-in request [:headers "last-event-id"])
                                (get-in request [:query-params "after"])
                                -1)))
       (catch NumberFormatException _ -1)))

(defn- events-body
  [^HtmlTerminal terminal initial-version]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^OutputStream output
              output-stream

              stream-id
              (random-uuid)

              close!
              #(try (.close output) (catch Throwable _ nil))]

          (gateway-server/register-contributed-sse! stream-id close!)
          (try (loop [after (long initial-version)]
                 (when-not (.isClosed terminal)
                   (let [frame (.awaitFrame terminal after 15000)
                         version (.version ^HtmlTerminalRenderer$Frame frame)]

                     (.write output
                             (util/utf8 (if (= version after) ": keepalive

" (frame-event frame))))
                     (.flush output)
                     (recur version))))
               (catch IOException _ nil)
               (finally (gateway-server/unregister-contributed-sse! stream-id) (close!)))))))

(defn- events-handler
  [request]
  {:status 200
   :headers {"Content-Type" "text/event-stream"
             "Cache-Control" "no-cache, no-transform"
             "X-Accel-Buffering" "no"
             "X-Content-Type-Options" "nosniff"}
   :body (events-body (ensure-terminal!) (last-event-id request))})

(defn routes-contribution
  "Mount the singleton SSR terminal at `/tui` on the Vis gateway."
  []
  {:prefix "/tui"
   :rev (str (System/identityHashCode #'routes-contribution))
   :form-params? true
   :protocol-open-uris #{"/tui" "/tui/events" "/tui/input" "/tui/resize"}
   :request-authed-fn request-authed?
   :on-unauthorized unauthorized-handler
   :stop-fn stop!
   :routes (fn [token]
             [["/tui" {:get (partial page-handler token)}]
              ["/tui/embed" {:get (browser-session-handler token embed-handler)}]
              ["/tui/events" {:get (browser-session-handler token events-handler)}]
              ["/tui/input" {:post (browser-session-handler token input-handler)}]
              ["/tui/resize" {:post (browser-session-handler token resize-handler)}]])})
