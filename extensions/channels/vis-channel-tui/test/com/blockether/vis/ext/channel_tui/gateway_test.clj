(ns com.blockether.vis.ext.channel-tui.gateway-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.gateway :as gateway]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.input KeyType]
           [com.googlecode.lanterna.terminal.html HtmlTerminal HtmlTerminalEndpoint
            HtmlTerminalEndpoint$Event]))

(defn- rv [sym] (ns-resolve 'com.blockether.vis.ext.channel-tui.gateway sym))

(defn- terminal
  []
  (-> (HtmlTerminal/builder)
      (.initialSize (TerminalSize. 10 4))
      (.columnRange 2 80)
      (.rowRange 2 40)
      (.build)))

(deftest route-contribution-owns-one-authenticated-browser-surface
  (let [contribution
        (gateway/routes-contribution)

        routes
        ((:routes contribution) "secret")

        request-authed?
        (:request-authed-fn contribution)]

    (is (= "/tui" (:prefix contribution)))
    (is (true? (:form-params? contribution)))
    (is (fn? (:stop-fn contribution)))
    (is (= #{"/tui" "/tui/events" "/tui/input" "/tui/resize"} (:protocol-open-uris contribution)))
    (is (= #{"/tui" "/tui/embed" "/tui/events" "/tui/input" "/tui/resize"}
           (set (map first routes))))
    (is (true? (request-authed? {:query-params {"token" "secret"}} "secret")))
    (is (true? (request-authed? {:cookies {"vis_tui_gateway" {:value "secret"}}} "secret")))
    (is (true? (request-authed? {:headers {"authorization" "Bearer secret"}} "secret")))
    (is (false? (request-authed? {:headers {"authorization" "Bearer wrong"}} "secret")))
    (is (false? (request-authed? {:query-params {"token" "wrong"}} "secret")))
    (is (false? (request-authed? {} "secret")))))

(deftest page-input-resize-and-stream-use-the-same-transport-neutral-terminal
  (with-open [terminal
              (terminal)

              endpoint
              (HtmlTerminalEndpoint. terminal)]

    (.putCharacter terminal \S)
    (.flush terminal)
    (with-redefs-fn {(rv 'ensure-endpoint!) (constantly endpoint)}
      (fn []
        (let [contribution
              (gateway/routes-contribution)

              routes
              (into {} ((:routes contribution) "secret"))

              cookie-request
              {:cookies {"vis_tui_gateway" {:value "secret"}}}

              bearer-request
              {:headers {"authorization" "Bearer secret"}}]

          (testing "the first authenticated response is rendered HTML, not a client frame model"
            (let [response
                  ((rv 'page-handler) "secret" cookie-request)

                  body
                  (:body response)]

              (is (= 200 (:status response)))
              (is (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
              (is (re-find #">S</span>" body))
              (is (re-find #"new EventSource" body))
              (is (not (re-find #"application/json|response\.json|/frame" body)))))
          (testing "the gateway secret becomes an HttpOnly browser session and leaves the URL"
            (let [response ((rv 'page-handler) "secret" {})]
              (is (= 303 (:status response)))
              (is (= "/tui" (get-in response [:headers "Location"])))
              (is (true? (get-in response [:cookies "vis_tui_gateway" :http-only])))
              (is (= :strict (get-in response [:cookies "vis_tui_gateway" :same-site]))))
            (is (= 303 (:status ((rv 'page-handler) "secret" {:query-params {"token" "secret"}}))))
            (is (= 401 (:status ((rv 'page-handler) "secret" {:query-params {"token" "wrong"}})))))
          ;; Regression, issue b30f87ac-f20e-4d7f-9fd2-416788d10527: an HTML attachment
          ;; stayed a portable snapshot in the Companion, so every key, tap and resize was discarded.
          (testing "the authenticated Companion receives a parent bridge without the gateway secret"
            (let [embed
                  (get-in routes ["/tui/embed" :get])

                  response
                  (embed (assoc bearer-request :query-params {"bridge" "phone-review"}))

                  body
                  (:body response)]

              (is (= 200 (:status response)))
              (is (re-find #"data-transport=\"parent\"" body))
              (is (re-find #"data-bridge-id=\"phone-review\"" body))
              (is (not (str/includes? body "secret")))))
          (testing "input routes accept the browser cookie or the canonical bearer header"
            (let [input
                  (get-in routes ["/tui/input" :post])

                  resize
                  (get-in routes ["/tui/resize" :post])]

              (is (= 401 (:status (input {:form-params {"kind" "key" "key" "Enter"}}))))
              (is (= 204
                     (:status (input (assoc cookie-request
                                       :form-params {"kind" "key" "key" "Enter"})))))
              (is (= KeyType/Enter (.getKeyType (.readInput terminal))))
              (is (= 204
                     (:status (input (assoc bearer-request
                                       :form-params {"kind" "key" "key" "Tab"})))))
              (is (= KeyType/Tab (.getKeyType (.readInput terminal))))
              (is (= 204
                     (:status (resize (assoc bearer-request
                                        :form-params {"cols" "33" "rows" "12"})))))
              (is (= (TerminalSize. 33 12) (.getTerminalSize terminal)))))
          (testing "SSE carries the server-rendered fragment itself"
            (let [^HtmlTerminalEndpoint$Event event
                  (.awaitEvent ^HtmlTerminalEndpoint endpoint -1 0)

                  body
                  (.body event)]

              (is (re-find #"event: frame" body))
              (is (re-find #"data: <div class=\"frame\"" body))
              (is (not (re-find #"\"runs\"|\"media\"" body))))))))))
