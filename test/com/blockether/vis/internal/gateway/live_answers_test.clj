;; Copyright (c) Blockether. All rights reserved.
;; Before this namespace, these operations had only a fixture answer, so a handler
;; change on them was not found (#487). Each test sends a request through the real
;; router, and the `response-check/answers` hook checks the real answer against its
;; schema. Stubs replace only the outside boundary: the network catalog fetch, the
;; local speech model and the push home folder.
(ns com.blockether.vis.internal.gateway.live-answers-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.speech.core :as speech]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.git :as git]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayInputStream File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(wiring/install!)

(defn- temp-dir
  ^File [prefix]
  (.getCanonicalFile (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))))

(defn- delete-tree!
  [^File dir]
  (doseq [^File f (reverse (file-seq dir))]
    (io/delete-file f true)))

(defn- request
  [method uri body]
  {:request-method method
   :uri uri
   :headers {"content-type" "application/json"
             "x-vis-client-id" "owner"
             "x-vis-protocol" (str gateway-contract/protocol-version)
             "x-vis-min-gateway-protocol" (str gateway-contract/minimum-gateway-protocol)}
   :body (ByteArrayInputStream. (.getBytes ^String (wire/json-str body) "UTF-8"))})

(defn- answer
  "The status and the parsed body of the answer of `handler` to one request."
  [handler method uri body]
  (let [response
        (handler (request method uri body))

        body
        (:body response)]

    [(:status response) (wire/parse-json (if (string? body) body (slurp body)))]))

(defn- with-gateway
  "Call `f` with a router handler, over an empty in-memory store and a server state
   that knows the client `owner`."
  [f]
  (let [store
        (ps/db-open! :memory)

        before
        @instance/server-state

        now
        (util/now-ms)]

    (reset! instance/server-state {:require-token? false
                                   :managed? false
                                   :clients {"owner" {:connected-at now :last-seen-at now}}})
    (try (with-redefs [lp/db-info (constantly store)]
           (f store (#'server/app "" [])))
         (finally (reset! instance/server-state before) (ps/db-close! store)))))

(defdescribe admin-routes-answer-their-schema
             (it "status, client lease, devices and decision models answer their schema"
                 (let [push-home
                       (temp-dir "vis-live-push")

                       previous
                       (System/getProperty "vis.push.home")]

                   (System/setProperty "vis.push.home" (str push-home))
                   (push/reload-devices!)
                   (try (with-gateway
                          (fn [_ handler]
                            (let [[status body] (answer handler :get "/v1/admin/status" {})]
                              (expect (= 200 status))
                              (expect (= "running" (get body "status"))))
                            (let [[status body]
                                  (answer handler :post "/v1/clients" {"kind" "test" "pid" 1})]
                              (expect (= 200 status))
                              (expect (string? (get body "client_id"))))
                            (let [[status body] (answer handler :get "/v1/devices" {})]
                              (expect (= 200 status))
                              (expect (= [] (get body "devices"))))
                            (let [[status body] (answer handler :get "/v1/decisions/models" {})]
                              (expect (= 200 status))
                              (expect (seq (get body "models"))))))
                        (finally (if previous
                                   (System/setProperty "vis.push.home" previous)
                                   (System/clearProperty "vis.push.home"))
                                 (push/reload-devices!)
                                 (delete-tree! push-home))))))

(defdescribe
  session-and-project-routes-answer-their-schema
  (it
    "project ensure and order, session resources, queue, fork and root change answer their schema"
    (let [project-root
          (temp-dir "vis-live-project")

          other-root
          (temp-dir "vis-live-other")]

      (try
        (with-gateway
          (fn [store handler]
            (let [sid
                  (str (h/store-session! store {:channel :api :title "Talk"}))

                  base
                  (str "/v1/sessions/" sid)

                  [status project]
                  (answer handler :post "/v1/projects/actions/ensure" {"root" (str project-root)})]

              (ps/db-store-session-turn!
                store
                {:parent-session-id sid :user-request "ask" :status :success})
              (expect (= 200 status))
              (expect (= (str project-root) (get project "workspace_root")))
              (expect (= [200 {"resources" []}] (answer handler :get (str base "/resources") {})))
              (expect (= [200 {"turn" nil}] (answer handler :post (str base "/drain-queue") {})))
              (expect (= [200 {"turn" nil}] (answer handler :post (str base "/resume-queue") {})))
              (let [[status body] (answer handler :post (str base "/forks") {})]
                (expect (= 200 status))
                (expect (not= sid (get-in body ["session" "id"]))))
              (let [[status body]
                    (answer handler :patch (str base "/workspace/root") {"path" (str other-root)})]
                (expect (= 200 status))
                (expect (= (str other-root) (get-in body ["workspace" "root"]))))
              (expect (= [200 {"project_id" (get project "id") "count" 1}]
                         (answer handler
                                 :patch
                                 (str "/v1/projects/" (get project "id") "/sessions")
                                 {"order" [sid]}))))))
        (finally (delete-tree! project-root) (delete-tree! other-root))))))

(defn- git!
  "Run `git <args>` in `dir` with a fixed identity; fail the test when git fails."
  [^File dir & args]
  (let [{:keys [exit err]} (git/run-git dir (into ["-c" "user.name=t" "-c" "user.email=t@t"] args))]
    (when-not (= 0 exit) (throw (ex-info (str "git failed: " err) {:args args})))))

(defdescribe
  workspace-branch-routes-answer-their-schema
  (it "lists the local branches and switches the session workspace to another one"
      (let [repo (temp-dir "vis-live-branches")]
        (git! repo "init" "-b" "main")
        (git! repo "commit" "--allow-empty" "-m" "one")
        (git! repo "branch" "feature")
        (try
          (with-gateway
            (fn [store handler]
              (let [sid
                    (str (h/store-session! store {:channel :api :title "Talk"}))

                    base
                    (str "/v1/sessions/" sid "/workspace")]

                (expect (= 200 (first (answer handler :patch (str base "/root") {"path" (str repo)}))))
                (let [[status body] (answer handler :get (str base "/branches") {})]
                  (expect (= 200 status))
                  (expect (= "main" (get body "branch")))
                  (expect (= #{"main" "feature"} (set (get body "branches")))))
                (let [[status body] (answer handler :patch (str base "/branch") {"branch" "feature"})]
                  (expect (= 200 status))
                  (expect (= "feature" (get body "branch"))))
                ;; The session footer reads the new branch at once, not after its cache expires.
                (expect (= "feature"
                           (get-in (second (answer handler :get base {})) ["workspace" "git" "branch"])))
                (expect (= 400
                           (first (answer handler :patch (str base "/branch") {"branch" "--detach"})))))))
          (finally (delete-tree! repo))))))

(defdescribe provider-routes-answer-their-schema
             (it "provider status and the provider model list answer their schema"
                 ;; The live catalog is a network call; the stub gives its model ids.
                 (with-redefs [providers/refresh-models-async!
                               (fn [& _]
                                 nil)

                               providers/fetch-models
                               (constantly ["model-b" "model-a"])]

                   (with-gateway
                     (fn [_ handler]
                       (let [[status body] (answer handler :get "/v1/providers/openai/status" {})]
                         (expect (= 200 status))
                         (expect (boolean? (get-in body ["status" "is_authenticated"]))))
                       (let [[status body] (answer handler :get "/v1/providers/openai/models" {})]
                         (expect (= 200 status))
                         (expect (every? string? (get body "models")))))))))

(defdescribe speech-model-routes-answer-their-schema
             (it "the voice and speech model state answers its schema"
                 ;; A local speech model is the outside boundary; the stub engine is ready.
                 (with-redefs [speech/engines
                               (fn [direction]
                                 [{:id (if (= :synthesize direction) :speaker :listener)
                                   :label "Local"
                                   :voices (constantly [{:id :alba :label "Alba" :language :en-GB}])
                                   :model-state (constantly {:state :ready})}])

                               speech/env-engine-id
                               (constantly nil)]

                   (with-gateway (fn [_ handler]
                                   (expect (= [200 {"status" "ready" "engine" "listener"}]
                                              (answer handler :get "/v1/voice/model" {})))
                                   (expect (= [200 {"status" "ready" "engine" "speaker"}]
                                              (answer handler :get "/v1/speech/model" {}))))))))
