(ns com.blockether.vis.tui.test-environment-test
  (:require [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.test-environment :as test-environment]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.net ConnectException Socket]))

(defdescribe no-live-gateway-test
             (it "points the client at a closed loopback port without a token"
                 ;; The hook already ran for this suite; a second install changes nothing.
                 (expect (true? (test-environment/install!)))
                 (let [{:keys [host port secret]} @@#'client/target*]
                   (expect (= "127.0.0.1" host))
                   (expect (nil? secret))
                   (expect (instance? ConnectException
                                      (try (.close (Socket. ^String host (int port)))
                                           nil
                                           (catch ConnectException e e)))))))
