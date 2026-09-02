(ns com.blockether.vis.internal.foundation.pty-test
  "What a libvisjail PTY child is allowed to inherit.

   The native spawn boundary must close every descriptor except its stdio and explicit
   control pipes. A child once inherited the gateway's listening socket; if that child
   outlived the gateway, the next start could not bind a port that nothing served."
  (:require [com.blockether.vis.internal.foundation.pty :as pty]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.net InetSocketAddress ServerSocket)))

(defn- sleep-binary
  "The native spawn ABI takes an absolute executable path."
  []
  (first (filter #(.exists (File. ^String %)) ["/bin/sleep" "/usr/bin/sleep"])))

(defn- bindable?
  "Can THIS process bind `port` on the wildcard address? A listening socket held
   by any other process answers no — SO_REUSEADDR only ever forgives TIME_WAIT."
  [port]
  (try (with-open [s (ServerSocket.)]
         (.bind s (InetSocketAddress. port))
         true)
       (catch java.io.IOException _ false)))

(defdescribe pty-inheritance-test
             (it "frees a listening port the parent closed while the child still runs"
                 ;; The gateway's bug, reduced: bind a port, spawn a pty child, drop the
                 ;; parent's socket. The port is free only if the child never got a copy.
                 (let [^ServerSocket listener
                       (doto (ServerSocket.) (.bind (InetSocketAddress. 0)))

                       port
                       (.getLocalPort listener)

                       child
                       (pty/spawn! {:command [(sleep-binary) "30"] :env {"PATH" "/usr/bin:/bin"}})]

                   (try (expect (false? (bindable? port)) "the parent still holds it")
                        (.close listener)
                        (expect
                          (true? (bindable? port))
                          "a pty child inherited the listening socket and is squatting on the port")
                        (finally ((:destroy child) true) ((:wait child)))))))

(defn- sh-binary
  "The native spawn ABI takes an absolute executable path."
  []
  (first (filter #(.exists (File. ^String %)) ["/bin/sh" "/usr/bin/sh"])))

;; Regression, CI flake on macos-latest: a provider callback that shelled out for
;; `printf regular-shell` came back with EMPTY output while the command exited 0.
;; The parent dropped its pty slave descriptor at spawn, so the CHILD's exit was
;; the LAST close of that terminal — and on macOS the last close REVOKES a tty and
;; discards whatever it still holds. Anything the child printed but the reader
;; thread had not copied out yet was gone, and the shell reported a command that
;; printed nothing.
(defdescribe pty-exit-output-test
             (it "keeps output the child wrote while nothing was reading the terminal"
                 (let [;; Parks the reader thread INSIDE the first chunk's fan-out, so the tail the
                       ;; child prints next is still sitting in the terminal when the child exits.
                       release
                       (promise)

                       chunks
                       (atom 0)

                       child
                       (pty/spawn! {:command [(sh-binary) "-c"
                                              "printf head-chunk; sleep 1; printf tail-chunk"]
                                    :env {"PATH" "/usr/bin:/bin"}})

                       unsubscribe
                       ((:add-listener child)
                         (fn [_]
                           (when (= 1 (swap! chunks inc)) (deref release 20000 nil))))

                       output
                       (future (slurp (:in child)))]

                   (try (Thread/sleep 2500)
                        (deliver release true)
                        (expect (= "head-chunktail-chunk" (deref output 20000 ::timed-out))
                                "the pty lost what the child printed just before it exited")
                        (expect (zero? (long ((:wait child)))))
                        (finally (unsubscribe) ((:destroy child) true))))))
