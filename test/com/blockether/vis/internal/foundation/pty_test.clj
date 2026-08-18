(ns com.blockether.vis.internal.foundation.pty-test
  "What a pty child is allowed to INHERIT.

   `posix_spawn` performs only the file actions it is handed — unlike the JDK's
   own spawn path, whose `jspawnhelper` closes every descriptor above stdio
   before it execs. The JVM sets FD_CLOEXEC on nothing, so a pty child used to
   inherit the whole descriptor table of the process that spawned it, the
   gateway's LISTENING socket included. A child that outlived the gateway and
   never exited on its own then held that socket forever and the next
   `gateway start` failed to bind a port nothing was serving."
  (:require [com.blockether.vis.internal.foundation.pty :as pty]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.net InetSocketAddress ServerSocket)))

(defn- sleep-binary
  "`posix_spawn` takes a PATH, never a PATH lookup."
  ^String []
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
  "`posix_spawn` takes a PATH, never a PATH lookup."
  ^String []
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
