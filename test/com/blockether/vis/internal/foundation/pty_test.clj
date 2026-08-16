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
                 (let
                   [^ServerSocket listener
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
