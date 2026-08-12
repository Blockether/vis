(ns com.blockether.vis.internal.foundation.pty-bridge-test
  "The passthrough bridge against a REAL pseudo-terminal. There is no stand-in
   handle here on purpose: a hand-written `{:add-listener :send}` map proves only
   that `serve!` calls two functions, never that a byte typed into the socket
   reaches a terminal and comes back out of it — which is the whole feature."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.pty :as pty]
            [com.blockether.vis.internal.foundation.pty-bridge :as pb]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.net UnixDomainSocketAddress)
           (java.nio ByteBuffer)
           (java.nio.channels SocketChannel)
           (java.nio.file Paths)))

(defn- cat-binary
  "`posix_spawn` takes a PATH, never a PATH lookup, so the smallest echoing child
   is resolved by hand."
  ^String []
  (first (filter #(.exists (File. ^String %)) ["/bin/cat" "/usr/bin/cat"])))

(defn- real-pty
  "A REAL pty running `cat` — the exact handle map production hands `serve!`
   (`internal.foundation.shell/pty-spawn!` returns this shape). Everything
   written to the master is echoed by the terminal and repeated by `cat`, so one
   round trip exercises `:send`, the reader loop and `:add-listener` together."
  []
  (pty/spawn! {:command [(cat-binary)] :env {"PATH" "/usr/bin:/bin" "TERM" "dumb"}}))

(defn- kill!
  [pty]
  (try ((:destroy pty) true) (catch Throwable _ nil))
  (try ((:wait pty)) (catch Throwable _ nil)))

(defn- tmp-sock
  []
  (str (File. (System/getProperty "java.io.tmpdir")
              (str "vis-bridge-test-" (System/nanoTime) ".sock"))))

(defn- connect
  ^SocketChannel [path]
  (doto (SocketChannel/open (UnixDomainSocketAddress/of (Paths/get ^String path
                                                                   (make-array String 0))))
    (.configureBlocking false)))

(defn- type!
  "Write every byte of `s` to the client socket, the way a human's terminal
   would."
  [^SocketChannel ch ^String s]
  (let [buf (ByteBuffer/wrap (.getBytes s))]
    (while (.hasRemaining buf) (.write ch buf))))

(defn- read-until
  "Accumulate what the bridge sends this client until `pred` holds on the text so
   far, or `ms` elapses. Returns the text read — the ASSERTION decides whether
   that was enough, so a timeout fails with the bytes that did arrive."
  ^String [^SocketChannel ch pred ms]
  (let
    [buf
     (ByteBuffer/allocate 4096)

     deadline
     (+ (System/currentTimeMillis) (long ms))]

    (loop [acc ""]
      (.clear buf)
      (let [n (.read ch buf)]
        (cond (pos? n) (let
                         [ba (byte-array n)
                          _ (.flip buf)
                          _ (.get buf ba)
                          acc' (str acc (String. ba))]

                         (if (pred acc') acc' (recur acc')))
              (neg? n) acc
              (or (pred acc) (> (System/currentTimeMillis) deadline)) acc
              :else (do (Thread/sleep 10) (recur acc)))))))

(defdescribe
  pty-bridge-test
  (it "socket-path encodes session + id and find-socket matches by id suffix"
      (let [p (pb/socket-path "sess-abc" "dev-server")]
        (expect (str/ends-with? (str (.getFileName p)) "__dev-server.sock"))
        (expect (str/includes? (str (.getFileName p)) "sess-abc"))))
  (it "replays buffered output, tees a real terminal's live output, and types into it"
      (let
        [pty
         (real-pty)

         path
         (tmp-sock)

         {:keys [stop] sp :path}
         (pb/serve! {:pty pty
                     :path path
                     :replay-fn (fn []
                                  (.getBytes "REPLAY\n"))})]

        (try (expect (.exists (File. ^String sp)))
             (with-open [ch (connect sp)]
               ;; a late attacher gets the replay buffer before anything live
               (expect (str/includes? (read-until ch #(str/includes? % "REPLAY") 2000) "REPLAY"))
               ;; bytes the human types cross the socket, reach the pty MASTER
               ;; (`:send`) and come back out of the terminal as live output teed
               ;; to this client — the whole loop, no stand-in anywhere in it
               (type! ch "hello-from-the-bridge\n")
               (expect (str/includes?
                         (read-until ch #(str/includes? % "hello-from-the-bridge") 10000)
                         "hello-from-the-bridge")))
             (finally (stop) (kill! pty)))
        ;; stop unlinks the socket file
        (expect (not (.exists (File. ^String sp))))))
  (it "find-socket resolves an explicit socket path"
      (let
        [pty
         (real-pty)

         path
         (tmp-sock)

         {:keys [stop] sp :path}
         (pb/serve! {:pty pty :path path})]

        (try (expect (= sp (str (pb/find-socket {:socket sp})))) (finally (stop) (kill! pty)))))
  (it "attach! returns exit code 2 for a missing socket"
      (expect (= 2
                 (pb/attach! {:socket (str (File.
                                             (System/getProperty "java.io.tmpdir")
                                             (str "vis-nope-" (System/nanoTime) ".sock")))})))))
