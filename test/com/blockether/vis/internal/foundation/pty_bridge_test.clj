(ns com.blockether.vis.internal.foundation.pty-bridge-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.pty-bridge :as pb]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.net UnixDomainSocketAddress)
           (java.nio ByteBuffer)
           (java.nio.channels SocketChannel)
           (java.nio.file Paths)))

(defn- fake-pty
  "A PTY-handle stand-in exposing just the surface `serve!` uses: `:add-listener`
   (records subscribers so a test can drive live output) and `:send` (records
   bytes forwarded from a client)."
  []
  (let [listeners
        (atom [])

        sent
        (atom [])]

    {:listeners listeners
     :sent sent
     :handle {:add-listener (fn [f]
                              (swap! listeners conj f)
                              (fn []
                                (swap! listeners (fn [ls]
                                                   (vec (remove #(identical? % f) ls))))))
              :send (fn [^bytes b]
                      (swap! sent conj (String. b)))}}))

(defn- tmp-sock
  []
  (str (System/getProperty "java.io.tmpdir") "vis-bridge-test-" (System/nanoTime) ".sock"))

(defn- connect
  ^SocketChannel [path]
  (SocketChannel/open (UnixDomainSocketAddress/of (Paths/get ^String path (make-array String 0)))))

(defn- read-str
  "Read whatever's available after a short settle, as a String."
  [^SocketChannel ch]
  (Thread/sleep 150)
  (let [buf
        (ByteBuffer/allocate 1024)

        _n
        (.read ch buf)]

    (.flip buf)
    (let [ba (byte-array (.remaining buf))]
      (.get buf ba)
      (String. ba))))

(defdescribe pty-bridge-test
             (it "socket-path encodes session + id and find-socket matches by id suffix"
                 (let [p (pb/socket-path "sess-abc" "dev-server")]
                   (expect (str/ends-with? (str (.getFileName p)) "__dev-server.sock"))
                   (expect (str/includes? (str (.getFileName p)) "sess-abc"))))
             (it "replays recent output, tees live output, and forwards client input"
                 (let [{:keys [sent handle listeners]}
                       (fake-pty)

                       path
                       (tmp-sock)

                       {:keys [stop] sp :path}
                       (pb/serve! {:pty handle
                                   :path path
                                   :replay-fn (fn []
                                                (.getBytes "REPLAY\n"))})]

                   (try (expect (.exists (File. ^String sp)))
                        (with-open [ch (connect sp)]
                          ;; late attacher gets the replay buffer
                          (expect (= "REPLAY\n" (read-str ch)))
                          ;; live master output is teed to the attached client
                          (Thread/sleep 50)
                          (doseq [l @listeners]
                            (l (.getBytes "LIVE\n")))
                          (expect (= "LIVE\n" (read-str ch)))
                          ;; bytes the human types flow into the pty master (:send)
                          (.write ch (ByteBuffer/wrap (.getBytes "typed\n")))
                          (Thread/sleep 150)
                          (expect (= ["typed\n"] @sent)))
                        (finally (stop)))
                   ;; stop unlinks the socket file
                   (expect (not (.exists (File. ^String sp))))))
             (it "find-socket resolves an explicit socket path"
                 (let [{:keys [handle]}
                       (fake-pty)

                       path
                       (tmp-sock)

                       {:keys [stop] sp :path}
                       (pb/serve! {:pty handle :path path})]

                   (try (expect (= sp (str (pb/find-socket {:socket sp})))) (finally (stop)))))
             (it "attach! returns exit code 2 for a missing socket"
                 (expect (= 2
                            (pb/attach! {:socket (str (System/getProperty "java.io.tmpdir")
                                                      "vis-nope-"
                                                      (System/nanoTime)
                                                      ".sock")})))))
