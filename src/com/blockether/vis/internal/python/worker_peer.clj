(ns com.blockether.vis.internal.python.worker-peer
  "Host-side client of the runtime worker's JSON-line protocol.
   Dispatch and cancellation stay in Vis; interpreter operations live in the runtime."
  (:require [charred.api :as json]
            [clojure.string :as str])
  (:import (java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter)
           (java.nio.channels Channels SocketChannel)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent Executors ExecutorService)
           (java.util.concurrent.atomic AtomicLong)))

(set! *warn-on-reflection* true)

(defn peer-over
  "A peer over `channel`: what to read, what to write, who is waiting for a
   reply, and which of the peer's requests this side is still serving."
  [^SocketChannel channel]
  {:channel channel
   :reader (BufferedReader. (InputStreamReader. (Channels/newInputStream channel)
                                                StandardCharsets/UTF_8))
   :writer (BufferedWriter. (OutputStreamWriter. (Channels/newOutputStream channel)
                                                 StandardCharsets/UTF_8))
   :pending (atom {})
   :serving (atom {})
   :seq (AtomicLong. 0)
   :workers (Executors/newCachedThreadPool
              (reify
                java.util.concurrent.ThreadFactory
                  (newThread [_ runnable]
                    (doto (Thread. ^Runnable runnable "vis-python-worker") (.setDaemon true)))))})

(defn send-line!
  "Write one message. Synchronized because both a reply and a fresh request can
   be written from different threads and a torn line is unparseable."
  [peer message]
  (let [^BufferedWriter writer (:writer peer)]
    (locking writer
      (.write writer ^String (json/write-json-str message))
      (.write writer "\n")
      (.flush writer))))

(defn request!
  "Ask the peer `message` and answer its reply value; its error throws here.
   `timeout-ms` bounds CONTROL messages only; ordinary work waits for its real
   result."
  ([peer message] (request! peer message nil))
  ([peer message timeout-ms]
   (let [id
         (.incrementAndGet ^AtomicLong (:seq peer))

         waiting
         (promise)]

     (swap! (:pending peer) assoc id waiting)
     (try (send-line! peer (assoc message "id" id))
          (let [reply (if timeout-ms (deref waiting (long timeout-ms) ::timed-out) @waiting)]
            (when (identical? ::timed-out reply)
              (throw (ex-info (str "the python worker did not answer " (get message "op"))
                              {:type :vis/python-worker-timeout
                               :op (get message "op")
                               :timeout-ms timeout-ms})))
            (if (contains? reply "error")
              (throw (ex-info (str (get reply "error"))
                              {:type :vis/python-worker :op (get message "op")}))
              (get reply "value")))
          (finally (swap! (:pending peer) dissoc id))))))

(defn pump!
  "Read this peer until it closes: a reply settles whoever waits for it, a
   request goes to `serve` on a thread of its own — serving inline would stall
   the pump behind a call that is itself waiting on this peer.

   On close every pending call fails with `reason`, because a caller parked on a
   child that has died would otherwise wait forever."
  [peer serve reason]
  (try (loop []

         (when-let [line (.readLine ^BufferedReader (:reader peer))]
           (when-not (str/blank? line)
             (let [message (json/read-json line :key-fn identity)]
               (if (contains? message "op")
                 (.submit ^ExecutorService (:workers peer) ^Runnable #(serve peer message))
                 (some-> (get @(:pending peer) (get message "id"))
                         (deliver message)))))
           (recur)))
       (catch Throwable _ nil)
       (finally (doseq [[_ waiting] @(:pending peer)]
                  (deliver waiting {"error" (reason)}))
                (.shutdownNow ^ExecutorService (:workers peer)))))

(defn claim-reply!
  "True exactly once per served request `id`: whoever answers it first — the tool
   that finished, or the interrupt that failed it — is the only answer the child
   ever reads. A late answer for an id already claimed is dropped here, never sent."
  [peer id]
  (let [[before _] (swap-vals! (:serving peer) dissoc id)]
    (contains? before id)))
