(ns com.blockether.vis.ext.foundation-voice.files
  "Streaming download and directory helpers, shared by the two things this
   extension fetches: sherpa-onnx's native libraries for THIS platform and the
   Parakeet ASR model."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io])
  (:import [java.io File FileOutputStream]
           [java.net URI]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(defonce ^:private download-http-client (delay (http/client {:connect-timeout 20000})))

(defn download!
  "Stream `url` to `path`, calling `(on-progress pct)` (0..99) as bytes land
   when the server reports a content length. nil `on-progress` is fine.
   Both timeouts are set: a silently stalled socket must FAIL (so the state
   machine can report :failed and the user can retry) rather than pin the
   download atom on :downloading forever, which leaves the UI's mic dead.

   A body that ENDS EARLY fails too, and that is the integrity check on
   everything this extension installs: a truncated `.onnx` or `.dylib` is still
   a PRESENT file, so a caller that only asks `.isFile` would install it and
   then abort the JVM on the next native load. When the server declared a
   length, the bytes written have to match it."
  [url path on-progress]
  (.mkdirs (.getParentFile (io/file path)))
  (let
    [uri
     (URI/create url)

     response
     (if (= "file" (.getScheme uri))
       (let [file (io/file uri)]
         {:body (io/input-stream file) :headers {"content-length" (str (.length file))}})
       (http/get url {:client @download-http-client :as :stream :timeout 120000 :throw true}))

     raw-total
     (get-in response [:headers "content-length"])

     raw-total
     (if (sequential? raw-total) (first raw-total) raw-total)

     ^long total
     (try (Long/parseLong (str raw-total)) (catch Throwable _ -1))

     written
     (with-open
       [^java.io.InputStream in
        (:body response)

        ^FileOutputStream out
        (FileOutputStream. (io/file path))]

       (let [buf (byte-array 1048576)]
         (loop [done 0]
           (let [n (.read in buf)]
             (if (neg? n)
               done
               (do (.write out buf 0 n)
                   (let [done' (+ done n)]
                     (when (and on-progress (pos? total))
                       (on-progress (min 99 (long (* 100 (/ (double done') (double total)))))))
                     (recur done'))))))))]

    (when (and (pos? total) (not= total written))
      (.delete (io/file path))
      (throw (ex-info "Download ended before the announced length"
                      {:type :voice/download-truncated
                       :url url
                       :expected-bytes total
                       :received-bytes written})))
    path))

(defn delete-dir!
  "Recursively delete `f`. Used to clear a staging dir and to replace an
   installed one, so it must not care whether `f` is a file or a tree."
  [^File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)]
      (delete-dir! c)))
  (.delete f))
