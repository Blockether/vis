(ns com.blockether.vis.ext.foundation-voice.files
  "Streaming download, checksum and archive helpers, shared by everything this
   extension installs: sherpa-onnx's native libraries for THIS platform, the
   Parakeet ASR model and every speech asset in the manifest."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.net URI]
           [java.security MessageDigest]
           [org.apache.commons.compress.archivers.tar TarArchiveInputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(defonce ^:private download-http-client (delay (http/client {:connect-timeout 20000})))

(def ^:private ^:const max-redirects 5)

(defn- header
  [response name]
  (let [v (get-in response [:headers name])]
    (if (sequential? v) (first v) v)))

(defn- hex ^String [^bytes digest] (str/join (map #(format "%02x" (bit-and (long %) 0xff)) digest)))

(defn- open-stream!
  "GET `url` as a stream, following redirects BY HAND so that `headers` — which
   may carry a bearer token — are DROPPED the moment the host changes. Hugging
   Face answers a 302 to an unauthenticated CDN, and a client that replayed the
   Authorization header there would hand the user's token to a third party.
   Returns the response whose `:body` is the stream."
  [url headers]
  (loop
    [url
     url

     headers
     (or headers {})

     hops
     0]

    (let
      [response
       (http/get url
                 {:client @download-http-client
                  :as :stream
                  :timeout 120000
                  :throw false
                  :follow-redirects :never
                  :headers headers})

       status
       (long (:status response))

       next-url
       (header response "location")]

      (cond (and (<= 300 status 399) next-url (< hops (long max-redirects)))
            (let
              [from
               (URI/create url)

               to
               (.resolve from (URI/create next-url))]

              (try (.close ^java.io.InputStream (:body response)) (catch Throwable _))
              (recur (str to) (if (= (.getHost to) (.getHost from)) headers {}) (inc hops)))
            (<= 400 status)
            (do (try (.close ^java.io.InputStream (:body response)) (catch Throwable _))
                ;; The status and the URL, never the request headers: one of them
                ;; may be a token.
                (throw (ex-info (str "Download refused with HTTP " status)
                                {:type :voice/download-refused :url url :status status})))
            :else response))))

(defn download!
  "Stream `url` to `path`, calling `(on-progress pct)` (0..99) as bytes land when
   the server reports a content length. `opts` may carry `:on-progress`,
   `:headers` and `:sha256`.

   Both timeouts are set: a silently stalled socket must FAIL (so the state
   machine can report :failed and the user can retry) rather than pin the
   download atom on :downloading forever, which leaves the UI's mic dead.

   A body that ENDS EARLY fails, and so does one whose bytes do not hash to
   `:sha256`. That is the integrity check on everything this extension installs:
   a truncated or substituted `.onnx` or `.dylib` is still a PRESENT file, so a
   caller that only asks `.isFile` would install it and then abort the JVM on
   the next native load. The digest is computed AS the bytes stream past, so
   verifying costs no second read and no second copy."
  [url path {:keys [on-progress headers sha256]}]
  (.mkdirs (.getParentFile (io/file path)))
  (let
    [uri
     (URI/create url)

     response
     (if (= "file" (.getScheme uri))
       (let [file (io/file uri)]
         {:body (io/input-stream file) :headers {"content-length" (str (.length file))}})
       (open-stream! url headers))

     ^long total
     (try (Long/parseLong (str (header response "content-length"))) (catch Throwable _ -1))

     ^MessageDigest digest
     (MessageDigest/getInstance "SHA-256")

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
                   (.update digest buf 0 n)
                   (let [done' (+ done n)]
                     (when (and on-progress (pos? total))
                       (on-progress (min 99 (long (* 100 (/ (double done') (double total)))))))
                     (recur done'))))))))

     actual
     (hex (.digest digest))]

    (when (and (pos? total) (not= total written))
      (.delete (io/file path))
      (throw (ex-info "Download ended before the announced length"
                      {:type :voice/download-truncated
                       :url url
                       :expected-bytes total
                       :received-bytes written})))
    (when (and (seq sha256) (not= (str/lower-case (str sha256)) actual))
      (.delete (io/file path))
      (throw
        (ex-info
          "Downloaded bytes do not match the checksum in the manifest"
          {:type :voice/checksum-mismatch :url url :expected-sha256 sha256 :actual-sha256 actual})))
    path))

(defn delete-dir!
  "Recursively delete `f`. Used to clear a staging dir and to replace an
   installed one, so it must not care whether `f` is a file or a tree."
  [^File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)]
      (delete-dir! c)))
  (.delete f))

(defn- safe-entry-name
  [entry-name]
  (let
    [parts (->> (str/split entry-name #"/")
                (remove str/blank?)
                ;; Release archives contain a top-level directory. Strip it so
                ;; the installed layout is the same whichever source delivered
                ;; it — Hugging Face serves the files without that wrapper.
                rest)]
    (when (and (seq parts) (not-any? #(or (= % "..") (str/includes? % "\\")) parts))
      (str/join File/separator parts))))

(defn extract-tar-bz2!
  "Extract `archive-path` into `target-dir`. Decompressing a several-hundred-MB
   bzip2 model takes MINUTES, so `on-progress` (0..99, optional) is driven by how
   far the COMPRESSED file has been consumed — read straight off the file
   channel, so the copy loop stays a plain read/write and no UI has to sit on a
   frozen number."
  ([archive-path target-dir] (extract-tar-bz2! archive-path target-dir nil))
  ([archive-path target-dir on-progress]
   (.mkdirs (io/file target-dir))
   (let
     [archive
      (io/file archive-path)

      total
      (.length archive)]

     (with-open
       [^FileInputStream fis
        (FileInputStream. archive)

        bz
        (BZip2CompressorInputStream. fis)

        tar
        (TarArchiveInputStream. bz)]

       (let
         [^java.nio.channels.FileChannel channel
          (.getChannel fis)

          buf
          (byte-array 262144)

          report!
          (fn []
            (when (and on-progress (pos? total))
              (on-progress (min 99
                                (long (* 100 (/ (double (.position channel)) (double total))))))))]

         (loop []

           (when-let [entry (.getNextTarEntry tar)]
             (when-let [relative (safe-entry-name (.getName entry))]
               (let [out-file (io/file target-dir relative)]
                 (if (.isDirectory entry)
                   (.mkdirs out-file)
                   (do (.mkdirs (.getParentFile out-file))
                       (with-open [out (FileOutputStream. out-file)]
                         (loop []

                           (let [n (.read tar buf)]
                             (when-not (neg? n) (.write out buf 0 n) (report!) (recur)))))))))
             (recur))))))
   target-dir))
