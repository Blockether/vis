(ns com.blockether.vis.internal.gateway.server.fs
  "File-system routes: directory browsing, workspace roots, and reading or opening
   a session's files."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.attachment.core :as attachment]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.external-opener :as external-opener]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [ring.core.protocols :as ring-protocols]))

;; Filesystem browse — picking a workspace root
;;
;; A machine OWNS its projects, so the only place that knows which folders exist
;; is the machine itself. The companion's "Switch project" sheet walks THIS tree;
;; it commits a folder and lets `POST /v1/sessions {root}` decide what that folder
;; is. Directories only — a picker that offers files offers a root that cannot be
;; one — and the two facts a chooser actually reads: how much is in it, and the
;; branch if it is a worktree.

(def ^:private fs-entry-limit
  "A home folder with 4000 entries is a scroll, not a picker. The client is told
   the list was cut so it can say `typing the path` instead of lying by omission."
  400)

(defn- fs-home [] (System/getProperty "user.home"))

(defn- expand-user
  "`~` and `~/x` name the GATEWAY user's home, never the phone's."
  [path]
  (let [path (str/trim (str path))]
    (cond (str/blank? path) (fs-home)
          (= "~" path) (fs-home)
          (str/starts-with? path "~/") (.getAbsolutePath (io/file (fs-home) (subs path 2)))
          :else path)))

(defn- git-branch
  "The branch `dir` has checked out, or nil when it is not a worktree. Read from
   `.git/HEAD` rather than shelled out: this runs once per row of a listing."
  [^java.io.File dir]
  (let [head (io/file dir ".git" "HEAD")]
    (when (.isFile head)
      (let [line (str/trim (slurp head))]
        (or (second (re-find #"^ref:\s+refs/heads/(.+)$" line))
            (when-not (str/blank? line) (subs line 0 (min 7 (count line)))))))))

(defn- fs-entry
  [^java.io.File dir]
  (let [kids
        (.listFiles dir)

        branch
        (git-branch dir)]

    (cond-> {:name (.getName dir)
             :path (.getAbsolutePath dir)
             :entry-count (if kids (alength kids) 0)
             :is-repo (some? branch)}
      branch
      (assoc :branch branch))))

(defn- browse-fs-handler
  "GET /v1/fs[?path=…] — the directories inside `path` (default: this user's home),
   so a client can pick a workspace root by recognition instead of by typing one.
   Dotfolders are skipped: they are not projects, and they are most of `~`."
  [request]
  (let [^java.io.File dir (io/file (expand-user (get-in request [:query-params "path"])))]
    (cond (not (.isDirectory dir)) (http/error-response 404
                                                        :not-a-directory "no such directory"
                                                        :path (.getAbsolutePath dir))
          (not (.canRead dir)) (http/error-response 403
                                                    :directory-unreadable
                                                    "that directory is not readable"
                                                    :path (.getAbsolutePath dir))
          :else (let [kids (or (.listFiles dir) (make-array java.io.File 0))
                      dirs (->> kids
                                (filter (fn [^java.io.File f]
                                          (and (.isDirectory f)
                                               (not (str/starts-with? (.getName f) ".")))))
                                (sort-by (fn [^java.io.File f]
                                           (str/lower-case (.getName f)))))]

                  (http/json-response {:path (.getAbsolutePath dir)
                                       :parent (some-> (.getParentFile dir)
                                                       .getAbsolutePath)
                                       :home (fs-home)
                                       :is-truncated (boolean (> (count dirs)
                                                                 (long fs-entry-limit)))
                                       :entries (mapv fs-entry (take fs-entry-limit dirs))})))))

(defn- create-directory-handler
  "POST /v1/fs/actions/mkdir {path, name} — one folder inside `path`, so a project
   can start somewhere that does not exist yet. One SEGMENT only: a picker that
   silently accepts `a/b/../..` is a picker that writes outside what it showed."
  [request]
  (let [{:strs [path name]}
        (http/body-json request)

        ^java.io.File parent
        (io/file (expand-user path))

        folder
        (str/trim (str name))]

    (cond (str/blank? folder)
          (http/error-response 400 :invalid-request "name must be a non-blank string")
          (or (re-find #"[/\\]" folder) (contains? #{"." ".."} folder))
          (http/error-response 400 :invalid-request "name must be a single folder name")
          (not (.isDirectory parent)) (http/error-response 404
                                                           :not-a-directory "no such directory"
                                                           :path (.getAbsolutePath parent))
          :else (let [^java.io.File made (io/file parent folder)]
                  (if (or (.isDirectory made) (.mkdir made))
                    (http/json-response 201 (fs-entry made))
                    (http/error-response 400
                                         :mkdir-failed "could not create that folder"
                                         :path (.getAbsolutePath made)))))))

(defn- session-readable-file
  "Resolve a file using the session's live filesystem roots and read gates. The same
   resolver serves previews and editor opens, so neither can bypass draft or deny rules."
  [sid requested]
  (let [root (get (state/session-workspace-info sid) "root")]
    (cond
      (or (not (string? requested)) (str/blank? requested))
      {:error (http/error-response 400 :invalid-request "path must be a non-blank string")}
      (str/blank? (str root))
      {:error (http/error-response 409 :workspace-unavailable "Session workspace is unavailable.")}
      :else
      (try
        (let [env (lp/env-for sid)
              ws (if-let [a (:workspace-atom env)]
                   @a
                   (:workspace env))
              active-root (workspace/normalize-root (or (:root ws) (:workspace/root env)))]

          (if (or (nil? env) (nil? active-root) (not= active-root (workspace/normalize-root root)))
            {:error
             (http/error-response 409 :workspace-unavailable "Session workspace is unavailable.")}
            (binding [workspace/*workspace-root* active-root
                      workspace/*filesystem-roots* (workspace/env-filesystem-roots env)]

              (let [^java.io.File target (workspace/resolve-file-path requested)
                    path (.getPath target)
                    refusal (or (when-let [reason (process-jail/deny-refusal env "file-read" path)]
                                  {:reason reason})
                                (when (extension/gate-hooked? :fs/access)
                                  (extension/run-gate-hooks :fs/access
                                                            env
                                                            {:operation "file-read" :path path})))]

                (cond refusal {:error (http/error-response 403
                                                           :file-access-denied (:reason refusal)
                                                           :path path)}
                      (not (.isFile target))
                      {:error (http/error-response 404 :not-a-file "no such file" :path path)}
                      :else {:file target})))))
        (catch clojure.lang.ExceptionInfo e
          (let [type (:type (ex-data e))]
            {:error
             (case type
               (:workspace/path-escape :workspace/path-denied)
               (http/error-response 403 :file-access-denied (ex-message e))

               (http/error-response 400 :invalid-request "that path could not be resolved"))}))
        (catch java.io.IOException _
          {:error (http/error-response 400 :invalid-request "that path could not be resolved")})
        (catch java.nio.file.InvalidPathException _
          {:error (http/error-response 400 :invalid-request "that path could not be resolved")})))))

(defn- open-file-handler
  "POST /v1/sessions/:sid/fs/actions/open {path} — open ONE file this session
   named, on the machine the session runs on.

   A step reports the files it read or patched by their absolute path, and a
   reader who can only look at that path has to find the file again by hand.
   The machine already knows where it is, so it hands it to the operator's
   editor — the same press the TUI gives a path on the transcript.

   Access follows the session's live read roots, draft mappings and file gates —
   never the client's filesystem scope."
  [request]
  (let [sid (http/path-sid request)]
    (if-not (and sid (state/soul sid))
      (http/session-404 (get-in request [:path-params :sid]))
      (let [found (session-readable-file sid (get (http/body-json request) "path"))
            ^java.io.File target (:file found)]

        (or (:error found)
            (let [{:keys [status error]} (external-opener/open-file-in-editor! (.getPath target))]
              (if (= :ok status)
                (http/json-response {:path (.getPath target) :is-open true})
                (http/error-response 400
                                     :open-failed (or error "that file could not be opened")
                                     :path (.getPath target)))))))))

(def ^:private preview-line-limit
  "Lines one preview answers with. A reader is looking AT a place in a file, not
   downloading the file; this covers a screen and the context around it."
  400)

(def ^:private preview-line-length
  "Characters one previewed line keeps. A minified bundle is one line of several
   megabytes, and no screen shows it."
  2000)

(def ^:private preview-byte-limit
  "Bytes a preview reads off disk before it stops, however deep the anchor sits.
   A file that big is a log or a bundle; the window says it was cut short."
  (* 2 1024 1024))

(defn- binary-file?
  "Text has no NUL byte in it. Sniffing the head is what `git` and `grep` do, and
   it keeps an image, an archive or a class file out of a text window."
  [^java.io.File file]
  (with-open [in (java.io.FileInputStream. file)]
    (let [buffer (byte-array 4096)
          read (.read in buffer)]

      (boolean (some zero? (take (max read 0) (seq buffer)))))))

(defn- file-window
  "The previewed lines of `file` from line `from`, each clipped, under the byte
   cap — with `is-truncated` when the cap stopped the read before the window ended."
  [^java.io.File file ^long from]
  (let [last-line
        (+ from (dec (long preview-line-limit)))

        byte-limit
        (long preview-byte-limit)

        line-length
        (long preview-line-length)]

    (with-open [^java.io.BufferedReader reader (io/reader file)]
      (loop [number 1
             bytes 0
             taken (transient [])]

        (let [line (when (and (<= number last-line) (< bytes byte-limit)) (.readLine reader))]
          (if (nil? line)
            {:lines (persistent! taken)
             :is-truncated (and (<= number last-line) (>= bytes byte-limit))}
            (recur (inc number)
                   (+ bytes (count line) 1)
                   (if (>= number from)
                     (conj! taken
                            (cond-> line
                              (> (count line) line-length)
                              (subs 0 preview-line-length)))
                     taken))))))))

(def ^:private image-byte-limit
  "Bytes one picture preview reads. A screenshot or a chart fits well under it; a file
   above it is a raw photo or a texture atlas, and the editor opens it on its machine.
   The picture goes as base64 inside JSON, which is 4/3 larger and held in memory more
   than once, and a phone decodes it. The contract sets the cap at 8 MiB."
  gateway-contract/max-preview-image-bytes)

(defn- bytes-at?
  "True when `head` holds the Latin-1 bytes of `ascii` from index `at`."
  [^bytes head at ^String ascii]
  (let [^bytes wanted
        (.getBytes ascii java.nio.charset.StandardCharsets/ISO_8859_1)

        from
        (int at)

        to
        (+ from (alength wanted))]

    (and (<= to (alength head)) (java.util.Arrays/equals head from to wanted 0 (alength wanted)))))

(defn- image-media-type
  "The media type of a picture that a browser draws, read from the first bytes of
   `head`, or nil. The bytes decide, not the file name: a `.png` with text stays out."
  [^bytes head]
  (cond (bytes-at? head 0 "\u0089PNG\r\n\u001a\n") "image/png"
        (bytes-at? head 0 "ÿØÿ") "image/jpeg"
        (or (bytes-at? head 0 "GIF87a") (bytes-at? head 0 "GIF89a")) "image/gif"
        (and (bytes-at? head 0 "RIFF") (bytes-at? head 8 "WEBP")) "image/webp"
        (and (bytes-at? head 4 "ftyp") (or (bytes-at? head 8 "avif") (bytes-at? head 8 "avis")))
        "image/avif"))

(defn- file-head
  "The first bytes of `file`, enough for `image-media-type` and `playable-media-type`.
   A Matroska header names its `webm` doc type in its first 64 bytes."
  ^bytes [^java.io.File file]
  (with-open [in (java.io.FileInputStream. file)]
    (.readNBytes in 64)))

(defn- mpeg-audio-frame?
  "True when `head` starts with an MPEG audio frame header that a player decodes:
   the frame sync, layer II or III, a real bit rate and a real sample rate.
   The loose sync test also accepts the UTF-16 byte order mark `FF FE`, and a
   text file is not a recording."
  [^bytes head]
  (and (>= (alength head) 3)
       (let [b1
             (bit-and (long (aget head 1)) 0xff)

             b2
             (bit-and (long (aget head 2)) 0xff)

             layer
             (bit-and (bit-shift-right b1 1) 3)

             bitrate
             (bit-shift-right b2 4)

             rate
             (bit-and (bit-shift-right b2 2) 3)]

         (and (= 0xff (bit-and (long (aget head 0)) 0xff))
              (= 0xe0 (bit-and b1 0xe0))
              (contains? #{1 2} layer)
              (< 0 bitrate 15)
              (not= 3 rate)))))

(defn- playable-media-type
  "The media type of a recording or a clip that a media element plays, read from
   the first bytes of `head`, or nil. The bytes decide, not the file name."
  [^bytes head]
  (let [audio (attachment/detect-audio-mime head)]
    (cond (and (= "audio/mpeg" audio) (not (bytes-at? head 0 "ID3")) (not (mpeg-audio-frame? head)))
          nil
          audio audio
          (attachment/detect-video-mime head) (attachment/detect-video-mime head)
          (bytes-at? head 0 "\u001aEß£")
          (if (str/includes? (String. head java.nio.charset.StandardCharsets/ISO_8859_1) "webm")
            "video/webm"
            "video/x-matroska"))))

;; Media content tickets
;;
;; An `<audio>` or `<video>` element fetches its source itself, with `Range`
;; requests, and it cannot add a bearer header or the protocol headers. The
;; preview hands out a signed address instead: the ticket names one session, one
;; resolved file and one expiry, and nothing else.

(def ^:private content-ticket-ttl-ms
  "How long one content address plays. A long episode is paused and resumed; a
   reader who comes back later reads the preview again for a new address."
  (* 12 60 60 1000))

(def ^:private content-route #"^/v1/sessions/[^/]+/fs/file/content$")

(defn content-ticket-request?
  "True when `request` asks for file content with a ticket. The auth and protocol
   gates let it through, because the handler checks the ticket itself."
  [request]
  (boolean (and (re-matches content-route (str (:uri request)))
                (let [ticket (get-in request [:query-params "ticket"])]
                  (and (string? ticket) (not (str/blank? ticket)))))))

(defonce ^:private process-ticket-key
  ;; Realized on first use at run time, never in the native-image heap.
  (delay (util/random-bytes 32)))

(defn- ticket-key
  "The key of content tickets: derived from the gateway token, so an address stays
   valid across a restart, else a random key of this process."
  ^bytes []
  (let [token (:token @instance/server-state)]
    (if (and (string? token) (not (str/blank? token)))
      (util/hmac-sha256 (util/utf8 token) (util/utf8 "vis fs content ticket v1"))
      @process-ticket-key)))

(defn- content-ticket
  "The ticket for `sid`, the resolved `path` and `expires`. The path goes last, so
   no field can move into another."
  ^String [sid path expires]
  (util/base64url (util/hmac-sha256 (ticket-key) (util/utf8 (str sid "\n" expires "\n" path)))))

(defn- url-encode
  ^String [s]
  (str/replace (java.net.URLEncoder/encode (str s) java.nio.charset.StandardCharsets/UTF_8)
               "+"
               "%20"))

(defn- content-url
  "The relative address of the content of `path` for `sid`, with a ticket that
   expires at `expires`."
  [sid path expires]
  (str "/v1/sessions/" sid
       "/fs/file/content?path=" (url-encode path)
       "&expires=" expires
       "&ticket=" (content-ticket sid path expires)))

(defn- file-preview
  "The preview of `target` as one answer whose `kind` says what it holds.

   - `image`: a picture that a browser draws, as base64 `data` with its `media-type`,
     or 413 above `image-byte-limit`.
   - `media`: a recording or a clip, with a signed `content-url` that streams it.
   - `binary`: another file with a NUL byte. Only its path and size come back.
   - `text`: the window of lines around `anchor`."
  [sid ^java.io.File target anchor]
  (let [path
        (.getPath target)

        head
        (file-head target)

        playable
        (playable-media-type head)]

    (if-let [media-type (image-media-type head)]
      (let [^bytes picture (with-open [in (java.io.FileInputStream. target)]
                             (.readNBytes in (int (inc (long image-byte-limit)))))]
        (if (> (alength picture) (long image-byte-limit))
          (http/error-response 413
                               :image-too-large "that picture is too large to preview"
                               :path path)
          (http/json-response {:path path
                               :kind "image"
                               :media-type media-type
                               :size-bytes (alength picture)
                               :data (.encodeToString (java.util.Base64/getEncoder) picture)})))
      (cond playable (let [expires (+ (util/now-ms) (long content-ticket-ttl-ms))]
                       (http/json-response {:path path
                                            :kind "media"
                                            :media-type playable
                                            :size-bytes (.length target)
                                            :content-url (content-url sid path expires)
                                            :expires-at expires}))
            (binary-file? target) (http/json-response
                                    {:path path :kind "binary" :size-bytes (.length target)})
            :else (let [from (max 1 (- (long anchor) (quot preview-line-limit 2)))
                        {:keys [lines is-truncated]} (file-window target from)]

                    (http/json-response {:path path
                                         :kind "text"
                                         :line anchor
                                         :first-line from
                                         :lines lines
                                         :is-truncated is-truncated
                                         :size-bytes (.length target)}))))))

(defn- read-file-handler
  "GET /v1/sessions/:sid/fs/file?path=…&line=… — the preview of one session-readable
   file, with a `kind` of `text`, `image`, `media` or `binary`.

   A path in a transcript names a file on the machine that ran the step. Opening
   an editor there is the right answer for whoever sits at that machine, and no
   answer at all for a reader holding a phone: the file is on the other side of
   the room. This hands back the file itself, so the place a step touched can be
   read where the session is being read. One route answers every file, so a client
   never guesses the kind from the name and asks again (#432).

   A PREVIEW, NOT A DOWNLOAD. Text is one window of clipped lines under a byte cap,
   a picture is capped, and another binary file is never read — the route cannot
   become a way to pull a repository through the gateway one file at a time. Both
   actions use `session-readable-file`."
  [request]
  (let [sid (http/path-sid request)]
    (if-not (and sid (state/soul sid))
      (http/session-404 (get-in request [:path-params :sid]))
      (let [asked-line (not-empty (str (get-in request [:query-params "line"])))
            line (when (and asked-line (re-matches #"\d+" asked-line)) (parse-long asked-line))
            found (session-readable-file sid (get-in request [:query-params "path"]))
            ^java.io.File target (:file found)]

        (cond (:error found) (:error found)
              (and asked-line (not (pos? (long (or line 0)))))
              (http/error-response 400 :invalid-request "line must be a positive whole number")
              :else (try (file-preview sid target (or line 1))
                         (catch java.io.IOException _
                           (http/error-response 400
                                                :invalid-request "that file could not be read"
                                                :path (.getPath target)))))))))

(def ^:private content-chunk-bytes
  "Bytes one read copies from disk to the socket. The file is never held whole."
  (* 64 1024))

(defn- file-range-body
  "A streamed body of `length` bytes of `file` from byte `start`. It reads the file
   in chunks while the socket takes them, so a long recording is never in memory."
  [^java.io.File file ^long start ^long length]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^java.io.OutputStream out
              output-stream

              buffer
              (byte-array content-chunk-bytes)]

          (try (with-open [raf (java.io.RandomAccessFile. file "r")]
                 (.seek raf start)
                 (loop [left length]
                   (when (pos? left)
                     (let [read (.read raf buffer 0 (int (min left (alength buffer))))]
                       (when (pos? read) (.write out buffer 0 read) (recur (- left read)))))))
               (finally (.close out)))))))

(defn- byte-range
  "The one byte range that the `Range` header `header` asks of a file of `size`
   bytes, as `[first last]`, `:unsatisfiable`, or nil for the whole file. A header
   that is not one valid `bytes` range is ignored, and several ranges get the whole
   file: a media element asks for one range at a time."
  [header ^long size]
  (when-let [[_ from to] (and (string? header)
                              (re-matches #"(?i)\s*bytes\s*=\s*(\d*)\s*-\s*(\d*)\s*" header))]
    (let [from-text from
          to-text to
          from (when (seq from-text) (parse-long from-text))
          to (when (seq to-text) (parse-long to-text))]

      (cond
        ;; a number too long for a long is not a range
        (or (and (seq from-text) (nil? from)) (and (seq to-text) (nil? to))) nil
        (and from to (< (long to) (long from))) nil
        from
        (if (>= (long from) size) :unsatisfiable [from (min (dec size) (long (or to (dec size))))])
        to (if (or (zero? (long to)) (zero? size))
             :unsatisfiable
             [(max 0 (- size (long to))) (dec size)])
        :else nil))))

(defn- content-headers
  [media-type]
  {"Content-Type" media-type
   "Accept-Ranges" "bytes"
   "X-Content-Type-Options" "nosniff"
   "Content-Security-Policy" "sandbox"
   "Cache-Control" "private"})

(defn- query-string-param
  "The query param `k` when it is one string, else nil. A repeated param is refused."
  [request k]
  (let [v (get-in request [:query-params k])]
    (when (string? v) v)))

(defn- ticket-refusal
  "The 401 answer when the ticket of `request` does not admit `sid` to `path`, or nil."
  [sid path request]
  (let [expires
        (query-string-param request "expires")

        ticket
        (query-string-param request "ticket")]

    (cond (not (and (string? path) (string? ticket) expires (re-matches #"\d{1,15}" expires)))
          (http/error-response 401 :invalid-ticket "missing or invalid content ticket")
          (not (util/constant-time=? (content-ticket sid path (parse-long expires)) ticket))
          (http/error-response 401 :invalid-ticket "missing or invalid content ticket")
          (< (long (parse-long expires)) (util/now-ms))
          (http/error-response 401
                               :ticket-expired
                               "the content ticket expired; read the file preview again"))))

(defn- read-content-handler
  "GET /v1/sessions/:sid/fs/file/content?path=…&expires=…&ticket=… — the bytes of
   one session-readable recording or clip, streamed from disk with `Range` support.

   A media element fetches its own source and cannot send the bearer token, so the
   signed ticket of the `media` preview is the authorization, on every gateway. The
   read gates run again on each request, because access can change while a long
   episode plays. Only a file whose first bytes are audio or video is served: the
   route cannot become a download of any file."
  [request]
  (let [sid
        (http/path-sid request)

        path
        (query-string-param request "path")]

    ;; The ticket goes first: without one, a caller cannot tell a session that exists.
    (or
      (ticket-refusal sid path request)
      (if-not (state/soul sid)
        (http/session-404 (get-in request [:path-params :sid]))
        (let [found
              (session-readable-file sid path)

              ^java.io.File target
              (:file found)]

          (cond (:error found) (:error found)
                ;; the ticket names the resolved file, so a path must name it the same way
                (not= path (.getPath target))
                (http/error-response 401 :invalid-ticket "missing or invalid content ticket")
                :else (try (if-let [media-type (playable-media-type (file-head target))]
                             (let [size (.length target)
                                   headers (content-headers media-type)
                                   wanted (byte-range (get-in request [:headers "range"]) size)]

                               (cond (= :unsatisfiable wanted)
                                     {:status 416
                                      :headers (assoc headers "Content-Range" (str "bytes */" size))
                                      :body ""}
                                     wanted
                                     (let [[first-byte last-byte] wanted
                                           length (inc (- (long last-byte) (long first-byte)))]

                                       {:status 206
                                        :headers (assoc headers
                                                   "Content-Range"
                                                   (str "bytes " first-byte "-" last-byte "/" size)
                                                   "Content-Length" (str length))
                                        :body (file-range-body target first-byte length)})
                                     :else {:status 200
                                            :headers (assoc headers "Content-Length" (str size))
                                            :body (file-range-body target 0 size)}))
                             (http/error-response 415
                                                  :unsupported-media-type
                                                  "that file is not audio or video"
                                                  :path path))
                           (catch java.io.IOException _
                             (http/error-response 400
                                                  :invalid-request "that file could not be read"
                                                  :path path)))))))))

(defn- workspace-handler
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:workspace (state/session-workspace-info sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- change-root-handler
  [request]
  (if-let [sid (http/path-sid request)]
    (let [{:strs [path]} (http/body-json request)]
      (http/json-response {:workspace (state/change-root! sid path)}))
    (http/session-404 (get-in request [:path-params :sid]))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/fs"] browse-fs-handler
   [:post "/v1/fs/actions/mkdir"] create-directory-handler
   [:get "/v1/sessions/:sid/workspace"] workspace-handler
   [:patch "/v1/sessions/:sid/workspace/root"] change-root-handler
   [:post "/v1/sessions/:sid/fs/actions/open"] open-file-handler
   [:get "/v1/sessions/:sid/fs/file"] read-file-handler
   [:get "/v1/sessions/:sid/fs/file/content"] read-content-handler})
