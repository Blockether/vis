(ns com.blockether.vis.internal.gateway.server.fs
  "File-system routes: directory browsing, workspace roots, and reading or opening
   a session's files."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.external-opener :as external-opener]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.workspace.core :as workspace]))

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

(defn- read-file-handler
  "GET /v1/sessions/:sid/fs/file?path=…&line=… — the LINES of one session-readable file
   around the line a press named.

   A path in a transcript names a file on the machine that ran the step. Opening
   an editor there is the right answer for whoever sits at that machine, and no
   answer at all for a reader holding a phone: the file is on the other side of
   the room. This hands back the text itself, so the place a step touched can be
   read where the session is being read.

   A PREVIEW, NOT A DOWNLOAD. One window of lines, each clipped, under a byte cap,
   and never a binary file — the route cannot become a way to pull a repository
   through the gateway one file at a time. Both actions use `session-readable-file`."
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
              :else (try (if (binary-file? target)
                           (http/error-response 415
                                                :not-text "that file is not text"
                                                :path (.getPath target))
                           (let [anchor (or line 1)
                                 from (max 1 (- anchor (quot preview-line-limit 2)))
                                 {:keys [lines is-truncated]} (file-window target from)]

                             (http/json-response {:path (.getPath target)
                                                  :line anchor
                                                  :first-line from
                                                  :lines lines
                                                  :is-truncated is-truncated
                                                  :size-bytes (.length target)})))
                         (catch java.io.IOException _
                           (http/error-response 400
                                                :invalid-request "that file could not be read"
                                                :path (.getPath target)))))))))

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
   [:get "/v1/sessions/:sid/fs/file"] read-file-handler})
