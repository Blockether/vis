(ns com.blockether.vis.internal.attachment.linked-reports
  "Snapshot deliberately linked workspace reports before persisting assistant prose.

   Only the session's primary workspace is eligible, never other host roots. Hidden
   paths, credential-like names, policy exclusions, traversal, symlinks, directories
   and unsupported report types fail closed. Reads walk directory handles without
   following symlinks; platforms without SecureDirectoryStream refuse publication.
   At most eight distinct links and 8 MiB per file are delivered per iteration.
   Attachments are human-only snapshots, not live paths. Existing attachment and
   remote links, code examples and images are not republished. Failed local links
   become plain text with a recovery action; no host exception or file bytes leak."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.channels Channels]
           [java.nio.file Files LinkOption Path SecureDirectoryStream StandardOpenOption]
           [java.nio.file.attribute BasicFileAttributeView FileAttribute]
           [java.util Base64]
           [org.commonmark.node Code Link Node SoftLineBreak SourceSpan Text]
           [org.commonmark.parser IncludeSourceSpans Parser]))

(def ^:private report-types
  {"md" "text/markdown"
   "markdown" "text/markdown"
   "txt" "text/plain"
   "csv" "text/csv"
   "tsv" "text/tab-separated-values"
   "pdf" "application/pdf"
   "html" "text/html"
   "htm" "text/html"
   "svg" "image/svg+xml"
   "png" "image/png"
   "jpg" "image/jpeg"
   "jpeg" "image/jpeg"
   "webp" "image/webp"})

(def ^:private byte-limit (* 8 1024 1024))

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private parser
  (.build (.includeSourceSpans (Parser/builder) IncludeSourceSpans/BLOCKS_AND_INLINES)))

(defn- children [^Node node] (take-while some? (iterate #(.getNext ^Node %) (.getFirstChild node))))

(defn- nodes [^Node node] (tree-seq #(seq (children %)) children node))

(defn- refuse! [] (throw (ex-info "File delivery refused" {})))

(defn- read-relative
  ^bytes [^SecureDirectoryStream directory parts]
  (let [^Path part (Path/of ^String (first parts) (make-array String 0))]
    (if (next parts)
      (with-open [child (.newDirectoryStream directory part no-follow)]
        (read-relative child (next parts)))
      (let [view (.getFileAttributeView directory part BasicFileAttributeView no-follow)]
        (when-not (.isRegularFile (.readAttributes ^BasicFileAttributeView view)) (refuse!))
        (with-open [channel (.newByteChannel directory
                                             part
                                             #{StandardOpenOption/READ LinkOption/NOFOLLOW_LINKS}
                                             (make-array FileAttribute 0))
                    input (Channels/newInputStream channel)]

          (let [bytes (.readNBytes input (int (inc (long byte-limit))))]
            (when (> (alength bytes) (long byte-limit)) (refuse!))
            bytes))))))

(defn- snapshot
  [environment destination]
  (when-not (and (:workspace/root environment) (:security-policy environment)) (refuse!))
  (let [workspace
        (.normalize (.toAbsolutePath (.toPath (io/file (:workspace/root environment)))))

        ;; Decode once, preserving literal plus signs in filenames.
        decoded
        (java.net.URLDecoder/decode (str/replace destination "+" "%2B") "UTF-8")

        file-path
        (str/replace decoded #"[?#].*$" "")

        requested
        (Path/of file-path (make-array String 0))

        _
        (when (some #(= ".." (str %)) requested) (refuse!))

        target
        (.normalize (.resolve workspace requested))

        parts
        (mapv str (.relativize workspace target))

        filename
        (last parts)

        media-type
        (get report-types (str/lower-case (last (str/split (or filename "") #"\."))))

        exclusions
        (concat (get-in environment [:security-policy :process-jail :deny-read])
                (get-in environment [:security-policy :process-jail :no-search]))]

    (when (or (not (.startsWith target workspace))
              (empty? parts)
              (nil? media-type)
              (some #(or (str/starts-with? % ".")
                         (re-find #"(?i)(credential|password|secret|token|private[-_]?key)" %))
                    parts)
              (some #(.startsWith target (.toPath (io/file %))) exclusions))
      (refuse!))
    (with-open [directory (Files/newDirectoryStream (.getRoot workspace))]
      (when-not (instance? SecureDirectoryStream directory) (refuse!))
      (let [bytes (read-relative directory (mapv str target))]
        {:id (random-uuid)
         :filename filename
         :media-type media-type
         :kind "file"
         :audience "user"
         :size (alength bytes)
         :base64 (.encodeToString (Base64/getEncoder) bytes)}))))

(defn- local-link?
  [destination]
  (and (not (str/blank? destination))
       (not (str/starts-with? destination "#"))
       (not (str/starts-with? destination "//"))
       (not (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*:" destination))))

(defn- rewrite
  [environment cache markdown]
  (if-not (string? markdown)
    markdown
    (let
      [edits
       (keep
         (fn [^Node node]
           (when (and (instance? Link node) (local-link? (.getDestination ^Link node)))
             (let [destination (.getDestination ^Link node)
                   cached (find @cache destination)
                   attachment (if cached
                                (val cached)
                                (let [attachment (when (< (count @cache) 8)
                                                   (try (snapshot environment destination)
                                                        (catch Exception _ nil)))]
                                  (swap! cache assoc destination attachment)
                                  attachment))
                   spans (.getSourceSpans node)
                   ^SourceSpan first-span (first spans)
                   ^SourceSpan last-span (last spans)
                   label (-> (apply str
                                    (keep #(cond (instance? Text %) (.getLiteral ^Text %)
                                                 (instance? Code %) (.getLiteral ^Code %)
                                                 (instance? SoftLineBreak %) " ")
                                          (nodes node)))
                             (str/replace #"([\\\[\]])" "\\$1"))]

               (when (and first-span last-span)
                 {:start (.getInputIndex first-span)
                  :end (+ (.getInputIndex last-span) (.getLength last-span))
                  :text
                  (if attachment
                    (str "[" label "](attachment://" (:id attachment) ")")
                    (str
                      label
                      " (local file unavailable or not eligible for delivery; "
                      "check the workspace path and access, then explicitly attach the file)."))}))))
         (nodes (.parse ^Parser parser markdown)))]
      (reduce (fn [s {:keys [start end text]}]
                (str (subs s 0 start) text (subs s end)))
              markdown
              (sort-by :start > edits)))))

(defn deliver-iteration
  "Rewrite only assistant prose and final answer prose; return snapshot rows beside
   the iteration for the existing transactional attachment writer. Never read from
   transcript GETs or accept paths from a device. No persistence means no delivery."
  [environment iteration]
  (if-not (:db-info environment)
    iteration
    (let [cache
          (atom {})

          rewrite-prose
          #(rewrite environment cache %)

          answer
          (get-in iteration [:final-result :answer])

          delivered
          (cond (vector? answer)
                (mapv #(if (= "prose" (get % "type")) (update % "markdown" rewrite-prose) %) answer)
                (and (map? answer) (string? (:answer answer))) (update answer :answer rewrite-prose)
                :else answer)

          result
          (cond-> iteration
            (:assistant-prose iteration)
            (update :assistant-prose rewrite-prose)

            (:final-result iteration)
            (assoc-in [:final-result :answer] delivered))]

      (assoc result :linked-report-attachments (vec (keep val @cache))))))
