(ns com.blockether.vis.tui.session-export
  "The export of one session to a file: the formats, the file name and the write.
   Also the file that an import reads.

   The gateway renders each format, so the TUI, the Companion and the CLI give the
   same file. The Companion names the file with the same rule."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.paths :as paths])
  (:import [java.io File]
           [java.nio.file CopyOption Files OpenOption StandardCopyOption]))

(def formats
  "The export formats in menu order. `:key` chooses the format in the menu, and
   `:ext` is the file extension."
  [{:id :html :label "HTML" :ext "html" :key \h :description "One page that opens in a browser"}
   {:id :md :label "Markdown" :ext "md" :key \m :description "The text of each turn"}
   {:id :atif
    :label "ATIF (.json)"
    :ext "json"
    :key \a
    :description "A trajectory that Vis and Harbor read"}
   {:id :snapshot
    :label "Snapshot"
    :ext "vissnap"
    :key \s
    :description "A file that Vis imports as a new session"}])

(def ^:const ^:private slug-limit "The most characters of the title part of a file name." 48)

(defn file-name
  "The file name of an export: the slug of `title`, the first 8 characters of `sid`
   and `ext`. The slug is the lower-case title with each run of other characters than
   a-z and 0-9 as one dash, at most `slug-limit` characters. A title without such
   characters gives `vis-session`."
  [title sid ext]
  (let [slug
        (-> (str/lower-case (str title))
            (str/replace #"[^a-z0-9]+" "-")
            (str/replace #"^-+|-+$" ""))

        cut
        (str/replace (subs slug 0 (min (count slug) (long slug-limit))) #"-+$" "")

        id
        (str sid)]

    (str (if (str/blank? cut) "vis-session" cut) "-" (subs id 0 (min 8 (count id))) "." ext)))

(defn write!
  "Write `data` as the file `name` in the folder `dir`, and answer the file. A second
   export of the same session and format replaces the first file. The bytes go to a
   temporary file beside it first, so a stopped write leaves no partial export."
  ^File [^File dir name ^bytes data]
  (.mkdirs dir)
  (let [target
        (File. dir (str name))

        tmp
        (.toPath (File. dir (str "." name ".tmp")))]

    (try (Files/write tmp data ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
         (Files/move tmp
                     (.toPath target)
                     (into-array CopyOption
                                 [StandardCopyOption/REPLACE_EXISTING
                                  StandardCopyOption/ATOMIC_MOVE]))
         target
         (finally (Files/deleteIfExists tmp)))))

(defn import-file
  "The readable file at the typed `path`, or nil. A leading `~` is the home folder,
   and quotes around the path are dropped, so a path pasted from a shell works."
  ^File [path]
  (let [typed
        (str/replace (str/trim (str path)) #"^(['\"])(.*)\1$" "$2")

        file
        (File. ^String (paths/expand-home typed))]

    (when (and (seq typed) (.isFile file) (.canRead file)) file)))
