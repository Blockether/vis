(ns com.blockether.vis.tui.attachment-intake
  "Adapters that turn terminal drops, clipboard images and picker selections into
   the composer's one structured attachment admission path. File paths are control
   input here; they never enter the user's prompt."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.composer-attachments :as composer]
            [com.blockether.vis.tui.attachments :as attachments]
            [com.blockether.vis.tui.file-picker :as file-picker]
            [com.blockether.vis.tui.format :as fmt]
            [com.blockether.vis.tui.paths :as paths])
  (:import [java.io File]
           [java.net URI]))

(set! *unchecked-math* :warn-on-boxed)

(defn- finish-token [tokens token] (if (seq token) (conj tokens (apply str token)) tokens))

(defn- drop-tokens
  "Shell-like tokenization for terminal file-drop payloads. Quotes group spaces and
   backslash escapes one following character; unmatched quotes/escapes are refused."
  [text]
  (loop [chars
         (seq (str text))

         quote-char
         nil

         escaped?
         false

         token
         []

         tokens
         []]

    (if-let [c (first chars)]
      (cond escaped? (recur (next chars) quote-char false (conj token c) tokens)
            (= c \\) (recur (next chars) quote-char true token tokens)
            quote-char (if (= c quote-char)
                         (recur (next chars) nil false token tokens)
                         (recur (next chars) quote-char false (conj token c) tokens))
            (or (= c \') (= c \")) (recur (next chars) c false token tokens)
            (Character/isWhitespace ^char c)
            (recur (next chars) nil false [] (finish-token tokens token))
            :else (recur (next chars) nil false (conj token c) tokens))
      (when-not (or quote-char escaped?) (finish-token tokens token)))))

(defn- token-file
  [workspace-root token]
  (try (let [token
             (if (str/starts-with? token "file://") (.getPath (URI. token)) token)

             expanded
             (paths/expand-home token)

             f
             (File. expanded)

             f
             (if (.isAbsolute f)
               f
               (File. (str (or workspace-root (System/getProperty "user.dir"))) expanded))]

         (when (and (.isFile f) (.canRead f)) (.getCanonicalFile f)))
       (catch Throwable _ nil)))

(defn dropped-files
  "Resolve a whole terminal drop payload to readable files. Every token must be a
   file, which keeps ordinary pasted prose on the text-paste path."
  [text workspace-root]
  (when-let [tokens (seq (drop-tokens text))]
    (let [files (mapv #(token-file workspace-root %) tokens)]
      (when (every? some? files) files))))

(defn- intake-files
  [capabilities current source files]
  (merge {:handled? true :source source} (composer/admit-files capabilities current files)))

(defn file-drop
  "Adapt bracketed-paste text when it consists wholly of dropped files. Returns
   `{:handled? false}` for ordinary text, otherwise the shared admission result."
  [capabilities current text workspace-root]
  (if-let [files (dropped-files text workspace-root)]
    (intake-files capabilities current :drop files)
    {:handled? false :source :drop}))

(defn clipboard-image
  "Adapt `input/read-clipboard-image!` output to shared admission. nil is harmless."
  [capabilities current clipboard]
  (if-let [path (:path clipboard)]
    (intake-files capabilities current :clipboard [path])
    {:handled? false :source :clipboard}))

(defn picker-selection
  "Adapt a multi-file picker selection to shared admission. nil means cancellation
   and leaves composer state untouched."
  [capabilities current selected-paths]
  (if (nil? selected-paths)
    {:handled? false :source :picker}
    (intake-files capabilities current :picker selected-paths)))

(defn- type-label
  "Short file type for a picker row: the extension in capitals, else the media subtype."
  [media-type ^File f]
  (or (some-> (re-find #"\.([A-Za-z0-9]{1,8})$" (.getName f))
              second
              str/upper-case)
      (some-> (second (str/split (str media-type) #"/"))
              str/upper-case)
      "FILE"))

(defn- label-base
  "The canonical `root` with a trailing separator, the prefix that `row-label` cuts."
  [root]
  (try (when-let [^String dir (not-empty (str root))]
         (str (.getCanonicalPath (File. dir)) File/separator))
       (catch Throwable _ nil)))

(defn- row-label
  "The picker label of `f`: relative to `base` inside it, else the path with `~`."
  [base ^File f]
  (let [path (.getCanonicalPath f)]
    (if (and base (str/starts-with? path base))
      (subs path (count base))
      (paths/abbreviate-home path))))

(defn- supported-item
  "The picker row of readable file `f`, or nil when the gateway contract does not
   take its sniffed media type."
  [contract base ^File f]
  (let [media-type (attachments/sniff-file-mime f)]
    (when (contains? (:media-types contract) media-type)
      {:id (.getCanonicalPath f)
       :label (row-label base f)
       :hint (str (type-label media-type f) " · " (fmt/format-bytes (.length f)))})))

(defn picker-items
  "Picker rows of the file-picker `rows` that are readable files of a type in the
   live gateway contract, in rank order. A row is `{:id path :label :hint}`: `:id`
   is the canonical path, `:label` the path relative to the workspace and `:hint`
   the type and size."
  [capabilities workspace-root rows]
  (if-let [contract (composer/attachment-contract capabilities)]
    (into []
          (comp (map #(if (map? %) (:path %) %))
                (keep #(token-file workspace-root (str %)))
                (keep (let [base (label-base workspace-root)]
                        #(supported-item contract base %)))
                (distinct))
          rows)
    []))

(defn workspace-picker-items
  "Picker rows for gateway-supported files from the workspace's fuzzy-file index."
  [capabilities workspace-root]
  (try (picker-items capabilities workspace-root (file-picker/fuzzy-file-rows "" {:limit 1000}))
       (catch Throwable _ [])))

(defn typed-path-items
  "The picker row of the file that `query` names as a path, or []. A path starts
   with `/`, `~`, `./`, `../` or `file://`, so it can name a file outside the
   workspace. Ordinary search text gives []."
  [capabilities workspace-root query]
  (let [q
        (str/trim (str query))

        contract
        (composer/attachment-contract capabilities)

        f
        (when (and contract (re-find #"^(/|~|\./|\.\./|file://)" q)) (token-file workspace-root q))]

    (if-let [item (when f (supported-item contract (label-base workspace-root) f))]
      [item]
      [])))
