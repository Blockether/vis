(ns com.blockether.vis.ext.channel-tui.attachment-intake
  "Adapters that turn terminal drops, clipboard images and picker selections into
   the composer's one structured attachment admission path. File paths are control
   input here; they never enter the user's prompt."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.composer-attachments :as composer]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.file-picker :as file-picker]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.paths :as paths])
  (:import [java.io File]
           [java.net URI]))

(set! *unchecked-math* :warn-on-boxed)

(defn fetch-gateway-capabilities!
  "Fetch the attachment contract through the canonical authenticated gateway
   client. Returns the string-keyed capabilities body on HTTP 200, otherwise nil."
  []
  (try (let [response (gateway-client/request! :get "/v1/capabilities" {:timeout-ms 5000})]
         (when (= 200 (:status response)) (wire/parse-json (:body response))))
       (catch Throwable _ nil)))

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

(defn picker-files
  "Filter file-picker rows to readable files whose sniffed type appears in the live
   gateway contract. Returns canonical paths in picker rank order."
  [capabilities workspace-root rows]
  (if-let [contract (composer/attachment-contract capabilities)]
    (into []
          (comp (map #(if (map? %) (:path %) %))
                (keep #(token-file workspace-root (str %)))
                (filter #(contains? (:media-types contract) (attachments/sniff-file-mime %)))
                (map #(.getCanonicalPath ^File %))
                (distinct))
          rows)
    []))

(defn workspace-picker-files
  "Gateway-supported files from the workspace's pooled fuzzy-file index."
  [capabilities workspace-root]
  (try (picker-files capabilities workspace-root (file-picker/fuzzy-file-rows "" {:limit 1000}))
       (catch Throwable _ [])))
