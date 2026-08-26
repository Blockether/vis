(ns com.blockether.vis.ext.channel-tui.artifact-inspector
  "The TUI's session-wide attachment inspector. Composer files and durable model
   artifacts remain separate lifecycles, but share one discoverable C-x i surface."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.format :as fmt])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [java.io File FileOutputStream]))

(set! *unchecked-math* :warn-on-boxed)

(defn fetch-session-artifacts!
  "Whole-session artifact index through the facade. Returns `{:artifacts [...]}`,
   or `{:artifacts [] :error string}` when the daemon cannot answer."
  [session-id]
  (if (str/blank? (str session-id))
    {:artifacts []}
    (if-let [artifacts (vis/gateway-session-artifacts session-id)]
      {:artifacts artifacts}
      {:artifacts [] :error "Artifact index unavailable"})))

(defn- field [m k] (or (get m k) (get m (keyword k))))

(defn- produced-rows
  "Newest artifact per filename, preserving how many durable versions it represents."
  [artifacts]
  (let [newest
        (reverse (vec artifacts))

        counts
        (frequencies (map #(str (or (field % "filename") "artifact")) newest))]

    (:rows
      (reduce (fn [{:keys [seen rows] :as acc} artifact]
                (let [filename (str (or (field artifact "filename") "artifact"))]
                  (if (contains? seen filename)
                    acc
                    {:seen (conj seen filename)
                     :rows (conj rows
                                 {:source :produced
                                  :filename filename
                                  :media-type (str (or (field artifact "media_type")
                                                       "application/octet-stream"))
                                  :size (field artifact "size")
                                  :version (field artifact "version")
                                  :version-count (get counts filename 1)
                                  :iteration-id (str (field artifact "iteration_id"))
                                  :index (field artifact "index")
                                  :artifact artifact})})))
              {:seen #{} :rows []}
              newest))))

(defn inspector-rows
  "Normalize staged composer files and produced artifacts into selectable rows.
   Staged rows come first; otherwise the newest produced artifact is selected."
  [staged artifacts]
  (into (mapv (fn [attachment]
                {:source :staged
                 :filename (str (or (:filename attachment) "attachment"))
                 :media-type (str (or (:media-type attachment) "application/octet-stream"))
                 :size (:size attachment)
                 :attachment attachment})
              staged)
        (produced-rows artifacts)))

(defn- size-label [size] (when (number? size) (fmt/format-bytes size)))

(defn- row-label
  [{:keys [source filename media-type size version version-count]}]
  (str filename
       (when (= source :produced)
         (str (when version (str "  v" version))
              (when (> (long version-count) 1) (str "  ·  " version-count " versions"))))
       "  ·  "
       media-type
       (when-let [label (size-label size)]
         (str "  ·  " label))))

(defn- display-rows
  [rows load-error]
  (let [staged
        (filterv #(= :staged (:source %)) rows)

        produced
        (filterv #(= :produced (:source %)) rows)]

    (cond-> []
      (seq staged)
      (into (cons {:header "Ready to send"} staged))

      (seq produced)
      (into (cons {:header "Produced in this session"} produced))

      (and (empty? rows) load-error)
      (conj {:empty load-error})

      (and (empty? rows) (nil? load-error))
      (conj {:empty "No attachments in this session"}))))

(defn- selected-display-index
  ^long [display selected]
  (loop [idx
         0

         selectable
         -1]

    (if-let [row (nth display idx nil)]
      (let [selectable (if (:source row) (inc selectable) selectable)]
        (if (= selectable selected) idx (recur (inc idx) selectable)))
      0)))

(defn inspector-modal-component
  "Pure modal component for the unified attachment inspector. Enter returns an
   `:open` action; Delete/Backspace returns `:remove` only for staged files."
  [staged artifacts load-error]
  (let [rows
        (inspector-rows staged artifacts)

        display
        (display-rows rows load-error)

        total
        (long (count rows))]

    {:init {:selected 0 :scroll 0}
     :measure (fn [_ cols rows-count]
                (let [footer
                      (cond-> [["↑/↓" "move"] ["Enter" "open"]]
                        (seq staged)
                        (conj ["Del" "remove"])

                        true
                        (conj ["Esc" "close"]))

                      content-w
                      (dlg/default-content-width cols)

                      content-h-req
                      (min 18 (max 3 (inc (count display))))

                      bounds
                      (dlg/dialog-bounds cols rows-count content-w content-h-req)

                      {:keys [content-top content-h hint-row]}
                      (dlg/dialog-layout bounds)

                      list-h
                      (max 1 (long content-h))]

                  {:cols cols
                   :rows rows-count
                   :footer footer
                   :content-w content-w
                   :content-h-req content-h-req
                   :bounds bounds
                   :content-top content-top
                   :content-h content-h
                   :hint-row hint-row
                   :list-h list-h}))
     :reconcile (fn [{:keys [selected scroll] :as state} {:keys [list-h]}]
                  (let [selected
                        (p/clamp (long selected) 0 (max 0 (dec (long total))))

                        display-index
                        (long (selected-display-index display selected))

                        max-scroll
                        (max 0 (- (long (count display)) (long list-h)))

                        scroll
                        (-> (long scroll)
                            (min display-index)
                            (max (inc (- display-index (long list-h))))
                            (p/clamp 0 max-scroll))]

                    (assoc state
                      :selected selected
                      :scroll scroll)))
     :paint (fn [g {:keys [selected scroll]}
                 {:keys [cols rows footer content-w content-h-req bounds content-top content-h
                         hint-row list-h]}]
              (let [{:keys [left inner-w]}
                    bounds

                    selected-display
                    (selected-display-index display selected)]

                (dlg/draw-dialog-chrome! g cols rows "Attachments" content-w content-h-req)
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
                (dotimes [i (min (long list-h) (- (count display) (long scroll)))]
                  (let [display-index (+ (long scroll) (long i))
                        item (nth display display-index)
                        row (+ (long content-top) (long i))]

                    (cond (:header item) (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                                             (p/put-str! g (+ (long left) 2) row (:header item)))
                          (:empty item) (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                                            (p/put-str! g
                                                        (+ (long left) 2)
                                                        row
                                                        (p/ellipsize (:empty item)
                                                                     (max 1 (- (long inner-w) 3)))))
                          :else (dlg/draw-selectable-row! g
                                                          left
                                                          row
                                                          inner-w
                                                          (= display-index selected-display)
                                                          (row-label item)))))
                (dlg/draw-hint-bar! g left hint-row inner-w footer)
                nil))
     :on-key (fn [{:keys [selected] :as state} ^KeyStroke key _]
               (let [clamp-selected
                     #(p/clamp % 0 (max 0 (dec (long total))))

                     selected-row
                     (nth rows selected nil)]

                 (condp = (.getKeyType key)
                   KeyType/Escape {::dlg/done nil}
                   KeyType/ArrowUp (assoc state :selected (clamp-selected (dec (long selected))))
                   KeyType/ArrowDown (assoc state :selected (clamp-selected (inc (long selected))))
                   KeyType/Home (assoc state :selected 0)
                   KeyType/End (assoc state :selected (max 0 (dec (long total))))
                   KeyType/Enter {::dlg/done (when selected-row {:action :open :row selected-row})}
                   KeyType/Delete {::dlg/done (when (= :staged (:source selected-row))
                                                {:action :remove :row selected-row})}
                   KeyType/Backspace {::dlg/done (when (= :staged (:source selected-row))
                                                   {:action :remove :row selected-row})}
                   state)))}))

(defn show!
  "Open the inspector and return its selected action, or nil on close."
  [^TerminalScreen screen staged artifacts load-error]
  (dlg/run-modal! screen (inspector-modal-component staged artifacts load-error)))

(defn materialize-artifact!
  "Fetch one produced artifact's durable bytes and write them under its original
   basename in a unique temporary directory. Returns the local File or nil."
  [session-id {:keys [filename iteration-id index]}]
  (when (and (not (str/blank? (str session-id)))
             (not (str/blank? (str iteration-id)))
             (number? index))
    (when-let [^bytes bytes (vis/gateway-iteration-attachment-bytes session-id iteration-id index)]
      (let [dir (doto (File. (System/getProperty "java.io.tmpdir")
                             (str "vis-artifact-" (random-uuid)))
                  (.mkdirs))
            basename (let [name (.getName (File. (str filename)))]
                       (if (str/blank? name) "artifact.bin" name))
            target (File. dir basename)]

        (with-open [out (FileOutputStream. target)]
          (.write out bytes))
        target))))
