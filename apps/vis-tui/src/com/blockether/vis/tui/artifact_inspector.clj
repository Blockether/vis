(ns com.blockether.vis.tui.artifact-inspector
  "The TUI's session-wide attachment inspector. Composer files and durable model
   artifacts remain separate lifecycles, but share one discoverable C-x i surface."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.paths :as paths]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.format :as fmt])
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

(defn- produced-rows
  "Newest artifact per filename, preserving how many durable versions it represents."
  [artifacts]
  (let [newest
        (reverse (vec artifacts))

        counts
        (frequencies (map #(str (or (get % "filename") "artifact")) newest))]

    (:rows
      (reduce (fn [{:keys [seen rows] :as acc} artifact]
                (let [filename (str (or (get artifact "filename") "artifact"))]
                  (if (contains? seen filename)
                    acc
                    {:seen (conj seen filename)
                     :rows (conj rows
                                 {:source :produced
                                  :filename filename
                                  :media-type (str (or (get artifact "media_type")
                                                       "application/octet-stream"))
                                  :size (get artifact "size")
                                  :version (get artifact "version")
                                  :commentable (true? (get artifact "commentable"))
                                  :version-count (get counts filename 1)
                                  :iteration-id (str (get artifact "iteration_id"))
                                  :index (get artifact "index")
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
  "Pure attachment inspector. Text opens in the annotator; o keeps the external opener."
  [staged artifacts load-error]
  (let [rows
        (vec (sort-by #(if (= :staged (:source %)) 0 1) (inspector-rows staged artifacts)))

        display
        (display-rows rows load-error)

        total
        (long (count rows))]

    {:init {:selected 0 :scroll 0}
     :measure (fn [_ cols rows-count]
                (let [footer
                      (cond-> [["↑/↓" "move"] ["Enter" "read"] ["o" "external"]]
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
     :on-key
     (fn [{:keys [selected] :as state} ^KeyStroke key {:keys [list-h]}]
       (let [clamp-selected
             #(p/clamp % 0 (max 0 (dec (long total))))

             selected-row
             (nth rows selected nil)]

         (condp = (.getKeyType key)
           KeyType/Escape {::dlg/done nil}
           KeyType/ArrowUp (assoc state :selected (clamp-selected (dec (long selected))))
           KeyType/ArrowDown (assoc state :selected (clamp-selected (inc (long selected))))
           KeyType/PageUp (assoc state
                            :selected (dlg/page-selected-index display selected list-h -1 :source))
           KeyType/PageDown (assoc state
                              :selected (dlg/page-selected-index display selected list-h 1 :source))
           KeyType/Home (assoc state :selected 0)
           KeyType/End (assoc state :selected (max 0 (dec (long total))))
           KeyType/Enter
           {::dlg/done (when selected-row
                         {:action
                          (if (and (= :produced (:source selected-row))
                                   (or (= diff/media-type (:media-type selected-row))
                                       (#{"text/markdown" "text/plain"} (:media-type selected-row))
                                       (re-find #"(?i)\.(md|txt|log)$" (:filename selected-row))))
                            :annotate
                            :open)
                          :row selected-row})}
           KeyType/Character (if (and (= \o (.getCharacter key)) selected-row)
                               {::dlg/done {:action :open :row selected-row}}
                               state)
           KeyType/Delete {::dlg/done (when (= :staged (:source selected-row))
                                        {:action :remove :row selected-row})}
           KeyType/Backspace {::dlg/done (when (= :staged (:source selected-row))
                                           {:action :remove :row selected-row})}
           state)))}))

(defn show!
  "Open the inspector and return its selected action, or nil on close."
  [^TerminalScreen screen staged artifacts load-error]
  (dlg/run-modal! screen (inspector-modal-component staged artifacts load-error)))

(defn- image-row-label
  "One image viewer table row: the number, the file name, then its type and size."
  [i {:keys [name label]}]
  (str " " (inc (long i)) "  " name (when-not (str/blank? label) (str "  ·  " label))))

(defn image-viewer-key
  "The next image viewer state for `key`, or `{::dlg/done result}`. Arrows move, 1-9 jump,
   Enter or o opens the picture in the system viewer, Esc or q closes."
  [{:keys [index] :as state} ^KeyStroke key images]
  (let [last-index
        (max 0 (dec (count images)))

        move
        #(assoc state :index (p/clamp (long %) 0 last-index))

        open
        {::dlg/done {:action :open :image (nth images index nil) :index index}}]

    (condp = (.getKeyType key)
      KeyType/Escape {::dlg/done nil}
      KeyType/ArrowLeft (move (dec (long index)))
      KeyType/ArrowUp (move (dec (long index)))
      KeyType/ArrowRight (move (inc (long index)))
      KeyType/ArrowDown (move (inc (long index)))
      KeyType/Home (move 0)
      KeyType/End (move last-index)
      KeyType/Enter open
      KeyType/Character (let [ch (.getCharacter key)]
                          (cond (#{\o \O} ch) open
                                (#{\q \Q} ch) {::dlg/done nil}
                                (<= (int \1) (int ch) (int \9))
                                (let [at (- (int ch) (int \1))]
                                  (if (<= at last-index) (move at) state))
                                :else state))
      state)))

(defn image-viewer-component
  "The image viewer modal over `images` (`render/markdown-images` entries), opened at `index`.

   On a graphical terminal it shows one picture ENLARGED to fill the dialog, with its number,
   name, type and size above it. `paint-images!` receives the picture region after each
   refresh, because Lanterna does not model terminal pictures. On any other terminal it is a
   numbered table of the pictures. In both forms the arrows and 1-9 select a picture and
   Enter opens it in the system viewer."
  [images index {:keys [graphical? paint-images!]}]
  (let [images
        (vec images)

        n
        (count images)

        footer
        [["←/→" "picture"] ["1-9" "jump"] ["Enter" "open"] ["Esc" "close"]]]

    {:init {:index (p/clamp (long (or index 0)) 0 (max 0 (dec n))) :scroll 0}
     :measure (fn [_ cols rows]
                (let [content-w
                      (if graphical? (max 20 (- (long cols) 10)) (dlg/default-content-width cols))

                      content-h-req
                      (if graphical? (max 6 (- (long rows) 8)) (min 18 (max 3 (inc n))))

                      bounds
                      (dlg/dialog-bounds cols rows content-w content-h-req)

                      layout
                      (dlg/dialog-layout bounds)]

                  (merge layout
                         {:cols cols
                          :rows rows
                          :content-w content-w
                          :content-h-req content-h-req
                          :bounds bounds
                          :list-h (max 1 (long (:content-h layout)))})))
     :reconcile (fn [{:keys [index scroll] :as state} {:keys [list-h]}]
                  ;; The table header takes the first list row.
                  (let [rows-h (max 1 (dec (long list-h)))]
                    (assoc state
                      :scroll (-> (long scroll)
                                  (min (long index))
                                  (max (inc (- (long index) rows-h)))
                                  (max 0)))))
     :paint (fn [g {:keys [index scroll]}
                 {:keys [cols rows content-w content-h-req content-top content-h hint-row bounds
                         list-h]}]
              (let [{:keys [left inner-w]}
                    bounds

                    left
                    (long left)

                    inner-w
                    (long inner-w)

                    image
                    (nth images index nil)

                    title
                    (if graphical? (str "Image " (inc (long index)) " of " n) "Images")]

                (dlg/draw-dialog-chrome! g cols rows title content-w content-h-req)
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc left) content-top inner-w content-h)
                (if graphical?
                  (when image
                    (p/styled g
                              [p/BOLD]
                              (p/put-str! g
                                          (+ left 2)
                                          content-top
                                          (p/ellipsize (:name image) (max 1 (- inner-w 4)))))
                    (p/set-colors! g t/dialog-hint t/dialog-bg)
                    (let [label
                          (str (inc (long index))
                               " / "
                               n
                               (when-not (str/blank? (:label image)) (str "  ·  " (:label image))))

                          width
                          (long (p/display-width label))]

                      (when (< (+ 4 width (long (p/display-width (:name image)))) inner-w)
                        (p/put-str! g (- (+ left inner-w) width 1) content-top label)))
                    (when-not (and (:width image) (:height image))
                      (p/put-str! g
                                  (+ left 2)
                                  (+ (long content-top) 2)
                                  "No preview. Press Enter to open the picture.")))
                  (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                      (p/put-str! g
                                  (+ left 2)
                                  content-top
                                  (p/ellipsize "#  Image  ·  Type · Size" (max 1 (- inner-w 3))))
                      (dotimes [i (min (max 0 (dec (long list-h))) (- n (long scroll)))]
                        (let [at (+ (long scroll) (long i))]
                          (dlg/draw-selectable-row! g
                                                    left
                                                    (+ (long content-top) 1 (long i))
                                                    inner-w
                                                    (= at (long index))
                                                    (image-row-label at (nth images at)))))))
                (dlg/draw-hint-bar! g left hint-row inner-w footer)
                nil))
     :after-refresh (fn [{:keys [index]} {:keys [bounds content-top content-h]}]
                      (when (and graphical? paint-images!)
                        (let [{:keys [path mime width height]} (nth images index nil)]
                          (paint-images!
                            (if (and path width height)
                              (let [{:keys [left inner-w]} bounds
                                    room-w (max 1 (- (long inner-w) 4))
                                    room-h (max 1 (- (long content-h) 2))
                                    {:keys [cols rows]}
                                    (timg/cell-size {:w width :h height} room-w room-h)]

                                [{:row (+ (long content-top) 2 (quot (- room-h (long rows)) 2))
                                  :col (+ (long left) 1 (quot (- (long inner-w) (long cols)) 2))
                                  :img {:id (str "image-viewer/" path)
                                        :path path
                                        :mime mime
                                        :cols cols
                                        :rows rows
                                        :width width
                                        :height height}}])
                              [])))))
     :on-key (fn [state key _]
               (image-viewer-key state key images))}))

(defn show-images!
  "Open the image viewer and return `{:action :open :image :index}` or nil on close."
  [^TerminalScreen screen images index opts]
  (dlg/run-modal! screen (image-viewer-component images index opts)))

(defn- artifact-link-row!
  [session-id url]
  (when-let [[_ attachment-id]
             (re-matches
               #"(?i)attachment://([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})"
               url)]
    (when-let [artifact (some #(when (= (str/lower-case attachment-id) (get % "attachment_id")) %)
                              (:artifacts (fetch-session-artifacts! session-id)))]
      {:filename (get artifact "filename")
       :media-type (get artifact "media_type")
       :iteration-id (get artifact "iteration_id")
       :index (get artifact "index")})))

(defn artifact-row!
  "The inspector row that `target` names: the row itself, or the row of an
   attachment://<uuid> link in this session's human-visible index. nil when the
   link names no artifact."
  [session-id target]
  (if (string? target) (artifact-link-row! session-id target) target))

(defn saved-artifacts-dir
  "The folder that keeps artifacts a person saves from the TUI:
   `<downloads>/Vis Artifacts`, the same folder the desktop app uses."
  ^File []
  (File. (paths/downloads-dir) "Vis Artifacts"))

(defn materialize-artifact!
  "Fetch one produced artifact's durable bytes and write them under its original
   basename in a new unique directory. Returns the local File or nil.
   `target` is an inspector row or an attachment://<uuid> link. Links resolve the
   exact version in this session's human-visible index, never a filesystem path.
   Without `parent`, the directory is temporary; with it, the file stays there."
  ([session-id target] (materialize-artifact! session-id target nil))
  ([session-id target ^File parent]
   (let [{:keys [filename iteration-id index]} (artifact-row! session-id target)]
     (when (and (not (str/blank? (str session-id)))
                (not (str/blank? (str iteration-id)))
                (number? index))
       (when-let [^bytes bytes
                  (vis/gateway-iteration-attachment-bytes session-id iteration-id index)]
         (let [dir (doto (if parent
                           (File. parent (str (random-uuid)))
                           (File. (System/getProperty "java.io.tmpdir")
                                  (str "vis-artifact-" (random-uuid))))
                     (.mkdirs))
               basename (let [name (.getName (File. (str filename)))]
                          (if (str/blank? name) "artifact.bin" name))
               target (File. dir basename)]

           (with-open [out (FileOutputStream. target)]
             (.write out bytes))
           target))))))
