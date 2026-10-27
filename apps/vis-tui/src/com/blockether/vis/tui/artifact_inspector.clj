(ns com.blockether.vis.tui.artifact-inspector
  "The TUI's session-wide attachment inspector. Composer files and durable model
   artifacts remain separate lifecycles, but share one discoverable C-x i surface."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.app-model :as app-model]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.paths :as paths]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.format :as fmt]
            [taoensso.telemere :as tel])
  (:import [com.googlecode.lanterna.gui2 ScrollBar]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [java.io File FileOutputStream]))

(set! *unchecked-math* :warn-on-boxed)

(defn- session-apps
  "The app views of `session-id` in wire shape, the active ones and then the
   archived ones, or nil when the gateway cannot answer."
  [session-id]
  (try (into (vis/gateway-app-views session-id false) (vis/gateway-app-views session-id true))
       (catch Throwable e
         (tel/log!
           {:level :warn :id ::app-statuses-failed :error e :data {:session-id (str session-id)}})
         nil)))

(defn- wire-app-id
  [wire]
  (some-> (get wire "view_id" (get wire "id"))
          str
          str/trim
          not-empty))

(defn- wire-view-id
  "The app view id of artifact index entry `artifact`, which has wire keys, or nil."
  [artifact]
  (when (= app-model/media-type (get artifact "media_type"))
    (some-> (get artifact "view_id")
            str
            str/trim
            not-empty)))

(defn- listed-app
  "An artifact index entry for app `wire` that no block filed: an app that a slash
   command or a program outside a turn opened. Its row opens the app."
  [wire]
  {"media_type" app-model/media-type
   "view_id" (wire-app-id wire)
   "title" (get wire "title")
   "filename" (str (or (not-empty (str (get wire "key"))) (wire-app-id wire)) ".app.json")})

(defn fetch-session-artifacts!
  "Whole-session artifact index through the facade. Returns `{:artifacts [...]}`,
   or `{:artifacts [] :error string}` when the daemon cannot answer. An app row
   also holds its lifecycle state in \"app_status\". Every app of the session has a
   row, also an app that no block filed."
  [session-id]
  (if (str/blank? (str session-id))
    {:artifacts []}
    (if-let [artifacts (vis/gateway-session-artifacts session-id)]
      (let [apps (session-apps session-id)
            statuses (into {}
                           (keep #(when-let [id (wire-app-id %)] [id
                                                                  (app-model/status
                                                                    (vis/wire->engine %))]))
                           apps)
            filed (set (keep wire-view-id artifacts))
            all
            (into (vec artifacts) (comp (remove #(filed (wire-app-id %))) (map listed-app)) apps)]

        {:artifacts (mapv #(if-let [status (get statuses (wire-view-id %))] (assoc %
                                                                              "app_status"
                                                                              (name status)) %)
                          all)})
      {:artifacts [] :error "Artifact index unavailable"})))

(defn- produced-rows
  "Newest artifact per filename, preserving how many durable versions it represents."
  [artifacts]
  (let [newest
        (reverse (vec artifacts))

        identity-of
        (fn [artifact]
          (or (some->> (wire-view-id artifact)
                       (str "app:"))
              (str (or (get artifact "filename") "artifact"))))

        counts
        (frequencies (map identity-of newest))]

    (:rows
      (reduce (fn [{:keys [seen rows] :as acc} artifact]
                (let [filename
                      (str (or (get artifact "filename") "artifact"))

                      key
                      (identity-of artifact)]

                  (if (contains? seen key)
                    acc
                    {:seen (conj seen key)
                     :rows (conj rows
                                 {:source :produced
                                  :filename filename
                                  :media-type (str (or (get artifact "media_type")
                                                       "application/octet-stream"))
                                  :size (get artifact "size")
                                  :version (get artifact "version")
                                  :version-count (get counts key 1)
                                  :iteration-id (str (get artifact "iteration_id"))
                                  :view-id (wire-view-id artifact)
                                  :title (get artifact "title")
                                  :app-status (some-> (get artifact "app_status")
                                                      keyword)
                                  :index (get artifact "index")
                                  :artifact artifact})})))
              {:seen #{} :rows []}
              newest))))

(defn inspector-rows
  "Normalize staged composer files, the pictures sent in the session's messages and
   produced artifacts into selectable rows, in that order. Each `sent` picture is an
   image viewer entry `{:path :mime :name :label}`."
  [staged sent artifacts]
  (-> (mapv (fn [attachment]
              {:source :staged
               :filename (str (or (:filename attachment) "attachment"))
               :media-type (str (or (:media-type attachment) "application/octet-stream"))
               :size (:size attachment)
               :attachment attachment})
            staged)
      (into (map-indexed (fn [i image]
                           {:source :sent
                            :filename (str (or (:name image) "picture"))
                            :media-type (str (or (:mime image) "image/png"))
                            :label (:label image)
                            :index i
                            :image image}))
            sent)
      (into (produced-rows artifacts))))

(defn- size-label [size] (when (number? size) (fmt/format-bytes size)))

(def ^:private thumb-cols "Thumbnail column width of the attachment table." 12)

(def ^:private block-h "Rows of one attachment in the table: its thumbnail and its text." 3)

(defn- image-row?
  [{:keys [media-type] :as row}]
  (and (not (app-model/app-row? row)) (str/starts-with? (str media-type) "image/")))

(defn- kind-badge
  "The short type name that the thumbnail cell shows for a row without a picture."
  [{:keys [filename media-type] :as row}]
  (if (app-model/app-row? row)
    "App"
    (or (some-> (or (re-find #"\.([A-Za-z0-9]{1,5})$" (str filename))
                    (re-find #"/([A-Za-z0-9]{1,5})$" (str media-type)))
                second
                str/upper-case)
        "File")))

(defn- row-lines
  "The text lines of one attachment row: its name, then its details."
  [{:keys [source filename size version version-count title app-status label] :as row}]
  (if (app-model/app-row? row)
    [(or (not-empty (str title)) filename)
     (str "App" (when app-status (str " · " (get app-model/status-label app-status))))]
    (if (= :sent source)
      [filename (not-empty (str label))]
      [filename
       (str/join " · "
                 (remove nil?
                   [(kind-badge row) (size-label size)
                    (when (and (= source :produced) version) (str "v" version))
                    (when (and (= source :produced) version-count (> (long version-count) 1))
                      (str version-count " versions"))]))])))

(defn- table-lines
  "The painted lines of the attachment table. Each section has a heading and a ruled
   table. An attachment owns [[block-h]] lines `{:block index :line n}`, where `index`
   is its place in `rows`."
  [rows load-error]
  (let [section (fn [heading source]
                  (when-let [items (seq (keep-indexed #(when (= source (:source %2)) %1) rows))]
                    (-> [{:header heading} {:rule "┬"}]
                        (into (comp (map (fn [i]
                                           (mapv #(hash-map :block i :line %) (range block-h))))
                                    (interpose [{:rule "┼"}])
                                    cat)
                              items)
                        (conj {:rule "┴"}))))]
    (cond-> (into []
                  cat
                  [(section "Ready to send" :staged) (section "Sent in this session" :sent)
                   (section "Produced in this session" :produced)])
      (and (empty? rows) load-error)
      (conj {:empty load-error})

      (and (empty? rows) (nil? load-error))
      (conj {:empty "No attachments in this session"}))))

(defn- block-span
  "The first and last line of attachment `selected` in `lines`, with the heading and
   rules around it, so that a scroll shows the whole table row."
  [lines selected]
  (let [start
        (long (or (first (keep-indexed
                           #(when (and (= selected (:block %2)) (zero? (long (:line %2)))) %1)
                           lines))
                  0))

        first-line
        (loop [k start]
          (if (and (pos? k) (not (:block (nth lines (dec k))))) (recur (dec k)) k))

        last-line
        (min (dec (count lines)) (+ start (long block-h)))]

    [first-line last-line]))

(defn- thumbnail-region
  "The terminal picture of attachment `row` at screen `row-top`, or nil without one. Every
   picture fills the whole thumbnail cell and is cut to its shape, so all thumbnails have
   one size."
  [thumbnail row left row-top]
  (let [{:keys [path mime width height] :as picture} (when thumbnail (thumbnail row))]
    (when (and (map? picture) path width height)
      {:row (long row-top)
       :col (+ (long left) 2)
       :img {:id (str "attachment-table/" path)
             :path path
             :mime mime
             :cols thumb-cols
             :rows block-h
             :width width
             :height height
             :view (timg/cover-view width height thumb-cols block-h (timg/cell-pixels))}})))

(defn- paint-table-line!
  "Paint one line of the attachment table."
  [g item rows thumbnail selected {:keys [left inner-w]} y]
  (let [left
        (long left)

        inner-w
        (long inner-w)

        rule-x
        (+ left 2 (long thumb-cols) 1)

        text-x
        (+ rule-x 2)

        text-w
        (max 1 (- (+ left inner-w) text-x 1))]

    (cond (:header item) (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                             (p/styled g [p/BOLD] (p/put-str! g (+ left 2) y (:header item))))
          (:empty item)
          (do (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g (+ left 2) y (p/ellipsize (:empty item) (max 1 (- inner-w 3)))))
          (:rule item) (do (p/set-colors! g t/turn-separator-fg t/dialog-bg)
                           (p/put-str! g
                                       (+ left 2)
                                       y
                                       (p/joined-horiz-line [(- rule-x left 2)
                                                             (- (+ left inner-w) rule-x 2)]
                                                            (first (:rule item)))))
          :else
          (let [i
                (long (:block item))

                line
                (long (:line item))

                row
                (nth rows i)

                selected?
                (= i (long selected))

                text
                (nth (row-lines row) line nil)

                picture
                (when thumbnail (thumbnail row))]

            (when (and (= 1 line) (not (map? picture)))
              (let [badge (p/ellipsize (if (= :loading picture) "…" (kind-badge row)) thumb-cols)]
                (p/set-colors! g t/dialog-hint t/dialog-bg)
                (p/put-str! g
                            (+ left 2 (quot (- (long thumb-cols) (long (p/display-width badge))) 2))
                            y
                            badge)))
            ;; The selection marks the text side, so the thumbnail keeps its own background.
            (p/set-colors! g t/dialog-fg t/dialog-bg)
            (p/styled g
                      (p/selection-styles selected?)
                      (p/fill-rect! g (inc rule-x) y (- (+ left inner-w) rule-x 2) 1)
                      (when text
                        (p/set-colors! g
                                       (if (or selected? (zero? line)) t/dialog-fg t/dialog-hint)
                                       t/dialog-bg)
                        (if (zero? line)
                          (p/styled g [p/BOLD] (p/put-str! g text-x y (p/ellipsize text text-w)))
                          (p/put-str! g text-x y (p/ellipsize text text-w)))))
            (p/set-colors! g t/turn-separator-fg t/dialog-bg)
            (p/put-str! g rule-x y "│")))
    (p/clear-styles! g)))

(defn- thumbnail-reader
  "A `:read-key` that also wakes the modal when a thumbnail finishes loading."
  [thumbnail rows]
  (let [pictures
        #(mapv thumbnail (filter image-row? rows))

        seen
        (volatile! (pictures))]

    (fn [screen]
      (loop []

        (let [now (pictures)]
          (cond (dlg/modal-input-pending? screen) (do (vreset! seen now)
                                                      (dlg/read-modal-key! screen))
                (not= now @seen) (do (vreset! seen now) nil)
                (some #{:loading} now) (do (Thread/sleep 30) (recur))
                :else (dlg/read-modal-key! screen)))))))

(defn inspector-modal-component
  "Pure attachment inspector: one table row for each attachment, with a thumbnail cell, a
   rule and the name and details. Enter opens an app on its app surface, a sent picture in
   the image viewer and any other row in its viewer; o keeps the external opener.

   `opts` can hold `:thumbnail`, a function from a row to its picture (`{:path :mime :width
   :height}`, `:loading` or nil), and `:paint-images!`, which receives the picture regions
   after each refresh. Without them, each thumbnail cell shows the type of its attachment."
  ([staged sent artifacts load-error]
   (inspector-modal-component staged sent artifacts load-error nil))
  ([staged sent artifacts load-error {:keys [thumbnail paint-images!]}]
   (let [rows
         (inspector-rows staged sent artifacts)

         lines
         (table-lines rows load-error)

         total
         (long (count rows))

         page
         #(max 1 (quot (long (:list-h %)) (inc (long block-h))))]

     (cond-> {:init {:selected 0 :scroll 0}
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
                               (min (max 3 (count lines)) (max 3 (- (long rows-count) 8)))

                               bounds
                               (dlg/dialog-bounds cols rows-count content-w content-h-req)

                               {:keys [content-top content-h hint-row]}
                               (dlg/dialog-layout bounds)]

                           {:cols cols
                            :rows rows-count
                            :footer footer
                            :content-w content-w
                            :content-h-req content-h-req
                            :bounds bounds
                            :content-top content-top
                            :content-h content-h
                            :hint-row hint-row
                            :list-h (max 1 (long content-h))}))
              :reconcile (fn [{:keys [selected scroll] :as state} {:keys [list-h]}]
                           (let [selected
                                 (p/clamp (long selected) 0 (max 0 (dec total)))

                                 [first-line last-line]
                                 (block-span lines selected)

                                 max-scroll
                                 (max 0 (- (long (count lines)) (long list-h)))

                                 scroll
                                 (-> (long scroll)
                                     (max (inc (- (long last-line) (long list-h))))
                                     (min (long first-line))
                                     (p/clamp 0 max-scroll))]

                             (assoc state
                               :selected selected
                               :scroll scroll)))
              :paint
              (fn [g {:keys [selected scroll]}
                   {screen-rows :rows
                    :keys [cols footer content-w content-h-req bounds content-top content-h hint-row
                           list-h]}]
                (let [{:keys [left inner-w]} bounds]
                  (dlg/draw-dialog-chrome! g cols screen-rows "Attachments" content-w content-h-req)
                  (p/set-colors! g t/dialog-fg t/dialog-bg)
                  (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
                  (dotimes [i (min (long list-h) (- (count lines) (long scroll)))]
                    (paint-table-line! g
                                       (nth lines (+ (long scroll) (long i)))
                                       rows
                                       thumbnail
                                       selected
                                       bounds
                                       (+ (long content-top) (long i))))
                  (dlg/draw-hint-bar! g left hint-row inner-w footer)
                  nil))
              :on-key
              (fn [{:keys [selected] :as state} ^KeyStroke key geom]
                (let [clamp-selected
                      #(p/clamp (long %) 0 (max 0 (dec total)))

                      selected-row
                      (nth rows selected nil)]

                  (condp = (.getKeyType key)
                    KeyType/Escape {::dlg/done nil}
                    KeyType/ArrowUp (assoc state :selected (clamp-selected (dec (long selected))))
                    KeyType/ArrowDown (assoc state :selected (clamp-selected (inc (long selected))))
                    KeyType/PageUp
                    (assoc state :selected (clamp-selected (- (long selected) (long (page geom)))))
                    KeyType/PageDown
                    (assoc state :selected (clamp-selected (+ (long selected) (long (page geom)))))
                    KeyType/Home (assoc state :selected 0)
                    KeyType/End (assoc state :selected (max 0 (dec total)))
                    KeyType/Enter {::dlg/done (when selected-row
                                                {:action (cond
                                                           (and (= :produced (:source selected-row))
                                                                (app-model/app-row? selected-row))
                                                           :app
                                                           (= :sent (:source selected-row)) :view
                                                           :else :open)
                                                 :row selected-row})}
                    KeyType/Character (if (and (= \o (.getCharacter key)) selected-row)
                                        {::dlg/done {:action :open :row selected-row}}
                                        state)
                    KeyType/Delete {::dlg/done (when (= :staged (:source selected-row))
                                                 {:action :remove :row selected-row})}
                    KeyType/Backspace {::dlg/done (when (= :staged (:source selected-row))
                                                    {:action :remove :row selected-row})}
                    state)))}
       paint-images!
       (assoc :after-refresh
         (fn [{:keys [scroll]} {:keys [bounds content-top list-h]}]
           (paint-images!
             (into []
                   (keep (fn [k]
                           (let [k
                                 (long k)

                                 item
                                 (nth lines k nil)]

                             (when (and item
                                        (zero? (long (:line item -1)))
                                        (< (+ k (dec (long block-h)))
                                           (+ (long scroll) (long list-h))))
                               (thumbnail-region thumbnail
                                                 (nth rows (:block item))
                                                 (:left bounds)
                                                 (+ (long content-top) (- k (long scroll))))))))
                   (range scroll (+ (long scroll) (long list-h)))))))

       thumbnail
       (assoc :read-key (thumbnail-reader thumbnail rows))))))

(defn show!
  "Open the inspector and return its selected action, or nil on close."
  ([screen staged sent artifacts load-error] (show! screen staged sent artifacts load-error nil))
  ([^TerminalScreen screen staged sent artifacts load-error opts]
   (dlg/run-modal! screen (inspector-modal-component staged sent artifacts load-error opts))))

(def ^:private image-zoom-max
  "Largest zoom of the image viewer, as a multiple of the fitted picture."
  8.0)

(def ^:private image-zoom-key-step "Zoom factor of one + or - key press." 1.5)

(def ^:private image-zoom-wheel-step "Zoom factor of one mouse wheel step." 1.25)

(def ^:private image-view-fit {:zoom 1.0 :cx 0.5 :cy 0.5})

(defn- clamp-double ^double [^double v ^double lo ^double hi] (max lo (min hi v)))

(defn image-view-frame
  "Where the viewer draws `image` for `state` in `geom`, or nil without its dimensions.

   At zoom 1 the whole picture fits the dialog. A larger zoom grows the box up to the dialog
   and shows the part of the picture around the center `:cx`/`:cy` (0 to 1 of the width and
   height). Returns `{:row :col :cols :rows :view {:x :y :w :h} :zoom :cx :cy :scale}`:
   `:view` is the shown source rectangle in pixels, `:scale` the source pixels per screen
   pixel, and `:cx`/`:cy` the center after the clamp that keeps the view on the picture."
  [{:keys [width height]} {:keys [zoom cx cy]} {:keys [bounds content-top content-h cell-px]}]
  (when (and width height (pos? (long width)) (pos? (long height)))
    (let [iw
          (double width)

          ih
          (double height)

          [cw ch]
          cell-px

          cw
          (double (max 1 (long (or cw 1))))

          ch
          (double (max 1 (long (or ch 1))))

          inner-w
          (long (:inner-w bounds))

          room-w
          (max 1 (- inner-w 4))

          room-h
          (max 1 (- (long content-h) 2))

          {fit-cols :cols fit-rows :rows}
          (timg/cell-size {:w width :h height} room-w room-h)

          base
          (max (/ iw (* (double fit-cols) cw)) (/ ih (* (double fit-rows) ch)))

          z
          (clamp-double (double (or zoom 1.0)) 1.0 image-zoom-max)

          s
          (/ base z)

          cols
          (if (= 1.0 z) (long fit-cols) (min room-w (max 1 (long (Math/ceil (/ iw (* s cw)))))))

          rows
          (if (= 1.0 z) (long fit-rows) (min room-h (max 1 (long (Math/ceil (/ ih (* s ch)))))))

          w
          (min iw (* (double cols) cw s))

          h
          (min ih (* (double rows) ch s))

          center-x
          (clamp-double (* iw (double (or cx 0.5))) (/ w 2.0) (- iw (/ w 2.0)))

          center-y
          (clamp-double (* ih (double (or cy 0.5))) (/ h 2.0) (- ih (/ h 2.0)))

          vw
          (max 1 (min (long width) (Math/round w)))

          vh
          (max 1 (min (long height) (Math/round h)))

          x
          (max 0 (min (- (long width) vw) (Math/round (- center-x (/ w 2.0)))))

          y
          (max 0 (min (- (long height) vh) (Math/round (- center-y (/ h 2.0)))))]

      {:row (+ (long content-top) 2 (quot (- room-h rows) 2))
       :col (+ (long (:left bounds)) 1 (quot (- inner-w cols) 2))
       :cols cols
       :rows rows
       :view {:x x :y y :w vw :h vh}
       :zoom z
       :cx (/ center-x iw)
       :cy (/ center-y ih)
       :scale s})))

(defn- settle-view
  "Store the clamped center of `state`, so a pan never drifts past an edge."
  [state image geom]
  (if-let [{:keys [cx cy]} (image-view-frame image state geom)]
    (assoc state
      :cx cx
      :cy cy)
    state))

(defn- zoom-view
  "Zoom `state` by `factor`. With an `anchor` cell `[col row]` on the picture, the picture
   point under it stays in place; else the view keeps its center."
  [state image geom factor anchor]
  (let [before
        (image-view-frame image state geom)

        zoom
        (clamp-double (* (double (:zoom state 1.0)) (double factor)) 1.0 image-zoom-max)]

    (cond (nil? before) state
          (<= zoom 1.0) (merge (dissoc state :drag) image-view-fit)
          :else
          (let [zoomed
                (assoc state :zoom zoom)

                after
                (image-view-frame image zoomed geom)

                [pc pr]
                anchor

                on-picture?
                (and anchor
                     (<= (long (:col before)) (long pc))
                     (< (long pc) (+ (long (:col before)) (long (:cols before))))
                     (<= (long (:row before)) (long pr))
                     (< (long pr) (+ (long (:row before)) (long (:rows before)))))]

            (if-not on-picture?
              (settle-view zoomed image geom)
              (let [[cw ch]
                    (:cell-px geom)

                    cw
                    (double (max 1 (long (or cw 1))))

                    ch
                    (double (max 1 (long (or ch 1))))

                    dx
                    (+ 0.5 (double (- (long pc) (long (:col before)))))

                    dy
                    (+ 0.5 (double (- (long pr) (long (:row before)))))

                    ;; The source pixel under the pointer before the zoom.
                    px
                    (+ (double (get-in before [:view :x])) (* dx cw (double (:scale before))))

                    py
                    (+ (double (get-in before [:view :y])) (* dy ch (double (:scale before))))

                    ;; The pointer offset inside the new box, in screen pixels.
                    ax
                    (* cw (+ 0.5 (double (- (long pc) (long (:col after))))))

                    ay
                    (* ch (+ 0.5 (double (- (long pr) (long (:row after))))))

                    s
                    (double (:scale after))

                    w
                    (double (get-in after [:view :w]))

                    h
                    (double (get-in after [:view :h]))]

                (settle-view (assoc zoomed
                               :cx (/ (+ (- px (* ax s)) (/ w 2.0)) (double (:width image)))
                               :cy (/ (+ (- py (* ay s)) (/ h 2.0)) (double (:height image))))
                             image
                             geom)))))))

(defn- pan-view
  "Move the view of a zoomed picture by `dx` and `dy` source pixels."
  [state image geom dx dy]
  (settle-view (assoc state
                 :cx (+ (double (:cx state 0.5)) (/ (double dx) (double (:width image))))
                 :cy (+ (double (:cy state 0.5)) (/ (double dy) (double (:height image)))))
               image
               geom))

(defn- zoomed? [state] (< 1.0 (double (:zoom state 1.0))))

(defn- image-viewer-pointer
  "The next viewer state for a pointer event: the wheel zooms around the pointer, and a drag
   with the primary button moves a zoomed picture."
  [state ^MouseAction key image geom]
  (let [pos
        (.getPosition key)

        col
        (.getColumn pos)

        row
        (.getRow pos)

        action
        (.getActionType key)

        wheel
        (ScrollBar/wheelStep key)

        frame
        (image-view-frame image state geom)]

    (cond (nil? frame) state
          wheel (zoom-view state
                           image
                           geom
                           (if (neg? (long wheel))
                             image-zoom-wheel-step
                             (/ 1.0 (double image-zoom-wheel-step)))
                           [col row])
          (and (= MouseActionType/CLICK_DOWN action) (zoomed? state))
          (assoc state :drag {:col col :row row :cx (:cx frame) :cy (:cy frame)})
          (and (= MouseActionType/DRAG action) (:drag state))
          (let [{start-col :col start-row :row start-cx :cx start-cy :cy}
                (:drag state)

                [cw ch]
                (:cell-px geom)

                s
                (double (:scale frame))]

            (pan-view (assoc state
                        :cx start-cx
                        :cy start-cy)
                      image
                      geom
                      (* -1.0 s (double (or cw 1)) (double (- (long col) (long start-col))))
                      (* -1.0 s (double (or ch 1)) (double (- (long row) (long start-row))))))
          (= MouseActionType/CLICK_RELEASE action) (dissoc state :drag)
          :else state)))

(defn image-viewer-key
  "The next image viewer state for `key`, or `{::dlg/done result}`.

   + and - zoom, 0 fits the picture again. On a zoomed picture the arrows move the view;
   else they select a picture. PgUp, PgDn, Home, End and 1-9 always select a picture. Enter
   or o opens the picture in the system viewer, Esc or q closes. With `geom`, the mouse wheel
   zooms around the pointer and a drag moves a zoomed picture."
  ([state key images] (image-viewer-key state key images nil))
  ([{:keys [index] :as state} key images geom]
   (let [last-index
         (max 0 (dec (count images)))

         image
         (nth images index nil)

         move
         #(merge (assoc state :index (p/clamp (long %) 0 last-index)) image-view-fit)

         open
         {::dlg/done {:action :open :image image :index index}}

         zoom
         #(if geom (zoom-view state image geom % nil) state)

         pan
         (fn [fx fy]
           (if-let [{:keys [view]} (and geom (image-view-frame image state geom))]
             (pan-view state
                       image
                       geom
                       (* (double fx) 0.25 (double (:w view)))
                       (* (double fy) 0.25 (double (:h view))))
             state))

         panning?
         (and geom (zoomed? state))]

     (if (instance? MouseAction key)
       (if geom (image-viewer-pointer state key image geom) state)
       (condp = (.getKeyType ^KeyStroke key)
         KeyType/Escape {::dlg/done nil}
         KeyType/ArrowLeft (if panning? (pan -1 0) (move (dec (long index))))
         KeyType/ArrowUp (if panning? (pan 0 -1) (move (dec (long index))))
         KeyType/ArrowRight (if panning? (pan 1 0) (move (inc (long index))))
         KeyType/ArrowDown (if panning? (pan 0 1) (move (inc (long index))))
         KeyType/PageUp (move (dec (long index)))
         KeyType/PageDown (move (inc (long index)))
         KeyType/Home (move 0)
         KeyType/End (move last-index)
         KeyType/Enter open
         KeyType/Character (let [ch (.getCharacter ^KeyStroke key)]
                             (cond (#{\o \O} ch) open
                                   (#{\q \Q} ch) {::dlg/done nil}
                                   (#{\+ \=} ch) (zoom image-zoom-key-step)
                                   (#{\- \_} ch) (zoom (/ 1.0 (double image-zoom-key-step)))
                                   (= \0 ch) (merge (dissoc state :drag) image-view-fit)
                                   (<= (int \1) (int ch) (int \9))
                                   (let [at (- (int ch) (int \1))]
                                     (if (<= at last-index) (move at) state))
                                   :else state))
         state)))))

(defn image-viewer-component
  "The image viewer modal over `images` (`render/markdown-images` entries), opened at `index`.

   It shows one picture ENLARGED to fill the dialog, with its number, name, type and size above
   it. The keys and the mouse wheel zoom it, and the arrows or a drag move a zoomed picture (see
   [[image-viewer-key]]). `paint-images!` receives the picture region after each refresh,
   because Lanterna does not model terminal pictures. Enter opens the selected picture in the
   system viewer. Only a graphical terminal opens this viewer."
  [images index {:keys [paint-images!]}]
  (let [images
        (vec images)

        n
        (count images)

        fit-footer
        [["←/→" "picture"] ["+/-" "zoom"] ["Enter" "open"] ["Esc" "close"]]

        zoomed-footer
        [["Arrows/drag" "move"] ["+/-" "zoom"] ["0" "fit"] ["Esc" "close"]]]

    {:init (merge {:index (p/clamp (long (or index 0)) 0 (max 0 (dec n)))} image-view-fit)
     :measure (fn [_ cols rows]
                (let [content-w
                      (max 20 (- (long cols) 10))

                      content-h-req
                      (max 6 (- (long rows) 8))

                      bounds
                      (dlg/dialog-bounds cols rows content-w content-h-req)]

                  (merge (dlg/dialog-layout bounds)
                         {:cols cols
                          :rows rows
                          :content-w content-w
                          :content-h-req content-h-req
                          :bounds bounds
                          :cell-px (timg/cell-pixels)})))
     :paint (fn [g {:keys [index] :as state}
                 {:keys [cols rows content-w content-h-req content-top content-h hint-row bounds]}]
              (let [{:keys [left inner-w]}
                    bounds

                    left
                    (long left)

                    inner-w
                    (long inner-w)

                    image
                    (nth images index nil)

                    zoomed
                    (zoomed? state)]

                (dlg/draw-dialog-chrome! g
                                         cols
                                         rows
                                         (str "Image " (inc (long index)) " of " n)
                                         content-w
                                         content-h-req)
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/fill-rect! g (inc left) content-top inner-w content-h)
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
                             (when-not (str/blank? (:label image)) (str "  ·  " (:label image)))
                             (when zoomed
                               (str "  ·  " (Math/round (* 100.0 (double (:zoom state)))) "%")))

                        width
                        (long (p/display-width label))]

                    (when (< (+ 4 width (long (p/display-width (:name image)))) inner-w)
                      (p/put-str! g (- (+ left inner-w) width 1) content-top label)))
                  (when-not (and (:width image) (:height image))
                    (p/put-str! g
                                (+ left 2)
                                (+ (long content-top) 2)
                                "No preview. Press Enter to open the picture.")))
                (dlg/draw-hint-bar! g left hint-row inner-w (if zoomed zoomed-footer fit-footer))
                nil))
     :after-refresh (fn [{:keys [index] :as state} geom]
                      (when paint-images!
                        (let [{:keys [path mime] :as image} (nth images index nil)]
                          (paint-images!
                            (if-let [{:keys [row col cols rows view] :as frame}
                                     (when path (image-view-frame image state geom))]
                              (let [zoomed (< 1.0 (double (:zoom frame)))
                                    {:keys [x y w h]} view]

                                [{:row row
                                  :col col
                                  ;; Each zoomed view is its own upload: the id names its part.
                                  :img (cond-> {:id (str "image-viewer/"
                                                         path
                                                         (when zoomed
                                                           (str "#" x "," y "," w "," h)))
                                                :path path
                                                :mime mime
                                                :cols cols
                                                :rows rows
                                                :width (if zoomed w (:width image))
                                                :height (if zoomed h (:height image))}
                                         zoomed
                                         (assoc :view view))}])
                              [])))))
     :on-key (fn [state key geom]
               (image-viewer-key state key images geom))}))

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

(defonce ^:private thumbnails
  ;; [session-id iteration-id index], or [source path] for a local file -> a delay of the
  ;; picture or of the Future that downloads it.
  (atom {}))

(defn- picture
  [^File file mime]
  (when (and file (.isFile file))
    (when-let [{:keys [w h]} (timg/probe-dimensions (.getPath file) mime)]
      {:path (.getPath file) :mime mime :width w :height h})))

(defn- download-thumbnail
  [session-id {:keys [media-type filename] :as row}]
  (try (picture (materialize-artifact! session-id
                                       row
                                       (File. (System/getProperty "java.io.tmpdir")
                                              "vis-attachment-thumbnails"))
                media-type)
       (catch Throwable e
         (tel/log! {:level :warn
                    :id ::thumbnail-failed
                    :error e
                    :data {:session-id (str session-id) :filename filename}}
                   "could not load an attachment thumbnail")
         nil)))

(defn thumbnail-loader
  "A function from an inspector row to its thumbnail: `{:path :mime :width :height}`,
   `:loading` while the bytes of a produced picture download, or nil. Each picture
   downloads once for each process."
  [session-id]
  (fn [{:keys [source media-type attachment image iteration-id index] :as row}]
    (when (image-row? row)
      (let [path
            (some-> (case source
                      :staged
                      (:path attachment)

                      :sent
                      (:path image)

                      nil)
                    str
                    not-empty)

            k
            (if (= :produced source) [(str session-id) iteration-id index] [source path])

            make
            #(if (= :produced source)
               (delay (vis/worker-future "vis-tui-attachment-thumbnail"
                                         (fn []
                                           (download-thumbnail session-id row))))
               (delay (if (and (:width image) (:height image))
                        {:path path :mime media-type :width (:width image) :height (:height image)}
                        (some-> path
                                File.
                                (picture media-type)))))

            loaded
            @(get (swap! thumbnails update k #(or % (make))) k)]

        (if (instance? java.util.concurrent.Future loaded)
          (if (realized? loaded) @loaded :loading)
          loaded)))))
