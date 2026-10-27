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

(defn- row-label
  [{:keys [source filename media-type size version version-count title app-status label] :as row}]
  (cond (app-model/app-row? row) (str (or (not-empty (str title)) filename)
                                      "  ·  App"
                                      (when app-status
                                        (str "  ·  " (get app-model/status-label app-status))))
        (and (= :sent source) (not (str/blank? label))) (str filename "  ·  " label)
        :else (str filename
                   (when (= source :produced)
                     (str (when version (str "  v" version))
                          (when (> (long version-count) 1)
                            (str "  ·  " version-count " versions"))))
                   "  ·  "
                   media-type
                   (when-let [label (size-label size)]
                     (str "  ·  " label)))))

(defn- display-rows
  [rows load-error]
  (let [staged
        (filterv #(= :staged (:source %)) rows)

        sent
        (filterv #(= :sent (:source %)) rows)

        produced
        (filterv #(= :produced (:source %)) rows)]

    (cond-> []
      (seq staged)
      (into (cons {:header "Ready to send"} staged))

      (seq sent)
      (into (cons {:header "Sent in this session"} sent))

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
  "Pure attachment inspector. Enter opens an app on its app surface, a sent picture in
   the image viewer and any other row in its viewer; o keeps the external opener."
  [staged sent artifacts load-error]
  (let [rows
        (inspector-rows staged sent artifacts)

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
           KeyType/Enter {::dlg/done (when selected-row
                                       {:action (cond (and (= :produced (:source selected-row))
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
           state)))}))

(defn show!
  "Open the inspector and return its selected action, or nil on close."
  [^TerminalScreen screen staged sent artifacts load-error]
  (dlg/run-modal! screen (inspector-modal-component staged sent artifacts load-error)))

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
