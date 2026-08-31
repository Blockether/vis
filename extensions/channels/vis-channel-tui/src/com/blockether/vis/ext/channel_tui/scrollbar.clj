(ns com.blockether.vis.ext.channel-tui.scrollbar
  "Thin Vis adapter over Lanterna's one scrollbar implementation.

   Lanterna owns geometry, drawing, hit-testing, wheel coalescing and
   click/drag interaction for both GUI2 components and raw TextGraphics
   painters. Existing Vis call sites keep their map-shaped boundary while all
   behavior comes from the Java control."
  (:require [com.blockether.vis.ext.channel-tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.gui2 Direction ScrollBar ScrollBar$DragResult
            ScrollBar$Geometry]
           [com.googlecode.lanterna.input MouseAction]))

(defn- nullable-int ^Integer [value] (when (some? value) (Integer/valueOf (int value))))

(defn- geometry-map
  [^ScrollBar$Geometry geometry]
  (when geometry
    {:thumb-top-rel (long (.thumbOffset geometry))
     :thumb-h (long (.thumbSize geometry))
     :max-scroll (long (.maximumPosition geometry))
     :track-h (long (.trackSize geometry))}))

(defn geometry
  "Return the shared one-cell thumb geometry, or nil when content fits."
  ([total-h inner-h scroll] (geometry total-h inner-h inner-h scroll))
  ([total-h inner-h track-h scroll]
   (geometry-map
     (ScrollBar/geometry (int total-h) (int inner-h) (int track-h) (nullable-int scroll)))))

(defn draw!
  "Paint Lanterna's vertical scrollbar and return its geometry map.

   Required opts: `:col :top :track-h :total-h :inner-h :scroll`.
   Optional palette opts default to the dialog theme."
  [^TextGraphics graphics
   {:keys [col top track-h total-h inner-h scroll track-fg track-bg thumb-fg thumb-bg]
    :or {track-fg t/dialog-border
         track-bg t/dialog-bg
         thumb-fg t/dialog-hint-key
         thumb-bg t/dialog-bg}}]
  (geometry-map (ScrollBar/draw graphics
                                Direction/VERTICAL
                                (TerminalPosition. (int col) (int top))
                                (int track-h)
                                (int total-h)
                                (int inner-h)
                                (nullable-int scroll)
                                track-fg
                                track-bg
                                thumb-fg
                                thumb-bg)))

(defn wheel-step
  "Return Lanterna's signed wheel delta including any coalesced event count."
  [event]
  (some-> (ScrollBar/wheelStep event)
          long))

(defn wheel-delta
  "Return -1 for wheel-up, +1 for wheel-down, else nil."
  [event]
  (some-> (wheel-step event)
          Long/signum))

(defn on-track?
  "True when `(mx,my)` is in the vertical track. `:x-band` extends aim left."
  [mx my {:keys [col top track-h x-band] :or {x-band 1}}]
  (ScrollBar/isOnTrack Direction/VERTICAL
                       (int mx)
                       (int my)
                       (TerminalPosition. (int col) (int top))
                       (int track-h)
                       (int x-band)))

(defn- native-geometry
  ^ScrollBar$Geometry [{:keys [thumb-top-rel thumb-h max-scroll track-h]}]
  (when (every? some? [thumb-top-rel thumb-h max-scroll track-h])
    (ScrollBar$Geometry. (int thumb-top-rel) (int thumb-h) (int max-scroll) (int track-h))))

(defn on-thumb?
  "True when `(mx,my)` lands on the thumb represented by `geom`."
  [mx my {:keys [col top x-band] :or {x-band 1}} geom]
  (ScrollBar/isOnThumb Direction/VERTICAL
                       (int mx)
                       (int my)
                       (TerminalPosition. (int col) (int top))
                       (int x-band)
                       (native-geometry geom)))

(defn scroll-from-mouse-y
  "Map an absolute pointer row to a clamped scroll position, preserving grip."
  ([mouse-y top track-h total-h inner-h]
   (scroll-from-mouse-y mouse-y top track-h total-h inner-h 0))
  ([mouse-y top track-h total-h inner-h grip-offset]
   (some-> (ScrollBar/scrollFromTrackCoordinate (int mouse-y)
                                                (int top)
                                                (int track-h)
                                                (int total-h)
                                                (int inner-h)
                                                (int (or grip-offset 0)))
           long)))

(defn mouse-drag-step
  "Fold one non-wheel MouseAction into the existing map-shaped drag protocol."
  [^MouseAction mouse {:keys [col top track-h total-h inner-h scroll x-band] :or {x-band 1}}
   drag-offset]
  (when-let [^ScrollBar$DragResult result (ScrollBar/dragStep mouse
                                                              Direction/VERTICAL
                                                              (TerminalPosition. (int col)
                                                                                 (int top))
                                                              (int track-h)
                                                              (int total-h)
                                                              (int inner-h)
                                                              (nullable-int scroll)
                                                              (nullable-int drag-offset)
                                                              (int x-band))]
    (cond (.release result) :release
          (and (some? (.gripOffset result)) (some? (.scrollPosition result)))
          {:arm (long (.gripOffset result)) :scroll (long (.scrollPosition result))}
          (some? (.gripOffset result)) {:arm (long (.gripOffset result))}
          (some? (.scrollPosition result)) {:scroll (long (.scrollPosition result))})))
