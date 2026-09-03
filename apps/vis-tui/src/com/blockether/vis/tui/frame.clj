(ns com.blockether.vis.tui.frame
  "Lanterna GUI2 grid for the complete Vis screen.

   Every screen-level surface is a real `TextGraphicsComponent` placed by one
   `GridLayout`: header, transcript, echo area, attachment rail, composer, and
   footer. Painters still own their cell-level visuals; this namespace owns only
   their measured rectangles. The same component tree therefore works with a
   native terminal, `HtmlTerminal`, and `HtmlTerminalView` without a second
   browser layout implementation."
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.gui2 Component GridLayout GridLayout$Alignment Panel
            Panel$DefaultPanelRenderer TextGraphicsComponent TextGraphicsComponent$Painter
            TextGUIGraphics]
           [com.googlecode.lanterna.screen Screen]))

(def section-order
  "Stable paint/layout order for the complete application frame."
  [:header :header-gap :transcript :echo :attachments :composer :footer])

(defn- checked-size
  ^long [label value]
  (let [value (long value)]
    (when (neg? value)
      (throw (IllegalArgumentException. (str (name label) " must be non-negative"))))
    value))

(defn- root-coordinate-graphics
  "Keep legacy screen coordinates while a GUI2 component supplies the real clip.
   The returned surface has root dimensions, but the component-local backend drops
   every write outside the laid-out component rectangle."
  ^TextGUIGraphics [^TextGUIGraphics graphics ^Component component ^TerminalSize root-size]
  (let [^TerminalPosition position (.getPosition component)]
    (.newTextGraphics graphics
                      (TerminalPosition. (- (.getColumn position)) (- (.getRow position)))
                      root-size)))

(defn component
  "Create one paint-only GUI2 component. `painter` receives the graphics supplied
   by GUI2 and the laid-out component, whose position and size are authoritative."
  ^TextGraphicsComponent [cols rows painter]
  (TextGraphicsComponent.
    (TerminalSize. (int (checked-size :columns cols)) (int (checked-size :rows rows)))
    (reify
      TextGraphicsComponent$Painter
        (paint [_ graphics component] (when painter (painter graphics component))))))

(defn- layout-data
  [grow-vertical?]
  (GridLayout/createLayoutData GridLayout$Alignment/FILL
                               GridLayout$Alignment/FILL
                               true
                               (boolean grow-vertical?)))

(defn layout
  "Resolve the complete screen into integer cell rectangles through Lanterna's
   `GridLayout`. Fixed heights are `:header`, `:attachments`, `:composer`, and
   `:footer`; the transcript receives every remaining row. Optional `painters`
   maps section IDs to `(fn [graphics component])` callbacks. Their graphics keep
   root-screen coordinates for the existing painters but are clipped by the real
   laid-out component."
  ([cols rows heights] (layout cols rows heights {}))
  ([cols rows
    {:keys [header attachments composer footer] :or {header 0 attachments 0 composer 0 footer 2}}
    painters]
   (let [cols
         (checked-size :columns cols)

         rows
         (checked-size :rows rows)

         heights
         {:header (checked-size :header header)
          :header-gap 1
          :transcript 0
          :echo 1
          :attachments (checked-size :attachments attachments)
          :composer (checked-size :composer composer)
          :footer (checked-size :footer footer)}

         size
         (TerminalSize. (int cols) (int rows))

         manager
         (doto (GridLayout. 1)
           (.setHorizontalSpacing 0)
           (.setVerticalSpacing 0)
           (.setTopMarginSize 0)
           (.setBottomMarginSize 0)
           (.setLeftMarginSize 0)
           (.setRightMarginSize 0))

         panel
         (Panel. manager)

         sections
         (into (array-map)
               (map (fn [id]
                      (let [painter
                            (get painters id)

                            section
                            (doto (component
                                    cols
                                    (get heights id)
                                    (when painter
                                      (fn [graphics component]
                                        (painter (root-coordinate-graphics graphics component size)
                                                 component))))
                              (.setLayoutData (layout-data (= id :transcript))))]

                        (.addComponent panel section)
                        [id section])))
               section-order)]

     (.setPosition panel TerminalPosition/TOP_LEFT_CORNER)
     (.setSize panel size)
     (.doLayout manager size (.getChildrenList panel))
     {:panel panel :sections sections :size size})))

(defn view
  "Build one full-size GUI2 view around a cell painter. This is the component form
   passed directly to `HtmlTerminalView` for an isolated interactive preview."
  ^Panel [cols rows painter]
  (let [cols
        (checked-size :columns cols)

        rows
        (checked-size :rows rows)

        manager
        (doto (GridLayout. 1)
          (.setHorizontalSpacing 0)
          (.setVerticalSpacing 0)
          (.setTopMarginSize 0)
          (.setBottomMarginSize 0)
          (.setLeftMarginSize 0)
          (.setRightMarginSize 0))

        panel
        (Panel. manager)

        child
        (doto (component cols rows painter) (.setLayoutData (layout-data true)))

        size
        (TerminalSize. (int cols) (int rows))]

    (.addComponent panel child)
    (.setPosition panel TerminalPosition/TOP_LEFT_CORNER)
    (.setSize panel size)
    (.doLayout manager size (.getChildrenList panel))
    panel))

(defn- expose-view-graphics
  ^TextGraphics [^Panel panel ^TextGraphics graphics captured]
  (let [^Panel$DefaultPanelRenderer renderer (.getRenderer panel)]
    ;; A bridge scopes an incremental painter; unlike a normal GUI2 panel it must
    ;; not erase cells that this painter did not touch.
    (.setFillAreaBeforeDrawingComponents renderer false)
    (.draw panel (TextGUIGraphics/from graphics))
    (or @captured (throw (IllegalStateException. "Grid view did not expose its graphics")))))

(defn view-graphics
  "Return graphics scoped by one full-size GridLayout component. Existing cell
   painters can use this bridge while GUI2 owns their view boundary and clipping.
   Creating the bridge is non-destructive, so partial repaints preserve other cells."
  ^TextGraphics [^TextGraphics graphics cols rows]
  (let [cols
        (checked-size :columns cols)

        rows
        (checked-size :rows rows)

        captured
        (volatile! nil)

        panel
        (view cols
              rows
              (fn [local-graphics _]
                (vreset! captured local-graphics)))

        clipped
        (.newTextGraphics graphics
                          TerminalPosition/TOP_LEFT_CORNER
                          (TerminalSize. (int cols) (int rows)))]

    (expose-view-graphics panel clipped captured)))

(defn surface-graphics
  "Create a terminal-screen surface scoped by one full-size GridLayout component."
  ^TextGraphics [^Screen screen cols rows]
  (view-graphics (.newTextGraphics screen) cols rows))


(defn bounds
  "Return a laid-out component's integer cell rectangle."
  [^Component component]
  (let [^TerminalPosition position
        (.getPosition component)

        ^TerminalSize size
        (.getSize component)]

    {:col (.getColumn position)
     :row (.getRow position)
     :cols (.getColumns size)
     :rows (.getRows size)}))

(defn paint!
  "Draw one laid-out section through its GUI2 renderer and component-local clip.
   Layout painters retain root-screen coordinates through [[layout]]'s adapter."
  [^TextGraphics graphics {:keys [sections]} section-id]
  (let [^TextGraphicsComponent section
        (or (get sections section-id)
            (throw (IllegalArgumentException. (str "Unknown frame section " section-id))))

        ^TerminalPosition position
        (.getPosition section)

        ^TerminalSize size
        (.getSize section)

        ^TextGUIGraphics local
        (.newTextGraphics (TextGUIGraphics/from graphics) position size)]

    (.draw section local)
    section))
