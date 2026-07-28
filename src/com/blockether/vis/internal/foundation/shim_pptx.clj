(ns com.blockether.vis.internal.foundation.shim-pptx
  "Built-in sandbox SHIM: a `pptx` (python-pptx) compatible module backed by
   Apache POI XSLF (`org.apache.poi/poi-ooxml`) so `from pptx import Presentation`
   writes real .pptx files without the CPython package. Presentations/slides/
   shapes live HOST-side in an integer registry; the Python classes are thin
   handle wrappers exchanging EMU geometry + base64 image/file bytes across the
   boundary."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis])
  (:import [java.awt Color]
           [java.awt.geom Rectangle2D$Double]
           [java.io ByteArrayOutputStream]
           [java.util Base64]
           [org.apache.poi.sl.usermodel PictureData$PictureType ShapeType TextParagraph$TextAlign]
           [org.apache.poi.xslf.usermodel XMLSlideShow XSLFSimpleShape XSLFSlide XSLFSlideLayout
            XSLFSlideMaster XSLFTextParagraph XSLFTextRun XSLFTextShape]
           [org.apache.xmlbeans XmlCursor XmlObject]
           [org.openxmlformats.schemas.drawingml.x2006.main CTTextParagraph
            CTTextParagraphProperties]
           [javax.xml.namespace QName]))

;; Host-side registry: handle (long) -> {:ss XMLSlideShow :slides (atom [XSLFSlide])
;;   :shapes (atom {shape-id {:shape XSLFTextShape :paras (atom [{:p .. :runs (atom [..])}])}})}.

(defonce ^:private pres-registry (atom {}))

(defonce ^:private pres-counter (atom 0))

(defonce ^:private shape-counter (atom 0))

(def ^:private ^:const emu-per-pt 12700.0)

(defn- emu->pt ^double [emu] (/ (double emu) emu-per-pt))

(defn- b64enc [^bytes ba] (.encodeToString (Base64/getEncoder) ba))

(defn- b64dec ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- entry-of
  [h]
  (or (get @pres-registry (long h)) (throw (ex-info "Presentation is closed." {}))))

(defn- shape-of
  [h sid]
  (or (get @(:shapes (entry-of h)) (long sid)) (throw (ex-info "No such shape." {}))))

(defn- para-of [h sid pidx] (nth @(:paras (shape-of h sid)) (long pidx)))

(defn- run-of [h sid pidx ridx] (nth @(:runs (para-of h sid pidx)) (long ridx)))

(def ^:private color-names
  {"black" "000000"
   "white" "FFFFFF"
   "red" "FF0000"
   "green" "008000"
   "blue" "0000FF"
   "yellow" "FFFF00"
   "cyan" "00FFFF"
   "magenta" "FF00FF"
   "navy" "000080"
   "orange" "FF6600"
   "pink" "FFC0CB"
   "purple" "800080"
   "gray" "808080"
   "grey" "808080"
   "silver" "C0C0C0"
   "lime" "00FF00"
   "brown" "800000"})

(defn- hex->awt
  ^Color [s]
  (let
    [low
     (str/lower-case (str s))

     h
     (or (color-names low) (if (str/starts-with? (str s) "#") (subs (str s) 1) (str s)))

     h
     (if (= 3 (count h)) (apply str (mapcat #(list % %) h)) h)]

    (Color. (Integer/parseInt h 16))))

(defn- text-align
  [a]
  (case (str/upper-case (str a))
    "CENTER"
    TextParagraph$TextAlign/CENTER

    "RIGHT"
    TextParagraph$TextAlign/RIGHT

    ("JUSTIFY" "JUSTIFY_LOW")
    TextParagraph$TextAlign/JUSTIFY

    TextParagraph$TextAlign/LEFT))

(defn- pic-type
  ^PictureData$PictureType [ext]
  (case (str/lower-case (str (or ext "png")))
    ("jpg" "jpeg")
    PictureData$PictureType/JPEG

    "gif"
    PictureData$PictureType/GIF

    "bmp"
    PictureData$PictureType/BMP

    "tiff"
    PictureData$PictureType/TIFF

    PictureData$PictureType/PNG))

(defn- shape-type
  [nm]
  (case (str/upper-case (str nm))
    "RECTANGLE"
    ShapeType/RECT

    "ROUNDED_RECTANGLE"
    ShapeType/ROUND_RECT

    "OVAL"
    ShapeType/ELLIPSE

    "ISOCELES_TRIANGLE"
    ShapeType/TRIANGLE

    "RIGHT_TRIANGLE"
    ShapeType/RT_TRIANGLE

    "DIAMOND"
    ShapeType/DIAMOND

    "PENTAGON"
    ShapeType/HOME_PLATE

    "HEXAGON"
    ShapeType/HEXAGON

    "CHEVRON"
    ShapeType/CHEVRON

    "STAR_5_POINT"
    ShapeType/STAR_5

    "RIGHT_ARROW"
    ShapeType/RIGHT_ARROW

    "LEFT_ARROW"
    ShapeType/LEFT_ARROW

    "UP_ARROW"
    ShapeType/UP_ARROW

    "DOWN_ARROW"
    ShapeType/DOWN_ARROW

    "HEART"
    ShapeType/HEART

    "CLOUD"
    ShapeType/CLOUD

    "SUN"
    ShapeType/SUN

    "MOON"
    ShapeType/MOON

    "LIGHTNING_BOLT"
    ShapeType/LIGHTNING_BOLT

    "PLAQUE"
    ShapeType/PLAQUE

    ShapeType/RECT))

;; python-pptx default-template slide-layout index -> POI SlideLayout type. POI's
;; built-in template ships real masters/layouts, so slides get real placeholders.
(def ^:private layout-type
  {0 "TITLE"
   1 "TITLE_AND_CONTENT"
   2 "SECTION_HEADER"
   3 "TWO_OBJ"
   4 "TWO_TX_TWO_OBJ"
   5 "TITLE_ONLY"
   6 "BLANK"
   7 "OBJ_TX"
   8 "PIC_TX"})

(def ^:private drawingml-ns "http://schemas.openxmlformats.org/drawingml/2006/main")

;; python-pptx shape-type -> its AutoShapeType basename (used to name shapes
;; exactly as python-pptx: "<basename> <shapeId-1>").
(def ^:private shape-basename
  {"RECTANGLE" "Rectangle"
   "ROUNDED_RECTANGLE" "Rounded Rectangle"
   "OVAL" "Oval"
   "ISOCELES_TRIANGLE" "Isosceles Triangle"
   "RIGHT_TRIANGLE" "Right Triangle"
   "DIAMOND" "Diamond"
   "PENTAGON" "Pentagon"
   "HEXAGON" "Hexagon"
   "CHEVRON" "Chevron"
   "STAR_5_POINT" "5-Point Star"
   "RIGHT_ARROW" "Right Arrow"
   "LEFT_ARROW" "Left Arrow"
   "UP_ARROW" "Up Arrow"
   "DOWN_ARROW" "Down Arrow"
   "HEART" "Heart"
   "CLOUD" "Cloud"
   "SUN" "Sun"
   "MOON" "Moon"
   "LIGHTNING_BOLT" "Lightning Bolt"
   "PLAQUE" "Plaque"})

(defn- set-shape-name!
  "Rename a shape's cNvPr (poi-ooxml-lite hides the typed setter, so edit the XML
   via an XmlCursor) to python-pptx's convention."
  [^XSLFSimpleShape shape nm]
  (let
    [^XmlObject xo
     (.getXmlObject shape)

     ^XmlCursor cur
     (.newCursor xo)]

    (.toFirstChild cur) ; nvSpPr / nvPicPr
    (.toFirstChild cur) ; cNvPr
    (.setAttributeText cur (QName. "name") nm)
    (.dispose cur)))

(defn- srgb-val
  ^String [s]
  (let [^Color c (hex->awt s)]
    (format "%02X%02X%02X" (.getRed c) (.getGreen c) (.getBlue c))))

(defn- defrpr-attr-keys
  [props]
  (let
    [ks (cond (map? props) (keys props)
              (instance? java.util.Map props) (seq (.keySet ^java.util.Map props))
              :else nil)]
    (filter #{"size" "bold" "italic" "underline"} (or ks ["bold" "italic" "underline" "size"]))))

(defn- build-defrpr!
  "Write python-pptx's <a:defRPr> (paragraph default run props) onto pPr via an
   XmlCursor. poi-ooxml-lite ships no CTBoolean etc., so the typed setters are
   unreachable; a raw cursor sidesteps the whole schema-class surface."
  [^CTTextParagraphProperties pPr props]
  (let [^XmlCursor cur (.newCursor pPr)]
    (.toEndToken cur)
    (.beginElement cur "defRPr" drawingml-ns)
    (doseq
      [k (defrpr-attr-keys props)
       :let [v (get props k)]]

      (case k
        "size"
        (.insertAttributeWithValue cur "sz" (str (long (/ (long v) 127))))

        "bold"
        (when (some? v) (.insertAttributeWithValue cur "b" (if v "1" "0")))

        "italic"
        (when (some? v) (.insertAttributeWithValue cur "i" (if v "1" "0")))

        "underline"
        (when (some? v) (.insertAttributeWithValue cur "u" (if v "sng" "none")))

        nil))
    (when-let [color (get props "color")]
      (.beginElement cur "solidFill" drawingml-ns)
      (.beginElement cur "srgbClr" drawingml-ns)
      (.insertAttributeWithValue cur "val" (srgb-val color))
      (.toParent cur)
      (.toParent cur)
      (.toEndToken cur)
      (.toNextToken cur))
    (when-let [nm (get props "name")]
      (.beginElement cur "latin" drawingml-ns)
      (.insertAttributeWithValue cur "typeface" (str nm)))
    (.dispose cur)))

(defn- new-text-shape!
  [shapes ^XSLFTextShape box]
  (.clearText box)
  (let
    [p
     (.addNewTextParagraph box)

     sid
     (swap! shape-counter inc)]

    (swap! shapes assoc sid {:shape box :paras (atom [{:p p :runs (atom [])}])})
    sid))

(defn- resync-paras!
  [se]
  (reset! (:paras se) (mapv (fn [p]
                              {:p p :runs (atom (vec (.getTextRuns ^XSLFTextParagraph p)))})
                            (.getTextParagraphs ^XSLFTextShape (:shape se)))))

;; Host operations (Apache POI XSLF).

(defn- op-new
  [width height]
  (let
    [ss
     (XMLSlideShow.)

     h
     (swap! pres-counter inc)]

    (.setPageSize ss
                  (java.awt.Dimension. (int (emu->pt (or width 9144000)))
                                       (int (emu->pt (or height 6858000)))))
    (swap! pres-registry assoc h {:ss ss :slides (atom []) :shapes (atom {})})
    (let [d (.getPageSize ss)]
      {"handle" h
       "width" (long (* (.getWidth d) emu-per-pt))
       "height" (long (* (.getHeight d) emu-per-pt))})))

(defn- op-slide-size
  [h]
  (let [d (.getPageSize ^XMLSlideShow (:ss (entry-of h)))]
    {"width" (long (* (.getWidth d) emu-per-pt)) "height" (long (* (.getHeight d) emu-per-pt))}))

(defn- op-set-slide-size
  [h width height]
  (.setPageSize ^XMLSlideShow (:ss (entry-of h))
                (java.awt.Dimension. (int (emu->pt width)) (int (emu->pt height))))
  nil)

(defn- op-add-slide
  [h layout-idx]
  (let
    [{:keys [^XMLSlideShow ss slides shapes]}
     (entry-of h)

     want
     (get layout-type (int (or layout-idx 6)) "BLANK")

     ^XSLFSlideMaster master
     (first (.getSlideMasters ss))

     layouts
     (.getSlideLayouts master)

     ^XSLFSlideLayout layout
     (or (first (filter #(= want (str (.getType ^XSLFSlideLayout %))) layouts))
         (first (filter #(= "BLANK" (str (.getType ^XSLFSlideLayout %))) layouts))
         (first layouts))

     ^XSLFSlide sl
     (.createSlide ss layout)

     specs
     (vec (map-indexed (fn [i ^XSLFTextShape ph]
                         {"idx" i
                          "ph_type" (str (.getPlaceholder ph))
                          "name" (.getShapeName ph)
                          "shape_id" (new-text-shape! shapes ph)})
                       (.getPlaceholders sl)))]

    (swap! slides conj sl)
    {"index" (dec (count @slides)) "placeholders" specs}))

(defn- op-add-textbox
  [h slide l t w hh]
  (let
    [{:keys [slides shapes]}
     (entry-of h)

     box
     (.createTextBox ^XSLFSlide (nth @slides (long slide)))]

    (.setAnchor box (Rectangle2D$Double. (emu->pt l) (emu->pt t) (emu->pt w) (emu->pt hh)))
    (set-shape-name! box (str "TextBox " (dec (.getShapeId box))))
    (new-text-shape! shapes box)))

(defn- op-add-autoshape
  [h slide type-name l t w hh]
  (let
    [{:keys [slides shapes]}
     (entry-of h)

     sh
     (.createAutoShape ^XSLFSlide (nth @slides (long slide)))]

    (.setShapeType sh (shape-type type-name))
    (.setAnchor sh (Rectangle2D$Double. (emu->pt l) (emu->pt t) (emu->pt w) (emu->pt hh)))
    (set-shape-name! sh
                     (str (get shape-basename (str/upper-case (str type-name)) "AutoShape")
                          " "
                          (dec (.getShapeId sh))))
    (new-text-shape! shapes sh)))

(defn- op-add-picture
  [h slide l t w hh b64 ext]
  (let
    [{:keys [^XMLSlideShow ss slides]}
     (entry-of h)

     sl
     ^XSLFSlide (nth @slides (long slide))

     pd
     (.addPicture ss ^bytes (b64dec b64) (pic-type ext))

     pic
     (.createPicture sl pd)

     dim
     (.getImageDimension pd)

     pw
     (if w (emu->pt w) (.getWidth dim))

     ph
     (if hh (emu->pt hh) (.getHeight dim))]

    (.setAnchor pic (Rectangle2D$Double. (emu->pt l) (emu->pt t) pw ph))
    (set-shape-name! pic (str "Picture " (dec (.getShapeId pic))))
    {"width" (long (* pw emu-per-pt)) "height" (long (* ph emu-per-pt))}))

(defn- op-set-shape-geom
  [h sid l t w hh]
  (let
    [sh
     ^XSLFSimpleShape (:shape (shape-of h sid))

     a
     (.getAnchor sh)]

    (.setAnchor sh
                (Rectangle2D$Double. (if l (emu->pt l) (.getX a))
                                     (if t (emu->pt t) (.getY a))
                                     (if w (emu->pt w) (.getWidth a))
                                     (if hh (emu->pt hh) (.getHeight a))))
    nil))

(defn- op-set-fill
  [h sid hex]
  (.setFillColor ^XSLFSimpleShape (:shape (shape-of h sid)) (hex->awt hex))
  nil)

(defn- op-set-shape-text
  [h sid text]
  (let [se (shape-of h sid)]
    (.setText ^XSLFTextShape (:shape se) (str text))
    (resync-paras! se)
    nil))

(defn- op-get-shape-text [h sid] (str (.getText ^XSLFTextShape (:shape (shape-of h sid)))))

(defn- op-add-para
  [h sid]
  (let
    [se
     (shape-of h sid)

     p
     (.addNewTextParagraph ^XSLFTextShape (:shape se))]

    (swap! (:paras se) conj {:p p :runs (atom [])})
    (dec (count @(:paras se)))))

(defn- op-add-run
  [h sid pidx]
  (let
    [pe
     (para-of h sid pidx)

     r
     (.addNewTextRun ^XSLFTextParagraph (:p pe))]

    (swap! (:runs pe) conj r)
    (dec (count @(:runs pe)))))

(defn- op-set-run-text
  [h sid pidx ridx text]
  (.setText ^XSLFTextRun (run-of h sid pidx ridx) (str text))
  nil)

(defn- op-set-para-text
  [h sid pidx text]
  (let
    [pe
     (para-of h sid pidx)

     runs
     @(:runs pe)]

    (if (seq runs)
      (do (.setText ^XSLFTextRun (first runs) (str text))
          (doseq [r (rest runs)]
            (.setText ^XSLFTextRun r "")))
      (let [r (.addNewTextRun ^XSLFTextParagraph (:p pe))]
        (swap! (:runs pe) conj r)
        (.setText r (str text))))
    nil))

(defn- apply-run-font!
  [^XSLFTextRun r props]
  (let [{:strs [bold italic underline size name color]} props]
    (when (some? bold) (.setBold r (boolean bold)))
    (when (some? italic) (.setItalic r (boolean italic)))
    (when (some? underline) (.setUnderlined r (boolean underline)))
    (when size (.setFontSize r (double (/ (double size) emu-per-pt))))
    (when name (.setFontFamily r (str name)))
    (when color (.setFontColor r (hex->awt color)))))

(defn- op-set-run-font [h sid pidx ridx props] (apply-run-font! (run-of h sid pidx ridx) props) nil)

(defn- op-set-para-font
  [h sid pidx props]
  (let
    [pe
     (para-of h sid pidx)

     ^CTTextParagraph ctp
     (.getXmlObject ^XSLFTextParagraph (:p pe))

     ^CTTextParagraphProperties pPr
     (if (.isSetPPr ctp) (.getPPr ctp) (.addNewPPr ctp))]

    ;; python-pptx paragraph.font lives on pPr's <a:defRPr> (default run props),
    ;; NOT on the runs; rebuild it cleanly on every cumulative flush.
    (when (.isSetDefRPr pPr) (.unsetDefRPr pPr))
    (build-defrpr! pPr props)
    nil))

(defn- op-set-para-align
  [h sid pidx a]
  (.setTextAlign ^XSLFTextParagraph (:p (para-of h sid pidx)) (text-align a))
  nil)

(defn- op-set-para-level
  [h sid pidx level]
  (.setIndentLevel ^XSLFTextParagraph (:p (para-of h sid pidx)) (int level))
  nil)

(defn- op-save
  [h]
  (let
    [{:keys [^XMLSlideShow ss]}
     (entry-of h)

     bos
     (ByteArrayOutputStream.)]

    (.write ss bos)
    (b64enc (.toByteArray bos))))

(defn- op-close
  [h]
  (when-let [{:keys [^XMLSlideShow ss]} (get @pres-registry (long h))]
    (.close ss)
    (swap! pres-registry dissoc (long h)))
  nil)

(defn- envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- pptx-bridge-bindings
  "Host callables (Apache POI XSLF) the pptx shim delegates to."
  []
  {"__vis_pptx_new__" (fn [w h]
                        (envelope #(op-new w h)))
   "__vis_pptx_slide_size__" (fn [h]
                               (envelope #(op-slide-size h)))
   "__vis_pptx_set_slide_size__" (fn [h w hh]
                                   (envelope #(op-set-slide-size h w hh)))
   "__vis_pptx_add_slide__" (fn [h li]
                              (envelope #(op-add-slide h li)))
   "__vis_pptx_add_textbox__" (fn [h s l t w hh]
                                (envelope #(op-add-textbox h s l t w hh)))
   "__vis_pptx_add_shape__" (fn [h s ty l t w hh]
                              (envelope #(op-add-autoshape h s ty l t w hh)))
   "__vis_pptx_add_picture__" (fn [h s l t w hh b64 ext]
                                (envelope #(op-add-picture h s l t w hh b64 ext)))
   "__vis_pptx_set_geom__" (fn [h sid l t w hh]
                             (envelope #(op-set-shape-geom h sid l t w hh)))
   "__vis_pptx_set_fill__" (fn [h sid hex]
                             (envelope #(op-set-fill h sid hex)))
   "__vis_pptx_set_shape_text__" (fn [h sid text]
                                   (envelope #(op-set-shape-text h sid text)))
   "__vis_pptx_get_shape_text__" (fn [h sid]
                                   (envelope #(op-get-shape-text h sid)))
   "__vis_pptx_add_para__" (fn [h sid]
                             (envelope #(op-add-para h sid)))
   "__vis_pptx_add_run__" (fn [h sid p]
                            (envelope #(op-add-run h sid p)))
   "__vis_pptx_set_run_text__" (fn [h sid p r t]
                                 (envelope #(op-set-run-text h sid p r t)))
   "__vis_pptx_set_para_text__" (fn [h sid p t]
                                  (envelope #(op-set-para-text h sid p t)))
   "__vis_pptx_set_run_font__" (fn [h sid p r props]
                                 (envelope #(op-set-run-font h sid p r props)))
   "__vis_pptx_set_para_font__" (fn [h sid p props]
                                  (envelope #(op-set-para-font h sid p props)))
   "__vis_pptx_set_para_align__" (fn [h sid p a]
                                   (envelope #(op-set-para-align h sid p a)))
   "__vis_pptx_set_para_level__" (fn [h sid p l]
                                   (envelope #(op-set-para-level h sid p l)))
   "__vis_pptx_save__" (fn [h]
                         (envelope #(op-save h)))
   "__vis_pptx_close__" (fn [h]
                          (envelope #(op-close h)))})

;; Python preamble: publishes a python-pptx-compatible module into sys.modules.


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pptx"
     :ext/description
     "Sandbox shim: a python-pptx-compatible module (Presentation/slides.add_slide/slide_layouts, shapes.add_textbox/add_shape/add_picture, text_frame/paragraphs/runs/font/color/alignment, title+body placeholders, slide_width/height, util Inches/Pt/Emu/Cm, RGBColor, PP_ALIGN/MSO_SHAPE enums) backed by pure-Java Apache POI XSLF. GraalPy can't install the CPython package; this makes `from pptx import Presentation` produce real .pptx files. No pip, no native wheel, no host binary."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pptx"
       :shim/imports ["pptx"]
       :shim/description
       "python-pptx-compatible .pptx writer backed by pure-Java Apache POI XSLF (presentations/slides/shapes by integer handle; file returns as base64 on save). Not supported: charts, animation, and advanced slide layouts."
       :shim/bindings pptx-bridge-bindings
       :shim/source "vis-shims/pptx.py"}]}))

(vis/register-extension! vis-extension)
