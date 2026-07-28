(ns com.blockether.vis.internal.foundation.shim-xlsxwriter
  "Built-in sandbox SHIM: an `xlsxwriter`-compatible module backed by Apache POI
   (`org.apache.poi/poi-ooxml`) so `import xlsxwriter` writes real .xlsx files
   without the CPython package. Workbooks/formats live HOST-side in an integer
   registry; the Python classes are thin handle wrappers; the finished file
   crosses the boundary as base64 bytes on `close()`."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis])
  (:import [java.io ByteArrayOutputStream]
           [java.util Base64]
           [org.apache.poi.common.usermodel HyperlinkType]
           [org.apache.poi.ss.usermodel BorderStyle Cell FillPatternType Font HorizontalAlignment
            VerticalAlignment]
           [org.apache.poi.ss.util CellRangeAddress WorkbookUtil]
           [org.apache.poi.xssf.usermodel XSSFCellStyle XSSFColor XSSFFont XSSFWorkbook]))

;; Host-side registry: handle (long) -> {:wb XSSFWorkbook :styles (atom [style])}.

(defonce ^:private wb-registry (atom {}))

(defonce ^:private wb-counter (atom 0))

(defn- b64enc [^bytes ba] (.encodeToString (Base64/getEncoder) ba))

(defn- entry-of [h] (or (get @wb-registry (long h)) (throw (ex-info "Workbook is closed." {}))))

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

(defn- hex->color
  ^XSSFColor [s]
  (let
    [low
     (str/lower-case (str s))

     h
     (or (color-names low) (if (str/starts-with? (str s) "#") (subs (str s) 1) (str s)))

     h
     (if (= 3 (count h)) (apply str (mapcat #(list % %) h)) h)

     n
     (Integer/parseInt h 16)

     argb
     (byte-array [(unchecked-byte 0xFF) (unchecked-byte (bit-shift-right n 16))
                  (unchecked-byte (bit-shift-right n 8)) (unchecked-byte n)])]

    (XSSFColor. ^bytes argb nil)))

(defn- h-align
  [a]
  (case (str/lower-case (str a))
    "left"
    HorizontalAlignment/LEFT

    ("center" "centre")
    HorizontalAlignment/CENTER

    "right"
    HorizontalAlignment/RIGHT

    "fill"
    HorizontalAlignment/FILL

    "justify"
    HorizontalAlignment/JUSTIFY

    "center_across"
    HorizontalAlignment/CENTER_SELECTION

    HorizontalAlignment/GENERAL))

(defn- v-align
  [a]
  (case (str/lower-case (str a))
    "top"
    VerticalAlignment/TOP

    "vcenter"
    VerticalAlignment/CENTER

    "bottom"
    VerticalAlignment/BOTTOM

    "vjustify"
    VerticalAlignment/JUSTIFY

    VerticalAlignment/BOTTOM))

(defn- border-style
  [b]
  (if (number? b)
    (case (long b)
      1
      BorderStyle/THIN

      2
      BorderStyle/MEDIUM

      3
      BorderStyle/DASHED

      4
      BorderStyle/DOTTED

      5
      BorderStyle/THICK

      6
      BorderStyle/DOUBLE

      7
      BorderStyle/HAIR

      BorderStyle/THIN)
    BorderStyle/THIN))

(defn- build-style
  ^XSSFCellStyle [^XSSFWorkbook wb props]
  (let
    [st
     (.createCellStyle wb)

     ^XSSFFont ft
     (.createFont wb)

     {:strs [bold italic underline font_size size font_name font font_color color bg_color
             num_format align valign text_wrap border]}
     props]

    (when bold (.setBold ft true))
    (when italic (.setItalic ft true))
    (when underline
      (.setUnderline
        ft
        (if (and (number? underline) (= 2 (long underline))) Font/U_DOUBLE Font/U_SINGLE)))
    (when-let [s (or font_size size)]
      (.setFontHeightInPoints ft (short (long s))))
    (when-let [n (or font_name font)]
      (.setFontName ft (str n)))
    (when-let [c (or font_color color)]
      (.setColor ft ^XSSFColor (hex->color c)))
    (.setFont st ft)
    (when bg_color
      (.setFillForegroundColor st ^XSSFColor (hex->color bg_color))
      (.setFillPattern st FillPatternType/SOLID_FOREGROUND))
    (when num_format
      (if (number? num_format)
        (.setDataFormat st (short (long num_format)))
        (.setDataFormat st (.getFormat (.createDataFormat wb) (str num_format)))))
    (when align (.setAlignment st (h-align align)))
    (when valign (.setVerticalAlignment st (v-align valign)))
    (when text_wrap (.setWrapText st true))
    (when border
      (let [bs (border-style border)]
        (.setBorderTop st bs)
        (.setBorderBottom st bs)
        (.setBorderLeft st bs)
        (.setBorderRight st bs)))
    st))

(defn- ensure-cell
  ^Cell [^XSSFWorkbook wb sheet row col]
  (let
    [sh
     (.getSheetAt wb (int sheet))

     r
     (or (.getRow sh (int row)) (.createRow sh (int row)))]

    (or (.getCell r (int col)) (.createCell r (int col)))))

(defn- style-of [styles fmt] (when (and fmt (>= (long fmt) 0)) (nth @styles (long fmt))))

(defn- set-datetime!
  [^Cell c ^String iso]
  (cond (re-matches #"\d{4}-\d{2}-\d{2}" iso) (.setCellValue c (java.time.LocalDate/parse iso))
        (str/includes? iso "T") (.setCellValue c (java.time.LocalDateTime/parse iso))
        :else (.setCellValue c iso)))

;; Host operations (Apache POI XSSF).

(defn- op-new
  []
  (let
    [wb
     (XSSFWorkbook.)

     h
     (swap! wb-counter inc)]

    (swap! wb-registry assoc h {:wb wb :styles (atom [])})
    h))

(defn- op-add-sheet
  [wb-h name]
  (let
    [{:keys [^XSSFWorkbook wb]}
     (entry-of wb-h)

     nm
     (if (and name (seq (str name)))
       (WorkbookUtil/createSafeSheetName (str name))
       (str "Sheet" (inc (.getNumberOfSheets wb))))

     sh
     (.createSheet wb nm)]

    {"index" (.getSheetIndex wb sh) "name" (.getSheetName sh)}))

(defn- op-add-format
  [wb-h props]
  (let [{:keys [^XSSFWorkbook wb styles]} (entry-of wb-h)]
    (swap! styles conj (build-style wb props))
    (dec (count @styles))))

(defn- op-write
  [wb-h sheet row col kind value fmt]
  (let
    [{:keys [wb styles]}
     (entry-of wb-h)

     c
     (ensure-cell wb sheet row col)]

    (case (str kind)
      "string"
      (.setCellValue c (str value))

      "number"
      (.setCellValue c (double value))

      "boolean"
      (.setCellValue c (boolean value))

      "formula"
      (.setCellFormula c
                       (let [f (str value)]
                         (if (str/starts-with? f "=") (subs f 1) f)))

      "datetime"
      (set-datetime! c (str value))

      "blank"
      nil)
    (when-let [st (style-of styles fmt)]
      (.setCellStyle c ^XSSFCellStyle st))
    nil))

(defn- op-url
  [wb-h sheet row col url string tip fmt]
  (let
    [{:keys [^XSSFWorkbook wb styles]}
     (entry-of wb-h)

     c
     (ensure-cell wb sheet row col)

     link
     (.createHyperlink (.getCreationHelper wb) HyperlinkType/URL)]

    (.setAddress link (str url))
    (.setHyperlink c link)
    (.setCellValue c (str (if (and string (seq (str string))) string url)))
    (when (and tip (seq (str tip))) (.setLabel link (str tip)))
    (when-let [st (style-of styles fmt)]
      (.setCellStyle c ^XSSFCellStyle st))
    nil))

(defn- op-merge
  [wb-h sheet r1 c1 r2 c2 kind value fmt]
  (let
    [{:keys [wb styles]}
     (entry-of wb-h)

     sh
     (.getSheetAt ^XSSFWorkbook wb (int sheet))]

    (.addMergedRegion sh (CellRangeAddress. (int r1) (int r2) (int c1) (int c2)))
    (op-write wb-h sheet r1 c1 kind value fmt)
    (when-let [st (style-of styles fmt)]
      (doseq
        [rr (range r1 (inc (long r2)))
         cc (range c1 (inc (long c2)))]

        (.setCellStyle (ensure-cell wb sheet rr cc) ^XSSFCellStyle st)))
    nil))

(defn- op-set-column
  [wb-h sheet first-col last-col width fmt hidden]
  (let
    [{:keys [^XSSFWorkbook wb styles]}
     (entry-of wb-h)

     sh
     (.getSheetAt wb (int sheet))]

    (doseq [col (range first-col (inc (long last-col)))]
      (when width (.setColumnWidth sh (int col) (int (Math/round (* 256.0 (double width))))))
      (when-let [st (style-of styles fmt)]
        (.setDefaultColumnStyle sh (int col) ^XSSFCellStyle st))
      (when hidden (.setColumnHidden sh (int col) true)))
    nil))

(defn- op-set-row
  [wb-h sheet row height fmt hidden]
  (let
    [{:keys [^XSSFWorkbook wb styles]}
     (entry-of wb-h)

     sh
     (.getSheetAt wb (int sheet))

     r
     (or (.getRow sh (int row)) (.createRow sh (int row)))]

    (when height (.setHeightInPoints r (float height)))
    (when-let [st (style-of styles fmt)]
      (.setRowStyle r ^XSSFCellStyle st))
    (when hidden (.setZeroHeight r true))
    nil))

(defn- op-close
  [wb-h]
  (let
    [{:keys [^XSSFWorkbook wb]}
     (entry-of wb-h)

     bos
     (ByteArrayOutputStream.)]

    (.write wb bos)
    (.close wb)
    (swap! wb-registry dissoc (long wb-h))
    (b64enc (.toByteArray bos))))

(defn- envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- xlsxwriter-bridge-bindings
  "Host callables (Apache POI XSSF) the xlsxwriter shim delegates to."
  []
  {"__vis_xlsx_new__" (fn []
                        (envelope op-new))
   "__vis_xlsx_add_sheet__" (fn [wb name]
                              (envelope #(op-add-sheet wb name)))
   "__vis_xlsx_add_format__" (fn [wb props]
                               (envelope #(op-add-format wb props)))
   "__vis_xlsx_write__" (fn [wb sheet row col kind value fmt]
                          (envelope #(op-write wb sheet row col kind value fmt)))
   "__vis_xlsx_url__" (fn [wb sheet row col url string tip fmt]
                        (envelope #(op-url wb sheet row col url string tip fmt)))
   "__vis_xlsx_merge__" (fn [wb sheet r1 c1 r2 c2 kind value fmt]
                          (envelope #(op-merge wb sheet r1 c1 r2 c2 kind value fmt)))
   "__vis_xlsx_set_column__" (fn [wb sheet f l width fmt hidden]
                               (envelope #(op-set-column wb sheet f l width fmt hidden)))
   "__vis_xlsx_set_row__" (fn [wb sheet row height fmt hidden]
                            (envelope #(op-set-row wb sheet row height fmt hidden)))
   "__vis_xlsx_close__" (fn [wb]
                          (envelope #(op-close wb)))})

;; Python preamble: publishes an xlsxwriter-compatible module into sys.modules.


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-xlsxwriter"
     :ext/description
     "Sandbox shim: an xlsxwriter-compatible module (Workbook/add_worksheet/add_format/write/write_string/number/formula/datetime/url/row/column, merge_range, set_column/set_row, A1 notation, utility helpers) backed by pure-Java Apache POI XSSF. GraalPy can't install the CPython package; this makes `import xlsxwriter` produce real .xlsx files. No pip, no native wheel, no host binary."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "xlsxwriter"
       :shim/imports ["xlsxwriter"]
       :shim/description
       "xlsxwriter-compatible .xlsx writer backed by pure-Java Apache POI (workbooks/styles by integer handle; file returns as base64 on close). Not supported: streaming (`constant_memory`), VBA, and some advanced chart/formatting options."
       :shim/bindings xlsxwriter-bridge-bindings
       :shim/source "vis-shims/xlsxwriter.py"}]}))

(vis/register-extension! vis-extension)
