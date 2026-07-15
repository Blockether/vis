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
  (let [low
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
  (let [st
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
  (let [sh
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
  (let [wb
        (XSSFWorkbook.)

        h
        (swap! wb-counter inc)]

    (swap! wb-registry assoc h {:wb wb :styles (atom [])})
    h))

(defn- op-add-sheet
  [wb-h name]
  (let [{:keys [^XSSFWorkbook wb]}
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
  (let [{:keys [wb styles]}
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
  (let [{:keys [^XSSFWorkbook wb styles]}
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
  (let [{:keys [wb styles]}
        (entry-of wb-h)

        sh
        (.getSheetAt ^XSSFWorkbook wb (int sheet))]

    (.addMergedRegion sh (CellRangeAddress. (int r1) (int r2) (int c1) (int c2)))
    (op-write wb-h sheet r1 c1 kind value fmt)
    (when-let [st (style-of styles fmt)]
      (doseq [rr (range r1 (inc r2))
              cc (range c1 (inc c2))]

        (.setCellStyle (ensure-cell wb sheet rr cc) ^XSSFCellStyle st)))
    nil))

(defn- op-set-column
  [wb-h sheet first-col last-col width fmt hidden]
  (let [{:keys [^XSSFWorkbook wb styles]}
        (entry-of wb-h)

        sh
        (.getSheetAt wb (int sheet))]

    (doseq [col (range first-col (inc last-col))]
      (when width (.setColumnWidth sh (int col) (int (Math/round (* 256.0 (double width))))))
      (when-let [st (style-of styles fmt)]
        (.setDefaultColumnStyle sh (int col) ^XSSFCellStyle st))
      (when hidden (.setColumnHidden sh (int col) true)))
    nil))

(defn- op-set-row
  [wb-h sheet row height fmt hidden]
  (let [{:keys [^XSSFWorkbook wb styles]}
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
  (let [{:keys [^XSSFWorkbook wb]}
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

(def ^:private xlsxwriter-shim-src
  "def __vis_install_xlsxwriter__():
    import sys, types, base64, datetime
    _bi = sys.modules['builtins']
    _new = __vis_xlsx_new__
    _add_sheet = __vis_xlsx_add_sheet__
    _add_format = __vis_xlsx_add_format__
    _write = __vis_xlsx_write__
    _url = __vis_xlsx_url__
    _merge = __vis_xlsx_merge__
    _set_col = __vis_xlsx_set_column__
    _set_row = __vis_xlsx_set_row__
    _close = __vis_xlsx_close__

    class XlsxWriterException(Exception):
        pass

    def _raise(ok, msg):
        if not ok:
            raise XlsxWriterException(str(msg))

    def _cell_to_rowcol(cell):
        cell = cell.upper().replace('$', '')
        i = 0
        col = 0
        while i < len(cell) and cell[i].isalpha():
            col = col * 26 + (ord(cell[i]) - ord('A') + 1)
            i += 1
        row = int(cell[i:]) if i < len(cell) else 1
        return (row - 1, col - 1)

    def _looks_cell(s):
        if not s or not s[0].isalpha():
            return False
        seen = False
        for ch in s.replace('$', ''):
            if ch.isalpha():
                if seen:
                    return False
            elif ch.isdigit():
                seen = True
            else:
                return False
        return seen

    def _detect(data):
        if data is None:
            return ('blank', '')
        if isinstance(data, bool):
            return ('boolean', data)
        if isinstance(data, (int, float)):
            return ('number', float(data))
        if isinstance(data, (datetime.datetime, datetime.date, datetime.time)):
            return ('datetime', data.isoformat())
        s = str(data)
        if s.startswith('='):
            return ('formula', s)
        return ('string', s)

    class Format:
        def __init__(self, props=None):
            self._props = dict(props or {})
            self._id = None

        def set_properties(self, props):
            self._props.update(props or {})
            self._id = None

        def __getattr__(self, name):
            if name.startswith('set_'):
                key = name[4:]

                def setter(value=True):
                    self._props[key] = value
                    self._id = None
                return setter
            raise AttributeError(name)

    class Worksheet:
        def __init__(self, wb, index, name):
            self._wb = wb
            self.index = index
            self.name = name

        def _rc(self, args):
            args = list(args)
            if args and isinstance(args[0], str) and _looks_cell(args[0]):
                r, c = _cell_to_rowcol(args[0])
                return r, c, args[1:]
            return args[0], args[1], args[2:]

        def _put(self, r, c, kind, val, fmt):
            ok, res = _write(self._wb._h, self.index, r, c, kind, val, self._wb._fmt_id(fmt))
            _raise(ok, res)
            return 0

        def write(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else None
            fmt = rest[1] if len(rest) > 1 else None
            kind, val = _detect(data)
            if kind == 'string' and (val.startswith('http://') or val.startswith('https://') or val.startswith('mailto:')):
                return self.write_url(r, c, val, fmt)
            return self._put(r, c, kind, val, fmt)

        def write_string(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'string', str(rest[0]) if rest else '', rest[1] if len(rest) > 1 else None)

        def write_number(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'number', float(rest[0]), rest[1] if len(rest) > 1 else None)

        def write_boolean(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'boolean', bool(rest[0]), rest[1] if len(rest) > 1 else None)

        def write_formula(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'formula', str(rest[0]), rest[1] if len(rest) > 1 else None)

        def write_datetime(self, *args):
            r, c, rest = self._rc(args)
            v = rest[0]
            iso = v.isoformat() if hasattr(v, 'isoformat') else str(v)
            return self._put(r, c, 'datetime', iso, rest[1] if len(rest) > 1 else None)

        def write_blank(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'blank', '', rest[1] if len(rest) > 1 else None)

        def write_url(self, *args):
            r, c, rest = self._rc(args)
            url = str(rest[0])
            fmt = rest[1] if len(rest) > 1 else None
            string = rest[2] if len(rest) > 2 else None
            tip = rest[3] if len(rest) > 3 else None
            ok, res = _url(self._wb._h, self.index, r, c, url, string, tip, self._wb._fmt_id(fmt))
            _raise(ok, res)
            return 0

        def write_row(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else []
            fmt = rest[1] if len(rest) > 1 else None
            for i, v in enumerate(data):
                self.write(r, c + i, v, fmt)
            return 0

        def write_column(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else []
            fmt = rest[1] if len(rest) > 1 else None
            for i, v in enumerate(data):
                self.write(r + i, c, v, fmt)
            return 0

        def merge_range(self, *args):
            args = list(args)
            if args and isinstance(args[0], str):
                a, b = args[0].split(':')
                r1, c1 = _cell_to_rowcol(a)
                r2, c2 = _cell_to_rowcol(b)
                rest = args[1:]
            else:
                r1, c1, r2, c2 = args[0], args[1], args[2], args[3]
                rest = args[4:]
            data = rest[0] if rest else None
            fmt = rest[1] if len(rest) > 1 else None
            kind, val = _detect(data)
            ok, res = _merge(self._wb._h, self.index, r1, c1, r2, c2, kind, val, self._wb._fmt_id(fmt))
            _raise(ok, res)
            return 0

        def set_column(self, *args):
            args = list(args)
            if args and isinstance(args[0], str):
                a, b = (args[0].split(':') + [args[0]])[:2]
                first_col = _cell_to_rowcol(a + '1')[1]
                last_col = _cell_to_rowcol(b + '1')[1]
                rest = args[1:]
            else:
                first_col, last_col = args[0], args[1]
                rest = args[2:]
            width = rest[0] if rest else None
            cell_format = rest[1] if len(rest) > 1 else None
            options = rest[2] if len(rest) > 2 else None
            hidden = bool(options.get('hidden')) if options else False
            ok, res = _set_col(self._wb._h, self.index, first_col, last_col, width, self._wb._fmt_id(cell_format), hidden)
            _raise(ok, res)
            return 0

        def set_row(self, row, height=None, cell_format=None, options=None):
            hidden = bool(options.get('hidden')) if options else False
            ok, res = _set_row(self._wb._h, self.index, row, height, self._wb._fmt_id(cell_format), hidden)
            _raise(ok, res)
            return 0

        def set_default_row(self, *a, **k):
            return 0

        def freeze_panes(self, *a, **k):
            return 0

        def autofit(self, *a, **k):
            return 0

        def activate(self):
            return 0

    class Workbook:
        def __init__(self, filename=None, options=None):
            ok, h = _new()
            _raise(ok, h)
            self._h = h
            self.filename = filename
            self._closed = False
            self.worksheets_objs = []
            self.data = None

        def add_worksheet(self, name=None):
            ok, res = _add_sheet(self._h, name)
            _raise(ok, res)
            ws = Worksheet(self, res['index'], res['name'])
            self.worksheets_objs.append(ws)
            return ws

        def add_format(self, properties=None):
            return Format(properties)

        def _fmt_id(self, fmt):
            if fmt is None:
                return -1
            if fmt._id is None:
                ok, fid = _add_format(self._h, fmt._props)
                _raise(ok, fid)
                fmt._id = fid
            return fmt._id

        def worksheets(self):
            return list(self.worksheets_objs)

        def get_worksheet_by_name(self, name):
            for ws in self.worksheets_objs:
                if ws.name == name:
                    return ws
            return None

        def define_name(self, *a, **k):
            return 0

        def set_properties(self, *a, **k):
            return 0

        def close(self):
            if self._closed:
                return
            ok, b64 = _close(self._h)
            _raise(ok, b64)
            self._closed = True
            data = base64.b64decode(b64)
            self.data = data
            if self.filename is not None:
                if hasattr(self.filename, 'write'):
                    self.filename.write(data)
                else:
                    with open(self.filename, 'wb') as f:
                        f.write(data)

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    mod = types.ModuleType('xlsxwriter')
    mod.Workbook = Workbook
    mod.Worksheet = Worksheet
    mod.Format = Format
    mod.XlsxWriterException = XlsxWriterException
    mod.__version__ = '3.2.9'

    _wbmod = types.ModuleType('xlsxwriter.workbook')
    _wbmod.Workbook = Workbook
    mod.workbook = _wbmod
    _wsmod = types.ModuleType('xlsxwriter.worksheet')
    _wsmod.Worksheet = Worksheet
    mod.worksheet = _wsmod
    _fmtmod = types.ModuleType('xlsxwriter.format')
    _fmtmod.Format = Format
    mod.format = _fmtmod
    _exc = types.ModuleType('xlsxwriter.exceptions')
    _exc.XlsxWriterException = XlsxWriterException
    mod.exceptions = _exc

    def _col_to_name(col):
        name = ''
        col += 1
        while col > 0:
            col, rem = divmod(col - 1, 26)
            name = chr(65 + rem) + name
        return name

    def xl_rowcol_to_cell(row, col, row_abs=False, col_abs=False):
        return ('$' if col_abs else '') + _col_to_name(col) + ('$' if row_abs else '') + str(row + 1)

    _util = types.ModuleType('xlsxwriter.utility')
    _util.xl_cell_to_rowcol = _cell_to_rowcol
    _util.xl_rowcol_to_cell = xl_rowcol_to_cell
    _util.xl_col_to_name = _col_to_name
    mod.utility = _util

    sys.modules['xlsxwriter'] = mod
    for _sub in ('workbook', 'worksheet', 'format', 'exceptions', 'utility'):
        sys.modules['xlsxwriter.' + _sub] = getattr(mod, _sub)
    try:
        _bi.xlsxwriter = mod
    except Exception:
        pass

__vis_install_xlsxwriter__()
del __vis_install_xlsxwriter__
")

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
       :shim/description
       "xlsxwriter-compatible .xlsx writer backed by pure-Java Apache POI (workbooks + styles live host-side by integer handle; the file crosses back as base64 on close)."
       :shim/bindings xlsxwriter-bridge-bindings
       :shim/preamble xlsxwriter-shim-src}]}))

(vis/register-extension! vis-extension)
