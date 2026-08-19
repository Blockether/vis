(ns com.blockether.vis.internal.pptx-compat-shim-test
  "The python-pptx-compatible shim installed into every sandbox context. Its
   ordinary Python object model is serialized once through the lock-free Rust
   OOXML writer in com.blockether/imaging; opening an existing deck uses the
   matching Rust reader. These tests exercise utility types, enums, chart data,
   real package generation, and an in-memory create/open/edit/save round-trip."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (tpc/shared)]
     ~@body))

(defdescribe
  pptx-module-test
  (it "publishes pptx + submodules and EMU util helpers / RGBColor"
      (with-python-context
        (expect
          (= [true true 914400 914400 360000 "FF0000"]
             (ev python-context
                 (str "import sys\n"
                      "from pptx import Presentation\n"
                      "from pptx.util import Inches, Pt, Cm, Emu\n"
                      "from pptx.dml.color import RGBColor\n"
                      "[sys.modules['pptx'].Presentation is Presentation,\n"
                      " sys.modules['pptx.util'].Inches is Inches,\n"
                      " int(Inches(1)), int(Pt(72)), int(Cm(1)), str(RGBColor(0xFF, 0, 0))]")))))))

(defdescribe
  pptx-roundtrip-test
  (it
    "writes a real .pptx that re-opens as valid OOXML with the expected text"
    (with-python-context
      (expect
        (=
          [true true true true true true true true true true 9144000 6858000]
          (ev
            python-context
            (str
              "from pptx import Presentation\n"
              "from pptx.util import Inches, Pt\n" "from pptx.dml.color import RGBColor\n"
              "from pptx.enum.text import PP_ALIGN\n" "from pptx.enum.shapes import MSO_SHAPE\n"
              "import io, zipfile\n" "prs = Presentation()\n"
              "slide = prs.slides.add_slide(prs.slide_layouts[0])\n"
              "slide.shapes.title.text = 'Hello Vis'\n"
              "tb = slide.shapes.add_textbox(Inches(1), Inches(2), Inches(5), Inches(1))\n"
              "tf = tb.text_frame\n"
              "tf.text = 'First line'\n" "p = tf.add_paragraph()\n"
              "p.text = 'Second'\n" "p.font.bold = True\n"
              "p.font.size = Pt(24)\n" "p.alignment = PP_ALIGN.CENTER\n"
              "tf.paragraphs[0].runs[0].font.color.rgb = RGBColor(0xFF, 0, 0)\n"
              "sh = slide.shapes.add_shape(MSO_SHAPE.ROUNDED_RECTANGLE, Inches(1), Inches(4), Inches(2), Inches(1))\n"
              "sh.text = 'In shape'\n" "sh.fill.solid()\n"
              "sh.fill.fore_color.rgb = RGBColor(0, 0x80, 0xFF)\n"
              "etb = slide.shapes.add_textbox(Inches(1), Inches(6), Inches(2), Inches(1))\n"
              "try:\n" "    _ = etb.fill.fore_color\n"
              "    fill_raises = False\n" "except TypeError:\n"
              "    fill_raises = True\n" "buf = io.BytesIO()\n"
              "prs.save(buf)\n" "data = buf.getvalue()\n"
              "z = zipfile.ZipFile(io.BytesIO(data))\n" "names = set(z.namelist())\n"
              "slidexml = z.read('ppt/slides/slide1.xml').decode('utf-8', 'ignore')\n"
              "[slide.shapes.title.is_placeholder,\n"
              " '<p:ph' in slidexml,\n" " data[:2] == b'PK',\n"
              " 'ppt/presentation.xml' in names,\n"
              " 'Hello Vis' in slidexml and 'First line' in slidexml and 'In shape' in slidexml,\n"
              " 'Rounded Rectangle 4' in slidexml,\n" " 'TextBox 3' in slidexml,\n"
              " '<a:defRPr' in slidexml,\n" " fill_raises,\n"
              " len(prs.slides) == 1,\n" " prs.slide_width, prs.slide_height]")))))))

(defdescribe pptx-package-submodule-test
             (it "imports chart data using the standard python-pptx package path"
                 (with-python-context
                   (expect (= [["one"] [["series" [1 2]]]]
                              (ev python-context
                                  (str "from pptx.chart.data import CategoryChartData\n"
                                       "data = CategoryChartData()\n"
                                       "data.add_category('one')\n"
                                       "data.add_series('series', [1,2])\n"
                                       "[list(data.categories),\n"
                                       " [[s.name, list(s.values)] for s in data.series]]")))))))

;; Creating a deck, saving it, and re-opening it — the shared prelude of the
;; re-open assertions below. Ends with `opened` bound to the re-read deck.
(def ^:private create-and-reopen-py
  (str
    "import io\n"
    "from pptx import Presentation\n" "from pptx.chart.data import CategoryChartData\n"
    "from pptx.enum.chart import XL_CHART_TYPE\n" "from pptx.util import Inches\n"
    "prs = Presentation()\n" "s = prs.slides.add_slide(prs.slide_layouts[6])\n"
    "s.shapes.add_textbox(Inches(1), Inches(1), Inches(3), Inches(1)).text = 'Hello again'\n"
    "table = s.shapes.add_table(2, 2, Inches(1), Inches(2), Inches(3), Inches(1)).table\n"
    "table.cell(0, 0).text = 'A'\n" "table.cell(1, 1).text = 'B'\n"
    "cd = CategoryChartData()\n" "cd.categories = ['Q1', 'Q2']\n"
    "cd.add_series('Sales', [3, 7])\n"
    "s.shapes.add_chart(XL_CHART_TYPE.COLUMN_CLUSTERED, Inches(4), Inches(1), Inches(5), Inches(3), cd)\n"
    "s.notes_slide.notes_text_frame.text = 'Speaker notes'\n" "b1 = io.BytesIO()\n"
    "prs.save(b1)\n" "opened = Presentation(io.BytesIO(b1.getvalue()))\n"))

(defdescribe
  pptx-open-edit-save-test
  (it
    "re-opens Rust OOXML into editable python-pptx shapes, tables, charts and notes"
    (with-python-context
      (expect
        (=
          ;; `repr` keeps the float-ness of plotted values visible: GraalPy → Clojure
          ;; conversion narrows an integral double back to a long.
          ["Hello again" "A" "B" "Q1" "Q2" "[3.0, 7.0]" "Speaker notes" "Edited" true]
          (ev
            python-context
            (str
              create-and-reopen-py
              "opened.slides[0].shapes[0].text = 'Edited'\n"
              "b2 = io.BytesIO()\n" "opened.save(b2)\n"
              "again = Presentation(io.BytesIO(b2.getvalue()))\n"
              "shapes = again.slides[0].shapes\n"
              "tbl = next(x.table for x in shapes if x.has_table)\n"
              "chart = next(x.chart for x in shapes if x.has_chart)\n"
              "['Hello again', tbl.cell(0, 0).text, tbl.cell(1, 1).text,\n"
              " chart.plots[0].categories[0], chart.plots[0].categories[1],\n"
              " repr(list(chart.series[0].values)), again.slides[0].notes_slide.notes_text_frame.text,\n"
              " shapes[0].text, b2.getvalue()[:2] == b'PK']")))))))

;; A 1x1 PNG — the smallest thing the Rust writer will accept as a picture part.
(def ^:private png-b64
  (str "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJ"
       "AAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="))

(defdescribe
  pptx-picture-crop-test
  (it
    "keeps picture crop fractions and the embedded image across save/re-open"
    (with-python-context
      (expect
        (=
          ;; crop-left/top/right/bottom, image bytes, `<a:srcRect>` in the slide
          ;; part, and the media part the picture points at.
          [0.25 0.125 0.0625 0.5 70 true ["ppt/media/image1.png"]]
          (ev
            python-context
            (str
              "import io, base64, zipfile\n"
              "from pptx import Presentation\n"
              "from pptx.util import Inches\n"
              "png = base64.b64decode('"
              png-b64
              "')\n"
              "prs = Presentation()\n" "s = prs.slides.add_slide(prs.slide_layouts[6])\n"
              "pic = s.shapes.add_picture(io.BytesIO(png), Inches(1), Inches(1), Inches(2), Inches(2))\n"
              "pic.crop_left, pic.crop_top = 0.25, 0.125\n"
              "pic.crop_right, pic.crop_bottom = 0.0625, 0.5\n" "b = io.BytesIO()\n"
              "prs.save(b)\n" "raw = b.getvalue()\n"
              "again = Presentation(io.BytesIO(raw))\n"
              "p2 = next(x for x in again.slides[0].shapes if hasattr(x, 'crop_left'))\n"
              "z = zipfile.ZipFile(io.BytesIO(raw))\n"
              "slidexml = z.read('ppt/slides/slide1.xml').decode('utf-8', 'ignore')\n"
              "[round(p2.crop_left, 4), round(p2.crop_top, 4),\n"
              " round(p2.crop_right, 4), round(p2.crop_bottom, 4),\n"
              " len(p2.image.blob), '<a:srcRect' in slidexml,\n"
              " sorted(n for n in z.namelist() if 'media' in n)]")))))))

(defdescribe
  pptx-chart-part-test
  (it
    "writes a real ppt/charts/chartN.xml part, not a picture of a chart"
    (with-python-context
      (expect
        (=
          [["ppt/charts/chart1.xml"] true true true true]
          (ev
            python-context
            (str
              "import io, zipfile\n"
              "from pptx import Presentation\n" "from pptx.chart.data import CategoryChartData\n"
              "from pptx.enum.chart import XL_CHART_TYPE\n" "from pptx.util import Inches\n"
              "prs = Presentation()\n" "s = prs.slides.add_slide(prs.slide_layouts[6])\n"
              "cd = CategoryChartData()\n" "cd.categories = ['Q1', 'Q2']\n"
              "cd.add_series('Sales', [3, 7])\n"
              "s.shapes.add_chart(XL_CHART_TYPE.COLUMN_CLUSTERED, Inches(4), Inches(1), Inches(5), Inches(3), cd)\n"
              "b = io.BytesIO()\n" "prs.save(b)\n"
              "raw = b.getvalue()\n" "z = zipfile.ZipFile(io.BytesIO(raw))\n"
              "cx = z.read('ppt/charts/chart1.xml').decode('utf-8', 'ignore')\n"
              "[sorted(n for n in z.namelist() if n.startswith('ppt/charts/')),\n"
              " '<c:barChart' in cx, 'Sales' in cx, 'Q1' in cx,\n"
              " 'ppt/charts/chart1.xml' in z.read('[Content_Types].xml').decode('utf-8', 'ignore')]")))))))

(defdescribe
  pptx-shape-format-roundtrip-test
  (it
    "keeps solid fill, outline colour/width/dash and a no-fill shape on re-open"
    (with-python-context
      (expect
        ;; The Rust reader hydrates `<a:solidFill>`/`<a:ln>` back into the same
        ;; fill and line specs the writer emits, so an edit-in-place deck does not
        ;; silently lose the formatting it came in with.
        (=
          ["solid" "009933" "112233" 2.25 "dash" "none"]
          (ev
            python-context
            (str
              "import io\n" "from pptx import Presentation\n"
              "from pptx.util import Inches, Pt\n" "from pptx.dml.color import RGBColor\n"
              "from pptx.enum.shapes import MSO_SHAPE\n"
              "from pptx.enum.dml import MSO_LINE_DASH_STYLE\n"
              "prs = Presentation()\n" "s = prs.slides.add_slide(prs.slide_layouts[6])\n"
              "sh = s.shapes.add_shape(MSO_SHAPE.OVAL, Inches(1), Inches(1), Inches(3), Inches(2))\n"
              "sh.fill.solid()\n"
              "sh.fill.fore_color.rgb = RGBColor(0x00, 0x99, 0x33)\n"
              "sh.line.color.rgb = RGBColor(0x11, 0x22, 0x33)\n"
              "sh.line.width = Pt(2.25)\n" "sh.line.dash_style = MSO_LINE_DASH_STYLE.DASH\n"
              "bare = s.shapes.add_shape(MSO_SHAPE.RECTANGLE, Inches(1), Inches(4), Inches(2), Inches(1))\n"
              "bare.fill.background()\n"
              "b = io.BytesIO()\n" "prs.save(b)\n"
              "again = Presentation(io.BytesIO(b.getvalue()))\n"
              "x, y = again.slides[0].shapes[0], again.slides[0].shapes[1]\n"
              "[x.fill.type, str(x.fill.fore_color.rgb), str(x.line.color.rgb),\n"
              " x.line.width.pt, str(x.line.dash_style), y.fill.type]")))))))


;; Regression: a freshly created deck legally has NO slides, and the OOXML kind
;; sniffer classified a presentation only by `ppt/slides/slide*` — so saving an
;; empty deck and opening it back failed with "not a readable workbook: Cannot
;; detect file format" (fixed in com.blockether/imaging 0.1.10).
(defdescribe pptx-slideless-round-trip-test
             (it "re-opens a deck that has no slides yet, layouts intact"
                 (with-python-context
                   (expect (= [0 11 1]
                              (ev python-context
                                  (str "import io\n"
                                       "from pptx import Presentation\n" "prs = Presentation()\n"
                                       "b = io.BytesIO()\n" "prs.save(b)\n"
                                       "again = Presentation(io.BytesIO(b.getvalue()))\n"
                                       "empty = len(again.slides)\n"
                                       "again.slides.add_slide(again.slide_layouts[6])\n"
                                       "[empty, len(again.slide_layouts), len(again.slides)]")))))))
