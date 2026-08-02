(ns com.blockether.vis.internal.pptx-compat-shim-test
  "The python-pptx-compatible shim installed into every sandbox context. Its
   ordinary Python object model is serialized once through the lock-free Rust
   OOXML writer in com.blockether/imaging; opening an existing deck uses the
   matching Rust reader. These tests exercise utility types, enums, chart data,
   real package generation, and an in-memory create/open/edit/save round-trip."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

;; A namespace-local context avoids paying GraalPy + shim bootstrap per assertion.
(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
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
