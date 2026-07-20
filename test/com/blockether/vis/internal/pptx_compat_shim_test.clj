(ns com.blockether.vis.internal.pptx-compat-shim-test
  "The pptx (python-pptx) compat shim installed into every sandbox context via
   the generic sandbox-shim mechanism: a `pptx` module published into
   `sys.modules`, backed by pure-Java Apache POI XSLF (GraalPy can't install the
   CPython package). Presentations/slides/shapes live host-side by integer
   handle and the finished file crosses back as base64. These tests exercise the
   util helpers + enums and a real round-trip: a deck built through the shim is
   re-opened as a zip and its OOXML parts / text are asserted."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

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
