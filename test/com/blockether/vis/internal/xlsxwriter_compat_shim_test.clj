(ns com.blockether.vis.internal.xlsxwriter-compat-shim-test
  "The xlsxwriter-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism: an `xlsxwriter` module published into
   `sys.modules`, backed by pure-Java Apache POI XSSF (GraalPy can't install the
   CPython package). Workbooks/formats live host-side by integer handle and the
   finished file crosses back as base64. These tests exercise the module
   surface, A1 notation helpers, and a real round-trip: a workbook built through
   the shim is re-opened as a zip and its OOXML parts / values are asserted."
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
  xlsxwriter-module-test
  (it "publishes xlsxwriter under sys.modules with submodules + utility helpers"
      (with-python-context
        (expect (= [true true true 0 3]
                   (ev python-context
                       (str "import sys, xlsxwriter\n"
                            "from xlsxwriter.utility import xl_cell_to_rowcol, xl_rowcol_to_cell\n"
                            "[xlsxwriter is sys.modules['xlsxwriter'],\n"
                            " sys.modules['xlsxwriter.workbook'].Workbook is xlsxwriter.Workbook,\n"
                            " xl_rowcol_to_cell(2, 3) == 'D3',\n"
                            " xl_cell_to_rowcol('A1')[0], xl_cell_to_rowcol('D1')[1]]")))))))

(defdescribe
  xlsxwriter-roundtrip-test
  (it
    "writes a real .xlsx that re-opens as valid OOXML with the expected values"
    (with-python-context
      (expect
        (=
          [true true true true true]
          (ev
            python-context
            (str
              "import xlsxwriter, io, zipfile\n" "buf = io.BytesIO()\n"
              "wb = xlsxwriter.Workbook(buf)\n" "ws = wb.add_worksheet('Data')\n"
              "f = wb.add_format({'bold': True, 'font_color': 'red', 'bg_color': '#FFFF00'})\n"
              "ws.write('A1', 'Name', f)\n"
              "ws.write(1, 0, 'Alice')\n" "ws.write_number(1, 1, 42.5)\n"
              "ws.write(2, 1, '=B2*2')\n" "ws.set_column('A:A', 20)\n"
              "ws.merge_range('A5:B5', 'Merged!', f)\n" "wb.close()\n"
              "data = buf.getvalue()\n" "z = zipfile.ZipFile(io.BytesIO(data))\n"
              "names = set(z.namelist())\n"
              "sheet = z.read('xl/worksheets/sheet1.xml').decode('utf-8', 'ignore')\n"
              "ss = ''.join(z.read(n).decode('utf-8', 'ignore') for n in names if 'sharedStrings' in n)\n"
              "[data[:2] == b'PK',\n"
              " 'xl/workbook.xml' in names,\n" " 'Alice' in ss and 'Merged!' in ss,\n"
              " 'B2*2' in sheet,\n" " len(data) > 1000]"))))))
  (it "returns bytes on close when no filename is given"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "import xlsxwriter\n"
                          "wb = xlsxwriter.Workbook()\n" "wb.add_worksheet().write(0, 0, 'x')\n"
                          "wb.close()\n"
                          "wb.data is not None and len(wb.data) > 0 and wb.data[:2] == b'PK'")))))))
