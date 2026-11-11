(ns com.blockether.vis.internal.foundation.shim-attach-test
  "The generic attachment shim (`attach`) installed into
   every sandbox context via the sandbox-shim mechanism. A tool PRODUCES an
   artifact and hands it to `attach`; the bytes are captured AT THE SOURCE
   into the per-block sink (drained into the block outcome's `:attachments`, which the
   loop persists as `:attachments`), with the media-type sniffed from magic bytes
   / extension / a utf-8 probe. No stdout fence, no parsing."
  (:require [charred.api :as charred]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.foundation.shim-attach :as shim-attach]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private picture-doubles
  "Python defining the two SHAPES `attach` duck-types, with no pip: a FIGURE
   (anything carrying `savefig`) and a Pillow IMAGE (`save` + `mode` + `size` and
   no `savefig`). Both emit a REAL PNG, so the host sniffs the media type and
   reads the dimensions off actual pixels. matplotlib and Pillow themselves are
   ordinary wheels a session pip-installs; a unit test pins the shape `attach`
   accepts, never the download."
  (str "import struct, zlib\n"
       "def _png(w, h):\n"
       "    raw = b''.join(b'\\x00' + b'\\xff\\x00\\x00\\xff' * w for _ in range(h))\n"
       "    def chunk(tag, body):\n" "        c = tag + body\n"
       "        return (struct.pack('>I', len(body)) + c\n"
       "                + struct.pack('>I', zlib.crc32(c) & 0xffffffff))\n"
       "    return (b'\\x89PNG\\r\\n\\x1a\\n'\n"
       "            + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, 8, 6, 0, 0, 0))\n"
       "            + chunk(b'IDAT', zlib.compress(raw))\n" "            + chunk(b'IEND', b''))\n"
       "class Figure:\n" "    def __init__(self, w, h):\n"
       "        self.size = (w, h)\n" "    def savefig(self, target, format='png'):\n"
       "        target.write(_png(*self.size))\n" "class Picture:\n"
       "    def __init__(self, w, h):\n" "        self.mode = 'RGBA'\n"
       "        self.size = (w, h)\n" "    def convert(self, mode):\n"
       "        return self\n" "    def save(self, target, format='PNG'):\n"
       "        if str(format).upper() in ('JPEG', 'JPG'):\n"
       "            target.write(b'\\xff\\xd8\\xff\\xe0' + b'\\x00' * 32)\n"
       "        else:\n" "            target.write(_png(*self.size))\n"))

(defn- temp-root
  ^String []
  (str (.toAbsolutePath (Files/createTempDirectory "vis-attach-test"
                                                   (make-array FileAttribute 0)))))

(defn- ctx-with-root
  "A sandbox context whose filesystem is confined to a fresh temp root."
  [root]
  (:python-context (tpc/new-context {}
                                    (fn []
                                      [root])
                                    {:enabled? false :jail-enabled? true})))

(defn- block
  "Run `code` as ONE driven block and return the flat outcome (so the per-block
   image sink is bound and drained into `:attachments`)."
  [pctx code]
  (ep/run-python-block pctx code))

(defn- ev [c code] (tpc/ev c code))

(defdescribe
  attach-in-memory-capture-test
  (it "records an in-memory artifact and hands back its descriptor"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "d = attach(b'a,b\\n1,2\\n', 'notes.txt')\n"
                        "print(d['filename'], d['version'], d['kind'], d['media_type'],\n"
                        "      d['size'], d['is_pending'], d['source'], len(d['id']))\n"))

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        ;; The producer is handed the artifact's IDENTITY, not a summary it has to
        ;; go looking for: the id its stored row will carry, and the cut this
        ;; filename just became.
        (expect (re-find #"notes\.txt 1 file text/plain 8 True tool 36" (str (:stdout out))))
        (expect (= 1 (count (:attachments out))))
        (expect (string? (:id att)))
        (expect (= 1 (:version att)))
        (expect (= "text/plain" (:media-type att)))
        (expect (= "file" (:kind att)))
        (expect (= "notes.txt" (:filename att)))
        (expect (= 8 (:size att)))
        ;; base64 round-trips to the original bytes
        (expect (= "a,b\n1,2\n"
                   (String. (.decode (java.util.Base64/getDecoder) ^String (:base64 att)))))))
  (it "detects an image by magic bytes -> kind image, image/png"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "png = bytes([0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A])\n"
                        "png += (13).to_bytes(4, 'big') + b'IHDR' + bytes(13)\n"
                        "attach(png, 'fig.dat')\n"))

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it
    "emits a vis-image display fence (host temp path + dims) for a decodable image"
    (let
      [pctx
       (ctx-with-root (temp-root))

       ;; a real 1x1 PNG so the host image decode yields dimensions
       out
       (block
         pctx
         (str
           "import base64\n"
           "png = base64.b64decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==')\n"
           "attach(png, 'dot.png')\n"))

       so
       (str (:stdout out))]

      (expect (nil? (:error out)))
      ;; the fence a graphical TUI/web reads to paint the image inline
      (expect (re-find #"vis-image" so))
      (expect (re-find #"1x1" so))
      ;; the header carries a readable HOST path under the DURABLE display cache
      ;; (`~/.vis/cache/display`), not an OS temp dir the system sweeps — that is
      ;; what lets a resumed TUI history repaint the picture
      (expect (re-find #"\.vis/cache/display/att-" so))
      ;; and the bytes are still captured for the vision-replay path
      (expect (= "image/png" (:media-type (first (:attachments out)))))))
  (it "records a non-decodable image (svg) with NO display fence"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "attach(b'<svg/>', 'a.svg', media_type='image/svg+xml')\n")]

        (expect (nil? (:error out)))
        (expect (not (re-find #"vis-image" (str (:stdout out)))))
        (expect (= "image/svg+xml" (:media-type (first (:attachments out)))))))
  (it "falls back to text/plain for undecorated utf-8 bytes"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "attach(b'just words', 'note')\n")

            [att]
            (:attachments out)]

        (expect (= "text/plain" (:media-type att)))
        (expect (= "file" (:kind att)))))
  (it "honours explicit kind / media_type overrides"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "attach(b'x', 'weird.bin', kind='image', media_type='image/svg+xml')\n")

            [att]
            (:attachments out)]

        (expect (= "image/svg+xml" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it "collects MANY artifacts from one block, in order"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx (str "attach(b'1', 'a.txt')\n" "attach(b'2', 'b.json')\n"))]

        (expect (= ["a.txt" "b.json"] (mapv :filename (:attachments out))))))
  (it "renders and captures a figure-shaped source with a positional filename"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx (str picture-doubles "attach(Figure(7, 4), 'plot.png')\n"))

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "plot.png" (:filename att)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))))
  ;; Regression: attach(pil_image, 'crop.png') fell through to the PATH branch
  ;; and died with "attach: no such file: <PIL.Image.Image ...>", so a picture
  ;; cropped in the sandbox could not be attached without writing it out first.
  (it "encodes and captures a Pillow-shaped image handed straight to attach"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx (str picture-doubles "attach(Picture(12, 9), 'crop.png')\n"))

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "crop.png" (:filename att)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))
        ;; the pixels really rode along: the fence carries the image's own dims
        (expect (re-find #"12x9" (str (:stdout out))))))
  (it "lets the filename choose the encoder for an image, defaulting to PNG"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str picture-doubles
                        "img = Picture(6, 6)\n"
                        "attach(img, 'shot.jpg')\n"
                        "attach(img)\n"))

            [jpg png]
            (:attachments out)]

        (expect (nil? (:error out)))
        ;; a JPEG has no alpha channel, so an RGBA image is converted, not refused
        (expect (= ["shot.jpg" "image/jpeg"] [(:filename jpg) (:media-type jpg)]))
        (expect (= ["image.png" "image/png"] [(:filename png) (:media-type png)]))))
  (it "refuses a source that is neither a path nor a producer, by name"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "try:\n" "    attach({'rows': 1}, 'data.json')\n"
                        "except TypeError as e:\n" "    print('RAISED', e)\n"))]

        (expect (nil? (:error out)))
        (expect (empty? (:attachments out)))))
  (it
    "carries a label into the vis-image fence summary — the picture's caption row"
    (let
      [pctx
       (ctx-with-root (temp-root))

       out
       (block
         pctx
         (str
           "import base64\n"
           "png = base64.b64decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==')\n"
           "attach(png, 'dot.png', label='Scoped to studio: 89 matches')\n"))

       so
       (str (:stdout out))]

      (expect (nil? (:error out)))
      ;; The SUMMARY line is what the TUI paints as the caption above the image,
      ;; so a labeled series of shots says which shot is which.
      (expect (re-find #"\[Image: dot\.png 1×1, [^\]]*\] Scoped to studio: 89 matches" so))))
  (it "prints a caption line for a labeled artifact that has no inline fence"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "attach(b'hello there', 'notes.txt', label='fleet counts')\n")]

        (expect (nil? (:error out)))
        (expect (re-find #"\[Attached: notes\.txt\] fleet counts" (str (:stdout out))))))
  (it "attach takes the same label kwarg for a file on disk"
      (let [root
            (temp-root)

            pctx
            (ctx-with-root root)

            out
            (block pctx
                   (str "with open("
                        (pr-str (str root "/note.txt"))
                        ", 'w') as f:\n"
                        "    f.write('hello')\n"
                        "attach("
                        (pr-str (str root "/note.txt"))
                        ", label='the note')\n"))]

        (expect (nil? (:error out)))
        (expect (some #(= "note.txt" (:filename %)) (:attachments out)))
        (expect (re-find #"\[Attached: note\.txt\] the note" (str (:stdout out))))))
  (it "emits a vis-table fence for a CSV artifact so it can be viewed as a table"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "attach(b'a,b\\n1,2\\nx,3\\n', 'data.csv', label='fleet counts')\n")

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "text/csv" (:media-type att)))
        ;; Tabular data is DISPLAYABLE: the fence is what the TUI table viewer
        ;; and the companion `DataTable` parse, so the payload (header row plus
        ;; up to 500 data rows) travels inline under five header lines.
        (expect (= (str "````vis-table\n" "[Table: data.csv 2 rows × 2 cols, 12 B] fleet counts\n"
                        "data.csv\n" "text/csv\n"
                        "2x2\n" "12 B\n"
                        "a,b\n" "1,2\n"
                        "x,3\n" "````\n")
                   (str (:stdout out)))))))

(defdescribe
  attach-path-test
  (it "reads a confined file from disk and captures it"
      (let [root
            (temp-root)

            pctx
            (ctx-with-root root)

            out
            (block pctx
                   (str "with open('"
                        root
                        "/report.json','w') as f:\n"
                        "    f.write('{}')\n"
                        "attach('"
                        root
                        "/report.json')\n"))

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "report.json" (:filename att)))
        (expect (= "application/json" (:media-type att)))))
  (it "refuses a path outside the filesystem roots"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "try:\n"
                        "    attach('/etc/hosts')\n" "    print('NO-RAISE')\n"
                        "except Exception as e:\n" "    print('RAISED', type(e).__name__)\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"RAISED" (str (:stdout out))))
        (expect (empty? (:attachments out)))))
  (it "takes a pathlib.Path and a `~`-relative path, and names the file plainly when it is missing"
      (let [root
            (temp-root)

            pctx
            (ctx-with-root root)

            out
            (block pctx
                   (str "import os, pathlib\n"
                        "root = "
                        (pr-str root)
                        "\n"
                        "with open(root + '/report.json','w') as f:\n" "    f.write('{}')\n"
                        ;; The interpreter and its environment serve every test in this JVM.
                        "home = os.environ.get('HOME')\n" "os.environ['HOME'] = root\n"
                        "try:\n" "    attach(pathlib.Path(root + '/report.json'), 'pathlib.json')\n"
                        "    attach('~/report.json', 'tilde.json')\n" "    try:\n"
                        "        attach(root + '/nope.json')\n" "    except Exception as e:\n"
                        "        print('RAISED', e)\n" "finally:\n"
                        "    if home is None:\n" "        os.environ.pop('HOME', None)\n"
                        "    else:\n" "        os.environ['HOME'] = home\n"))]

        (expect (nil? (:error out)))
        (expect (= ["pathlib.json" "tilde.json"]
                   (filterv #{"pathlib.json" "tilde.json"} (mapv :filename (:attachments out)))))
        (expect (re-find #"RAISED attach: no such file: .*nope\.json" (str (:stdout out)))))))

(defdescribe
  attach-reference-test
  ;; #450: a file above the attachment limit stays on disk. attach() keeps a
  ;; reference to its path, the row carries no bytes, and the read verbs say where
  ;; the file is instead of loading it.
  (it
    "keeps a file above the limit as a reference to its path"
    (let [root
          (.getCanonicalPath (java.io.File. (temp-root)))

          file
          (java.io.File. root "talk.mp3")

          _
          (with-open [out (java.io.FileOutputStream. file)]
            (.write out
                    (byte-array (concat (map unchecked-byte [0x49 0x44 0x33 4 0 0 0 0 0 0])
                                        (repeat 990 (unchecked-byte 7))))))

          out
          (extension/with-context
            {:env {:session-id (random-uuid) :workspace {:root root}}}
            (binding [mpl-capture/*attachment-limit-bytes* 100]
              (block (ctx-with-root root)
                     (str "d = attach("
                          (pr-str (.getPath file))
                          ")\n"
                          "print('DESC', d['storage'], d['path'], d['size'], d['audience'],"
                          " d['media_type'])\n"
                          "try:\n" "    read_attachment('talk.mp3')\n"
                          "except Exception as e:\n" "    print('READ', e)\n"
                          "small = attach(b'abc', 'small.txt')\n"
                          "print('SMALL', small.get('storage'))\n"))))

          [reference small]
          (:attachments out)

          so
          (str (:stdout out))]

      (expect (nil? (:error out)))
      (expect (str/includes? so (str "DESC reference " (.getPath file) " 1000 user audio/mpeg")))
      (expect (= (str "vis-ref://" (.getPath file)) (:storage-uri reference)))
      (expect (nil? (:base64 reference)))
      (expect (= 1000 (:size reference)))
      (expect (re-find #"READ read_attachment: \"talk.mp3\" is a reference to .*talk\.mp3" so))
      (expect (str/includes? so "SMALL None"))
      (expect (some? (:base64 small)))))
  ;; A file has one media type: the reference of a large file and the stored bytes
  ;; of the same file agree, also when its first audio frame is longer than 4 kB.
  (it
    "names a file by the same rule as a reference and as bytes"
    (let [root
          (.getCanonicalPath (java.io.File. (temp-root)))

          write!
          (fn [filename data]
            (let [file (java.io.File. root (str filename))]
              (with-open [out (java.io.FileOutputStream. file)]
                (.write out (byte-array (map unchecked-byte data))))
              (.getPath file)))

          clip
          (write! "clip.dat"
                  (apply concat
                    (repeat 6 (concat [0xff 0xf1 0x4d 0xc2 0x32 0x9f 0xfc] (repeat 4493 0)))))

          log
          (write! "server.log"
                  (apply concat
                    (repeat 400 [0xff 0xe2 0xeb 0xe5 0xed 0xe8 0xe5 0x20 0x31 0x32 0x0a])))

          run
          (fn [limit]
            (extension/with-context
              {:env {:session-id (random-uuid) :workspace {:root root}}}
              (binding [mpl-capture/*attachment-limit-bytes* limit]
                (:stdout
                  (block
                    (ctx-with-root root)
                    (str "for p in ("
                         (pr-str clip)
                         ", "
                         (pr-str log)
                         "):\n"
                         "    d = attach(p)\n"
                         "    print('TYPE', d.get('storage') or 'bytes', d['media_type'])\n"))))))]

      (expect (= ["TYPE reference audio/aac" "TYPE reference application/octet-stream"]
                 (re-seq #"TYPE .*" (str (run 100)))))
      (expect (= ["TYPE bytes audio/aac" "TYPE bytes application/octet-stream"]
                 (re-seq #"TYPE .*" (str (run 1000000)))))))
  (it "refuses in-memory bytes above the limit, because no file can be referenced"
      (let [out (binding [mpl-capture/*attachment-limit-bytes* 100]
                  (block (ctx-with-root (temp-root))
                         (str "try:\n" "    attach(b'a' * 200, 'big.bin')\n"
                              "except Exception as e:\n" "    print('RAISED', e)\n")))]
        (expect (empty? (:attachments out)))
        (expect (re-find #"RAISED attach: payload 200 bytes exceeds the 100 bytes attachment limit"
                         (str (:stdout out)))))))

(defdescribe
  attach-office-container-test
  ;; #451: a workbook is a ZIP archive. A browser saves `application/zip` as
  ;; `name.xlsx.zip`, so the ZIP magic must not hide the Office type of the name.
  (it
    "names a ZIP-based Office file by its extension and keeps a plain archive a ZIP"
    (let [root
          (.getCanonicalPath (java.io.File. (temp-root)))

          pk
          (byte-array (concat (map unchecked-byte [0x50 0x4b 0x03 0x04]) (repeat 996 (byte 0))))

          _
          (doseq [n ["big.xlsx" "big.docx"]]
            (with-open [out (java.io.FileOutputStream. (java.io.File. root ^String n))]
              (.write out pk)))

          out
          (extension/with-context {:env {:session-id (random-uuid) :workspace {:root root}}}
                                  (binding [mpl-capture/*attachment-limit-bytes* 100]
                                    (block (ctx-with-root root)
                                           (str
                                             "pk = b'PK\\x03\\x04' + bytes(20)\n"
                                             "print('MT', attach(pk, 'small.xlsx')['media_type'])\n"
                                             "print('MT', attach(pk, 'bundle.zip')['media_type'])\n"
                                             "print('MT', attach("
                                             (pr-str (str root "/big.xlsx"))
                                             ")['media_type'])\n"
                                             "print('MT', attach("
                                             (pr-str (str root "/big.docx"))
                                             ")['media_type'])\n"))))

          so
          (str (:stdout out))]

      (expect (nil? (:error out)))
      (expect (= ["application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                  "application/zip"
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                  "application/vnd.openxmlformats-officedocument.wordprocessingml.document"]
                 (mapv second (re-seq #"MT (\S+)" so)))))))

(defdescribe
  attach-discovery-test
  (it "discovers declared capabilities instead of the internal shim identity"
      (with-redefs [extension/sandbox-shims (constantly [{:shim/name "internal-id"
                                                          :shim/imports ["actual_module"]
                                                          :shim/globals ["actual_global"]
                                                          :shim/source
                                                          "vis-shims-test/discovery.py"}])]
        (let [pctx (ctx-with-root (temp-root))]
          (expect (= ["actual_global" "actual_module"]
                     (vec (ev pctx "sorted([i.name for i in apropos('actual')][:2])"))))
          (expect (= 42 (ev pctx "import actual_module; actual_module.answer")))
          (expect (= 42 (ev pctx "actual_global()")))
          (expect (false? (ev pctx "'internal-id' in [i.name for i in apropos('internal')]")))
          (expect (true? (ev pctx "'Synthetic discovery contract.' in doc('actual_module')"))))))
  (it "raises when called with no active capture sink (outside a driven block)"
      (let [pctx (ctx-with-root (temp-root))]
        ;; a bare .eval does NOT bind the per-block sink, so the bridge refuses
        (expect (re-find #"attach: call inside python_execution"
                         (ev pctx
                             (str "\ntry:\n" "    attach(b'x', 'y.txt')\n"
                                  "    _r = 'NO-RAISE'\n" "except Exception as e:\n"
                                  "    _r = str(e)\n" "_r")))))))

(defdescribe explicit-attachment-only-test
             ;; Files written by Python are not attachments unless explicitly attached.
             (it "records NOTHING for a file the sandbox merely writes"
                 (let [root
                       (temp-root)

                       pctx
                       (ctx-with-root root)

                       ;; Ordinary writes under system temp are not collected either.
                       out
                       (block pctx
                              (str "with open('"
                                   root
                                   "/plain.txt', 'w') as f:\n"
                                   "    f.write('hi')\n"
                                   "print('ok')\n"))]

                   (expect (nil? (:error out)))
                   (expect (empty? (:attachments out)))))
             (it "gives the sandbox no outbox to write into: VIS_OUTBOX is undefined"
                 (let [pctx (ctx-with-root (temp-root))]
                   (expect (false? (ev pctx "'VIS_OUTBOX' in globals()")))
                   (expect (false? (ev pctx "import os; 'VIS_OUTBOX' in os.environ")))))
             (it "still records what a tool DELIBERATELY attaches in the same block"
                 (let [root
                       (temp-root)

                       pctx
                       (ctx-with-root root)

                       out
                       (block pctx
                              (str "with open('"
                                   root
                                   "/scratch.csv', 'w') as f:\n"
                                   "    f.write('a,b\\n1,2\\n')\n"
                                   "attach(b'a,b\\n1,2\\n', 'kept.csv')\n"))]

                   (expect (nil? (:error out)))
                   (expect (= 1 (count (:attachments out))))
                   (expect (= "kept.csv" (:filename (first (:attachments out))))))))

(defdescribe
  gather-conveys-attachment-sink-test
  "Regression (turn 28/29): an artifact produced by a tool running INSIDE
   `await gather(...)` is made on a POOL thread, not the block's, and every host
   door answers on the interpreter's own thread besides. The per-block
   `*attachment-sink*` must travel to whichever thread the door runs on
   (`python-host/conveying`) so `attach` still lands in the block's
   `:attachments` — no silent drop, no nil sink."
  (it "captures every gather-produced artifact into the block's :attachments"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "async def mk(name):\n" "    return attach(b'payload', name)\n"
                        "r = await gather(mk('a.txt'), mk('b.txt'), mk('c.txt'))\n"
                        "print(len(r))\n"))

            atts
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (re-find #"^3" (str (:stdout out))))
        ;; all three, produced on pool threads, reached the block's sink
        (expect (= 3 (count atts)))
        (expect (= #{"a.txt" "b.txt" "c.txt"} (set (map :filename atts)))))))

(defn- fake-reader
  "In-memory attachment reader holding ONE image artifact (id a1) — the shape
   `run-python-code` binds around a block so `list_attachments` /
   `read_attachment` can re-fetch prior session artifacts."
  []
  {:list (fn []
           [{:id "a1"
             :filename "chart.png"
             :media-type "image/png"
             :kind "image"
             :size 7
             :position 0
             :turn-id "turn-1"
             :tool-call-id "call-1"
             :iteration-id "it1"}])
   :read (fn [id]
           (when (= id "a1")
             {:id "a1"
              :base64 (.encodeToString (java.util.Base64/getEncoder) (.getBytes "PNGDATA"))
              :media-type "image/png"
              :filename "chart.png"
              :kind "image"
              :size 7
              :storage-uri nil}))
   :reinspect (fn [id]
                (when (= id "a1")
                  (let [a {:id "a1"
                           :base64 (.encodeToString (java.util.Base64/getEncoder)
                                                    (.getBytes "PNGDATA"))
                           :media-type "image/png"
                           :filename "chart.png"
                           :kind "image"
                           :size 7
                           :storage-uri nil}]
                    (mpl-capture/queue-reinspection! a)
                    a)))})

(defdescribe
  vis-attachments-reader-test
  "Read-back twins: with `*attachment-reader*` bound, `list_attachments()` lists the
   session's artifacts as descriptor DICTS (snake_case keys), `get_attachment` is
   the one descriptor and `read_attachment` hands back the raw BYTES and nothing
   else. All three take the SAME target — the filename the artifact was attached
   under, or an id out of a descriptor — so a caller never has to know which of
   the two it is holding. Unbound, they raise a catchable host error instead of
   silently returning nothing."
  (it "lists metadata and reads the bytes back"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (binding [mpl-capture/*attachment-reader* (fake-reader)]
              (block pctx
                     (str
                       "a = list_attachments()[0]\n" "d = get_attachment('a1')\n"
                       "r = read_attachment('a1')\n"
                       "print(a['id'], a['media_type'], a['tool_call_id'], a['iteration_id'],\n"
                       "      a['turn_id'])\n"
                       "print(type(r).__name__, r.decode('utf-8'), d['filename'], d['size'])\n")))

            so
            (str (:stdout out))]

        (expect (nil? (:error out)))
        (expect (re-find #"a1 image/png call-1 it1 turn-1" so))
        (expect (re-find #"bytes PNGDATA chart.png 7" so))))
  (it "addresses one artifact by its filename exactly as by its id"
      (let [pctx
            (ctx-with-root (temp-root))

            sink
            (atom [])

            out
            (binding [mpl-capture/*attachment-reader*
                      (fake-reader)

                      mpl-capture/*attachment-reinspection-sink*
                      sink]

              (block pctx
                     (str "print(read_attachment('chart.png').decode('utf-8'))\n"
                          "print(get_attachment('chart.png')['id'])\n"
                          "print(show_attachment('chart.png')['id'])\n"
                          "try:\n" "    read_attachment('nope.png')\n"
                          "except Exception as e:\n"
                          "    print('RAISED', 'id or filename' in str(e))\n")))

            so
            (str (:stdout out))]

        (expect (nil? (:error out)))
        ;; The name reaches the same bytes the id does — that IS the rule.
        (expect (re-find #"(?m)^PNGDATA$" so))
        ;; get_ and show_ resolve the name to the one stored cut, id a1.
        (expect (= 2 (count (re-seq #"(?m)^a1$" so))))
        (expect (= ["a1"] (mapv :id @sink)))
        ;; One vocabulary in the failure too: neither spelling exists.
        (expect (re-find #"RAISED True" so))))
  (it "shows a persisted image to the model for one request without duplicating it"
      (let [pctx
            (ctx-with-root (temp-root))

            sink
            (atom [])

            out
            (binding [mpl-capture/*attachment-reader*
                      (fake-reader)

                      mpl-capture/*attachment-reinspection-sink*
                      sink]

              (block pctx "r = show_attachment('a1')\nprint(r['id'], r['media_type'])"))]

        (expect (nil? (:error out)))
        (expect (re-find #"a1 image/png" (str (:stdout out))))
        (expect (= ["a1"] (mapv :id @sink)))))
  (it "coalesces repeated reinspection requests by durable attachment id"
      (let [sink
            (atom [])

            att
            {:id "a1" :media-type "image/png"}]

        (binding [mpl-capture/*attachment-reinspection-sink* sink]
          (mpl-capture/queue-reinspection! att)
          (mpl-capture/queue-reinspection! att))
        (expect (= ["a1"] (mapv :id @sink)))))
  (it "raises on an unknown id"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (binding [mpl-capture/*attachment-reader* (fake-reader)]
              (block pctx
                     (str "try:\n"
                          "    read_attachment('zzz')\n" "    print('NO-RAISE')\n"
                          "except Exception as e:\n"
                          "    print('RAISED', 'unknown id or filename' in str(e))\n")))]

        (expect (nil? (:error out)))
        (expect (re-find #"RAISED True" (str (:stdout out))))))
  ;; Regression, vis session f904a444: passing the local Path that produced an
  ;; image yielded only a generic missing-id error, forcing a documentation
  ;; round trip before the caller discovered the required attach(path) step.
  (it
    "gives a local image path the exact persistence remediation"
    (let [pctx
          (ctx-with-root (temp-root))

          out
          (binding [mpl-capture/*attachment-reader* (fake-reader)]
            (block pctx
                   (str
                     "try:\n"
                     "    show_attachment(Path('local-proof.png'))\n" "except Exception as e:\n"
                     "    print('HINT', 'show_attachment(attach(path))' in str(e))\n"
                     "print('DOC', 'show_attachment(attach(path))' in doc('show_attachment'))\n")))]

      (expect (nil? (:error out)))
      (expect (re-find #"HINT True" (str (:stdout out))))
      (expect (re-find #"DOC True" (str (:stdout out))))))
  (it "answers with the RUNNING block's own artifacts when no reader is bound"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "attach(b'solo', 'solo.txt')\n"
                        "print([r['filename'] for r in list_attachments()])\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"solo\.txt" (str (:stdout out))))))
  (it "raises where there is neither a reader nor a sink (outside a driven block)"
      (expect (= "list_attachments: call inside python_execution."
                 (try ((get (#'shim-attach/attach-bridge-bindings) "__vis_list_attachments__"))
                      (catch clojure.lang.ExceptionInfo e (ex-message e)))))))

(defn- attach-out
  "Run ONE `attach*` call in a fresh sandbox; the block result with `:row`
   bound to its single recorded attachment."
  [code]
  (let [out (block (ctx-with-root (temp-root)) code)]
    (assoc out :row (first (:attachments out)))))

(defdescribe
  attach-audience-test
  "`audience` is the ONE knob for WHO an artifact was attached FOR: \"both\" (the
   default) is painted for the human AND sent to the model, \"user\" is stored and
   painted but never becomes a wire image block (the opt-out for the one cost
   multimodal history cannot undo: an image RE-UPLOADED in full on every later
   request), \"model\" rides the request and is never painted."
  (it "defaults to audience \"both\" and paints the inline image fence"
      (let [out (attach-out "attach(b'PNGDATA', 'chart.png', media_type='image/png')\n")]
        (expect (nil? (:error out)))
        (expect (= 1 (count (:attachments out))))
        (expect (= "chart.png" (:filename (:row out))))
        (expect (= "both" (:audience (:row out))))))
  (it "stamps audience \"user\" so the send-time gate keeps the bytes off the wire"
      (let [out (attach-out (str "attach(b'PNGDATA', 'chart.png', media_type='image/png', "
                                 "audience='user')\n"))]
        (expect (nil? (:error out)))
        (expect (= "user" (:audience (:row out))))))
  (it "says NOTHING to the human for audience \"model\""
      (let [out (attach-out (str "attach(b'PNGDATA', 'chart.png', media_type='image/png', "
                                 "audience='model', label='for my own eyes')\n"))]
        (expect (nil? (:error out)))
        (expect (= 1 (count (:attachments out))))
        (expect (= "model" (:audience (:row out))))
        ;; Staying silent IS the feature: no fence, no caption line, nothing in
        ;; the block naming an artifact the human was never meant to review.
        (expect (not (re-find #"chart\.png" (str (:stdout out)))))))
  (it "refuses an audience outside the closed vocabulary"
      (let [out (attach-out (str "try:\n"
                                 "    attach(b'PNGDATA', 'c.png', media_type='image/png', "
                                 "audience='everyone')\n"
                                 "except ValueError as e:\n"
                                 "    print('RAISED', 'both' in str(e))\n"))]
        (expect (empty? (:attachments out)))
        (expect (re-find #"RAISED True" (str (:stdout out)))))))

(defn- versioned-reader
  "Reader holding ONE artifact under three names' worth of history: `chart.png`
   at versions 1..3 plus an unrelated `notes.txt`, listed out of order so the
   shim's own sort is what puts the thread back together."
  []
  {:list
   (fn []
     [{:id "v2" :filename "chart.png" :version 2 :media-type "image/png" :kind "image" :size 7}
      {:id "n1" :filename "notes.txt" :version 1 :media-type "text/plain" :kind "table" :size 3}
      {:id "v3" :filename "chart.png" :version 3 :media-type "image/png" :kind "image" :size 9}
      {:id "v1" :filename "chart.png" :version 1 :media-type "image/png" :kind "image" :size 5}])
   :read (constantly nil)
   :reinspect (constantly nil)})

(defdescribe
  vis-attachment-versions-test
  "An artifact is a NAME with a history, and both attachment accessors carry it:
   `list_attachments(name)` hands back that whole thread oldest-first and
   `get_attachment(name)` the latest cut — `get_attachment(name, n)` an exact
   one — so a block can pick up where the previous iteration left off instead of
   attaching a fourth unrelated chart."
  (it
    "walks one artifact's versions, defaults to the latest, and indexes backwards"
    (let [pctx
          (ctx-with-root (temp-root))

          out
          (binding [mpl-capture/*attachment-reader* (versioned-reader)]
            (block
              pctx
              (str "vs = list_attachments('chart.png')\n"
                   "print([v['version'] for v in vs], [v['id'] for v in vs])\n"
                   "print(get_attachment('chart.png')['id'])\n"
                   "print(get_attachment('chart.png', 1)['id'])\n"
                   "print(get_attachment('chart.png', -2)['id'])\n"
                   "print(get_attachment('v2')['id'])\n" "print(list_attachments('nope.png'))\n"
                   "try:\n" "    get_attachment('chart.png', 9)\n"
                   "except Exception as e:\n" "    print('RAISED', 'versions: 1, 2, 3' in str(e))\n"
                   "try:\n" "    get_attachment('nope.png')\n"
                   "except Exception:\n" "    print('MISSING')\n")))

          so
          (str (:stdout out))]

      (expect (nil? (:error out)))
      ;; Oldest-first, and only THIS artifact's cuts — notes.txt is its own thread.
      (expect (re-find #"\[1, 2, 3\] \['v1', 'v2', 'v3'\]" so))
      ;; No version asked for means the newest one: the primary view is the latest.
      (expect (re-find #"(?m)^v3$" so))
      (expect (re-find #"(?m)^v1$" so))
      ;; -1 is the latest, so -2 is the cut before it.
      (expect (re-find #"(?m)^v2$" so))
      ;; An id still wins over the name lookup: one call answers both questions.
      (expect (= 2 (count (re-seq #"(?m)^v2$" so))))
      ;; An unknown name is an empty thread, never an error.
      (expect (re-find #"(?m)^\[\]$" so))
      ;; A version that never existed says which ones did.
      (expect (re-find #"RAISED True" so))
      (expect (re-find #"MISSING" so)))))

;; Regression, session 55ed67f6 (iOS companion work): `attach` returned None and
;; the artifact it had just stored was invisible to `list_attachments`,
;; `get_attachment`, `read_attachment` and `show_attachment` until the NEXT
;; iteration — so `show_attachment(attach(path))` refused with "no attachment
;; with id or filename 'None'", and addressing the fresh artifact by the very
;; filename it was attached under refused too. A wrong keyword compounded it:
;; `attach(p, name=..., title=...)` reported an unexpected keyword argument '2'.
(defdescribe
  attach-same-block-addressing-test
  "AN ARTIFACT IS ADDRESSABLE THE MOMENT IT EXISTS. `attach` hands back the
   stored artifact's descriptor — the id its database row will carry and the cut
   this filename just became — and every read verb answers over the artifacts
   this block just attached as well as the ones already persisted, because the
   producer cannot wait a turn to look at what it produced."
  (it "addresses a fresh artifact by descriptor, by filename and by id"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "d = attach(b'a,b\\n1,2\\n', 'fresh.csv')\n"
                        "print(get_attachment(d)['id'] == d['id'],\n"
                        "      get_attachment('fresh.csv')['id'] == d['id'],\n"
                        "      get_attachment(d['id'])['id'] == d['id'])\n"
                        "print(read_attachment(d) == b'a,b\\n1,2\\n',\n"
                        "      [r['filename'] for r in list_attachments()])\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"True True True" (str (:stdout out))))
        (expect (re-find #"True \['fresh\.csv'\]" (str (:stdout out))))))
  (it "chains versions of one name inside a single block"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block
              pctx
              (str
                "attach(b'one', 'chart.png', media_type='image/png')\n"
                "d2 = attach(b'two', 'chart.png', media_type='image/png')\n"
                "print(d2['version'], [r['version'] for r in list_attachments('chart.png')])\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"2 \[1, 2\]" (str (:stdout out))))))
  (it "show_attachment names a fresh image instead of refusing it"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "d = attach(b'PNGDATA', 'shot.png', media_type='image/png')\n"
                        "s = show_attachment(d)\n"
                        "print(s['id'] == d['id'], s['filename'], s['size'])\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"True shot\.png 7" (str (:stdout out))))))
  (it "queues a fresh user-only image for the next request, and never a wire one"
      (let [pctx
            (ctx-with-root (temp-root))

            sink
            (atom [])

            out
            (binding [mpl-capture/*attachment-reinspection-sink* sink]
              (block pctx
                     (str "u = attach(b'PNGDATA', 'user.png', media_type='image/png', "
                          "audience='user')\n"
                          "b = attach(b'PNGDATA', 'both.png', media_type='image/png')\n"
                          "show_attachment(u)\n" "show_attachment(b)\n")))]

        (expect (nil? (:error out)))
        ;; The one the human alone was shown is put in front of the model; the
        ;; one already riding this request is not sent twice.
        (expect (= ["user.png"] (mapv :filename @sink)))))
  ;; Regression, vis session a64d44c2: `attach(p, name=…, title=…)` raised
  ;; TypeError and stored NOTHING — an artifact lost to two words, by a model
  ;; that reaches for `name=` again the next turn.
  (it "folds a near-miss keyword onto the canonical one"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "d = attach(b'x', name='a.txt', title='a caption')\n"
                        "print(d['filename'])\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"a\.txt" (str (:stdout out))))
        (expect (= [["a.txt" "a caption"]] (mapv (juxt :filename :label) (:attachments out))))))
  (it "reads path= as the source it means"
      (let [root
            (temp-root)

            _
            (spit (str root "/note.txt") "hello")

            pctx
            (ctx-with-root root)

            out
            (block pctx
                   (str "d = attach(path=" (pr-str (str root "/note.txt"))
                        ")\n" "print(d['filename'], d['size'])\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"note\.txt 5" (str (:stdout out))))))
  (it "refuses one idea named twice, a keyword that spells nothing, and no source"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "try:\n" "    attach(b'x', filename='a.txt', name='b.txt')\n"
                        "except TypeError as e:\n" "    print('TWICE', 'named twice' in str(e))\n"
                        "try:\n" "    attach(b'x', colour='red')\n"
                        "except TypeError as e:\n"
                        "    print('UNKNOWN', \"no keyword 'colour'\" in str(e))\n"
                        "try:\n" "    attach(filename='a.txt')\n"
                        "except TypeError as e:\n"
                        "    print('NOSOURCE', 'source is required' in str(e))\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"TWICE True" (str (:stdout out))))
        (expect (re-find #"UNKNOWN True" (str (:stdout out))))
        (expect (re-find #"NOSOURCE True" (str (:stdout out))))
        (expect (empty? (:attachments out))))))

(defdescribe
  attach-pages-are-canonical-test
  "A prebound global's page is the whole contract its caller reads, so it wears
   the shape every documented Vis verb wears: the CALL LINE and the KEYS are
   STRUCTURE (`__vis_calls__` / `__vis_keys__`, printed above the document and
   unrelated to the `apropos` name filter), the keys line is where requiredness
   is stated, and the result is stated exactly once. The five attachment globals
   shipped prose that opened with a retyped signature and named no keys at all."
  (it
    "states call, keys and result for every attachment global, matched to the def"
    (let [pctx
          (ctx-with-root (temp-root))

          out
          (block pctx
                 (str "import inspect\n" "bad = []\n"
                      "for n in ('attach', 'list_attachments', 'get_attachment',\n"
                      "          'read_attachment', 'show_attachment'):\n"
                      "    fn = globals()[n]\n" "    call = __vis_calls__.get(n, '')\n"
                      "    keyline = __vis_keys__.get(n, '')\n"
                      "    page = __vis_docs__.get(n, '')\n"
                      "    ps = [p for p in inspect.signature(fn).parameters.values()\n"
                      "          if p.kind is not p.VAR_KEYWORD]\n"
                      "    want = n + '(' + ', '.join(str(p) for p in ps) + ')'\n"
                      "    named = [k.split(' ')[0]\n"
                      "             for k in keyline[len('Keys: '):].split(' · ')]\n"
                      "    if call != want:\n"
                      "        bad.append((n, 'call', call, want))\n"
                      "    if not keyline.startswith('Keys: '):\n"
                      "        bad.append((n, 'keys', keyline))\n"
                      "    if [k for k in named if k not in [p.name for p in ps]]:\n"
                      "        bad.append((n, 'key nothing takes', named))\n"
                      "    if [p.name for p in ps if p.name not in named]:\n"
                      "        bad.append((n, 'undocumented parameter', named))\n"
                      "    if 'Raw result:' not in page:\n"
                      "        bad.append((n, 'no result contract', page[:40]))\n"
                      "    if page.startswith(n + '('):\n"
                      "        bad.append((n, 'signature retyped into the prose', page[:40]))\n"
                      "print(bad)\n"))]

      (expect (nil? (:error out)))
      ;; REQUIRED is the machine-readable half of the same line.
      (expect (= "[]" (str/trim (str (:stdout out))))))))

(defdescribe
  compact-attachment-host-errors-test
  "Attachment host failures use the same observed boundary as filesystem tools."
  (it
    "reports one sentence without repeating the block or the tool name"
    (tpc/with-own
      [ctx {}]
      (doseq
        [[code message symbol]
         [["attach(b'', 'empty.txt')" "attach: empty payload." :attach]
          ["get_attachment('missing.png')"
           "get_attachment: unknown id or filename \"missing.png\"; use list_attachments()."
           :get_attachment]
          ["read_attachment('missing.png')"
           "read_attachment: unknown id or filename \"missing.png\"; use list_attachments()."
           :read_attachment]
          ["show_attachment('missing.png')"
           "show_attachment: unknown id or filename \"missing.png\"; local image: show_attachment(attach(path))."
           :show_attachment]]]
        (let [out
              (ep/run-python-block ctx (str "print('before')\n" code "\nprint('after')") "t1/i1")]
          (expect (= message (get-in out [:error :message])))
          (expect (= :python/host (get-in out [:error :data :phase])))
          (expect (= :vis/tool-failure (get-in out [:error :data :type])))
          (expect (= symbol (get-in out [:error :data :symbol])))
          (expect (= "before\n" (:stdout out)))))))
  (it
    "keeps argument errors as Python errors and host refusals catchable"
    (tpc/with-own
      [ctx {}]
      (let
        [bad
         (ep/run-python-block ctx "attach(b'x', 'x.txt', audience='everyone')" "t1/i1")

         caught
         (ep/run-python-block
           ctx
           "try:\n    get_attachment('missing.png')\nexcept Exception as e:\n    print(type(e).__name__)"
           "t1/i2")]

        (expect (= :python/runtime (get-in bad [:error :data :phase])))
        (expect (str/starts-with? (get-in bad [:error :message]) "ValueError:"))
        (expect (nil? (:error caught)))
        (expect (= "VisToolError\n" (:stdout caught))))))
  ;; Same regression as the `ls` shim: every accessor decodes what the bridge
  ;; answered, so a non-JSON answer names the boundary it crossed instead of
  ;; raising a bare JSONDecodeError at the caller's own line.
  (it "names the host bridge when its answer is not JSON"
      (tpc/with-own
        [ctx {}]
        (let [code (str
                     "g = globals()\n"
                     "fake = {\"__vis_record_attachment__\": lambda *a: \"[Ljava@1\",\n"
                     "        \"__vis_list_attachments__\": lambda *a: \"<html>no</html>\",\n"
                     "        \"__vis_get_attachment__\": lambda *a: \"[#object[Row]]\"}\n"
                     "saved = {k: g[k] for k in fake if k in g}\n"
                     "g.update(fake)\n" "def told(f):\n"
                     "    try:\n" "        f()\n"
                     "        return \"none\"\n" "    except Exception as e:\n"
                     "        m = str(e)\n"
                     "        return \"%s:%s:%s\" % (type(e).__name__, m.split(\":\")[0],\n"
                     "                              \"not JSON\" in m)\n" "try:\n"
                     "    print(told(lambda: attach(b\"x\", \"x.txt\")), told(list_attachments),\n"
                     "          told(lambda: get_attachment(\"x.txt\")))\n"
                     "finally:\n" "    g.update(saved)")]
          (expect (= (str "RuntimeError:attach:True RuntimeError:list_attachments:True "
                          "RuntimeError:get_attachment:True\n")
                     (:stdout (ep/run-python-block ctx code "t1/i1")))))))
  (it "crosses the jailed worker boundary without adding Python source context"
      (tpc/with-own
        [ctx {} (constantly [(System/getProperty "user.dir")]) {:worker? true :jail-enabled? true}]
        (let [out (ep/run-python-block ctx "read_attachment('missing.png')" "t1/i1")]
          (expect
            (= "read_attachment: unknown id or filename \"missing.png\"; use list_attachments()."
               (get-in out [:error :message])))
          (expect (= :python/host (get-in out [:error :data :phase])))
          (expect (= :read_attachment (get-in out [:error :data :symbol]))))))
  (it "keeps attachment bytes out of Activity and preserves attachment presentation"
      (tpc/with-own [ctx {}]
                    (let [events
                          (atom [])

                          out
                          (binding [extension/*tool-event-sink* #(swap! events conj %)]
                            (ep/run-python-block
                              ctx
                              (str "a = attach(b'attachment-payload-fixture', 'note.txt')\n"
                                   "assert read_attachment(a) == b'attachment-payload-fixture'\n"
                                   "show_attachment(a)")
                              "t1/i1"))]

                      (expect (= 1 (count (:attachments out))))
                      (expect (= "show_attachment: \"note.txt\" is not an image."
                                 (get-in out [:error :message])))
                      (expect (empty? @events))
                      (expect (not (str/includes? (pr-str @events)
                                                  "YXR0YWNobWVudC1wYXlsb2FkLWZpeHR1cmU=")))))))

(defn- extension-attach
  "Call the `vis.attach` door of a Python extension; answer the descriptor or the error."
  [opts]
  (let [door (get (shim-attach/extension-host-doors) "__vis_host_attach__")]
    (try (charred/read-json (door opts)) (catch Exception e (ex-message e)))))

(defdescribe
  extension-attach-door-test
  (it "takes an explicit type, then the magic bytes, then the filename hint"
      (binding [mpl-capture/*attachment-sink* (atom [])]
        (let [pdf (.encodeToString (java.util.Base64/getEncoder) (.getBytes "%PDF-1.4\n" "UTF-8"))
              csv (.encodeToString (java.util.Base64/getEncoder) (.getBytes "a,b\n1,2\n" "UTF-8"))]

          (expect (= ["application/pdf" "doc" "user"]
                     ((juxt #(get % "media-type") #(get % "kind") #(get % "audience"))
                       (extension-attach
                         {"base64" pdf "filename" "a.pdf" "media_type_hint" "text/csv"}))))
          (expect (= ["text/csv" "file" 2]
                     (do (extension-attach
                           {"base64" csv "filename" "t.csv" "media_type_hint" "text/csv"})
                         ((juxt #(get % "media-type") #(get % "kind") #(get % "version"))
                           (extension-attach
                             {"base64" csv "filename" "t.csv" "media_type_hint" "text/csv"})))))
          (expect (= "text/plain"
                     (get (extension-attach {"base64" csv "filename" "x" "media_type" "text/plain"})
                          "media-type")))
          (expect (= "text/plain"
                     (get (extension-attach {"base64" csv "filename" "x"}) "media-type"))))))
  (it "keeps a ZIP archive a ZIP unless the filename hint names its format"
      (binding [mpl-capture/*attachment-sink* (atom [])]
        (let [zip (let [out (java.io.ByteArrayOutputStream.)]
                    (with-open [z (java.util.zip.ZipOutputStream. out)]
                      (.putNextEntry z (java.util.zip.ZipEntry. "a.txt"))
                      (.write z (.getBytes "a" "UTF-8"))
                      (.closeEntry z))
                    (.encodeToString (java.util.Base64/getEncoder) (.toByteArray out)))
              xlsx "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"]

          (expect (= "application/zip"
                     (get (extension-attach {"base64" zip "filename" "bundle.zip"}) "media-type")))
          (expect (= xlsx
                     (get (extension-attach
                            {"base64" zip "filename" "report.xlsx" "media_type_hint" xlsx})
                          "media-type"))))))
  (it "refuses a document for the model and a call outside a block"
      (let [pdf (.encodeToString (java.util.Base64/getEncoder) (.getBytes "%PDF-1.4\n" "UTF-8"))]
        (binding [mpl-capture/*attachment-sink* (atom [])]
          (expect (str/includes? (extension-attach {"base64" pdf "audience" "model"})
                                 "documents require audience='user'")))
        (expect (str/includes? (extension-attach {"base64" pdf}) "call inside python_execution")))))

(def ^:private door-cases
  "`[label python-bytes filename expected-media-type]` rows for both attach doors.
   A nil filename means that the caller gives none."
  [["text" "b'BMW sales rose 4% in Q3\\n'" "notes.txt" "text/plain"]
   ["mp4" "bytes([0, 0, 0, 0x18]) + b'ftypmp42' + bytes(4) + b'mp42isom'" nil "video/mp4"]
   ["m4a" "bytes([0, 0, 0, 0x18]) + b'ftypM4A ' + bytes(4) + b'M4A mp42'" nil "audio/mp4"]
   ["flac" "b'fLaC' + bytes([0, 0, 0, 0x22])" nil "audio/flac"]
   ["svg" "b'<svg xmlns=\"http://www.w3.org/2000/svg\"/>'" nil "image/svg+xml"]
   ["mp3" "mp3_22k" nil "audio/mpeg"] ["words" "b'just words'" "note" "text/plain"]
   ["binary" "bytes([0x00, 0x01, 0xFE, 0xFF])" "blob" "application/octet-stream"]
   ["page" "b'<!doctype html><html><svg width=\"1\"/></html>'" "report.html" "text/html"]
   ["json" "b'{\"a\": 1}\\n'" "result.json" "application/json"]
   ;; A text byte-order mark is not an MP3 frame: the filename names the type.
   ["utf16" "'hello\\n'.encode('utf-16')" "notes.txt" "text/plain"]
   ["utf16-csv" "'a,b\\n1,2\\n'.encode('utf-16')" "data.csv" "text/csv"]
   ["utf32" "'hello\\n'.encode('utf-32')" "notes.txt" "text/plain"]
   ["latin1" "b'\\xff\\xfe plain'" "notes.txt" "text/plain"]
   ["utf16-svg" "'<svg xmlns=\"http://www.w3.org/2000/svg\"/>'.encode('utf-16')" "pic.svg"
    "image/svg+xml"] ["mp3-fb" "mp3" nil "audio/mpeg"]
   ;; An animated PNG is a PNG, with or without a name.
   ["apng" "apng" "real.apng" "image/png"] ["apng-bare" "apng" nil "image/png"]
   ;; HTML can omit the html tag.
   ["html5" "b'<!doctype html>\\n<head><title>t</title></head><body>x</body>\\n'" nil "text/html"]
   ;; One line of JSON is not JSON Lines, unless the name says so.
   ["geojson" "b'{\"type\": \"FeatureCollection\", \"features\": []}\\n'" "map.geojson"
    "text/plain"] ["notebook" "b'{\"cells\": [], \"nbformat\": 4}\\n'" "nb.ipynb" "text/plain"]
   ["json-line" "b'{\"a\": 1}\\n'" nil "text/plain"]
   ["jsonl-name" "b'{\"a\": 1}\\n'" "events.jsonl" "application/x-ndjson"]
   ["ndjson-name" "b'{\"a\": 1}\\n'" "events.ndjson" "application/x-ndjson"]
   ["jsonl-lines" "b'{\"a\": 1}\\n{\"a\": 2}\\n'" nil "application/x-ndjson"]
   ;; A processing instruction can hold a `>`.
   ["svg-pi" "svg_pi" nil "image/svg+xml"]
   ;; Only the type of a ZIP-based format names ZIP bytes.
   ["zip-txt" "pk" "notes.txt" "application/zip"] ["zip-png" "pk" "pic.png" "application/zip"]
   ["zip-epub" "pk" "book.epub" "application/epub+zip"]
   ;; Text in an 8-bit encoding is not audio when the filename says text: in
   ;; Windows-1251, the letter `ya` is the byte `FF`, the start of a frame header.
   ["cp1251-frame" "bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a')" "notes.txt" "text/plain"]
   ["cp1251-csv" "bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a')" "data.csv" "text/csv"]
   ["cp1251-adts" "bytes.fromhex('fff1edee20e820efeeedfff2edee0a')" "notes.txt" "text/plain"]
   ["telnet" "bytes.fromhex('fffd18fffd20fffd23fffd270d0a6c6f67696e3a20')" "notes.txt" "text/plain"]
   ["flac-words" "b'fLaC is a codec\\n'" "notes.txt" "text/plain"]
   ;; Real media keeps its type under a text filename: its bytes are not text.
   ["png-txt" "png" "pic.txt" "image/png"]
   ["flac-txt" "b'fLaC' + bytes([0, 0, 0, 0x22]) + bytes(34)" "song.txt" "audio/flac"]
   ["id3-txt" "b'ID3' + bytes([4, 0, 0, 0, 0, 0, 10]) + bytes(16)" "song.txt" "audio/mpeg"]
   ;; A frame header alone is not a recording: a second frame must follow at the
   ;; frame length. So 8-bit text is never audio, and a recording keeps its type
   ;; under a text filename.
   ["cp1251-log" "bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a')" "server.log"
    "application/octet-stream"]
   ["cp1251-bare" "bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a')" nil
    "application/octet-stream"]
   ["cp1251-long" "bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a') * 200" "server.log"
    "application/octet-stream"]
   ;; The same word can stand at the frame length by chance: the third header decides.
   ["cp1251-twice"
    "bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a') + b' ' * 1425 + bytes.fromhex('ffe2ebe5ede8e520eff0e8f0eee4fb0a') + b' ' * 3000"
    nil "application/octet-stream"]
   ["telnet-bare" "bytes.fromhex('fffd18fffd20fffd23fffd270d0a6c6f67696e3a20')" nil
    "application/octet-stream"]
   ["frame-alone" "bytes([0xFF, 0xFB, 0x90, 0x64])" nil "application/octet-stream"]
   ["mp3-txt" "mp3" "song.txt" "audio/mpeg"] ["mp3-log" "mp3" "song.log" "audio/mpeg"]
   ["aac" "aac" nil "audio/aac"] ["aac-txt" "aac" "song.txt" "audio/aac"]
   ;; A line of fixed width can put the same word at each frame length. Text holds no
   ;; binary control character, so it is not media under any filename.
   ["cp1251-lines" "bytes.fromhex('ffe2ebe5ede8e52031320a') * 400" "server.log"
    "application/octet-stream"]
   ["cp1251-lines-json" "bytes.fromhex('ffe2ebe5ede8e52031320a') * 400" "data.json"
    "application/json"] ["flac-words-bare" "b'fLaC is a codec\\n'" nil "text/plain"]
   ["flac-words-json" "b'fLaC is a codec\\n'" "data.json" "application/json"]])

(def ^:private door-prelude
  "Python names that the rows of [[door-cases]] use: `apng` is a real 1x1 PNG
   with an `acTL` chunk after its header, `svg_pi` is an SVG with a `>` inside a
   processing instruction, `pk` is the head of a ZIP archive, and `mp3`, `mp3_22k`
   and `aac` are two audio frames each, without a tag."
  (str
    "import base64, binascii, mimetypes\n"
    "png = base64.b64decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==')\n"
    "actl = b'acTL' + bytes([0, 0, 0, 1, 0, 0, 0, 0])\n"
    "apng = png[:33] + (8).to_bytes(4, 'big') + actl + binascii.crc32(actl).to_bytes(4, 'big')"
    " + png[33:]\n"
    "svg_pi = (b'<?xml version=\"1.0\"?>\\n<?xml-stylesheet href=\"a>b.css\" type=\"text/css\"?>\\n'"
    " + b'<svg xmlns=\"http://www.w3.org/2000/svg\"/>\\n')\n"
    "pk = b'PK\\x03\\x04' + bytes(20)\n"
    "mp3 = (bytes([0xFF, 0xFB, 0x90, 0x00]) + bytes(413)) * 2\n"
    "mp3_22k = (bytes([0xFF, 0xF3, 0x90, 0x00]) + bytes(257)) * 2\n"
    "aac = (bytes.fromhex('fff15080021ffc') + bytes(9)) * 2\n"))

(defdescribe
  attach-doors-media-type-test
  (it
    "gives the same bytes and filename the same media type at the sandbox door and the SDK door"
    (let [out
          (block (ctx-with-root (temp-root))
                 (str door-prelude
                      (str/join (for [[label data filename] door-cases]
                                  (str "d = "
                                       data
                                       "\n"
                                       "n = "
                                       (if filename (pr-str filename) "None")
                                       "\n"
                                       "r = attach(d, n) if n else attach(d)\n"
                                       "h = mimetypes.guess_type(n)[0] if n else None\n"
                                       "print('CASE', " (pr-str label)
                                       ", r['media_type'], r['kind'], r['audience'],"
                                       " base64.b64encode(d).decode(), h or '-')\n")))))

          sandbox
          (into {}
                (for [[_ label media-type kind audience b64 hint]
                      (re-seq #"CASE (\S+) (\S+) (\S+) (\S+) (\S+) (\S+)" (str (:stdout out)))]
                  [label
                   {:media-type media-type :kind kind :audience audience :b64 b64 :hint hint}]))]

      (expect (nil? (:error out)))
      (binding [mpl-capture/*attachment-sink* (atom [])]
        (doseq [[label _ filename expected] door-cases]
          (let [{:keys [media-type kind audience b64 hint]} (get sandbox label)
                row (extension-attach (cond-> {"base64" b64}
                                        filename
                                        (assoc "filename" filename)

                                        (not= "-" hint)
                                        (assoc "media_type_hint" hint)))]

            (expect (= [label expected] [label media-type]))
            (expect (= [label expected] [label (get row "media-type")]))
            (expect (= [label kind] [label (get row "kind")]))
            (expect (= [label audience] [label (get row "audience")])))))))
  (it "keeps the picture of an animated PNG and the audience of text and of one line of JSON"
      (let [out
            (block
              (ctx-with-root (temp-root))
              (str door-prelude
                   "print('APNG', attach(apng, 'real.apng')['kind'])\n"
                   "j = b'{\"type\": \"FeatureCollection\", \"features\": []}\\n'\n"
                   "print('JSON', attach(j, 'map.geojson')['audience'],"
                   " attach(j, 'model.geojson', audience='model')['audience'])\n"
                   "print('WIDE', attach('hello\\n'.encode('utf-16'), 'notes.txt')['audience'])\n"))

            so
            (str (:stdout out))

            geojson
            (.encodeToString (java.util.Base64/getEncoder)
                             (.getBytes "{\"type\": \"FeatureCollection\", \"features\": []}\n"
                                        "UTF-8"))]

        (expect (nil? (:error out)))
        (expect (re-find #"APNG image" so))
        (expect (re-find #"vis-image" so))
        (expect (re-find #"JSON both model" so))
        (expect (re-find #"WIDE both" so))
        (binding [mpl-capture/*attachment-sink* (atom [])]
          (expect (= "model"
                     (get (extension-attach
                            {"base64" geojson "filename" "map.geojson" "audience" "model"})
                          "audience"))))))
  (it
    "lets an explicit media type win at both doors"
    (let
      [out
       (block
         (ctx-with-root (temp-root))
         "print('MT', attach(b'fLaC' + bytes(4), 'a.flac', media_type='text/x-note')['media_type'])\n")

       flac
       (.encodeToString (java.util.Base64/getEncoder)
                        (byte-array (concat (.getBytes "fLaC" "US-ASCII") (repeat 4 (byte 0)))))]

      (expect (nil? (:error out)))
      (expect (re-find #"MT text/x-note" (str (:stdout out))))
      (binding [mpl-capture/*attachment-sink* (atom [])]
        (expect (= "text/x-note"
                   (get (extension-attach
                          {"base64" flac "filename" "a.flac" "media_type" "text/x-note"})
                        "media-type"))))))
  (it
    "names a file above the limit from its first bytes, also when the cut splits a character"
    (let [root
          (.getCanonicalPath (java.io.File. (temp-root)))

          mp4
          (byte-array
            (concat [0 0 0 0x18] (.getBytes "ftypmp42" "US-ASCII") (repeat 6000 (byte 0))))

          _
          (with-open [out (java.io.FileOutputStream. (java.io.File. root "clip"))]
            (.write out mp4))

          _
          (spit (java.io.File. root "big.log")
                (str "a" (str/join (repeat 3000 "é")))
                :encoding
                "UTF-8")

          _
          (with-open [out (java.io.FileOutputStream. (java.io.File. root "wide.txt"))]
            (.write out (byte-array [0xff 0xfe]))
            (.write out (.getBytes (str/join (repeat 2000 "hello\n")) "UTF-16LE")))

          out
          (extension/with-context
            {:env {:session-id (random-uuid) :workspace {:root root}}}
            (binding [mpl-capture/*attachment-limit-bytes* 100]
              (block (ctx-with-root root)
                     (str "for n in ('clip', 'big.log', 'wide.txt'):\n"
                          "    r = attach(" (pr-str (str root "/"))
                          " + n)\n"
                          "    print('REF', r['storage'], r['media_type'], r['kind'])\n"))))]

      (expect (nil? (:error out)))
      (expect (= [["reference" "video/mp4" "file"] ["reference" "text/plain" "file"]
                  ["reference" "text/plain" "file"]]
                 (mapv (comp vec rest) (re-seq #"REF (\S+) (\S+) (\S+)" (str (:stdout out)))))))))

(defn- app-block
  "Run `code` as one block of session `sid`, with view records in a temp root."
  [sid code]
  (with-redefs-fn {(requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir)
                   (constantly (java.io.File. (temp-root) "views"))}
    #(binding [extension/*current-environment* {:session-id sid}] (block (ctx-with-root (temp-root))
                                                                         code))))

(defdescribe attach-app-test
             ;; #421: `attach(ui.app(...))` opens an app of the block's session through the
             ;; sandbox app door, and answers the handle that drives it.
             (it "opens an app in the session of the block and drives it"
                 (let [sid
                       (str (random-uuid))

                       out
                       (app-block sid
                                  (str "from blockether.vis.extension import ui\n"
                                       "app = attach(ui.app('Review', ui.thread('general')))\n"
                                       "app.reply('general', 'Done.')\n"
                                       "print(type(app).__name__, app.state()['session_id'] == '"
                                       sid
                                       "')\n"
                                       "print([m['body'] for m in app['general'].messages])\n"
                                       "app.close()\n"))]

                   (expect (nil? (:error out)))
                   (expect (re-find #"LiveAttachment True" (str (:stdout out))))
                   (expect (re-find #"\['Done\.'\]" (str (:stdout out))))))
             (it "refuses file arguments for an app"
                 (let [out (app-block
                             (str (random-uuid))
                             (str
                               "from blockether.vis.extension import ui\n"
                               "try:\n"
                               "    attach(ui.app('Review', ui.thread('general')), 'review.json')\n"
                               "except TypeError as e:\n" "    print('refused:', e)\n"))]
                   (expect (nil? (:error out)))
                   (expect (re-find #"refused:" (str (:stdout out)))))))
