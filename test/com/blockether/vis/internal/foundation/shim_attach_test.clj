(ns com.blockether.vis.internal.foundation.shim-attach-test
  "The generic attachment shim (`attach`) installed into
   every sandbox context via the sandbox-shim mechanism. A tool PRODUCES an
   artifact and hands it to `attach`; the bytes are captured AT THE SOURCE
   into the per-block sink (drained into the block outcome's `:attachments`, which the
   loop persists as `:attachments`), with the media-type sniffed from magic bytes
   / extension / a utf-8 probe. No stdout fence, no parsing."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.foundation.shim-attach :as shim-attach]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context Value]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent Callable Executors Future]))

(defn- temp-root
  ^String []
  (str (.toAbsolutePath (Files/createTempDirectory "vis-attach-test"
                                                   (make-array FileAttribute 0)))))

(defn- ctx-with-root
  "A sandbox context whose filesystem is confined to a fresh temp root."
  [root]
  (:python-context (ep/create-python-context {}
                                             (fn []
                                               [root]))))

(defn- block
  "Run `code` as ONE driven block and return the flat outcome (so the per-block
   image sink is bound and drained into `:attachments`)."
  [^Context pctx code]
  (ep/run-python-block pctx code))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defdescribe
  attach-in-memory-capture-test
  (it "records an in-memory artifact without returning displayable metadata"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx "attach(b'a,b\\n1,2\\n', 'notes.txt')\n")

         [att]
         (:attachments out)]

        (expect (nil? (:error out)))
        ;; Attachment APIs are side-effect-only: a bare call must not make the
        ;; python_execution card show a summary dictionary.
        (expect (nil? (:result out)))
        (expect (empty? (str (:stdout out))))
        (expect (= 1 (count (:attachments out))))
        (expect (= "text/plain" (:media-type att)))
        (expect (= "file" (:kind att)))
        (expect (= "notes.txt" (:filename att)))
        (expect (= 8 (:size att)))
        ;; base64 round-trips to the original bytes
        (expect (= "a,b\n1,2\n"
                   (String. (.decode (java.util.Base64/getDecoder) ^String (:base64 att)))))))
  (it "detects an image by magic bytes -> kind image, image/png"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx
                (str "png = bytes([0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A]) + b'body'\n"
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
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx "attach(b'<svg/>', 'a.svg', media_type='image/svg+xml')\n")]

        (expect (nil? (:error out)))
        (expect (not (re-find #"vis-image" (str (:stdout out)))))
        (expect (= "image/svg+xml" (:media-type (first (:attachments out)))))))
  (it "falls back to text/plain for undecorated utf-8 bytes"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx "attach(b'just words', 'note')\n")

         [att]
         (:attachments out)]

        (expect (= "text/plain" (:media-type att)))
        (expect (= "file" (:kind att)))))
  (it "honours explicit kind / media_type overrides"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx "attach(b'x', 'weird.bin', kind='image', media_type='image/svg+xml')\n")

         [att]
         (:attachments out)]

        (expect (= "image/svg+xml" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it "collects MANY artifacts from one block, in order"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx (str "attach(b'1', 'a.txt')\n" "attach(b'2', 'b.json')\n"))]

        (expect (= ["a.txt" "b.json"] (mapv :filename (:attachments out))))))
  (it "renders and captures a matplotlib Figure with a positional filename"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx
                (str "import matplotlib.pyplot as plt\n" "fig, ax = plt.subplots(figsize=(7, 4))\n"
                     "ax.plot([0, 1], [0, 1])\n" "ax.set(title='plot', xlabel='x', ylabel='y')\n"
                     "attach(fig, 'plot.png')\n" "plt.close(fig)\n"))

         [att]
         (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "plot.png" (:filename att)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))))
  ;; Regression: attach(pil_image, 'crop.png') fell through to the PATH branch
  ;; and died with "attach: no such file: <PIL.Image.Image ...>", so a picture
  ;; cropped in the sandbox could not be attached without writing it out first.
  (it "encodes and captures a PIL image handed straight to attach"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx
                (str "from PIL import Image\n"
                     "img = Image.new('RGBA', (12, 9), (255, 0, 0, 255))\n"
                     "attach(img, 'crop.png')\n"))

         [att]
         (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "crop.png" (:filename att)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))
        ;; the pixels really rode along: the fence carries the image's own dims
        (expect (re-find #"12x9" (str (:stdout out))))))
  (it "lets the filename choose the encoder for a PIL image, defaulting to PNG"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx
                (str "from PIL import Image\n"
                     "img = Image.new('RGBA', (6, 6), (0, 128, 255, 255))\n"
                     "attach(img, 'shot.jpg')\n" "attach(img)\n"))

         [jpg png]
         (:attachments out)]

        (expect (nil? (:error out)))
        ;; a JPEG has no alpha channel, so an RGBA image is converted, not refused
        (expect (= ["shot.jpg" "image/jpeg"] [(:filename jpg) (:media-type jpg)]))
        (expect (= ["image.png" "image/png"] [(:filename png) (:media-type png)]))))
  (it
    "refuses a source that is neither a path nor a producer, by name"
    (let
      [pctx
       (ctx-with-root (temp-root))

       out
       (block pctx
              (str "try:\n" "    attach({'rows': 1}, 'data.json')\n"
                   "except TypeError as e:\n" "    print('RAISED', e)\n"))]

      (expect (nil? (:error out)))
      ;; naming the SHAPE that was wrong, never a repr reported as a missing file
      (expect
        (re-find
          #"RAISED attach: source must be a path, bytes, a PIL image or a matplotlib figure, got dict"
          (str (:stdout out))))
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
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx "attach(b'hello there', 'notes.txt', label='fleet counts')\n")]

        (expect (nil? (:error out)))
        (expect (re-find #"\[Attached: notes\.txt\] fleet counts" (str (:stdout out))))))
  (it "attach takes the same label kwarg for a file on disk"
      (let
        [root
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
      (let
        [pctx
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
      (let
        [root
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
      (let
        [pctx
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
      (let
        [root
         (temp-root)

         pctx
         (ctx-with-root root)

         out
         (block pctx
                (str "import os, pathlib\n"
                     "root = " (pr-str root)
                     "\n" "with open(root + '/report.json','w') as f:\n"
                     "    f.write('{}')\n" "os.environ['HOME'] = root\n"
                     "attach(pathlib.Path(root + '/report.json'), 'pathlib.json')\n"
                     "attach('~/report.json', 'tilde.json')\n"
                     "try:\n" "    attach(root + '/nope.json')\n"
                     "except Exception as e:\n" "    print('RAISED', e)\n"))]

        (expect (nil? (:error out)))
        (expect (= ["pathlib.json" "tilde.json"]
                   (filterv #{"pathlib.json" "tilde.json"} (mapv :filename (:attachments out)))))
        (expect (re-find #"RAISED attach: no such file: .*nope\.json" (str (:stdout out)))))))

(defdescribe
  attach-discovery-test
  (it "surfaces attach / list_attachments via apropos and doc"
      (let [pctx (ctx-with-root (temp-root))]
        ;; `apropos` is FULL TEXT and ranked: the five name hits come before every
        ;; document that merely mentions attaching.
        (expect (= ["attach" "get_attachment" "list_attachments" "read_attachment"
                    "show_attachment"]
                   (vec (ev pctx "sorted(list(apropos('attach'))[:5])"))))
        (expect (false? (ev pctx "'attachments' in apropos('attach')")))
        (expect (true? (ev pctx "'callable' in doc('attach')")))
        (expect (true? (ev pctx "'callable' in doc('show_attachment')")))))
  (it "discovers declared capabilities instead of the internal shim identity"
      (with-redefs
        [extension/sandbox-shims (constantly [{:shim/name "internal-id"
                                               :shim/imports ["actual_module"]
                                               :shim/globals ["actual_global"]
                                               :shim/description "Synthetic discovery contract."
                                               :shim/source "vis-shims-test/discovery.py"}])]
        (let [pctx (ctx-with-root (temp-root))]
          (expect (= ["actual_global" "actual_module"]
                     (vec (ev pctx "sorted(list(apropos('actual'))[:2])"))))
          (expect (= 42 (ev pctx "import actual_module; actual_module.answer")))
          (expect (= 42 (ev pctx "actual_global()")))
          (expect (false? (ev pctx "'internal-id' in apropos('internal')")))
          (expect (true? (ev pctx "'Synthetic discovery contract.' in doc('actual_module')"))))))
  (it "raises when called with no active capture sink (outside a driven block)"
      (let [pctx (ctx-with-root (temp-root))]
        ;; a bare .eval does NOT bind the per-block sink, so the bridge refuses
        (expect (re-find #"no active capture sink"
                         (ev pctx
                             (str "\ntry:\n" "    attach(b'x', 'y.txt')\n"
                                  "    _r = 'NO-RAISE'\n" "except Exception as e:\n"
                                  "    _r = str(e)\n" "_r")))))))

(defdescribe
  vis-outbox-capture-test
  (it "captures a file WRITTEN into $VIS_OUTBOX as an attachment (no attach call)"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx
                (str "import os\n"
                     "with open(os.path.join(os.environ['VIS_OUTBOX'], 'm.csv'), 'w') as f:\n"
                     "    f.write('a,b\\n1,2\\n')\n" "print('ok')\n"))

         [att]
         (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= 1 (count (:attachments out))))
        (expect (= "m.csv" (:filename att)))
        (expect (= "text/csv" (:media-type att)))
        (expect (= "file" (:kind att)))))
  (it "captures a confined write under a system temp root too (scratch tap, not just $VIS_OUTBOX)"
      (let
        [root
         (temp-root)

         pctx
         (ctx-with-root root)

         out
         (block pctx
                (str "with open('"
                     root
                     "/plain.txt', 'w') as f:\n"
                     "    f.write('hi')\n"
                     "print('ok')\n"))]

        (expect (nil? (:error out)))
        ;; The outbox tap is widened to any system temp root (/tmp, $TMPDIR):
        ;; a `temp-root` write is scratch under it, so it captures at the source.
        (expect (= 1 (count (:attachments out))))
        (expect (= "plain.txt" (:filename (first (:attachments out))))))))

(defn- conveying-gather
  "Faithful replica of loop.clj's `gather-fn`: submit each thunk to a
   virtual-thread executor wrapped in `bound-fn*` (which snapshots the caller's
   thread-local binding frame — INCLUDING `*attachment-sink*` — and replays it
   on the worker), then `.get` in submission order. This IS the conveyance
   under test: an artifact produced inside `await gather(...)` runs on a virtual
   thread yet must still reach the block's `:attachments`."
  [^java.util.concurrent.ExecutorService executor]
  (fn [& thunks]
    (let
      [thunks
       (if (and (= 1 (count thunks)) (sequential? (first thunks)))
         (vec (first thunks))
         (vec thunks))

       call
       (fn [t]
         (cond (instance? Value t) (.execute ^Value t (object-array 0))
               (ifn? t) (t)
               :else t))

       futs
       (mapv (fn [t]
               (.submit executor
                        ^Callable
                        (bound-fn* (fn []
                                     (call t)))))
             thunks)]

      (mapv (fn [^Future f]
              (.get f))
            futs))))

(defn- ctx-with-gather
  "A confined sandbox context with `__vis_par__` wired to a faithful
   virtual-thread `gather`, so `await gather(...)` runs its awaitables on
   virtual threads exactly like the real loop."
  [root executor]
  (:python-context (ep/create-python-context {(symbol "__vis_par__") (conveying-gather executor)}
                                             (fn []
                                               [root]))))

(defdescribe
  gather-conveys-attachment-sink-test
  "Regression (turn 28/29): an artifact produced by a tool running INSIDE
   `await gather(...)` executes on a gather-executor virtual thread, not the
   block thread. `bound-fn*` must convey the per-block `*attachment-sink*` to
   that thread so `attach` still lands in the block's `:attachments`
   — no silent drop, no nil sink."
  (it "captures every gather-produced artifact into the block's :attachments"
      (let
        [ex
         (Executors/newVirtualThreadPerTaskExecutor)

         pctx
         (ctx-with-gather (temp-root) ex)

         out
         (try (block pctx
                     (str "async def mk(name):\n" "    return attach(b'payload', name)\n"
                          "r = await gather(mk('a.txt'), mk('b.txt'), mk('c.txt'))\n"
                          "print(len(r))\n"))
              (finally (.shutdownNow ex)))

         atts
         (:attachments out)]

        (expect (nil? (:error out)))
        (expect (re-find #"^3" (str (:stdout out))))
        ;; all three, produced on virtual threads, reached the block's sink
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
                  (let
                    [a {:id "a1"
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
   the two it is holding. Unbound, they raise a clear RuntimeError instead of
   silently returning nothing."
  (it "lists metadata and reads the bytes back"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (binding [mpl-capture/*attachment-reader* (fake-reader)]
           (block pctx
                  (str "a = list_attachments()[0]\n" "d = get_attachment('a1')\n"
                       "r = read_attachment('a1')\n"
                       "print(a['id'], a['media_type'], a['tool_call_id'], a['iteration_id'],\n"
                       "      a['turn_id'])\n"
                       "print(type(r).__name__, r.decode('utf-8'), d['filename'], d['size'])\n")))

         so
         (str (:stdout out))]

        (expect (nil? (:error out)))
        (expect (re-find #"a1 image/png call-1 it1 turn-1" so))
        (expect (re-find #"bytes PNGDATA chart.png 7" so))))
  (it
    "addresses one artifact by its filename exactly as by its id"
    (let
      [pctx
       (ctx-with-root (temp-root))

       sink
       (atom [])

       out
       (binding
         [mpl-capture/*attachment-reader*
          (fake-reader)

          mpl-capture/*attachment-reinspection-sink*
          sink]

         (block pctx
                (str "print(read_attachment('chart.png').decode('utf-8'))\n"
                     "print(get_attachment('chart.png')['id'])\n"
                     "print(show_attachment('chart.png')['id'])\n"
                     "try:\n" "    read_attachment('nope.png')\n"
                     "except LookupError as e:\n"
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
      (let
        [pctx
         (ctx-with-root (temp-root))

         sink
         (atom [])

         out
         (binding
           [mpl-capture/*attachment-reader*
            (fake-reader)

            mpl-capture/*attachment-reinspection-sink*
            sink]

           (block pctx "r = show_attachment('a1')\nprint(r['id'], r['media_type'])"))]

        (expect (nil? (:error out)))
        (expect (re-find #"a1 image/png" (str (:stdout out))))
        (expect (= ["a1"] (mapv :id @sink)))))
  (it "coalesces repeated reinspection requests by durable attachment id"
      (let
        [sink
         (atom [])

         att
         {:id "a1" :media-type "image/png"}]

        (binding [mpl-capture/*attachment-reinspection-sink* sink]
          (mpl-capture/queue-reinspection! att)
          (mpl-capture/queue-reinspection! att))
        (expect (= ["a1"] (mapv :id @sink)))))
  (it "raises on an unknown id"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (binding [mpl-capture/*attachment-reader* (fake-reader)]
           (block pctx
                  (str "try:\n"
                       "    read_attachment('zzz')\n" "    print('NO-RAISE')\n"
                       "except Exception as e:\n"
                       "    print('RAISED', 'no attachment' in str(e))\n")))]

        (expect (nil? (:error out)))
        (expect (re-find #"RAISED True" (str (:stdout out))))))
  (it "raises when no attachment reader is bound (outside a driven read)"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (block pctx
                (str "try:\n"
                     "    list_attachments()\n" "    print('NO-RAISE')\n"
                     "except Exception as e:\n"
                     "    print('RAISED', 'no active attachment reader' in str(e))\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"RAISED True" (str (:stdout out)))))))

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
      (let
        [out (attach-out (str "attach(b'PNGDATA', 'chart.png', media_type='image/png', "
                              "audience='user')\n"))]
        (expect (nil? (:error out)))
        (expect (= "user" (:audience (:row out))))))
  (it "says NOTHING to the human for audience \"model\""
      (let
        [out (attach-out (str "attach(b'PNGDATA', 'chart.png', media_type='image/png', "
                              "audience='model', label='for my own eyes')\n"))]
        (expect (nil? (:error out)))
        (expect (= 1 (count (:attachments out))))
        (expect (= "model" (:audience (:row out))))
        ;; Staying silent IS the feature: no fence, no caption line, nothing in
        ;; the block naming an artifact the human was never meant to review.
        (expect (not (re-find #"chart\.png" (str (:stdout out)))))))
  (it "refuses an audience outside the closed vocabulary"
      (let
        [out (attach-out (str "try:\n"
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
    (let
      [pctx
       (ctx-with-root (temp-root))

       out
       (binding [mpl-capture/*attachment-reader* (versioned-reader)]
         (block pctx
                (str "vs = list_attachments('chart.png')\n"
                     "print([v['version'] for v in vs], [v['id'] for v in vs])\n"
                     "print(get_attachment('chart.png')['id'])\n"
                     "print(get_attachment('chart.png', 1)['id'])\n"
                     "print(get_attachment('chart.png', -2)['id'])\n"
                     "print(get_attachment('v2')['id'])\n" "print(list_attachments('nope.png'))\n"
                     "try:\n" "    get_attachment('chart.png', 9)\n"
                     "except LookupError as e:\n"
                     "    print('RAISED', 'versions: 1, 2, 3' in str(e))\n"
                     "try:\n" "    get_attachment('nope.png')\n"
                     "except LookupError:\n" "    print('MISSING')\n")))

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

(defdescribe
  attach-continuity-guidance-test
  "Versioning only pays off if the WRITE side reaches for it: a model that names
   the next cut `report_v2.png` gets two loose artifacts and the thread it was
   supposed to continue is gone. So every surface a block reads BEFORE attaching
   — the shim description that rides the prompt, and the sandbox `doc()` plus
   `__doc__` of both attach twins — carries the same rule in the same words."
  (it "tells the write side to keep one document under one name"
      (let
        [shim
         (->> shim-attach/vis-extension
              :ext/sandbox-shims
              (filter #(= "attachments" (:shim/name %)))
              first)

         descr
         (str (:shim/description shim))]

        (expect (re-find #"SAME DOCUMENT, SAME NAME" descr))
        (expect (re-find #"(?i)next VERSION" descr))
        (expect (re-find #"(?i)different document" descr)))
      (let [pctx (ctx-with-root (temp-root))]
        (doseq [n ["attach"]]
          ;; The rule is on the callable's own docstring...
          (expect (true? (ev pctx (str "'SAME DOCUMENT, SAME NAME' in " n ".__doc__")))
                  (str n ".__doc__ must state the same-name rule"))
          ;; ...and on the `doc()` entry the model actually looks up.
          (expect (true? (ev pctx (str "'SAME DOCUMENT, SAME NAME' in doc('" n "')")))
                  (str "doc('" n "') must state the same-name rule"))))))
