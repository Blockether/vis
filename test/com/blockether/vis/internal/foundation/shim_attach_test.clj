(ns com.blockether.vis.internal.foundation.shim-attach-test
  "The generic attachment shim (`vis_attach` / `vis_attach_bytes`) installed into
   every sandbox context via the sandbox-shim mechanism. A tool PRODUCES an
   artifact and hands it to `vis_attach`; the bytes are captured AT THE SOURCE
   into the per-block sink (drained into the block outcome's `:attachments`, which the
   loop persists as `:attachments`), with the media-type sniffed from magic bytes
   / extension / a utf-8 probe. No stdout fence, no parsing."
  (:require [com.blockether.vis.internal.env-python :as ep]
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
  vis-attach-bytes-capture-test
  (it "records an in-memory artifact into the block's :attachments with sniffed type"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "r = vis_attach_bytes('a,b\\n1,2\\n', 'data.csv')\nprint(r['size'])")

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= 1 (count (:attachments out))))
        (expect (= "text/csv" (:media-type att)))
        (expect (= "file" (:kind att)))
        (expect (= "data.csv" (:filename att)))
        (expect (= 8 (:size att)))
        ;; base64 round-trips to the original bytes
        (expect (= "a,b\n1,2\n"
                   (String. (.decode (java.util.Base64/getDecoder) ^String (:base64 att)))))))
  (it "detects an image by magic bytes -> kind image, image/png"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "png = bytes([0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A]) + b'body'\n"
                        "vis_attach_bytes(png, 'fig.dat')\n"))

            [att]
            (:attachments out)]

        (expect (nil? (:error out)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it "falls back to text/plain for undecorated utf-8 bytes"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "vis_attach_bytes('just words', 'note')\n")

            [att]
            (:attachments out)]

        (expect (= "text/plain" (:media-type att)))
        (expect (= "file" (:kind att)))))
  (it "honours explicit kind / media_type overrides"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   "vis_attach_bytes('x', 'weird.bin', kind='image', media_type='image/svg+xml')\n")

            [att]
            (:attachments out)]

        (expect (= "image/svg+xml" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it "collects MANY artifacts from one block, in order"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "vis_attach_bytes('1', 'a.txt')\n" "vis_attach_bytes('2', 'b.json')\n"))]

        (expect (= ["a.txt" "b.json"] (mapv :filename (:attachments out)))))))

(defdescribe vis-attach-path-test
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
                                   "vis_attach('"
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
                                   "    vis_attach('/etc/hosts')\n" "    print('NO-RAISE')\n"
                                   "except Exception as e:\n"
                                   "    print('RAISED', type(e).__name__)\n"))]

                   (expect (nil? (:error out)))
                   (expect (re-find #"RAISED" (str (:stdout out))))
                   (expect (empty? (:attachments out))))))

(defdescribe vis-attach-discovery-test
             (it "surfaces vis_attach / vis_attach_bytes via apropos and doc"
                 (let [pctx (ctx-with-root (temp-root))]
                   (expect (= ["vis_attach" "vis_attach_bytes"]
                              (vec (ev pctx "sorted(apropos('vis_attach'))"))))
                   (expect (true? (ev pctx "'callable' in doc('vis_attach')")))))
             (it "raises when called with no active capture sink (outside a driven block)"
                 (let [pctx (ctx-with-root (temp-root))]
                   ;; a bare .eval does NOT bind the per-block sink, so the bridge refuses
                   (expect (re-find #"no active capture sink"
                                    (ev pctx
                                        (str "\ntry:\n" "    vis_attach_bytes('x', 'y.txt')\n"
                                             "    _r = 'NO-RAISE'\n" "except Exception as e:\n"
                                             "    _r = str(e)\n" "_r")))))))

(defdescribe
  vis-outbox-capture-test
  (it "captures a file WRITTEN into $VIS_OUTBOX as an attachment (no vis_attach call)"
      (let [pctx
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
  (it "leaves a plain workspace-root write UNcaptured (only $VIS_OUTBOX is tapped)"
      (let [root
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
        (expect (empty? (:attachments out))))))

(defn- conveying-gather
  "Faithful replica of loop.clj's `gather-fn`: submit each thunk to a
   virtual-thread executor wrapped in `bound-fn*` (which snapshots the caller's
   thread-local binding frame — INCLUDING `*attachment-sink*` — and replays it
   on the worker), then `.get` in submission order. This IS the conveyance
   under test: an artifact produced inside `await gather(...)` runs on a virtual
   thread yet must still reach the block's `:attachments`."
  [executor]
  (fn [& thunks]
    (let [thunks
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
   that thread so `vis_attach_bytes` still lands in the block's `:attachments`
   — no silent drop, no nil sink."
  (it "captures every gather-produced artifact into the block's :attachments"
      (let [ex
            (Executors/newVirtualThreadPerTaskExecutor)

            pctx
            (ctx-with-gather (temp-root) ex)

            out
            (try (block pctx
                        (str "async def mk(name):\n"
                             "    return vis_attach_bytes('payload', name)\n"
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
