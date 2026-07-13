(ns com.blockether.vis.internal.foundation.shim-attach-test
  "The generic attachment shim (`vis_attach` / `vis_attach_bytes`) installed into
   every sandbox context via the sandbox-shim mechanism. A tool PRODUCES an
   artifact and hands it to `vis_attach`; the bytes are captured AT THE SOURCE
   into the per-block sink (drained into the block outcome's `:attachments`, which the
   loop persists as `:attachments`), with the media-type sniffed from magic bytes
   / extension / a utf-8 probe. No stdout fence, no parsing."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
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
             (it "surfaces vis_attach / vis_attach_bytes / vis_attachments via apropos and doc"
                 (let [pctx (ctx-with-root (temp-root))]
                   (expect (= ["vis_attach" "vis_attach_bytes" "vis_attachments"]
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
  (it "captures a confined write under a system temp root too (scratch tap, not just $VIS_OUTBOX)"
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

(defn- fake-reader
  "In-memory attachment reader holding ONE image artifact (id a1) — the shape
   `run-python-code` binds around a block so `vis_attachments` /
   `vis_read_attachment` can re-fetch prior session artifacts."
  []
  {:list (fn []
           [{:id "a1"
             :filename "chart.png"
             :media-type "image/png"
             :kind "image"
             :size 7
             :position 0
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
              :storage-uri nil}))})

(defdescribe
  vis-attachments-reader-test
  "Read-back twins: with `*attachment-reader*` bound, `vis_attachments()` lists
   the session's artifacts (snake_case keys) and `vis_read_attachment(id)` fetches
   one back as bytes through the confined sandbox. Unbound, both raise a clear
   RuntimeError instead of silently returning nothing."
  (it "lists metadata and reads the bytes back"
      (let
        [pctx
         (ctx-with-root (temp-root))

         out
         (binding [mpl-capture/*attachment-reader* (fake-reader)]
           (block
             pctx
             (str
               "a = vis_attachments()[0]\n" "r = vis_read_attachment('a1')\n"
               "print(a['id'], a['media_type'], a['tool_call_id'], a['iteration_id'])\n"
               "print(r['bytes'].decode('utf-8'), r['media_type'], r['filename'], r['size'])\n")))

         so
         (str (:stdout out))]

        (expect (nil? (:error out)))
        (expect (re-find #"a1 image/png call-1 it1" so))
        (expect (re-find #"PNGDATA image/png chart.png 7" so))))
  (it "raises on an unknown id"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (binding [mpl-capture/*attachment-reader* (fake-reader)]
              (block pctx
                     (str "try:\n"
                          "    vis_read_attachment('zzz')\n" "    print('NO-RAISE')\n"
                          "except Exception as e:\n"
                          "    print('RAISED', 'no attachment' in str(e))\n")))]

        (expect (nil? (:error out)))
        (expect (re-find #"RAISED True" (str (:stdout out))))))
  (it "raises when no attachment reader is bound (outside a driven read)"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "try:\n"
                        "    vis_attachments()\n" "    print('NO-RAISE')\n"
                        "except Exception as e:\n"
                        "    print('RAISED', 'no active attachment reader' in str(e))\n"))]

        (expect (nil? (:error out)))
        (expect (re-find #"RAISED True" (str (:stdout out)))))))
