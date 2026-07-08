(ns com.blockether.vis.internal.foundation.shim-attach-test
  "The generic attachment shim (`vis_attach` / `vis_attach_bytes`) installed into
   every sandbox context via the sandbox-shim mechanism. A tool PRODUCES an
   artifact and hands it to `vis_attach`; the bytes are captured AT THE SOURCE
   into the per-block sink (drained into the block outcome's `:images`, which the
   loop persists as `:attachments`), with the media-type sniffed from magic bytes
   / extension / a utf-8 probe. No stdout fence, no parsing."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

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
   image sink is bound and drained into `:images`)."
  [^Context pctx code]
  (ep/run-python-block pctx code))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defdescribe
  vis-attach-bytes-capture-test
  (it "records an in-memory artifact into the block's :images with sniffed type"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "r = vis_attach_bytes('a,b\\n1,2\\n', 'data.csv')\nprint(r['size'])")

            [att]
            (:images out)]

        (expect (nil? (:error out)))
        (expect (= 1 (count (:images out))))
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
            (:images out)]

        (expect (nil? (:error out)))
        (expect (= "image/png" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it "falls back to text/plain for undecorated utf-8 bytes"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx "vis_attach_bytes('just words', 'note')\n")

            [att]
            (:images out)]

        (expect (= "text/plain" (:media-type att)))
        (expect (= "file" (:kind att)))))
  (it "honours explicit kind / media_type overrides"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   "vis_attach_bytes('x', 'weird.bin', kind='image', media_type='image/svg+xml')\n")

            [att]
            (:images out)]

        (expect (= "image/svg+xml" (:media-type att)))
        (expect (= "image" (:kind att)))))
  (it "collects MANY artifacts from one block, in order"
      (let [pctx
            (ctx-with-root (temp-root))

            out
            (block pctx
                   (str "vis_attach_bytes('1', 'a.txt')\n" "vis_attach_bytes('2', 'b.json')\n"))]

        (expect (= ["a.txt" "b.json"] (mapv :filename (:images out)))))))

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
                       (:images out)]

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
                   (expect (empty? (:images out))))))

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
