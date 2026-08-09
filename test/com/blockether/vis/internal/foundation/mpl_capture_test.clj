(ns com.blockether.vis.internal.foundation.mpl-capture-test
  "The image SINK feeding the iteration-attachment rail: the matplotlib render fn
   calls `record-image!` right where it already holds the PNG bytes, and
   `run-python-block` binds `*image-sink*` around one block's eval then `drain`s
   it into the outcome's `:attachments`. There is NO stdout-fence parsing anywhere —
   these cover append-into-bound-sink (order preserved), the unbound no-op, and
   the drain shape."
  (:require [com.blockether.vis.internal.foundation.mpl-capture :as cap]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private img
  "A produced-image attachment map, the shape the render fn hands `record-image!`."
  {:kind "image"
   :media-type "image/png"
   :base64 "AQID"
   :size 3
   :filename "fig-1.png"
   :dims "640x480"})

(defn- captured
  "The filenames the temp/outbox tap actually KEPT after `named-contents`
   (`[[name content] …]`) were written into one fresh temp dir — the whole
   incidental-capture filter in a single call."
  [named-contents]
  (let
    [dir
     (java.nio.file.Files/createTempDirectory "vis-capture-test"
                                              (make-array java.nio.file.attribute.FileAttribute 0))

     sink
     (atom [])]

    (binding
      [cap/*attachment-sink*
       sink

       cap/*outbox-seen*
       (atom #{})]

      (doseq [[name content] named-contents]
        (let [p (.resolve dir ^String name)]
          (spit (.toFile p) ^String content)
          (cap/record-file! p))))
    (mapv :filename @sink)))

(defdescribe mpl-capture-test
             (describe "record-image! + *image-sink*"
                       (it "appends into the bound per-block sink, in call order"
                           (let [sink (atom [])]
                             (binding [cap/*attachment-sink* sink]
                               (cap/record-attachment! img)
                               (cap/record-attachment! (assoc img :filename "fig-2.png")))
                             (expect (= 2 (count @sink)))
                             (expect (= ["fig-1.png" "fig-2.png"] (mapv :filename @sink)))))
                       (it "is a silent no-op (returns nil) when no sink is bound"
                           (expect (nil? (cap/record-attachment! img))))
                       (it "never throws when the bound sink is nil"
                           (binding [cap/*attachment-sink* nil]
                             (expect (nil? (cap/record-attachment! img))))))
             (describe "drain"
                       (it "returns the collected images as a plain vector"
                           (let [sink (atom [])]
                             (binding [cap/*attachment-sink* sink]
                               (cap/record-attachment! img))
                             (expect (= [img] (cap/drain sink)))))
                       (it "returns nil for an empty sink (the block produced nothing)"
                           (expect (nil? (cap/drain (atom [])))))
                       (it "returns nil for a nil sink" (expect (nil? (cap/drain nil)))))
             (describe
               "record-file! — what the incidental tap refuses"
               (it "captures normal writes but skips empty + noisy-extension files"
                   (expect (= ["keep.txt" "data.csv"]
                              (captured [["keep.txt" "hello"] ["junk.pyc" "bytes"] ["app.lock" "1"]
                                         ["empty.dat" ""] ["data.csv" "a,b"]]))))
               ;; Regression: `tempfile.gettempdir()` probes the temp dir by creating a
               ;; random 8-character file with NO extension and writing four bytes into it,
               ;; and that probe surfaced in the session as an attachment chip — a blocklist
               ;; of suffixes cannot see a file that carries none.
               (it "skips nameless tempfile scratch but keeps a NAMED extensionless file"
                   ;; Eight lower-case letters is a WORD as often as a random draw, so the
                   ;; rule refuses to guess without a digit or an underscore.
                   (expect (= ["Makefile" "manifest" "report"]
                              (captured [["486h_evp" "blat"] ["tmpa1b2c3d4" "scratch"]
                                         ["Makefile" "all:\n"] ["manifest" "one: 1\n"]
                                         ["report" "a real artifact"]]))))
               ;; Regression: a build that lands in a temp dir — the package it assembles,
               ;; the native libraries it links, a heap dump, a source map, a digest sidecar
               ;; — filled the session with chips for files no reader ever opens.
               (it "skips toolchain build output: packages, native libraries, dumps, sidecars"
                   (expect (= ["notes.md"]
                              (captured [["app.jar" "assembled"] ["libvis.dylib" "linked"]
                                         ["addon.node" "linked"] ["heap.hprof" "dumped"]
                                         ["bundle.js.map" "{\"version\":3}"]
                                         ["dist.tsbuildinfo" "{}"] ["app.jar.sha256" "deadbeef"]
                                         ["notes.md" "# what the run found\n"]]))))))
