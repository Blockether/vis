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

(defdescribe
  mpl-capture-test
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
  (describe "record-file! size/extension filter"
            (it "captures normal writes but skips empty + noisy-extension files"
                (let
                  [dir
                   (java.nio.file.Files/createTempDirectory
                     "vis-filter-test"
                     (make-array java.nio.file.attribute.FileAttribute 0))

                   wf
                   (fn [name ^String s]
                     (let [p (.resolve dir ^String name)]
                       (spit (.toFile p) s)
                       p))

                   sink
                   (atom [])]

                  (binding
                    [cap/*attachment-sink*
                     sink

                     cap/*outbox-seen*
                     (atom #{})]

                    (cap/record-file! (wf "keep.txt" "hello"))
                    (cap/record-file! (wf "junk.pyc" "bytes"))
                    (cap/record-file! (wf "app.lock" "1"))
                    (cap/record-file! (wf "empty.dat" ""))
                    (cap/record-file! (wf "data.csv" "a,b")))
                  (expect (= ["keep.txt" "data.csv"] (mapv :filename @sink)))))
            ;; Regression: `tempfile.gettempdir()` probes the temp dir by creating a
            ;; random 8-character file with NO extension and writing four bytes into
            ;; it, and that probe surfaced in the session as an attachment chip — a
            ;; blocklist of suffixes cannot see a file that carries none.
            (it "skips nameless tempfile scratch but keeps a NAMED extensionless file"
                (let
                  [dir
                   (java.nio.file.Files/createTempDirectory
                     "vis-anon-test"
                     (make-array java.nio.file.attribute.FileAttribute 0))

                   wf
                   (fn [name ^String s]
                     (let [p (.resolve dir ^String name)]
                       (spit (.toFile p) s)
                       p))

                   sink
                   (atom [])]

                  (binding
                    [cap/*attachment-sink*
                     sink

                     cap/*outbox-seen*
                     (atom #{})]

                    (cap/record-file! (wf "486h_evp" "blat"))
                    (cap/record-file! (wf "tmpa1b2c3d4" "scratch"))
                    (cap/record-file! (wf "Makefile" "all:\n"))
                    ;; Eight lower-case letters is a WORD as often as a random draw, so the
                    ;; rule refuses to guess without a digit or an underscore.
                    (cap/record-file! (wf "manifest" "one: 1\n"))
                    (cap/record-file! (wf "report" "a real artifact")))
                  (expect (= ["Makefile" "manifest" "report"] (mapv :filename @sink)))))))
