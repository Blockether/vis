(ns com.blockether.vis.internal.foundation.mpl-capture-test
  "Tests for explicit attachment collection, draining and the display cache."
  (:require [com.blockether.vis.internal.foundation.mpl-capture :as cap]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private img
  "A produced-image attachment passed to `record-attachment!`."
  {:kind "image"
   :media-type "image/png"
   :base64 "AQID"
   :size 3
   :filename "fig-1.png"
   :dims "640x480"})

(defdescribe mpl-capture-test
             (describe "record-attachment! + *attachment-sink*"
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
                             (let [[rec] (cap/drain sink)]
                               ;; Identity is stamped AT THE SINK — the id the
                               ;; row is stored under and the cut this name
                               ;; became — so the producer can address the
                               ;; artifact inside the block that made it.
                               (expect (= img (dissoc rec :id :version)))
                               (expect (string? (:id rec)))
                               (expect (= 1 (:version rec))))))
                       (it "returns nil for an empty sink (the block produced nothing)"
                           (expect (nil? (cap/drain (atom [])))))
                       (it "returns nil for a nil sink" (expect (nil? (cap/drain nil))))))

(defdescribe display-cache-file-test
             (it "writes one content-addressed file and reuses it for identical bytes"
                 (let [dir
                       (.toFile (java.nio.file.Files/createTempDirectory
                                  "vis-mpl-display"
                                  (make-array java.nio.file.attribute.FileAttribute 0)))

                       [a b]
                       (binding [cap/*display-home* (.getPath dir)]
                         [(cap/display-cache-file "fig-" "png" (.getBytes "picture"))
                          (cap/display-cache-file "fig-" "png" (.getBytes "picture"))])]

                   (expect (= (.getPath a) (.getPath b)))
                   (expect (= 1 (count (.listFiles dir))))
                   (expect (= "picture" (slurp a)))))
             ;; Regression: a reused cache file kept the stamp of its FIRST render, so
             ;; `housekeeping/sweep-stale!` would age out a picture that had been rendered
             ;; again this morning because its content was first seen a month ago.
             (it "re-stamps the file it reuses, so its age is the last render and not the first"
                 (let [dir
                       (.toFile (java.nio.file.Files/createTempDirectory
                                  "vis-mpl-display-age"
                                  (make-array java.nio.file.attribute.FileAttribute 0)))

                       bs
                       (.getBytes "picture")

                       old
                       (- (System/currentTimeMillis) (* 40 86400000))]

                   (binding [cap/*display-home* (.getPath dir)]
                     (let [first-render (cap/display-cache-file "fig-" "png" bs)]
                       (.setLastModified first-render old)
                       (let [again (cap/display-cache-file "fig-" "png" bs)]
                         (expect (= (.getPath first-render) (.getPath again)))
                         (expect (< old (.lastModified again)))))))))
