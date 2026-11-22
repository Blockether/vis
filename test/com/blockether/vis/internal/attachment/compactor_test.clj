(ns com.blockether.vis.internal.attachment.compactor-test
  "What the database keeps after the image compactor: the step stores an image as it
   arrived, and the background pass leaves the same picture in fewer bytes."
  (:require [com.blockether.imaging :as imaging]
            [com.blockether.vis.internal.attachment.compactor :as compactor]
            [com.blockether.vis.internal.attachment.storage :as as]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.util Arrays Base64)))

(h/use-mem-store!)

(defn- gradient-png
  "A real 300x200 PNG from a speed-tuned encoder, larger than it needs to be."
  ^bytes []
  (let [img (imaging/blank 300 200 "white")]
    (dotimes [x 300]
      (dotimes [y 200]
        (imaging/put-pixel! img x y [(mod (* x 7) 256) (mod (* y 11) 256) (mod (+ x y) 256) 255])))
    (imaging/encode img :png)))

(defn- pixels-of ^bytes [^bytes encoded] (imaging/pixels (imaging/decode encoded)))

(defn- rows
  [s]
  (h/raw-query s
               {:select [:id :bytes :size_bytes :compaction_pending]
                :from :session_attachment
                :order-by [:position]}))

(defn- stored-turn!
  "Store one turn with `payloads` through the offload rail; answer its rows."
  [s payloads]
  (let [cid (h/store-session! s {:channel :cli})
        atts (as/offload-attachments
               (mapv (fn [^bytes b]
                       {:kind "image"
                        :media-type "image/png"
                        :size (alength b)
                        :base64 (.encodeToString (Base64/getEncoder) b)})
                     payloads))]
    (persistance/db-store-session-turn! s
                                        {:parent-session-id cid
                                         :user-request "look"
                                         :attachments atts})
    (rows s)))

(defdescribe
  image-compactor-test
  (it "stores the picture as it arrived, then compacts it in the background"
      (let [s (h/store)
            png (gradient-png)
            [before] (stored-turn! s [png])
            n (compactor/drain! s)
            [after] (rows s)
            ^bytes stored (:bytes after)]
        ;; The step waits for no optimiser: the row holds the original bytes.
        (expect (Arrays/equals png ^bytes (:bytes before)))
        (expect (= 1 (:compaction_pending before)))
        (expect (= 1 n))
        (expect (< (alength stored) (alength png)))
        (expect (= (alength stored) (:size_bytes after)))
        (expect (= 0 (:compaction_pending after)))
        (expect (Arrays/equals (pixels-of png) (pixels-of stored)))
        ;; A second pass finds no work and changes nothing.
        (expect (= 0 (compactor/drain! s)))
        (expect (Arrays/equals stored ^bytes (:bytes (first (rows s)))))))
  (it "keeps bytes that the optimiser cannot read or cannot make smaller"
      (let [s (h/store)
            claimed (.getBytes (apply str (repeat 3000 "p")) "UTF-8")
            tight (imaging/optimize (gradient-png))
            _ (stored-turn! s [claimed tight])
            _ (compactor/drain! s)
            after (rows s)]
        (expect (= [0 0] (mapv :compaction_pending after)))
        (expect (Arrays/equals claimed ^bytes (:bytes (first after))))
        (expect (Arrays/equals tight ^bytes (:bytes (second after))))))
  (it "keeps bytes that changed while the compactor worked on them"
      (let [s (h/store)
            png (gradient-png)
            [row] (stored-turn! s [png])
            other (doto (aclone png) (aset-byte 100 (unchecked-byte (inc (aget png 100)))))
            _ (h/raw-query s {:update :session_attachment :set {:bytes other} :where [:= :id (:id row)]})
            _ (persistance/db-finish-attachment-compaction! s (:id row) png (byte-array [1 2 3]))
            [after] (rows s)]
        (expect (Arrays/equals other ^bytes (:bytes after)))
        (expect (= 0 (:compaction_pending after))))))
