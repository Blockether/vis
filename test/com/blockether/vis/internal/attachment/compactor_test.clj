(ns com.blockether.vis.internal.attachment.compactor-test
  "What the database keeps after the attachment compactor: the step stores a payload
   as it arrived, and the background pass keeps the same payload in fewer bytes."
  (:require [com.blockether.imaging :as imaging]
            [com.blockether.vis.internal.attachment.compactor :as compactor]
            [com.blockether.vis.internal.attachment.image-convert :as image-convert]
            [com.blockether.vis.internal.attachment.storage :as as]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.persistance.sqlite.core :as sqlite-core]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io ByteArrayOutputStream DataOutputStream)
           (java.nio.charset StandardCharsets)
           (java.util Arrays Base64)
           (java.util.zip CRC32)))

(h/use-mem-store!)

(defn- gradient-png
  "A real 300x200 PNG from a speed-tuned encoder, larger than it needs to be."
  ^bytes []
  (let [img (imaging/blank 300 200 "white")]
    (dotimes [x 300]
      (dotimes [y 200]
        (imaging/put-pixel! img x y [(mod (* x 7) 256) (mod (* y 11) 256) (mod (+ x y) 256) 255])))
    (imaging/encode img :png)))

(defn- ascii ^bytes [^String s] (.getBytes s StandardCharsets/ISO_8859_1))

(defn- png-chunk
  "One PNG chunk of `type` with `data`, length and CRC included."
  ^bytes [^String type ^bytes data]
  (let [out
        (ByteArrayOutputStream.)

        dout
        (DataOutputStream. out)

        crc
        (CRC32.)]

    (.writeInt dout (alength data))
    (.write dout (ascii type))
    (.write dout data)
    (.update crc (ascii type))
    (.update crc data)
    (.writeInt dout (unchecked-int (.getValue crc)))
    (.toByteArray out)))

(def ^:private exif-rotated
  "A big-endian TIFF block with one tag: orientation 6, shown turned 90 degrees."
  (byte-array (map unchecked-byte
                   [0x4d 0x4d 0 0x2a 0 0 0 8 0 1 0x01 0x12 0 3 0 0 0 1 0 6 0 0 0 0 0 0 0 0])))

(defn- with-metadata-png
  "The gradient PNG with `eXIf` orientation, `gAMA` and `sBIT` chunks after its IHDR."
  ^bytes []
  (let [^bytes png
        (gradient-png)

        head
        (+ 8 25)

        out
        (ByteArrayOutputStream.)]

    (.write out png 0 head)
    (.write out (png-chunk "gAMA" (byte-array (map unchecked-byte [0 0 0xb1 0x8f]))))
    (.write out (png-chunk "eXIf" exif-rotated))
    (.write out (png-chunk "sBIT" (byte-array [5 6 5 8])))
    (.write out png head (- (alength png) head))
    (.toByteArray out)))

(defn- png-chunk-types
  [^bytes png]
  (loop [at
         8

         types
         #{}]

    (if (>= (+ at 8) (alength png))
      types
      (let [n (bit-or (bit-shift-left (bit-and (aget png at) 0xff) 24)
                      (bit-shift-left (bit-and (aget png (+ at 1)) 0xff) 16)
                      (bit-shift-left (bit-and (aget png (+ at 2)) 0xff) 8)
                      (bit-and (aget png (+ at 3)) 0xff))]
        (recur (+ at 12 n)
               (conj types (String. png (int (+ at 4)) 4 StandardCharsets/ISO_8859_1)))))))

(defn- rotated-jpeg
  "A JPEG with an EXIF orientation segment right after its SOI marker."
  ^bytes []
  (let [^bytes jpeg
        (imaging/encode (imaging/decode (gradient-png)) :jpeg)

        app1
        (byte-array (concat (ascii "Exif") [0 0] exif-rotated))

        n
        (+ 2 (alength app1))

        out
        (ByteArrayOutputStream.)]

    (.write out jpeg 0 2)
    (.write out
            (byte-array (map unchecked-byte [0xff 0xe1 (bit-shift-right n 8) (bit-and n 0xff)])))
    (.write out app1)
    (.write out jpeg 2 (- (alength jpeg) 2))
    (.toByteArray out)))

(defn- pixels-of ^bytes [^bytes encoded] (imaging/pixels (imaging/decode encoded)))

(defn- rows
  [s]
  (h/raw-query s
               {:select [:id :bytes :size_bytes :compaction_pending :storage_codec]
                :from :session_attachment
                :order-by [:position]}))

(defn- read-back
  "The payload that a reader of the attachment `id` gets."
  ^bytes [s id]
  (.decode (Base64/getDecoder) ^String (:base64 (persistance/db-read-attachment s id))))

(defn- stored-turn!
  "Store one turn with `payloads`, `[media-type bytes]` pairs, through the offload
   rail; answer its session id."
  [s payloads]
  (let [cid
        (h/store-session! s {:channel :cli})

        atts
        (as/offload-attachments (mapv (fn [[media-type ^bytes b]]
                                        {:kind "image"
                                         :media-type media-type
                                         :size (alength b)
                                         :base64 (.encodeToString (Base64/getEncoder) b)})
                                      payloads))]

    (persistance/db-store-session-turn!
      s
      {:parent-session-id cid :user-request "look" :attachments atts})
    cid))

(def ^:private page
  "An HTML page as a tool writes it: long and repetitive."
  (ascii (apply str (repeat 400 "<tr><td class=\"cell\">value</td><td>more text</td></tr>\n"))))

(defdescribe
  attachment-compactor-test
  (it "stores the picture as it arrived, then compacts it in the background"
      (let [s
            (h/store)

            png
            (gradient-png)

            _
            (stored-turn! s [["image/png" png]])

            [before]
            (rows s)

            {n :rows}
            (compactor/drain! s)

            [after]
            (rows s)

            stored
            (read-back s (:id after))]

        (expect (Arrays/equals png ^bytes (:bytes before)))
        (expect (= 1 (:compaction_pending before)))
        (expect (= 1 n))
        (expect (< (alength stored) (alength png)))
        (expect (= (alength stored) (:size_bytes after)))
        (expect (= 0 (:compaction_pending after)))
        (expect (Arrays/equals (pixels-of png) (pixels-of stored)))
        (expect (= {:rows 0 :more? false} (compactor/drain! s)))))
  (it "keeps the orientation and the gamma of a PNG"
      (let [s
            (h/store)

            png
            (with-metadata-png)

            _
            (stored-turn! s [["image/png" png]])

            _
            (compactor/drain! s)

            stored
            (read-back s (:id (first (rows s))))]

        (expect (< (alength stored) (alength png)))
        (expect (contains? (png-chunk-types stored) "eXIf"))
        (expect (contains? (png-chunk-types stored) "gAMA"))
        (expect (contains? (png-chunk-types stored) "sBIT"))))
  (it "makes a JPEG smaller and keeps its pixels and its orientation"
      (let [s
            (h/store)

            jpeg
            (rotated-jpeg)

            _
            (stored-turn! s [["image/jpeg" jpeg]])

            _
            (compactor/drain! s)

            stored
            (read-back s (:id (first (rows s))))]

        (expect (< (alength stored) (alength jpeg)))
        (expect (Arrays/equals (pixels-of jpeg) (pixels-of stored)))
        (expect (= (seq (Arrays/copyOfRange jpeg 0 40)) (seq (Arrays/copyOfRange stored 0 40))))))
  (it "stores a compressible payload as zstd and gives back the same bytes"
      (let [s
            (h/store)

            cid
            (stored-turn! s [["text/html" page]])

            _
            (compactor/drain! s)

            [row]
            (rows s)]

        (expect (= "zstd" (:storage_codec row)))
        (expect (< (* 4 (alength ^bytes (:bytes row))) (alength ^bytes page)))
        (expect (= (alength ^bytes page) (:size_bytes row)))
        (expect (Arrays/equals ^bytes page (read-back s (:id row))))
        (expect (some #(Arrays/equals ^bytes page ^bytes %)
                      (vals (:blobs (persistance/db-export-session-tree s cid)))))))
  (it "keeps a payload out of the transcript by its size, not by its stored frame"
      (let [s
            (h/store)

            _
            (stored-turn! s [["text/html" page]])

            _
            (compactor/drain! s)

            [row]
            (rows s)

            soul
            (:session_turn_soul_id
              (first (h/raw-query s {:select [:session_turn_soul_id] :from :session_attachment})))

            listed
            (get (persistance/db-list-turns-attachments s
                                                        [soul]
                                                        {:inline-limit (alength ^bytes
                                                                                (:bytes row))})
                 soul)]

        (expect (= 1 (count listed)))
        (expect (nil? (:base64 (first listed))))
        (expect (= (alength ^bytes page) (:size (first listed))))))
  (it
    "keeps the other rows moving when the optimiser fails on one"
    (let [s
          (h/store)

          png
          (gradient-png)

          _
          (stored-turn! s [["image/png" png] ["text/html" page]])]

      (with-redefs [image-convert/compact
                    (fn [data]
                      (if (Arrays/equals png ^bytes data) (throw (ex-info "The decoder stopped" {})) data))]
        (expect (= {:rows 2 :more? false} (compactor/drain! s))))
      (expect (= [["raw" 0] ["zstd" 0]] (mapv (juxt :storage_codec :compaction_pending) (rows s))))
      (expect (Arrays/equals png (read-back s (:id (first (rows s))))))))
  (it "never reads again a payload that stopped the process"
      (let [s
            (h/store)

            _
            (stored-turn! s [["image/png" (gradient-png)]])]

        (expect (some? (persistance/db-claim-next-attachment-compaction! s)))
        (expect (= {:rows 0 :more? false} (compactor/drain! s)))))
  (it "gives the janitor back after its budget, with rows still waiting"
      (let [s
            (h/store)

            _
            (stored-turn! s [["text/html" page] ["text/html" page]])]

        (expect (= {:rows 1 :more? true} (compactor/drain! s 0)))
        (expect (= {:rows 1 :more? false} (compactor/drain! s)))))
  (it "compacts the payloads that an older store kept without a codec"
      (let [s
            (h/store)

            _
            (stored-turn! s [["text/html" page]])

            ds
            (:datasource s)

            rewrites
            @#'sqlite-core/store-rewrites]

        (h/raw-query s {:update :session_attachment :set {:compaction_pending 0}})
        (h/raw-query s
                     {:raw (str "PRAGMA user_version = "
                                (.indexOf ^java.util.List rewrites
                                          @#'sqlite-core/queue-attachment-compaction!))})
        (#'sqlite-core/rewrite-older-store! ds)
        (expect (= {:rows 1 :more? false} (compactor/drain! s)))
        (expect (= "zstd" (:storage_codec (first (rows s)))))))
  (it "moves the payload of an older store to the last column and keeps each row"
      (let [s
            (h/store)

            columns
            #(mapv :name (h/raw-query s {:raw "SELECT name FROM pragma_table_info('session_attachment')"}))

            indexes
            #(set (map :name
                       (h/raw-query s
                                    {:raw (str "SELECT name FROM sqlite_master WHERE type = 'index'"
                                               " AND tbl_name = 'session_attachment' AND sql IS NOT NULL")})))

            held
            (indexes)

            rewrites
            @#'sqlite-core/store-rewrites]

        ;; An older store: a column that SQLite added after the payload.
        (h/raw-query s {:raw "ALTER TABLE session_attachment DROP COLUMN reference"})
        (h/raw-query s {:raw "ALTER TABLE session_attachment ADD COLUMN reference TEXT"})
        (stored-turn! s [["text/html" page]])
        (h/raw-query s {:update :session_attachment :set {:reference "[FILE #1]"}})
        (expect (= "reference" (peek (columns))))
        (h/raw-query s {:raw (str "PRAGMA user_version = " (dec (count rewrites)))})
        (#'sqlite-core/rewrite-older-store! (:datasource s))
        (expect (= "bytes" (peek (columns))))
        (expect (= held (indexes)))
        (expect (= "[FILE #1]" (:reference (first (h/raw-query s {:select [:reference] :from :session_attachment})))))
        (expect (Arrays/equals ^bytes page (read-back s (:id (first (rows s))))))))
  (it "keeps bytes that changed while the compactor worked on them"
      (let [s
            (h/store)

            png
            (gradient-png)

            _
            (stored-turn! s [["image/png" png]])

            [row]
            (rows s)

            other
            (doto (aclone png) (aset-byte 100 (unchecked-byte (inc (aget png 100)))))

            _
            (h/raw-query
              s
              {:update :session_attachment :set {:bytes other} :where [:= :id (:id row)]})

            _
            (persistance/db-finish-attachment-compaction!
              s
              (:id row)
              png
              {:bytes (byte-array [1 2 3]) :codec "raw" :size 3})

            [after]
            (rows s)]

        (expect (Arrays/equals other ^bytes (:bytes after))))))
