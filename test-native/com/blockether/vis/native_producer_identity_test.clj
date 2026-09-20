(ns com.blockether.vis.native-producer-identity-test
  "Every vis process must mint its OWN journal producer identity.

   `producer-id` and `producer-pid` are top-level forms, and native-image runs
   top-level forms at BUILD time: the id and the pid were linked INTO the image,
   so every process started from that binary published under one identity and
   one long-dead pid. Siblings then read each other's journal lines as their own
   and every liveness marker looked orphaned to the next process that scanned
   the directory, which is what emptied `bus/live-turns` and made a project
   header say `1 live` while two sessions were running.

   Only the linked image can show this: on the JVM those forms are evaluated
   once per process and always look correct."
  (:require [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File RandomAccessFile]
           [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(def ^:private producer-fragment
  "A complete `\"_producer\":\"<uuid>\"` fragment. The PREFIX alone is a legitimate
   string constant — the image builds the marker from it at runtime — so only a
   prefix followed by a minted id proves the identity was frozen at build time."
  #"\"_producer\":\"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\"")

(def ^:private CHUNK_BYTES
  "Bytes read per pass. The image is ~350 MiB, so it is scanned in windows rather
   than loaded whole."
  (* 8 1024 1024))

(def ^:private OVERLAP_BYTES
  "Bytes each window re-reads, so a fragment straddling a window boundary is still
   matched whole."
  128)

(defn- baked-producer-ids
  "Every producer identity LINKED INTO `bin`, in file order."
  [^File bin]
  (with-open [raf (RandomAccessFile. bin "r")]
    (let [total (.length raf)]
      (loop [pos 0
             found []]

        (if (>= pos total)
          found
          (let [len (int (min (long CHUNK_BYTES) (- total pos)))
                buf (byte-array len)
                _ (do (.seek raf pos) (.readFully raf buf))
                window (String. buf StandardCharsets/ISO_8859_1)
                next-pos (if (< (+ pos len) total) (- (+ pos len) (long OVERLAP_BYTES)) total)]

            (recur next-pos (into found (re-seq producer-fragment window)))))))))

(defdescribe native-producer-identity-is-minted-at-runtime-test
             (it "links no journal producer identity into the image"
                 (let [^File bin
                       (#'native/require-binary)

                       baked
                       (distinct (baked-producer-ids bin))]

                   (expect (empty? baked)
                           (str "The image carries a build-time producer identity "
                                (pr-str (vec (take 3 baked)))
                                ". Every process started from it publishes under that id and"
                                " under the pid of the build, so a sibling's journal lines read"
                                " as our own and live-turn markers are reaped as orphans -"
                                " running sessions then disappear from the fleet's live count.")))))
