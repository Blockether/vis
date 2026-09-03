(ns com.blockether.vis.tui.gif
  "Animated GIF decoding for terminal video verification, delegated to imaging."
  (:require [com.blockether.imaging :as imaging]))

(defn- gif?
  [^bytes data]
  (boolean (and data
                (>= (alength data) 6)
                (= "GIF8" (String. data 0 4 java.nio.charset.StandardCharsets/US_ASCII))
                (= (int \a) (bit-and (aget data 5) 0xff)))))

(defn- rgba->argb
  ^ints [^bytes data]
  (let [out (int-array (quot (alength data) 4))]
    (dotimes [pixel (alength out)]
      (let [offset (* pixel 4)]
        (aset out
              pixel
              (unchecked-int (bit-or (bit-shift-left (bit-and (aget data (+ offset 3)) 0xff) 24)
                                     (bit-shift-left (bit-and (aget data offset) 0xff) 16)
                                     (bit-shift-left (bit-and (aget data (+ offset 1)) 0xff) 8)
                                     (bit-and (aget data (+ offset 2)) 0xff))))))
    out))

(defn decode
  [^bytes data]
  (when (gif? data)
    (let [decoded (imaging/decode-gif data)]
      (-> decoded
          (update :loop-count long)
          (update :frames
                  (fn [frames]
                    (mapv (fn [frame]
                            {:delay-ms (long (:delay-ms frame))
                             :argb (rgba->argb ^bytes (:rgba frame))})
                          frames)))))))
