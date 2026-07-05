(ns com.blockether.vis.ext.channel-tui.terminal-image-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe capability-detection-test
  (it "kitty / ghostty / wezterm / warp all speak the Kitty protocol"
    (expect (= :kitty (:images (timg/detect-capabilities {"TERM_PROGRAM" "kitty"}))))
    (expect (= :kitty (:images (timg/detect-capabilities {"TERM_PROGRAM" "ghostty"}))))
    (expect (= :kitty (:images (timg/detect-capabilities {"WEZTERM_PANE" "1"}))))
    (expect (= :kitty (:images (timg/detect-capabilities {"WARP_SESSION_ID" "x"})))))

  (it "iTerm2 is detected from its session env"
    (expect (= :iterm2 (:images (timg/detect-capabilities {"ITERM_SESSION_ID" "w0"})))))

  (it "tmux / screen / unknown terminals report no image support"
    (expect (nil? (:images (timg/detect-capabilities {"TMUX" "/tmp/x" "TERM_PROGRAM" "ghostty"}))))
    (expect (nil? (:images (timg/detect-capabilities {"TERM" "screen-256color"}))))
    (expect (nil? (:images (timg/detect-capabilities {"TERM_PROGRAM" "apple_terminal"}))))))

(defdescribe dimension-sniffing-test
  (it "reads png IHDR width/height from magic bytes"
    (let [png (byte-array (concat [0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a]
                            [0 0 0 13] (map int "IHDR")
                            [0 0 0x04 0x00]   ;; width 1024
                            [0 0 0x03 0x00]   ;; height 768
                            [8 6 0 0 0]))]
      (expect (= {:w 1024 :h 768} (timg/image-dimensions png "image/png")))))

  (it "reads gif logical-screen width/height (little-endian)"
    (let [gif (byte-array (concat (map int "GIF89a")
                            [0x20 0x03 0x58 0x02] ;; 800 x 600 LE
                            [0 0 0 0]))]
      (expect (= {:w 800 :h 600} (timg/image-dimensions gif "image/gif"))))))

(defdescribe cell-box-sizing-test
  (it "aspect-preserving fit into a cell box, never exceeding the caps"
    (let [{:keys [cols rows]} (timg/cell-size {:w 1200 :h 800} 60 40)]
      (expect (<= 1 cols 60))
      (expect (<= 1 rows 40))
      ;; landscape image: width hits the cap before height
      (expect (= 60 cols)))))

(defdescribe png-transcode-test
  ;; Kitty's f=100 is PNG-only, so a JPEG drop must be re-encoded to PNG.
  (it "transcodes a JPEG file into a downscaled PNG base64 payload"
    (let [img  (java.awt.image.BufferedImage. 400 300
                 java.awt.image.BufferedImage/TYPE_INT_RGB)
          tmp  (java.io.File/createTempFile "vis-timg" ".jpg")]
      (try
        (javax.imageio.ImageIO/write img "jpg" tmp)
        (let [b64   (timg/transcode->png-base64 (.getAbsolutePath tmp) {:cols 20 :rows 10})
              bytes (.decode (java.util.Base64/getDecoder) ^String b64)]
          (expect (string? b64))
          ;; PNG magic 0x89 'P' 'N' 'G'
          (expect (= [-119 80 78 71]
                    (map #(aget ^bytes bytes %) (range 4)))))
        (finally (.delete tmp))))))

(defdescribe kitty-encoding-test
  (it "small payload is a single a=T transmit+display escape with C=1"
    (let [s (timg/encode-kitty "AAAA" {:cols 10 :rows 5})]
      (expect (str/starts-with? s "\u001b_G"))
      (expect (str/includes? s "a=T"))
      (expect (str/includes? s "C=1"))
      (expect (str/includes? s "c=10,r=5"))
      (expect (str/ends-with? s "\u001b\\"))))

  (it "a payload over one chunk is split into m=1 … m=0 chunks"
    (let [big (apply str (repeat 5000 \A))
          s   (timg/encode-kitty big {:cols 1 :rows 1})]
      (expect (str/includes? s "m=1;"))
      (expect (str/includes? s "m=0;")))))
