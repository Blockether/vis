(ns com.blockether.vis.ext.channel-tui.terminal-image-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  capability-detection-test
  (it "kitty / ghostty / wezterm / warp all speak the Kitty protocol"
      (expect (= :kitty (:images (timg/detect-capabilities {"TERM_PROGRAM" "kitty"}))))
      (expect (= :kitty (:images (timg/detect-capabilities {"TERM_PROGRAM" "ghostty"}))))
      (expect (= :kitty (:images (timg/detect-capabilities {"WEZTERM_PANE" "1"}))))
      (expect (= :kitty (:images (timg/detect-capabilities {"WARP_SESSION_ID" "x"})))))
  (it "iTerm2 is detected from its session env"
      (expect (= :iterm2 (:images (timg/detect-capabilities {"ITERM_SESSION_ID" "w0"})))))
  (it "tmux / screen / unknown terminals report no image support"
      (expect (nil? (:images (timg/detect-capabilities {"TMUX" "/tmp/x"
                                                        "TERM_PROGRAM" "ghostty"}))))
      (expect (nil? (:images (timg/detect-capabilities {"TERM" "screen-256color"}))))
      (expect (nil? (:images (timg/detect-capabilities {"TERM_PROGRAM" "apple_terminal"}))))))

(defdescribe dimension-sniffing-test
             (it "reads png IHDR width/height from magic bytes"
                 (let
                   [png (byte-array (concat [0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a]
                                            [0 0 0 13]
                                            (map int "IHDR")
                                            [0 0 0x04 0x00] ;; width 1024
                                            [0 0 0x03 0x00] ;; height 768
                                            [8 6 0 0 0]))]
                   (expect (= {:w 1024 :h 768} (timg/image-dimensions png "image/png")))))
             (it "reads gif logical-screen width/height (little-endian)"
                 (let
                   [gif (byte-array (concat (map int "GIF89a")
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
             (it
               "transcodes a JPEG file into a downscaled PNG base64 payload"
               (let
                 [img
                  (java.awt.image.BufferedImage. 400 300 java.awt.image.BufferedImage/TYPE_INT_RGB)

                  tmp
                  (java.io.File/createTempFile "vis-timg" ".jpg")]

                 (try (javax.imageio.ImageIO/write img "jpg" tmp)
                      (let
                        [b64
                         (timg/transcode->png-base64 (.getAbsolutePath tmp) {:cols 20 :rows 10})

                         bytes
                         (.decode (java.util.Base64/getDecoder) ^String b64)]

                        (expect (string? b64))
                        ;; PNG magic 0x89 'P' 'N' 'G'
                        (expect (= [-119 80 78 71] (map #(aget ^bytes bytes %) (range 4)))))
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
                 (let
                   [big
                    (apply str (repeat 5000 \A))

                    s
                    (timg/encode-kitty big {:cols 1 :rows 1})]

                   (expect (str/includes? s "m=1;"))
                   (expect (str/includes? s "m=0;")))))

(defdescribe
  cell-size-report-test
  (it "parses a CSI 16t cell-size reply ESC[6;<h>;<w>t"
      (expect (= {:w 9 :h 18} (timg/parse-cell-size-report "\u001b[6;18;9t"))))
  (it "derives cell size from CSI 14t (px) + CSI 18t (cells) when 16t is absent"
      ;; 1600x900 px text area over 100 cols x 30 rows => 16x30 px cells.
      (expect (= {:w 16 :h 30} (timg/parse-cell-size-report "\u001b[4;900;1600t\u001b[8;30;100t"))))
  (it "tolerates the replies interleaved with other bytes / arbitrary order"
      (expect (= {:w 8 :h 17}
                 (timg/parse-cell-size-report "junk\u001b[8;40;120t\u001b[4;680;960tmore"))))
  (it "returns nil for a silent / unparseable terminal"
      (expect (nil? (timg/parse-cell-size-report "")))
      (expect (nil? (timg/parse-cell-size-report nil)))
      (expect (nil? (timg/parse-cell-size-report "\u001b[6;0;0t")))
      (expect (nil? (timg/parse-cell-size-report "random noise")))))

(defdescribe cell-dimensions-drive-box-sizing-test
             ;; The whole point of the startup cell-size probe: feeding the REAL cell px
             ;; size changes the box aspect so the reserved rows match how the terminal
             ;; actually lays out the image (issue #32 follow-up).
             (it "a square-cell terminal reserves a square box for a square image"
                 (try (timg/set-cell-dimensions! 20 20)
                      (let [{:keys [cols rows]} (timg/cell-size {:w 500 :h 500} 40 40)]
                        (expect (= cols rows)))
                      ;; The default 9x18 (tall) cell makes the SAME square image span more
                      ;; cols than rows — proving the box tracks the cell dims, not a constant.
                      (timg/set-cell-dimensions! 9 18)
                      (let [{:keys [cols rows]} (timg/cell-size {:w 500 :h 500} 40 40)]
                        (expect (> cols rows)))
                      (finally (timg/set-cell-dimensions! 9 18)))))
