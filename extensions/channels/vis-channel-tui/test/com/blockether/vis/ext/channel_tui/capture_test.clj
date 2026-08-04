(ns com.blockether.vis.ext.channel-tui.capture-test
  "Pins the capture contract the PNG rasterizer depends on: one frame per flush,
   every cell carrying char + fg + bg + bold in the colours a TERMINAL would
   really show, queued keys reaching the dialog, and a real PNG on disk."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.ext.channel-tui.cinema :as cinema]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna SGR TextColor$RGB]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn- paint-two-frames!
  [{:keys [screen g]}]
  (doto ^TextGraphics g
    (.setForegroundColor (TextColor$RGB. 255 0 0))
    (.setBackgroundColor (TextColor$RGB. 0 0 16))
    (.enableModifiers (into-array SGR [SGR/BOLD]))
    (.putString 2 1 "hi"))
  (.refresh ^TerminalScreen screen)
  (.putString ^TextGraphics g 2 2 "bye")
  (.refresh ^TerminalScreen screen)
  :done)

(defn- tmp [nm] (str (System/getProperty "java.io.tmpdir") "/" nm))

(defdescribe
  capture-test
  (it "records one frame per flush, with the exact chars, colors and bold flag"
      (let
        [{:keys [cols rows frames ret]} (cap/capture! {:cols 20 :rows 5 :paint! paint-two-frames!})]
        (expect (= 20 cols))
        (expect (= 5 rows))
        (expect (= :done ret))
        (expect (= 2 (count frames)))
        (expect (= {:ch "h" :fg [255 0 0] :bg [0 0 16] :bold true}
                   (get-in (vec (first frames)) [1 2])))
        (expect (str/includes? (cap/frame-text (first frames)) "hi"))
        (expect (not (str/includes? (cap/frame-text (first frames)) "bye")))
        (expect (str/includes? (cap/frame-text (second frames)) "bye"))
        ;; every captured row is exactly `cols` wide, which the rasterizer assumes
        (expect (every? #(= 20 (count %)) (first frames)))))
  ;; Regression: Lanterna reports its DEFAULT colour as ANSI black, so every cell
  ;; the paint never touched was captured as #000000 and rasterized as a void --
  ;; a screenshot of the app on black paper, which the app never shows.
  (it "gives an untouched cell the theme's own paper, never black"
      (let
        [{:keys [frames]}
         (cap/capture! {:cols 8
                        :rows 2
                        :paint! (fn [{:keys [^TerminalScreen screen]}]
                                  (.refresh screen))})

         c
         (get-in (vec (first frames)) [0 0])]

        (expect (= (cinema/cell nil) c))
        (expect (not= [0 0 0] (:bg c)))))
  (it "feeds queued keys to the painted dialog before it runs"
      (let
        [{:keys [ret]} (cap/capture! {:cols 10
                                      :rows 3
                                      :keys [\c :esc]
                                      :paint! (fn
                                                [{:keys [^DefaultVirtualTerminal terminal
                                                         ^TerminalScreen screen]}]
                                                (.refresh screen)
                                                [(.getCharacter ^KeyStroke (.readInput terminal))
                                                 (.getKeyType ^KeyStroke (.readInput terminal))])})]
        (expect (= [(Character/valueOf \c) KeyType/Escape] ret))))
  (it "captures a throwing paint instead of losing the frame"
      (let
        [{:keys [frames ret]} (cap/capture! {:cols 8
                                             :rows 2
                                             :paint! (fn [{:keys [^TerminalScreen screen]}]
                                                       (.refresh screen)
                                                       (throw (ex-info "boom" {})))})]
        (expect (= 1 (count frames)))
        (expect (str/includes? (str ret) "boom"))))
  (it "rasterizes the chosen frames to real PNGs in the captured colours"
      (let
        [{:keys [pngs]}
         (cap/capture-png! (tmp "vis-capture-test")
                           {:cols 20 :rows 5 :indexes [1] :font-size 12 :paint! paint-two-frames!})

         path
         (first pngs)

         header
         (mapv int (take 4 (java.nio.file.Files/readAllBytes (.toPath (io/file path)))))

         rows
         (cap/png-rows path)]

        (expect (= 1 (count pngs)))
        (expect (str/ends-with? path "vis-capture-test-1.png"))
        ;; a real PNG signature, not a File that merely exists
        (expect (= [-119 80 78 71] header))
        ;; the corner is the theme's paper, not a black void
        (expect (= (:bg (cinema/cell nil)) (get-in rows [0 0])))
        ;; and the paint's own background really is somewhere on the image
        (expect (contains? (set (apply concat rows)) [0 0 16]))
        (.delete (io/file path))))
  (it "maps characters and key names onto Lanterna strokes"
      (expect (= KeyType/Escape (.getKeyType (cap/key-stroke :esc))))
      (expect (= KeyType/Enter (.getKeyType (cap/key-stroke :enter))))
      (expect (= KeyType/ArrowDown (.getKeyType (cap/key-stroke :down))))
      (expect (= (Character/valueOf \c) (.getCharacter (cap/key-stroke \c))))))
