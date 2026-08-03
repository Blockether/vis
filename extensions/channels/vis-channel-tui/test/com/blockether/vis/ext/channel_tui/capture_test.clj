(ns com.blockether.vis.ext.channel-tui.capture-test
  "Pins the capture contract the PNG rasterizer depends on: one frame per flush,
   every cell carrying char + fg + bg + bold, and queued keys reaching the dialog."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
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

(defdescribe
  capture-test
  (it "records one frame per flush, with the exact chars, colors and bold flag"
      (let
        [{:keys [cols rows frames ret]} (cap/capture! {:cols 20 :rows 5 :paint! paint-two-frames!})]
        (expect (= 20 cols))
        (expect (= 5 rows))
        (expect (= :done ret))
        (expect (= 2 (count frames)))
        (expect (= ["h" "#ff0000" "#000010" true] (get-in (vec (first frames)) [1 2])))
        (expect (str/includes? (cap/frame-text (first frames)) "hi"))
        (expect (not (str/includes? (cap/frame-text (first frames)) "bye")))
        (expect (str/includes? (cap/frame-text (second frames)) "bye"))
        ;; every captured row is exactly `cols` wide, which the rasterizer assumes
        (expect (every? #(= 20 (count %)) (first frames)))))
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
  (it "writes rasterizer JSON with cols, rows and one cell array per character"
      (let
        [path
         (str (System/getProperty "java.io.tmpdir") "/vis-capture-test.json")

         capture
         (cap/capture-json! path {:cols 20 :rows 5 :paint! paint-two-frames!})

         json
         (slurp path)]

        (expect (= 2 (count (:frames capture))))
        (expect (str/includes? json "{\"cols\":20,\"rows\":5,\"frames\":[["))
        (expect (str/includes? json "[\"h\",\"#ff0000\",\"#000010\",true]"))
        (.delete (java.io.File. path))))
  (it "maps characters and key names onto Lanterna strokes"
      (expect (= KeyType/Escape (.getKeyType (cap/key-stroke :esc))))
      (expect (= KeyType/Enter (.getKeyType (cap/key-stroke :enter))))
      (expect (= KeyType/ArrowDown (.getKeyType (cap/key-stroke :down))))
      (expect (= (Character/valueOf \c) (.getCharacter (cap/key-stroke \c))))))
