(ns com.blockether.vis.ext.channel-tui.capture-test
  "Pins the capture contract the PNG rasterizer depends on: one frame per flush,
   every cell carrying char + fg + bg + bold + italic + underline in the colours a
   TERMINAL would really show, queued keys reaching the dialog, and `shot!`
   handing back the path of a real PNG."
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

(defdescribe
  capture-test
  (it "records one frame per flush, with the exact chars, colors and bold flag"
      (let [{:keys [cols rows frames ret]} (cap/capture!
                                             {:cols 20 :rows 5 :paint! paint-two-frames!})]
        (expect (= 20 cols))
        (expect (= 5 rows))
        (expect (= :done ret))
        (expect (= 2 (count frames)))
        (expect (= {:ch "h" :fg [255 0 0] :bg [0 0 16] :bold true :italic false :underline false}
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
      (let [{:keys [frames]}
            (cap/capture! {:cols 8
                           :rows 2
                           :paint! (fn [{:keys [^TerminalScreen screen]}]
                                     (.refresh screen))})

            c
            (get-in (vec (first frames)) [0 0])]

        (expect (= (cinema/cell nil) c))
        (expect (not= [0 0 0] (:bg c)))))
  (it "feeds queued keys to the painted dialog before it runs"
      (let [{:keys [ret]} (cap/capture! {:cols 10
                                         :rows 3
                                         :keys [\c :esc]
                                         :paint! (fn [{:keys [^DefaultVirtualTerminal terminal
                                                              ^TerminalScreen screen]}]
                                                   (.refresh screen)
                                                   [(.getCharacter ^KeyStroke (.readInput terminal))
                                                    (.getKeyType ^KeyStroke
                                                                 (.readInput terminal))])})]
        (expect (= [(Character/valueOf \c) KeyType/Escape] ret))))
  (it "keeps the frames of a throwing paint, and reports the throw"
      (let [{:keys [frames error]} (cap/capture! {:cols 8
                                                  :rows 2
                                                  :paint! (fn [{:keys [^TerminalScreen screen]}]
                                                            (.refresh screen)
                                                            (throw (ex-info "boom" {})))})]
        (expect (= 1 (count frames)))
        (expect (str/includes? (str error) "boom"))))
  ;; Regression: `capture!` grabbed the VIRTUAL TERMINAL, but a `:paint!` that
  ;; never called `screen.refresh` leaves its drawing in the SCREEN's back buffer
  ;; -- so the promised fallback frame came back completely blank and `shot!`
  ;; wrote an empty sheet of paper. Four different OTP dialog states photographed
  ;; to byte-identical white PNGs before anybody noticed, because this test only
  ;; counted the frame instead of looking at it.
  (it "always yields a frame CARRYING the paint, even when it never flushed"
      (let [{:keys [frames]} (cap/capture! {:cols 6
                                            :rows 2
                                            :paint! (fn [{:keys [^TextGraphics g]}]
                                                      (.putString g 0 0 "hi")
                                                      :no-refresh)})]
        (expect (= 1 (count frames)))
        (expect (= 2 (count (first frames))))
        (expect (str/includes? (cap/frame-text (first frames)) "hi"))))
  (it "hands back the PATH of ONE real PNG of the paint"
      (let [png
            (cap/shot!
              {:cols 20 :rows 5 :font-size 12 :out "vis-capture-test" :paint! paint-two-frames!})

            header
            (mapv int (take 4 (java.nio.file.Files/readAllBytes (.toPath (io/file png)))))

            rows
            (cap/png-rows png)]

        (expect (str/ends-with? png "vis-capture-test.png"))
        ;; a bare name lands in the shot directory, never in the working tree
        (expect (str/includes? png "vis-tui"))
        ;; a real PNG signature, not a File that merely exists
        (expect (= [-119 80 78 71] header))
        ;; the paint's own background really is on the image
        (expect (contains? (set (apply concat rows)) [0 0 16]))
        ;; and it is the LAST flush that was drawn -- the state a user would see
        (expect (str/includes? (cap/frame-text (cap/capture!
                                                 {:cols 20 :rows 5 :paint! paint-two-frames!}))
                               "bye"))
        (.delete (io/file png))))
  (it "trims the blank margins, so the picture is the paint and not an ocean of paper"
      (let [shot
            (fn [trim nm]
              (cap/png-rows
                (cap/shot!
                  {:cols 40 :rows 12 :font-size 12 :trim trim :out nm :paint! paint-two-frames!})))

            full
            (shot false "vis-capture-full")

            trimmed
            (shot true "vis-capture-trim")]

        (expect (< (count trimmed) (count full)))
        (expect (< (count (first trimmed)) (count (first full))))
        ;; the type survived the crop
        (expect (pos? (cap/ink trimmed)))))
  (it "writes one numbered picture per flush"
      (let [pngs
            (cap/shots!
              {:cols 20 :rows 5 :font-size 12 :out "vis-capture-frames" :paint! paint-two-frames!})]
        (expect (= 2 (count pngs)))
        (expect (str/ends-with? (first pngs) "vis-capture-frames-0.png"))
        (expect (every? #(.exists (io/file %)) pngs))
        (run! #(.delete (io/file %)) pngs)))
  (it "writes the picture of a paint that threw, then fails loudly"
      (let [e (try (cap/shot! {:cols 8
                               :rows 2
                               :out "vis-capture-boom"
                               :paint! (fn [{:keys [^TerminalScreen screen]}]
                                         (.refresh screen)
                                         (throw (ex-info "boom" {})))})
                   nil
                   (catch clojure.lang.ExceptionInfo t t))]
        (expect (some? e))
        (expect (str/includes? (str (ex-cause e)) "boom"))
        (expect (.exists (io/file ^String (:png (ex-data e)))))))
  (it "maps characters and key names onto Lanterna strokes"
      (expect (= KeyType/Escape (.getKeyType (cap/key-stroke :esc))))
      (expect (= KeyType/Enter (.getKeyType (cap/key-stroke :enter))))
      (expect (= KeyType/ArrowDown (.getKeyType (cap/key-stroke :down))))
      (expect (= (Character/valueOf \c) (.getCharacter (cap/key-stroke \c))))
      ;; an unknown key SAYS so, instead of a NullPointerException out of Lanterna
      (expect (some? (try (cap/key-stroke :nope) nil (catch clojure.lang.ExceptionInfo t t))))))
