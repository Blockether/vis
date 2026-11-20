(ns com.blockether.vis.tui.frame-test
  (:require [com.blockether.vis.tui.frame :as frame]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize TextCharacter]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.ansi ANSITerminal]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]))

(defn- refresh-bytes
  "What one `frame/refresh!` sends to an in-memory ANSI terminal after cell `ch` changed
   at column 3, row 2, with the caret at column 5, row 4."
  [ch]
  (let [out
        (ByteArrayOutputStream.)

        terminal
        (proxy [ANSITerminal] [(ByteArrayInputStream. (byte-array 0)) out StandardCharsets/UTF_8]
          (findTerminalSize [] (TerminalSize. 20 6)))

        screen
        (TerminalScreen. terminal)]

    (.startScreen screen)
    (.setCursorPosition screen (TerminalPosition. 5 4))
    (frame/refresh! screen)
    (.reset out)
    (.setCharacter screen 3 2 ^TextCharacter (first (TextCharacter/fromCharacter ch)))
    (frame/refresh! screen)
    (.toString out "UTF-8")))

;; User report: the caret must not flicker over the screen while a scrolled frame is written.
(defdescribe refresh-hides-the-caret-while-cells-change
             (it "hides the caret before the changed cells and shows it again after them"
                 (let [s
                       (refresh-bytes \x)

                       hide
                       (.indexOf s "\u001b[?25l")

                       cell
                       (.indexOf s "x")

                       show
                       (.lastIndexOf s "\u001b[?25h")]

                   (expect (<= 0 hide))
                   (expect (< hide cell))
                   (expect (< cell show)))))
