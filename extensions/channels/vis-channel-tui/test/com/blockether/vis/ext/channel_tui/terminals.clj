(ns com.blockether.vis.ext.channel-tui.terminals
  "Virtual-terminal SCAFFOLDING every paint test shares: a started Lanterna
   `DefaultVirtualTerminal` + `TerminalScreen`, one keystroke constructor, and
   two readers of the back-buffer — `grid` (every row, blanks kept) and
   `painted-rows` (non-blank rows plus their BOLD cells).

   Test support, never runtime code — it lives beside `capture.clj` so a
   component test and a dialog test look at the SAME pixels the same way."
  (:require [clojure.string :as str])
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize TextCharacter]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn virtual-screen
  []
  ;; Clear any interrupt flag leaked onto this (lazytest-reused) thread by a
  ;; prior cancellation test. Lanterna's DefaultVirtualTerminal.readInput
  ;; throws "Unexpected interrupt" when Thread.interrupted() is set, which
  ;; made the wheel-coalescing reads flaky depending on test order.
  (Thread/interrupted)
  (let
    [terminal
     (DefaultVirtualTerminal. (TerminalSize. 80 30))

     screen
     (TerminalScreen. terminal)]

    (.startScreen screen)
    {:terminal terminal :screen screen}))

(defn keystroke
  "One input event for a virtual terminal: `:esc` for Escape, any other
   character as itself."
  [c]
  (if (= :esc c)
    (KeyStroke. KeyType/Escape)
    (KeyStroke. (Character/valueOf (char c)) false false false)))

(defn painted-rows
  "Non-blank rows of the virtual terminal as `{:text :bold}`. `:bold` keeps only the
   cells carrying SGR/BOLD, so an ARMED flag is distinguishable from a dim one."
  [^DefaultVirtualTerminal terminal]
  (->> (range 30)
       (map (fn [y]
              (reduce (fn [acc x]
                        (let
                          [^TextCharacter ch
                           (.getCharacter terminal (TerminalPosition. (int x) (int y)))

                           s
                           (if ch (.getCharacterString ch) " ")]

                          (-> acc
                              (update :text str s)
                              (update
                                :bold
                                str
                                (if (and ch (contains? (set (.getModifiers ch)) SGR/BOLD)) s "")))))
                      {:text "" :bold ""}
                      (range 80))))
       (map #(update % :text str/trimr))
       (remove #(str/blank? (:text %)))
       vec))

(defn grid
  "EVERY terminal row as a string, blanks KEPT."
  [^DefaultVirtualTerminal terminal]
  (vec (for [y (range 30)]
         (apply str
           (for [x (range 80)]
             (let [^TextCharacter ch (.getCharacter terminal (TerminalPosition. (int x) (int y)))]
               (if ch (.getCharacterString ch) " ")))))))
