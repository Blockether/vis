(ns com.blockether.vis.tui.canvas-map-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.canvas-map :as map*]
            [com.blockether.vis.tui.primitives :as p]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TextColor$ANSI]
           [com.googlecode.lanterna.graphics BasicTextImage]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]))

(defn- board
  [n]
  {:id "board"
   :type :canvas
   :is-selectable true
   :flow :right
   :zoom 1
   :cards (mapv (fn [i]
                  (cond-> {:id (str i) :title (str "Card " i)}
                    (pos? (long i))
                    (assoc :edges [{:from (str (dec (long i)))}])))
                (range n))})

(defn- screen-center
  [node camera viewport]
  (let [{:keys [x y] bw :w bh :h}
        (get-in (map*/plan-for node (:level camera) viewport) [:boxes (:focus camera)])

        [left top]
        (:position camera)]

    [(- (+ x (quot bw 2)) left) (- (+ y (quot bh 2)) top)]))

(defn- visible-center? [[x y] [w h]] (and (<= 0 x (dec (long w))) (<= 0 y (dec (long h)))))

(defdescribe
  map-camera
  (it "holds the focused card at the same screen cell while zooming"
      (let [node
            (assoc (board 10) :focus "5")

            v
            [40 9]

            start
            (map*/reconcile {} node v 0)

            near
            (map*/zoom start node v 1 nil 0)]

        (expect (= (screen-center node start v) (screen-center node near v)))))
  (it "fits the last card as well as the first when title tiles cannot fit"
      (let [node
            (board 200)

            [w h :as v]
            [38 6]

            start
            (map*/reconcile {} node v 0)

            fit
            (map*/fit start node v 1)

            plan
            (map*/plan-for node (:level fit) v)

            shown
            (map*/runs plan fit v 160 nil)]

        (expect (= (set (map :id (:cards node))) (set (map :id shown))))
        (expect (every? #(and (< -1 (:x %) w) (< -1 (:y %) h)) shown))))
  (it "stops a pan at either end and settles the glide on wall-clock time"
      (let [node
            (board 12)

            v
            [40 10]

            st
            (map*/reconcile {} node v 0)

            left
            (map*/pan st node v -9999 -9999 0)

            right
            (map*/pan left node v 9999 9999 100)]

        (expect (zero? (first (:position left))))
        (expect (< (first (map*/camera-at right 100))
                   (first (map*/camera-at right 175))
                   (first (map*/camera-at right 251))))
        (expect (= (:position right) (:position (map*/pan right node v 50 0 251))))))
  (it
    "follows an owner update until a manual move and follows again on reset"
    (let [node
          (board 10)

          v
          [36 8]

          st
          (map*/reconcile {} node v 0)

          owner
          (assoc node
            :focus "3"
            :zoom 2)

          follow
          (map*/reconcile st owner v 0)

          manual
          (map*/pan follow owner v 20 0 180)

          next
          (assoc owner
            :focus "6"
            :zoom 1)

          held
          (map*/reconcile manual next v 400)

          reset
          (map*/reset-camera held next v 400)]

      (expect (= :near (:level follow)))
      (expect (= "3" (:focus follow)))
      (expect (= (:position manual) (:position held)))
      (expect (= "6" (:focus reset)))
      (expect (= :normal (:level reset)))))
  (it "blocks manual camera changes on a locked board"
      (let [node
            (assoc (board 10)
              :focus "5"
              :is-locked true)

            v
            [40 10]

            st
            (map*/reconcile {} node v 0)]

        (expect (= st (map*/pan st node v -40 0 1)))
        (expect (= st (map*/zoom st node v 1 nil 1)))
        (expect (= st (map*/fit st node v 1)))
        (expect (= (:position st) (:position (map*/tab-card st node v 1 1))))))
  (it "keeps the tabbed card in the viewport after a resize changes the flow"
      (let [node
            (assoc (board 20) :flow :auto)

            wide
            [80 12]

            narrow
            [38 5]

            st
            (map*/reconcile {} node wide 0)

            tab
            (nth (iterate #(map*/tab-card % node wide 1 10) st) 14)

            resized
            (map*/reconcile tab node narrow 350)]

        (expect (= "14" (:focus resized)))
        (expect (visible-center? (screen-center node tab wide) wide))
        (expect (visible-center? (screen-center node resized narrow) narrow)))))

(defdescribe map-layout
             (it "places an item after the frame that holds its dependency"
                 (let [cards
                       [{:id "frame" :kind "frame" :title "Frame"}
                        {:id "build" :parent "frame" :title "Build"}
                        {:id "check" :title "Check" :edges [{:from "build"}]}]

                       {:keys [boxes]}
                       (map*/layout cards :normal :right)]

                   (expect (> (get-in boxes ["check" :x])
                              (+ (get-in boxes ["frame" :x]) (get-in boxes ["frame" :w]))))))
             (it "draws nested frames even when another group of frames has a parent cycle"
                 (let [cards
                       [{:id "a" :kind "frame" :parent "b" :title "A"}
                        {:id "b" :kind "frame" :parent "a" :title "B"}
                        {:id "c" :kind "frame" :title "C"} {:id "d" :parent "c" :title "D"}]

                       plan
                       (map*/layout cards :normal :down)

                       parent
                       (get-in plan [:boxes "c"])

                       child
                       (get-in plan [:boxes "d"])]

                   (expect (= #{"a" "b" "c" "d"} (set (keys (:boxes plan)))))
                   (expect (< (:x parent) (:x child) (+ (:x parent) (:w parent))))
                   (expect (< (:y parent) (:y child) (+ (:y parent) (:h parent))))))
             (it "does not split wide clusters when the camera cuts both sides of a title"
                 (let [title
                       "漢🧑‍💻 مرحبا"

                       node
                       {:id "board" :cards [{:id "card" :title title}] :zoom 0.5}

                       v
                       [3 2]

                       camera
                       (assoc (map*/reconcile {} node v 0) :position [3 0])

                       plan
                       (map*/plan-for node (:level camera) v)

                       shown
                       (map*/runs plan camera v 0 nil)]

                   (expect (seq shown))
                   (expect (every? #(<= (p/display-width (:text %)) 3) shown))
                   (expect (not-any? #(str/starts-with? (:text %) "‍") shown)))))

(defdescribe
  app-map
  (it
    "selects the tabbed card and keeps an app letter shortcut available in map mode"
    (let [node
          (board 10)

          view
          {:id "fixture" :nodes [node] :shortcuts [{:key "r" :label "Reply" :action "reply"}]}

          effects
          {:select! identity
           :shortcut! (fn [_ _]
                        nil)}

          calls
          (atom [])

          effects
          (assoc effects
            :select! #(swap! calls conj %)
            :shortcut! (fn [action _]
                         (swap! calls conj action)))

          key
          (fn [st k]
            (let [g (surface/measure st 80 20 view)]
              (surface/handle-key (surface/reconcile st g) k g effects)))

          map
          (key (surface/initial-state) (KeyStroke. KeyType/F4))

          next
          (key map (KeyStroke. KeyType/Tab))]

      (key next (KeyStroke. KeyType/Enter))
      (key next (KeyStroke. (Character/valueOf \r) false false))
      (expect (= ["1"] (:item-ids (first @calls))))
      (expect (= :shortcut (:action (second @calls))))
      (expect (nil? (:canvas-map (key next (KeyStroke. KeyType/F4))))))))

(defdescribe map-regressions
  (it "does not select a card outside a locked camera"
    (let [node (assoc (board 10) :focus "9" :is-locked true)
          view {:id "app" :nodes [node]}
          calls (atom [])
          key (fn [st k]
                (let [g (surface/measure st 40 12 view)]
                  (surface/handle-key (surface/reconcile st g) (KeyStroke. k) g
                                      {:select! #(swap! calls conj (:item-ids %))})))
          opened (key (surface/initial-state) KeyType/F4)
          tabbed (key opened KeyType/Tab)]
      (key tabbed KeyType/Enter)
      (expect (= [["9"]] @calls))
      (expect (= "9" (get-in (key tabbed KeyType/ReverseTab) [:canvas-map :focus])))))

  (it "keeps the title of a tall card in view after zoom and resize"
    (let [node {:id "b" :zoom 1 :focus "card"
                :cards [{:id "card" :title "Visible title"
                         :text (str/join "\n" (repeat 30 "Long card body"))}]}
          big [60 15]
          small [32 2]
          start (map*/reconcile {} node small 0)
          zoomed (map*/zoom start node small 1 nil 0)
          wide (map*/reconcile {} (assoc node :zoom 2) big 0)
          resized (map*/reconcile wide (assoc node :zoom 2) small 200)]
      (doseq [camera [zoomed resized]]
        (let [plan (map*/plan-for node (:level camera) small)]
          (expect (some #(str/includes? (:text %) "Visible title")
                        (map*/runs plan camera small 400 nil)))))))

  (it "toggles with m only when the app does not bind it"
    (let [view {:id "app" :nodes [(board 2)]}
          initial (surface/initial-state)
          calls (atom [])
          press (fn [st view]
                  (surface/handle-key st (KeyStroke. (Character/valueOf \m) false false)
                                      (surface/measure st 40 12 view)
                                      {:shortcut! (fn [action _] (swap! calls conj action))}))
          opened (press initial view)]
      (expect (some? (:canvas-map opened)))
      (expect (nil? (:canvas-map (press opened view))))
      (expect (nil? (:canvas-map (press initial (assoc view :shortcuts [{:key "m" :action "mark"}])))))
      (expect (= :shortcut (:action (first @calls)))))))

(defdescribe map-wheel
  (it "pans horizontally without changing the camera row"
    (let [node (assoc (board 12) :focus "5")
          view {:id "v" :nodes [node]}
          initial (surface/initial-state)
          geom (surface/measure initial 80 24 view)
          st (surface/handle-key initial (KeyStroke. KeyType/F4) geom {})
          [left top] (get-in st [:canvas-map :position])
          pos (TerminalPosition. 38 (inc (long (:content-top geom))))
          wheel (fn [action button shift?]
                  (get-in (surface/handle-key st (MouseAction. action button pos 1 false false shift?) geom {})
                          [:canvas-map :position]))]
      (expect (= [true true true]
                 (mapv (fn [[[x y] op]] (and (= top y) (op (long x) (long left))))
                       [[(wheel MouseActionType/SCROLL_LEFT 6 false) <]
                        [(wheel MouseActionType/SCROLL_RIGHT 7 false) >]
                        [(wheel MouseActionType/SCROLL_DOWN 5 true) >]])))))

  (it "keeps the card under Ctrl+wheel at the pointer"
    (let [node (assoc (board 12) :focus "5")
          view {:id "v" :nodes [node]}
          initial (surface/initial-state)
          geom (surface/measure initial 80 24 view)
          st (surface/handle-key initial (KeyStroke. KeyType/F4) geom {})
          camera (:canvas-map st)
          [w h :as v] (:viewport camera)
          [x y :as point] [(quot (long w) 2) (quot (long h) 2)]
          screen (TerminalPosition. (+ (long x) 1 (long (get-in geom [:bounds :left])))
                                    (+ (long y) (long (:content-top geom))))
          ctrl (MouseAction. MouseActionType/SCROLL_UP 4 screen 1 true false false)
          zoomed (:canvas-map (surface/handle-key st ctrl geom {}))
          under (fn [camera]
                  (map*/hit-card (map*/plan-for node (:level camera) v)
                                 (mapv + (:position camera) point)))]
      (expect (= :near (:level zoomed)))
      (expect (= "5" (under camera) (under zoomed))))))

(defdescribe map-selection
  (it "marks selection separately from focus even when the inks match"
    (let [[w h :as viewport] [80 20]
          node (assoc (board 12) :focus "0")
          camera (map*/reconcile {} node viewport 0)
          ink TextColor$ANSI/WHITE
          inks {:paper TextColor$ANSI/BLACK :card ink :edge ink :selected ink}
          marker (fn [camera selected id]
                   (let [plan (map*/plan-for node (:level camera) viewport)
                         image (BasicTextImage. (int w) (int h))
                         {:keys [x y h]} (get-in plan [:boxes id])
                         [left top] (map*/clamp-camera plan viewport (map*/camera-at camera 0))
                         offset (if (> (long h) 1) 1 0)]
                     (map*/paint! (.newTextGraphics image) plan camera [0 0] viewport 0 nil inks selected)
                     (.getCharacterString (.getCharacterAt image
                                                           (int (+ (- (long x) (long left)) offset))
                                                           (int (+ (- (long y) (long top)) offset))))))
          overview (map*/fit camera node viewport 0)]
      (expect (= :overview (:level (map*/plan-for node (:level overview) viewport))))
      (expect (not= (marker camera #{} "1") (marker camera #{"1"} "1")))
      (expect (= (marker camera #{} "0") (marker camera #{"1"} "0")))
      (expect (not= (marker overview #{} "1") (marker overview #{"1"} "1")))
      (expect (not= (marker overview #{} "0") (marker overview #{"0"} "0"))))))
