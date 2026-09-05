(ns com.blockether.vis.tui.html-backend-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.header :as header]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.terminal-image :as timg]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.gui2 Button GridLayout Panel TextGraphicsComponent]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlMedia$Kind HtmlTerminal HtmlTerminalView]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn- cell-grid
  [^DefaultVirtualTerminal terminal cols rows]
  (mapv (fn [row]
          (mapv (fn [col]
                  (.getCharacter terminal (TerminalPosition. (int col) (int row))))
                (range cols)))
        (range rows)))

(deftest screen-accepts-a-transport-neutral-html-terminal-test
  (with-open [terminal (-> (HtmlTerminal/builder)
                           (.build))]
    (is (identical? terminal (#'screen/create-terminal! {:html-terminal terminal})))
    (is (.contains (.renderLiveHtml terminal "/tui") "data-endpoint-prefix=\"/tui\""))
    (let [method-names (set (map #(.getName ^java.lang.reflect.Method %)
                                 (.getMethods HtmlTerminal)))]
      (is (not-any? method-names ["getUrl" "getUri" "getPort" "hasEmbeddedServer"])))))

(deftest html-media-uses-the-resolved-image-cell-box-test
  (let [terminal
        (-> (HtmlTerminal/builder)
            (.initialSize (TerminalSize. 40 12))
            (.build))

        active
        (deref #'screen/active-html-terminal)

        region
        {:row 3
         :col 4
         :img {:id "turn-1-image-1" :path "/tmp/preview.png" :mime "image/png" :cols 8 :rows 5}}]

    (try (reset! active terminal)
         (timg/set-backend! :html)
         (with-redefs [timg/video-source?
                       (constantly false)

                       timg/html-png-data
                       (fn [_ _]
                         (byte-array [1 2 3]))]

           (#'screen/paint-terminal-images! [region])
           (let [media (first (.getMedia terminal))]
             (is (= 1 (count (.getMedia terminal))))
             (is (= "turn-1-image-1" (.getId media)))
             (is (= HtmlMedia$Kind/IMAGE (.getKind media)))
             (is (= (TerminalPosition. 4 3) (.getPosition media)))
             (is (= (TerminalSize. 8 5) (.getSize media))))
           (#'screen/drop-terminal-images!)
           (is (empty? (.getMedia terminal))))
         (finally (reset! active nil) (timg/set-backend! :native) (.close terminal)))))

(deftest html-backend-reserves-inline-image-layout-test
  (try (timg/set-backend! :html)
       (is (= :html (timg/images-protocol)))
       (is (true? (timg/graphical-terminal?)))
       (finally (timg/set-backend! :native))))

(deftest header-actions-use-lanterna-grid-and-render-alone-test
  (let [panel
        (header/header-actions-component)

        preferred
        (.getPreferredSize ^Panel panel)

        html
        (HtmlTerminalView/render panel preferred "Vis header actions")]

    (is (instance? GridLayout (.getLayoutManager ^Panel panel)))
    (is (every? #(instance? Button %) (.getChildrenList ^Panel panel)))
    (is (= 1 (.getRows preferred)))
    (is (.contains html "Vis header actions"))
    (is (.contains html "help ("))
    (is (.contains html "data-live=\"false\""))))

(deftest header-actions-run-as-an-interactive-html-component-test
  (let [triggered
        (promise)

        panel
        (header/header-actions-component #(deliver triggered %))

        view
        (HtmlTerminalView/start panel (.getPreferredSize ^Panel panel) "Interactive Vis header")]

    (try (.addInput (.getTerminal view) (KeyStroke. KeyType/Enter))
         (is (= :header-help (deref triggered 3000 ::timeout)))
         (is (.contains (.renderHtml view) "help ("))
         (finally (.close view)))))

(deftest all-terminal-surfaces-enter-through-the-grid-test
  (let [source-root
        (-> (io/resource "com/blockether/vis/tui/frame.clj")
            .toURI
            io/file
            .getParentFile)

        calls-by-file
        (into (sorted-map)
              (keep (fn [file]
                      (when (and (.isFile file) (.endsWith (.getName file) ".clj"))
                        (let [calls (count (re-seq #"\.newTextGraphics" (slurp file)))]
                          (when (pos? calls) [(.getName file) calls])))))
              (file-seq source-root))]

    ;; `render` and `screen` only create child clips from grid-owned graphics.
    ;; Every terminal-screen root and section clip is centralized in `frame`.
    (is (= {"frame.clj" 4 "render.clj" 1 "screen.clj" 1} calls-by-file))))

(deftest full-screen-surfaces-use-one-lanterna-grid-test
  (let [{:keys [panel sections]}
        (frame/layout 80 30 {:header 3 :attachments 2 :composer 3 :footer 2})

        bounds
        (update-vals sections frame/bounds)]

    (is (instance? GridLayout (.getLayoutManager ^Panel panel)))
    (is (= [:header :header-gap :transcript :echo :attachments :composer :footer]
           (vec (keys sections))))
    (is (every? #(instance? TextGraphicsComponent %) (vals sections)))
    (is (= {:col 0 :row 0 :cols 80 :rows 3} (:header bounds)))
    (is (= {:col 0 :row 4 :cols 80 :rows 18} (:transcript bounds)))
    (is (= {:col 0 :row 22 :cols 80 :rows 1} (:echo bounds)))
    (is (= {:col 0 :row 23 :cols 80 :rows 2} (:attachments bounds)))
    (is (= {:col 0 :row 25 :cols 80 :rows 3} (:composer bounds)))
    (is (= {:col 0 :row 28 :cols 80 :rows 2} (:footer bounds)))))

(deftest grid-sections-fill-every-supported-column-count-test
  (doseq [cols (range 20 401)]
    (let [sections (:sections
                     (frame/layout cols 40 {:header 3 :attachments 2 :composer 4 :footer 2}))
          bounds (mapv (comp frame/bounds sections) frame/section-order)]

      (is (every? #(= {:col 0 :cols cols} (select-keys % [:col :cols])) bounds)
          (str "every section fills " cols " columns"))
      (is (= (mapv :row bounds) (vec (butlast (reductions + 0 (map :rows bounds)))))
          (str "sections are contiguous at " cols " columns"))
      (is (= 40 (reduce + (map :rows bounds)))
          (str "sections fill every row at " cols " columns")))))

(deftest laid-out-section-painters-stay-inside-their-grid-cell-test
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. 12 5))

        screen
        (doto (TerminalScreen. terminal) (.startScreen))

        root
        (frame/layout 12
                      5
                      {:header 1 :attachments 0 :composer 0 :footer 0}
                      {:transcript (fn [graphics _]
                                     (.putString graphics 0 1 "above")
                                     (.putString graphics 0 2 "inside")
                                     (.putString graphics 0 4 "below"))})]

    (try (frame/paint! (.newTextGraphics screen) root :transcript)
         (.refresh screen)
         (is (= \space (.getCharacter (.getCharacter terminal (TerminalPosition. 0 1)))))
         (is (= \i (.getCharacter (.getCharacter terminal (TerminalPosition. 0 2)))))
         (is (= \space (.getCharacter (.getCharacter terminal (TerminalPosition. 0 4)))))
         (finally (.stopScreen screen)))))

(deftest complete-frame-cells-match-html-and-terminal-at-every-width-test
  (let [rows
        32

        initial-size
        (TerminalSize. 20 rows)

        terminal
        (DefaultVirtualTerminal. initial-size)

        html
        (-> (HtmlTerminal/builder)
            (.initialSize initial-size)
            (.columnRange 20 400)
            (.rowRange rows rows)
            (.browserResize false)
            (.build))

        terminal-screen
        (doto (TerminalScreen. terminal) (.startScreen))

        html-screen
        (doto (TerminalScreen. html) (.startScreen))

        db
        {:config nil
         :session nil
         :title "Grid parity"
         :messages [{:id "user" :role :user :text "Zażółć gęślą — grid 界"}
                    {:id "assistant" :role :assistant :text "The same cells reach both backends."}]
         :scroll scroll/follow
         :input (input/paste-text (input/empty-input) "interactive draft")
         :settings {}
         :pending-sends []
         :detail-expansions {}
         :live-views []
         :loading? false
         :cancelling? false
         :progress nil
         :channel-status {}
         :tabs []
         :tab-locals {}
         :slash-command-index 0
         :render-version 0}]

    (try (doseq [cols (range 20 401)]
           (let [size (TerminalSize. cols rows)]
             (.setTerminalSize terminal size)
             (.setTerminalSize html size)
             (.doResizeIfNecessary terminal-screen)
             (.doResizeIfNecessary html-screen)
             (with-redefs [timg/images-protocol (constantly nil)]
               (#'screen/render-frame! terminal-screen cols rows db 1000)
               (#'screen/render-frame! html-screen cols rows db 1000))
             (is (= (cell-grid terminal cols rows) (cell-grid html cols rows))
                 (str "HTML and terminal cells differ at " cols " columns"))))
         (finally (.stopScreen terminal-screen)
                  (.stopScreen html-screen)
                  (.close html)
                  (.close terminal)))))

(deftest any-grid-view-renders-as-standalone-html-test
  (let [view
        (frame/view 18
                    2
                    (fn [graphics _]
                      (.putString graphics 0 0 "Standalone view")))

        html
        (HtmlTerminalView/render view (TerminalSize. 18 2) "Standalone Vis view")]

    (is (instance? GridLayout (.getLayoutManager ^Panel view)))
    (is (= 1 (count (.getChildrenList ^Panel view))))
    (is (.contains html ">Standalone</span>"))
    (is (.contains html ">view</span>"))
    (is (.contains html "Standalone Vis view"))))

(deftest grid-surface-owns-size-and-clipping-test
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. 12 5))

        screen
        (doto (TerminalScreen. terminal) (.startScreen))]

    (try (let [base (.newTextGraphics screen)]
           (.putString base 0 0 "K")
           (let [graphics (frame/surface-graphics screen 8 3)]
             (is (= (TerminalSize. 8 3) (.getSize graphics)))
             (.putString graphics 7 2 "XY")
             (.refresh screen)
             (is (= \K (.getCharacter (.getCharacter terminal (TerminalPosition. 0 0)))))
             (is (= \X (.getCharacter (.getCharacter terminal (TerminalPosition. 7 2)))))
             (is (= \space (.getCharacter (.getCharacter terminal (TerminalPosition. 8 2)))))))
         (finally (.stopScreen screen)))))
