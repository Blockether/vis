(ns vis-tui.review
  "Local production Activity fixture. Run with :html-review after building Lanterna.
   start! returns a Closeable map with :url; every repaint updates the static HTML file.
   One review per JVM: the production hit map is shared. No gateway connection."
  (:require [com.blockether.vis.tui.html-backend-test :as fixture]
            [com.blockether.vis.tui.interactions :as interactions])
  (:import [com.googlecode.lanterna.input MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal HtmlTerminalPreview]
           [java.io Closeable]
           [java.nio.file Path]))

(defrecord Review [url terminal screen preview worker running lock]
  Closeable
    (close [_]
      (when (compare-and-set! running true false)
        (.interrupt ^Thread worker)
        (.join ^Thread worker 2000)
        (locking lock
          (.close ^HtmlTerminalPreview preview)
          (.stopScreen ^TerminalScreen screen)
          (.close ^HtmlTerminal terminal)))))

(defn start!
  "Review the production Activity fixture and export each painted state.
   path is an explicit destination. cols defaults to a readable 40-column phone frame.
   Close the returned handle to release the loopback server, render loop and terminal."
  ([path] (start! path 40))
  ([path cols]
   (let [destination
         (Path/of (str path) (make-array String 0))

         terminal
         (fixture/activity-review-terminal cols 80)

         screen
         (TerminalScreen. terminal)

         running
         (atom true)

         lock
         (Object.)

         folds
         (atom {})

         rows
         (fixture/activity-result-rows)

         paint!
         (fn []
           (let [painted (fixture/paint-activity-review! screen rows @folds)]
             (.writeHtml terminal destination (int (:review-rows painted)))))]

     (try (.startScreen screen)
          (.setCursorPosition screen nil)
          (paint!)
          (let [preview
                (HtmlTerminalPreview/start terminal)

                worker
                (Thread/startVirtualThread
                  (fn []
                    (try (let [size (atom (.getTerminalSize terminal))]
                           (while @running
                             (locking lock
                               (let [input (.pollInput terminal)
                                     next-size (.getTerminalSize terminal)
                                     changed? (when (and (instance? MouseAction input)
                                                         (= MouseActionType/CLICK_DOWN
                                                            (.getActionType ^MouseAction input)))
                                                (when-let [region (.lookup interactions/hit-map
                                                                           ^MouseAction input)]
                                                  (when (= :toggle-details (:kind region))
                                                    (swap! folds fixture/toggle-review-region
                                                      region)
                                                    true)))]

                                 (when (or changed? (not= @size next-size)) (paint!))
                                 (reset! size next-size)))
                             (Thread/sleep 20)))
                         (catch InterruptedException _))))]

            (->Review (str (.getUri preview)) terminal screen preview worker running lock))
          (catch Exception error (.stopScreen screen) (.close terminal) (throw error))))))

(defn -main
  "Start one local review: clojure -M:html-review /absolute/path/activity-tui.html [columns]."
  [path & [columns]]
  (when-not path (throw (ex-info "An HTML output path is required" {})))
  (with-open [^Closeable review (start! path (if columns (parse-long columns) 40))]
    (println (:url review))
    (println "Static HTML updates after each change. Stop this process to close the preview.")
    (loop []

      (Thread/sleep 1000)
      (recur))))
