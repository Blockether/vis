(ns com.blockether.vis.tui.message-links-test
  "Bare URLs in a message that the user sends show as links and open on click (#446)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.tui.external-opener :as opener]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna SGR TerminalSize TextCharacter]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private cases
  "The cases that the Companion also reads."
  (get (json/read-str (slurp (io/resource "vis-contract/fixtures/message-links.json"))) "cases"))

(defn- link-urls
  "The URLs of the link spans of `line-meta`, in order. A wrapped URL counts once."
  [line-meta]
  (vec (distinct (map :url (mapcat :links line-meta)))))

(defn- painted-links
  "Paint `message` into a virtual terminal. Returns each clickable URL with the glyphs
   under its click regions and whether all of them are underlined."
  [message]
  (let [screen
        (doto (TerminalScreen. (DefaultVirtualTerminal. (TerminalSize. 80 40))) (.startScreen))

        g
        (.newTextGraphics screen)]

    (try (.reset interactions/hit-map)
         (.beginFrame interactions/hit-map)
         (render/draw-chat-bubble! g message 0 2 60 {:viewport-top 0 :viewport-h 40})
         (.commitFrame interactions/hit-map)
         (->> (for [row
                    (range 40)

                    col
                    (range 80)

                    :let [hit
                          (.lookup interactions/hit-map col row)]
                    :when (= :url (:kind hit))
                    :let [^TextCharacter cell
                          (.getBackCharacter screen (int col) (int row))]]

                {:url (:url hit)
                 :ch (.getCharacterString cell)
                 :underlined? (contains? (.getModifiers cell) SGR/UNDERLINE)})
              (group-by :url)
              (into {}
                    (map (fn [[url cells]]
                           [url
                            {:text (apply str (map :ch cells))
                             :underlined? (every? :underlined? cells)}]))))
         (finally (.stopScreen screen)))))

(defn- press-painted-link
  "Paint `message`, press its only link as the mouse does, and return what the openers got."
  [message]
  (let [term
        (doto (TerminalScreen. (DefaultVirtualTerminal. (TerminalSize. 80 40))) (.startScreen))

        opened
        (promise)]

    (try (.reset interactions/hit-map)
         (.beginFrame interactions/hit-map)
         (render/draw-chat-bubble! (.newTextGraphics term)
                                   message
                                   0 2
                                   60 {:viewport-top 0 :viewport-h 40})
         (.commitFrame interactions/hit-map)
         (let [hit (first (for [row (range 40)
                                col (range 80)
                                :let [hit (.lookup interactions/hit-map col row)]
                                :when (= :url (:kind hit))]

                            hit))]
           (with-redefs-fn {#'opener/open-local! (fn [path]
                                                   (deliver opened [:local path])
                                                   {:status :ok})
                            #'opener/open! (fn [url]
                                             (deliver opened [:url url])
                                             {:status :ok})}
             (fn []
               ((deref #'screen/open-click-target!) hit)
               (deref opened 2000 ::timeout))))
         (finally (.stopScreen term)))))

(defdescribe
  user-message-links-test
  (it "links the bare URLs of the shared cases, also in a windowed long message"
      (doseq [{:strs [name text links]} cases]
        (let [message {:role :user :text text}]
          (expect (= links (link-urls (:line-meta (virtual/project-message message 80 {})))) name)
          (expect (= links
                     (link-urls
                       (:line-meta
                         (virtual/project-message message 80 {} {:window-start 0 :window-num 20}))))
                  name))))
  (it "opens exactly the underlined URL, without the full stop after it"
      (let [{:keys [prewrapped-lines line-meta]}
            (virtual/project-message
              {:role :user :text "Open https://jira.example.com/browse/CARS-12085. Thanks"}
              60
              {})]
        (expect
          (= {"https://jira.example.com/browse/CARS-12085"
              {:text "https://jira.example.com/browse/CARS-12085" :underlined? true}}
             (painted-links
               {:role :user :text "" :prewrapped-lines prewrapped-lines :line-meta line-meta})))))
  (it "opens a wrapped URL of a message that a running turn received from each of its rows"
      (let [url
            "https://example.com/a/very/long/path/that/wraps/over/rows"

            entries
            (#'render/delivered-message-entries (str "go to " url ", then stop") 0 24 {})]

        (expect (< 1 (count (filter :meta entries))))
        (expect (= {url {:text url :underlined? true}}
                   (painted-links {:role :assistant
                                   :text ""
                                   :prewrapped-lines (mapv :line entries)
                                   :line-meta (mapv :meta entries)})))))
  (it "opens a file URL that the user wrote as a local file, also outside the workspace"
      (let [{:keys [prewrapped-lines line-meta]}
            (virtual/project-message {:role :user :text "Play file:///tmp/my%20show.m4b now"} 60 {})

            delivered
            (#'render/delivered-message-entries "Play file:///tmp/b.mp3 now" 0 40 {})]

        (expect
          (= [:local "/tmp/my show.m4b"]
             (press-painted-link
               {:role :user :text "" :prewrapped-lines prewrapped-lines :line-meta line-meta})))
        (expect (= [:local "/tmp/b.mp3"]
                   (press-painted-link {:role :assistant
                                        :text ""
                                        :prewrapped-lines (mapv :line delivered)
                                        :line-meta (mapv :meta delivered)}))))))
