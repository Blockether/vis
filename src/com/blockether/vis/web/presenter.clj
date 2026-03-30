(ns com.blockether.vis.web.presenter
  "Page layout — orchestrates components, no rendering logic here."
  (:require [com.blockether.vis.web.presenter.message :as message]
            [com.blockether.vis.web.presenter.topbar :as topbar]
            [com.blockether.vis.web.presenter.sheet :as sheet]
            [com.blockether.vis.web.presenter.input-bar :as input-bar]
            [com.blockether.vis.web.presenter.sidebar :as sidebar]
            [hiccup2.core :as h]))

(def ^:const page-size 8)

(defn page [current-id sessions-list messages & [{:keys [offset]}]]
  (str
   "<!DOCTYPE html>"
   (h/html
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"}]
      [:title "vis"]
      [:link {:rel "stylesheet" :href "/css/app.css"}]
      [:link {:rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"}]
      [:script {:src "https://cdn.jsdelivr.net/npm/marked/marked.min.js"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"}]
      [:script {:src "https://unpkg.com/lucide@latest"}]]
     [:body
      (topbar/render current-id sessions-list (seq messages))
      (sheet/render current-id sessions-list)
      (sidebar/render)
      [:div.main
       [:div#chat.chat {:data-total (str (count messages))
                        :data-showing (str (min (count messages) page-size))
                        :data-session current-id}
        (let [total     (count messages)
              show-from (max 0 (- total (or offset page-size)))
              visible   (subvec (vec messages) show-from)]
          [:div.chat-inner
           (if (empty? messages)
             [:div.empty ""]
             (list
              (when (pos? show-from)
                [:div#load-more.load-more {:data-from (str show-from)} "Loading..."])
              (map-indexed (fn [i msg] (message/render-msg (+ show-from i) msg)) visible)))])]
       (input-bar/render current-id)]
      [:script {:src "/js/app.js"}]]])))
