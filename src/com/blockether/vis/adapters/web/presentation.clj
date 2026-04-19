(ns com.blockether.vis.adapters.web.presentation
  "Page layout — orchestrates components, no rendering logic here."
  (:require [com.blockether.vis.adapters.web.presentation.message :as message]
            [com.blockether.vis.adapters.web.presentation.topbar :as topbar]
            [com.blockether.vis.adapters.web.presentation.sheet :as sheet]
            [com.blockether.vis.adapters.web.presentation.input-bar :as input-bar]
            [com.blockether.vis.adapters.web.presentation.sidebar :as sidebar]
            [hiccup2.core :as h]))

(def ^:const page-size 8)

(defn not-found-page []
  (str
    "<!DOCTYPE html>"
    (h/html
      [:html {:lang "en"}
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"}]
        [:title "vis — not found"]
        [:link {:rel "stylesheet" :href "/css/app.css"}]
        [:script {:src "https://unpkg.com/lucide@latest"}]]
       [:body
        [:div.main
         [:div#chat.chat
          [:div.chat-inner
           [:div.not-found
            [:div.not-found-icon [:i {:data-lucide "message-circle-off"}]]
            [:h2.not-found-title "This conversation doesn't exist"]
            [:p.not-found-body "It may have been deleted, or the link might be outdated."]
            [:a.not-found-link {:href "/"} "Open latest chat"]]]]]]])))

(defn page [current-id conversations messages & [{:keys [offset]}]]
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
        (topbar/render current-id conversations (seq messages))
        (sheet/render current-id conversations)
        (sidebar/render)
        [:div.main
         [:div#chat.chat {:data-total (str (count messages))
                          :data-showing (str (min (count messages) page-size))
                          :data-conversation current-id}
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
