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
  (let [total      (count messages)
        ;; `offset` is the caller's "how many tail messages do I want?" —
        ;; page renders the last `offset` messages (default = page-size).
        ;; Both `show-from` (first visible index) and the `data-showing`
        ;; attribute must derive from the SAME count so the client-side
        ;; infinite-scroll can compute `next-offset = showing + page-size`
        ;; and actually advance. Before, `data-showing` was pinned at
        ;; `page-size`, so each scroll-up fetched offset=16 over and over.
        take-last (max 0 (min total (or offset page-size)))
        show-from (- total take-last)
        visible   (subvec (vec messages) show-from)]
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
           [:div#chat.chat {:data-total (str total)
                            :data-showing (str (count visible))
                            :data-page-size (str page-size)
                            :data-conversation current-id}
            [:div.chat-inner
             (if (empty? messages)
               [:div.empty ""]
               (list
                 (when (pos? show-from)
                   [:div#load-more.load-more {:data-from (str show-from)} "Loading..."])
                 (map-indexed (fn [i msg] (message/render-msg (+ show-from i) msg)) visible)))]]
           (input-bar/render current-id)]
          [:script {:src "/js/app.js"}]]]))))
