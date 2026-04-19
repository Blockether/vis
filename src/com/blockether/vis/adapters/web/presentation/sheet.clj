(ns com.blockether.vis.adapters.web.presentation.sheet
  "Conversation list bottom sheet.")

(defn render [current-id conversations]
  [:div#sheet.sheet
   [:div.sheet-bg {:onclick "closeSheet()"}]
   [:div.sheet-content
    [:div.sheet-handle]
    [:div.sheet-header
     [:span "Conversations"]
     [:a.sheet-new-btn {:href "/conversations/new"}
      [:i {:data-lucide "plus"}] "New"]]
     ;; `data-id` on each row lets the client locate and patch a specific
     ;; sheet entry when a title changes (e.g. auto-title LLM call) without
     ;; a full-page reload. Pair with `syncConversationList()` in app.js.
     [:div.sheet-list
      (for [{:keys [id name]} conversations]
        [:div.sheet-item {:class (when (= id current-id) "active")
                          :data-id (str id)}
         [:a.sheet-item-name {:href (str "/conversations/" id)} name]
         [:a.sheet-item-del {:href (str "/conversations/" id "/delete") :onclick "return confirm('Delete?')"}
          [:i {:data-lucide "trash-2"}]]])]]])
