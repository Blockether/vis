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
    [:div.sheet-list
     (for [{:keys [id name]} conversations]
       [:div.sheet-item {:class (when (= id current-id) "active")}
        [:a.sheet-item-name {:href (str "/conversations/" id)} name]
        [:a.sheet-item-del {:href (str "/conversations/" id "/delete") :onclick "return confirm('Delete?')"}
         [:i {:data-lucide "trash-2"}]]])]]])
