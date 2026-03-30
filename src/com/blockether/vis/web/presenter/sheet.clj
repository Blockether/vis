(ns com.blockether.vis.web.presenter.sheet
  "Session list bottom sheet.")

(defn render [current-id sessions-list]
  [:div#sheet.sheet
   [:div.sheet-bg {:onclick "closeSheet()"}]
   [:div.sheet-content
    [:div.sheet-handle]
    [:div.sheet-header
     [:span "Sessions"]
     [:a.sheet-new-btn {:href "/new"}
      [:i {:data-lucide "plus"}] "New"]]
    [:div.sheet-list
     (for [{:keys [id name]} sessions-list]
       [:div.sheet-item {:class (when (= id current-id) "active")}
        [:a.sheet-item-name {:href (str "/s/" id)} name]
        [:a.sheet-item-del {:href (str "/s/" id "/delete") :onclick "return confirm('Delete?')"}
         [:i {:data-lucide "trash-2"}]]])]]])
