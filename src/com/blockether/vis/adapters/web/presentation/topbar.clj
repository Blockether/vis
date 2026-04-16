(ns com.blockether.vis.adapters.web.presentation.topbar
  "Top navigation bar — conversation title, copy mode, context viewer.")

(defn render [current-id conversations has-messages?]
  (let [current-name (or (some #(when (= (:id %) current-id) (:name %)) conversations) "New Chat")]
    [:div.topbar
     ;; Left spacer to balance the two right buttons
     [:div.topbar-spacer]
     [:div.topbar-title {:onclick "openSheet()"} current-name]
     [:button.topbar-btn {:id "select-btn"
                          :onclick "toggleSelectMode()"
                          :title "Select messages"
                          :disabled (when-not has-messages? true)
                          :class (when-not has-messages? "topbar-btn-disabled")}
      [:i {:data-lucide "copy"}]]
     [:button.topbar-btn {:onclick "showContext()" :title "View context"}
      [:i {:data-lucide "brain"}]]]))
