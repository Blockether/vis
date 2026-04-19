(ns com.blockether.vis.adapters.web.presentation.sidebar
  "Context viewer sidebar — shows the conversation's context: user-defined
   variables and (collapsed) agent-loop SYSTEM variables.")

(defn render []
  [:div#context-sidebar.sidebar
   [:div.sidebar-header
    [:span "Context"]
    [:button.sidebar-close {:onclick "closeContext()"}
     [:i {:data-lucide "x"}]]]
   [:div#sidebar-content.sidebar-content
    [:div.ctx-empty "Loading..."]]])
