(ns com.blockether.vis.web.presenter.sidebar
  "Context viewer sidebar — shows agent memory: context, learnings, variables.")

(defn render []
  [:div#context-sidebar.sidebar
   [:div.sidebar-header
    [:span "Agent Memory"]
    [:button.sidebar-close {:onclick "closeContext()"}
     [:i {:data-lucide "x"}]]]
   [:div#sidebar-content.sidebar-content
    [:div.ctx-empty "Loading..."]]])
