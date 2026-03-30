(ns com.blockether.vis.web.presenter.input-bar
  "Chat input form with submit button.")

(defn render [current-id]
  [:div.input-bar
   [:form#form {:method "POST" :action (str "/s/" current-id)}
    [:input#input {:type "text" :name "q" :placeholder "Message..." :autofocus true :autocomplete "off"}]
    [:button#btn {:type "submit" :disabled true}
     [:i {:data-lucide "arrow-up"}]]]])
