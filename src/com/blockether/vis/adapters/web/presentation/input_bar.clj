(ns com.blockether.vis.adapters.web.presentation.input-bar
  "Chat input form with submit button.")

(defn render [current-id]
  [:div.input-bar
   [:form#form {:method "POST" :action (str "/conversations/" current-id)}
    [:input#input {:type "text" :name "q" :placeholder "Message..." :autofocus true :autocomplete "off"}]
    [:button#btn {:type "submit" :disabled true}
     [:i {:data-lucide "arrow-up"}]]]])
