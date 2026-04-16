(ns com.blockether.vis.adapters.web.presentation.input-bar
  "Chat input form with submit button.")

(defn render [current-id]
  [:div.input-bar
   [:form#form {:method "POST" :action (str "/conversations/" current-id)}
    [:textarea#input {:name "q" :placeholder "Message..." :autofocus true :autocomplete "off" :rows 1}]
    [:button#btn {:type "submit" :disabled true}
     [:i {:data-lucide "arrow-up"}]]]])
