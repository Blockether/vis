(ns com.blockether.vis.channels.web.presentation.input-bar
  "Chat input form with submit button and dictation mic button.")

(defn render [current-id]
  [:div.input-bar
   [:form#form {:method "POST" :action (str "/conversations/" current-id)}
    [:textarea#input {:name "q" :placeholder "Message..." :autofocus true :autocomplete "off" :rows 1}]
    ;; Hidden by default; JS feature-detects Web Speech API and reveals
    ;; the button on supported browsers (Chrome, Edge, Safari). On Firefox
    ;; and other unsupported engines it stays hidden — no broken affordance.
    [:button#mic-btn {:type "button"
                      :title "Dictate (tap to start, tap again to stop)"
                      :data-mic-state "idle"
                      :hidden true
                      :data-dictate-url (str "/conversations/" current-id "/dictate")}
     [:i {:data-lucide "mic"}]]
    [:button#btn {:type "submit" :disabled true}
     [:i {:data-lucide "arrow-up"}]]]])
