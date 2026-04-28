(ns com.blockether.vis-loop.progress
  "Streaming progress tracker for iteration chunks.

   Reusable across TUI, web, Telegram, CLI. The on-chunk callback
   receives streaming chunks from svar's `ask!`:

       {:iteration N :thinking str :code [str] :done? bool}

   Thinking (reasoning) streams live as the LLM thinks. Code/expressions
   arrive after the LLM finishes its response. `:done? true` marks the
   final chunk of an iteration.

   Lives in vis-loop because the chunk shape is part of the loop's
   contract with svar — channels consume it but don't define it.")

(defn make-progress-tracker
  "Create a progress tracker for streaming iteration chunks.
   Returns {:on-chunk fn, :get-timeline fn}.

   `on-update` is called (on-update timeline chunk) on every chunk.
   Timeline is a vec of chunk maps, deduplicated by iteration."
  ([] (make-progress-tracker nil))
  ([{:keys [on-update]}]
   (let [timeline (atom {})  ;; iteration-num → latest chunk
         as-vec   #(mapv val (sort-by key %))]
     {:on-chunk  (fn [chunk]
                   (let [iteration (:iteration chunk)
                         tl        (swap! timeline assoc iteration chunk)]
                     (when on-update
                       (on-update (as-vec tl) chunk))))
      :get-timeline #(as-vec @timeline)})))
