(ns com.blockether.vis.internal.progress
  "Streaming progress tracker — leaf module.

   Channels (TUI, CLI agent, Telegram) consume svar's per-iteration
   chunks `{:iteration N :thinking str :code [str] :done? bool}` and
   pipe them through this tracker for incremental rendering. The
   tracker deduplicates by `:iteration`, so a streaming chunk that
   updates the same iteration multiple times collapses into one
   timeline entry.

   Public API:

     `(make-progress-tracker)`              — fresh tracker, no callback
     `(make-progress-tracker {:on-update})` — invokes `(on-update timeline chunk)`
                                              on every chunk

   Returns `{:on-chunk fn :get-timeline fn}`. Pass the `:on-chunk` fn
   under `:hooks {:on-chunk ...}` of `conversations/send!`.")

(defn make-progress-tracker
  "Create a progress tracker for streaming iteration chunks.
   Returns `{:on-chunk fn :get-timeline fn}`.

   `on-update` is called `(on-update timeline chunk)` on every chunk.
   Timeline is a vec of chunk maps deduplicated by `:iteration`."
  ([] (make-progress-tracker nil))
  ([{:keys [on-update]}]
   (let [timeline (atom {})
         as-vec   #(mapv val (sort-by key %))]
     {:on-chunk     (fn [chunk]
                      (let [iteration (:iteration chunk)
                            tl        (swap! timeline assoc iteration chunk)]
                        (when on-update
                          (on-update (as-vec tl) chunk))))
      :get-timeline #(as-vec @timeline)})))
