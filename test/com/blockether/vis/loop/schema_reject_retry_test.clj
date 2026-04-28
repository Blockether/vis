(ns com.blockether.vis.loop.schema-reject-retry-test
  "Bounded retry of `llm/ask!` on `:svar.spec/schema-rejected`.

   Some providers (notably GLM-5.1 under :deep reasoning) occasionally
   return a bare JSON-string in `content` instead of the structured
   iteration envelope svar's spec demands. svar throws
   `:svar.spec/schema-rejected`. Without a retry layer that exception
   advances the iteration counter, bills the consecutive-error budget,
   and writes a useless DB row -- none of which describes an actual
   reasoning failure of the model. `ask-with-schema-retry!` retries
   the SAME call up to `MAX_SCHEMA_REJECT_RETRIES` times with a
   one-shot reminder appended; only after retries are exhausted does
   the rejection bubble out to the iteration loop.

   These tests stub `llm/ask!` directly. We exercise three branches:
     1. First-call success                 -> no retry, no reminder.
     2. First fails, retry succeeds        -> ONE reminder, returns OK.
     3. All attempts fail (1 + N retries)  -> rejection bubbles out."
  (:require
   ;; The production call lives in `iteration.core` and aliases
   ;; svar's INTERNAL llm namespace -- redef it directly so the
   ;; stub actually intercepts. (Aliasing `svar.core` here would
   ;; redef a different var that the production path never resolves.)
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core
    :as iter]
   [lazytest.core :refer [defdescribe it expect]]))

(defn- schema-reject-ex
  "Build the same exception svar's `internal.spec/str->data-with-spec`
   throws when the provider returns a bare JSON-string."
  [preview]
  (ex-info
    (str "Your response did not match the JSON schema contract. "
      "PRODUCE valid JSON/EDN matching the schema fields. NO prose "
      "outside the structure.")
    {:type :svar.spec/schema-rejected
     :reason :not-a-map
     :received-type "String"
     :raw-data preview
     :raw-data-preview (pr-str preview)}))

(defn- ok-result
  "Minimal shape of what `llm/ask!` returns when parsing succeeds."
  []
  {:result      {:thinking "ok" :code []}
   :tokens      {:input 10 :output 5 :reasoning 0 :total 15}
   :duration-ms 1.0})

(defn- with-stubbed-ask!
  "Run `f` with `llm/ask!` replaced by a stub that pops one outcome per
   call from `outcomes` and either returns it (`:ok`) or throws it
   (instance of Throwable). Returns
     {:result <f result OR :threw> :calls <vec of :messages> :exception <or nil>}."
  [outcomes f]
  (let [calls (atom [])
        remaining (atom (vec outcomes))
        stub (fn [_router opts]
               (swap! calls conj (:messages opts))
               (let [next-out (first @remaining)]
                 (swap! remaining subvec 1)
                 (if (instance? Throwable next-out)
                   (throw next-out)
                   next-out)))]
    (with-redefs [llm/ask! stub]
      (try
        {:result    (f) :calls @calls :exception nil}
        (catch Throwable t
          {:result    :threw :calls @calls :exception t})))))

(defn- run-helper
  "Invoke `ask-with-schema-retry!` with sensible defaults. `chunks` is
   an atom that the helper appends to via `:on-chunk`."
  [chunks & {:keys [max-retries]
             :or   {max-retries 2}}]
  (iter/ask-with-schema-retry!
    ::router-stub
    {:spec ::iteration-spec-stub
     :messages [{:role "user" :content "Q"}]
     :routing {}
     :check-context? false}
    {:iteration   3
     :on-chunk    (fn [chunk] (swap! chunks conj chunk))
     :max-retries max-retries}))

(defdescribe ask-with-schema-retry-test
  (it "first-call success: returns immediately, no retry, no reminder"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask! [(ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 1 (count calls)))
      ;; No reminder was appended.
      (expect (= [{:role "user" :content "Q"}] (first calls)))
      ;; No on-chunk schema-reject events.
      (expect (empty? (filter :schema-reject-retry @chunks)))))

  (it "transient rejection: retry succeeds; ONE reminder, iteration loop never sees the failure"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "Looking at what I have so far")
             (ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 2 (count calls)))
      ;; First call: original messages.
      (expect (= 1 (count (first calls))))
      ;; Second call: original messages + ONE reminder.
      (expect (= 2 (count (second calls))))
      (let [reminder (last (second calls))]
        (expect (= "user" (:role reminder)))
        (expect (re-find #"\[svar/schema-reject 1/2\]" (:content reminder)))
        (expect (re-find #"top-level value MUST be a JSON/EDN map"
                  (:content reminder)))
        ;; The reminder includes the literal raw-data preview so the
        ;; model sees what it sent.
        (expect (re-find #"Looking at what I have so far"
                  (:content reminder))))
      ;; on-chunk was notified exactly once with the retry counter.
      (let [retry-chunks (filter :schema-reject-retry @chunks)]
        (expect (= 1 (count retry-chunks)))
        (expect (= 1 (:schema-reject-retry (first retry-chunks))))
        (expect (= 2 (:schema-reject-max (first retry-chunks))))
        (expect (= 3 (:iteration (first retry-chunks)))))))

  (it "two transient rejections: second retry succeeds; reminder is replaced, not accumulated"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "first prose")
             (schema-reject-ex "second prose")
             (ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 3 (count calls)))
      ;; Each retry replaces the reminder, never accumulates -> messages
      ;; on attempt 2 have exactly 2 entries (original + 1 reminder),
      ;; not 3.
      (expect (= 1 (count (nth calls 0))))
      (expect (= 2 (count (nth calls 1))))
      (expect (= 2 (count (nth calls 2))))
      ;; Reminders carry the CURRENT attempt counter, not a stale one.
      (expect (re-find #"\[svar/schema-reject 1/2\]"
                (:content (last (nth calls 1)))))
      (expect (re-find #"\[svar/schema-reject 2/2\]"
                (:content (last (nth calls 2)))))
      ;; The second reminder cites the second prose preview, not the
      ;; first -- the helper inspects the current rejection.
      (expect (re-find #"second prose"
                (:content (last (nth calls 2)))))))

  (it "rejection budget exhausted: bubbles out to the iteration loop with the original ex-data"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "p1")
             (schema-reject-ex "p2")
             (schema-reject-ex "p3")]
            #(run-helper chunks :max-retries 2))]
      (expect (= :threw result))
      (expect (some? exception))
      (expect (= :svar.spec/schema-rejected (:type (ex-data exception))))
      (expect (= "String" (:received-type (ex-data exception))))
      ;; 1 + 2 retries = 3 attempts.
      (expect (= 3 (count calls)))
      ;; Two retry chunks fired for the two retries (the final
      ;; bubble-out is NOT a retry chunk -- it's a real failure).
      (expect (= 2 (count (filter :schema-reject-retry @chunks))))))

  (it "non-schema rejection bubbles immediately, no retry"
    (let [chunks (atom [])
          other-ex (ex-info "boom" {:type :something/else})
          {:keys [result calls exception]}
          (with-stubbed-ask! [other-ex]
            #(run-helper chunks))]
      (expect (= :threw result))
      (expect (= other-ex exception))
      (expect (= 1 (count calls)))
      (expect (empty? (filter :schema-reject-retry @chunks)))))

  (it "max-retries=0 disables the retry layer (parity with calling llm/ask! directly)"
    (let [chunks (atom [])
          {:keys [result exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "first prose")]
            #(run-helper chunks :max-retries 0))]
      (expect (= :threw result))
      (expect (= :svar.spec/schema-rejected (:type (ex-data exception))))
      (expect (empty? (filter :schema-reject-retry @chunks))))))
