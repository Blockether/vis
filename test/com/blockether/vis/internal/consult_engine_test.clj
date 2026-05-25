(ns com.blockether.vis.internal.consult-engine-test
  "mini-SCI consult runner tests.

   The router is mocked via with-redefs on `svar/ask-code!`. Tests
   exercise the per-intent runner end-to-end: prompt assembly →
   provider call (mocked) → fence parse → SCI eval → entry-map shape."
  (:require
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.consult :as consult]
   [com.blockether.vis.internal.consult-engine :as ce]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env []
  {:router ::router
   :ctx-atom (atom {:session/id "test" :session/turn 1
                    :session/scope {:turn 1 :iter 1 :next-form 1}
                    :session/specs {}
                    :session/tasks {}
                    :session/facts {}
                    :session/trailer []})
   :consult-config consult/DEFAULT_PREFERENCE_MAP
   :active-extensions []})

(defn- mk-intent
  ([] (mk-intent {}))
  ([overrides]
   (merge {:consult-id :review
           :preference :deep
           :focus ["verify claim X"]
           :question "Summarize the paper's evidence."
           :born "t1/i1/f1"}
     overrides)))

(defn- mock-ask-code [fence]
  (fn [_router _opts]
    {:blocks [{:lang "clojure" :source fence}]
     :raw fence}))

;; ---------------------------------------------------------------------------
;; happy paths
;; ---------------------------------------------------------------------------

(defdescribe run-consult-happy-path-test
  (describe "successful consult returns a structured entry"
    (let [env (mk-env)
          fence "(done {:content \"Reflexion (arxiv 2303.11366) reports 91% on HumanEval.\"
                        :citations [{:type :paper :url \"https://arxiv.org/abs/2303.11366\"
                                     :title \"Reflexion\"}]
                        :confidence :high
                        :focus-met? [true]})"]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]

          (it ":status :active when (answer …) was emitted"
            (expect (= :active (:status entry))))

          (it ":content carried verbatim"
            (expect (= "Reflexion (arxiv 2303.11366) reports 91% on HumanEval."
                      (:content entry))))

          (it ":citations preserved with full citation map"
            (expect (= 1 (count (:citations entry))))
            (expect (= :paper (-> entry :citations first :type))))

          (it ":confidence echoed"
            (expect (= :high (:confidence entry))))

          (it ":focus-met? vec preserved"
            (expect (= [true] (:focus-met? entry))))

          (it ":preference + :consult-id + :focus stamped from the intent"
            (expect (= :deep (:preference entry)))
            (expect (= :review (:consult-id entry)))
            (expect (= ["verify claim X"] (:focus entry))))

          (it ":duration-ms is a non-negative long"
            (expect (number? (:duration-ms entry)))
            (expect (<= 0 (:duration-ms entry))))

          (it ":retries starts at 0"
            (expect (= 0 (:retries entry))))))))

  (describe ":confidence defaults to :medium when omitted"
    (let [env (mk-env)
          fence "(done {:content \"plain answer\"})"]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it "defaulted"
            (expect (= :medium (:confidence entry))))))))

  (describe "citation list capped at consult/MAX_CITATIONS with `:too-many-citations` count"
    (let [env (mk-env)
          ;; 17 citations — 2 over MAX_CITATIONS (15)
          fence
          (str "(done {:content \"x\""
            " :citations [" (apply str
                              (repeat 17
                                "{:type :web :url \"u\" :title \"t\"} "))
            "]})")]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it ":citations truncated to MAX_CITATIONS"
            (expect (= consult/MAX_CITATIONS (count (:citations entry)))))
          (it ":too-many-citations stamps the dropped tail count"
            (expect (= 2 (:too-many-citations entry)))))))))

;; ---------------------------------------------------------------------------
;; failure paths
;; ---------------------------------------------------------------------------

(defdescribe run-consult-failure-paths-test
  (describe "no clojure fence in the response → :no-fence"
    (let [env (mk-env)]
      (with-redefs [svar/ask-code! (fn [& _]
                                     {:blocks [] :raw "no fence here"})]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it ":status :failed :error :no-fence"
            (expect (= :failed (:status entry)))
            (expect (= :no-fence (:error entry))))))))

  (describe "fence without (done …) → :missing-answer"
    (let [env (mk-env)
          fence "(def x 1)"]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it ":status :failed :error :missing-answer"
            (expect (= :failed (:status entry)))
            (expect (= :missing-answer (:error entry))))))))

  (describe "(done non-map) → :malformed-answer"
    (let [env (mk-env)
          fence "(done \"bare string\")"]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it ":status :failed :error :malformed-answer"
            (expect (= :failed (:status entry)))
            (expect (= :malformed-answer (:error entry))))))))

  (describe "(done {:content blank}) → :malformed-answer"
    (let [env (mk-env)
          fence "(done {:content \"   \"})"]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it ":status :failed"
            (expect (= :failed (:status entry))))
          (it ":error :malformed-answer"
            (expect (= :malformed-answer (:error entry))))))))

  (describe "router throws → :provider-error"
    (let [env (mk-env)]
      (with-redefs [svar/ask-code! (fn [& _] (throw (ex-info "503 upstream" {})))]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it ":status :failed :error :provider-error"
            (expect (= :failed (:status entry)))
            (expect (= :provider-error (:error entry)))
            (expect (= "503 upstream" (:reason entry))))))))

  (describe "missing routing for preference → :consult-router-missing"
    (let [env (assoc (mk-env) :consult-config {:fast {:provider :a :model "m"}})
          intent (mk-intent {:preference :deep})]
      (let [entry (ce/run-consult! env intent)]
        (it ":status :failed :error :consult-router-missing"
          (expect (= :failed (:status entry)))
          (expect (= :consult-router-missing (:error entry))))))))

;; ---------------------------------------------------------------------------
;; bad citations: dropped silently
;; ---------------------------------------------------------------------------

(defdescribe citation-coercion-test
  (describe "drops malformed citations (no :type, no url+title, bad :type)"
    (let [env (mk-env)
          fence (str "(done {:content \"x\""
                  " :citations [{:type :paper :url \"u\" :title \"good\"}"
                  "             {:url \"u\"}"        ;; no :type → drop
                  "             {:type :paper}"      ;; no url+title → drop
                  "             {:type :nuclear :url \"u\"}]})")] ;; bad :type → drop
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))]
          (it "only the well-formed paper citation survives"
            (expect (= 1 (count (:citations entry))))
            (expect (= :paper (-> entry :citations first :type)))
            (expect (= "good" (-> entry :citations first :title)))))))))

;; ---------------------------------------------------------------------------
;; thread isolation: primary's ctx-atom is NOT mutated by the runner
;; ---------------------------------------------------------------------------

(defdescribe thread-isolation-test
  (describe "the runner does not mutate primary's ctx-atom"
    (let [env (mk-env)
          baseline @(:ctx-atom env)
          fence "(done {:content \"x\"})"]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (ce/run-consult! env (mk-intent))
        (it "ctx-atom unchanged after the runner returned"
          (expect (= baseline @(:ctx-atom env))))))))

;; ---------------------------------------------------------------------------
;; Retry-to-fit (token cap on :content)
;; ---------------------------------------------------------------------------

(defn- words [n]
  ;; Roughly 1 token per word for these short ASCII tokens.
  (clojure.string/join " " (repeat n "word")))

(defn- mock-ask-multistep [fences]
  ;; Returns successive fences on successive calls. Uses an atom so the
  ;; second call sees the second fence and so on.
  (let [box (atom (vec fences))]
    (fn [_router _opts]
      (let [f (first @box)]
        (swap! box subvec 1)
        {:blocks [{:lang "clojure" :source f}]
         :raw f}))))

(defdescribe retry-to-fit-test
  (describe "first attempt fits the cap → no retry, :retries 0"
    (let [env (mk-env)
          fence (str "(done {:content \"" (words 50) "\"})")] ;; ≈ 50 tokens; well under 1k :fast cap
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent {:preference :fast}))]
          (it ":status :active"
            (expect (= :active (:status entry))))
          (it ":retries 0"
            (expect (= 0 (:retries entry))))))))

  (describe "first attempt overflows, retry fits → :status :active :retries 1"
    (let [env (mk-env)
          ;; :fast cap is 1000 tokens. First fence has ~1500 words; second has ~50.
          big   (str "(done {:content \"" (words 1500) "\"})")
          small (str "(done {:content \"" (words 50) "\"})")]
      (with-redefs [svar/ask-code! (mock-ask-multistep [big small])]
        (let [entry (ce/run-consult! env (mk-intent {:preference :fast}))]
          (it ":status :active"
            (expect (= :active (:status entry))))
          (it ":retries 1"
            (expect (= 1 (:retries entry))))
          (it ":content carries the SECOND (compressed) answer"
            (expect (clojure.string/includes? (:content entry) "word")))))))

  (describe "both attempts overflow → :status :failed :error :exceeds-cap"
    (let [env (mk-env)
          big1 (str "(done {:content \"" (words 1500) "\"})")
          big2 (str "(done {:content \"" (words 1400) "\"})")]
      (with-redefs [svar/ask-code! (mock-ask-multistep [big1 big2])]
        (let [entry (ce/run-consult! env (mk-intent {:preference :fast}))]
          (it ":status :failed"
            (expect (= :failed (:status entry))))
          (it ":error :exceeds-cap"
            (expect (= :exceeds-cap (:error entry))))
          (it ":retries 1 (attempted exactly twice)"
            (expect (= 1 (:retries entry))))
          (it ":reason carries both attempt token counts + cap"
            (expect (clojure.string/includes? (:reason entry) "cap 1000")))))))

  (describe "second attempt fails (provider error) carries :retries 1 forward"
    (let [env (mk-env)
          big (str "(done {:content \"" (words 1500) "\"})")]
      ;; first call returns oversize fence; second throws
      (let [calls (atom 0)]
        (with-redefs [svar/ask-code!
                      (fn [_r _o]
                        (let [n (swap! calls inc)]
                          (if (= n 1)
                            {:blocks [{:lang "clojure" :source big}]}
                            (throw (ex-info "503" {})))))]
          (let [entry (ce/run-consult! env (mk-intent {:preference :fast}))]
            (it ":status :failed"
              (expect (= :failed (:status entry))))
            (it ":error :provider-error"
              (expect (= :provider-error (:error entry))))
            (it ":retries stamps 1 (second attempt was made)"
              (expect (= 1 (:retries entry))))))))))

(defdescribe excerpt-clip-test
  (describe "long citation excerpts are tail-clipped at MAX_EXCERPT_TOKENS"
    (let [env (mk-env)
          big-excerpt (words 1000) ;; well over 500-token cap
          fence (str "(done {:content \"x\" :citations [{:type :paper :url \"u\" :title \"t\" :excerpt \""
                  big-excerpt "\"}]})")]
      (with-redefs [svar/ask-code! (mock-ask-code fence)]
        (let [entry (ce/run-consult! env (mk-intent))
              excerpt (-> entry :citations first :excerpt)]
          (it "citation kept"
            (expect (= 1 (count (:citations entry)))))
          (it "excerpt is shorter than the original"
            (expect (< (count excerpt) (count big-excerpt))))
          (it "excerpt fits within MAX_EXCERPT_TOKENS"
            (let [tokens-ns (the-ns 'com.blockether.vis.internal.tokens)
                  count-tokens (ns-resolve tokens-ns 'count-tokens)]
              (expect (<= (count-tokens excerpt) 500)))))))))
