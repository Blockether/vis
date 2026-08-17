(ns com.blockether.vis.internal.vision-describe-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.vision-describe :as vd]
            [lazytest.core :refer [defdescribe expect it]]))

;; 1x1 red PNG — REAL pixels. Every image this side-channel sends crosses the same
;; send-time gate as a wire image, so a placeholder payload is (correctly) refused.
(def ^:private png-b64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")

(defn- image
  [label]
  {:path label :filename label :media-type "image/png" :base64 png-b64 :size 68 :size-label "68B"})

(defn- distinct-image
  "An image with unique BYTES, so the content-keyed cache treats it as its own row."
  [label]
  (assoc (image label) :base64 (str png-b64 label)))

(defn- mixed-fleet
  "The shape this feature exists for: the cheap/default model is BLIND, and one
   model elsewhere in the same fleet can see."
  []
  (svar/make-router
    [{:id :blind
      :api-key "k"
      :base-url "http://blind.invalid"
      :api-style :openai
      :models [{:name "cheap-blind"
                :pricing {:input 0.1 :output 0.2}
                :intelligence :low
                :speed :fast
                :capabilities #{:chat}}]}
     {:id :seeing
      :api-key "k"
      :base-url "http://seeing.invalid"
      :api-style :openai
      :models [{:name "pricey-seer"
                :pricing {:input 2.0 :output 5.0}
                :intelligence :high
                :speed :medium
                :capabilities #{:chat :vision}}]}]))

(defn- blind-fleet
  []
  (svar/make-router [{:id :blind
                      :api-key "k"
                      :base-url "http://blind.invalid"
                      :api-style :openai
                      :models [{:name "cheap-blind" :capabilities #{:chat}}]}]))

(defn- with-asks
  "Run `f` with `svar/ask!` answering `reply` (a fn of the ask opts) and every call
   recorded. Returns `{:result … :calls [{:router … :opts …}]}`."
  [reply f]
  (vd/clear-cache!)
  (let
    [calls
     (atom [])

     result
     (with-redefs-fn {#'svar/ask! (fn [router opts]
                                    (swap! calls conj {:router router :opts opts})
                                    (reply opts))}
       f)]

    {:result result :calls @calls}))

(defn- described-label
  "The label the describer put ON THE WIRE, recovered from the ask opts."
  [opts]
  (second (re-find #"Image: ([^\s\\\"]+)" (pr-str (:messages opts)))))

(defn- descriptions
  "`ask!` answering with the image's own label, so alignment is provable."
  [opts]
  {:result {:description (str "seen: " (described-label opts))}})

(defdescribe
  vision-describe-routing-test
  "The side-channel picks a SEEING model by capability and never spends the
   foreground turn's rate-limit budget doing it."
  (it "resolves the fleet's seeing model, not its cheap blind default"
      (let [m (vd/sighted-model (mixed-fleet))]
        (expect (= "pricey-seer" (:name m)))
        (expect (= :seeing (:provider m)))
        (expect (contains? (:capabilities m) :vision))))
  (it "resolves nothing when no configured model can see"
      (expect (nil? (vd/sighted-model (blind-fleet))))
      (expect (false? (vd/available? (blind-fleet))))
      (expect (true? (vd/available? (mixed-fleet)))))
  ;; Regression: Vis passes router-SHAPED config maps around (its own
  ;; `resolve-effective-model` is structural for exactly that reason), but this probe
  ;; went straight to svar's stateful resolver — a router with no live provider state
  ;; threw an NPE from inside request assembly and killed the whole turn, images or no
  ;; images.
  (it "treats a router it cannot resolve as a blind fleet instead of throwing"
      (let [config-shaped {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}]
        (expect (nil? (vd/sighted-model config-shaped)))
        (expect (false? (vd/available? config-shaped)))
        (expect (nil? (vd/describe-images config-shaped "ctx" [(image "/tmp/a.png")])))
        (expect (= {} (vd/describe-attachments config-shaped "ctx" [(image "/tmp/a.png")])))
        (expect (nil? (vd/sighted-model nil)))
        (expect (false? (vd/available? nil)))))
  (it "asks with a vision-required routing and an agent-initiated header"
      (let
        [{:keys [calls]}
         (with-asks descriptions #(vd/describe-images (mixed-fleet) "ctx" [(image "/tmp/a.png")]))

         opts
         (:opts (first calls))]

        (expect (= 1 (count calls)))
        (expect (= #{:vision} (:capabilities (:routing opts))))
        (expect (= :off (:reasoning opts)))
        (expect (some? (:spec opts)))
        (expect (= (:llm-headers opts) rt/AGENT_INITIATOR_HEADERS))))
  (it "runs on a router that never waits out a 429 and retries once"
      (let
        [{:keys [calls]}
         (with-asks descriptions #(vd/describe-images (mixed-fleet) "ctx" [(image "/tmp/a.png")]))

         router
         (:router (first calls))]

        ;; The user's turn owns the quota: no backoff schedule, no cross-provider
        ;; failover, one HTTP attempt.
        (expect (= [] (:same-provider-delays-ms (:rate-limit router))))
        (expect (false? (:fallback-provider? (:rate-limit router))))
        (expect (false? (:respect-retry-after? (:rate-limit router))))
        (expect (= 1 (:max-retries (:network router))))))
  (it "sends the pixels and the user's request to the describing model"
      (let
        [{:keys [calls]}
         (with-asks
           descriptions
           #(vd/describe-images (mixed-fleet) "why does the build fail?" [(image "/tmp/shot.png")]))

         wire
         (pr-str (:messages (:opts (first calls))))]

        (expect (str/includes? wire png-b64))
        (expect (str/includes? wire "/tmp/shot.png"))
        (expect (str/includes? wire "why does the build fail?")))))

(defdescribe
  vision-describe-images-test
  "`describe-images` answers ALIGNED to its input, once per image."
  (it "returns one description per image, carrying the describing model"
      (let
        [{:keys [result calls]}
         (with-asks
           descriptions
           #(vd/describe-images (mixed-fleet) "ctx" [(distinct-image "a") (distinct-image "b")]))]
        (expect (= 2 (count calls)))
        (expect (= 2 (count result)))
        ;; Aligned to the INPUT order, not to whatever order the asks finished in.
        (expect (= ["seen: a" "seen: b"] (mapv :text result)))
        (expect (= #{"pricey-seer"} (set (map :model result))))))
  (it "describes the same image ONCE — a replayed attachment is not re-billed"
      (vd/clear-cache!)
      (let
        [calls
         (atom 0)

         img
         (distinct-image "cached")

         run
         (fn []
           (with-redefs-fn {#'svar/ask! (fn [_ _]
                                          (swap! calls inc)
                                          {:result {:description "a red pixel"}})}
             #(vd/describe-images (mixed-fleet) "ctx" [img])))

         first-pass
         (run)

         second-pass
         (run)]

        (expect (= 1 @calls))
        (expect (= "a red pixel" (:text (first first-pass))))
        (expect (= first-pass second-pass))))
  (it "returns nil — and asks nothing — when the fallback toggle is off"
      (toggles/set-value! "vision_fallback_describe" false)
      (try (let
             [{:keys [result calls]}
              (with-asks descriptions
                         #(vd/describe-images (mixed-fleet) "ctx" [(image "/tmp/a.png")]))]
             (expect (nil? result))
             (expect (empty? calls)))
           (finally (toggles/reset-to-default! "vision_fallback_describe"))))
  (it "returns nil — and asks nothing — when the whole fleet is blind"
      (let
        [{:keys [result calls]}
         (with-asks descriptions #(vd/describe-images (blind-fleet) "ctx" [(image "/tmp/a.png")]))]
        (expect (nil? result))
        (expect (empty? calls))))
  (it "returns nil for no images at all"
      (expect (nil? (vd/describe-images (mixed-fleet) "ctx" []))))
  (it "keeps alignment when the describing model refuses"
      (let
        [{:keys [result]} (with-asks
                            (fn [_]
                              (throw (ex-info "429 from the plan" {})))
                            #(vd/describe-images (mixed-fleet) "ctx" [(distinct-image "x")]))]
        ;; A refusal costs the description, never the turn: the caller falls back
        ;; to exactly today's blind manifest.
        (expect (= [nil] result))))
  (it "caps how many UNCACHED images one pass describes"
      (let
        [images
         (mapv #(distinct-image (str "img-" %)) (range 9))

         {:keys [result calls]}
         (with-asks descriptions #(vd/describe-images (mixed-fleet) "ctx" images))]

        (expect (= 6 (count calls)))
        (expect (= 9 (count result)))
        (expect (= 6 (count (filter some? result))))
        (expect (every? nil? (drop 6 result))))))

(defdescribe
  vision-describe-attachments-test
  "`describe-attachments` keys its answer by the SAME label the prompt manifest
   prints, so a description and the row it describes cannot drift apart."
  (it "keys descriptions by the attachment's manifest label"
      (let
        [att
         {:path "/tmp/shot.png" :media-type "image/png" :base64 png-b64}

         {:keys [result]}
         (with-asks descriptions #(vd/describe-attachments (mixed-fleet) "ctx" [att]))]

        (expect (= ["/tmp/shot.png"] (keys result)))
        (expect (= (attachments/image-label att) (first (keys result))))
        (expect (= "seen: /tmp/shot.png" (:text (get result "/tmp/shot.png"))))))
  (it "never describes a HUMAN-ONLY row"
      (let
        [{:keys [result calls]} (with-asks descriptions
                                           #(vd/describe-attachments (mixed-fleet)
                                                                     "ctx"
                                                                     [{:path "/tmp/private.png"
                                                                       :media-type "image/png"
                                                                       :base64 png-b64
                                                                       :audience "user"}]))]
        ;; The caller kept those bytes off the wire on purpose; a side-channel that
        ;; sends them anyway would defeat `audience="user"` entirely.
        (expect (empty? calls))
        (expect (= {} result))))
  (it "answers an empty map when nothing describable came out of the gate"
      (let
        [{:keys [result]} (with-asks descriptions
                                     #(vd/describe-attachments (mixed-fleet) "ctx" []))]
        (expect (= {} result)))))

(defdescribe vision-descriptions-message-test
             "The replay message is TEXT, attributed, and honest about being second-hand."
             (it "names the model, quotes every description and warns it is not sight"
                 (let
                   [msg
                    (vd/descriptions-message
                      [{:label "fig-1.png" :text "a red pixel" :model "pricey-seer"}
                       {:label "fig-2.png" :text "a bar chart" :model "pricey-seer"}])

                    content
                    (:content msg)]

                   (expect (= "user" (:role msg)))
                   (expect (string? content))
                   (expect (str/includes? content "pricey-seer"))
                   (expect (str/includes? content "fig-1.png: a red pixel"))
                   (expect (str/includes? content "fig-2.png: a bar chart"))
                   (expect (str/includes? content "second-hand"))
                   (expect (str/includes? content "read_attachment"))))
             (it "is nil when nothing was described"
                 (expect (nil? (vd/descriptions-message [])))
                 (expect (nil? (vd/descriptions-message nil)))))

(defdescribe
  vision-fallback-end-to-end-test
  "The whole blind path as the loop wires it: a user attachment crosses the
   send-time image gate, a sighted model reports on it, and the report lands in the
   provider message the BLIND model actually receives — with no pixels attached."
  (it "turns an unseeable attachment into text in the outgoing user message"
      (let
        [att
         {:path "/tmp/shot.png" :media-type "image/png" :base64 png-b64 :size 68 :size-label "68B"}

         {:keys [result]}
         (with-asks descriptions
                    #(vd/describe-attachments (mixed-fleet) "what is on the screen?" [att]))

         msgs
         (prompt/assemble-initial-messages {:stable-prompt-messages []
                                            :initial-user-content "what is on /tmp/shot.png?"
                                            :vision? false
                                            :user-images [att]
                                            :image-descriptions result})

         user
         (last msgs)]

        ;; A blind wire: plain string content, no image block anywhere.
        (expect (string? (:content user)))
        (expect (not (str/includes? (pr-str msgs) png-b64)))
        ;; …but the content is no longer lost: it is quoted, attributed, and flagged.
        (expect (str/includes? (:content user) "seen: /tmp/shot.png"))
        (expect (str/includes? (:content user) "pricey-seer"))
        (expect (str/includes? (:content user) "second-hand"))))
  (it "leaves the message exactly as it is today when nothing can see"
      (let
        [att
         {:path "/tmp/shot.png" :media-type "image/png" :base64 png-b64 :size 68 :size-label "68B"}

         {:keys [result calls]}
         (with-asks descriptions
                    #(vd/describe-attachments (blind-fleet) "what is on the screen?" [att]))

         user
         (last (prompt/assemble-initial-messages {:stable-prompt-messages []
                                                  :initial-user-content "look"
                                                  :vision? false
                                                  :user-images [att]
                                                  :image-descriptions result}))]

        (expect (empty? calls))
        (expect (str/includes? (:content user) "NOT attached"))
        (expect (str/includes? (:content user) "PIL")))))
