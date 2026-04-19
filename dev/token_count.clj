(ns token-count
  "Dev-only tool: render the system prompt in a few common envs and count
   tokens with jtokkit so we know what we're paying per iteration.

   Usage:  clojure -M:dev -i dev/token_count.clj"
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.core :as core]
   [com.blockether.vis.loop.runtime.prompt :as prompt]
   [com.blockether.vis.loop.runtime.query.routing :as routing])
  (:import
   [com.knuddels.jtokkit Encodings]
   [com.knuddels.jtokkit.api EncodingType ModelType]))

(def ^:private registry (Encodings/newDefaultEncodingRegistry))

(defn- enc
  "Model-specific encoding when known (o200k for gpt-5/4o, cl100k for 3.5/4)."
  [model]
  (cond
    (str/includes? model "gpt-5")   (.getEncoding registry EncodingType/O200K_BASE)
    (str/includes? model "gpt-4o")  (.getEncoding registry EncodingType/O200K_BASE)
    (str/includes? model "claude")  (.getEncoding registry EncodingType/CL100K_BASE)
    :else                           (.getEncoding registry EncodingType/O200K_BASE)))

(defn- count-tokens [model text]
  (-> (enc model) (.countTokens text)))

(defn- stub-router []
  (llm/make-router [{:id :stub
                     :api-key "stub"
                     :base-url "http://localhost:1"
                     :models [{:name "gpt-5-mini"}]}]))

(defn- fresh-env []
  (core/create-env (stub-router) {:db :temp}))

(defn- render-prompt
  "Render the system prompt exactly like iteration-loop does."
  [env]
  (let [has-reasoning? (boolean (routing/provider-has-reasoning? (:router env)))]
    (prompt/build-system-prompt
      {:env env
       :has-reasoning? has-reasoning?
       :has-documents? false
       :tool-defs (when-let [a (:tool-registry-atom env)] (vals @a))
       :skill-registry (when-let [a (:skill-registry-atom env)] @a)})))

(defn- section-breakdown
  "Break the prompt into its labelled sections and count tokens per section,
   so we can see which block is actually expensive."
  [model text]
  (let [markers ["MINDSET:" "SYMBOLIC REASONING"
                 "CONTEXT MODEL" "ARCH:"
                 "STEERING" "GROUNDING:" "PERF:" "CLJ:"
                 "<tools>" "RESPONSE FORMAT:" "RULES:"]
        positions (for [m markers
                        :let [i (str/index-of text m)]
                        :when i]
                    [m i])
        sorted (sort-by second positions)
        bounds (concat [[ "PROLOGUE" 0]] sorted)
        segments (map (fn [[[name start] [_ end]]]
                        {:name name :text (subs text start (or end (count text)))})
                   (partition 2 1 (concat bounds [[nil (count text)]])))]
    (mapv (fn [{:keys [name text]}]
            {:section name
             :chars (count text)
             :tokens (count-tokens model text)})
      segments)))

(defn -main [& _]
  (let [env (fresh-env)]
    (try
      (let [prompt-text (render-prompt env)
            chars (count prompt-text)
            for-model (fn [m]
                        {:model m
                         :tokens (count-tokens m prompt-text)})]
        (spit "/tmp/vis-system-prompt.txt" prompt-text)
        (println "\n=== SYSTEM PROMPT SIZE (stub env, no docs, no git) ===")
        (println "chars:" chars)
        (doseq [m ["gpt-5-mini" "gpt-4o" "claude-sonnet-4-6"]]
          (let [r (for-model m)]
            (println (format "%-24s -> %6d tokens" (:model r) (:tokens r)))))
        (println "\n=== SECTION BREAKDOWN (gpt-5-mini / o200k_base) ===")
        (doseq [{:keys [section chars tokens]} (section-breakdown "gpt-5-mini" prompt-text)]
          (println (format "  %-22s %5d chars  %5d tokens"
                     (or section "?") chars tokens)))
        (println)
        (println "Full prompt written to /tmp/vis-system-prompt.txt"))
      (finally
        (core/dispose-env! env)))))
