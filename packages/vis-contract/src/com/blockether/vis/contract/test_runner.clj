(ns com.blockether.vis.contract.test-runner
  "Language-neutral test selection and result contract, validated by JSON Schema."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(def ^:private contract (delay (document/load! "test-runner")))

(defn package-document "The validated language-neutral test-runner document." [] @contract)

(def selector-keys
  "Optional selector keys accepted by every runner."
  (mapv keyword (get @contract "selector_keys")))

(def result-keys
  "Uniform result keys shared by every runner."
  (mapv keyword (get @contract "result_keys")))

(defn selectors-valid?
  "True when `value` satisfies the selector JSON Schema."
  [value]
  (document/valid? "test-runner" "selectors" value))

(defn result-valid?
  "True when `value` satisfies the test-result JSON Schema."
  [value]
  (document/valid? "test-runner" "result" value))


(defn ->str-vec
  "Coerce a scalar or sequence to trimmed non-blank strings."
  [x]
  (let [xs (cond (nil? x) []
                 (sequential? x) x
                 :else [x])]
    (->> xs
         (map str)
         (map str/trim)
         (remove str/blank?)
         vec)))

(defn split-node-id
  "Split `<path>::<test-name>` into `{:path :var}`. Blank parts become nil."
  [entry]
  (let [[p v] (str/split (str entry) #"::" 2)]
    {:path (not-empty (str/trim (str p))) :var (not-empty (str/trim (str v)))}))

(defn normalize-selectors
  "Normalize raw selectors and split every path node id."
  [m]
  (let [m (or m {})]
    {:paths (mapv split-node-id (->str-vec (:paths m)))
     :include (->str-vec (:include m))
     :exclude (->str-vec (:exclude m))}))

(defn selected?
  "Apply node-id and tag selectors to one resolved test. Exclusion wins."
  [{:keys [vars include exclude]} {test-ns :ns test-name :name tags :tags}]
  (let [tags
        (set tags)

        inc*
        (set include)

        exc*
        (set exclude)

        var-hit?
        (fn [{:keys [ns name]}]
          (and (or (nil? ns) (= ns test-ns)) (= name test-name)))]

    (cond (some exc* tags) false
          (and (seq vars) (not (some var-hit? vars))) false
          (and (seq inc*) (not (some inc* tags))) false
          :else true)))
