(ns com.blockether.vis.tui.presets
  "The model of a settings row whose editor is `presets`: a preset gives one value to each
   slot. `editor_options` names the slots and the built-in presets; the value names the
   active preset and the presets of the person. Every function here returns a new value
   that the editor writes whole, and the gateway checks it."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.client :as vis])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(defn slots
  "The slots of a preset, in display order: `{\"id\" \"label\" \"description\"}` maps."
  [options]
  (vec (get options "slots")))

(defn builtins
  "The presets that Vis includes, in display order: `{\"id\" \"label\"}` maps."
  [options]
  (vec (get options "builtin")))

(defn customs
  "The presets of the person: `{\"name\" \"slots\"}` maps."
  [value]
  (vec (get value "custom")))

(defn builtin
  "The built-in preset with `id`, or nil."
  [options id]
  (some #(when (= id (get % "id")) %) (builtins options)))

(defn custom
  "The custom preset named `preset-name`, or nil."
  [value preset-name]
  (some #(when (= preset-name (get % "name")) %) (customs value)))

(defn active
  "The name or id of the active preset: the saved one, else the first built-in."
  [options value]
  (or (not-empty (str (get value "active"))) (get (first (builtins options)) "id")))

(defn preset-label
  "The display name of `preset`: the label of a built-in id, else the custom name itself."
  [options preset]
  (or (get (builtin options preset) "label") (str preset)))

(defn preset-slots
  "Slot id to value for `preset`. Each slot of a built-in preset is its id; a custom preset
   gives its own values, and a slot that it does not name is empty."
  [options value preset]
  (let [ids (map #(get % "id") (slots options))]
    (if (builtin options preset)
      (zipmap ids (repeat preset))
      (let [own (get (custom value preset) "slots")]
        (zipmap ids (map #(str (get own % "")) ids))))))

(defn slot-meaning
  "What a slot value does, in words: nothing, the sound of a built-in preset or a file."
  [options slot-value]
  (let [text (str slot-value)]
    (cond (str/blank? text) "No sound"
          (builtin options text) (str (preset-label options text) " (built in)")
          :else text)))

(defn help
  "How to fill in a slot value, with the built-in preset ids of `options`."
  [options]
  (str "Empty plays nothing. "
       (str/join ", " (map #(get % "id") (builtins options)))
       " play the sound of that preset. Other text is a file path on the gateway machine."))

(defn- with-active [value preset] {"active" preset "custom" (customs value)})

(defn choose "`value` with `preset` active." [value preset] (with-active value preset))

(defn add-preset
  "`value` with a new custom preset at the end, made active."
  [value preset-name preset-slots]
  {"active" preset-name "custom" (conj (customs value) {"name" preset-name "slots" preset-slots})})

(defn change-preset
  "`value` with the custom preset `old-name` renamed to `new-name` and given `preset-slots`.
   The active preset follows the rename."
  [value old-name new-name preset-slots]
  {"active" (let [current (get value "active")]
              (if (= current old-name) new-name current))
   "custom" (mapv #(if (= old-name (get % "name")) {"name" new-name "slots" preset-slots} %)
                  (customs value))})

(defn delete-preset
  "`value` without the custom preset `preset-name`. When it was active, the first built-in
   preset becomes active."
  [options value preset-name]
  {"active" (let [current (get value "active")]
              (if (= current preset-name) (get (first (builtins options)) "id") current))
   "custom" (vec (remove #(= preset-name (get % "name")) (customs value)))})

(defn full?
  "True when the person has as many presets as `max_custom` allows."
  [options value]
  (let [limit (get options "max_custom")]
    (boolean (and (number? limit) (>= (count (customs value)) (long limit))))))

(defn previewable?
  "True when the gateway can preview one slot value of this row."
  [options]
  (true? (get options "is_previewable")))

(defn preview-path
  "The gateway path that previews one slot value of the row `id`."
  [id]
  (str "/v1/settings/" (URLEncoder/encode (str id) StandardCharsets/UTF_8) "/preview"))

(defn preview-body
  "The preview request for `slot` of `preset`: the slot value that the preset gives."
  [options value preset slot]
  {"slot" slot "value" (get (preset-slots options value preset) slot "")})

(defn preview!
  "Play one slot value on the gateway machine. Returns `{:played? bool}` or `{:error text}`."
  [id body]
  (try (let [response
             (vis/request! :post (preview-path id) {:body body :timeout-ms 15000})

             parsed
             (try (wire/parse-json (str (:body response))) (catch Exception _ nil))]

         (if (= 200 (:status response))
           {:played? (true? (get parsed "is_played"))}
           {:error (or (get-in parsed ["error" "message"])
                       (get parsed "message")
                       (str "The gateway answered " (:status response) "."))}))
       (catch Exception e {:error (or (ex-message e) "The gateway did not answer.")})))
