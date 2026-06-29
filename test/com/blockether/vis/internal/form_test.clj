(ns com.blockether.vis.internal.form-test
  "Guard: the canonical per-form DISPLAY set survives the gateway round-trip.

   This is the regression net for the whole class of bug that motivated
   `internal/form.clj` — a boundary (the gateway `block.output` payload, the
   client projection) silently dropping a display field so the live channel lost
   the card / badge while persisted ones kept it. If a NEW key is added to
   `form/display-keys` but a boundary stops carrying it, `survives-the-gateway`
   fails — no more chasing it through tmux."
  (:require
   [com.blockether.vis.internal.form :as form]
   [com.blockether.vis.internal.gateway.state :as gw]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private chunk->event @#'gw/chunk->event)

(defn- sentinel
  "A non-nil sentinel VALUE per display key, shaped like the real thing so the
   round-trip exercises keyword-valued fields (`:tool-color-role`) and scalars."
  [k]
  (case k
    :tool-color-role :tool-color/search
    :result-kind     :tool
    :result-detail   {:op :rg :hit-count 9}
    :render-segments [{:kind :code}]
    (:timeout? :repaired? :auto-repaired?) true
    ;; everything else: a distinctive string
    (str "sentinel-" (namespace k) "-" (name k))))

(defn- simulate-wire
  "Mimic the JSON wire: keyword KEYS become snake_case strings and keyword VALUES
   become plain strings (the exact lossiness `<-wire` must undo)."
  [payload]
  (into {}
    (map (fn [[k v]]
           [(if (keyword? k)
              (clojure.string/replace (str (when (namespace k) (str (namespace k) "/")) (name k)) "-" "_")
              k)
            (if (keyword? v) (subs (str v) 1) v)]))
    payload))

(defdescribe form-gateway-roundtrip-test
  (it "every display key survives loop chunk -> gateway block.output -> wire -> <-wire"
    (let [chunk (into {:phase :form-result :iteration 1 :position 0}
                  (map (fn [k] [k (sentinel k)]))
                  form/display-keys)
          [type _store payload] (chunk->event chunk)
          back (form/<-wire (simulate-wire payload))]
      (expect (= "block.output" type))
      ;; The gateway carried, and <-wire recovered, EVERY canonical display key.
      (doseq [k form/display-keys]
        (expect (some? (get back k))
          (str k " was dropped on the gateway round-trip — add it to a boundary projection")))
      ;; Keyword-valued fields come back as KEYWORDS (not the wire's strings), or
      ;; the channel's keyword dispatch (badge colour) silently misses.
      (expect (= :tool-color/search (:tool-color-role back)))
      (expect (keyword? (:tool-color-role back)))))

  (it "->display drops nils so a merge never stamps empty keys"
    (expect (= {} (form/->display {:result nil :vis/tool-name nil})))
    (expect (= {:vis/tool-name "rg"} (form/->display {:vis/tool-name "rg" :result-render nil})))))
