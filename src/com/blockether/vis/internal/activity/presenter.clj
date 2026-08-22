(ns com.blockether.vis.internal.activity.presenter
  "Closed semantic presenter registry for Activity rows. Presenters return data,
   never channel markup, and never inspect Python source.")

(def presenters #{:generic :shell :tests :patch :observation :lint :repl :format :list})

(defn presenter-for
  "The explicitly declared presenter, or the bounded generic fallback."
  [_operation declared]
  (if (contains? presenters declared) declared :generic))

(defn classification
  "The symbol entry's explicit tag, or generic when no declaration exists."
  [event]
  (or (:classification event) :generic))

(defn row-summary
  "Bounded already-redacted summary selected from semantic event fields."
  [event]
  (or (:phrase event)
      (:label event)
      (:result-summary event)
      (:error-summary event)
      (some-> (:operation event)
              name)))
