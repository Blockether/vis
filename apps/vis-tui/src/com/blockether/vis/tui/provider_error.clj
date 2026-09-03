(ns com.blockether.vis.tui.provider-error
  "Provider-error presentation projected by the gateway.

   The standalone TUI does not classify provider failures or depend on Svar. The
   gateway owns that decision and attaches `:provider-error-info` to each error;
   this namespace only exposes the projected fields to the renderer.")

(defn- info [err] (when (map? err) (:provider-error-info err)))

(defn provider-error-kind [err] (or (:kind (info err)) :generic))

(defn provider-error-title [err] (:title (info err)))

(defn provider-error-explanation [err] (:explanation (info err)))

(defn provider-error-next-step [err] (:next-step (info err)))

(defn provider-error-facts [err] (vec (or (:facts (info err)) [])))

(defn provider-error-raw-body [err] (:body (info err)))

(defn split-error-label
  "Split an `ALL-CAPS: body` presentation line into `[label body]`."
  [s]
  (if-let [[_ label body] (re-matches #"(?s)^([A-Z ]+):\s*(.*)$" (str s))]
    [(str label ": ") body]
    [nil (str s)]))
