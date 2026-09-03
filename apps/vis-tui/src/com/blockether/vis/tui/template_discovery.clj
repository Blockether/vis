(ns com.blockether.vis.tui.template-discovery
  "Stable first-wins de-duplication for local prompt templates.")

(defn dedup-by-name
  [rows]
  (->> rows
       (reduce (fn [{:keys [seen out]} row]
                 (let [name (:name row)]
                   (if (contains? seen name)
                     {:seen seen :out out}
                     {:seen (conj seen name) :out (conj out row)})))
               {:seen #{} :out []})
       :out))
