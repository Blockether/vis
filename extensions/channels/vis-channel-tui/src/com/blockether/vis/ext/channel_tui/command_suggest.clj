(ns com.blockether.vis.ext.channel-tui.command-suggest
  "Pure slash-command discovery/filtering for the TUI prompt."
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:private default-limit 6)

(defn clamp-index
  [idx total]
  (let [total (long (max 0 (long (or total 0))))]
    (if (zero? total) 0 (max 0 (min (dec total) (long (or idx 0)))))))

(defn move-index
  ;; CLAMP at the ends — no wraparound. Scrolling past the first/last row
  ;; parks there instead of cycling forever (both slash + `@` file pickers).
  [idx delta total]
  (let [total (long (max 0 (long (or total 0))))]
    (if (zero? total) 0 (max 0 (min (dec total) (+ (long (or idx 0)) (long (or delta 0))))))))

(defn- command-id->name
  [id]
  (cond (keyword? id) (if-let [ns (namespace id)]
                        (str ns "/" (name id))
                        (name id))
        (symbol? id) (str id)
        :else (str id)))

(defn- command-args [cmd] (or (:args cmd) (:cmd/args cmd) []))

(defn- arg-name [arg] (str (or (:name arg) (:cmd.arg/name arg))))

(defn- flag-token
  [arg]
  (let [n (arg-name arg)]
    (if (str/starts-with? n "--") n (str "--" n))))

(defn- arg-token
  [{:keys [kind type required] :as arg}]
  (let [n
        (arg-name arg)

        required?
        (true? required)

        token
        (case kind
          :flag
          (if (= :boolean type) (flag-token arg) (str (flag-token arg) " <" n ">"))

          :positional
          (str "<" n ">")

          (str "<" n ">"))]

    (if required? token (str "[" token "]"))))

(defn- command-name [cmd] (command-id->name (:id cmd)))

(defn usage
  "Return compact slash-command usage, including extension-provided args."
  [cmd]
  (let [base
        (str "/" (command-name cmd))

        args
        (seq (keep arg-token (command-args cmd)))]

    (cond-> base
      args
      (str " " (str/join " " args)))))

(defn enrich
  [cmd]
  (let [name (or (:slash/name cmd) (command-name cmd))]
    (assoc cmd
      :slash/name name
      :slash/usage (or (:slash/usage cmd) (usage cmd))
      :slash/search (str/lower-case (str name " " (:label cmd) " " (:doc cmd))))))

(defn slash-query
  "Return {:query q :args rest} when text begins with a slash command."
  [text]
  (let [s (str/triml (or text ""))]
    (when (and (str/starts-with? s "/") (not (str/starts-with? s "//")))
      (let [without-slash (subs s 1)
            [token args] (str/split without-slash #"\s+" 2)]

        {:query (or token "") :args (or args "")}))))

(defn- slash-command-token-complete?
  [text]
  (let [s (str/triml (or text ""))]
    (and (str/starts-with? s "/")
         (not (str/starts-with? s "//"))
         (boolean (re-find #"\s" (subs s 1))))))

(defn- fuzzy-score
  [query candidate]
  (let [q
        (str/lower-case (or query ""))

        c
        (str/lower-case (or candidate ""))]

    (cond (str/blank? q) [0 0]
          (str/starts-with? c q) [0 0]
          (str/includes? c q) [1 (or (str/index-of c q) 0)]
          :else (loop [qi
                       0

                       ci
                       0

                       gaps
                       0

                       last-ci
                       -1]

                  (cond
                    (= qi (count q)) [2 gaps]
                    (= ci (count c)) nil
                    (= (.charAt q qi) (.charAt c ci))
                    (recur (inc qi) (inc ci) (+ gaps (if (neg? last-ci) ci (- ci last-ci 1))) ci)
                    :else (recur qi (inc ci) gaps last-ci))))))

(defn suggestions
  "Return slash-command suggestions for `text` from menu `commands`.
   Suggestions are fuzzy matched against id/name, label, and doc."
  ([text commands] (suggestions text commands {}))
  ([text commands {:keys [limit selected-index] :or {limit default-limit}}]
   (when-not (slash-command-token-complete? text)
     (when-let [{:keys [query args]} (slash-query text)]
       (let [matches (keep-indexed (fn [idx cmd]
                                     (let [cmd' (enrich cmd)
                                           score (fuzzy-score query (:slash/search cmd'))]

                                       (when score
                                         (assoc cmd'
                                           :slash/score score
                                           :slash/index idx
                                           :slash/args args))))
                                   commands)
             result (->> matches
                         (sort-by (juxt :slash/score :slash/index))
                         (take limit)
                         vec)
             sel (clamp-index selected-index (count result))]

         (mapv (fn [idx suggestion]
                 (assoc suggestion :slash/selected? (= idx sel)))
               (range)
               result))))))

(defn selected-suggestion
  [suggestions]
  (or (some #(when (:slash/selected? %) %) suggestions) (first suggestions)))

(defn completion-text
  [suggestion]
  (let [args (str/trim (or (:slash/args suggestion) ""))]
    (str "/" (:slash/name suggestion) (if (str/blank? args) " " (str " " args)))))

(defn exact-command
  "Return a command only when the typed slash token exactly names it."
  [text commands]
  (when-let [{:keys [query args]} (slash-query text)]
    (let [needle (str/lower-case query)]
      (some (fn [cmd]
              (let [cmd' (enrich cmd)]
                (when (= needle (str/lower-case (:slash/name cmd')))
                  (assoc cmd' :slash/args args))))
            commands))))
