(ns com.blockether.vis.loop.runtime.shared
  (:require [clojure.spec.alpha :as s]))

(def MAX_RESULT_DISPLAY_CHARS 30000)

(s/def ::env map?)
(s/def ::non-blank-string (s/and string? (complement clojure.string/blank?)))
(s/def ::iteration-store-opts map?)

(defn validate! [spec v]
  (when-not (s/valid? spec v)
    (throw (ex-info "Validation failed" {:spec spec :value v :explain (s/explain-data spec v)})))
  v)

(defn truncate
  ([s n]
   (let [s (str s)]
     (if (> (count s) n) (subs s 0 n) s))))

(defn truncate-with-marker [s n]
  (let [s (str s)]
    (if (> (count s) n)
      (str (subs s 0 (max 0 (- n 16))) " …[truncated]")
      s)))

(defn strip-sandbox-ns [s]
  (-> (str s)
      (clojure.string/replace "sandbox/" "")))

(defn realize-value [v]
  (cond
    (instance? clojure.lang.IDeref v) @v
    (map? v) (into {} (map (fn [[k vv]] [k (realize-value vv)]) v))
    (vector? v) (mapv realize-value v)
    (set? v) (set (map realize-value v))
    (sequential? v) (doall (map realize-value v))
    :else v))

(defn result->display
  ([v] (result->display v :short))
  ([v _mode] (truncate-with-marker (pr-str (realize-value v)) MAX_RESULT_DISPLAY_CHARS)))

(defn shape [v]
  (cond
    (map? v) :map
    (vector? v) :vector
    (set? v) :set
    (list? v) :list
    (string? v) :string
    (number? v) :number
    (keyword? v) :keyword
    (symbol? v) :symbol
    (nil? v) :nil
    :else :value))

(defn ->uuid [] (java.util.UUID/randomUUID))

(defn format-exception-short [^Throwable t]
  {:class (.getName (class t))
   :message (or (ex-message t) (str t))})

(defn format-exception [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t)
    {:data (ex-data t)
     :context context}))
