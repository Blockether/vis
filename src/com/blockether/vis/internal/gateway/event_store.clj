(ns com.blockether.vis.internal.gateway.event-store
  "Process-local replay files retain locally assigned cursors, never shared-bus sequence numbers."
  (:require [com.blockether.vis.internal.gateway.turn-archive :as archive]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files Path]))

(defonce ^:private locks (vec (repeatedly 64 #(Object.))))

(defn lock-for [sid] (nth locks (mod (hash (str sid)) (count locks))))

(def ^:dynamic *max-bytes* (* 16 1024 1024))

(defn write-event!
  "Persist before publishing the descriptor. Failed writes declare a replay gap."
  [event]
  (let [descriptor (select-keys event ["seq" "type" "turn_id" "iteration" "form_index"])]
    (try (let [file (archive/write! event)]
           (assoc descriptor
             ::file file
             ::bytes (Files/size (Path/of ^String file (make-array String 0)))))
         (catch Exception e
           (tel/log! {:level :error :id ::write-failed :data {:error-class (.getName (class e))}})
           (assoc descriptor ::missing? true)))))

(defn read-event [descriptor] (archive/read-turn (::file descriptor)))

(defn delete-event!
  [descriptor]
  (when-let [file (::file descriptor)]
    (archive/delete! file)))

(defn discard-replaced!
  "Delete retired files after the registry transition, never within a CAS retry."
  [before after extra]
  (let [retained (into #{} (keep ::file) after)]
    (doseq [descriptor (cond-> (vec before)
                         extra
                         (conj extra))
            :when (not (contains? retained (::file descriptor)))]

      (delete-event! descriptor))))
