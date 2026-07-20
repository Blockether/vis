#!/usr/bin/env bb
;; Strip native-image-agent noise from reachability-metadata.json.
;;
;; The agent (run with config-merge-dir) MERGES, so each regen re-captures
;; Clojure-internal classes that are pure noise under graal-build-time's
;; `InitClojureClasses` (which build-time-initializes every Clojure class):
;;
;;   * bare `<ns>__init` namespace classes   — no members requested; redundant
;;   * `…$fn__<N>` anonymous-fn classes        — the gensym <N> changes every
;;                                               compile, so pinning it is stale
;;
;; Run AFTER regenerating with the native-image tracing agent:
;;   bb scripts/clean-reachability.bb            # clean every metadata file
;;   bb scripts/clean-reachability.bb <file ...> # clean specific files
;;
;; Removes ONLY those two categories; every real reflection/resource/foreign
;; entry (Java classes, methods, fields, jni, proxy) is kept untouched.

(require '[cheshire.core :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[babashka.fs :as fs])

(defn- noise? [entry]
  (let [t (get entry "type")]
    (when (string? t)
      (or (re-find #"\$fn__\d+$" t)
          (and (str/ends-with? t "__init")
               ;; bare: only a "type" key, no methods/fields/jni requested
               (= ["type"] (keys entry)))))))

(defn- clean-file [path]
  (let [data (json/parse-string (slurp path))
        refl (get data "reflection")]
    (if-not (vector? refl)
      (do (println "skip (no reflection):" path) 0)
      (let [kept (vec (remove noise? refl))
            removed (- (count refl) (count kept))]
        (when (pos? removed)
          (spit path (str (json/generate-string (assoc data "reflection" kept)
                                                 {:pretty true})
                          "\n")))
        (println (format "%-70s %d -> %d (removed %d)"
                         path (count refl) (count kept) removed))
        removed))))

(defn- all-files []
  (->> (fs/glob "." "**/META-INF/native-image/**/reachability-metadata.json")
       (map str)
       (remove #(str/includes? % "/target/"))
       sort))

(let [files (if (seq *command-line-args*) *command-line-args* (all-files))
      total (reduce + 0 (map clean-file files))]
  (println (format "Done. Removed %d noise entr%s across %d file(s)."
                   total (if (= total 1) "y" "ies") (count files))))
