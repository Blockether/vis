(ns com.blockether.vis.audit-inventory-test
  "`audit/README.md` is the licensing record reviewers rely on, and it is
   GENERATED from the network (`bb scripts/gen-audit.bb`). A CDN that has not
   yet published a freshly released artifact degrades one cell to `UNKNOWN`/`—`,
   and once that is committed the document states the wrong license for
   something we ship — exactly what happened to `com.blockether/imaging 0.1.7`.

   This gate is offline and cheap: every in-house `com.blockether/*` coordinate
   the root deps.edn pins must appear in the inventory at that exact version
   with a resolved license and jar size, and wherever Maven already cached the
   artifact the stated license must agree with the POM the build consumed."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private audit-file (io/file "audit" "README.md"))

(def ^:private row-re
  ;; | `group/artifact` | `version` | License | Jar size | Ownership |
  #"\|\s+`([^`]+)`\s+\|\s+`([^`]+)`\s+\|([^|]+)\|([^|]+)\|.*")

(defn- inventory
  "Coordinate -> {:version :license :size} for every §5 inventory row."
  []
  (into {}
        (keep (fn [line]
                (when-let [[_ sym version license size] (re-matches row-re line)]
                  [sym {:version version :license (str/trim license) :size (str/trim size)}])))
        (str/split-lines (slurp audit-file))))

(defn- in-house-coords
  "Every `com.blockether/*` mvn coordinate the root deps.edn pins."
  []
  (into (sorted-map)
        (keep (fn [[sym coord]]
                (when (and (symbol? sym) (= "com.blockether" (namespace sym)) (:mvn/version coord))
                  [(str sym) (:mvn/version coord)])))
        (:deps (edn/read-string (slurp (io/file "deps.edn"))))))

(defn- cached-pom-license
  "The <licenses><name> of the locally cached POM, when Maven resolved this
   coordinate; nil when the artifact is not in ~/.m2 (fresh CI checkout)."
  [coord version]
  (let
    [[group artifact]
     (str/split coord #"/")

     f
     (apply io/file
       (System/getProperty "user.home")
       ".m2"
       "repository"
       (concat (str/split group #"\.") [artifact version (str artifact "-" version ".pom")]))]

    (when (.isFile ^java.io.File f)
      (some-> (re-find #"(?s)<licenses>.*?<name>(.*?)</name>" (slurp f))
              second
              str/trim))))

(defn- normalize
  "The same POM-name -> short id mapping `scripts/gen-audit.bb` renders with,
   so a row can be compared against the artifact's own declaration."
  [raw]
  (let [s (str/lower-case (str raw))]
    (cond (or (str/includes? s "lesser general public") (str/includes? s "lgpl")) "LGPL-3.0"
          (str/includes? s "eclipse public") (if (str/includes? s "2.0") "EPL-2.0" "EPL-1.0")
          (str/includes? s "apache") "Apache-2.0"
          (or (str/includes? s "universal permissive") (re-find #"\bupl\b" s)) "UPL-1.0"
          (re-find #"\bmit\b" s) "MIT"
          :else raw)))

(defdescribe
  audit-inventory-test
  (it "states a resolved license and jar size for every in-house coordinate"
      (expect (.isFile audit-file))
      (let [rows (inventory)]
        (doseq
          [[coord version] (in-house-coords)
           :let [row (get rows coord)]]

          (expect (some? row) (str coord " is missing from audit/README.md"))
          (expect (= version (:version row))
                  (str coord " is pinned at " version " but audit/README.md lists " (:version row)))
          (expect (not (contains? #{"UNKNOWN" "(floating)" ""} (:license row)))
                  (str coord " has an unresolved license: " (:license row)))
          (expect (not= "—" (:size row)) (str coord " has an unresolved jar size")))))
  (it "keeps com.blockether/imaging MIT — the license its published POM declares"
      (let [row (get (inventory) "com.blockether/imaging")]
        (expect (= "MIT" (:license row)))
        (when-let [declared (cached-pom-license "com.blockether/imaging" (:version row))]
          (expect (re-find #"(?i)\bmit\b" declared)
                  (str "the cached POM declares " declared ", not MIT")))))
  (it "agrees with every cached POM, so no in-house row drifts from the artifact"
      (let [rows (inventory)]
        (doseq
          [[coord version] (in-house-coords)
           :let [declared (cached-pom-license coord version)]
           :when declared]

          (let [stated (:license (get rows coord))]
            (expect (= (normalize declared) stated)
                    (str coord " states " stated " but its POM declares " declared)))))))

(defdescribe
  audit-generated-date-test
  ;; `.github/workflows/audit-md.yml` regenerates this document and fails on ANY
  ;; diff. A wall-clock stamp therefore reddened the pipeline on every dependency
  ;; bump made on a later day than the last regeneration, with the date line as
  ;; the only diff. `scripts/gen-audit.bb` renders a placeholder and `stamp-date`
  ;; keeps the committed date while the rest of the document is byte-identical.
  (it "the generator resolves its date against the committed doc, not the clock"
      (let [src (slurp (io/file "scripts" "gen-audit.bb"))]
        (expect (str/includes? src "> Generated \" today \"."))
        (expect (re-find #"today\s+date-placeholder\]" src))
        (expect (str/includes? src "(defn- stamp-date"))
        ;; the ONLY clock read left feeds stamp-date's "content changed" branch
        (expect (= 1 (count (re-seq #"LocalDate/now" src))))
        (expect (str/includes? (second (str/split src #"\(defn- stamp-date")) "LocalDate/now"))))
  (it "the committed document states one resolved ISO date"
      (expect (= 1 (count (re-seq #"(?m)^> Generated \d{4}-\d{2}-\d{2}\.$" (slurp audit-file)))))))
