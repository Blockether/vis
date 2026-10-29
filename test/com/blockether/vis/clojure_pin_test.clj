(ns com.blockether.vis.clojure-pin-test
  "The Clojure version is pinned in the root deps.edn, and every module agrees.

   The source runtime is `clojure -M:vis`, so the JVM's Clojure comes from a
   dependency graph rooted in the USER's Clojure CLI install, whose own deps.edn
   declares `org.clojure/clojure` as a top-level dep. A top dep wins in
   tools.deps, so without an explicit pin here an older CLI silently downgraded
   the engine below 1.12 and nothing compiled. Unit-testable because it is pure
   EDN, and cheap precisely where the failure is otherwise unreachable: it only
   ever appears on somebody else's machine."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- clojure-pin
  "`org.clojure/clojure`'s :mvn/version in the deps.edn at `path`, or nil."
  [path]
  (get-in (edn/read-string (slurp (io/file path))) [:deps 'org.clojure/clojure :mvn/version]))

(defn- deps-files
  "Every deps.edn of this repository's own modules, root first."
  []
  (into ["deps.edn"]
        (sort (into []
                    (comp (filter #(= "deps.edn" (.getName ^java.io.File %)))
                          (map #(.getPath ^java.io.File %))
                          ;; Scenario fixtures under e2e are throwaway projects, not modules.
                          (remove #(str/includes? % "/e2e/")))
                    (mapcat #(file-seq (io/file %)) ["extensions" "packages"])))))

(defdescribe clojure-pin-test
             (it "pins the SAME version in every module that names Clojure"
                 (let [root-pin
                       (clojure-pin "deps.edn")

                       pins
                       (into {}
                             (keep (fn [path]
                                     (when-let [v (clojure-pin path)]
                                       [path v])))
                             (deps-files))]

                   (expect (seq (dissoc pins "deps.edn")))
                   (doseq [[path v] pins]
                     (expect (= root-pin v)
                             (str path " pins Clojure " v ", root pins " root-pin))))))
