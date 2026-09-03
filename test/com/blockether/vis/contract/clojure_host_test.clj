(ns com.blockether.vis.contract.clojure-host-test
  "The executable dependency boundary for Core, gateway, SDKs and first-party consumers.
   The contract freezes every present violation by source-file count, so refactors can
   only shrink the debt and a new edge or hand-written protocol literal fails at once."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.clojure-host :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- repo-root
  "The repository root when the suite runs from it, else nil (nothing to scan)."
  []
  (when (.exists (io/file "deps.edn")) "."))

(defn- ns->resource
  "The classpath file a namespace name loads from — dots become directories and a
   hyphen becomes an underscore, the way the compiler munges it."
  [nsname]
  (str (-> (str nsname)
           (str/replace "." "/")
           (str/replace "-" "_"))
       ".clj"))

(defn- debt-namespaces
  []
  (->> (contract/internal-debt)
       vals
       (mapcat keys)
       set))

(defdescribe
  clojure-host-contract-test
  (it "reads the versioned graph contract and names every migration input"
      (expect (= 'com.blockether.vis.core (contract/facade)))
      (expect (= 2 (contract/version)))
      (expect (seq (contract/extension-roots)))
      (expect (seq (contract/javascript-wire-files)))
      (expect (every? #(.isDirectory (io/file %)) (contract/extension-roots)))
      (expect (every? #(.isFile (io/file %)) (contract/javascript-wire-files))))
  (it "names only debt dependencies that still exist"
      (let [missing (remove #(io/resource (ns->resource %)) (debt-namespaces))]
        (expect (empty? missing)
                (str "the contract freezes Clojure dependencies that are gone: "
                     (str/join ", " missing)
                     " — shrink packages/vis-contract/resources/vis-contract/clojure-host.edn"))))
  (it "freezes the exact forbidden Clojure dependency counts"
      (when-let [root (repo-root)]
        (let [locations (contract/dependency-violations root)
              actual (contract/dependency-debt root)
              expected (contract/internal-debt)]

          (expect
            (= expected actual)
            (str
              "the SDK-only Clojure graph changed. New debt is forbidden and removed debt must be"
              " deleted from clojure-host.edn.\nexpected " (pr-str expected)
              "\nactual " (pr-str actual)
              "\nlocations " (pr-str locations))))))
  (it "freezes JavaScript wire literals and rejects direct JavaScript/Python contract imports"
      (when-let [root (repo-root)]
        (let [locations (contract/consumer-wire-violations root)
              actual (contract/wire-debt-counts root)
              expected (contract/wire-debt)]

          (expect
            (= expected actual)
            (str "consumer wire debt changed. SDK migrations may shrink it; consumers may not add"
                 " literals or import vis-contract directly.\nexpected " (pr-str expected)
                 "\nactual " (pr-str actual)
                 "\nlocations " (pr-str locations))))))
  (it "attributes an extension edge to the source file that writes it"
      (when-let [root (repo-root)]
        (let [paths (get-in (contract/dependency-violations root)
                            [:debt/production 'com.blockether.vis.internal.workspace])]
          (expect (seq paths))
          (expect (some #(str/includes? % "vis-foundation-search") paths)
                  (str "the search extension requires internal.workspace; the scanner saw "
                       paths)))))
  (it "applies the final layer rule before namespaces are moved"
      (when-let [root (repo-root)]
        (let [paths (get-in (contract/dependency-violations root)
                            [:debt/layers 'com.blockether.vis.internal.gateway.state])]
          (expect (= #{"src/com/blockether/vis/internal/gateway/server.clj"} paths)))))
  (it "attributes one canonical route to every protocol module that spells it"
      (when-let [root (repo-root)]
        (let [paths (get-in (contract/consumer-wire-violations root)
                            [:debt/javascript [:route "/healthz"]])]
          (expect (= #{"apps/vis-companion/src/lib/compat.ts"
                       "apps/vis-companion/src/lib/gateway.ts"
                       "apps/vis-companion/src/lib/types.ts"}
                     paths))))))
