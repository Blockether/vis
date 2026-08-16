(ns com.blockether.vis.internal.clojure-contract-test
  "The Clojure half of the extension contract: `com.blockether.vis.core` is the only
   namespace an extension should require, and `clojure-host.edn` freezes the places
   that already do not obey. Nothing but this suite keeps that promise honest."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.clojure-contract :as contract]
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

(defdescribe
  clojure-contract-test
  (it "reads a valid contract naming the facade and the extension roots"
      (expect (= 'com.blockether.vis.core (contract/facade)))
      (expect (pos? (contract/version)))
      (expect (seq (contract/extension-roots)))
      (expect (every? #(.isDirectory (io/file %)) (contract/extension-roots))))
  (it "names only internal namespaces that still exist"
      (let
        [{:debt/keys [production test]}
         (contract/internal-debt)

         missing
         (remove #(io/resource (ns->resource %)) (concat production test))]

        (expect (empty? missing)
                (str "the contract freezes internal namespaces that are gone: "
                     (str/join ", " missing)
                     " — delete the entry from resources/vis-contract/clojure-host.edn"))))
  (it
    "fails when an extension reaches past the facade into a namespace the contract does not freeze"
    (when-let [root (repo-root)]
      (let
        [found (contract/internal-requires root)
         debt (contract/internal-debt)
         new-coupling
         (for
           [scope [:debt/production :debt/test]
            [nsname paths] (sort-by key (get found scope))
            :when (not (contains? (get debt scope) nsname))]

           (str nsname " (" (name scope) ") required by " (str/join ", " (sort paths))))]

        (expect (empty? new-coupling)
                (str "extensions now require internal namespaces the host contract does not"
                     " allow:\n" (str/join "\n" new-coupling)
                     "\nExport what the extension needs from " (contract/facade)
                     ", or — only if it is host-level behavior — add the namespace to"
                     " resources/vis-contract/clojure-host.edn with a reason in review.")))))
  (it "fails when frozen coupling is stale, so the debt list can only shrink"
      (when-let [root (repo-root)]
        (let
          [found (contract/internal-requires root)
           debt (contract/internal-debt)
           stale (for
                   [scope [:debt/production :debt/test]
                    nsname (sort (get debt scope))
                    :when (not (contains? (get found scope) nsname))]

                   (str nsname " (" (name scope) ")"))]

          (expect (empty? stale)
                  (str "the contract still freezes coupling nothing uses:\n" (str/join "\n" stale)
                       "\nDelete these from resources/vis-contract/clojure-host.edn — the"
                       " debt list is only allowed to shrink.")))))
  ;; The gate is only worth having if the scanner really sees a libspec, so one
  ;; known require is pinned to the file that writes it.
  (it "attributes a require it finds to the extension file that wrote it"
      (when-let [root (repo-root)]
        (let
          [paths (get-in (contract/internal-requires root)
                         [:debt/production 'com.blockether.vis.internal.theme])]
          (expect (seq paths))
          (expect (some #(str/includes? % "vis-channel-tui") paths)
                  (str "the TUI channel requires internal.theme; the scanner saw " paths))))))
