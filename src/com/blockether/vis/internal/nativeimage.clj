(ns com.blockether.vis.internal.nativeimage
  "GraalVM native-image build-time Feature: initialize Clojure namespaces the
   *right* way so build-time class initialization doesn't blow up.

   The problem: `graal-build-time` registers every Clojure-generated package for
   build-time class initialization. native-image then runs each `<clinit>` RAW on
   a parallel analysis worker thread, with no Clojure thread-binding frame. Any
   namespace whose body has a top-level `(set! *warn-on-reflection* true)` (most
   libraries — babashka.fs, next.jdbc, rewrite-clj, honeysql, nippy, …) throws

       java.lang.IllegalStateException: Can't change/establish root binding of:
       *warn-on-reflection* with set

   because `set!` on a dynamic var requires a thread binding. Core namespaces
   (clojure.string, clojure.spec.alpha, …) survive only because clojure.core's
   bootstrap loads them through `require`, which DOES push that binding. Libraries
   reached directly by the analysis get no such courtesy.

   The fix: in `beforeAnalysis` — which runs in the image-builder JVM, before the
   analysis can raw-init anything — `require` every app + extension namespace with
   the compiler vars bound. `require` initializes each class through Clojure's
   loader (binding active), so its `set!` succeeds; by the time the analysis marks
   the class build-time-initialized it is already initialized and is not re-run.

   Wired via `--features=com.blockether.vis.internal.nativeimage` in main's
   `resources/META-INF/native-image/com.blockether/vis/native-image.properties`,
   alongside graal-build-time's feature. Build-time only — never loaded at runtime."
  (:gen-class :implements [org.graalvm.nativeimage.hosted.Feature])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private preload-resource "META-INF/vis-native-image/preload.edn")

(defn- preload-namespaces
  "The namespaces build.clj found to have a top-level (set! *warn-on-reflection* …);
   it writes them to `preload-resource` on the image classpath."
  []
  (if-let [url (io/resource preload-resource)]
    (edn/read-string (slurp url))
    (do (println "[vis/native-image] WARNING: no" preload-resource "on classpath") [])))

;; gen-class generates a throwing stub for EVERY interface method we don't define
;; (it does not inherit the interface's `default` bodies), and native-image calls
;; many of them — so we implement the whole Feature lifecycle. Only beforeAnalysis
;; does work; the rest are no-ops.

(defn -getURL [_] "https://github.com/blockether/vis")
(defn -getDescription [_] "vis: require app + extension namespaces with *warn-on-reflection* bound")
(defn -isInConfiguration [_ _] true)
(defn -getRequiredFeatures [_] [])

(defn -beforeAnalysis
  [_ _]
  ;; Require every offending namespace WITH the compiler vars bound, so its
  ;; class is initialized through Clojure's loader (which pushes the binding) and
  ;; its top-level (set! *warn-on-reflection* …) succeeds — before the analysis
  ;; can raw-init it on a binding-less worker thread.
  (binding [*warn-on-reflection*
            false

            *unchecked-math*
            false]

    (let [nses (preload-namespaces)]
      (println "[vis/native-image] pre-initializing" (count nses) "namespaces via require…")
      (doseq [ns-str nses]
        (try (require (symbol ns-str))
             (catch Throwable t
               ;; a require that can't resolve is harmless here; native-image will
               ;; surface a real reachability problem on its own if one exists.
               (println "[vis/native-image]   skipped" ns-str "-" (.getMessage t)))))
      (println "[vis/native-image] namespace pre-initialization done"))))

;; remaining lifecycle hooks: no-ops (must exist so gen-class doesn't stub-throw)
;; onRegistration(OnRegistrationAccess) arrived in GraalVM 25.1 — without the
;; no-op the whole build aborts with "onRegistration ... not defined?".
(defn -onRegistration [_ _])
(defn -afterRegistration [_ _])
(defn -duringSetup [_ _])
(defn -duringAnalysis [_ _])
(defn -afterAnalysis [_ _])
(defn -onAnalysisExit [_ _])
(defn -beforeUniverseBuilding [_ _])
(defn -beforeCompilation [_ _])
(defn -afterCompilation [_ _])
(defn -beforeHeapLayout [_ _])
(defn -afterHeapLayout [_ _])
(defn -beforeImageWrite [_ _])
(defn -afterImageWrite [_ _])
(defn -cleanup [_])
