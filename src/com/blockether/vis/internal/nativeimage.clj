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
   analysis can raw-init anything — `require` those namespaces with the compiler
   vars bound. `require` initializes each class through Clojure's loader (binding
   active), so its `set!` succeeds; by the time the analysis marks the class
   build-time-initialized it is already initialized and is not re-run.

   The preload list is NARROW on purpose (build.clj `write-preload-namespaces!`):
   the reflection-setting namespaces plus the EXTENSION entry namespaces, never
   the whole source tree. Loading unreachable code in the builder JVM runs its
   load-time side effects and persists whatever they build into the image heap —
   a live jdk.internal.net.http.HttpClientFacade that way aborted the analysis.

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

(defn- cause-chain
  "Every throwable in `t`'s cause chain rendered `Class: message`, outermost
   first. An `ExceptionInInitializerError` carries NO message of its own, so
   printing only the outer one — as this Feature used to — reports a build-time
   class-initialization failure as the word `nil` and loses the reason entirely."
  [^Throwable t]
  (->> (iterate (fn [^Throwable c]
                  (.getCause c))
                t)
       (take-while some?)
       (take 8)
       (map (fn [^Throwable c]
              (str (.getName (class c)) ": " (.getMessage c))))))

(defn- absent-namespace?
  "True when the preload entry simply is not on the IMAGE classpath. The list is
   scanned off source dirs and dependency jars, so it names build-only libraries
   (clojure.tools.deps and friends) the image never carries — harmless."
  [^Throwable t]
  (boolean (and (instance? java.io.FileNotFoundException t)
                (some-> (.getMessage t)
                        (.startsWith "Could not locate")))))

(defn- own-namespace?
  "Vis' own code, as opposed to a third-party library on the preload list."
  [ns-str]
  (.startsWith ^String ns-str "com.blockether."))

;; gen-class generates a throwing stub for EVERY interface method we don't define
;; (it does not inherit the interface's `default` bodies), and native-image calls
;; many of them — so we implement the whole Feature lifecycle. Only beforeAnalysis
;; does work; the rest are no-ops.

(defn -getURL [_] "https://github.com/blockether/vis")

(defn -getDescription
  [_]
  "vis-agent: require app + extension namespaces with *warn-on-reflection* bound")

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

    (let [nses
          (preload-namespaces)

          broken
          (atom [])]

      (println "[vis/native-image] pre-initializing" (count nses) "namespaces via require…")
      (doseq [ns-str nses]
        (try (require (symbol ns-str))
             (catch Throwable t
               ;; A build-only library that never reaches the image classpath is
               ;; harmless. Anything ELSE is a namespace this image cannot serve:
               ;; the first failure poisons its `__init` class, every later
               ;; namespace that touches it dies with "Could not initialize
               ;; class", and the binary reports that only when a user runs it.
               (if (absent-namespace? t)
                 (println "[vis/native-image]   skipped" ns-str "-" (.getMessage t))
                 (do (println "[vis/native-image]   FAILED" ns-str)
                     (doseq [line (cause-chain t)]
                       (println "[vis/native-image]     " line))
                     (when (own-namespace? ns-str) (swap! broken conj ns-str)))))))
      (println "[vis/native-image] namespace pre-initialization done")
      ;; Vis' OWN namespace failing to initialize is a DEAD BINARY, not a
      ;; warning: v0.1.39 shipped that way — every command died with
      ;; "Could not locate com/blockether/vis/core__init.class" while the build
      ;; was green, because the builder knew at minute one and said `nil`.
      (when (seq @broken)
        (throw (ex-info (str "native image is unusable: " (count @broken)
                             " Vis namespace(s) failed build-time initialization — " (first
                                                                                       @broken))
                        {:namespaces @broken}))))))

;; remaining lifecycle hooks: no-ops (must exist so gen-class doesn't stub-throw)
;; onRegistration(OnRegistrationAccess) arrived in GraalVM 25.1 — without the
;; no-op the whole build aborts with "onRegistration ... not defined?".
(defn -onRegistration [_ _])

(defn -afterRegistration [_ _])

(defn -duringSetup
  [_ _]
  ;; FFM downcall stubs for lanterna's native TTY control
  ;; (com.googlecode.lanterna.terminal.ansi.TTYDeviceControl, java.lang.foreign):
  ;; termios + ioctl(TIOCGWINSZ) instead of forking
  ;; /bin/stty. A native image can only make a downcall whose FunctionDescriptor
  ;; was registered at BUILD time — without this the binary raises
  ;; MissingForeignRegistrationError, which lanterna catches and silently
  ;; degrades back to stty. Registering here keeps the fast path in the binary.
  (try
    (let [layouts
          (fn ^"[Ljava.lang.foreign.MemoryLayout;" [ls]
            (into-array java.lang.foreign.MemoryLayout ls))

          descriptor
          (fn [args]
            (java.lang.foreign.FunctionDescriptor/of java.lang.foreign.ValueLayout/JAVA_INT
                                                     (layouts args)))

          ;; REFLECTIVE on purpose. `RuntimeForeignAccess` is @Platforms(HOSTED_ONLY):
          ;; it exists in the image BUILDER, never in the image. graal-build-time
          ;; initializes every Clojure namespace at build time, so this Feature's
          ;; Vars land in the image heap and the analysis PARSES this fn as
          ;; application code — a static reference then aborts the build with
          ;; "Type is not available in this platform:
          ;; org.graalvm.nativeimage.hosted.RuntimeForeignAccess". Looking the
          ;; class up by name keeps the build-time call working and leaves the
          ;; parsed method free of the hosted type.
          register!
          (fn [desc & options]
            (let [k
                  (Class/forName "org.graalvm.nativeimage.hosted.RuntimeForeignAccess")

                  m
                  (->> (.getMethods k)
                       (filter (fn [^java.lang.reflect.Method mm]
                                 (and (= "registerForDowncall" (.getName mm))
                                      (= 2 (alength (.getParameterTypes mm))))))
                       first)

                  _
                  (when-not m
                    (throw (ex-info (str "no registerForDowncall/2 on " k
                                         " - had " (mapv str (.getMethods k)))
                                    {})))

                  opt-t
                  (.getComponentType ^Class
                                     (aget (.getParameterTypes ^java.lang.reflect.Method m) 1))]

              (.invoke ^java.lang.reflect.Method m
                       nil
                       (into-array Object [desc (into-array opt-t options)]))))]

      ;; NEVER on Linux, where the TTY is driven by forking /bin/stty exactly as it
      ;; was through v0.1.32: this registration has never been proven there, and
      ;; lanterna's `TTYDeviceControl` <clinit> catches the resulting
      ;; MissingForeignRegistrationError and leaves SUPPORTED false. macOS keeps the
      ;; termios/ioctl fast path.
      ;;
      ;; The SIGSEGV that killed v0.1.33-v0.1.35 inside the generated downcall stub
      ;; was NOT caused by registering here — skipping it did not stop the crash
      ;; (measured again on the 2026-08-13 dry run). The class was INITIALIZED AT
      ;; BUILD TIME, so the image inherited the builder JVM's MethodHandles and
      ;; SUPPORTED=true with no stubs behind them; `build.clj` now initializes
      ;; `TTYDeviceControl` at run time, which is what makes either outcome safe.
      (if (.contains (.toLowerCase (str (System/getProperty "os.name" ""))) "linux")
        (println "[vis/native-image] FFM downcall registration SKIPPED on Linux -"
                 "the TTY is driven by forking /bin/stty (see the comment above)")
        (do
          ;; open(const char*, int) / close(int)
          (register! (descriptor [java.lang.foreign.ValueLayout/ADDRESS
                                  java.lang.foreign.ValueLayout/JAVA_INT]))
          (register! (descriptor [java.lang.foreign.ValueLayout/JAVA_INT]))
          ;; tcgetattr(int, struct termios*) / tcsetattr(int, int, const struct termios*)
          (register! (descriptor [java.lang.foreign.ValueLayout/JAVA_INT
                                  java.lang.foreign.ValueLayout/ADDRESS]))
          (register! (descriptor [java.lang.foreign.ValueLayout/JAVA_INT
                                  java.lang.foreign.ValueLayout/JAVA_INT
                                  java.lang.foreign.ValueLayout/ADDRESS]))
          ;; ioctl(int, unsigned long, ...) — variadic from argument index 2, which is
          ;; how the winsize pointer must be passed on Apple silicon.
          (register! (descriptor [java.lang.foreign.ValueLayout/JAVA_INT
                                  java.lang.foreign.ValueLayout/JAVA_LONG
                                  java.lang.foreign.ValueLayout/ADDRESS])
                     (java.lang.foreign.Linker$Option/firstVariadicArg 2))
          (println "[vis/native-image] registered 5 FFM downcalls for native TTY control"))))
    (catch Throwable t
      ;; Older builder without RuntimeForeignAccess, or FFM support switched off:
      ;; the TUI still works, it just forks stty like it always did.
      (println "[vis/native-image] FFM downcall registration skipped -" (.getMessage t)))))

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
