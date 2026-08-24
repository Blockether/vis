(ns com.blockether.vis.native-reachability-test
  "Reflective members the native image would otherwise refuse at RUN time.

   `graal-build-time` covers Clojure's own generated classes, but it cannot see a
   constructor that Clojure resolves through `clojure.lang.Reflector` — an
   interop form whose argument types are not known at compile time. In a JVM run
   those calls just work, so nothing in the test suite or the native BUILD fails;
   the image dies later, in a user's terminal, with
   `Cannot reflectively invoke constructor ...`.

   MEASURED regression (v0.1.25 binary): every YAML document containing a plain
   integer went through `yamlstar.numbers/parse-safe-integer`, which calls
   `(bigint value)` on the raw STRING. `clojure.core/bigint` ends in
   `(BigInteger. x)` with an untyped `x`, i.e. a reflective
   `java.math.BigInteger(java.lang.String)` — unregistered, so `vis-agent doctor`,
   `vis-agent sessions list`, `vis-agent providers status` and one-shot prompts aborted for
   every workspace. `clojure.tools.reader` (SCI/edamame) reaches the same
   constructor with a radix, `(BigInteger. n (int radix))`, for every integer
   literal it reads.

   MEASURED regression (v0.1.24 binary): a `~/.vis/config.yml` — any YAML at
   all — aborted every command with `Cannot reflectively invoke
   java.lang.Character.codePointAt(java.lang.CharSequence, int)`. yamlstar's
   scanner walks the document through that untyped interop form, so READING a
   config file at startup is what needed it.

   This test pins those registrations in the metadata that ships inside the
   image. Deleting one re-breaks the binary while every JVM test stays green."
  (:require [charred.api :as charred]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.nativeimage :as nativeimage]
            [lazytest.core :refer [defdescribe expect it]]
            [yamlstar.core :as yamlstar]))

(def ^:private metadata-file
  (io/file "resources" "META-INF"
           "native-image" "com.blockether"
           "vis" "reachability-metadata.json"))

(defn- reflection-entries
  "The `reflection` section of vis's own reachability metadata."
  []
  (get (charred/read-json (slurp metadata-file)) "reflection"))

(defn- registered-constructor?
  "True when `type` registers `<init>` with exactly `parameter-types`."
  [entries type parameter-types]
  (boolean (some (fn [entry]
                   (and (= type (get entry "type"))
                        (some (fn [m]
                                (and (= "<init>" (get m "name"))
                                     (= parameter-types (vec (get m "parameterTypes")))))
                              (get entry "methods"))))
                 entries)))

(defn- registered-method?
  "True when `type` registers `name` with exactly `parameter-types`."
  [entries type name parameter-types]
  (boolean (some (fn [entry]
                   (and (= type (get entry "type"))
                        (some (fn [m]
                                (and (= name (get m "name"))
                                     (= parameter-types (vec (get m "parameterTypes")))))
                              (get entry "methods"))))
                 entries)))

(defn- registered-type?
  "True when `type` is registered at all, so `Class/forName` can resolve it."
  [entries type]
  (boolean (some (fn [entry]
                   (= type (get entry "type")))
                 entries)))
(defdescribe native-reachability-test
             (it "ships reachability metadata that parses"
                 (expect (.isFile metadata-file))
                 (expect (vector? (reflection-entries))))
             (it "registers BigInteger(String) — every YAML integer needs it"
                 (expect (registered-constructor? (reflection-entries)
                                                  "java.math.BigInteger"
                                                  ["java.lang.String"])))
             (it "registers BigInteger(String,int) — every tools.reader integer literal needs it"
                 (expect (registered-constructor? (reflection-entries)
                                                  "java.math.BigInteger"
                                                  ["java.lang.String" "int"])))
             (it "keeps the reflective YAML integer path this registration exists for"
                 ;; yamlstar hands the raw scalar text to clojure.core/bigint, so loading a
                 ;; plain integer is exactly the call the native image must be able to make.
                 (expect (= {"n" 3} (yamlstar/load "n: 3")))
                 (expect (= {"n" 9007199254740991} (yamlstar/load "n: 9007199254740991"))))
             (it "registers Character/codePointAt(CharSequence,int) — every YAML document needs it"
                 (expect (registered-method? (reflection-entries)
                                             "java.lang.Character"
                                             "codePointAt"
                                             ["java.lang.CharSequence" "int"])))
             (it "keeps the reflective YAML scan path this registration exists for"
                 ;; The scanner reaches codePointAt for every character it reads, so a
                 ;; nested config document is exactly the call the image must be able to make.
                 (expect (= {"gateway" {"host" "0.0.0.0" "port" 7890}}
                            (yamlstar/load "gateway:\n  host: 0.0.0.0\n  port: 7890\n")))))


;; MEASURED regression (this working tree, before the registration below): the gateway
;; served every request on `ring.adapter.jetty9`, whose Jetty handler is a `:gen-class`
;; with gen-class's default `:load-impl-ns true`. Constructing it therefore runs
;; `(clojure.lang.RT/load "ring/adapter/jetty9/handlers/sync")`, which resolves the
;; namespace's AOT `__init` class through `Class/forName` — a name assembled at run
;; time, invisible to the analysis, so the class was left out of the image. Every JVM
;; test stayed green and the native BUILD stayed green; the binary died on `vis-agent
;; gateway start` with "Could not locate ring/adapter/jetty9/handlers/sync__init.class
;; ... on classpath" before it could bind a port.
(defdescribe
  the-http-adapters-gen-class-namespace-reaches-the-native-image-test
  (it "registers the handler's __init class, the only thing RT/load can resolve"
      (expect (registered-type? (reflection-entries) "ring.adapter.jetty9.handlers.sync__init")))
  (it "keeps the gateway on the adapter this registration exists for"
      ;; A different adapter makes the registration dead weight; the point of
      ;; the pair is that one of them fails when the two drift apart.
      (expect (str/includes?
                (slurp (io/file "src" "com" "blockether" "vis" "internal" "gateway" "server.clj"))
                "[ring.adapter.jetty9 :as jetty]"))))


(defn- with-os-name
  "Run `f` with the build host's reported `os.name`, restoring the real one."
  [os f]
  (let [original (System/getProperty "os.name")]
    (try (System/setProperty "os.name" os) (f) (finally (System/setProperty "os.name" original)))))

;; Regression: v0.1.32 was the last native release whose Linux binaries shipped.
;; v0.1.33 added the FFM downcall registration below, and v0.1.33/34/35 all died in
;; `native-binary-paints-the-tui-test` with a SIGSEGV inside the generated stub for
;; lanterna's `open("/dev/tty", …)` — on x64 AND arm64, every attempt. Registering the
;; descriptor is what lets `TTYDeviceControl`'s <clinit> succeed in the image; skipping
;; it leaves that <clinit>'s own `catch Throwable` to mark the native TTY unsupported,
;; and the terminal forks /bin/stty exactly as it did through v0.1.32.
(defdescribe
  native-tty-downcalls-are-not-registered-on-linux-test
  (it "skips the FFM registration when the image is built on Linux"
      (let [out (with-out-str (with-os-name "Linux" #(nativeimage/-duringSetup nil nil)))]
        (expect (re-find #"SKIPPED on Linux" out))
        (expect (nil? (re-find #"registered 5 FFM downcalls" out)))))
  (it "still takes the fast path on the platform it was proven on"
      ;; The builder class is HOSTED-ONLY, so off Linux this reaches the
      ;; registration and fails on `RuntimeForeignAccess` being absent from a
      ;; plain JVM — which is precisely NOT the Linux skip.
      (let [out (with-out-str (with-os-name "Mac OS X" #(nativeimage/-duringSetup nil nil)))]
        (expect (nil? (re-find #"SKIPPED on Linux" out))))))


(def ^:private manifest-file (io/file "resources" "META-INF" "vis" "manifest.edn"))

(defn- manifest-initialization-nses
  "Namespaces derived from the ordered initializer symbols in the closed manifest."
  []
  (->> (:initialization (edn/read-string (slurp manifest-file)))
       (map (comp symbol namespace))
       set))

(defn- first-party-source-dirs
  "Clojure source roots the build derives from root paths and local dependencies."
  []
  (let [deps
        (edn/read-string (slurp (io/file "deps.edn")))

        local-roots
        (keep :local/root (vals (:deps deps)))]

    (->> (concat (:paths deps) (map #(str % "/src") local-roots))
         (map io/file)
         (filter #(.isDirectory ^java.io.File %)))))

(defn- source-ns
  [file]
  (try (with-open [reader (java.io.PushbackReader. (io/reader file))]
         (binding [*read-eval* false]
           (let [form (read {:read-cond :allow :eof nil} reader)]
             (when (and (seq? form) (= 'ns (first form))) (first (filter symbol? (rest form)))))))
       (catch Throwable _ nil)))

(defn- first-party-source-nses
  "Every namespace in the first-party source closure consumed by the native build."
  []
  (->> (first-party-source-dirs)
       (mapcat file-seq)
       (filter (fn [^java.io.File file]
                 (and (.isFile file) (re-matches #".*\.cljc?$" (.getName file)))))
       (keep source-ns)
       set))

(defn- nses-loaded-by-name
  "Extension namespaces Vis resolves by name at runtime."
  []
  (->> (file-seq (io/file "extensions"))
       (filter (fn [^java.io.File file]
                 (and (.isFile file)
                      (str/ends-with? (str file) ".clj")
                      (str/includes? (str file) "/src/"))))
       (mapcat
         (fn [file]
           (let [src (slurp file)]
             (concat
               (map second
                    (re-seq
                      #"requiring-resolve\s+'(com\.blockether\.vis\.ext\.[A-Za-z0-9.*+!_?<>=-]+)/"
                      src))
               (map second
                    (re-seq
                      #":persistance/ns\s+'(com\.blockether\.vis\.ext\.[A-Za-z0-9.*+!_?<>=-]+)"
                      src))))))
       (map symbol)
       set))

;; Regression: native binaries cannot define a namespace when a lazy handler is
;; first selected. The build keeps JVM initialization cheap while treating the
;; closed first-party source set and manifest initializer namespaces as reachable.
(defdescribe closed-manifest-namespaces-reach-the-native-image-test
             (it "derives native initializer roots from the one distribution manifest"
                 (let [initializers
                       (manifest-initialization-nses)

                       sources
                       (first-party-source-nses)

                       build-src
                       (slurp (io/file "build.clj"))]

                   (expect (seq initializers))
                   (expect (empty? (remove sources initializers))
                           "every initializer namespace must be in first-party source")
                   (expect (str/includes? build-src
                                          "(manifest-initialization-namespaces class-dir)")
                           "the native preload must derive initializer roots from manifest.edn")))
             (it "preloads the first-party source closure for implementations resolved by name"
                 (let [sources
                       (first-party-source-nses)

                       missing
                       (sort (remove sources (nses-loaded-by-name)))

                       build-src
                       (slurp (io/file "build.clj"))]

                   (expect (empty? missing)
                           (str "runtime-resolved namespaces missing from first-party source: "
                                (pr-str missing)))
                   (expect (re-find #"source\s+\(map\s+str\s+\(source-namespaces\)\)" build-src)
                           "the native preload must include the full first-party source closure"))))


;; Regression: the v0.1.33-v0.1.35 binaries — and the 2026-08-13 dry run — died in
;; `native-binary-paints-the-tui-test` with a SIGSEGV inside the generated downcall
;; stub, on x64 and arm64 alike, the first time the TUI opened /dev/tty. Lanterna's
;; `TTYDeviceControl` <clinit> builds its termios/ioctl MethodHandles; initialized
;; in the BUILDER JVM (where java.lang.foreign works) the image inherited
;; SUPPORTED=true and handles with no stubs behind them. It has to decide in the
;; BINARY instead.
(defdescribe
  tty-device-control-initializes-at-run-time-test
  (it "keeps lanterna's TTY control out of build-time initialization"
      (let [src (slurp (io/file "build.clj"))]
        (expect (str/includes?
                  src
                  "--initialize-at-run-time=com.googlecode.lanterna.terminal.ansi.TTYDeviceControl")
                (str "build.clj must initialize TTYDeviceControl at RUN time, or the "
                     "binary segfaults on its first TUI frame")))))


;; Regression, v0.1.39: the released binary was DEAD — every command aborted with
;; "Could not locate com/blockether/vis/core__init.class ... on classpath", and
;; the build that produced it was green. `all-source-roots` copied a hardcoded
;; ["src" "resources"] into the AOT class dir while the root deps.edn `:paths`
;; had grown a third entry, `packages/vis-agent/src`. Without it the image had no
;; `vis/__init__.py`, so `env-python` threw while `com.blockether.vis.core` was
;; initializing in the BUILDER, poisoning that class for the whole image.
(defdescribe
  aot-copies-every-root-classpath-root-test
  (it "reads the root's AOT source roots from deps.edn, never a hardcoded pair"
      (let [src (slurp (io/file "build.clj"))]
        (expect (str/includes? src "(vec (:paths deps))")
                (str "build.clj's all-source-roots must copy EVERY root :paths entry into "
                     "the image; a literal list drops the next one that is added"))
        (expect (not (str/includes? src "(into [\"src\" \"resources\"]"))
                "the hardcoded root pair is what shipped an image with no vis/__init__.py")))
  (it "keeps the distributable vis Python module on the classpath it is read from"
      (let [paths (:paths (edn/read-string (slurp (io/file "deps.edn"))))]
        (expect (some #{"packages/vis-agent/src"} paths)
                (str "the engine slurps `vis/__init__.py` off the classpath; its root "
                     "belongs in deps.edn :paths, which is also what the image copies"))
        (expect (some? (io/resource "vis/__init__.py"))
                "vis/__init__.py must resolve as a classpath resource"))))
