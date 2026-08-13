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
   `java.math.BigInteger(java.lang.String)` — unregistered, so `vis doctor`,
   `vis sessions list`, `vis providers status` and one-shot prompts aborted for
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
            [com.blockether.vis.internal.extension :as extension]
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


(defn- build-clj-builtin-nses
  "The `builtin-extension-nses` vector `build.clj` writes into the image's
   build-time preload list, read from the source it actually ships."
  []
  (let
    [src
     (slurp (io/file "build.clj"))

     at
     (str/index-of src "(def ^:private builtin-extension-nses")]

    (set (edn/read-string (subs src (str/index-of src "[" at))))))

;; Regression: `foundation.introspection` reached extension/builtin-extension-nses
;; and never reached build.clj's copy of it, so the FRESHLY BUILT binary died on
;; its first line — "Could not locate
;; com/blockether/vis/internal/foundation/introspection__init.class on classpath"
;; — before it could paint anything. A runtime `require` in a native image can
;; only find a namespace the build-time preload list already initialized, and
;; `load-builtin-extensions!` requires every name in that vector, so a name in one
;; list and not the other is a binary that cannot start.
(defdescribe builtin-extension-nses-reach-the-native-image-test
             (it "keeps build.clj's preload copy identical to the list vis requires"
                 (let
                   [shipped
                    (set (map str @#'extension/builtin-extension-nses))

                    preloaded
                    (build-clj-builtin-nses)]

                   (expect (= shipped preloaded)
                           (str "build.clj's builtin-extension-nses drifted. Missing from the "
                                "image (the binary dies at startup): "
                                (pr-str (sort (remove preloaded shipped)))
                                " — listed there but not shipped: "
                                (pr-str (sort (remove shipped preloaded))))))))


(defn- extension-manifest-files
  "Every shipped extension discovery manifest under `extensions/`."
  []
  (->> (file-seq (io/file "extensions"))
       (filter (fn [^java.io.File f]
                 (and (.isFile f) (str/ends-with? (str f) "META-INF/vis-extension/vis.edn"))))))

(defn- declared-image-nses
  "Every namespace an extension manifest declares — `:nses` (required at
   discovery) plus `:image-nses` (build-time initialized only). Both end up in
   the image's preload list; only the first is required at startup."
  []
  (set (for
         [f
          (extension-manifest-files)

          [_ entry]
          (edn/read-string (slurp f))

          ns
          (concat (:nses entry) (:image-nses entry))]

         (str ns))))

(defn- nses-loaded-by-name
  "Extension namespaces vis resolves BY NAME at run time: every
   `(requiring-resolve 'com.blockether.vis.ext.…/…)` in shipped extension source,
   and every backend registrar's `:persistance/ns`."
  []
  (->> (file-seq (io/file "extensions"))
       (filter (fn [^java.io.File f]
                 (and (.isFile f) (str/ends-with? (str f) ".clj") (str/includes? (str f) "/src/"))))
       (mapcat
         (fn [f]
           (let [src (slurp f)]
             (concat
               (map second
                    (re-seq
                      #"requiring-resolve\s+'(com\.blockether\.vis\.ext\.[A-Za-z0-9.*+!_?<>=-]+)/"
                      src))
               (map second
                    (re-seq
                      #":persistance/ns\s+'(com\.blockether\.vis\.ext\.[A-Za-z0-9.*+!_?<>=-]+)"
                      src))))))
       set))

;; Regression: the FRESHLY BUILT v0.1.35+ binary aborted on `vis` (the TUI) with
;; "Could not locate com/blockether/vis/ext/channel_tui/screen__init.class" and on
;; any DB command with "Backend :sqlite … failed to load". Both namespaces are
;; deliberately kept OUT of `:nses` — discovery must not pay for Lanterna or JDBC —
;; and are reached with `requiring-resolve` on first use. A JVM loads them then;
;; a native image CANNOT ("Classes cannot be defined at runtime"), so a namespace
;; the image never build-time initialized is simply not there. Every JVM test
;; stayed green while the binary could not start its own terminal UI.
(defdescribe nses-loaded-by-name-reach-the-native-image-test
             (it "declares every extension namespace vis resolves by name at run time"
                 (let
                   [declared
                    (declared-image-nses)

                    missing
                    (sort (remove declared (nses-loaded-by-name)))]

                   (expect (empty? missing)
                           (str "these namespaces are loaded by NAME at run time but no extension "
                                "manifest declares them, so the native image does not contain them "
                                "(the binary dies on first use): "
                                (pr-str missing)
                                " — add each to its manifest's :image-nses"))))
             (it "keeps build.clj reading BOTH manifest keys into the preload list"
                 (let [src (slurp (io/file "build.clj"))]
                   (expect (str/includes? src ":image-nses")
                           "build.clj must fold :image-nses into the native-image preload list")
                   (expect (str/includes? src ":nses")))))


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
