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

   This test pins both registrations in the metadata that ships inside the
   image. Deleting one re-breaks the binary while every JVM test stays green."
  (:require [charred.api :as charred]
            [clojure.java.io :as io]
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
                 (expect (= {"n" 9007199254740991} (yamlstar/load "n: 9007199254740991")))))
