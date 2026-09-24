(ns com.blockether.vis.internal.decisions.registry-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.io File FileOutputStream]
           [java.nio.file Files]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- fixture!
  [revision & [extra identity]]
  (let [archive
        (File/createTempFile "vis-decision-upload-" ".zip")

        model-id
        (get identity "model" "laya-typed-decisions")

        required
        (remove #{"PROVENANCE.json" "LICENSE.txt"} (assets/inference-required model-id))

        contents
        (merge (into {}
                     (map (fn [name]
                            [name (.getBytes (str name revision) "UTF-8")])
                          required))
               extra)

        provenance
        (merge {"model" model-id
                "revision" revision
                "kind" "inference"
                "format" "onnx"
                "precision" "fp32"
                "license" "Apache-2.0"}
               identity
               {"files" (into {}
                              (map (fn [[name bytes]]
                                     [name
                                      {"bytes" (alength ^bytes bytes)
                                       "sha256" (util/sha256-hex bytes)}]))
                              contents)})]

    (with-open [zip (ZipOutputStream. (FileOutputStream. archive))]
      (doseq [[name bytes] (conj (vec contents)
                                 ["PROVENANCE.json" (.getBytes (wire/json-str provenance) "UTF-8")]
                                 ["LICENSE.txt" (.getBytes "Apache-2.0" "UTF-8")])]
        (.putNextEntry zip (ZipEntry. name))
        (.write zip ^bytes bytes)
        (.closeEntry zip)))
    archive))

(defn- digest [^File archive] (util/sha256-hex (Files/readAllBytes (.toPath archive))))

(defn- error-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest imported-versions-and-aliases-survive-restart-with-cas
  (let [root
        (io/file (System/getProperty "java.io.tmpdir") (str "vis-import-" (random-uuid)))

        first-zip
        (fixture! (apply str (repeat 64 "1")))

        second-zip
        (fixture! (apply str (repeat 64 "2")))

        validated
        (atom [])]

    (try (with-redefs [assets/models-root (constantly (str root))]
           (let [validate! (fn [_ dir]
                             (swap! validated conj (slurp (io/file dir "model.onnx"))))
                 first (registry/register! first-zip (digest first-zip) validate!)
                 duplicate (registry/register! first-zip (digest first-zip) validate!)
                 second (registry/register! second-zip (digest second-zip) validate!)
                 old (get first "model_ref")
                 new (get second "model_ref")]

             (is (= first duplicate))
             (is (= 2 (count @validated)))
             (is (not= old new))
             (is (= 2 (count (registry/versions))))
             (is (= old (get (registry/activate! "sales" old nil) "model_ref")))
             (is (= :decisions/alias-conflict
                    (:type (error-data #(registry/activate! "sales" new nil)))))
             (is (= old (get (registry/resolve-model "sales") :model-ref)))
             (is (= new (get (registry/activate! "sales" new old) "model_ref")))
             (is (= new (get (registry/get-alias "sales") "model_ref")))
             (is (= old (get (registry/resolve-model old) :model-ref)))
             (is (= new (get (registry/resolve-model "sales") :model-ref)))
             (is (= :decisions/alias-conflict
                    (:type (error-data #(registry/activate! "sales" old old)))))))
         (finally (files/delete-dir! root) (.delete first-zip) (.delete second-zip)))))

(deftest import-rejects-unsafe-and-incomplete-bundles-without-registering
  (let [root
        (io/file (System/getProperty "java.io.tmpdir") (str "vis-reject-" (random-uuid)))

        escape
        (fixture! (apply str (repeat 64 "1")) {"../outside.onnx" (.getBytes "outside" "UTF-8")})

        missing
        (fixture! (apply str (repeat 64 "2"))
                  {"model.safetensors" (.getBytes "training secret" "UTF-8")})]

    (try (with-redefs [assets/models-root (constantly (str root))]
           (is (= :decisions/archive-checksum
                  (:type (error-data #(registry/register! escape
                                                          (apply str (repeat 64 "0"))
                                                          (fn [& _]))))))
           (is (= :decisions/unsafe-path
                  (:type (error-data #(registry/register! escape
                                                          (digest escape)
                                                          (fn [& _]))))))
           (is (= :decisions/invalid-archive
                  (:type (error-data #(registry/register! missing
                                                          (digest missing)
                                                          (fn [& _]))))))
           (is (empty? (registry/versions)))
           (is (not (.exists (io/file root "aliases.json")))))
         (finally (files/delete-dir! root) (.delete escape) (.delete missing)))))

(deftest gliner-imports-preserve-identity-and-reject-ambiguous-architecture
  (let [root
        (io/file (System/getProperty "java.io.tmpdir") (str "vis-gliner-import-" (random-uuid)))

        identities
        [{"model" "gliner2.5-base" "family" "gliner2.5" "architecture" "boundary"}
         {"model" "gliner2.5-decide" "family" "gliner2.5" "architecture" "span"}]

        archives
        (mapv (fn [n identity]
                (fixture! (apply str (repeat 64 (str n))) nil identity))
              [1 2]
              identities)

        forged
        (fixture! (apply str (repeat 64 "3"))
                  nil
                  {"model" "gliner2.5-base" "family" "gliner2.5" "architecture" "span"})

        validated
        (atom [])]

    (try (with-redefs [assets/models-root (constantly (str root))]
           (doseq [[identity archive] (map vector identities archives)]
             (let [model-id (get identity "model")
                   alias (str "local-" (if (= model-id "gliner2.5-base") "base" "decide"))
                   saved (registry/register! archive
                                             (digest archive)
                                             (fn [model _]
                                               (swap! validated conj (:id model))))
                   ref (get saved "model_ref")]

               (is (= model-id (get-in (registry/resolve-model ref) [:model :id])))
               (is (= (get identity "architecture") (get assets/gliner-architectures model-id)))
               (is (nil? (registry/get-alias alias)))
               (is (= :decisions/invalid-alias
                      (:type (error-data #(registry/activate! model-id ref nil)))))
               (is (= ref (get (registry/activate! alias ref nil) "model_ref")))
               (is (= model-id (get-in (registry/resolve-model alias) [:model :id])))
               (is (= saved
                      (registry/register! archive
                                          (digest archive)
                                          (fn [& _]
                                            (throw (ex-info "Duplicate validation" {}))))))))
           (is (= (mapv #(get % "model") identities) @validated))
           (is (= 2 (count (registry/versions))))
           (is (= :decisions/invalid-archive
                  (:type (error-data #(registry/register! forged
                                                          (digest forged)
                                                          (fn [& _]))))))
           (is (= 2 (count (registry/versions)))))
         (finally (files/delete-dir! root)
                  (doseq [archive (conj archives forged)]
                    (.delete ^File archive))))))
