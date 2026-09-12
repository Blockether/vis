(ns com.blockether.vis.internal.config.yaml-cache-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [yamlstar.core :as yamlstar])
  (:import (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute FileTime)
           (java.util.concurrent TimeUnit)))

(defn- with-config-files
  [f]
  (let [dir (.toFile (Files/createTempDirectory "vis-yaml-cache" (make-array FileAttribute 0)))]
    (try (config/invalidate-config-cache!)
         (with-redefs [config/config-dir (constantly (.getPath (io/file dir "store")))]
           (binding [workspace/*workspace-root* (.getCanonicalPath dir)]
             (f dir)))
         (finally (config/invalidate-config-cache!)
                  (doseq [file (reverse (file-seq dir))]
                    (.delete ^java.io.File file))))))

(deftest unchanged-yaml-is-read-and-parsed-once
  ;; Regression: session identity bypassed the merged-config cache on every row.
  (with-config-files
    (fn [dir]
      (let [file
            (io/file dir "vis.yml")

            reads
            (atom 0)

            parses
            (atom 0)

            read-file
            slurp

            parse-yaml
            yamlstar/load]

        (spit file "agent_name: Ada\nsystem_prompt: '${VIS_YAML_CACHE_TEST}'\n")
        (with-redefs [clojure.core/slurp
                      (fn [& args]
                        (swap! reads inc)
                        (apply read-file args))

                      yamlstar/load
                      (fn [text]
                        (swap! parses inc)
                        (parse-yaml text))]

          (let [first-map (#'config/parse-yaml-config-map file)]
            (dotimes [_ 3]
              (is (identical? first-map (#'config/parse-yaml-config-map (.getPath file)))))
            (is (= "${VIS_YAML_CACHE_TEST}" (get first-map "system_prompt"))))
          (is (= 1 @reads))
          (is (= 1 @parses)))))))

(deftest yaml-cache-observes-file-lifecycle-and-precise-stamps
  (with-config-files
    (fn [dir]
      (let [file
            (io/file dir "vis.yml")

            path
            (.toPath file)

            parse-file
            #(#'config/parse-yaml-config-map file)

            time-a
            (FileTime/from 1700000000000000000 TimeUnit/NANOSECONDS)

            time-b
            (FileTime/from 1700000000000000100 TimeUnit/NANOSECONDS)]

        (is (nil? (parse-file)))
        (spit file "agent_name: Ada\n")
        (Files/setLastModifiedTime path time-a)
        (is (= {"agent_name" "Ada"} (parse-file)))
        (testing "same-size edits within one millisecond still invalidate"
          (let [millis (.lastModified file)]
            (spit file "agent_name: Eve\n")
            (Files/setLastModifiedTime path time-b)
            (is (= millis (.lastModified file)))
            (is (= {"agent_name" "Eve"} (parse-file)))))
        (testing "size changes invalidate even when mtime is preserved"
          (spit file "agent_name: Grace\n")
          (Files/setLastModifiedTime path time-b)
          (is (= {"agent_name" "Grace"} (parse-file))))
        (is (.delete file))
        (is (nil? (parse-file)))
        (spit file "agent_name: New\n")
        (is (= {"agent_name" "New"} (parse-file)))))))

(deftest malformed-and-non-map-yaml-are-cached-without-hiding-repairs
  (with-config-files (fn [dir]
                       (doseq [text ["agent_name: [\n" "- a\n- b\n" ""]]
                         (let [file (io/file dir "vis.yml")
                               parses (atom 0)
                               parse-yaml yamlstar/load]

                           (spit file text)
                           (with-redefs [yamlstar/load (fn [source]
                                                         (swap! parses inc)
                                                         (parse-yaml source))]
                             (dotimes [_ 3]
                               (is (nil? (#'config/parse-yaml-config-map file))))
                             (is (= 1 @parses))
                             (spit file "agent_name: Repaired\n")
                             (is (= "Repaired"
                                    (get (#'config/parse-yaml-config-map file) "agent_name")))
                             (is (= 2 @parses))))))))

(deftest explicit-invalidation-and-reload-clear-every-config-cache
  (with-config-files
    (fn [dir]
      (let [file
            (io/file dir "vis.yml")

            path
            (.toPath file)]

        (doseq [invalidate [config/invalidate-config-cache!
                            #(with-redefs [config/load-config config/load-config-raw
                                           config/active-config (atom nil)]
                               (config/reload-config!))]]
          (config/invalidate-config-cache!)
          (spit file "agent_name: Ada\n")
          (let [stamp (Files/getLastModifiedTime path (make-array LinkOption 0))]
            (is (= "Ada" (get (config/load-config-raw) "agent_name")))
            (spit file "agent_name: Eve\n")
            (Files/setLastModifiedTime path stamp)
            (invalidate)
            (is (= "Eve" (get (config/load-config-raw) "agent_name")))
            (is (= "Eve" (config/agent-name (.getPath dir))))))))))

(deftest simultaneous-readers-share-one-cold-parse
  (with-config-files
    (fn [dir]
      (let [file
            (io/file dir "vis.yml")

            parses
            (atom 0)

            parse-yaml
            yamlstar/load

            start
            (promise)

            entered
            (promise)

            release
            (promise)]

        (spit file "agent_name: Ada\n")
        (with-redefs [yamlstar/load (fn [text]
                                      (swap! parses inc)
                                      (deliver entered true)
                                      (deref release 5000 nil)
                                      (parse-yaml text))]
          (let [readers (mapv (fn [_]
                                (future @start (#'config/parse-yaml-config-map file)))
                              (range 8))]
            (try (deliver start true)
                 (is (true? (deref entered 5000 nil)))
                 (deliver release true)
                 (doseq [reader readers]
                   (is (= {"agent_name" "Ada"} (deref reader 5000 ::timeout))))
                 (is (= 1 @parses))
                 (finally (deliver release true)))))))))

(deftest workspace-identities-reuse-all-unchanged-yaml-tiers
  (with-config-files
    (fn [dir]
      (let [store
            (io/file dir "store")

            a
            (io/file dir "a")

            b
            (io/file dir "b")

            parses
            (atom 0)

            parse-yaml
            yamlstar/load]

        (doseq [directory [store a b (io/file a ".vis")]]
          (.mkdirs directory))
        (spit (io/file store "config.yaml") "agent_name: Global\n")
        (spit (io/file store "state.yml") "toggles:\n  shell: true\n")
        (spit (io/file a "vis.yaml") "agent_name: Ada\n")
        (spit (io/file a ".vis/config.yaml") "agent_name: Overlay\n")
        (spit (io/file b "vis.yml") "agent_name: Grace\n")
        (with-redefs [yamlstar/load (fn [text]
                                      (swap! parses inc)
                                      (parse-yaml text))]
          (dotimes [_ 3]
            (is (= "Overlay" (config/agent-name (.getPath a))))
            (is (= "Grace" (config/agent-name (.getPath b)))))
          (is (= 5 @parses))
          (spit (io/file store "state.yml") "agent_name: Personal\n")
          (is (= "Personal" (config/agent-name (.getPath a))))
          (is (= "Personal" (config/agent-name (.getPath b))))
          (is (= 6 @parses)))))))

(deftest yaml-cache-bounds-distinct-paths-not-file-versions
  (with-config-files
    (fn [dir]
      (with-redefs [config/yaml-config-cache-limit 2]
        (let [a (io/file dir "a.yml")
              b (io/file dir "b.yml")
              c (io/file dir "c.yml")
              parse-file #'config/parse-yaml-config-map
              parses (atom 0)
              parse-yaml yamlstar/load]

          (doseq [file [a b c]]
            (spit file "agent_name: Ada\n"))
          (with-redefs [yamlstar/load (fn [text]
                                        (swap! parses inc)
                                        (parse-yaml text))]
            (parse-file a)
            (parse-file b)
            (spit b "agent_name: Updated\n")
            (parse-file b)
            (parse-file c)
            (is (= 4 @parses))
            (is (= "Ada" (get (parse-file a) "agent_name")))
            (is (= 5 @parses)))
          (let [{:keys [entries order]} @@#'config/yaml-config-cache]
            (is (= 2 (count entries) (count order)))
            (is (= (set (keys entries)) (set order)))))))))

(deftest invalidation-during-a-parse-cannot-repopulate-the-cache
  (with-config-files
    (fn [dir]
      (let [file
            (io/file dir "vis.yml")

            parse-yaml
            yamlstar/load

            entered
            (promise)

            release
            (promise)]

        (spit file "agent_name: Ada\n")
        (with-redefs [yamlstar/load (fn [text]
                                      (when (str/includes? text "Ada")
                                        (deliver entered true)
                                        (deref release 5000 nil))
                                      (parse-yaml text))]
          (let [old-read (future (#'config/parse-yaml-config-map file))]
            (try (is (true? (deref entered 5000 nil)))
                 (spit file "agent_name: Eve\n")
                 (config/invalidate-config-cache!)
                 (is (= "Eve" (get (#'config/parse-yaml-config-map file) "agent_name")))
                 (deliver release true)
                 (is (= "Ada" (get (deref old-read 5000 nil) "agent_name")))
                 (is (= "Eve" (get (#'config/parse-yaml-config-map file) "agent_name")))
                 (finally (deliver release true)))))))))

(deftest yaml-character-matching-compiles-without-reflection
  ;; The old parser reflected Character/codePointAt once per matched codepoint.
  (let [warnings (java.io.StringWriter.)]
    (binding [*warn-on-reflection* true
              *err* warnings]

      (require 'yaml-parser.parser :reload))
    (is (not (str/includes? (str warnings) "java.lang.Character")) (str warnings)))
  (is (= {"agent_name" "助手😀" "system_prompt" "First\nSecond\n"}
         (yamlstar/load "agent_name: 助手😀\nsystem_prompt: |\n  First\n  Second\n"))))
