(ns com.blockether.vis.internal.config.yaml-cache-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]
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

(defdescribe
  unchanged-yaml-is-read-and-parsed-once
  (it "unchanged yaml is read and parsed once"
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
                  (expect (identical? first-map (#'config/parse-yaml-config-map (.getPath file)))))
                (expect (= "${VIS_YAML_CACHE_TEST}" (get first-map "system_prompt"))))
              (expect (= 1 @reads))
              (expect (= 1 @parses))))))))

(defdescribe
  yaml-cache-observes-file-lifecycle-and-precise-stamps
  (it
    "yaml cache observes file lifecycle and precise stamps"
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

          (expect (nil? (parse-file)))
          (spit file "agent_name: Ada\n")
          (Files/setLastModifiedTime path time-a)
          (expect (= {"agent_name" "Ada"} (parse-file)))
          ;; same-size edits within one millisecond still invalidate
          (let [millis (.lastModified file)]
            (spit file "agent_name: Eve\n")
            (Files/setLastModifiedTime path time-b)
            (expect (= millis (.lastModified file)))
            (expect (= {"agent_name" "Eve"} (parse-file))))
          ;; size changes invalidate even when mtime is preserved
          (spit file "agent_name: Grace\n")
          (Files/setLastModifiedTime path time-b)
          (expect (= {"agent_name" "Grace"} (parse-file)))
          (expect (.delete file))
          (expect (nil? (parse-file)))
          (spit file "agent_name: New\n")
          (expect (= {"agent_name" "New"} (parse-file))))))))

(defdescribe malformed-and-non-map-yaml-are-cached-without-hiding-repairs
             (it "malformed and non map yaml are cached without hiding repairs"
                 (with-config-files
                   (fn [dir]
                     (doseq [text ["agent_name: [\n" "- a\n- b\n" ""]]
                       (let [file (io/file dir "vis.yml")
                             parses (atom 0)
                             parse-yaml yamlstar/load]

                         (spit file text)
                         (with-redefs [yamlstar/load (fn [source]
                                                       (swap! parses inc)
                                                       (parse-yaml source))]
                           (dotimes [_ 3]
                             (expect (nil? (#'config/parse-yaml-config-map file))))
                           (expect (= 1 @parses))
                           (spit file "agent_name: Repaired\n")
                           (expect (= "Repaired"
                                      (get (#'config/parse-yaml-config-map file) "agent_name")))
                           (expect (= 2 @parses)))))))))

(defdescribe explicit-invalidation-and-reload-clear-every-config-cache
             (it "explicit invalidation and reload clear every config cache"
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
                           (expect (= "Ada" (get (config/load-config-raw) "agent_name")))
                           (spit file "agent_name: Eve\n")
                           (Files/setLastModifiedTime path stamp)
                           (invalidate)
                           (expect (= "Eve" (get (config/load-config-raw) "agent_name")))
                           (expect (= "Eve" (config/agent-name (.getPath dir)))))))))))

(defdescribe
  simultaneous-readers-share-one-cold-parse
  (it
    "simultaneous readers share one cold parse"
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
                   (expect (true? (deref entered 5000 nil)))
                   (deliver release true)
                   (doseq [reader readers]
                     (expect (= {"agent_name" "Ada"} (deref reader 5000 ::timeout))))
                   (expect (= 1 @parses))
                   (finally (deliver release true))))))))))

(defdescribe
  workspace-identities-reuse-all-unchanged-yaml-tiers
  (it
    "workspace identities reuse all unchanged yaml tiers"
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
              (expect (= "Overlay" (config/agent-name (.getPath a))))
              (expect (= "Grace" (config/agent-name (.getPath b)))))
            (expect (= 5 @parses))
            (spit (io/file store "state.yml") "agent_name: Personal\n")
            (expect (= "Personal" (config/agent-name (.getPath a))))
            (expect (= "Personal" (config/agent-name (.getPath b))))
            (expect (= 6 @parses))))))))

(defdescribe yaml-cache-bounds-distinct-paths-not-file-versions
             (it "yaml cache bounds distinct paths not file versions"
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
                           (expect (= 4 @parses))
                           (expect (= "Ada" (get (parse-file a) "agent_name")))
                           (expect (= 5 @parses)))
                         (let [{:keys [entries order]} @@#'config/yaml-config-cache]
                           (expect (= 2 (count entries) (count order)))
                           (expect (= (set (keys entries)) (set order))))))))))

(defdescribe
  invalidation-during-a-parse-cannot-repopulate-the-cache
  (it "invalidation during a parse cannot repopulate the cache"
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
                (try (expect (true? (deref entered 5000 nil)))
                     (spit file "agent_name: Eve\n")
                     (config/invalidate-config-cache!)
                     (expect (= "Eve" (get (#'config/parse-yaml-config-map file) "agent_name")))
                     (deliver release true)
                     (expect (= "Ada" (get (deref old-read 5000 nil) "agent_name")))
                     (expect (= "Eve" (get (#'config/parse-yaml-config-map file) "agent_name")))
                     (finally (deliver release true))))))))))

(defdescribe
  yaml-character-matching-compiles-without-reflection
  (it "yaml character matching compiles without reflection"
      ;; The old parser reflected Character/codePointAt once per matched codepoint.
      (let [warnings (java.io.StringWriter.)]
        (binding [*warn-on-reflection* true
                  *err* warnings]

          (require 'yaml-parser.parser :reload))
        (expect (not (str/includes? (str warnings) "java.lang.Character")) (str warnings)))
      (expect (= {"agent_name" "助手😀" "system_prompt" "First\nSecond\n"}
                 (yamlstar/load "agent_name: 助手😀\nsystem_prompt: |\n  First\n  Second\n")))))
