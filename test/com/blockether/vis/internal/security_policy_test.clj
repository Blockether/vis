(ns com.blockether.vis.internal.security-policy-test
  (:require [com.blockether.vis.internal.security-policy :as policy]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]))

(defdescribe
  security-policy-snapshot-test
  (it
    "resolves configured paths once, hashes the policy, and renders HOME-relative access"
    (let
      [home
       (.getCanonicalFile (.toFile (Files/createTempDirectory
                                     "vis-policy-home"
                                     (make-array java.nio.file.attribute.FileAttribute 0))))

       project
       (doto (java.io.File. home "vis") .mkdirs)

       sibling
       (doto (java.io.File. home "spel") .mkdirs)

       cache
       (doto (java.io.File. home ".m2") .mkdirs)

       cfg
       {"workspace" {"filesystem"
                     [{"id" "spel" "path" "~/spel" "description" "Sibling repo"}
                      {"id" "ro" "path" "~/read-only" "access" "read-only"}
                      {"id" "m2" "path" "~/.m2" "search" false "description" "Maven cache"}]}
        "jail" {"enabled" true "filesystem" {"allow" ["spel" "ro" "m2"]} "inbound_ports" [5273]}
        "network" {"allowed_domains" ["example.com"]}}

       snapshot
       (policy/snapshot cfg {:base-dir (.getPath project) :home (.getPath home)})

       view
       (policy/access-view snapshot [(.getPath project)])]

      (expect (= [(.getCanonicalPath sibling) (.getCanonicalPath cache)]
                 (policy/read-write-roots snapshot)))
      (expect (= "~/vis" (policy/home-relative (.getPath project) (.getPath home))))
      (expect (= ["~/vis" "~/spel" "~/.m2"] (get-in view ["filesystem" "read_write"])))
      (expect (= ["~/read-only"] (get-in view ["filesystem" "process_read_only"])))
      (expect (= ["~/.m2"] (get-in view ["filesystem" "no_search"])))
      (expect (= {"~/spel" "Sibling repo" "~/.m2" "Maven cache"}
                 (get-in view ["filesystem" "descriptions"])))
      (expect (= [5273] (get-in view ["network" "inbound_ports"])))
      (expect (= "reload" (get view "changes_require")))
      (expect (re-matches #"sha256:[0-9a-f]{64}" (get view "generation")))))
  (it "keeps a stable generation for equivalent snapshots and changes it with policy"
      (let
        [base
         {"jail" {"filesystem" {"allow" []}} "network" {"allowed_domains" ["example.com"]}}

         a
         (policy/snapshot base)

         b
         (policy/snapshot base)

         c
         (policy/snapshot (assoc-in base ["network" "allowed_domains"] ["other.example"]))]

        (expect (= (:generation a) (:generation b)))
        (expect (not= (:generation a) (:generation c)))))
  (it "surfaces config_error in the access view only when the policy carries one"
      (let
        [snap
         (policy/snapshot {"network" {"allowed_domains" ["example.com"]}})

         clean
         (policy/access-view snap [])

         erred
         (policy/access-view
           (assoc snap
             :config-error {"source" "vis.yml"
                            "problems" ["toggles: unknown top-level config key (config is closed)"]
                            "hint" "fix it and /reload"})
           [])]

        (expect (not (contains? clean "config_error")))
        (expect (= "vis.yml" (get-in erred ["config_error" "source"])))
        (expect (= ["toggles: unknown top-level config key (config is closed)"]
                   (get-in erred ["config_error" "problems"]))))))
