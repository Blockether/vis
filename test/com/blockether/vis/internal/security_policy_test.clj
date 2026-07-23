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
       (.toFile (Files/createTempDirectory "vis-policy-home"
                                           (make-array java.nio.file.attribute.FileAttribute 0)))

       project
       (doto (java.io.File. home "vis") .mkdirs)

       sibling
       (doto (java.io.File. home "spel") .mkdirs)

       _
       (doto (java.io.File. home ".m2") .mkdirs)

       cfg
       {"sandbox" true
        "jail" {"filesystem" {"allow-read-write" ["../spel"]
                              "allow-read" ["~/read-only"]
                              "language-caches" ["~/.m2"]}
                "inbound-ports" [5273]}
        "network" {"allowed-domains" ["example.com"]}}

       snapshot
       (policy/snapshot cfg {:base-dir (.getPath project) :home (.getPath home)})

       view
       (policy/access-view snapshot [(.getPath project)])]

      (expect (= [(.getCanonicalPath sibling)] (policy/read-write-roots snapshot)))
      (expect (= "~/vis" (policy/home-relative (.getPath project) (.getPath home))))
      (expect (= ["~/vis" "~/spel"] (get-in view ["filesystem" "read_write"])))
      (expect (= ["~/read-only"] (get-in view ["filesystem" "process_read_only"])))
      (expect (= [{"path" "~/.m2" "access" "read_write"}]
                 (get-in view ["filesystem" "process_only" "language_caches"])))
      (expect (= [5273] (get-in view ["network" "inbound_ports"])))
      (expect (= "reload" (get view "changes_require")))
      (expect (re-matches #"sha256:[0-9a-f]{64}" (get view "generation")))))
  (it "keeps a stable generation for equivalent snapshots and changes it with policy"
      (let
        [base
         {"jail" {"filesystem" {"allow-read-write" []}}
          "network" {"allowed-domains" ["example.com"]}}

         a
         (policy/snapshot base)

         b
         (policy/snapshot base)

         c
         (policy/snapshot (assoc-in base ["network" "allowed-domains"] ["other.example"]))]

        (expect (= (:generation a) (:generation b)))
        (expect (not= (:generation a) (:generation c))))))
