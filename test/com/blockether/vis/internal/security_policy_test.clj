(ns com.blockether.vis.internal.security-policy-test
  (:require [com.blockether.vis.internal.config-spec :as config-spec]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.security-policy :as policy]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]))

(defdescribe
  security-policy-snapshot-test
  (it
    "resolves configured paths once, hashes the policy, and renders HOME-relative access"
    (let [home
          (.getCanonicalFile (.toFile (Files/createTempDirectory
                                        "vis-policy-home"
                                        (make-array java.nio.file.attribute.FileAttribute 0))))

          project
          (doto (java.io.File. home "vis") .mkdirs)

          sibling
          (doto (java.io.File. home "demo") .mkdirs)

          cache
          (doto (java.io.File. home ".m2") .mkdirs)

          cfg
          {"workspace"
           {"filesystem"
            [{"id" "demo" "path" "~/demo" "description" "Sibling repo" "draft" "copy-and-apply"}
             {"id" "ro" "path" "~/read-only" "access" "read-only" "draft" "not-allowed"}
             {"id" "m2" "path" "~/.m2" "search" false "description" "Maven cache"}]}
           "jail" {"enabled" true
                   "filesystem" {"allow" ["demo" "ro" "m2"]}
                   "network" {"allowed_domains" ["example.com"] "inbound_ports" [5273]}}}

          snapshot
          (policy/snapshot cfg {:base-dir (.getPath project) :home (.getPath home)})

          view
          (policy/access-view snapshot [(.getPath project)])]

      ;; The implicit session folder (`~/.vis`) is always granted, engine-level.
      (expect (= [(.getCanonicalPath sibling) (.getCanonicalPath cache)
                  (.getPath (java.io.File. home ".vis"))]
                 (policy/read-write-roots snapshot)))
      (expect (= "~/vis" (policy/home-relative (.getPath project) (.getPath home))))
      (expect (= "~/vis/AGENTS.md"
                 (paths/abbreviate-home (.getPath (java.io.File. project "AGENTS.md"))
                                        (.getPath home))))
      (expect (= "~/" (paths/abbreviate-home (.getPath home) (.getPath home))))
      (expect (= "relative/AGENTS.md" (paths/abbreviate-home "relative/AGENTS.md" (.getPath home))))
      (expect (= (str (.getPath home) "-other/AGENTS.md")
                 (paths/abbreviate-home (str (.getPath home) "-other/AGENTS.md") (.getPath home))))
      (expect (= ["~/vis" "~/demo" "~/.m2" "~/.vis"] (get-in view ["filesystem" "read_write"])))
      (expect (= ["~/read-only"] (get-in view ["filesystem" "process_read_only"])))
      (expect (= ["~/.m2" "~/.vis"] (get-in view ["filesystem" "no_search"])))
      (expect (= {"~/demo" "Sibling repo"
                  "~/.m2" "Maven cache"
                  "~/.vis" (get config-spec/vis-home-entry "description")}
                 (get-in view ["filesystem" "descriptions"])))
      ;; Only roots that opt OUT of the default `shared` isolation are named.
      (expect (= {"~/demo" "copy-and-apply" "~/read-only" "not-allowed"}
                 (get-in view ["filesystem" "draft"])))
      (expect (= {(.getCanonicalPath sibling) :copy-and-apply
                  (.getCanonicalPath (java.io.File. home "read-only")) :not-allowed}
                 (policy/draft-policies snapshot)))
      (expect (= [5273] (get-in view ["network" "inbound_ports"])))
      (expect (= "reload" (get view "changes_require")))
      ;; Confinement is named `jail` end to end: the access view says `is_jailed`,
      ;; never `sandboxed` (which read like the Python sandbox).
      (expect (true? (get view "is_jailed")))
      (expect (not (contains? view "sandboxed")))
      (expect (re-matches #"sha256:[0-9a-f]{64}" (get view "generation")))))
  (it
    "grants unrestricted explicit filesystem access when the jail is disabled"
    (let [home
          (.getCanonicalFile (.toFile (Files/createTempDirectory
                                        "vis-policy-open"
                                        (make-array java.nio.file.attribute.FileAttribute 0))))

          project
          (doto (java.io.File. home "vis") .mkdirs)

          base
          (.getPath project)

          snapshot
          (policy/snapshot {"jail" {"enabled" false}} {:base-dir base :home (.getPath home)})

          host-roots
          (->> (java.io.File/listRoots)
               (mapv #(.getCanonicalPath ^java.io.File %)))

          view
          (policy/access-view snapshot [base])]

      (expect (false? (:jail-enabled snapshot)))
      (expect (false? (get view "is_jailed")))
      (expect (= host-roots (policy/read-write-roots snapshot)))
      (expect (= host-roots (policy/no-search-roots snapshot)))
      (expect (= (vec (distinct (concat ["~/vis"] host-roots)))
                 (get-in view ["filesystem" "read_write"])))
      (expect (= host-roots (get-in view ["filesystem" "no_search"])))
      (expect (= (mapv (fn [root]
                         {:trunk root :clone root :draft :shared :no-search? true})
                       host-roots)
                 (workspace/env-filesystem-roots {:security-policy snapshot
                                                  :security/filesystem-roots []
                                                  :security/no-search-roots []})))))
  (it
    "keeps a stable generation for equivalent snapshots and changes it with policy"
    (let [base
          {"jail"
           {"enabled" true "filesystem" {"allow" []} "network" {"allowed_domains" ["example.com"]}}}

          a
          (policy/snapshot base)

          b
          (policy/snapshot base)

          c
          (policy/snapshot (assoc-in base ["jail" "network" "allowed_domains"] ["other.example"]))]

      (expect (= (:generation a) (:generation b)))
      (expect (not= (:generation a) (:generation c)))))
  (it "surfaces config_error in the access view only when the policy carries one"
      (let [snap
            (policy/snapshot {"jail" {"network" {"allowed_domains" ["example.com"]}}})

            clean
            (policy/access-view snap [])

            erred
            (policy/access-view (assoc snap
                                  :config-error
                                  {"source" "vis.yml"
                                   "problems"
                                   ["toggles: unknown top-level config key (config is closed)"]
                                   "hint" "fix it and /reload"})
                                [])]

        (expect (not (contains? clean "config_error")))
        (expect (= "vis.yml" (get-in erred ["config_error" "source"])))
        (expect (= ["toggles: unknown top-level config key (config is closed)"]
                   (get-in erred ["config_error" "problems"]))))))
