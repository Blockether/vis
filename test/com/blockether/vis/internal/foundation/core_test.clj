(ns com.blockether.vis.internal.foundation.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.context.renderer :as renderer]
            [com.blockether.vis.internal.python.env :as python-env]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.test-python-context :as tpc]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe registered-tool-activity-test
             (it "gives every first-party tool readable copy and an explicit visibility policy"
                 (let [entries (for [ext [foundation/vis-extension mcp/vis-extension]
                                     entry (get-in ext [:ext/engine :ext.engine/symbols])
                                     :when (and (:ext.symbol/fn entry)
                                                (not (:ext.symbol/raw? entry)))]

                                 entry)]
                   (doseq [entry entries
                           :let [declaration (:ext.symbol/activity entry)
                                 headline (:headline declaration)
                                 symbol (:ext.symbol/symbol entry)]]

                     (expect (map? declaration) (str symbol))
                     (expect (and (string? headline) (re-matches #"[A-Z][A-Za-z ]+" headline)))
                     (expect (boolean? (:show-start declaration)) (str symbol))
                     (expect (fn? (:render declaration)))))))

(defdescribe
  project-path-prompt-runtime-contract-test
  (it
    "renders and binds one registry, including removals as append-only prompt deltas"
    (tpc/with-own
      [python {}]
      (let [snapshot
            (policy/snapshot {"workspace" {"filesystem"
                                           [{"id" "library" "path" "/projects/library"}
                                            {"id" "cache" "path" "/projects/cache" "search" false}]}
                              "jail" {"enabled" false}})

            env
            {:workspace/root "/projects/main" :security-policy snapshot}

            ctx
            ((:ext/ctx-fn foundation/vis-extension) env)

            standing
            (renderer/ctx-static-map {:ctx ctx})

            rendered
            (renderer/render-ctx-static {:ctx ctx})

            removed-ctx
            ((:ext/ctx-fn foundation/vis-extension)
              (assoc env :security-policy (policy/snapshot {"jail" {"enabled" true}})))

            removed
            (renderer/ctx-static-map {:ctx removed-ctx})

            delta
            (renderer/render-ctx-delta standing removed)]

        (expect (= "/projects/main" (get-in standing ["workspace" "root"])))
        (expect (= [{"cwd" "/projects/library" "python_name" "library_path"}]
                   (filter #(get % "python_name")
                           (get-in standing ["workspace" "filesystem_roots"]))))
        (expect (not (contains? (get standing "workspace") "path_globals")))
        (expect (str/includes? rendered "\"python_name\": \"library_path\""))
        (expect (= 1 (count (re-seq #"\"/projects/library\"" rendered))))
        ;; The jail's automatic read-only grants stay in the one path list; the project leaves it.
        (expect (str/includes? delta "session[\"workspace\"][\"filesystem_roots\"]"))
        (expect (not (str/includes? delta "/projects/library")))
        (python-env/bind-ctx! python standing)
        (expect (= "/projects/main /projects/library\n"
                   (:stdout (python-env/run-python-block
                              python
                              "print(project_root_path, library_path)"))))
        (python-env/bind-ctx! python removed)
        (expect (= "False\n"
                   (:stdout (python-env/run-python-block
                              python
                              "print('library_path' in globals())"))))))))

(defdescribe
  vis-foundation-aggregator-test
  (it "includes leadership guidance only when subagents are enabled"
      ;; Subagents run on Council messages, so the guidance needs both settings on.
      (with-redefs [toggles/enabled?
                    #(contains? #{"council" "subagents"} %)

                    toggles/value-of
                    (constantly "off")]

        (let [prompt ((:ext/prompt-fn foundation/vis-extension) {})]
          (expect (str/includes? prompt "## Subagents (Council)")))))
  (it "is the manifest's single initializer for core facilities"
      (let [initialization (set (manifest/initializers))]
        (expect (contains? initialization 'com.blockether.vis.internal.foundation.core/register!))
        (doseq [removed ['com.blockether.vis.internal.foundation.introspection/register!
                         'com.blockether.vis.internal.foundation.shell/register!]]
          (expect (not (contains? initialization removed)))))
      (foundation/register!)
      (expect (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions)))))
