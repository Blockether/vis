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

(defdescribe
  registered-tool-activity-test
  (it
    "gives every first-party tool readable copy and an explicit visibility policy"
    (let [entries
          (for [ext
                [foundation/vis-extension mcp/vis-extension]

                entry
                (get-in ext [:ext/engine :ext.engine/symbols])

                :when (and (:ext.symbol/fn entry) (not (:ext.symbol/raw? entry)))]

            entry)

          end-only
          '#{cat patch _shell-logs _shell-type council.read council.get council.threads
             council.members draft-status main-agent-instructions update_goal council.publish
             council.cancel council.extend council.deny council.grant council.result
             automations.list automations.get automations.create automations.update
             automations.delete automations.run automations.runs}]

      (expect (= end-only
                 (set (keep #(when (false? (get-in % [:ext.symbol/activity :show-start]))
                               (:ext.symbol/symbol %))
                            entries))))
      (doseq [entry
              entries

              :let [declaration
                    (:ext.symbol/activity entry)

                    headline
                    (:headline declaration)

                    symbol
                    (:ext.symbol/symbol entry)]]

        (expect (map? declaration) (str symbol))
        (expect (and (string? headline) (re-matches #"[A-Z][A-Za-z ]+" headline)))
        (expect (= (not (contains? end-only symbol)) (:show-start declaration)) (str symbol))
        (expect (fn? (:render declaration)))))))

(defdescribe
  draft-workflow-prompt-test
  (it
    "requires the draft workflow only for explicitly enabled backends"
    (let [value-of toggles/value-of]
      (doseq [backend ["auto" "worktree" "rift"]]
        (with-redefs [toggles/value-of (fn [id]
                                         (if (= "draft_backend" id) backend (value-of id)))]
          (let [prompt ((:ext/prompt-fn foundation/vis-extension) {})]
            (doseq
              [required
               ["## Draft workflow" "standing authorization to create drafts without asking"
                "every change-making task (code, tests, documentation and configuration)"
                "Read-only questions, analysis and diff previews do not require a draft"
                "draft_status()" "draft_create(\"task-name\")" "before editing"
                "this session's draft for the same task"
                "Never edit the shared checkout or another session's draft" "project_root_path"
                "next block" "Keep edits, formatting and verification in the draft" "draft_diff()"
                "roots=[project_root_path, sibling_path]" "draft_sync()" "draft_approve()"
                "commits and may push"
                "user or applicable project instructions authorize commit and push"
                "Review-first, local-only and no-commit/push requests leave the draft unapproved"
                ;; Completed merged drafts are cleanup, not a new approval gate.
                "fetch origin" "origin/<target_branch>" "before editing" "no pending changes"
                "all draft commits merged into the target" "required publication succeeded"
                "draft_discard()` without asking" "Do not discard an active task"
                "Confirm destructive discard only when unapproved changes or unmerged commits would be lost"
                "If drafts are unavailable or blocked, report the blocker"
                "Never silently fall back to shared-checkout edits"]]
              (expect (str/includes? prompt required) (str backend ": " required))))))))
  (it "omits the workflow by default, removes it when switched off and restores it on opt-in"
      (let [value-of toggles/value-of]
        (doseq [backend [nil "" "unknown" "auto" "off" "rift"]]
          (with-redefs [toggles/value-of (fn [id]
                                           (if (= "draft_backend" id) backend (value-of id)))]
            (let [prompt ((:ext/prompt-fn foundation/vis-extension) {})]
              (expect (= (contains? #{"auto" "rift"} backend)
                         (str/includes? prompt "## Draft workflow")))
              (when-not (contains? #{"auto" "rift"} backend)
                (doseq [absent ["draft_create(" "draft_approve(" "draft_discard("]]
                  (expect (not (str/includes? prompt absent)))))))))))

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
          (expect (str/includes? prompt "## Subagents (Council)"))
          (expect (str/includes? prompt "do only your task")))))
  (it "is the manifest's single initializer for core facilities"
      (let [initialization (set (manifest/initializers))]
        (expect (contains? initialization 'com.blockether.vis.internal.foundation.core/register!))
        (doseq [removed ['com.blockether.vis.internal.foundation.introspection/register!
                         'com.blockether.vis.internal.foundation.shell/register!]]
          (expect (not (contains? initialization removed)))))
      (foundation/register!)
      (expect (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions)))))
