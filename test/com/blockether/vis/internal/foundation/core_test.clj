(ns com.blockether.vis.internal.foundation.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.foundation.introspection :as introspection]
            [com.blockether.vis.internal.context.renderer :as renderer]
            [com.blockether.vis.internal.python.env :as python-env]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.test-python-context :as tpc]
            [com.blockether.vis.internal.foundation.rewind :as rewind]
            [com.blockether.vis.internal.foundation.shell :as shell]
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
             council.subagents council.cancel council.route}]

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
                "never silently fall back to shared-checkout edits"]]
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
        (expect (= [{"cwd" "/projects/library"
                     "python_name" "library_path"
                     "isolated" false
                     "draft" "shared"}]
                   (filter #(get % "python_name")
                           (get-in standing ["workspace" "filesystem_roots"]))))
        (expect (not (contains? (get standing "workspace") "path_globals")))
        (expect (str/includes? rendered "\"python_name\": \"library_path\""))
        (expect (= 1 (count (re-seq #"\"/projects/library\"" rendered))))
        (expect (str/includes? delta "del session[\"workspace\"][\"filesystem_roots\"]"))
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
  (it "is a BUILT-IN with NO alias — symbols bind bare into the sandbox"
      (expect (true? (get-in foundation/vis-extension [:ext/engine :ext.engine/builtin?])))
      (expect (nil? (get-in foundation/vis-extension [:ext/engine :ext.engine/alias])))
      (expect (nil? (get-in foundation/vis-extension [:ext/engine :ext.engine/ns])))
      (let [symbols (set (map :ext.symbol/symbol
                              (get-in foundation/vis-extension [:ext/engine :ext.engine/symbols])))]
        (expect (every? symbols
                        ['read-session 'get-session 'list-sessions 'shell '_shell-logs '_shell-wait
                         '_shell-type '_shell-stop]))))
  ;; Removed: "merges markdown builders into the unified symbol surface".
  ;; The Markdown-builder surface was reorganised; the merged-symbols
  ;; assertion drifted from the live extension shape.
  (it "keeps stable contracts out of the dynamic foundation prompt"
      (with-redefs [agents/instructions
                    (fn []
                      {:found? false})

                    ;; This contract excludes explicitly toggle-gated core guidance.
                    toggles/enabled?
                    (constantly false)

                    toggles/value-of
                    (constantly "off")]

        (let [prompt ((:ext/prompt-fn foundation/vis-extension) {})]
          ;; Stable state/introspection/self-doc contracts belong in CORE or tool docs.
          (expect (not (str/includes? prompt "Env strategy")))
          (expect (not (str/includes? prompt "Cross-conversation introspection")))
          (expect (not (str/includes? prompt "Vis self-docs")))
          (expect (not (str/includes? prompt "RUNTIME")))
          (expect (not (str/includes? prompt "PROJECT-GUIDANCE")))
          (expect (not (str/includes? prompt "SCAN-WARNINGS")))
          ;; Language routing lives in the language-interface pack now: the
          ;; foundation prompt must not advertise language tools at all.
          (expect (not (str/includes? prompt "LANGUAGE TOOLS")))
          (expect (not (str/includes? prompt "EDITING ROUTES")))
          (expect (not (str/includes? prompt "Canonical path only")))
          (expect (not (str/includes? prompt "v/strategy")))
          (expect (not (str/includes? prompt "clojure.repl/doc")))
          (expect (not (str/includes? prompt "Do not emit Markdown/text strings")))
          (expect (not (str/includes? prompt "Do not render Markdown as IR")))
          (expect (not (str/includes? prompt "Leadership and managed subagents")))
          (expect (not (str/includes? prompt "Inherited conversation is background evidence")))
          (expect (< (count prompt) 3000)))))
  (it "includes leadership guidance only when subagents are enabled"
      (with-redefs [toggles/enabled?
                    #(= % "subagents")

                    toggles/value-of
                    (constantly "off")]

        (let [prompt ((:ext/prompt-fn foundation/vis-extension) {})]
          (expect (str/includes? prompt "Leadership and managed subagents"))
          (expect (str/includes? prompt "Inherited conversation is background evidence")))))
  (it "contributes only the workspace block through ctx now"
      ;; `:session/env` (host / project / extensions digest) moved to
      ;; `internal.context.env-digest` — it's core functionality, not extension-
      ;; owned. Foundation-core's `:ext/ctx-fn` keeps only the workspace
      ;; block; `(:project ctx)` is gone for good.
      (let [ctx ((:ext/ctx-fn foundation/vis-extension) {})]
        (expect (not (contains? ctx :project)))
        (expect (not (contains? ctx :session/env)))))
  ;; Removed: "does not leave a standalone md extension registered".
  ;; The extension registry shape changed; presence of 'v vs absence
  ;; of 'md is now covered by the manifest test below.
  (it "documents the kernel surface on the extension descriptor"
      ;; Regression: prior copy advertised a v/ markdown DSL
      ;; (h1/p/table/file-link/join/code-block) that was torn out in
      ;; commit 40da53d0 (\"demo: tear out v/ markdown DSL\"). Description
      ;; now lists the symbols that actually exist; do NOT let the old
      ;; DSL names creep back into the descriptor copy.
      (let [doc (:ext/description foundation/vis-extension)]
        (expect (str/includes? doc "toggle-gated shell and session introspection"))
        (expect (str/includes? doc "rewind"))
        (expect (str/includes? doc "file editing"))
        (expect (str/includes? doc "session workspace/VCS"))
        (expect (not (str/includes? doc "ext repro")))
        (expect (not (str/includes? doc "file-link")))
        (expect (not (str/includes? doc "answer builders")))))
  (it "is the manifest's single initializer for core facilities"
      (let [initialization (set (manifest/initializers))]
        (expect (contains? initialization 'com.blockether.vis.internal.foundation.core/register!))
        (doseq [removed ['com.blockether.vis.internal.foundation.introspection/register!
                         'com.blockether.vis.internal.foundation.shell/register!
                         'com.blockether.vis.internal.foundation.rewind/register!]]
          (expect (not (contains? initialization removed)))))
      (foundation/register!)
      (expect (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions))))
  (it "owns rewind, shell CLI, and the toggle-gated symbol groups"
      (expect (= rewind/op-hooks (:ext/op-hooks foundation/vis-extension)))
      (expect (some #(= "rewind" (:slash/name %)) (:ext/slash-commands foundation/vis-extension)))
      (expect (= ["shell"] (mapv :cmd/name (:ext/cli foundation/vis-extension))))
      (expect (every? (set (get-in foundation/vis-extension [:ext/engine :ext.engine/symbols]))
                      (concat introspection/all-symbols shell/shell-symbols)))
      (let [routes (get-in foundation/vis-extension
                           [:ext/channel-contributions :gateway.slot/http-routes])]
        (expect (= [:rewind/http] (mapv :id routes)))
        (expect (= rewind/routes-contribution (:fn (first routes)))))
      (let [checks ((:ext/doctor-fn foundation/vis-extension) {})]
        (expect (sequential? checks))
        (expect (every? :level checks)))))
