(ns com.blockether.vis.internal.extension-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-channel-fn
  [& _]
  nil)

(defdescribe prompt-normalization-test
  (it "normalizes string and fn extension prompts"
    (let [prompt-text "\n\n    First line\n\n\n\n      Nested line\n"
          string-ext (extension/extension
                       {:ext/name "test.prompt-string"
                        :ext/description "Test prompt string."
                        :ext/prompt prompt-text})
          fn-ext (extension/extension
                   {:ext/name "test.prompt-fn"
                    :ext/description "Test prompt fn."
                    :ext/prompt (fn [_] prompt-text)})]
      (expect (= "First line\n\n  Nested line" ((:ext/prompt string-ext) {})))
      (expect (= "First line\n\n  Nested line" ((:ext/prompt fn-ext) {}))))))

(defdescribe ctx-contributions-test
  (it "binds active workspace root while building extension ctx"
    (let [root (.getCanonicalPath (java.io.File. "target/test-workspace-ctx"))
          ext  {:ext/name "test.ctx-workspace"
                :ext/ctx  (fn [_]
                            {:project {:ctx-root workspace/*workspace-root*
                                       :cwd      (.getCanonicalPath (workspace/cwd))}})}
          ctx  (extension/ctx-contributions {:workspace/root root} [ext])]
      (expect (= root (get-in ctx [:project :ctx-root])))
      (expect (= root (get-in ctx [:project :cwd]))))))

(defdescribe channel-contributions-test
  (it "extension accepts channel contributions and derives channel kind"
    (let [ext (extension/extension
                {:ext/name "test.channel-contribution"
                 :ext/description "Test channel contribution."
                 :ext/channel-contributions
                 {:tui.slot/commands
                  [{:id :test/command
                    :fn #'sample-channel-fn}]}})]
      (expect (= "channels" (:ext/kind ext)))
      (expect (= {:tui.slot/commands [{:id :test/command
                                       :fn #'sample-channel-fn}]}
                (:ext/channel-contributions ext)))))

  (it "normalizes slot keys into channel-id and slot fields"
    (with-redefs [extension/registered-extensions
                  (fn []
                    [{:ext/channel-contributions
                      {:tui.slot/commands
                       [{:id :voice/input
                         :fn #'sample-channel-fn}]
                       :telegram.slot/preamble
                       [{:id :telegram/preamble
                         :fn #'sample-channel-fn}]}}])]
      (expect (= [{:id :voice/input
                   :fn #'sample-channel-fn
                   :channel-id :tui
                   :slot :tui.slot/commands}]
                (extension/channel-contributions-for :tui :tui.slot/commands)))
      (expect (= [:tui.slot/commands]
                (mapv :slot (extension/channel-contributions-for :tui)))))))

(defdescribe workspace-backend-extension-test
  (it "registers and deregisters workspace backends with their extension"
    (let [backend-id :test/extension-workspace
          ext-name "test.workspace-backend"
          backend (workspace/workspace-backend
                    {:workspace.backend/id backend-id
                     :workspace.backend/priority 500
                     :workspace.backend/capabilities #{:isolated-fork :rollback}
                     :workspace.backend/available-fn (constantly true)
                     :workspace.backend/fork-fn (fn [_] "/tmp/test-workspace")
                     :workspace.backend/discard-fn (fn [_] nil)})]
      (try
        (extension/register-extension!
          {:ext/name ext-name
           :ext/description "Workspace backend registration test."
           :ext/workspace-backends [backend]})
        (expect (some #(= backend-id (:workspace.backend/id %))
                  (workspace/registered-backends)))
        (finally
          (extension/deregister-extension! ext-name)))
      (expect (not-any? #(= backend-id (:workspace.backend/id %))
                (workspace/registered-backends))))))

(defdescribe startable-resource-visibility-test
  ;; The SAME `registered-startable-resources` feeds the web Resources modal AND
  ;; the TUI resource dialog, so a `:visible-fn` gate (e.g. MCP behind
  ;; :mcp/enabled) hides a startable from BOTH channels at once. This pins that
  ;; filter so the two surfaces can never drift.
  (it "drops startables whose :visible-fn is false; keeps gate-less + true ones"
    (let [ext-name "test.startable-visibility"
          flag     (atom false)
          kinds    (fn [] (set (map :kind (extension/registered-startable-resources))))]
      (try
        (extension/register-extension!
          {:ext/name ext-name
           :ext/description "Startable visibility test."
           :ext/startable-resources
           [{:kind :test/always   :label "always"  :start-fn (fn [_ _] nil)}
            {:kind :test/gated     :label "gated"   :start-fn (fn [_ _] nil)
             :visible-fn (fn [] @flag)}]})
        ;; flag false → gated startable hidden, always-on one present
        (reset! flag false)
        (expect (contains? (kinds) :test/always))
        (expect (not (contains? (kinds) :test/gated)))
        ;; flip the gate on → it appears (same fn, both channels see it)
        (reset! flag true)
        (expect (contains? (kinds) :test/gated))
        (finally
          (extension/deregister-extension! ext-name)))
      (expect (not (contains? (kinds) :test/gated)))))

  (it "a throwing :visible-fn fails OPEN (shown), never hides a needed control"
    (let [ext-name "test.startable-visibility-throw"
          kinds    (fn [] (set (map :kind (extension/registered-startable-resources))))]
      (try
        (extension/register-extension!
          {:ext/name ext-name
           :ext/description "Startable visibility fail-open test."
           :ext/startable-resources
           [{:kind :test/boom :label "boom" :start-fn (fn [_ _] nil)
             :visible-fn (fn [] (throw (ex-info "nope" {})))}]})
        (expect (contains? (kinds) :test/boom))
        (finally
          (extension/deregister-extension! ext-name))))))

(defdescribe slash-command-registration-test
  (it "slash path collisions across extensions are rejected at register-extension! time"
    ;; The union of `:ext/slash-commands` across all registered
    ;; extensions must contain unique `[parent name]`
    ;; paths. A second extension that declares the same path as an
    ;; already-registered extension is refused. Hot reload of the
    ;; SAME extension id is still allowed because its prior entry is
    ;; excluded from the conflict scan.
    (try
      (extension/register-extension!
        {:ext/name        "test.slash-collide-a"
         :ext/description "first owner of /probe"
         :ext/slash-commands
         [{:slash/name   "probe"
           :slash/doc    "probe original"
           :slash/run-fn (fn [_] {:slash/status :ok})}]})
      (let [thrown (try
                     (extension/register-extension!
                       {:ext/name        "test.slash-collide-b"
                        :ext/description "duplicate owner of /probe"
                        :ext/slash-commands
                        [{:slash/name   "probe"
                          :slash/doc    "probe dup"
                          :slash/run-fn (fn [_] {:slash/status :ok})}]})
                     nil
                     (catch clojure.lang.ExceptionInfo e
                       (ex-data e)))]
        (expect (= :extension/slash-path-collision (:type thrown)))
        (expect (= ["probe"] (-> thrown :collisions first :path))))
      ;; Hot-reload of the SAME id with the SAME path = allowed.
      (expect (some? (extension/register-extension!
                       {:ext/name        "test.slash-collide-a"
                        :ext/description "reload owner of /probe"
                        :ext/slash-commands
                        [{:slash/name   "probe"
                          :slash/doc    "probe reloaded"
                          :slash/run-fn (fn [_] {:slash/status :ok})}]})))
      (finally
        (extension/deregister-extension! "test.slash-collide-a")
        (extension/deregister-extension! "test.slash-collide-b")))))
