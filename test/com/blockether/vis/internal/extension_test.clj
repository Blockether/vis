(ns com.blockether.vis.internal.extension-test
  (:require [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as vis-loop]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-channel-fn [& _] nil)

(defdescribe prompt-normalization-test
             (it "normalizes string and fn extension prompts"
                 (let
                   [prompt-text
                    "\n\n    First line\n\n\n\n      Nested line\n"

                    string-ext
                    (extension/extension {:ext/name "test.prompt-string"
                                          :ext/description "Test prompt string."
                                          :ext/prompt-fn prompt-text})

                    fn-ext
                    (extension/extension {:ext/name "test.prompt-fn"
                                          :ext/description "Test prompt fn."
                                          :ext/prompt-fn (fn [_]
                                                           prompt-text)})]

                   (expect (= "First line\n\n  Nested line" ((:ext/prompt-fn string-ext) {})))
                   (expect (= "First line\n\n  Nested line" ((:ext/prompt-fn fn-ext) {}))))))

(defdescribe ctx-contributions-test
             (it "binds active workspace root while building extension ctx"
                 (let
                   [root
                    (.getCanonicalPath (java.io.File. "target/test-workspace-ctx"))

                    ext
                    {:ext/name "test.ctx-workspace"
                     :ext/ctx-fn (fn [_]
                                   {:project {:ctx-root workspace/*workspace-root*
                                              :cwd (.getCanonicalPath (workspace/cwd))}})}

                    ctx
                    (extension/ctx-contributions {:workspace/root root} [ext])]

                   (expect (= root (get-in ctx [:project :ctx-root])))
                   (expect (= root (get-in ctx [:project :cwd]))))))

(defdescribe
  channel-contributions-test
  (it "extension accepts channel contributions and derives channel kind"
      (let
        [ext (extension/extension {:ext/name "test.channel-contribution"
                                   :ext/description "Test channel contribution."
                                   :ext/channel-contributions {:tui.slot/commands
                                                               [{:id :test/command
                                                                 :fn #'sample-channel-fn}]}})]
        (expect (= "channels" (:ext/kind ext)))
        (expect (= {:tui.slot/commands [{:id :test/command :fn #'sample-channel-fn}]}
                   (:ext/channel-contributions ext)))))
  (it "normalizes slot keys into channel-id and slot fields"
      (with-redefs
        [extension/registered-extensions
         (fn []
           [{:ext/channel-contributions
             {:tui.slot/commands [{:id :voice/input :fn #'sample-channel-fn}]
              :api.slot/preamble [{:id :api/preamble :fn #'sample-channel-fn}]}}])]
        (expect
          (= [{:id :voice/input :fn #'sample-channel-fn :channel-id :tui :slot :tui.slot/commands}]
             (extension/channel-contributions-for :tui :tui.slot/commands)))
        (expect (= [:tui.slot/commands] (mapv :slot (extension/channel-contributions-for :tui)))))))

(defdescribe
  slash-command-registration-test
  (it "slash path collisions across extensions are rejected at register-extension! time"
      ;; The union of `:ext/slash-commands` across all registered
      ;; extensions must contain unique `[parent name]`
      ;; paths. A second extension that declares the same path as an
      ;; already-registered extension is refused. Hot reload of the
      ;; SAME extension id is still allowed because its prior entry is
      ;; excluded from the conflict scan.
      (try (extension/register-extension! {:ext/name "test.slash-collide-a"
                                           :ext/description "first owner of /probe"
                                           :ext/slash-commands [{:slash/name "probe"
                                                                 :slash/doc "probe original"
                                                                 :slash/run-fn (fn [_]
                                                                                 {:slash/status
                                                                                  :ok})}]})
           (let
             [thrown (try (extension/register-extension!
                            {:ext/name "test.slash-collide-b"
                             :ext/description "duplicate owner of /probe"
                             :ext/slash-commands [{:slash/name "probe"
                                                   :slash/doc "probe dup"
                                                   :slash/run-fn (fn [_]
                                                                   {:slash/status :ok})}]})
                          nil
                          (catch clojure.lang.ExceptionInfo e (ex-data e)))]
             (expect (= :extension/slash-path-collision (:type thrown)))
             (expect (= ["probe"]
                        (-> thrown
                            :collisions
                            first
                            :path))))
           ;; Hot-reload of the SAME id with the SAME path = allowed.
           (expect (some? (extension/register-extension!
                            {:ext/name "test.slash-collide-a"
                             :ext/description "reload owner of /probe"
                             :ext/slash-commands [{:slash/name "probe"
                                                   :slash/doc "probe reloaded"
                                                   :slash/run-fn (fn [_]
                                                                   {:slash/status :ok})}]})))
           (finally (extension/deregister-extension! "test.slash-collide-a")
                    (extension/deregister-extension! "test.slash-collide-b")))))

(defdescribe
  op-hook-test
  "Generic cross-cutting operation hooks: any extension may decorate an op it
   does NOT own, wired once at the invoke-symbol-wrapper chokepoint."
  (let
    [run-after
     @#'extension/run-op-after-hooks

     run-before
     @#'extension/run-op-before-hooks]

    (it "after-hooks compose: the result threads through each registered hook"
        (extension/register-op-hook! {:op :ophtest1
                                      :owner :a
                                      :fn (fn [_ _ _ r]
                                            (update-in r [:result :n] (fnil inc 0)))})
        (extension/register-op-hook! {:op :ophtest1
                                      :owner :b
                                      :fn (fn [_ _ _ r]
                                            (update-in r [:result :n] (fnil inc 0)))})
        (let [out (run-after :ophtest1 {} [] (extension/success {:result {:n 0}}))]
          (expect (= 2 (get-in out [:result :n])))))
    (it "registration is idempotent per owner+phase — re-register REPLACES, no dup"
        (extension/register-op-hook! {:op :ophtest2
                                      :owner :a
                                      :fn (fn [_ _ _ r]
                                            (assoc-in r [:result :v] 1))})
        (extension/register-op-hook! {:op :ophtest2
                                      :owner :a
                                      :fn (fn [_ _ _ r]
                                            (assoc-in r [:result :v] 9))})
        (let [out (run-after :ophtest2 {} [] (extension/success {:result {}}))]
          (expect (= 9 (get-in out [:result :v])))
          (expect (= 1 (count (get (deref @#'extension/op-hooks) :ophtest2))))))
    (it "a throwing after-hook is SKIPPED (best-effort) and the result is unchanged"
        (extension/register-op-hook! {:op :ophtest3
                                      :owner :a
                                      :fn (fn [_ _ _ _]
                                            (throw (ex-info "boom" {})))})
        (let [base (extension/success {:result {:ok true}})]
          (expect (= base (run-after :ophtest3 {} [] base)))))
    (it "before-hooks can rewrite the args vector"
        (extension/register-op-hook! {:op :ophtest4
                                      :phase :before
                                      :owner :a
                                      :fn (fn [_ _ args]
                                            (conj (vec args) :extra))})
        (expect (= [:x :extra] (run-before :ophtest4 {} [:x]))))
    (it "the generic operation dispatcher invokes a host operation with no :around hook"
        (expect (= 3 (extension/invoke-operation :ophtest-none {} + [1 2]))))
    (it "around middleware wraps the call, catches a throw, and recovers"
        (extension/register-op-hook! {:op :ophtest5
                                      :phase :around
                                      :owner :a
                                      :fn (fn [_ _ args nxt]
                                            (try (nxt args) (catch Throwable _ :recovered)))})
        (expect (= :recovered
                   (extension/invoke-operation :ophtest5
                                               {}
                                               (fn [& _]
                                                 (throw (ex-info "boom" {})))
                                               [:x]))))
    (it "around middleware can RETRY with rewritten args (the don't-fail pattern)"
        (let [attempts (atom 0)]
          (extension/register-op-hook! {:op :ophtest6
                                        :phase :around
                                        :owner :a
                                        :fn (fn [_ _ args nxt]
                                              (try (nxt args) (catch Throwable _ (nxt [:fixed]))))})
          (let
            [f (fn [a]
                 (swap! attempts inc)
                 (if (= a :fixed) :ok (throw (ex-info "nope" {}))))]
            (expect (= :ok (extension/invoke-operation :ophtest6 {} f [:bad])))
            (expect (= 2 @attempts)))))
    (it "declarative :ext/op-hooks install on register and tear down on deregister"
        (try (extension/register-extension! {:ext/name "test.ophooks-ext"
                                             :ext/description "declarative op-hooks lifecycle"
                                             :ext/op-hooks [{:op :ophtest-decl
                                                             :phase :after
                                                             :fn (fn [_ _ _ r]
                                                                   r)}]})
             (let [hooks (get (deref @#'extension/op-hooks) :ophtest-decl)]
               (expect (= 1 (count hooks)))
               ;; owner derived from the ext name, no explicit :owner in the manifest
               (expect (= :ext/test.ophooks-ext (:owner (first hooks)))))
             (extension/deregister-extension! "test.ophooks-ext")
             (expect (nil? (get (deref @#'extension/op-hooks) :ophtest-decl)))
             (finally (extension/deregister-extension! "test.ophooks-ext"))))
    (it "unregister-op-hooks-for-owner! dynamically tears down ALL of an owner's hooks"
        (extension/register-op-hook! {:op :ophtest-o1
                                      :owner :ext/zz
                                      :fn (fn [_ _ _ r]
                                            r)})
        (extension/register-op-hook! {:op :ophtest-o2
                                      :owner :ext/zz
                                      :fn (fn [_ _ _ r]
                                            r)})
        (extension/unregister-op-hooks-for-owner! :ext/zz)
        (expect (nil? (get (deref @#'extension/op-hooks) :ophtest-o1)))
        (expect (nil? (get (deref @#'extension/op-hooks) :ophtest-o2))))))

(defdescribe reload-hooks-test
             (it "runs every hook and isolates failures per id"
                 (let [ran (atom [])]
                   (extension/register-reload-hook! ::rh-ok
                                                    (fn []
                                                      (swap! ran conj :ok)))
                   (extension/register-reload-hook! ::rh-boom
                                                    (fn []
                                                      (throw (ex-info "boom" {}))))
                   (try (let [results (extension/run-reload-hooks!)]
                          (expect (= [:ok] @ran))
                          (expect (true? (:ok? (get results ::rh-ok))))
                          (expect (false? (:ok? (get results ::rh-boom))))
                          (expect (= "boom" (:error (get results ::rh-boom)))))
                        (finally
                          ;; neutralize the fixtures so other tests' /reload paths stay clean
                          (extension/register-reload-hook! ::rh-ok
                                                           (fn []
                                                             nil))
                          (extension/register-reload-hook! ::rh-boom
                                                           (fn []
                                                             nil))))))
             (it "re-registering an id replaces the hook (idempotent)"
                 (let [n (atom 0)]
                   (extension/register-reload-hook! ::rh-idem
                                                    (fn []
                                                      (swap! n inc)))
                   (extension/register-reload-hook! ::rh-idem
                                                    (fn []
                                                      (swap! n + 10)))
                   (try (extension/run-reload-hooks!)
                        (expect (= 10 @n))
                        (finally (extension/register-reload-hook! ::rh-idem
                                                                  (fn []
                                                                    nil)))))))

(def ^:private folded->pos #'com.blockether.vis.internal.extension/folded-kwargs->positional)

(defdescribe
  folded-kwargs->positional-test
  (it "all-kwargs on a shaped tool bind IDENTICALLY to positional"
      ;; a :call {:pos ["id"] :opt-pos ["n"]} tool — the reported defect:
      ;; `tool(id=…, n=…)` folded to one dict must expand to [id n].
      (expect (= ["lint" 100] (folded->pos {:pos ["id"] :opt-pos ["n"]} [{"id" "lint" "n" 100}])))
      (expect (= ["lint"] (folded->pos {:pos ["id"] :opt-pos ["n"]} [{"id" "lint"}])))
      ;; shell/{:opt-pos [cmd] :rest :opt}: leftover keys ride a trailing opts dict.
      (expect (= ["ls"] (folded->pos {:pos ["cmd"] :rest :opt} [{"cmd" "ls"}])))
      (expect (= ["ls" {"cwd" "/tmp"}]
                 (folded->pos {:pos ["cmd"] :rest :opt} [{"cmd" "ls" "cwd" "/tmp"}])))
      ;; an id+cmd shape folds to [id cmd], an op-only call folds to [id {op …}],
      ;; and a {:pos [id text] :rest :opt} shape folds to [id text].
      (expect (= ["x" "sleep 1"]
                 (folded->pos {:pos ["id"] :opt-pos ["cmd"] :rest :opt}
                              [{"id" "x" "cmd" "sleep 1"}])))
      (expect (= ["x" {"op" "stop"}]
                 (folded->pos {:pos ["id"] :opt-pos ["cmd"] :rest :opt} [{"id" "x" "op" "stop"}])))
      (expect (= ["x" "hi"]
                 (folded->pos {:pos ["id" "text"] :rest :opt} [{"id" "x" "text" "hi"}]))))
  (it "leaves everything ambiguous or already-correct untouched"
      ;; already positional — pass through verbatim.
      (expect (= ["lint" 100] (folded->pos {:pos ["id"] :opt-pos ["n"]} ["lint" 100])))
      ;; no :call shape — the generic single-dict tools keep their lone map.
      (expect (= [{"id" "lint"}] (folded->pos nil [{"id" "lint"}])))
      ;; a required :pos key missing — a genuine single-map positional, not kwargs.
      (expect (= [{"id" "x"}] (folded->pos {:pos ["id" "cmd"]} [{"id" "x"}])))
      ;; undeclared key with no :rest — do not silently drop it.
      (expect (= [{"id" "x" "bogus" 1}] (folded->pos {:pos ["id"]} [{"id" "x" "bogus" 1}])))
      ;; function-valued shape — cannot key-spread, leave as-is.
      (expect (= [{"id" "x"}]
                 (folded->pos (fn [_]
                                "src")
                              [{"id" "x"}])))
      ;; empty / multi-arg / non-map — never touched.
      (expect (= [] (folded->pos {:pos ["id"]} [])))
      (expect (= ["x" {"n" 1}] (folded->pos {:pos ["id"]} ["x" {"n" 1}])))))

;; ── ONE context for the extension and the session (issue #104) ────────────────
;;
;; Regression, issue #104: each callback site installed its OWN SUBSET of the
;; context. The turn hooks, the ctx contribution and the prompt/activation
;; callbacks bound the extension identity but NOT the session env, so anything
;; those callbacks raised — `vis.ask` above all — was session-less: the gateway
;; bridge had no session to append the request to and no surface ever drew it.
(defdescribe
  one-extension-context-test
  (it
    "every extension callback site runs inside the session's own context"
    (let
      [seen
       (atom [])

       snap
       (fn [site]
         (swap! seen conj
           {:site site
            :session-id (:session-id extension/*current-environment*)
            :root workspace/*workspace-root*
            :ext (extension/current-extension-id)})
         nil)

       phase-hook
       (fn [phase]
         {:id (keyword "one-context" (name phase))
          :phase phase
          :fn (fn [_ctx]
                (snap phase))})

       ext
       {:ext/name "one-context-probe"
        :ext/hooks [(phase-hook :turn.answer/validate) (phase-hook :turn.iteration/start)]
        :ext/ctx-fn (fn [_env]
                      (snap :ext/ctx-fn)
                      {})
        :ext/prompt-fn (fn [_env]
                         (snap :ext/prompt-fn))
        :ext/activation-fn (fn [_env]
                             (snap :ext/activation-fn)
                             true)}

       root
       (workspace/normalize-root (System/getProperty "user.dir"))

       env
       {:session-id "sid-one-context" :workspace/root root :extensions (atom [ext])}]

      (vis-loop/final-answer-gate-error env 1 [] "an answer" [ext])
      (#'vis-loop/collect-iteration-start-hints env [ext] {:environment env})
      (extension/ctx-contributions env [ext])
      (prompt/active-extensions env)
      (#'prompt/extensions-prompt-block env [ext])
      (expect (= #{:turn.answer/validate :turn.iteration/start :ext/ctx-fn :ext/prompt-fn
                   :ext/activation-fn}
                 (set (map :site @seen))))
      ;; ONE context: same session, same workspace, same identity, every site.
      (expect (every? (fn [s]
                        (= {:session-id "sid-one-context" :root root :ext "one-context-probe"}
                           (dissoc s :site)))
                      @seen))))
  (it "a callback handed no environment of its own keeps the caller's session"
      ;; Regression, issue #104: a caller with nothing to say about the session
      ;; passes an EMPTY env — `(prompt-fn {})`. That used to REPLACE the live
      ;; context with an empty map, so `vis.state` fell through to the
      ;; process-wide DB and `vis.ask` raised a session-less request. An empty
      ;; env is not a context: it inherits the ambient one, and only a real env
      ;; replaces it.
      (extension/with-context
        {:env {:session-id "sid-ambient" :db-info ::probe-db}}
        (expect (= "sid-ambient"
                   (extension/with-context {:env {}}
                                           (:session-id extension/*current-environment*))))
        (expect (= ::probe-db
                   (extension/with-context {:env nil} (:db-info extension/*current-environment*))))
        (expect (= "sid-inner"
                   (extension/with-context {:env {:session-id "sid-inner"}}
                                           (:session-id extension/*current-environment*)))))))

;; ── format_code/lint_code/etc. share invoke-symbol-wrapper with draft-aware cwd ──
;;
;; Turn 10-13's bug (`shell`/`repl*` bypassing `with-context`) lived in
;; `run-native-handler`, the seam ONLY for symbols carrying an explicit
;; `:ext.symbol/handler`. `format_code`/`lint_code` (and every plain
;; `:inject-env? true` engine symbol without a `:handler`) never went through
;; that seam at all: they dispatch through `invoke-symbol-wrapper`, which has
;; wrapped every call in `with-context` since before this investigation
;; started. This pins that the SAME generic path — the one
;; `format_code`/`lint_code` actually use — resolves `workspace/*workspace-root*`
;; from the call's own `:workspace/root`, so a draft session's format/lint
;; already run against the draft, not trunk.
(defn draft-cwd-probe
  "A plain engine-bound symbol shaped like format_code/lint_code: no :handler,
   :inject-env? true so the live env lands as its first argument."
  [env]
  (extension/success {:result {"root" (str workspace/*workspace-root*)
                               "env-root" (str (:workspace/root env))}}))

(defdescribe invoke-symbol-wrapper-workspace-root-test
             (it "binds workspace/*workspace-root* from :workspace/root for a plain engine symbol"
                 (let
                   [draft-root
                    (.getCanonicalPath (java.io.File. "target/test-draft-cwd-probe"))

                    sym
                    (extension/symbol #'draft-cwd-probe {:tag :observation :inject-env? true})

                    ext
                    {:ext/name "test.draft-cwd-probe" :ext/engine {:ext.engine/symbols [sym]}}

                    result
                    (extension/invoke-symbol-wrapper ext sym [] {:workspace/root draft-root})]

                   (expect (= draft-root (get result "root")))
                   (expect (= draft-root (get result "env-root"))))))

;; Phase 2: a function is reached by TYPING its name in a python block, so the
;; registry may not carry a second, schema-shaped door beside that name. This
;; walks the LIVE registry rather than a fixture, so a symbol that quietly
;; re-grows a JSON Schema (or a `native-tool?` flag) fails here.
(defdescribe every-function-is-a-python-name-test
             (it "registers no schema and no native-tool flag on any symbol"
                 ;; Reading the docs table loads the built-in extensions, so the walk
                 ;; below sees the same registry a live session does.
                 (extension/sandbox-symbol-docs)
                 (let
                   [entries
                    (into [] (mapcat extension/ext-symbols) (extension/registered-extensions))

                    dead-keys
                    (into #{}
                          (comp (mapcat keys)
                                (filter (fn [k]
                                          ;; `:ext.symbol/call` STAYS: it maps a kwargs dict onto
                                          ;; positional params for a PYTHON call, and is not a schema.
                                          (contains? #{:ext.symbol/schema :ext.symbol/native-tool?
                                                       :ext.symbol/replay}
                                                     k))))
                          entries)]

                   (expect (< 20 (count entries)))
                   (expect (= #{} dead-keys))))
             (it "binds every doc-bearing symbol under a bare Python name"
                 (let
                   [docs
                    (extension/sandbox-symbol-docs)

                    bound
                    (set (keys (extension/builtin-sandbox-bindings (fn []
                                                                     nil))))]

                   (expect (seq docs))
                   (doseq [sym (keys docs)]
                     ;; A name the model can type: one segment, munged to underscores
                     ;; when it reaches the sandbox (`_shell-stop` -> `_shell_stop`).
                     (expect (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" (name sym)) (str sym)))
                   ;; Every bound door is DOCUMENTED — `doc(name)` is the only contract
                   ;; a function has now, so a binding without one is unreachable prose.
                   ;; (The reverse does not hold: an aliased extension such as MCP's
                   ;; `call` documents itself here and binds as `mcp__call` per turn.)
                   (doseq [sym bound]
                     (expect (contains? (set (keys docs)) sym) (str sym))))))
