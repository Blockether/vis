(ns com.blockether.vis.internal.python.signature-test
  "Registered call shapes inspected and invoked through the real sandbox boundary."
  (:require [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.foundation.shell-log :as shell-log]
            [com.blockether.vis.internal.gateway.resources :as resources]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]))

;; Other suites may deregister foundation after the manifest has initialized it.
;; Restore only the registration this fixture adds, matching the session-read tests.
(set-ns-context! [(around-each [f]
                               (let [registered? (some #(= "foundation-core" (:ext/name %))
                                                       (extension/registered-extensions))]
                                 (when-not registered? (foundation/register!))
                                 (try (f)
                                      (finally (when-not registered?
                                                 (extension/deregister-extension!
                                                   "foundation-core"))))))])

(defdescribe
  registered-signature-test
  ;; #232 follow-up: host options must be inspectable without printing whole docs.
  (it
    "inspects every registered signature in local and worker sandboxes"
    (let [signatures
          (extension/sandbox-symbol-signatures)

          probes
          (into {}
                (map-indexed (fn [i [sym signature]]
                               [(str sym)
                                {"binding" (str "signature_probe_" i) "signature" signature}]))
                (sort-by (comp str key) signatures))

          bindings
          (into {}
                (map (fn [[_ probe]]
                       [(symbol (get probe "binding"))
                        (fn [& _]
                          nil)]))
                probes)]

      (expect (seq signatures))
      (doseq [worker? [false true]]
        (tpc/with-own
          [ctx bindings nil {:worker? worker?}]
          (doseq [[_ probe] probes]
            (ep/set-python-binding-signature! ctx
                                              (symbol (get probe "binding"))
                                              (get probe "signature")))
          (ep/set-python-binding! ctx 'signature_probes probes)
          (let
            [answer
             (ep/run-python-block
               ctx
               (str
                 "import inspect\n"
                 "observed = {}\n" "for name, probe in signature_probes.items():\n"
                 "    fn = globals()[probe['binding']]\n" "    actual = inspect.signature(fn)\n"
                 "    expected = probe['signature'].replace('...', 'Ellipsis')\n"
                 "    assert str(actual) == expected, (name, str(actual), expected)\n"
                 "    observed[name] = actual.parameters\n"
                 "publish = observed['council.publish']\n"
                 "assert publish['kind'].kind is inspect.Parameter.KEYWORD_ONLY\n"
                 "assert publish['kind'].default is inspect.Parameter.empty\n"
                 "assert publish['title'].default is Ellipsis\n"
                 "assert observed['grep']['query'].kind is inspect.Parameter.KEYWORD_ONLY\n"
                 "assert observed['grep']['paths'].default is Ellipsis\n"
                 "assert not observed['main-agent-instructions']\n"
                 "assert list(observed['patch']) == ['path', 'edits']\n"
                 "assert all(p.default is inspect.Parameter.empty for p in observed['patch'].values())\n"
                 "assert not observed['council.subagents']\n"
                 "assert list(observed['council.cancel']) == ['session_id']\n"
                 "assert 'paths' in inspect.signature(ls).parameters\n" "print(len(observed))\n"))]
            (expect (nil? (:error answer)) (pr-str answer))
            (expect (= (str (count signatures) "\n") (:stdout answer)))))))))

(defdescribe
  signature-refresh-test
  ;; #232 follow-up: cached inspection must follow metadata updates, not just reload.
  (it
    "refreshes retained callables while doc and keys restamps preserve the prototype"
    (doseq [worker? [false true]]
      (tpc/with-own
        [ctx
         {'signature_refresh.probe (fn [& _]
                                     nil)} nil {:worker? worker?}]
        (ep/set-python-binding-signature! ctx 'signature_refresh.probe "(value)")
        (let [answer (ep/run-python-block ctx
                                          (str
                                            "import inspect\n" "kept = signature_refresh.probe\n"
                                            "original = kept.__wrapped__\n"
                                            "assert str(inspect.signature(kept)) == '(value)'\n"))]
          (expect (nil? (:error answer)) (pr-str answer)))
        (ep/set-python-binding-signature! ctx 'signature_refresh.probe "(value, *, repeat=...)")
        (let [answer (ep/run-python-block
                       ctx
                       (str "assert kept is signature_refresh.probe\n"
                            "assert kept.__wrapped__ is not original\n"
                            "assert str(inspect.signature(kept)) == '(value, *, repeat=Ellipsis)'\n"
                            "updated = kept.__wrapped__\n"))]
          (expect (nil? (:error answer)) (pr-str answer)))
        (ep/set-python-binding-doc! ctx 'signature_refresh.probe "Inspect a value.")
        (ep/set-python-binding-keys! ctx 'signature_refresh.probe "repeat (optional)")
        (let [answer (ep/run-python-block
                       ctx
                       (str "assert kept.__wrapped__ is updated\n"
                            "assert str(inspect.signature(kept)) == '(value, *, repeat=Ellipsis)'\n"
                            "assert kept.__doc__ == 'Inspect a value.'\n" "print('refreshed')\n"))]
          (expect (nil? (:error answer)) (pr-str answer))
          (expect (= "refreshed\n" (:stdout answer))))))))

(defn- inspect-options
  "Echo a harmless options map through the observed-tool dispatcher."
  [_env options]
  (extension/success {:result options}))

(defn- inspect-positional
  "Echo a harmless positional argument and its options."
  [_env content options]
  (extension/success {:result {"content" content "options" options}}))

(defdescribe
  inspected-call-forms-test
  (it
    "keeps dictionary, positional and keyword invocations despite the canonical signature"
    (let [ext
          {:ext/name "signature-fixture"}

          options
          (extension/symbol #'inspect-options
                            {:symbol 'signature_options
                             :inject-env? true
                             :tag :observation
                             :activity {:headline "Inspect options" :show-start false}
                             :call {:pos ["options"] :rest :always}
                             :params [{:name "query" :required? true} {:name "limit"}]})

          positional
          (extension/symbol #'inspect-positional
                            {:symbol 'signature_positional
                             :inject-env? true
                             :tag :observation
                             :activity {:headline "Inspect arguments" :show-start false}
                             :call {:pos ["content"] :rest :always}
                             :params [{:name "kind" :required? true} {:name "title"}]})

          bindings
          (into {}
                (map (fn [entry]
                       [(:ext.symbol/symbol entry)
                        (fn [& args]
                          (extension/invoke-symbol-wrapper ext entry args {}))]))
                [options positional])]

      (doseq [worker? [false true]]
        (tpc/with-own
          [ctx bindings nil {:worker? worker?}]
          (doseq [entry [options positional]]
            (ep/set-python-binding-signature! ctx
                                              (:ext.symbol/symbol entry)
                                              (extension/symbol-signature entry)))
          (let
            [answer
             (ep/run-python-block
               ctx
               (str
                 "import inspect\n"
                 "sig = inspect.signature(signature_options)\n"
                 "assert sig.parameters['query'].kind is inspect.Parameter.KEYWORD_ONLY\n"
                 "assert sig.parameters['query'].default is inspect.Parameter.empty\n"
                 "assert sig.parameters['limit'].default is Ellipsis\n"
                 "mapped = await signature_options({'query': 'needle', 'limit': 2})\n"
                 "keyword = await signature_options(query='needle', limit=2)\n"
                 "assert dict(mapped) == dict(keyword), (dict(mapped), dict(keyword))\n"
                 "assert mapped['query'] == 'needle' and mapped['limit'] == 2\n"
                 "omitted = await signature_options(query='needle')\n"
                 "assert omitted['query'] == 'needle' and 'limit' not in omitted\n"
                 "sig = inspect.signature(signature_positional)\n"
                 "assert sig.parameters['kind'].default is inspect.Parameter.empty\n"
                 "positional = await signature_positional('body', {'kind': 'observation'})\n"
                 "keyword = await signature_positional(content='body', kind='observation')\n"
                 "mixed = await signature_positional('body', kind='observation')\n"
                 "mapped = await signature_positional({'content': 'body', 'kind': 'observation'})\n"
                 "assert positional == keyword == mixed == mapped\n"
                 "assert positional['content'] == 'body'\n"
                 "assert dict(positional['options']) == {'kind': 'observation'}\n"
                 "print('equivalent')\n"))]
            (expect (nil? (:error answer)) (pr-str answer))
            (expect (= "equivalent\n" (:stdout answer)))))))))

(defdescribe
  typed-extension-signature-test
  ;; #273: Python extension annotations were dropped at the sandbox boundary, so
  ;; `inspect.signature` and `__annotations__` showed untyped parameters.
  (it
    "resolves annotated signature text into inspectable types in local and worker sandboxes"
    (doseq [worker? [false true]]
      (tpc/with-own
        [ctx
         {'typed_probe (fn [& args]
                         (str "called:" (count args)))} nil {:worker? worker?}]
        (ep/set-python-binding-signature!
          ctx
          'typed_probe
          "(name: str, /, *args: int, loud: bool = ..., note: str | None = None, **opts: Any) -> 'Results'")
        (let
          [answer
           (ep/run-python-block
             ctx
             (str
               "import inspect\n"
               "import typing\n" "sig = inspect.signature(typed_probe)\n"
               "assert str(sig) == \"(name: str, /, *args: int, loud: bool = Ellipsis, note: str | None = None, **opts: Any) -> 'Results'\", str(sig)\n"
               "assert sig.parameters['name'].kind is inspect.Parameter.POSITIONAL_ONLY\n"
               "assert sig.parameters['name'].annotation is str\n"
               "assert sig.parameters['loud'].default is Ellipsis\n"
               "assert sig.return_annotation == 'Results'\n" "notes = typed_probe.__annotations__\n"
               "assert notes['name'] is str and notes['args'] is int and notes['loud'] is bool, notes\n"
               "assert notes['note'] == (str | None), notes['note']\n"
               "assert notes['opts'] is typing.Any, notes['opts']\n"
               "assert notes['return'] == 'Results', notes['return']\n"
               "try:\n" "    typing.get_type_hints(typed_probe)\n"
               "except NameError as exc:\n" "    assert 'Results' in str(exc), exc\n"
               "else:\n"
               "    raise AssertionError('unresolved record name must stay a forward reference')\n"
               "class Results: pass\n"
               "hints = typing.get_type_hints(typed_probe, localns={'Results': Results})\n"
               "assert hints['return'] is Results and hints['note'] == typing.Optional[str], hints\n"
               "called = await typed_probe('x', loud=True)\n"
               "assert called == 'called:2', called\n" "print('typed')\n"))]
          (expect (nil? (:error answer)) (pr-str answer))
          (expect (= "typed\n" (:stdout answer))))))))

(defdescribe
  shell-method-signature-test
  (it
    "inspects returned shell methods and uses their named arguments"
    (extension/sandbox-symbol-signatures)
    (binding [workspace/*workspace-root* (workspace/trunk-root)]
      (let [sid (str "signature-shell-" (random-uuid))
            env {:session-id sid}
            ext {:ext/name "foundation-core"}
            bindings (into {}
                           (map (fn [entry]
                                  [(:ext.symbol/symbol entry)
                                   (fn [& args]
                                     (extension/invoke-symbol-wrapper ext entry args env))]))
                           shell/shell-symbols)]

        (try
          (tpc/with-own
            [ctx bindings nil {:worker? true}]
            (let
              [answer
               (ep/run-python-block
                 ctx
                 (str
                   "import inspect\n" "sh = await shell('printf inspection-ready')\n"
                   "try:\n"
                   "    signatures = {name: inspect.signature(getattr(sh, name)) for name in ('logs', 'wait', 'type', 'stop')}\n"
                   "    assert 'offset' in signatures['logs'].parameters\n"
                   "    assert 'lines' in signatures['logs'].parameters\n"
                   "    assert 'seconds' in signatures['wait'].parameters\n"
                   "    assert signatures['type'].parameters['text'].default is inspect.Parameter.empty\n"
                   "    assert 'is_enter' in signatures['type'].parameters\n"
                   "    assert not signatures['stop'].parameters\n"
                   "    assert all('self' not in sig.parameters for sig in signatures.values())\n"
                   "    done = await sh.wait(seconds=10)\n"
                   "    assert done['exit'] == 0\n" "    page = await sh.logs(offset=0)\n"
                   "    assert 'inspection-ready' in page['out']\n"
                   "    print('shell methods inspected')\n"
                   "finally:\n" "    await sh.stop()\n"))]
              (expect (nil? (:error answer)) (pr-str answer))
              (expect (= "shell methods inspected\n" (:stdout answer)))))
          (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid)))))))
