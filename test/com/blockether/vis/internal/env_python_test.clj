(ns com.blockether.vis.internal.env-python-test
  "Vis-owned integration at the embedded-CPython boundary. Exhaustive interpreter
   semantics live in vis-python-runtime; this suite pins only host wiring and surfaces."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as ext]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python-runtime :as python-runtime]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  canonical-python-literal-test
  (it
    "renders boundary data without a CPython printer context"
    (let [data
          (array-map "none" nil "flags" [true false] "text" "a\n\"\\\t" "nested" (array-map "x" 1))]
      (expect
        (=
          "{\"none\": None, \"flags\": [True, False], \"text\": \"a\\n\\\"\\\\\\t\", \"nested\": {\"x\": 1}}"
          (ep/ctx->python-str data)))))
  (it "keeps scalar and temporal representations Python-compatible"
      (expect (= ["nan" "inf" "-inf" "\"1970-01-01T00:00:00Z\""
                  "\"00000000-0000-0000-0000-000000000001\""]
                 (mapv ep/ctx->python-str
                       [##NaN ##Inf ##-Inf (java.util.Date. 0)
                        (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")]))))
  (it "preserves the historical multiline layout at the 100-column boundary"
      (let [rendered (ep/ctx->python-str (array-map "first" (apply str (repeat 100 "x"))
                                                    "second" [1 2]))]
        (expect (str/starts-with? rendered "{\n \"first\": "))
        (expect (str/includes? rendered "\n \"second\": [1, 2]\n}")))))

(defdescribe auto-imported-python-names-test
             (it "makes every advertised Python name available without an import"
                 (let [ctx
                       (tpc/shared)

                       names
                       (ep/ctx->python-str ep/AUTO_IMPORTED_PYTHON_NAMES)

                       result
                       (ep/run-python-block
                         ctx
                         (str "names = " names
                              "\n"
                              "print([name for name in names if not hasattr(builtins, name)])"))]

                   (expect (= "[]\n" (:stdout result))))))


(defdescribe
  block-error-fidelity-test
  "The model must ALWAYS see its own Python error. The caret/position walk
   (`__vis_error_pos__`) reads the raised exception's traceback frames, and a
   fault inside that walk must never replace the error it was describing. It
   used to run INSIDE the guest `except`, so an internal fault REPLACED the real
   exception and every failing block
   surfaced as `INTERNAL engine/tool fault - a host call returned null`. The
   walk now runs on the HOST side, where it is catchable: a broken position
   walk may only cost the caret span, never the message."
  (it "reports the real Python exception for an uncaught error"
      (let [ctx
            (tpc/shared)

            err
            (:error (ep/run-python-block ctx "raise ValueError('probe-real')"))]

        (expect (str/includes? (:message err) "ValueError: probe-real"))
        (expect (= :python/runtime (:phase (:data err))))))
  (it "keeps the real exception when the position walk itself fails"
      (let [ctx
            (tpc/shared)

            _
            (ep/run-python-block ctx
                                 (str "def __vis_err_pos_now__():\n"
                                      "    raise RuntimeError('simulated fault')\n"
                                      "globals()['__vis_err_pos_now__'] = __vis_err_pos_now__\n"))

            err
            (:error (ep/run-python-block ctx "raise ValueError('probe-degraded')"))]

        (expect (str/includes? (:message err) "ValueError: probe-degraded"))
        (expect (not (str/includes? (:message err) "host call returned null"))))))

(defdescribe python-binding-aliases-test
             ;; A host verb is reachable in the sandbox under its canonical Python name
             ;; PLUS the intentional compatibility aliases; a missing alias is a bare
             ;; NameError to the model, which reads as "the tool is gone" and invites a spin.
             (it "exposes grep as grep/find_files/find and unaliased tools as themselves"
                 (expect (= ["grep" "find_files" "find"] (ep/python-binding-names 'grep)))
                 (expect (= ["shell"] (ep/python-binding-names 'shell)))
                 (expect (= ["_shell_logs"] (ep/python-binding-names '_shell-logs))))
             (it "routes every alias to the SAME tool in a live context"
                 (let [ctx
                       (tpc/shared-with! {'grep (fn grep-stub [& args]
                                                  {"op" "grep" "args" (vec args)})})

                       result
                       (ep/run-python-block ctx
                                            (str
                                              "print(grep('a')['op'], grep('a')['args'])\n"
                                              "print(find('x')['op'], find_files('x')['args'])"))]

                   (expect (= "grep ['a']\ngrep ['x']\n" (:stdout result))))))

(defdescribe
  gathered-grep-options-test
  ;; Regression, issue #166: a grep dispatched by `gather` lost its session on
  ;; the pool worker, so `is_regex` never reached the host tool.
  (it "carries is_regex through direct and helper-wrapped gathered calls"
      (let [seen (atom [])]
        (tpc/with-own
          [ctx
           {'grep (fn [& args]
                    (swap! seen conj (vec args))
                    {"op" "grep"})}]
          (let [result (ep/run-python-block ctx
                                            (str "async def call(options):\n"
                                                 "    return (await grep(options))['op']\n"
                                                 "print(await gather("
                                                 "grep({'query': 'a.*b', 'is_regex': True}), "
                                                 "call({'query': 'c.+d', 'is_regex': True})))"))]
            (expect (= "[{'op': 'grep'}, 'grep']\n" (:stdout result)))
            (expect (= #{[{"query" "a.*b" "is_regex" true}] [{"query" "c.+d" "is_regex" true}]}
                       (set @seen))))))))



(defdescribe
  doc-apropos-surface-test
  "The in-sandbox self-discovery surface must stay complete and clean: every
   bound sandbox verb answers `doc(name)` with its own contract, and `apropos('')`
   lists the real verbs while excluding Python builtins and the async-runtime
   `asyncio` shim global. Guards the docstring→doc wiring
   (`extension/sandbox-symbol-docs`) and the `apropos` non-tool filter."
  (let [bind
        (ext/builtin-sandbox-bindings (fn []
                                        nil))

        ctx
        (:python-context (tpc/new-context bind))

        run
        (fn [code]
          (str (:stdout (ep/run-python-block ctx code))))

        ;; Every kernel verb wired into this sandbox — the exact set whose docs
        ;; must be seeded. Keyed by their Python name.
        native
        (for [e
              (ext/registered-extensions)

              s
              (ext/ext-symbols e)

              :when (contains? bind (:ext.symbol/symbol s))]

          (ep/sym->py-name (:ext.symbol/symbol s)))]

    (it "every wired sandbox verb exposes a non-empty doc"
        (expect (seq native)) ;; sanity: we actually tested some
        (let [out (run (str "import json\nbad=[]\n"
                            "for n in ["
                            (str/join ", " (map pr-str native))
                            "]:\n"
                            "    d = doc(n)\n"
                            "    if ('is not a handle' in d) or (not d.strip()):\n"
                            "        bad.append(n)\n" "print('BAD='+json.dumps(bad))"))]
          (expect (re-find #"BAD=\[\]" out))))
    ;; The CALL LINE is the one thing prose cannot supply: which parameters the
    ;; verb takes, which are required, in what order. `doc(name)` renders it from
    ;; the DECLARED signature, so a page can never drift from the contract — and
    ;; no verb may answer with the async trampoline's own `(*a, **k)` instead.
    (it "every wired sandbox verb's page opens with its own call line"
        (let [out (run (str "import json\nbad=[]\n"
                            "for n in [" (str/join ", " (map pr-str native))
                            "]:\n" "    if (n + '(') not in doc(n).splitlines()[2]:\n"
                            "        bad.append(n)\n" "print('NOCALL='+json.dumps(bad))"))]
          (expect (re-find #"NOCALL=\[\]" out))))
    ;; Regression, doc quality: a tool page carried prose alone — nothing named the
    ;; parameters, and an options-dict verb stated its REQUIRED keys nowhere
    ;; `doc(name)` could reach, so the contract was learned from refusals. Both
    ;; lines are rendered from the DECLARATION above the document; putting them
    ;; INSIDE the text instead cost `patch` fourteen ranks on its own ask, because
    ;; a document's first line is one of the three scored fields.
    (it "opens every tool page with its call line and names its required keys once"
        (let [out (run
                    (str
                      "import json\n" "bad = []\n"
                      "keyed = []\n" "for item in apropos():\n"
                      "    if item.type != 'tool': continue\n" "    L = doc(item).splitlines()\n"
                      "    if len(L) < 3 or (item.name + '(') not in L[2]: bad.append(item.name)\n"
                      "    if len(L) > 3 and L[3].startswith('Keys:'): keyed.append(item.name)\n"
                      "print('NOCALL='+json.dumps(bad))\n" "print('KEYED='+str(len(keyed) > 8))\n"
                      "print('REQUIRED='+str('code (REQUIRED)' in doc('repl_eval')))\n"
                      "print('ONCE='+str(doc('patch').count('patch(path, edits)')))"))]
          (expect (re-find #"NOCALL=\[\]" out))
          (expect (str/includes? out "KEYED=True"))
          (expect (str/includes? out "REQUIRED=True"))
          (expect (str/includes? out "ONCE=1"))))
    (it "apropos('') lists real tools but not builtins or the asyncio shim"
        (let [out (run (str "a=[i.name for i in apropos('')]\n"
                            "print('asyncio='+str('asyncio' in a),"
                            "'len='+str('len' in a)," "'ls='+str('ls' in a),"
                            "'grep='+str('grep' in a)," "'patch='+str('patch' in a))"))]
          (expect (re-find #"asyncio=False" out))
          (expect (re-find #"len=False" out))
          (expect (re-find #"ls=True" out))
          ;; `rg`/`find_files` were replaced by `grep` (name + content search in one tool)
          (expect (re-find #"grep=True" out))
          (expect (re-find #"patch=True" out))))
    (it "filters symbol names with regular expressions, never document bodies"
        (let [out (run (str "print('body='+str(len(apropos('REGULAR-EXPRESSION FILTER'))))\n"
                            "print('exact='+apropos(r'^format_code$')[0].name)"))]
          (expect (str/includes? out "body=0"))
          (expect (str/includes? out "exact=format_code"))))
    (it "rejects an invalid regular expression"
        (let [result (ep/run-python-block ctx "apropos('[')")]
          (expect (some? (:error result)))))
    (it "preserves corpus order and never caps matches"
        (let [out (run (str "all_names = [i.name for i in apropos('')]\n"
                            "matched = [i.name for i in apropos('.*')]\n"
                            "print('same='+str(matched == all_names))\n"
                            "print('whole='+str(len(matched) > 25))"))]
          (expect (str/includes? out "same=True"))
          (expect (str/includes? out "whole=True"))))
    (it "answers each hit with its type, name and opening body"
        (let [out (run (str
                         "def local_helper():\n"
                         "    return 1\n" "item = apropos(r'^patch$')[0]\n"
                         "print('fields='+','.join(item._fields))\n" "print('type='+item.type)\n"
                         "print('body='+str(len(item.body) > 0))\n"
                         "print('local='+str('local_helper' in [i.name for i in apropos('')]))"))]
          (expect (str/includes? out "fields=type,name,body"))
          (expect (str/includes? out "type=tool"))
          (expect (str/includes? out "body=True"))
          (expect (str/includes? out "local=False"))))
    (it "answers a shim global as its own symbol"
        (let [out (run (str "hits = apropos(r'^ls$')\n"
                            "print('first=' + hits[0].name)\n" "print('type=' + hits[0].type)\n"
                            "print('item=' + str(len(doc(hits[0])) > 0))\n"
                            "print('same=' + str(doc(hits[0]) == doc('ls')))"))]
          (expect (str/includes? out "first=ls"))
          (expect (str/includes? out "type=function"))
          (expect (str/includes? out "item=True"))
          (expect (str/includes? out "same=True"))))
    (it "answers bare and empty calls with the same complete records"
        (let [out
              (run (str
                     "bare = apropos()\n"
                     "empty = apropos('')\n"
                     "print('same='+str([i.name for i in bare] == [i.name for i in empty]))\n"
                     "print('fields='+','.join(bare[0]._fields))\n"
                     "print('body='+str(len([i for i in bare if i.name == 'grep'][0].body) > 0))"))]
          (expect (str/includes? out "same=True"))
          (expect (str/includes? out "fields=type,name,body"))
          (expect (str/includes? out "body=True"))))
    (it "never turns a bound loop variable into a document"
        (let [out (run (str "x = 3\nday_set = {'a'}\n" "a = [i.name for i in apropos('')]\n"
                            "print('x='+str('x' in a), 'day_set='+str('day_set' in a),"
                            " 'grep='+str('grep' in a))"))]
          (expect (str/includes? out "x=False"))
          (expect (str/includes? out "day_set=False"))
          (expect (str/includes? out "grep=True"))))
    (it "apropos and doc describe their own callable contracts"
        (let [out (run (str "print(doc('apropos'))\n" "print(doc('doc'))"))]
          (expect (str/includes? out "apropos(pattern='')"))
          (expect (str/includes? out "REGULAR-EXPRESSION FILTER over every SYMBOL name"))
          (expect (str/includes? out "doc(target) -> str"))
          (expect (str/includes? out "A skill is one of these documents and nothing more"))))
    (it "gather exposes its concurrency contract through apropos and doc"
        (let [out (run (str "print([i.body for i in apropos('gather') if i.name == 'gather'][0])\n"
                            "print(doc('gather'))"))]
          (expect (str/includes? out "gather(*awaitables) -> list"))
          (expect (str/includes? out "independent deferred tool calls"))
          (expect (str/includes? out "results preserve input order"))
          (expect (str/includes? out "keep dependent calls sequential"))
          (expect (str/includes? out "every failing slot index"))))
    ;; The fold receipt promises nothing it cannot keep: with the native-result
    ;; store gone there is no coordinate to hand back, so the doc says the gist is
    ;; what survives and `read_session()` is the only door to the rest.
    (it "fold_session documents what a fold keeps and what it drops"
        (ep/set-python-binding! ctx 'fold-session identity)
        (let [out (run (str "print([i.body for i in apropos('fold_session')"
                            " if i.name == 'fold_session'][0])\n"
                            "print(doc('fold_session'))"))]
          (expect (str/includes? out "fold_session(key, gist=None) -> str"))
          (expect (str/includes? out "The key is a STRING"))
          (expect (str/includes? out "Folding changes rendering, not storage"))
          (expect (str/includes? out "there is no destructive unfold command"))
          (expect (str/includes? out "its GIST is what survives"))
          (expect (str/includes? out "s = await read_session()"))
          (expect (str/includes? out "['iterations'][...]['blocks']"))
          (expect (str/includes? out "broader newer fold supersedes fully covered"))
          (expect (str/includes? out "Partial overlaps remain separate"))
          ;; No coordinate, no store: the words that used to promise recovery are gone.
          (expect (not (str/includes? out "`ntr`")))
          (expect (not (str/includes? out "# saved:")))))))


(defdescribe persist-session-defs-budget-test
             ;; Regression: `persist-session-defs!` runs on the turn thread right after EVERY
             ;; block and one line BEFORE the turn's outcome is persisted, and "best effort"
             ;; covered only a THROW. A session whose guest snapshot never comes back - one
             ;; interpreter, one GIL, so the snapshot waits on whatever the guest is still
             ;; running - held the turn worker forever: no terminal ever reached the durable
             ;; turn row, the turn stayed `:running` in every listing, and the next cancel was
             ;; refused as `:not-running`, so pressing stop did nothing.
             (it "returns within its budget when the guest snapshot never returns"
                 (tpc/with-own
                   [ctx {}]
                   (let [sid
                         (str "vis-test-defs-budget-" (System/nanoTime))

                         _
                         (ep/run-python-block ctx
                                              (str "import time\n" "def __vis_defs_snapshot__():\n"
                                                   "    time.sleep(30)\n" "    return ''\n"))

                         started
                         (System/currentTimeMillis)

                         out
                         (ep/persist-session-defs! ctx sid)

                         elapsed
                         (- (System/currentTimeMillis) started)]

                     (try (expect (nil? out))
                          (expect
                            (< elapsed 15000)
                            (str "persist-session-defs! held the turn thread for " elapsed "ms"))
                          (finally (.delete (io/file (paths/sandbox-defs-file sid)))))))))
(defdescribe
  sandbox-open-flush-test
  ;; Bytes a block wrote must be on disk before the tool that reads them runs.
  ;; The dropped-handle case the interpreter closes by refcount; a handle the
  ;; block still HOLDS nothing closes, so the runner flushes tracked writable
  ;; handles before every host call and at the end of the block. Regression:
  ;; a block wrote a commit message and `git commit -F` read an empty file.
  (it
    "puts a block's unclosed write on disk, before a tool call and at block end"
    (let [dir
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-open-flush"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          end-file
          (java.io.File. dir "at-end.txt")

          mid-file
          (java.io.File. dir "before-tool.txt")

          held-file
          (java.io.File. dir "held-open.txt")

          ctx
          (:python-context (tpc/new-context {'read_back (fn [& args]
                                                          (let [f (java.io.File. (str (first
                                                                                        args)))]
                                                            (if (.exists f) (slurp f) "")))}
                                            (constantly [dir])))

          at-end
          (ep/run-python-block ctx
                               (str "open(" (pr-str (.getAbsolutePath end-file))
                                    ", 'w').write('flushed-bytes')\n" "print('block-done')"))

          before-tool
          (ep/run-python-block ctx
                               (str "open("
                                    (pr-str (.getAbsolutePath mid-file))
                                    ", 'w').write('mid-bytes')\n"
                                    "print(read_back("
                                    (pr-str (.getAbsolutePath mid-file))
                                    "))"))

          held
          (ep/run-python-block ctx
                               (str "fh = open("
                                    (pr-str (.getAbsolutePath held-file))
                                    ", 'w')\n"
                                    "fh.write('held-bytes')\n"
                                    "print(read_back("
                                    (pr-str (.getAbsolutePath held-file))
                                    "))"))]

      (try (expect (nil? (:error at-end)))
           (expect (= "block-done\n" (:stdout at-end)))
           (expect (= "flushed-bytes" (slurp end-file)))
           ;; A tool that reads a just-written file sees the bytes.
           (expect (nil? (:error before-tool)))
           (expect (= "mid-bytes\n" (:stdout before-tool)))
           ;; A handle the block still HOLDS is the case refcounting does not
           ;; cover: nothing dropped it, so its buffer is unflushed until the
           ;; runner flushes — and the tool that reads the file runs first.
           (expect (nil? (:error held)))
           (expect (= "held-bytes\n" (:stdout held)))
           (finally (run! #(.delete ^java.io.File %) [end-file mid-file held-file dir]))))))


;; A BARE sandbox verb (`_shell_logs`) is called from Python with no schema in
;; front of it — so `doc(name)` is the only place its result keys are stated.
(defdescribe
  bare-verb-docs-test
  "`doc` states the raw-result contract for the private handle transports, which
   no listing advertises."
  (let [bind
        (ext/builtin-sandbox-bindings (fn []
                                        nil))

        ctx
        (:python-context (tpc/new-context bind))

        run
        (fn [code]
          (str (:stdout (ep/run-python-block ctx code))))]

    (it "the private handle transports stay out of every listing"
        (let [out (run (str "names = [i.name for i in apropos('')]\n"
                            "print('hidden='+str('_shell_logs' in names))\n"
                            "print('shell='+str('shell' in names))"))]
          (expect (str/includes? out "hidden=False"))
          (expect (str/includes? out "shell=True"))))
    (it "the handle verbs carry their raw-result contract in doc"
        (let [out (run (str "print('LOGS<'+doc('_shell_logs')+'>')\n"
                            "print('STOP<'+doc('_shell_stop')+'>')"))]
          ;; ONE shell result shape: `logs` fills `out` like a foreground run does,
          ;; and `stop` answers the same keys — never a stage-scoped subset.
          (expect (str/includes? out "The same shell result shape as every other stage"))
          (expect (str/includes? out "`out` is the window this read returned"))
          (expect (str/includes? out "(`stage` \"stop\"): `status` \"stopped\", `exit`."))))))

;; ONE corpus, two verbs. `apropos` filters every reachable symbol NAME with a
;; regular expression, and `doc` retrieves one of those documents whole.
(defdescribe
  discovery-is-two-verbs-test
  "`apropos(pattern)` filters names, `doc(target)` returns one document, and bare
   `doc()` prints the curated index rather than dumping the corpus."
  (let [bind
        (ext/builtin-sandbox-bindings (fn []
                                        nil))

        ctx
        (:python-context (tpc/new-context bind))

        run
        (fn [code]
          (str (:stdout (ep/run-python-block ctx code))))]

    (it "matches document names rather than page bodies"
        (let [out (run (str "body = apropos('lingua')\n" "exact = apropos(r'^python-sandbox$')\n"
                            "print('body='+str(len(body)))\n" "print('exact='+exact[0].name)"))]
          (expect (str/includes? out "body=0"))
          (expect (str/includes? out "exact=python-sandbox"))))
    (it "doc retrieves a documentation page by slug, forgiving case and `.md`"
        (let [out (run (str "a = doc('gateway')\n"
                            "b = doc('Gateway.MD')\n" "print('same='+str(a == b))\n"
                            "print('head='+a.splitlines()[0])\n"
                            "print('body='+str('pairing' in a.lower()))"))]
          (expect (str/includes? out "same=True"))
          (expect (str/includes? out "head=# gateway"))
          (expect (str/includes? out "body=True"))))
    ;; A skill is a document like any other: `doc` prints the complete source and
    ;; there is no verb to call, so the entry carries no call line at all.
    (it "doc returns a skill whole, with no verb to invoke and no session effect"
        (try (doc-corpus/register-source!
               ::whole-skill
               (fn []
                 [{:name "fixture-skill"
                   :kind "skill"
                   :text "Fixture skill

Follow every fixture step without truncation."}]))
             (let [out
                   (run
                     (str
                       "d = doc('fixture-skill')\n" "print('call='+str('skill(' in d))\n"
                       "print('whole='+str('Follow every fixture step without truncation.' in d))\n"
                       "print('bound='+str('skill' in globals()))"))]
               (expect (str/includes? out "call=False"))
               (expect (str/includes? out "whole=True"))
               (expect (str/includes? out "bound=False")))
             (finally (doc-corpus/register-source! ::whole-skill (constantly [])))))
    (it "bare doc() is the curated index, not the corpus"
        (let [out (run (str "idx = doc()\n"
                            "print('curated='+str(idx.count(' — ') < len(apropos(''))))\n"
                            "print('grep='+str('grep — ' in idx))\n"
                            "print('points='+str('apropos(pattern)' in idx))"))]
          (expect (str/includes? out "curated=True"))
          (expect (str/includes? out "grep=True"))
          (expect (str/includes? out "points=True"))))
    (it "a miss points to exact retrieval and regular-expression discovery"
        (let [out (run "print(doc('gatewa'))")]
          (expect (str/includes? out "gatewa"))
          (expect (str/includes? out "is not a handle"))
          (expect (str/includes? out "apropos(pattern)"))))))

;; Regression: a documentation slug must never shadow a bound function — the
;; corpus is seeded with `setdefault`, so the callable contract wins its name.
(defdescribe a-page-never-shadows-a-function-test
             (it "keeps the function's own contract when a document claims its name"
                 (try (doc-corpus/register-source! ::collision
                                                   (fn []
                                                     [{:name "ls" :text "PAGE THAT MUST LOSE"}]))
                      (tpc/with-own [ctx
                                     (ext/builtin-sandbox-bindings (fn []
                                                                     nil))]
                                    (let [out (str (:stdout
                                                     (ep/run-python-block ctx "print(doc('ls'))")))]
                                      (expect (not (str/includes? out "PAGE THAT MUST LOSE")))
                                      (expect (str/includes? out "ls"))))
                      (finally (doc-corpus/register-source! ::collision (constantly []))))))

;; A context that reads documents charges every session for a corpus the great
;; majority never open, and freezes a copy into a context that outlives every
;; `/reload`. So a shim's text, its call form and its kind are PULLED on the
;; `apropos`/`doc` that asks, and building the context must read nothing at all.
(defdescribe building-a-context-reads-no-documents-test
             (it "reads the corpus on the first ask, never while building the context"
                 (let [reads
                       (atom 0)

                       whole
                       doc-corpus/entries]

                   (with-redefs [doc-corpus/entries (fn []
                                                      (swap! reads inc)
                                                      (whole))]
                     (tpc/with-own
                       [ctx
                        (ext/builtin-sandbox-bindings (fn []
                                                        nil))]
                       (expect (zero? @reads) "building a context read the document corpus")
                       (let [out (str (:stdout (ep/run-python-block ctx "print(doc('ls'))")))]
                         (expect (pos? @reads))
                         (expect (str/includes? out "ls("))))))))
(comment
 ;; Dynamic skill and MCP sources are read at call time because the Python
 ;; context outlives reloads of those sources.
)
(defdescribe
  the-document-corpus-is-read-live-test
  (it "answers a document that appeared after the context was built, and drops it when it goes"
      (tpc/with-own
        [ctx
         (ext/builtin-sandbox-bindings (fn []
                                         nil))]
        (let [read-it (fn []
                        (str (:stdout (ep/run-python-block ctx "print(doc('freight-planning'))"))))]
          (expect (str/includes? (read-it) "is not a handle"))
          (try (doc-corpus/register-source! ::late
                                            (fn []
                                              [{:name "freight-planning"
                                                :kind "skill"
                                                :text "Routes crates of tangerines by rail."}]))
               (let [out (read-it)
                     hits (str (:stdout
                                 (ep/run-python-block
                                   ctx
                                   (str "print('found='+str('freight-planning' in "
                                        "[i.name for i in apropos(r'^freight-planning$')]))"))))]

                 (expect (str/includes? out "# freight-planning"))
                 (expect (str/includes? out "tangerines by rail"))
                 (expect (str/includes? hits "found=True")))
               (finally (doc-corpus/register-source! ::late (constantly []))))
          (expect (str/includes? (read-it) "is not a handle"))))))

(defdescribe tool-introspection-test
             ;; Every bound tool was a bare `(*a, **k)` trampoline with an empty docstring,
             ;; so `help(tool)` and `inspect.signature(tool)` showed nothing of the
             ;; contract the host declares for it.
             (it "carries the host doc and the declared parameters onto the bound callable"
                 (let [ctx
                       (tpc/shared-with! {'meta-probe (fn [& args]
                                                        (str "called:" (count args)))})

                       _
                       (ep/set-python-binding-doc!
                         ctx
                         'meta-probe
                         "meta_probe(language=None, **kwargs) -> str. The declared contract.")

                       _
                       (ep/set-python-binding-signature! ctx 'meta-probe "language=None, **kwargs")

                       result
                       (ep/run-python-block ctx
                                            (str "import inspect\n"
                                                 "print(str(inspect.signature(meta_probe)))\n"
                                                 "print(meta_probe.__doc__.split('.')[0])\n"
                                                 "print(meta_probe.__name__)\n"))]

                   (expect (= (str "(language=None, **kwargs)\n"
                                   "meta_probe(language=None, **kwargs) -> str\n"
                                   "meta_probe\n")
                              (:stdout result)))))
             (it "keeps accepting the call shapes the reported signature does not name"
                 (let [ctx
                       (tpc/shared-with! {'meta-probe (fn [& args]
                                                        (str "called:" (count args)))})

                       _
                       (ep/set-python-binding-signature! ctx 'meta-probe "language=None, **kwargs")

                       result
                       (ep/run-python-block ctx
                                            (str "kw = meta_probe(language=\"python\", extra=1)\n"
                                                 "mapped = meta_probe({\"language\": \"python\"})\n"
                                                 "print(kw, mapped)\n"))]

                   (expect (= "called:1 called:1\n" (:stdout result))))))




(defdescribe
  defs-verb-test
  ;; Listing your own helpers meant writing a globals()/co_filename comprehension
  ;; by hand every time, and reading one back meant remembering `inspect`.
  (it "lists the session's own functions, and refuses a name it never defined"
      (tpc/with-own [ctx {}]
                    (let [empty-out
                          (:stdout (ep/run-python-block ctx "print(defs())"))

                          _
                          (ep/run-python-block
                            ctx
                            "from json import dumps\ndef widen(a, b=2):\n    return a * b\n")

                          listed
                          (:stdout (ep/run-python-block ctx "print(defs())"))

                          source
                          (:stdout (ep/run-python-block ctx "print(defs(\"widen\"))"))

                          missing
                          (:stdout (ep/run-python-block ctx
                                                        (str "try:\n" "    defs(\"nope\")\n"
                                                             "except NameError as exc:\n"
                                                             "    print(\"refused:\", exc)\n")))]

                      (expect (str/includes? empty-out "no functions defined by this session yet"))
                      (expect (str/includes? listed "widen(a, b=2)"))
                      ;; An IMPORTED function is not this session's definition.
                      (expect (not (str/includes? listed "dumps")))
                      (expect (str/includes? source "def widen(a, b=2):"))
                      (expect (str/includes? missing "refused:")))))
  ;; A helper the session wrote is a DOCUMENT for `defs()` and `doc(name)`: the
  ;; docstring is what the listing previews and what the page prints. `apropos`
  ;; deliberately omits session definitions; `defs()` is their catalogue.
  (it
    "reads a helper's docstring as its gist and its page, and keeps it out of apropos"
    (tpc/with-own
      [ctx {}]
      (let
        [_
         (ep/run-python-block
           ctx
           "def kebab_to_snake(text):\n    \"\"\"Rewrite a kebab-case identifier as snake_case.\n\n    Splits on the hyphen the way the wire keys do, so a wire name and an\n    engine keyword round-trip.\n    \"\"\"\n    return text.replace('-', '_')\n\ndef quiet(x):\n    return x\n")

         listed
         (:stdout (ep/run-python-block ctx "print(defs())"))

         page
         (:stdout (ep/run-python-block ctx "print(doc('kebab_to_snake'))"))

         bare
         (:stdout (ep/run-python-block ctx "print(doc('quiet'))"))

         found
         (:stdout
           (ep/run-python-block
             ctx
             "hits = apropos('rewrite a kebab-case identifier')\nprint('found=' + str(any(i.name == 'kebab_to_snake' for i in hits)))\nnames = [i.name for i in apropos('')]\nprint('listed=' + str('kebab_to_snake' in names or 'quiet' in names))\nprint('page=' + str('Splits on the hyphen' in doc('kebab_to_snake')))"))]

        ;; The listing previews the first line, and counts what is still missing.
        (expect (str/includes? listed "Rewrite a kebab-case identifier as snake_case."))
        (expect (str/includes? listed "1 has no docstring"))
        ;; The page is the call line and the WHOLE docstring, like any tool's.
        (expect (str/includes? page "kebab_to_snake(text)"))
        (expect (str/includes? page "Splits on the hyphen"))
        ;; An undocumented helper has no page, so the page says what would make one.
        (expect (str/includes? bare "quiet(x)"))
        (expect (str/includes? bare "carries no docstring"))
        ;; Its own words never rank it: a session's def is not part of the searchable surface.
        (expect (str/includes? found "found=False"))
        (expect (str/includes? found "listed=False"))
        ;; It is still readable by name — `defs()` is the catalogue that addresses it.
        (expect (str/includes? found "page=True"))))))


(defdescribe
  ensure-interpreter-second-caller-test
  (it "makes a caller that arrives mid-start WAIT for the interpreter"
      ;; The flag used to be set before the start finished, so the second caller
      ;; returned to an interpreter still inside `Py_Initialize` and confined it —
      ;; the audit hook then refused the interpreter's OWN startup (`getpath`
      ;; raising OSError) and every session after it failed in `vispython_exec`.
      ;; Measured on a gateway prewarming its api and tui sessions together.
      (let [entered
            (java.util.concurrent.CountDownLatch. 1)

            release
            (java.util.concurrent.CountDownLatch. 1)

            second-returned
            (java.util.concurrent.CountDownLatch. 1)

            flag
            #'ep/interpreter-started

            was
            @@flag]

        (with-redefs [python-runtime/ensure-library!
                      (fn []
                        nil)

                      runtime/initialize!
                      (fn [_]
                        (.countDown entered)
                        (.await release)
                        nil)

                      runtime/logs!
                      (fn [_]
                        nil)]

          (reset! @flag false)
          (try (let [starter (future (ep/ensure-interpreter!))]
                 (.await entered)
                 (future (ep/ensure-interpreter!) (.countDown second-returned))
                 (expect (false?
                           (.await second-returned 300 java.util.concurrent.TimeUnit/MILLISECONDS))
                         "a second caller returned while the interpreter was still starting")
                 (.countDown release)
                 @starter
                 (expect (.await second-returned 10 java.util.concurrent.TimeUnit/SECONDS)))
               (finally (.countDown release) (reset! @flag was)))))))

(defdescribe
  jail-decides-confinement-test
  ;; ONE switch, and it means what it says. With a jail the guest is confined to
  ;; the session's roots; without one nothing is confined — because a session
  ;; with no jail already reaches the whole machine through `shell`, and a
  ;; Python-only boundary standing beside an open shell is theatre. A session
  ;; that wants a boundary without a jail asks for one per call, with
  ;; `jailed_shell`.
  (it "confines the guest to the session's roots when the session has a jail"
      (tpc/with-own [ctx {} (constantly [(System/getProperty "user.dir")]) {:jail-enabled? true}]
                    (let [answer (ep/run-python-block ctx "print(open('/etc/hosts').read()[:1])")]
                      (expect (some? (:error answer)))
                      (expect (str/includes? (str (get-in answer [:error :message]))
                                             "outside approved filesystem roots")))))
  (it "leaves the guest unconfined when the session has no jail"
      (tpc/with-own [ctx {} (constantly [(System/getProperty "user.dir")]) {:jail-enabled? false}]
                    (let [answer (ep/run-python-block ctx
                                                      "print(len(open('/etc/hosts').read()) > 0)")]
                      (expect (nil? (:error answer)))
                      (expect (str/includes? (str (:stdout answer)) "True"))))))
