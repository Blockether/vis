(ns com.blockether.vis.internal.python.door-parity-test
  "One Python verb, three doors, one answer. A verb of `python_execution` and its
   twin in the extension SDK must give the same value, the same printed text and
   the same error. Each row of [[cases]] runs at three doors with no mock: the
   sandbox in the engine process, the sandbox in a worker process, and a real
   extension worker that calls the SDK. The answer of the first door is the
   expected answer of the two others."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.extension-tools-test :as tools-test]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private describe-source
  "Python that turns the `out` of a row, or its error, into plain data. Each door
   runs this same text."
  (str
    "import json as _parity_json\n"
    "\n" "\n"
    "def _parity_value(value):\n" "    try:\n"
    "        dumped = _parity_json.dumps(value, sort_keys=True)\n"
    "    except Exception as error:\n"
    "        dumped = f'{type(error).__name__}: {error}'\n" "    return {\n"
    "        'type': type(value).__name__,\n" "        'str': str(value),\n"
    "        'repr': repr(value),\n"
    "        'fields': sorted(str(k) for k in value.keys()) if hasattr(value, 'keys') else None,\n"
    "        'json': dumped,\n" "    }\n"
    "\n" "\n"
    "def _parity_error(error):\n"
    "    return {'error': type(error).__name__, 'message': str(error)}\n"))

(def ^:private picture-source
  "Python doubles of a Matplotlib figure and of a Pillow image, with no package.
   `Picture.save` refuses JPEG for a mode with alpha, as Pillow does."
  (str
    "import struct as _struct\n" "import zlib as _zlib\n"
    "\n" "\n"
    "def _png(w, h):\n"
    "    raw = b''.join(b'\\x00' + b'\\xff\\x00\\x00\\xff' * w for _ in range(h))\n"
    "\n" "    def chunk(tag, body):\n"
    "        c = tag + body\n"
    "        return _struct.pack('>I', len(body)) + c + _struct.pack('>I', _zlib.crc32(c) & 0xFFFFFFFF)\n"
    "\n" "    head = _struct.pack('>IIBBBBB', w, h, 8, 6, 0, 0, 0)\n"
    "    return (b'\\x89PNG\\r\\n\\x1a\\n' + chunk(b'IHDR', head)\n"
    "            + chunk(b'IDAT', _zlib.compress(raw)) + chunk(b'IEND', b''))\n"
    "\n" "\n"
    "class Figure:\n" "    def savefig(self, target, format='png'):\n"
    "        target.write(_png(3, 2))\n" "\n"
    "\n" "class Picture:\n"
    "    def __init__(self, mode='RGB'):\n" "        self.mode = mode\n"
    "        self.size = (3, 2)\n" "\n"
    "    def convert(self, mode):\n" "        return Picture(mode)\n"
    "\n" "    def save(self, target, format='PNG'):\n"
    "        if format == 'JPEG' and self.mode not in ('RGB', 'L'):\n"
    "            raise OSError(f'cannot write mode {self.mode} as JPEG')\n"
    "        if format == 'JPEG':\n"
    "            target.write(b'\\xff\\xd8\\xff\\xe0' + bytes(32))\n"
    "        elif format == 'GIF':\n" "            target.write(b'GIF89a' + bytes(32))\n"
    "        else:\n" "            target.write(_png(3, 2))\n"))

(def ^:private parity-source
  "A Python extension with one tool. `parity.run(code, root)` runs the code of a
   row with the verbs of the SDK and describes its `out`."
  (str
    "from __future__ import annotations\n" "import ast as _parity_ast\n"
    "import blockether.vis.extension as vis\n"
    "def grep(*args, **kwargs):\n    raise AssertionError('extension grep must not run')\n"
    "\n" describe-source
    "\n" picture-source
    "\n" "\n"
    "async def _parity_tool(name, *args, **kwargs):\n"
    "    return vis.tools[name](*args, **kwargs)\n"
    "\n" "\n"
    "async def _parity_settle(value):\n" "    return value\n"
    "\n" "\n"
    "class Parity:\n" "    def run(self, code: str, root: str) -> str:\n"
    "        \"Run one parity row with the verbs of the SDK.\"\n"
    "        scope = {name: getattr(vis, name) for name in dir(vis) if not name.startswith('_')}\n"
    "        scope.update(ROOT=root, Figure=Figure, Picture=Picture, tool=_parity_tool, settle=_parity_settle)\n"
    "        try:\n"
    "            flags = _parity_ast.PyCF_ALLOW_TOP_LEVEL_AWAIT\n"
    "            waiting = eval(compile(code, '<row>', 'exec', flags=flags), scope)\n"
    "            if waiting is not None:\n" "                try:\n"
    "                    waiting.send(None)\n" "                except StopIteration:\n"
    "                    pass\n" "                else:\n"
    "                    raise RuntimeError('the row waits, and a tool of an extension cannot wait')\n"
    "            described = _parity_value(scope['out'])\n"
    "        except Exception as error:\n" "            described = _parity_error(error)\n"
    "        return _parity_json.dumps(described)\n" "\n"
    "\n"
    "vis.register_extension(vis.Extension(name=\"parity\", description=\"Parity fixture\", alias=\"parity\", symbols=[vis.Symbol(Parity(), name=\"parity\")]))\n"))

(def ^:private engine-cases
  "`[id code]` rows that need a file, a figure, a picture or a tool of the session. `code` is Python that sets `out`. It uses only names that each
   door has: the verbs, `ROOT`, `Figure`, `Picture`, `tool` and `settle`. `await
   tool(name, ...)` calls the tool `name` of the session as the door calls it.
   `await settle(value)` waits for a value that the sandbox gives as an awaitable."
  [;; -- attach: each shape of source --
   [:attach-path "out = attach(ROOT + '/notes.txt')"]
   [:attach-path-renamed "out = attach(ROOT + '/notes.txt', 'renamed.md', label='Two\\nlines')"]
   [:attach-figure "out = attach(Figure())"]
   [:attach-figure-named "out = attach(Figure(), 'plot.png', label='A plot')"]
   [:attach-image "out = attach(Picture(), 'crop.png')"]
   [:attach-image-default-name "out = attach(Picture())"]
   [:attach-rgba-jpeg "out = attach(Picture('RGBA'), 'shot.jpg')"]
   [:attach-image-gif "out = attach(Picture(), 'anim.gif')"]
   [:attach-media-type "out = attach(b'fLaC' + bytes(4), 'a.flac', media_type='text/x-note')"]
   [:attach-pathlike "import pathlib\nout = attach(pathlib.Path(ROOT) / 'notes.txt')"]
   ;; -- attach: keyword aliases --
   [:alias-path "out = attach(path=ROOT + '/notes.txt')"]
   [:alias-file "out = attach(file=ROOT + '/notes.txt')"]
   [:alias-src "out = attach(src=ROOT + '/notes.txt')"]
   ;; -- attach: refusals --
   [:attach-missing "out = attach(ROOT + '/absent.txt')"] [:attach-directory "out = attach(ROOT)"]
   [:attach-document "out = attach(b'%PDF-1.4\\n', 'r.pdf')"]
   [:attach-for-model "out = attach(Figure(), 'plot.png', audience='model')"]
   ;; -- attach: a file above the limit stays on disk as a reference --
   [:reference "out = attach(ROOT + '/big.mp4', label='Clip')"]
   [:reference-for-model "out = attach(ROOT + '/big.mp4', audience='model')"]
   [:bytes-above-limit "out = attach(bytes(5000), 'big.bin')"]
   ;; -- the four read verbs --
   [:get-by-filename "attach(b'one\\n', 'n.txt')\nout = get_attachment('n.txt')"]
   [:get-by-id "r = attach(b'one\\n', 'n.txt')\nout = get_attachment(r['id'])"]
   [:get-by-descriptor "r = attach(b'one\\n', 'n.txt')\nout = get_attachment(r)"]
   [:get-version-float "attach(b'one\\n', 'n.txt')\nout = get_attachment('n.txt', 1.0)"]
   [:read-by-filename "attach(b'one\\n', 'n.txt')\nout = read_attachment('n.txt')"]
   [:read-by-descriptor "out = read_attachment(attach(b'one\\n', 'n.txt'))"]
   [:read-by-version
    "attach(b'one\\n', 'n.txt')\nattach(b'two!\\n', 'n.txt')\nout = read_attachment('n.txt', -2)"]
   [:read-version-wrong "attach(b'one\\n', 'n.txt')\nout = read_attachment('n.txt', 'first')"]
   [:read-reference "out = read_attachment(attach(ROOT + '/big.mp4'))"]
   [:show-image "out = show_attachment(attach(Figure(), 'plot.png', audience='user'))"]
   [:show-by-filename
    "attach(Figure(), 'plot.png', audience='user')\nout = show_attachment('plot.png')"]
   [:show-version-wrong "attach(Figure(), 'plot.png')\nout = show_attachment('plot.png', 'first')"]
   [:show-reference "out = show_attachment(attach(ROOT + '/big.mp4'))"]
   [:list-all "attach(b'one\\n', 'n.txt')\nattach(Figure())\nout = {'rows': list_attachments()}"]
   [:list-one-name
    "attach(b'one\\n', 'n.txt')\nattach(b'two!\\n', 'n.txt')\nattach(Figure())\nout = {'rows': list_attachments('n.txt')}"]
   [:list-unknown-name "out = {'rows': list_attachments('absent.txt')}"]
   [:list-empty "out = {'rows': list_attachments()}"]
   ;; -- the session verbs --
   [:read-session
    "s = await settle(read_session())\nout = [type(s).__name__, 'op' in s, s['session']['title'], s['scope']]"]
   [:read-session-unknown
    "s = await settle(read_session('00000000-0000-4000-8000-000000000000'))\nout = [type(s).__name__, s['session'], s['transcript']]"]
   [:list-sessions
    "rows = await settle(list_sessions())\nout = [type(rows).__name__, [row['title'] for row in rows]]"]
   ;; -- a typed result of a tool --
   [:record "out = await tool('svc.reserve', 'x', profile='p')"]
   [:record-fields
    "r = await tool('svc.reserve', 'x')\nout = [r.id, r['profile'], r.browser.engine, type(r.browser).__name__]"]
   [:record-missing-attribute "out = (await tool('svc.reserve', 'x')).missing"]
   [:record-missing-key "out = (await tool('svc.reserve', 'x'))['session']"]
   [:record-index "out = (await tool('svc.reserve', 'x'))[0]"]
   [:record-frozen "r = await tool('svc.reserve', 'x')\nr.id = 'changed'\nout = r"]
   [:record-frozen-key "r = await tool('svc.reserve', 'x')\nr['id'] = 'changed'\nout = r"]
   [:record-sequence
    "p = await tool('svc.page', 'r-x')\nout = [type(p).__name__, [row.engine for row in p], len(p), p[0].engine, dict(p)]"]
   [:record-text "s = await tool('svc.status')\nout = [str(s), repr(s)]"]
   [:record-pickle
    "import pickle\nr = await tool('svc.reserve', 'x')\nc = pickle.loads(pickle.dumps(r))\nout = [type(c).__name__, repr(c), c == r, c.browser.engine]"]
   [:record-copy
    "import copy\nr = await tool('svc.reserve', 'x')\nout = [repr(copy.copy(r)), repr(copy.deepcopy(r)), copy.deepcopy(r) == r]"]
   [:record-as-argument "r = await tool('svc.reserve', 'x')\nout = await tool('svc.name', r)"]
   [:record-nested-argument
    "r = await tool('svc.reserve', 'x')\nout = await tool('svc.kinds', [r.browser])"]
   [:record-plain "out = await tool('svc.plain')"] [:tool-failure "out = await tool('svc.fail')"]
   ;; -- a shell handle --
   [:shell-wait
    "sh = await tool('shell', {'command': 'echo handle'})\nsh.wait(20)\nout = [type(sh).__name__, sh['status'], sh['exit'], sh['out'], sh.logs()['out']]"]
   [:shell-logs-lines
    "sh = await tool('shell', {'command': 'seq 1 20'})\nsh.wait(20)\nout = sh.logs(0, 10)['out']"]
   [:shell-type
    "sh = await tool('shell', {'command': 'read word; echo got-$word', 'stdin': 'tty'})\nsh.type('typed\\n')\nsh.wait(20)\nout = [sh['status'], 'got-typed' in sh['out']]"]
   [:shell-stop
    "sh = await tool('shell', {'command': 'sleep 30'})\nsh.stop()\nout = sh['status'] != 'running'"]
   [:shell-wrong-key "sh = await tool('shell', {'command': 'true'})\nsh.wait(20)\nout = sh['nope']"]
   ;; -- pages of ls and grep --
   [:ls-pages
    "page = await tool('ls', ROOT, limit=1)\nout = [type(page).__name__, str(page), str(next(page, None)), str(page.all()), [str(p) for p in page.pages()]]"]
   [:text-probes
    "r = await tool('svc.reserve', 'x')\ns = await tool('svc.name', r)\nout = [s.get('missing', 'fallback'), list(s.keys())]"]
   [:session-summary
    "s = await settle(read_session())\nout = [s.get('op'), str(s).startswith('read_session '), s.session.title]"]
   [:session-missing-key "s = await settle(read_session())\nout = s['bad_field']"]
   [:sessions-empty
    "rows = await settle(list_sessions('no-such-title-zzzz'))\nout = [type(rows).__name__, len(rows), rows.get('missing', 'fallback')]"]
   [:grep-capped
    "p = await tool('grep', {'query':['BMW'], 'paths':[ROOT + '/paging.txt'], 'limit':1})\nq = next(p)\nout = [p.next_offset, q.next_offset, 'BMW two' in q, str(next(p)) == str(q), 'BMW three' in p.all(), 'stopped after 1' in p.all(max_pages=1)]"]
   [:grep-keywords
    "p = await tool('grep', query=['BMW'], paths=[ROOT + '/paging.txt'], limit=1)\nout = ['BMW two' in next(p), 'BMW three' in p.all()]"]
   [:grep-empty
    "p = await tool('grep', {'query':['no-such-text-zzzz'], 'paths':[ROOT + '/paging.txt'], 'limit':1})\nout = [next(p, None), next(p, None), str(p.all()) == str(p)]"]
   [:grep-pages
    "page = await tool('grep', {'query': ['BMW'], 'paths': [ROOT]})\nout = [type(page).__name__, str(page), str(next(page, None)), str(page.all())]"]])

(def ^:private verb-fixture
  "The contract fixture of the attachment verbs: rows that need bytes only, each
   with the answer of the engine. The SDK test `test_attach_parity.py` runs the
   same rows on the host outside Vis."
  (json/read-json (slurp (io/resource "vis-contract/fixtures/attachment-verbs.json"))))

(def ^:private cases
  "Each row: the rows of the engine, then the rows of [[verb-fixture]]."
  (into engine-cases
        (map (fn [row]
               [(keyword (get row "name")) (str/join "\n" (get row "code"))]))
        (get verb-fixture "cases")))

(defn- indent [code] (str/join "\n" (map #(str "    " %) (str/split-lines code))))

(defn- sandbox-block
  "The block of one row at a sandbox door: the row runs in the block itself."
  [root code]
  (str describe-source
       picture-source
       "import inspect as _parity_inspect\n"
       "async def tool(name, *args, **kwargs):\n"
       "    head, *rest = name.split('.')\n"
       "    target = globals()[head]\n"
       "    for part in rest:\n"
       "        target = getattr(target, part)\n"
       "    result = target(*args, **kwargs)\n"
       "    return await result if _parity_inspect.isawaitable(result) else result\n"
       "async def settle(value):\n"
       "    return await value if _parity_inspect.isawaitable(value) else value\n"
       "ROOT = "
       (pr-str root)
       "\n"
       "try:\n"
       (indent code)
       "\n"
       "    _parity = _parity_value(out)\n" "except Exception as _parity_failure:\n"
       "    _parity = _parity_error(_parity_failure)\n"
       "print('\\nPARITY ' + _parity_json.dumps(_parity))\n"))

(defn- extension-block
  "The block of one row at the extension door: a tool of an extension runs the row."
  [root code]
  (str "print('\\nPARITY ' + await parity.run(" (pr-str code) ", " (pr-str root) "))\n"))

(defn- answer
  "The answer of one block: what the block printed before the marker, and the
   described `out` or error. Ids and the temporary root differ for each call."
  [root outcome]
  (let [stdout
        (str (:stdout outcome))

        [printed described]
        (str/split stdout #"\nPARITY " 2)

        plain
        #(-> (str %)
             (str/replace root "<root>")
             (str/replace #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" "<id>"))]

    (if (and described (nil? (:error outcome)))
      (assoc (update-vals (json/read-json described) #(if (string? %) (plain %) %))
        "printed" (plain printed))
      {"block-error" (plain (:error outcome)) "printed" (plain stdout)})))

(defn- with-doors
  "Run `f` with `run`, a function `(run door code)` that answers one row at one door."
  [f]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-parity-ext" (make-array FileAttribute 0)))

        root
        (.getCanonicalPath (.toFile (Files/createTempDirectory "vis-parity"
                                                               (make-array FileAttribute 0))))

        store
        (ps/db-create-connection! :memory)

        session-id
        (h/store-session! store {:title "Parity"})]

    (try
      (foundation/register!)
      (spit (io/file dir "svc.py") @#'tools-test/svc-source)
      (spit (io/file dir "parity.py") parity-source)
      (spit (io/file root "notes.txt") "BMW sales rose 4% in Q3\n")
      (spit (io/file root "paging.txt") "BMW one\nBMW two\nBMW three\n")
      (with-open [out (java.io.FileOutputStream. (io/file root "big.mp4"))]
        (.write out
                (byte-array
                  (concat [0 0 0 0x18] (.getBytes "ftypmp42" "US-ASCII") (repeat 6000 (byte 0))))))
      (let [result
            (pyx/reload-python-extensions! {:dirs [(str dir)]})

            exts
            (filterv #(#{"svc" "parity"} (:ext/name %)) (extension/registered-extensions))

            active
            (into exts
                  (filter #(= "foundation-core" (:ext/name %)))
                  (extension/registered-extensions))

            env
            {:db-info store
             :session-id session-id
             :workspace {:root root}
             :extensions (atom active)
             :active-extensions (atom active)}

            bindings
            (into (extension/builtin-sandbox-bindings (constantly env))
                  (map #(extension/wrap-extension-thunked % (constantly env)))
                  exts)

            context
            (fn [options]
              (:python-context (tpc/new-context bindings
                                                (fn []
                                                  [root])
                                                options)))

            doors
            {:sandbox [(context {:enabled? false :jail-enabled? true}) sandbox-block]
             :worker [(context {:enabled? false :jail-enabled? true :worker? true}) sandbox-block]
             :extension [(context {:enabled? false :jail-enabled? true}) extension-block]}]

        (expect (= 2 (:loaded result)))
        (try (binding [extension/*current-environment*
                       env

                       mpl-capture/*attachment-limit-bytes*
                       4096]

               (f (fn [door code]
                    (let [[ctx block] (doors door)]
                      (answer root (ep/run-python-block ctx (block root code)))))))
             (finally (doseq [[ctx] (vals doors)]
                        (ep/dispose-python-context! ctx)))))
      (finally (pyx/reload-python-extensions! {:dirs []})
               (ps/db-dispose-connection! store)
               (doseq [file (concat (reverse (file-seq dir)) (reverse (file-seq (io/file root))))]
                 (io/delete-file file true))))))

(def ^:private documented-differences
  "Each difference between a door and the sandbox that stays, with its reason. The
   expected answer of the door is the answer of the sandbox with `:fields` put in,
   or `:answer` as a whole. `:rows` limits a difference to those rows. A row that
   is here is compared, not skipped."
  [{:door :extension
    :fields {"printed" ""}
    :reason (str "`attach` and `show_attachment` print a preview only in the block of "
                 "`python_execution`. A tool of an extension has no channel into the output "
                 "of the block that called it, so the same code prints nothing there.")}
   {:door :extension
    :rows #{:ls-pages}
    :answer {"error" "VisToolError"
             "message" (str "vis.tools: no active tool is named `ls` in this session; pass the "
                            "name python_execution calls, such as `alias_name` or `alias.method`")
             "printed" ""}
    :reason
    "`ls` is a shim of the sandbox, not a tool of the session, so `vis.tools` has no `ls`."}])

(defn- expected-at
  "The answer that `door` must give for row `id`, from the answer of the sandbox."
  [door id sandbox-answer]
  (reduce (fn [answer {:keys [rows fields] :as difference}]
            (cond (not= door (:door difference)) answer
                  (and rows (not (rows id))) answer
                  (contains? difference :answer) (:answer difference)
                  :else (merge answer fields)))
          sandbox-answer
          documented-differences))

(def ^:private fixed-answers
  "The fields of the sandbox answer that some rows must hold. One front runs at the
   three doors, so a change of the front changes each door equally: only a fixed
   value sees it. Of the JSON value, a row of the engine names the keys that it
   checks. Each row of [[verb-fixture]] has such an answer, and holds the whole value."
  (into {:text-probes {"value" ["fallback" []]}
         :session-summary {"value" ["read_session" true "Parity"]}
         :sessions-empty {"value" ["__VisResultList__" 0 "fallback"]}
         :grep-capped {"value" [1 2 true true true true]}
         :grep-keywords {"value" [true true]}
         :grep-empty {"value" [nil nil true]}
         :attach-rgba-jpeg {"json" {"media_type" "image/jpeg" "filename" "shot.jpg"}}
         :attach-image-gif {"json" {"media_type" "image/gif" "filename" "anim.gif"}}
         :attach-figure {"json" {"media_type" "image/png" "filename" "figure.png"}}
         :attach-image-default-name {"json" {"media_type" "image/png" "filename" "image.png"}}
         :attach-path {"json" {"filename" "notes.txt"}}
         :alias-path {"json" {"filename" "notes.txt"}}
         :alias-file {"json" {"filename" "notes.txt"}}
         :alias-src {"json" {"filename" "notes.txt"}}
         :attach-directory {"error" "IsADirectoryError"}}
        (map (fn [row]
               [(keyword (get row "name")) (get row "expect")]))
        (get verb-fixture "cases")))

(defn- fixed-view
  "The fields of `answer` that `expected` names. `value` is the whole JSON value and
   `keys` is each of its keys."
  [expected answer]
  (let [value (some-> (get answer "json")
                      json/read-json)]
    (into {}
          (map (fn [[field wanted]]
                 [field
                  (case field
                    "value"
                    value

                    "json"
                    (select-keys value (keys wanted))

                    "keys"
                    (some-> value
                            keys
                            sort
                            vec)

                    (get answer field))]))
          expected)))

(def ^:private twin-verbs-code
  "A row that sets `out` to each public verb of the SDK that the sandbox also binds."
  (str "import blockether.vis.extension as _sdk\n"
       "out = sorted(name for name in dir(_sdk) if not name.startswith('_')"
       " and callable(getattr(_sdk, name)) and not isinstance(getattr(_sdk, name), type)"
       " and callable(globals().get(name)))"))

(defn- called?
  "True when the code of some row calls `verb`, by its name or through `tool`."
  [verb]
  (let [call (re-pattern (str "(?<![\\w.])" verb "\\(|tool\\('" verb "['.]"))]
    (boolean (some #(re-find call (second %)) cases))))

(defdescribe
  door-parity-test
  (it
    "gives each row the same answer at the worker sandbox and at an extension as in the sandbox"
    (with-doors
      (fn [run]
        (let [twins
              (run :sandbox twin-verbs-code)

              verbs
              (some-> (get twins "json")
                      json/read-json)

              known
              (set (map first cases))

              differences
              (vec (for [[id code]
                         cases

                         :let [sandbox
                               (run :sandbox code)]
                         door
                         [:worker :extension]

                         :let [expected
                               (expected-at door id sandbox)

                               got
                               (run door code)]
                         :when (not= expected got)]

                     {:row id :door door :expected expected :got got}))

              code-of
              (into {} cases)

              fixed
              (vec (for [[id expected]
                         fixed-answers

                         :let [got
                               (fixed-view expected (run :sandbox (code-of id)))]
                         :when (not= expected got)]

                     {:row id :expected expected :got got}))]

          ;; Each public verb of the SDK with a twin in the sandbox has a row.
          (expect (seq verbs) (pr-str twins))
          (expect (= [] (remove called? verbs)))
          (expect (= [] (remove known (mapcat :rows documented-differences))))
          (expect (= [] (remove known (keys fixed-answers))))
          (expect (= [] fixed))
          (expect (= [] differences)))))))
