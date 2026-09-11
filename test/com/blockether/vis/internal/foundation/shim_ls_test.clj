(ns com.blockether.vis.internal.foundation.shim-ls-test
  "The `ls` sandbox SHIM: the directory listing as an ordinary Python call.

   The listing returns a printable Python string. Host failures remain catchable
   Python exceptions, and the `:fs/access` gate decides which trees may be seen."
  (:require [clojure.string :as string]
            [charred.api :as json]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.extension.core :as extension]
            ;; Registers the shim, exactly as the built-in loader does in production.
            [com.blockether.vis.internal.foundation.shim-ls :as shim-ls]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sandbox
  "A sandbox context with the built-in shims installed and no filesystem
   confinement, so the repo's own tree is the fixture."
  []
  (:python-context (tpc/new-context {})))

(defn- out
  "Stdout of `code` run as ONE driven block."
  [ctx code]
  (let [r (ep/run-python-block ctx code "t1/i1")]
    (expect (nil? (:error r)))
    (:stdout r)))

(defn- with-fs-gate!
  "Install `hook-fn` as the one `:fs/access` gate for `body`, then tear it down.
   The gate lives in the GLOBAL op-hook registry, so the sandbox helper asks the
   same boundary the native readers ask."
  [hook-fn body]
  (try (extension/register-op-hook! {:op :fs/access :owner :ext/test-ls-shim-gate :fn hook-fn})
       (body)
       (finally (extension/unregister-op-hooks-for-owner! :ext/test-ls-shim-gate))))

(defdescribe
  ls-shim-listing-test
  "`ls` answers ONE compact tree string, in the documented order."
  (it "renders a header and one line per entry, directories first then alphabetical"
      (let [ctx
            (sandbox)

            code
            (str "text = ls(\"src/com/blockether/vis/internal/foundation\")\n"
                 "lines = text.split(\"\\n\")\n"
                 "head, body = lines[0], lines[1:]\n" "labels = [l[2:] for l in body]\n"
                 "dirs = [l for l in labels if l.endswith(\"/\")]\n"
                 "files = [l for l in labels if not l.endswith(\"/\")]\n"
                 "print(head.endswith(\"d %sf\" % len(files)) and \"foundation\" in head,\n"
                 "      labels[:len(dirs)] == dirs, dirs == sorted(dirs),\n"
                 "      \"editing/\" in dirs, any(f.startswith(\"core.clj  \") for f in files),\n"
                 "      body[-1].startswith(\"\\u2514 \"), body[0].startswith(\"\\u251c \"))")]

        (expect (= "True True True True True True True\n" (out ctx code)))))
  (it "batches a LIST of paths into one blank-line separated section per directory"
      (let
        [ctx
         (sandbox)

         code
         (str
           "text = ls([\"resources/vis-shims\",\n"
           "           {\"path\": \"src/com/blockether/vis/internal/foundation\", \"depth\": 1}])\n"
           "first, second = text.split(\"\\n\\n\")\n"
           "print(first.split(\"\\n\")[0].split(\"  \")[0].endswith(\"vis-shims\"),\n"
           "      \"ls.py  \" in first, \"editing/\" in second, \"ls.py\" not in second)")]

        (expect (= "True True True True\n" (out ctx code)))))
  (it "indents children at depth, counts them on the directory, and hides dotfiles"
      (let
        [ctx
         (sandbox)

         code
         (str
           "text = ls(\"src/com/blockether/vis/internal/foundation\", depth=2)\n"
           "editing = [l for l in text.split(\"\\n\") if l[2:].startswith(\"editing/ \")][0]\n"
           "nested = [l for l in text.split(\"\\n\") if l[2:4] in (\"\\u251c \", \"\\u2514 \")]\n"
           "hidden = ls(\".\", is_hidden=True)\n" "plain = ls(\".\")\n"
           "print(int(editing.split(\"/ \")[1]) > 0,\n"
           "      any(\"core.clj  \" in l for l in nested),\n"
           "      \".gitignore  \" in hidden, \".gitignore\" in plain,\n"
           "      \"target/\" in plain)")]

        ;; gitignored entries are never listed, on either axis
        (expect (= "True True True False False\n" (out ctx code)))))
  (it "accepts hidden as a keyword alias, overriding is_hidden when supplied"
      (let [ctx
            (sandbox)

            code
            (str "plain = ls('.')\n" "shown = ls('.', is_hidden=True)\n"
                 "print(ls('.', hidden=True) == shown,\n" "      ls('.', hidden=False) == plain,\n"
                 "      ls('.', is_hidden=True, hidden=False) == plain,\n"
                 "      ls(['.'], hidden=True) == shown) \n")]

        (expect (= "True True True True\n" (out ctx code)))))
  ;; Regression: the optional pattern keyword previously raised TypeError.
  (it "filters basenames by optional glob, preserving matching descendants and batch overrides"
      (let [ctx
            (sandbox)

            code
            (str "from pathlib import Path\n"
                 "p = Path('resources/vis-shims')\n" "plain = ls(p)\n"
                 "filtered = ls(p, pattern='l?.py')\n"
                 "batch = ls([p, {'path': p, 'pattern': None}], pattern='l?.py').split('\\n\\n')\n"
                 "tree = ls('resources', depth=2, pattern='ls.py')\n"
                 "print(ls(p, pattern=None) == plain, ls(p, pattern='*') == plain,\n"
                 "      '  0d 1f\\n' in filtered, 'ls.py  ' in filtered,\n"
                 "      batch == [filtered, plain], 'vis-shims/ 1' in tree,\n"
                 "      'ls.py  ' in tree, 'attach.py  ' not in tree,\n"
                 "      ls(p, pattern='__no_match__').endswith('  empty'),\n"
                 "      '.gitignore  ' not in ls('.', pattern='.git*'),\n"
                 "      '.gitignore  ' in ls('.', pattern='.git*', hidden=True))")]

        (expect (= "True True True True True True True True True True True\n" (out ctx code)))))
  (it "rejects malformed patterns and filters case-sensitively within the requested depth"
      (let [ctx
            (sandbox)

            code
            (str "def rejected(pattern):\n"
                 "    try:\n" "        ls('resources', pattern=pattern)\n"
                 "    except Exception as e:\n" "        return type(e).__name__\n"
                 "print(rejected(42), rejected('['),\n"
                 "      ls('resources/vis-shims', pattern='LS.PY').endswith('  empty'),\n"
                 "      ls('resources', depth=1, pattern='ls.py').endswith('  empty'),\n"
                 "      'vis-shims/' in ls('resources', pattern='vis-shims'))")]

        (expect (= "VisToolError VisToolError True True True\n" (out ctx code)))))
  (it "sizes a file in at most four characters"
      (let [ctx
            (sandbox)

            code
            (str "text = ls(\"resources/vis-shims\")\n"
                 "sizes = [l.rsplit(\"  \", 1)[1] for l in text.split(\"\\n\")[1:]]\n"
                 "print(all(len(s) <= 4 for s in sizes),\n"
                 "      all(s[-1].isdigit() or s[-1] in \"kMGT\" for s in sizes))")]

        (expect (= "True True\n" (out ctx code)))))
  (it "documents itself in the sandbox `__vis_docs__` table"
      (let [ctx
            (sandbox)

            code
            (str
              "d = globals()[\"__vis_docs__\"][\"ls\"]\n"
              "print('pattern=None' in d, 'case-sensitive basename glob' in d,\n"
              "      'not a regex' in d, d == ls.__doc__,\n"
              "      all(word in d for word in ['depth', 'is_hidden', 'gitignored', 'STRING']))")]

        (expect (= "True True True True True\n" (out ctx code)))))
  ;; Regression: `ls(Path("src/..."))` raised `TypeError: 'PosixPath' object is not iterable` —
  ;; one path-like argument was iterated as if it were a list of paths.
  (it
    "takes a pathlib.Path alone, in a batch, and inside a per-path spec"
    (let
      [ctx
       (sandbox)

       code
       (str
         "from pathlib import Path\n"
         "root = Path(\"src/com/blockether/vis/internal/foundation\")\n"
         "text = ls(root)\n"
         "batch = ls([Path(\"resources/vis-shims\"), {\"path\": root, \"depth\": 1}]).split(\"\\n\\n\")\n"
         "print(\"core.clj  \" in text, len(batch) == 2,\n"
         "      \"ls.py  \" in batch[0], \"editing/\" in batch[1])")]

      (expect (= "True True True True\n" (out ctx code))))))

(defdescribe
  ls-shim-worker-test
  ;; Regression: the deployed session worker must return listing rows, not a
  ;; transport envelope that the Python shim attempts to parse as JSON.
  (it "lists pathlib paths through the confined session worker"
      (let [directory
            (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))

            roots-fn
            (constantly [directory])]

        (tpc/with-own
          [ctx {} roots-fn
           {:worker? true
            :worker-policy-fn (fn []
                                {:roots-fn roots-fn :net-enabled? false})
            :jail-enabled? true
            :enabled? false}]
          (expect
            (= "True True True True\n"
               (out ctx
                    (str "from pathlib import Path\n"
                         "text = ls(Path('.'), depth=2)\n"
                         "batch = ls([Path('resources/vis-shims'), "
                         "{'path': Path('src/com/blockether/vis/internal/foundation')}])\n"
                         "print('AGENTS.md  ' in text, 'vis-shims/' in text, "
                         "'ls.py  ' in batch and 'core.clj  ' in batch, "
                         "'  0d 1f\\n' in ls(Path('resources/vis-shims'), pattern='ls.py'))"))))))))

(defdescribe
  ls-shim-failure-test
  "A failure is a Python exception, not a sentence to parse."
  (it "maps refusal / missing / file / malformed onto catchable exceptions"
      (let [ctx
            (sandbox)

            code
            (str "def kind(f):\n"
                 "    try:\n" "        f()\n"
                 "        return \"none\"\n" "    except Exception as e:\n"
                 "        return type(e).__name__\n" "print(kind(lambda: ls(\"deps.edn\")),\n"
                 "      kind(lambda: ls(\"src/com/blockether/nope\")),\n"
                 "      kind(lambda: ls([])))")]

        (expect (= "VisToolError VisToolError VisToolError\n" (out ctx code)))))
  ;; Regression, issue #126: an invented address (a filesystem path assembled from
  ;; a language namespace) bounced with nothing but "no such path", so the next
  ;; call guessed again. The recovery has to survive the move into Python.
  (it "names the nearest existing directory in the host error"
      (let [ctx
            (sandbox)

            code
            (str "try:\n"
                 "    ls(\"src/com/blockether/nope\")\n" "except Exception as e:\n"
                 "    m = str(e)\n"
                 "print(\"list `src/com/blockether` first\" in m, \"namespace\" not in m)")]

        (expect (= "True True\n" (out ctx code)))))
  (it
    "raises a host tool error when the `:fs/access` gate refuses the directory"
    (let
      [ctx
       (sandbox)

       hook
       (fn [_env _op ctx]
         (when (string/includes? (str (:path ctx)) "vis-shims")
           "the shim sources are sealed; ask their owner"))

       code
       (str
         "def kind(f):\n" "    try:\n"
         "        f()\n" "        return \"none\"\n"
         "    except Exception as e:\n"
         "        return type(e).__name__ + \":\" + (\"sealed\" in str(e) and \"said\" or \"mute\")\n"
         "print(kind(lambda: ls(\"resources/vis-shims\")),\n"
         "      kind(lambda: ls(\"src/com/blockether/vis/internal/foundation\")))")]

      ;; The gate's own sentence crosses the boundary verbatim, and a directory
      ;; it did not name stays readable.
      (expect (= "VisToolError:said none\n" (with-fs-gate! hook #(out ctx code)))))))

(defdescribe
  ls-shim-host-failure-test
  "Listing failures use the normal host-tool error boundary."
  ;; Regression, issue #126: a guessed path in a batch must name the real parent
  ;; without repeating the Python block, and retain the canonical failure identity.
  (it "reports one compact host error and stops a batch containing a missing directory"
      (tpc/with-own
        [ctx {}]
        (let [code
              (str "print('before')\n"
                   "paths = ['resources/vis-shims', 'resources/__vis_missing_ls__/nested']\n"
                   "ls(paths)\n" "print('after')")

              result
              (ep/run-python-block ctx code "t1/i1")

              {:keys [message data]}
              (:error result)]

          (expect (= "before\n" (:stdout result)))
          (expect
            (=
              "ls: no such directory `resources/__vis_missing_ls__/nested`; list `resources` first."
              message))
          (expect (= :python/host (:phase data)))
          (expect (= :vis/tool-failure (:type data)))
          (expect (= :ls (:symbol data)))
          (expect (= 3 (:line data))))))
  (it "keeps absolute missing-path diagnostics relative to the project"
      (tpc/with-own
        [ctx {}]
        (let [code
              (str "from pathlib import Path\n"
                   "ls(Path('resources/__vis_missing_ls__/nested').absolute())")

              result
              (ep/run-python-block ctx code "t1/i1")]

          (expect
            (=
              "ls: no such directory `resources/__vis_missing_ls__/nested`; list `resources` first."
              (get-in result [:error :message]))))))
  (it "keeps Python argument errors distinct from host failures"
      (tpc/with-own [ctx {}]
                    (let [result
                          (ep/run-python-block ctx "ls('resources', depth='deep')" "t1/i1")

                          {:keys [message data]}
                          (:error result)]

                      (expect (= :python/runtime (:phase data)))
                      (expect (string/starts-with? message "ValueError:"))
                      (expect (string/includes? message "ls('resources', depth='deep')"))))))

(defdescribe
  ls-symbol-content-test
  "Symbol-owned listing presentation."
  (it
    "shows only the final listing on the real shim invocation and preserves the returned tree"
    (let [ctx
          (sandbox)

          events
          (atom [])

          state
          (atom activity/empty-state)

          visible-counts
          (atom [])]

      (binding [extension/*tool-event-sink* (fn [event]
                                              (swap! events conj event)
                                              (swap! state activity/reduce-event event)
                                              (swap! visible-counts conj
                                                (count (:rows (activity/presentation @state)))))]
        (expect (string/includes? (out ctx "print(ls('resources/vis-shims'))") "ls.py")))
      (let [projection
            (activity/presentation @state)

            row
            (first (:rows projection))]

        (expect (= :observation (:classification (first @events))))
        (expect (= "ls" (:operation row)))
        (expect (= "succeeded" (:state row)))
        (expect (= [0 0 1] @visible-counts))
        (expect (= [:start :content :terminal] (mapv :phase @events)))
        (expect (false? (:show-start (first @events))))
        (expect (= "Listed directory" (get-in row [:presentation "headline"])))
        (expect (string/includes? (get-in row [:presentation "summary"]) "resources/vis-shims"))
        (expect (= @state (activity/replay @events)))
        (expect (string/includes? (get-in row [:presentation "summary"]) "files"))
        (expect (= "table" (get-in row [:presentation "content" 0 "type"])))
        (expect (some #(= "ls.py" (first %)) (get-in row [:presentation "content" 0 "rows"]))))))
  (it "bounds batch content and reports omitted entries without losing nested paths"
      (let [entry
            {"name" "same.txt" "path" "root/nested/same.txt" "type" "file" "size" 42}

            directory
            {"path" "root" "entries" (vec (repeat 40 entry))}

            content
            (#'shim-ls/listing-presentation (repeat 5 directory))

            tables
            (mapcat #(get % "content") (get content "sections"))

            hostile
            (apply str (repeat 2000 (char 1)))

            bounded
            (#'shim-ls/listing-presentation
             (repeat 5 {"path" hostile "entries" (repeat 40 (assoc entry "path" hostile))}))]

        (expect (= "Listed 5 directories" (get content "headline")))
        (expect (= 4 (count (get content "sections"))))
        (expect (= [12 12 12 12] (mapv #(count (get % "rows")) tables)))
        (expect (= "nested/same.txt" (get-in content ["sections" 0 "content" 0 "rows" 0 0])))
        (expect (string/includes? (get-in content ["sections" 0 "summary"]) "12 of 40"))
        (expect (string/includes? (get content "summary") "showing 4 of 5 directories"))
        (expect (< (alength (.getBytes ^String (json/write-json-str bounded) "UTF-8")) 32768))))
  (it "makes an empty directory understandable without any preceding start"
      (let [view (#'shim-ls/listing-presentation [{"path" "empty" "entries" []}])]
        (expect (= "Listed directory" (get view "headline")))
        (expect (= "empty · 0 directories · 0 files" (get view "summary")))
        (expect (= [] (get-in view ["content" 0 "rows"])))))
  (it "keeps failure truthful and catchable when Activity is enabled"
      (let [ctx
            (sandbox)

            events
            (atom [])]

        (binding [extension/*tool-event-sink* #(swap! events conj %)]
          (expect (= "caught\n"
                     (out ctx "try:\n    ls('deps.edn')\nexcept Exception:\n    print('caught')"))))
        (let [projection (-> @events
                             activity/replay
                             activity/presentation)]
          (expect (false? (:show-start (first @events))))
          (expect (= [:start :terminal] (mapv :phase @events)))
          (expect (= "List directories" (get-in projection [:rows 0 :presentation "headline"])))
          (expect (= "failed" (:state projection)))
          (expect (= 1 (get-in projection [:counts :failed])))
          (expect (= 1 (count (:rows projection))))))))
