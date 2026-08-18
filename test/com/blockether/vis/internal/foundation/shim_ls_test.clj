(ns com.blockether.vis.internal.foundation.shim-ls-test
  "The `ls` sandbox SHIM: the directory listing as an ordinary Python call.

   What these tests hold down is the whole reason the listing left the tool
   layer: it is called like a function, it answers Python DATA (a list of dicts,
   not an envelope to unwrap), every failure is a Python exception a caller can
   catch, and the `:fs/access` gate still decides which trees may be seen."
  (:require [clojure.string :as string]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            ;; Registers the shim, exactly as the built-in loader does in production.
            [com.blockether.vis.internal.foundation.shim-ls]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- sandbox
  "A sandbox context with the built-in shims installed and no filesystem
   confinement, so the repo's own tree is the fixture."
  ^Context []
  (:python-context (ep/create-python-context {})))

(defn- out
  "Stdout of `code` run as ONE driven block."
  [^Context ctx code]
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
  (it "sizes a file in at most four characters"
      (let [ctx
            (sandbox)

            code
            (str "text = ls(\"resources/vis-shims\")\n"
                 "sizes = [l.rsplit(\"  \", 1)[1] for l in text.split(\"\\n\")[1:]]\n"
                 "print(all(len(s) <= 4 for s in sizes),\n"
                 "      all(s[-1].isdigit() or s[-1] in \"kMGT\" for s in sizes))")]

        (expect (= "True True\n" (out ctx code)))))
  (it
    "documents itself in the sandbox `__vis_docs__` table"
    (let [ctx
          (sandbox)

          code
          (str "d = globals()[\"__vis_docs__\"][\"ls\"]\n"
               "print(\"depth\" in d, \"is_hidden\" in d, \"gitignored\" in d, \"STRING\" in d)")]

      (expect (= "True True True True\n" (out ctx code)))))
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

        (expect (= "NotADirectoryError FileNotFoundError ValueError\n" (out ctx code)))))
  ;; Regression, issue #126: an invented address (a filesystem path assembled from
  ;; a language namespace) bounced with nothing but "no such path", so the next
  ;; call guessed again. The recovery has to survive the move into Python.
  (it "names the nearest existing directory in the FileNotFoundError"
      (let [ctx
            (sandbox)

            code
            (str "try:\n"
                 "    ls(\"src/com/blockether/nope\")\n" "except FileNotFoundError as e:\n"
                 "    m = str(e)\n"
                 "print(\"nearest existing directory\" in m, \"namespace\" in m)")]

        (expect (= "True True\n" (out ctx code)))))
  (it
    "raises PermissionError when the `:fs/access` gate refuses the directory"
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
      (expect (= "PermissionError:said none\n" (with-fs-gate! hook #(out ctx code)))))))
