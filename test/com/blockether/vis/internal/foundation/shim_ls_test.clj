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
  "`ls` answers Python data, in the documented order."
  (it "lists ONE directory as its entries, directories first then alphabetical"
      (let
        [ctx
         (sandbox)

         code
         (str "rows = ls(\"src/com/blockether/vis/internal/foundation\")\n"
              "kinds = [r[\"type\"] for r in rows]\n" "names = [r[\"name\"] for r in rows]\n"
              "dirs = [n for n, k in zip(names, kinds) if k == \"dir\"]\n"
              "files = [n for n, k in zip(names, kinds) if k == \"file\"]\n"
              "print(kinds == sorted(kinds, key=lambda k: 0 if k == \"dir\" else 1),\n"
              "      dirs == sorted(dirs), files == sorted(files),\n"
              "      \"editing\" in names, \"core.clj\" in names,\n"
              "      all(\"size\" in r and \"path\" in r for r in rows))")]

        (expect (= "True True True True True True\n" (out ctx code)))))
  (it "batches a LIST of paths into one row per directory, in request order"
      (let
        [ctx
         (sandbox)

         code
         (str
           "rows = ls([\"resources/vis-shims\",\n"
           "           {\"path\": \"src/com/blockether/vis/internal/foundation\", \"depth\": 1}])\n"
           "print(len(rows), rows[0][\"path\"].endswith(\"vis-shims\"),\n"
           "      any(e[\"name\"] == \"ls.py\" for e in rows[0][\"entries\"]),\n"
           "      any(e[\"name\"] == \"editing\" for e in rows[1][\"entries\"]))")]

        (expect (= "2 True True True\n" (out ctx code)))))
  (it "nests `children` at depth, and hides dotfiles until `is_hidden`"
      (let
        [ctx
         (sandbox)

         code
         (str "rows = ls(\"src/com/blockether/vis/internal/foundation\", depth=2)\n"
              "editing = [r for r in rows if r[\"name\"] == \"editing\"][0]\n"
              "hidden = {r[\"name\"] for r in ls(\".\", is_hidden=True)}\n"
              "plain = {r[\"name\"] for r in ls(\".\")}\n"
              "print(any(c[\"name\"] == \"core.clj\" for c in editing[\"children\"]),\n"
              "      \".gitignore\" in hidden, \".gitignore\" in plain,\n"
              "      \"target\" in plain)")]

        ;; gitignored entries are never listed, on either axis
        (expect (= "True True False False\n" (out ctx code)))))
  (it "documents itself in the sandbox `__vis_docs__` table"
      (let
        [ctx
         (sandbox)

         code
         (str "d = globals()[\"__vis_docs__\"][\"ls\"]\n"
              "print(\"depth\" in d, \"is_hidden\" in d, \"gitignored\" in d)")]

        (expect (= "True True True\n" (out ctx code))))))

(defdescribe
  ls-shim-failure-test
  "A failure is a Python exception, not a sentence to parse."
  (it "maps refusal / missing / file / malformed onto catchable exceptions"
      (let
        [ctx
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
      (let
        [ctx
         (sandbox)

         code
         (str "try:\n"
              "    ls(\"src/com/blockether/nope\")\n" "except FileNotFoundError as e:\n"
              "    m = str(e)\n" "print(\"nearest existing directory\" in m, \"namespace\" in m)")]

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
