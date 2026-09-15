(ns com.blockether.vis.draft-boundary-test
  "Native regressions for multi-repository draft isolation (#241, #242, #243)."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.internal.workspace.git :as git]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io File]))

(set! *warn-on-reflection* true)

(defn- repository!
  [^File home name]
  (let [dir (doto (io/file home name) .mkdirs)]
    (spit (io/file dir "same.txt") (str name " original\n"))
    (spit (io/file dir ".gitignore") ".vis/\nrun.log\n")
    (doseq [args [["init" "-b" "main"] ["config" "user.name" "Native fixture"]
                  ["config" "user.email" "native@example.com"] ["config" "commit.gpgsign" "false"]
                  ["add" "."] ["commit" "-m" "test: seed native draft fixture"]]]
      (let [result (git/run-git dir args)]
        (when-not (zero? (long (:exit result)))
          (throw (ex-info "Cannot seed native draft repository" result)))))
    dir))

(defn- script
  [^File first-repo ^File second-repo]
  [(str "originals = [Path(" (pr-str (.getCanonicalPath first-repo))
        "), Path(" (pr-str (.getCanonicalPath second-repo))
        ")]\n" "created = draft_create('native-boundary', roots=[project_root_path, second_path])\n"
        "assert created['in_draft'] and len(created['repositories']) == 2, created\n"
        "print('Native create ' + 'verified')")
   (str "assert project_root_path != originals[0] and second_path != originals[1]\n"
        "copies = [project_root_path, second_path]\n" "for index, clone in enumerate(copies):\n"
        "    (clone / 'same.txt').write_text('draft change %s\\n' % index)\n"
        "    (clone / 'created.txt').write_text('created %s\\n' % index)\n"
        "for original in originals:\n" "    try:\n"
        "        (original / 'forbidden.txt').write_text('must not escape')\n"
        "    except PermissionError:\n"
        "        pass\n" "    else:\n"
        "        raise AssertionError('original checkout accepted a write')\n"
        "print('Native aliases and confinement ' + 'verified')")
   (str
     "review = draft_diff()\n"
     "assert review['repository_count'] == 2 and not review['empty'], review\n"
     "assert len(review['checkpoint']) == 2, review\n"
     "assert len({a['filename'] for a in review['attachments']}) == 2\n"
     "for artifact in review['attachments']:\n"
     "    document = json.loads(read_attachment(artifact))\n"
     "    assert 'same.txt' in document['patch'] and 'created.txt' in document['patch'], document\n"
     "    print('NATIVE_DRAFT_DOCUMENT=' + json.dumps(document))\n"
     "print('Native review ' + 'verified')")
   (str "discarded = draft_discard()\n"
        "assert discarded['status'] == 'discarded', discarded\n"
        "print('Native discard ' + 'verified')")
   (str "assert not draft_status()['in_draft']\n"
        "assert project_root_path == originals[0] and second_path == originals[1]\n"
        "print('Native remap restoration ' + 'verified')")])

(defdescribe
  native-multi-repository-draft-boundary-test
  (it
    "remaps two catalog aliases, confines originals and emits canonical repository diffs"
    (let [home (#'native/temp-dir "vis-native-draft-boundary")]
      (try
        (let [first-repo (repository! home "first")
              second-repo (repository! home "second")
              codes (script first-repo second-repo)
              calls (atom 0)
              original-stream @#'native/stream-body
              original-whole @#'native/whole-body
              reply (fn [stream? text]
                      (let [index (dec (long (swap! calls inc)))]
                        (if-let [code (get codes index)]
                          (#'native/python-tool-body code (inc index) stream?)
                          ((if stream? original-stream original-whole) text))))]

          (with-redefs-fn {#'native/stream-body #(reply true %)
                           #'native/whole-body #(reply false %)}
            (fn []
              (let [{:keys [server asked port]} (#'native/start-stub-provider!
                                                 "Draft boundary check complete")]
                (try
                  (#'native/overlay! first-repo port)
                  (spit
                    (io/file first-repo ".vis/config.yml")
                    (str
                      "\ntoggles:\n  draft_backend: worktree\n"
                      "workspace:\n  filesystem:\n"
                      "    - id: first\n      path: "
                      (pr-str (.getCanonicalPath ^File first-repo))
                      "\n      python_name: first_path\n      access: read-write\n      draft: shared\n"
                      "    - id: second\n      path: " (pr-str (.getCanonicalPath ^File
                                                                                  second-repo))
                      "\n      python_name: second_path\n      access: read-write\n      draft: shared\n"
                      "jail:\n  enabled: true\n  filesystem:\n    allow: [first, second]\n")
                    :append
                    true)
                  (let
                    [{:keys [finished? exit output]}
                     (#'native/run-binary
                      first-repo
                      [(.getAbsolutePath ^File (#'native/require-binary))
                       (str "-Duser.home=" (.getAbsolutePath ^File home)) "--db"
                       (.getAbsolutePath (io/file home "sessions")) "--raw"
                       "Run the supplied draft fixture, including its explicit discard, then finish."]
                      240)
                     tools (->> @asked
                                (mapcat #(get (json/read-json (:body %)) "messages"))
                                (filter #(= "tool" (get % "role")))
                                (map #(str (get % "content")))
                                distinct)
                     documents (->> tools
                                    (mapcat str/split-lines)
                                    (filter #(str/starts-with? % "NATIVE_DRAFT_DOCUMENT="))
                                    (map #(subs % (count "NATIVE_DRAFT_DOCUMENT=")))
                                    distinct)]

                    (expect finished? output)
                    (expect (= 0 exit) output)
                    (doseq [marker ["Native create verified"
                                    "Native aliases and confinement verified"
                                    "Native review verified" "Native discard verified"
                                    "Native remap restoration verified"]]
                      (expect (some #(str/includes? % marker) tools) (pr-str tools)))
                    (expect (= 2 (count documents)) (pr-str tools))
                    (doseq [document documents]
                      (expect (= 1 (get (diff/parse! document) "schema_version"))))
                    (doseq [repo [first-repo second-repo]]
                      (expect (= (str (.getName ^File repo) " original\n")
                                 (slurp (io/file repo "same.txt"))))
                      (expect (not (.exists (io/file repo "forbidden.txt")))))
                    (expect (> @calls (count codes))))
                  (finally (.stop ^HttpServer server 0)))))))
        (finally (#'native/delete-tree! home))))))
