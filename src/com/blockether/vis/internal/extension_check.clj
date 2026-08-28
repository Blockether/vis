(ns ^:no-doc com.blockether.vis.internal.extension-check
  "Static check of a Python extension file: does it parse, does it only reach for
   `vis` names that exist, and would the forms it asks for be accepted?

   Running the file would answer all three, but running it is exactly what an
   author cannot afford before a human is watching: an extension's top level may
   shell out, write state or mount a dialog. So nothing here executes the
   extension. The file is PARSED (`vis-python/extension_check.py`, `ast` only),
   the statically knowable arguments of every `vis.ask` and every `vis.live` call
   are reconstructed, and what comes out is judged by CALLING
   [[com.blockether.vis.internal.view/normalize-request]] or
   [[com.blockether.vis.internal.view/normalize-live-view]] -- the engine's
   own normalizers, the seams every running dialog and every mounted view cross,
   so a refusal here is the line the author would have seen in front of the human.

   The `vis` module the checker reads is the REAL one: the bootstrap is evaluated
   with every host callback bound to a refusal (`python-extensions/bind-inert-host!`),
   so `vis.plaintext(...)` builds its dict while `vis.shell(...)` cannot run. The
   two exceptions are `__vis_host_request_input__`, bound to a judge that
   normalizes the request and settles it `undeliverable`, and `__vis_host_live__`,
   bound to a judge that normalizes the view and mounts nothing: a form is checked
   by ASKING for it and a view by OPENING it, and neither reaches a human.

   What cannot be known without running the file (a field list assembled from an
   argument, an f-string title, a comprehension) is COUNTED as skipped, never
   guessed at: a checker that invents values reports problems nobody has."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.view :as view]
            [com.blockether.vis.internal.python-extensions :as px])
  (:import (java.io File)
           (org.graalvm.polyglot Context Value)))

(def ^:private checker-python
  "The checker's Python body, slurped from its classpath resource through the ONE
   shared reader so a resource missing from the native image fails loudly."
  (env/runtime-python-src "vis-python/extension_check.py"))

(def ^:private unasked-answer
  "What [[judge-request]] hands a checked `vis.ask` back: nobody was asked, so the
   only honest settlement is the engine's own word for a request that reached no
   surface."
  (json/write-json-str (view/answer->wire {:is-submitted false :reason "undeliverable"})))

(defn- judge-request
  "The checker's `request_input`: normalize the request the way a real ask does,
   then answer `undeliverable` instead of drawing, publishing or parking
   anything. A bad spec throws here exactly where `vis.ask` throws for an
   extension that runs, and that line is what the checker reports."
  [request-json & _]
  (let [request (json/read-json (str request-json) :key-fn identity)]
    (view/normalize-request (cond-> request
                              (map? request)
                              (dissoc "channel_id" "channel_ids")))
    unasked-answer))

(defn- judge-view
  "The checker's `live`: normalize the view the way a real `vis.live` does, then
   answer the handle it would have answered -- without mounting it, publishing an
   event or opening a record. A node the vocabulary has no name for throws here
   exactly where the engine throws for an extension that runs, and that line is
   what the checker reports."
  [envelope-json & _]
  (let [envelope
        (json/read-json (str envelope-json) :key-fn identity)

        declared
        (get envelope "view")

        view
        (view/normalize-live-view (cond-> declared
                                    (map? declared)
                                    (dissoc "channel_id" "channel_ids")))]

    (wire/json-str {:view-id (:id view) :is-open true :view view})))

(defn- checker-context
  "A context holding the real `vis` module with an inert host, plus the checker."
  ^Context []
  (let [^Context ctx (px/build-context "extension-check")]
    (px/bind-inert-host! ctx
                         {"__vis_host_request_input__" judge-request
                          "__vis_host_live__" judge-view})
    (locking ctx
      (.eval ctx "python" ^String px/bootstrap-python)
      (.eval ctx "python" ^String checker-python))
    ctx))

(defn- problem [kind line column message] {:kind kind :line line :column column :message message})

(defn- ->report
  "The checker's JSON verdict as a Clojure report."
  [path json-str]
  (let [m (json/read-json (str json-str) :key-fn identity)]
    {:path path
     :checked (long (get m "checked" 0))
     :skipped (long (get m "skipped" 0))
     :is-valid (boolean (get m "is_valid"))
     :problems (mapv (fn [p]
                       (problem (get p "kind")
                                (long (get p "line" 0))
                                (long (get p "column" 0))
                                (get p "message")))
                     (get m "problems"))}))

(defn- check-in
  "Check one `source` inside an already-built checker context."
  [^Context ctx source path]
  (locking ctx
    (let [f ^Value (.getMember (.getBindings ctx "python") "vis_check_source")]
      (->report path (.asString ^Value (.execute f (object-array [(str source) (str path)])))))))

(defn- unreadable
  [path message]
  {:path path :checked 0 :skipped 0 :is-valid false :problems [(problem "unreadable" 0 0 message)]})

(defn check-sources
  "Check a seq of `[path source]` pairs in ONE context. Returns one report per
   pair: `{:path :checked :skipped :is-valid :problems [{:kind :line :column
   :message}]}`. Reports are data -- a file with problems is a verdict, never a
   throw, so a run over a directory always reaches the last file."
  [pairs]
  (if-not (seq pairs)
    []
    (let [ctx (checker-context)]
      (try (mapv (fn [[path source]]
                   (check-in ctx source path))
                 pairs)
           (finally (.close ctx))))))

(defn check-source
  "Check one extension `source`, labelled `path`."
  ([source] (check-source source "<source>"))
  ([source path] (first (check-sources [[path source]]))))

(defn check-files
  "Check `files` (anything `io/file` accepts). A file that cannot be read is its
   own `unreadable` problem rather than an aborted run."
  [files]
  (let [read-one
        (fn [f]
          (let [^File file (io/file f)]
            (try [(.getPath file) (slurp file)] (catch Exception e [(.getPath file) e]))))

        pairs
        (mapv read-one files)

        ok
        (filterv (fn [[_ source]]
                   (string? source))
          pairs)

        reports
        (into {} (map (juxt :path identity)) (check-sources ok))]

    (mapv (fn [[path source]]
            (if (string? source)
              (get reports path)
              (unreadable path (ex-message ^Exception source))))
          pairs)))

(defn expand-paths
  "Every file `paths` names: a directory contributes its own `*.py` in name
   order, non-recursively. No paths at all means the extension directories vis
   itself loads, so `vis-agent extension check` with no arguments checks exactly
   what would load next."
  [paths]
  (let [roots (if (seq paths) (mapv io/file paths) (px/default-extension-dirs))]
    (vec (mapcat (fn [^File f]
                   (if (.isDirectory f)
                     (sort (filter (fn [^File c]
                                     (str/ends-with? (.getName c) ".py"))
                                   (or (seq (.listFiles f)) [])))
                     [f]))
                 roots))))

(defn ok? "Did every report come back valid?" [reports] (every? :is-valid reports))

(defn- plural [n word] (str n " " word (when (not= 1 n) "s")))

(defn- report-lines
  [{:keys [path problems checked skipped is-valid]}]
  (into [(str (if is-valid "ok   " "FAIL ")
              path
              "  ("
              (plural checked "form")
              " checked"
              (when (pos? ^long skipped) (str ", " skipped " skipped"))
              ")")]
        (map (fn [{:keys [kind line column message]}]
               (str "  " path ":" line ":" column ": " kind ": " message)))
        problems))

(defn report-text
  "The whole run as the text `vis-agent extension check` prints: one line per
   file, one indented line per problem, and a closing tally."
  [reports]
  (let [total
        (reduce + 0 (map (comp long :checked) reports))

        bad
        (reduce + 0 (map (comp count :problems) reports))]

    (str/join "\n"
              (concat (mapcat report-lines reports)
                      [(str (plural (count reports) "file")
                            ", " (plural total "form")
                            " checked, " (plural bad "problem"))]))))
