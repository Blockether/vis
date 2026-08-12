(ns com.blockether.vis.native-image-env-capture-test
  "The native binary must read the MACHINE IT RUNS ON, not the machine it was built on.

   `native-image` (via the `graal-build-time` feature) initializes every Clojure
   namespace at BUILD time and snapshots the resulting class state into the image
   heap. A top-level `(def x (System/getProperty \"user.home\"))` is therefore
   evaluated on the BUILDER and frozen: the shipped binary looks for the builder's
   `~/.vis`, and `(def *color-enabled?* (boolean (System/console)))` freezes `false`
   because a build has no controlling terminal.

   Two gates:
     - a STATIC scan of every shipped source file for top-level `def`/`defonce`
       forms that read the environment outside a `delay`/`fn`;
     - BEHAVIORAL checks that the paths and the color decision actually follow the
       process' own `user.home` / `user.dir` when they are asked."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config :as config]
            [lazytest.core :refer [defdescribe expect it]]))

;; =============================================================================
;; A tiny top-level-form reader (no classpath, no tools.reader: the scan must see
;; the SOURCE, including namespaces this test never loads).
;; =============================================================================

(defn- form-end
  "Index just past the balanced form that starts at `start` (a `(` in `text`).
   Skips strings, regex literals, character literals and line comments."
  ^long [^String text ^long start]
  (let [n (.length text)]
    (loop
      [i start
       depth 0]

      (if (>= i n)
        n
        (let [c (.charAt text i)]
          (case c
            \\
            (recur (+ i 2) depth)

            \;
            (recur (long (let [nl (.indexOf text (int \newline) i)]
                           (if (neg? nl) n nl)))
                   depth)

            \"
            (let
              [close (loop [j (inc i)]
                       (cond (>= j n) n
                             (= \\ (.charAt text j)) (recur (+ j 2))
                             (= \" (.charAt text j)) (inc j)
                             :else (recur (inc j))))]
              (recur (long close) depth))

            \(
            (recur (inc i) (inc depth))

            \)
            (if (= 1 depth) (inc i) (recur (inc i) (dec depth)))

            (recur (inc i) depth)))))))

(defn- top-level-forms
  "Every top-level parenthesized form in `text`, as source strings."
  [^String text]
  (let [n (.length text)]
    (loop
      [i (long 0)
       acc []]

      (if (>= i n)
        acc
        (let [c (.charAt text (int i))]
          (cond (= \; c) (let [nl (.indexOf text (int \newline) (int i))]
                           (recur (long (if (neg? nl) n (inc nl))) acc))
                (= \" c) (recur (form-end (str "(" text) i) acc)
                (= \( c) (let [end (form-end text i)]
                           (recur end (conj acc [i (subs text (int i) (min n end))])))
                :else (recur (inc i) acc)))))))

(def ^:private lazy-head
  "Openers whose body is evaluated LATER, so an environment read inside one is
   fine at build time: `delay`, any `fn`, `#()`, `memoize`, `reify`, `proxy`."
  #"\(delay[\s)]|\(fn[\s*\[]|#\(|\(memoize[\s)]|\(reify[\s(]|\(proxy[\s(]")

(defn- strip-lazy
  "Remove every deferred subform, so only what RUNS at namespace load remains."
  [^String form]
  (loop [s form]
    (let [m (re-matcher lazy-head s)]
      (if-not (.find m)
        s
        (let [open (.indexOf s "(" (.start m))]
          (recur (str (subs s 0 open) (subs s (form-end s open)))))))))

(def ^:private env-read
  "Reads that answer differently on the build machine than on the user's.

   `os.*` is exempt: native-image never cross-compiles, so the build platform IS
   the run platform. So is `org.graalvm.nativeimage.imagecode` — reading it AT
   BUILD TIME is exactly how a namespace detects that it is being built.

   The second alternative catches the indirect form: our own runtime-path
   helpers. `(def refresher (make-refresher {:lock-path (str (auth-file) \".lock\")}))`
   reads no property itself, yet it froze the builder's lock path into the
   binary just the same."
  #"\(System/getenv|\(System/console|\(System/getProperty(?!\s+\"(os\.|org\.graalvm\.nativeimage\.imagecode))|\((?:[a-zA-Z0-9.*+!_'?<>=-]+/)?(?:config-dir|state-path|db-path|default-db-spec|log-path|auth-file|state-dir|registry-file)[\s)]")

(defn- source-roots
  []
  (cons (io/file "src")
        (->> (file-seq (io/file "extensions"))
             (filter #(and (.isDirectory ^java.io.File %) (= "src" (.getName ^java.io.File %))))
             sort)))

(defn- clj-files
  []
  (->> (mapcat file-seq (source-roots))
       (filter #(and (.isFile ^java.io.File %) (str/ends-with? (.getName ^java.io.File %) ".clj")))
       sort))

(defn- strip-strings
  "Blank out string literals, so an `@` inside a docstring is not read as a deref.

   A CHARACTER SCAN, never a regex: Java matches `\"(?:\\.|[^\"\\])*\"` by
   recursing once per character of the literal, so one long docstring overflowed
   the stack on a 1 MB-stack JVM (Linux CI) while passing on macOS's larger one."
  [^String form]
  (let
    [n
     (.length form)

     sb
     (StringBuilder. n)]

    (loop [i 0]
      (when (< i n)
        (let [c (.charAt form (int i))]
          (if (= \" c)
            (do (.append sb "\"\"")
                (recur (long (loop [j (inc (long i))]
                               (cond (>= j n) n
                                     (= \\ (.charAt form (int j))) (recur (+ j 2))
                                     (= \" (.charAt form (int j))) (inc j)
                                     :else (recur (inc j)))))))
            (do (.append sb c) (recur (inc (long i))))))))
    (.toString sb)))

(defn- deferred-names
  "Names this file defines as a `delay`/`future`/`promise` — the values whose whole
   point is that they have NOT run yet."
  [^String text]
  (into #{}
        (keep (fn [[_ form]]
                (when (re-find #"\((?:delay|future|promise)[\s)]" form)
                  (second (re-find #"^\(def(?:once)?\s+(?:\^[^\s]+\s+)*([^\s()\[\]{}]+)" form)))))
        (top-level-forms text)))

(defn- forces-deferred?
  "Does this top-level form RUN a deferred value while the namespace loads?
   `(force x)` says so outright; `@x` / `(deref x)` only when `x` is one of this
   file's own deferred defs, because deref of an atom is a different animal."
  [^String form ^String text]
  (let [code (strip-strings form)]
    (boolean (or (re-find #"\(force[\s)]" code)
                 (re-find #"@\((?:delay|future|promise)[\s)]" code)
                 (some (fn [^String nm]
                         (re-find (re-pattern (str "(?:@|\\(deref\\s+)"
                                                   (java.util.regex.Pattern/quote nm)
                                                   "[\\s)]"))
                                  code))
                       (deferred-names text))))))

(defn- scan
  "`{:file :line :form}` for every top-level `def`/`defonce` in the shipped source
   whose load-time code (deferred subforms removed) satisfies `(match? form text)`."
  [match?]
  (for
    [^java.io.File f
     (clj-files)

     :let [text
           (slurp f)]
     [^long offset form]
     (top-level-forms text)

     :when (re-find #"^\(def(once)?[\s^]" form)
     :when (match? (strip-lazy form) text)]

    {:file (.getPath f)
     :line (inc (count (re-seq #"\n" (subs text 0 offset))))
     :form (first (str/split-lines form))}))

(defn- offenders
  "Top-level `def`/`defonce` forms that read the environment eagerly."
  []
  (scan (fn [form _text]
          (re-find env-read form))))

(defn- forcing-offenders
  "Top-level `def`/`defonce` forms that force a deferred value at namespace load."
  []
  (scan forces-deferred?))

;; Regression: the installed native binary read the BUILD machine — it logged to the
;; builder's `~/.vis/vis.log` ("no such file or directory" on the user's box), looked
;; for provider auth JSON and `state.yml` there, and printed the CLI with no color at
;; all because `System/console` is nil during a native-image build.
(defdescribe
  native-image-environment-capture-test
  (it "no shipped namespace freezes the environment into a top-level def"
      (let [found (offenders)]
        (expect (empty? found)
                (str "Top-level def/defonce reading the environment at namespace load. "
                     "native-image folds these on the BUILD machine — use a function "
                     "(paths) or a `delay` (tunables):\n"
                     (str/join "\n"
                               (for [{:keys [file line form]} found]
                                 (str "  " file ":" line "  " form)))))))
  (it "the scanner itself sees an eager read and forgives a deferred one"
      (let
        [eager
         "(def home (System/getProperty \"user.home\"))"

         lazy
         "(def home (delay (System/getProperty \"user.home\")))"]

        (expect (re-find env-read (strip-lazy eager)))
        (expect (nil? (re-find env-read (strip-lazy lazy))))
        (expect
          (= 2 (count (top-level-forms (str eager "\n;; (def x (System/getenv \"A\"))\n" lazy)))))))
  (it "a top-level call to a runtime-path helper counts as a read"
      (let
        [eager
         "(def refresher (make-refresher {:lock-path (str (auth-file) \".lock\")}))"

         lazy
         "(def refresher (delay (make-refresher {:lock-path (str (auth-file) \".lock\")})))"

         qualified
         "(def db (config/default-db-spec))"]

        (expect (re-find env-read (strip-lazy eager)))
        (expect (re-find env-read (strip-lazy qualified)))
        (expect (nil? (re-find env-read (strip-lazy lazy))))))
  ;; Regression, issue #N: every native build after v0.1.32 died with
  ;; "HttpClientFacade found in the image heap". A provider extension held
  ;; `(def DEFAULT_MODELS (or (force live-catalog) SEED_MODELS))` — the delay was
  ;; deferred and then forced right back, so the catalog HTTP call ran during
  ;; <clinit>, which native-image performs at build time.
  (it "no shipped namespace forces a deferred value at namespace load"
      (let [found (forcing-offenders)]
        (expect (empty? found)
                (str "Top-level def/defonce forcing a delay/future/promise at namespace load. "
                     "native-image runs <clinit> on the BUILD machine, so whatever the "
                     "deferred body does (an HTTP call, a file read) happens there and its "
                     "objects land in the image heap:\n"
                     (str/join "\n"
                               (for [{:keys [file line form]} found]
                                 (str "  " file ":" line "  " form)))))))
  (it "the force scanner reads code, not prose, and knows a delay from an atom"
      (let
        [file
         (str "(def live-catalog (delay (fetch!)))\n" "(def active-theme-id (atom :dark))\n")

         eager
         "(def models (or (force live-catalog) SEED))"

         derefed
         "(def models @live-catalog)"

         atom-deref
         "(def default-theme (theme/theme @active-theme-id))"

         lazy
         "(def models (delay (force live-catalog)))"

         prose
         "(def models \"Ask @maintainer for the catalog.\" SEED)"]

        (expect (forces-deferred? (strip-lazy eager) file))
        (expect (forces-deferred? (strip-lazy derefed) file))
        ;; An atom is state, not a deferred computation: reading one at load runs
        ;; nothing the builder could freeze.
        (expect (not (forces-deferred? (strip-lazy atom-deref) file)))
        (expect (not (forces-deferred? (strip-lazy lazy) file)))
        (expect (not (forces-deferred? (strip-lazy prose) file))))))

(defn- with-home
  [home f]
  (let [original (System/getProperty "user.home")]
    (try (System/setProperty "user.home" home)
         (f)
         (finally (System/setProperty "user.home" original)))))

(defdescribe runtime-paths-follow-this-process-test
             (it "config paths resolve against the RUNNING process' home"
                 (with-home "/tmp/vis-home-probe"
                            (fn []
                              (expect (= "/tmp/vis-home-probe/.vis" (config/config-dir)))
                              (expect (= "/tmp/vis-home-probe/.vis/state.yml" (config/state-path)))
                              (expect (= "/tmp/vis-home-probe/.vis/vis.mdb" (config/db-path)))
                              (expect (= {:backend :sqlite :path "/tmp/vis-home-probe/.vis/vis.mdb"}
                                         (config/default-db-spec)))
                              ;; Per-process by construction (`paths/log-file`): the
                              ;; DIRECTORY follows this process' home, the name carries
                              ;; the pid so two vis processes never rotate one file.
                              (expect (= (str "/tmp/vis-home-probe/.vis/logs/vis-"
                                              (.pid (java.lang.ProcessHandle/current))
                                              ".log")
                                         (config/log-path))))))
             (it "dotenv defaults resolve against the working directory when consulted"
                 (expect (= (str (System/getProperty "user.dir") "/.env")
                            (#'config/dotenv-path :cwd "/.env")))
                 (expect (= (str (System/getProperty "user.dir") "/.env.local")
                            (#'config/dotenv-path :cwd "/.env.local")))
                 ;; An explicit binding still wins, and nil still means "no file".
                 (expect (= "/tmp/other.env" (#'config/dotenv-path "/tmp/other.env" "/.env")))
                 (expect (nil? (#'config/dotenv-path nil "/.env")))))

(defdescribe
  color-is-decided-at-runtime-test
  (it "the color toggle carries no build-time snapshot"
      (expect
        (nil? (var-get #'commandline/*color-enabled?*))
        "*color-enabled?* must default to nil (auto-detect per call), never a captured boolean")
      (expect (= (boolean (and (System/console)
                               (str/blank? (System/getenv "NO_COLOR"))
                               (not= "dumb" (System/getenv "TERM"))))
                 (commandline/color-enabled?))))
  (it "an explicit binding still forces the answer"
      (binding [commandline/*color-enabled?* true]
        (expect (true? (commandline/color-enabled?))))
      (binding [commandline/*color-enabled?* false]
        (expect (false? (commandline/color-enabled?))))))
