(ns com.blockether.vis.internal.gateway.runtime
  "Runtime identity, daemon staleness and compatibility diagnostics.

   Canonical protocol numbers, headers, handshake parsing and the pure compatibility
   verdict live in `com.blockether.vis.contract.gateway`. This namespace contributes
   the release/build identity of the running process and the concrete client/server
   adapters that combine it with that contract."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as contract]))


(defn release-version
  "Human release version of this build: the `vis/VERSION` resource written at
   build time from the repo-root VIS_VERSION, verbatim (`0.1.28`), else
   `dev`. Ordered ONLY for staleness ([[newer-release?]]) - never for
   compatibility, which is [[contract/protocol-version]]'s job alone."
  []
  (or (some-> (io/resource "vis/VERSION")
              slurp
              str/trim
              not-empty)
      "dev"))

(defn- version-parts
  "Numeric segments of a human release version, or nil when it carries no order at
   all (`dev`, a git sha, nil). Only the leading dotted digits count and any
   `-rc1`/`+build` suffix is dropped, so a prerelease ranks with the release it
   precedes rather than pretending to be a different number."
  [v]
  (when-let [head (some-> v
                          str
                          str/trim
                          not-empty
                          (str/split #"[-+]")
                          first)]
    (let [segs (str/split head #"\.")]
      (when (every? #(re-matches #"\d+" %) segs) (mapv #(Long/parseLong ^String %) segs)))))

(defn newer-release?
  "True when release `a` is STRICTLY newer than release `b`, compared segment by
   segment as numbers (`0.1.10` > `0.1.9`) with the shorter one zero-padded, so
   `0.2` and `0.2.0` are the same release rather than being ranked by length.

   A version without order - `dev`, a checkout, nil, anything [[version-parts]]
   cannot read - is neither newer nor older. That is deliberate: it makes the
   comparison one-directional and total, so a source build never decides that a
   released daemon is stale and two builds can never bounce each other in turn."
  [a b]
  (boolean (when-let [x (version-parts a)]
             (when-let [y (version-parts b)]
               (let [width (max (count x) (count y))
                     pad (fn [v]
                           (into v (repeat (- width (count v)) 0)))]

                 (pos? (compare (pad x) (pad y))))))))

(defn- resource-file
  "The FILE a classpath resource resolves to, or nil when it lives inside a jar or
   a native image, or is absent."
  [path]
  (try (let [^java.net.URL url (io/resource path)]
         (when (and url (= "file" (.getProtocol url))) (io/file (.toURI url))))
       (catch Throwable _ nil)))

(defn- checkout-root
  "The git checkout this process LOADED its code from: the first ancestor of this
   namespace's own source file that holds a `.git`. nil for anything running out of
   a jar or an image, which carries its identity in `vis/BUILD` instead."
  []
  (when-let [^java.io.File f (resource-file "com/blockether/vis/internal/gateway/runtime.clj")]
    (loop [^java.io.File dir (.getParentFile f)]
      (when dir (if (.exists (io/file dir ".git")) dir (recur (.getParentFile dir)))))))

(defn- git-dir
  "The directory holding `HEAD` for `root`: `.git` itself, or the path a linked
   worktree's `.git` FILE points at."
  [^java.io.File root]
  (let [^java.io.File dot (io/file root ".git")]
    (cond (.isDirectory dot) dot
          (.isFile dot) (let [line (str/trim (slurp dot))]
                          (when (str/starts-with? line "gitdir:")
                            (let [p (str/trim (subs line (count "gitdir:")))
                                  ^java.io.File f (io/file p)
                                  ^java.io.File d (if (.isAbsolute f) f (io/file root p))]

                              (when (.isDirectory d) d))))
          :else nil)))

(defn- ref-dirs
  "`gitdir` plus, for a linked worktree, the COMMON dir where its refs really live."
  [^java.io.File gitdir]
  (let [^java.io.File c (io/file gitdir "commondir")]
    (distinct [gitdir
               (if (.isFile c)
                 (let [p (str/trim (slurp c))
                       ^java.io.File f (io/file p)]

                   (if (.isAbsolute f) f (io/file gitdir p)))
                 gitdir)])))

(defn- ref-sha
  "The commit a ref names, loose file first and then `packed-refs`."
  [dirs ref]
  (or (some (fn [^java.io.File d]
              (let [^java.io.File loose (io/file d ref)]
                (when (.isFile loose) (not-empty (str/trim (slurp loose))))))
            dirs)
      (some (fn [^java.io.File d]
              (let [^java.io.File packed (io/file d "packed-refs")]
                (when (.isFile packed)
                  (some (fn [line]
                          (let [[sha named] (str/split (str/trim line) #"\s+")]
                            (when (= named ref) sha)))
                        (str/split-lines (slurp packed))))))
            dirs)))

(defn- head-sha
  "The commit `HEAD` names, read straight off disk - no `git` process on a path
   every client start pays for."
  [^java.io.File gitdir]
  (let [^java.io.File head (io/file gitdir "HEAD")]
    (when (.isFile head)
      (let [line (str/trim (slurp head))]
        (if (str/starts-with? line "ref:")
          (ref-sha (ref-dirs gitdir) (str/trim (subs line (count "ref:"))))
          (not-empty line))))))

(defn- short-commit
  "A commit in the ONE shape both distributions compare in: twelve hex characters,
   keeping a `-dirty` marker a build stamped onto it. A native image records the full
   sha and a source run reads it off `HEAD` - the same commit must not look like two
   builds because one half wrote more characters. Anything that is not a sha
   (`unknown`) has no identity and answers nil."
  [commit]
  (when-let [c (some-> commit
                       str
                       str/trim
                       not-empty)]
    (let [[sha & marks] (str/split c #"-")]
      (when (re-matches #"[0-9a-f]{7,40}" sha)
        (str/join "-" (cons (subs sha 0 (min 12 (count sha))) marks))))))
(defn- checkout-build-id
  "The identity of a SOURCE run: the short `HEAD` sha of the checkout this process
   loaded its code from.

   The COMMIT is the whole answer. A worktree edited past its index is still that
   commit: the alternative - stamping the newest source mtime beside the sha -
   made one commit look like two builds whenever two runs held different
   classpaths, and charged every cold start a walk over every file in the
   checkout to say so."
  [^java.io.File root]
  (when-let [gitdir (git-dir root)]
    (short-commit (head-sha gitdir))))

(def ^:private build-identity
  (delay (try (let [stamped (some-> (io/resource "vis/BUILD")
                                    slurp
                                    str/trim
                                    not-empty
                                    (str/split #"\s+")
                                    second
                                    not-empty)]
                (or (short-commit stamped)
                    (some-> (checkout-root)
                            checkout-build-id)))
              (catch Throwable _ nil))))

(defn build-id
  "WHICH CODE this process is running, as an opaque identity - never an order, and
   nil when this build cannot say. It answers the question a release version cannot:
   two `dev` runs, or two builds of the same VIS_VERSION, are the same build only
   when they came from the same commit.

   One value across both distributions, because a daemon is replaced by whichever
   client finds it and the two halves need not be the same shape:
     native/uberjar  the commit stamped into `vis/BUILD` at build time
     source (JVM)    the short `HEAD` sha of the checkout it loaded from

   Computed ONCE per process ON PURPOSE: a daemon must keep advertising the build
   it LOADED, not whatever is on disk minutes later, or nothing would ever look
   stale."
  []
  @build-identity)

(defn superseded?
  "True when a peer running `their-version` / `their-build` is running code THIS
   build replaces. The two inputs answer different questions and are consulted in
   that order:

     version  an ORDER ([[newer-release?]]). A strictly newer peer is never
              replaced, so an old client can never downgrade a fresh daemon.
     build    an IDENTITY, and only where the versions carry no order between them
              (`dev` against `dev`, or the same VIS_VERSION built twice). Different
              commit means different code; an unknown build on either side means
              no verdict at all.

   Identity is symmetric where an order is not, so acting on it MUST be bounded by
   the caller - `client/bounce-stale-daemon!` replaces a daemon at most once per
   process, which turns the worst case into one restart instead of two builds
   trading a daemon back and forth."
  [{:keys [our-version their-version our-build their-build]}]
  (cond (newer-release? our-version their-version) true
        (newer-release? their-version our-version) false
        :else (boolean (and our-build their-build (not= our-build their-build)))))

(defn handshake
  "What THIS build advertises on health, status and capabilities responses."
  []
  {:protocol contract/protocol-version
   :min-client contract/minimum-client-protocol
   :min-gateway contract/minimum-gateway-protocol
   :version (release-version)
   :build (build-id)})

(defn client-headers
  "Headers every Vis client stamps on a gateway request."
  [client-name]
  {(contract/header :protocol) (str contract/protocol-version)
   (contract/header :minimum-gateway-protocol) (str contract/minimum-gateway-protocol)
   (contract/header :client) (str client-name)
   (contract/header :client-version) (release-version)})


(defn request->client
  "Read the client's advertised protocol from a Ring request's normalized headers."
  [request]
  (let [h
        (:headers request)

        parsed
        (contract/wire->handshake {"protocol" (get h (contract/header :protocol))
                                   "min_gateway"
                                   (get h (contract/header :minimum-gateway-protocol))})]

    {:protocol (:protocol parsed)
     :min-gateway (:min-gateway parsed)
     :name (some-> (get h (contract/header :client))
                   str
                   not-empty)
     :version (some-> (get h (contract/header :client-version))
                      str
                      not-empty)}))


(defn gateway-verdict
  "The gateway's own judgement of one inbound request: [[contract/verdict]] with
   this build filled in as the gateway half."
  [request]
  (let [{:keys [protocol min-gateway name version]} (request->client request)]
    (contract/verdict {:gateway-protocol contract/protocol-version
                       :gateway-min-client contract/minimum-client-protocol
                       :gateway-version (release-version)
                       :client-protocol protocol
                       :client-min-gateway min-gateway
                       :client-name name
                       :client-version version})))

(defn client-verdict
  "A client's judgement of the gateway it just probed, given that gateway's
   ENGINE handshake map (the [[contract/wire->handshake]] of its advertised `protocol`
   block; nil/all-nil = a gateway too old to advertise one). `client-name` names
   this client in the rendered copy."
  [client-name gateway-handshake]
  (let [{:keys [protocol min-client version]} (or gateway-handshake {})]
    (contract/verdict {:gateway-protocol protocol
                       :gateway-min-client min-client
                       :gateway-version version
                       :client-protocol contract/protocol-version
                       :client-min-gateway contract/minimum-gateway-protocol
                       :client-name client-name
                       :client-version (release-version)})))

(defn explain
  "Human copy for a verdict: a title, one plain-language summary, and ORDERED
   remedy steps. One writer for every surface (terminal panel, gateway 426
   body, companion screen) so the wording never drifts between them."
  [{:keys [reason gateway-protocol gateway-min-client client-protocol client-min-gateway
           client-name]}]
  (case reason
    "client-too-old"
    {:title "Update this client"
     :summary (str "The gateway speaks protocol "
                   gateway-protocol
                   " and no longer serves clients below protocol "
                   gateway-min-client
                   ". This "
                   (or client-name "client")
                   " speaks protocol "
                   client-protocol
                   ".")
     :remedy ["Update Vis on this device to the version running the gateway."
              "Reload the app (or restart the TUI) once the update lands."]}

    "gateway-too-old"
    {:title "Update the gateway"
     :summary (str "This "
                   (or client-name "client")
                   " needs gateway protocol "
                   client-min-gateway
                   " or newer, but the gateway speaks protocol "
                   gateway-protocol
                   ".")
     :remedy ["Update Vis on the machine hosting the gateway."
              "Restart it: vis-agent gateway stop && vis-agent gateway start"]}

    "unknown"
    (if (nil? gateway-protocol)
      {:title "Update the gateway"
       :summary "The gateway did not advertise the current Vis wire protocol and is unsupported."
       :remedy ["Update Vis on the machine hosting the gateway."
                "Restart it: vis-agent gateway stop && vis-agent gateway start"]}
      {:title "Update this client"
       :summary (str "This "
                     (or client-name "client")
                     " did not advertise the current Vis wire protocol and is unsupported.")
       :remedy ["Update Vis on this device to the version running the gateway."
                "Reload the app (or restart the TUI) once the update lands."]})

    {:title "Versions match"
     :summary (str "Gateway and "
                   (or client-name "client")
                   " both speak protocol "
                   (or gateway-protocol client-protocol)
                   ".")
     :remedy []}))

(def ^:private panel-width 70)

(defn- wrap-words
  "Greedy word wrap to `width` columns. The panel is plain text on a terminal we
   do not control, so long copy folds instead of being clipped."
  [s ^long width]
  (reduce (fn [lines ^String word]
            (let [^String tail (peek lines)]
              (if (or (nil? tail) (> (+ (count tail) 1 (count word)) width))
                (conj lines word)
                (conj (pop lines) (str tail " " word)))))
          []
          (remove str/blank? (str/split (str s) #"\s+"))))

(defn- panel-row
  [s]
  (let [w
        (long panel-width)

        body
        (str "  " s)

        clipped
        (if (> (count body) (- w 2)) (subs body 0 (- w 2)) body)]

    (str "│" clipped (apply str (repeat (- w 2 (count clipped)) \space)) "│")))

(defn- peer-row
  [label name-str version protocol note]
  (format "%-9s %-14s %-10s protocol %s%s"
          label
          (or name-str "-")
          (or version "unknown")
          (or protocol "unknown")
          (if note (str "   " note) "")))

(defn panel-lines
  "The terminal SCREEN for an incompatible peer: a boxed, colour-free panel of
   plain lines. Returned as data (never printed here) so the CLI, the TUI, and
   tests all render the exact same block."
  [{:keys [gateway-protocol gateway-min-client gateway-version client-protocol client-min-gateway
           client-version client-name upgrade]
    :as v}]
  (let [{:keys [title summary remedy]}
        (explain v)

        w
        (long panel-width)

        rule
        (apply str (repeat (- w 2) "─"))]

    (concat [(str "╭" rule "╮") (panel-row "VIS · VERSION MISMATCH") (str "├" rule "┤")
             (panel-row title) (panel-row "")]
            (map panel-row (wrap-words summary (- w 6)))
            [(panel-row "")
             (panel-row (peer-row "client"
                                  (or client-name "client")
                                  client-version
                                  client-protocol
                                  (when (= "client" upgrade) "<- too old")))
             (panel-row (peer-row "gateway"
                                  "vis"
                                  gateway-version
                                  gateway-protocol
                                  (when (= "gateway" upgrade) "<- too old"))) (panel-row "")]
            (map panel-row
                 (wrap-words (str "gateway serves clients >= protocol " (or gateway-min-client "?")
                                  " · client needs gateway >= protocol " (or client-min-gateway
                                                                             "?"))
                             (- w 6)))
            (when (seq remedy)
              (concat [(panel-row "") (panel-row "Do this")]
                      (map-indexed (fn [^long i step]
                                     (panel-row (str "  " (inc i) ". " step)))
                                   remedy)))
            [(str "╰" rule "╯")])))

(defn incompatible-ex
  "The ex-info a client throws when it refuses to drive an incompatible
   gateway. Carries `:vis/user-error` (so the top-level CLI prints it clean and
   exits 2) plus `:vis/panel`, the pre-rendered [[panel-lines]] screen."
  [verdict-map]
  (let [{:keys [title summary]} (explain verdict-map)]
    (ex-info (str title " — " summary)
             {:type :gateway/incompatible
              :vis/user-error true
              :vis/panel (vec (panel-lines verdict-map))
              :compatibility verdict-map})))
