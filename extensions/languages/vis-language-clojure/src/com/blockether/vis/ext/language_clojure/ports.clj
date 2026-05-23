(ns com.blockether.vis.ext.language-clojure.ports
  "nREPL port discovery for `clj/*` tools.

   Sources, in priority order:
     1. Explicit `:port` argument on the calling tool.
     2. `.nrepl-port` files: workspace root, then each ancestor up to
        the user's home, then `$HOME/.lein/repl-port` and
        `$HOME/.clojure/.nrepl-port`. First readable wins for
        `find-default`; `discover-all` returns every hit.
     3. (Future) `jps`-based scan. Skipped for now: cross-platform
        flakiness and ambiguity (multiple JVMs, no easy way to map a
        pid to the project that owns it). Explicit `.nrepl-port` is
        already what real workflows produce.

   Returned ports are positive integers in 1..65535; anything else is
   filtered out so the caller never tries to dial a bogus port."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- parse-port
  "Read a `.nrepl-port` file and return the integer port, or nil on
   any parse / IO failure. nREPL writes just the number, possibly
   with trailing whitespace."
  [^java.io.File f]
  (when (and f (.isFile f) (.canRead f))
    (try
      (let [raw (str/trim (slurp f))
            n   (Long/parseLong raw)]
        (when (and (pos? n) (< n 65536))
          (int n)))
      (catch Throwable _ nil))))

(defn- ancestor-chain
  "Workspace root + every ancestor up to (and including) `$HOME`.
   Used so a deeply-nested cwd can still see the project's
   top-level `.nrepl-port`."
  [^java.io.File root]
  (let [home (some-> (System/getProperty "user.home") io/file)
        seen (volatile! #{})]
    (loop [^java.io.File d  root
           acc              []]
      (cond
        (nil? d)                 acc
        (contains? @seen (.getAbsolutePath d)) acc
        :else
        (do
          (vswap! seen conj (.getAbsolutePath d))
          (let [acc' (conj acc d)]
            (if (and home (= (.getAbsolutePath d) (.getAbsolutePath home)))
              acc'
              (recur (.getParentFile d) acc'))))))))

(defn- candidate-files
  "Ordered seq of every `.nrepl-port` candidate file we will probe."
  [^java.io.File root]
  (let [home (some-> (System/getProperty "user.home") io/file)]
    (concat
      (map #(io/file % ".nrepl-port") (ancestor-chain root))
      (when home
        [(io/file home ".lein" "repl-port")
         (io/file home ".clojure" ".nrepl-port")]))))

(defn discover-all
  "Return a vec of `{:port int :source path}` for every readable
   port file we can see, in priority order, deduped on port number."
  [workspace-root]
  (let [root (some-> workspace-root io/file)]
    (when root
      (let [seen (volatile! #{})
            out  (volatile! [])]
        (doseq [^java.io.File f (candidate-files root)
                :let [p (parse-port f)]
                :when p]
          (when-not (contains? @seen p)
            (vswap! seen conj p)
            (vswap! out conj {:port   p
                              :source (.getAbsolutePath f)})))
        @out))))

(defn find-default
  "Best-effort default port for the workspace. Returns an integer or
   nil. Callers MUST handle nil with a hint pointing at `clj/ports`."
  [workspace-root]
  (some-> (discover-all workspace-root) first :port))
