(ns com.blockether.vis.ext.language-typescript-bun.runner
  "Resolve WHICH bun binary launches the REPL / tests, mirroring how the Python
   pack picks uv / poetry / venv. Bun has no per-project interpreter zoo: it is
   `bun` on PATH, else the official installer location (~/.bun/bin/bun)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- windows? [] (str/starts-with? (str (System/getProperty "os.name")) "Windows"))

(defn- on-path?
  "Is executable `bin` resolvable on PATH? (Tries the name and, on Windows, the
   .exe variant.)"
  [bin]
  (let [names
        (cond-> [bin]
          (windows?)
          (conj (str bin ".exe")))

        dirs
        (str/split (or (System/getenv "PATH") "")
                   (re-pattern (java.util.regex.Pattern/quote (System/getProperty
                                                                "path.separator"))))]

    (boolean (some (fn [d]
                     (some (fn [n]
                             (let [f (io/file d n)]
                               (and (.isFile f) (.canExecute f))))
                           names))
                   dirs))))

(defn- home-bun
  "Canonical path of ~/.bun/bin/bun when present + executable, else nil."
  []
  (let [f (io/file (System/getProperty "user.home") ".bun" "bin" (if (windows?) "bun.exe" "bun"))]
    (when (and (.isFile f) (.canExecute f)) (.getCanonicalPath f))))

(defn available?
  "Is ANY bun launchable on this machine?"
  []
  (boolean (or (on-path? "bun") (home-bun))))

(defn resolve-command
  "The argv PREFIX that launches bun for a project in `_root`. Detection order:
     1. `bun` on PATH
     2. ~/.bun/bin/bun (official installer default)
   Throws when neither exists — the pack activated on a Bun-looking workspace
   but the machine can't run it."
  [_root]
  (cond (on-path? "bun") ["bun"]
        (home-bun) [(home-bun)]
        :else (throw (ex-info "bun not found — install it (https://bun.com) or put it on PATH"
                              {:type :ts/no-bun}))))
