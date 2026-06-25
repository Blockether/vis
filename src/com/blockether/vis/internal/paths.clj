(ns com.blockether.vis.internal.paths
  "Cross-platform path helpers. A LEAF namespace (no project deps) so any
   layer — core, extensions, tests — can normalize without a require cycle.")

(defn unixify
  "Normalize a path string to `/` separators on every OS. Java's `File`/`Path`
   APIs yield `\\` on Windows, and there is no stdlib/`fs` call that hands back
   a `/`-string there — so this is the single canonical normalizer.

   Use it ONLY where a path is DATA: compared, glob-matched, shown to the model,
   or embedded in a URL / wire / DB. NEVER for real filesystem I/O — `io/file`,
   `.exists`, JGit, nio all take native paths fine. Returns nil for nil input."
  ^String [s]
  (when s (.replace (str s) "\\" "/")))
