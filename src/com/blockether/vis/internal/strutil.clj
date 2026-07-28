(ns com.blockether.vis.internal.strutil
  "Shared tiny string helpers. A dependency-free leaf so any namespace can use it
   without risking a cycle.")

(defn truncate
  "Head-clip `s` to at most `n` chars (no ellipsis)."
  [s ^long n]
  (let
    [s
     (str s)

     c
     (long (count s))]

    (if (> c n) (subs s 0 n) s)))

(defn fence-delimiter
  "Markdown fence delimiter (a backtick run) longer than any backtick run in
   `body`. Arbitrary content — a file being read, a diff of a Markdown file, a
   tool's stdout — carries Markdown fences of its own; a fixed triple-backtick
   wrapper is then ambiguous and the INNER fence closes the outer block early,
   so everything after it renders as prose instead of code. CommonMark permits
   longer fences: pick the shortest safe one."
  [body]
  (let
    [max-run
     (->> (re-seq #"`+" (str body))
          (map count)
          (reduce max 0))]

    (apply str (repeat (max 3 (inc (long max-run))) "`"))))

(defn fenced
  "Wrap `body` in a fenced code block `body` itself cannot close early, with an
   optional `lang` info string. Callers keep their own blank/nil guards."
  ([body] (fenced body nil))
  ([body lang]
   (let
     [body
      (str body)

      delimiter
      (fence-delimiter body)]

     (str delimiter (or lang "") "\n" body "\n" delimiter))))
