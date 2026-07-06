(ns com.blockether.vis.internal.gitignore
  "Minimal, pure-Clojure `.gitignore` matcher — the JGit-free replacement for
   `IgnoreNode`. Parses ONE `.gitignore` file (the one at a walk root) into
   ordered rules and evaluates a `/`-separated relative path against them with
   git's semantics: last matching rule wins, `!` negates, a trailing `/`
   restricts to directories, a leading/embedded `/` anchors to the root, and a
   slash-free pattern matches at any depth."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(defn- translate-body
  "Translate a gitignore pattern body (leading `/` already stripped, trailing
   `/` already stripped) into a Java regex fragment. `*`→`[^/]*`, `**`→`.*`,
   `**/`→optional dir prefix, `?`→`[^/]`, `[...]` char classes preserved
   (`[!` → `[^`); every other char is taken literally."
  ^String [^String pat]
  (let [n
        (count pat)

        sb
        (StringBuilder.)]

    (loop [i 0]
      (when (< i n)
        (let [c (.charAt pat i)]
          (cond
            ;; `**`
            (and (= c \*) (< (inc i) n) (= (.charAt pat (inc i)) \*))
            (cond
              ;; leading `**/` → zero or more leading directories
              (and (< (+ i 2) n) (= (.charAt pat (+ i 2)) \/)) (do (.append sb "(?:.*/)?")
                                                                   (recur (+ i 3)))
              :else (do (.append sb ".*") (recur (+ i 2))))
            (= c \*) (do (.append sb "[^/]*") (recur (inc i)))
            (= c \?) (do (.append sb "[^/]") (recur (inc i)))
            (= c \[) (let [close (.indexOf pat "]" (inc i))]
                       (if (neg? close)
                         (do (.append sb "\\[") (recur (inc i)))
                         (let [body (subs pat (inc i) close)
                               body (if (str/starts-with? body "!") (str "^" (subs body 1)) body)]

                           (.append sb "[")
                           (.append sb body)
                           (.append sb "]")
                           (recur (inc close)))))
            ;; regex metacharacters taken literally
            (#{\. \+ \( \) \{ \} \$ \^ \| \\} c)
            (do (.append sb "\\") (.append sb c) (recur (inc i)))
            :else (do (.append sb c) (recur (inc i)))))))
    (.toString sb)))

(defn- compile-rule
  "Compile one raw `.gitignore` line into a rule map, or nil for blank/comment
   lines. `{:neg? :dir? :self <Pattern> :under <Pattern>}`."
  [^String raw]
  (let [line (str/replace raw #"\s+$" "")]
    (when-not (or (str/blank? line) (str/starts-with? line "#"))
      (let [neg? (str/starts-with? line "!")
            line (if neg? (subs line 1) line)
            ;; an unescaped leading `\` escapes a literal `#`/`!`
            line (if (str/starts-with? line "\\") (subs line 1) line)
            dir? (str/ends-with? line "/")
            line (if dir? (subs line 0 (dec (count line))) line)
            ;; anchored when a `/` appears anywhere but the (already stripped)
            ;; trailing one — including a leading `/`.
            anchored? (str/includes? line "/")
            line (if (str/starts-with? line "/") (subs line 1) line)
            body (translate-body line)
            prefix (if anchored? "^" "(?:^|.*/)")
            self (re-pattern (str prefix body "$"))
            under (re-pattern (str prefix body "/.*$"))]

        {:neg? neg? :dir? dir? :self self :under under}))))

(defn load-matcher
  "Parse `root`/.gitignore into an ordered rule vector, or nil when absent.
   The returned value is opaque; pass it to `ignored?`."
  [^File root]
  (let [gi (io/file root ".gitignore")]
    (when (.exists gi)
      (let [rules (into [] (keep compile-rule) (str/split-lines (slurp gi)))]
        (when (seq rules) rules)))))

(defn ignored?
  "True when the `/`-separated relative path `rel` (with `path-dir?` telling
   whether it is a directory) is ignored by `matcher`. Last matching rule wins;
   a `!` rule un-ignores. nil matcher → false."
  [matcher ^String rel path-dir?]
  (boolean (when (and matcher (seq rel))
             (reduce (fn [ignored {:keys [neg? dir? self under]}]
                       (let [match? (if dir?
                                      ;; dir-only rule: children (`under`) always match; the
                                      ;; exact path matches only when it is itself a dir.
                                      (or (boolean (re-find under rel))
                                          (and path-dir? (boolean (re-find self rel))))
                                      (or (boolean (re-find self rel))
                                          (boolean (re-find under rel))))]
                         (if match? (not neg?) ignored)))
                     false
                     matcher))))
