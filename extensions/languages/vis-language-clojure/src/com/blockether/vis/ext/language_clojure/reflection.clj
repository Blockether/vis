(ns com.blockether.vis.ext.language-clojure.reflection
  "The `:general` lint provider: Clojure COMPILER warnings — reflection and
   boxed math.

   Unlike clj-kondo (static analysis over source text), these warnings only
   exist at COMPILE time: the compiler emits them while it resolves interop /
   code. So this provider COMPILES whatever the lint TARGETS — a `lint_code` code
   string, or each source file being linted — in a throwaway namespace that is
   torn down afterwards, so the running system is never mutated and nothing leaks.

   It compiles the code in a throwaway namespace with `*warn-on-reflection*` and
   `*unchecked-math* :warn-on-boxed` bound, captures the compiler's `*err*`
   stream, and parses each warning line

     `Reflection warning, <file>:<row>:<col> - <message>`
     `Boxed math warning, <file>:<row>:<col> - <message>`

   into the uniform lint finding map, tagged `\"provider\" \"general\"`:
   `{\"file\" \"row\" \"col\" \"level\" \"warning\" \"type\" \"reflection\"|\"boxed-math\"
     \"message\" \"provider\" \"general\"}`."
  (:require [clojure.string :as str]))

(def provider "The provider tag every finding from this namespace carries." "general")

(def ^:private warning-re
  "Matches one compiler warning line: kind, source, row, col, message."
  #"(?m)^(Reflection|Boxed math) warning, (.+?):(\d+):(\d+) - (.+)$")

(defn- warning->finding
  "Shape one regex match (a `[whole kind src row col message]` vector) into the
   uniform finding map. `file` overrides the compiler-reported source (which is
   `null` for a `load-string` snippet); when nil the reported source is kept."
  [file [_ kind src row col message]]
  {"file" (if (str/blank? (str file)) src file)
   "row" (Long/parseLong row)
   "col" (Long/parseLong col)
   "level" "warning"
   "type" (if (= kind "Boxed math") "boxed-math" "reflection")
   "message" message
   "provider" provider})

(defn compile-warnings
  "Compile `code` (a Clojure source string) with the compiler's reflection and
   boxed-math warnings on, and return the parsed findings vector (each tagged
   `\"provider\" \"general\"`).

   `file` (optional) is reported as each finding's `\"file\"` — pass the linted
   target (or `\"<stdin>\"` for a snippet) so these findings group with the
   clj-kondo ones. Compilation is the point (this runs only over code we
   execute), but it happens in a throwaway namespace and every namespace the
   code creates is removed afterwards. A hard compile error yields whatever
   warnings were emitted before it (possibly none) — never throws."
  ([code] (compile-warnings code nil))
  ([code file]
   (if (str/blank? (str code))
     []
     (let [before
           (set (all-ns))

           sw
           (java.io.StringWriter.)]

       (try (binding [*err*
                      sw

                      *warn-on-reflection*
                      true

                      *unchecked-math*
                      :warn-on-boxed

                      *ns*
                      (create-ns (gensym "vis-lint-reflect-"))]

              (clojure.core/refer-clojure)
              (try (load-string (str code)) (catch Throwable _ nil)))
            (mapv (partial warning->finding file) (re-seq warning-re (str sw)))
            (finally (doseq [n (all-ns)]
                       (when-not (contains? before n) (remove-ns (ns-name n))))))))))
