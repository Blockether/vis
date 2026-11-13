(ns com.blockether.vis.internal.foundation.editing.version
  "Numbered line rows: the `N│ text` shape that `grep` prints."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:const gutter
  "Separator between the line number and the line text in every rendered row.
   U+2502 plus a space: it never occurs in source, so `line.split(\"│ \", 1)` is exact."
  "│ ")

(defn render-numbered-block
  "Render `[[line-number text] …]` as `N│ text` rows, each prefixed by `indent`.
   A CRLF line renders without its CR: the text a reader copies back must not
   carry a second line ending."
  (^String [tuples] (render-numbered-block tuples ""))
  (^String [tuples indent]
   (str/join "\n"
             (map (fn [[ln text]]
                    (let [s
                          (str text)

                          s
                          (if (str/ends-with? s "\r") (subs s 0 (dec (count s))) s)]

                      (str indent ln gutter s)))
                  tuples))))
