(ns com.blockether.vis.internal.strutil
  "Shared tiny string helpers. A dependency-free leaf so any namespace can use it
   without risking a cycle.")

(defn truncate
  "Head-clip `s` to at most `n` chars (no ellipsis)."
  [s n]
  (let [s (str s)]
    (if (> (count s) n) (subs s 0 n) s)))
