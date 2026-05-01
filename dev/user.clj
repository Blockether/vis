(ns user
  "REPL conveniences for `clojure -M:dev` / nREPL sessions."
  (:require [com.blockether.vis.dev :as dev]))

(def start-nrepl! dev/start-nrepl!)
(def stop-nrepl! dev/stop-nrepl!)
(def nrepl-status dev/nrepl-status)
(def cli! dev/cli!)
(def tui! dev/tui!)
(def in-process-tui! dev/in-process-tui!)
