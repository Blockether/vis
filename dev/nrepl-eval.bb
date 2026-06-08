#!/usr/bin/env bb
;; Minimal nREPL client: reads code from stdin, evals on the running REPL
;; (port from .nrepl-port), prints value / out / err. Usage:
;;   echo '(+ 1 2)' | bb dev/nrepl-eval.bb
(require '[bencode.core :as b]
         '[clojure.java.io :as io]
         '[clojure.string :as str])
(import '[java.net Socket])
(let [port (Integer/parseInt (str/trim (slurp ".nrepl-port")))
      code (slurp *in*)
      sock (Socket. "127.0.0.1" port)
      out  (io/output-stream sock)
      in   (java.io.PushbackInputStream. (io/input-stream sock))
      ->s  (fn [x] (if (bytes? x) (String. ^bytes x "UTF-8") x))]
  (b/write-bencode out {"op" "eval" "code" code})
  (.flush out)
  (loop []
    (let [msg (b/read-bencode in)
          m   (into {} (map (fn [[k v]] [k (->s v)])) msg)]
      (when-let [v (get m "value")] (println "=>" v))
      (when-let [o (get m "out")]   (print o))
      (when-let [e (get m "err")]   (binding [*out* *err*] (print e)))
      (when-let [ex (get m "ex")]   (println "EX:" ex))
      (let [status (get msg "status")
            status (when status (map ->s status))]
        (if (some #{"done"} status) :done (recur))))))
