(ns com.blockether.vis.internal.foundation.pty
  "The PTY adapter for `libvisjail`.

   Native descriptor ownership, terminal setup, process groups, waiting and
   signals live in `vis-python-runtime`; this namespace keeps only the handle
   map consumed by shell and its passthrough bridge."
  (:require [com.blockether.vis.internal.process-jail :as process-jail]))

(defn- tee-input
  [^java.io.InputStream raw listeners]
  (let [input
        (java.io.PipedInputStream. (* 64 1024))

        output
        (java.io.PipedOutputStream. input)]

    (doto (Thread/ofPlatform)
      (.daemon true)
      (.name "visjail-pty-tee")
      (.start ^Runnable
              (fn []
                (let [buffer (byte-array 8192)]
                  (try (with-open [source raw
                                   sink output]

                         (loop []

                           (let [n (.read source buffer)]
                             (when (pos? n)
                               (let [chunk (java.util.Arrays/copyOf ^bytes buffer n)]
                                 (.write sink chunk)
                                 (.flush sink)
                                 (doseq [listener @listeners]
                                   (try (listener chunk) (catch Throwable _ nil))))
                               (recur)))))
                       (catch Throwable _ nil))))))
    input))

(defn spawn!
  "Spawn `command` under a real pseudo-terminal. Options are `:dir`, a COMPLETE
   `:env`, `:cols`, `:rows`, and an optional process-jail `:policy`. Returns
   `{:pid :in :send :wait :alive? :destroy :add-listener}`."
  [{:keys [command dir env cols rows policy] :or {cols 120 rows 40}}]
  (let [^Process process
        (process-jail/spawn!
          command
          dir
          policy
          {:environment env :pty? true :merge-stderr? true :columns cols :rows rows})

        ^java.io.OutputStream input
        (.getOutputStream process)

        listeners
        (atom #{})

        output
        (tee-input (.getInputStream process) listeners)]

    {:pid (.pid process)
     :in output
     :send (fn [^bytes bytes]
             (.write input bytes)
             (.flush input))
     :wait (fn []
             (.waitFor process))
     :alive? (fn []
               (.isAlive process))
     :destroy (fn [force?]
                (if force? (.destroyForcibly process) (.destroy process)))
     :add-listener (fn [listener]
                     (swap! listeners conj listener)
                     (fn []
                       (swap! listeners disj listener)))}))
