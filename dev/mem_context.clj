(ns mem-context
  "Measure the retained heap cost of ONE embedded-GraalPy sandbox context — the
   number that decides whether idle-tab context eviction is worth building.
   Each open tab/session owns a `create-python-context` Context; they share the
   engine (JIT code) but each has its own Python heap (globals, imported stdlib,
   the sandbox shim). Run: clojure -M:vis:mem"
  (:require [com.blockether.vis.internal.env-python :as env])
  (:import [org.graalvm.polyglot Context]))

(defn live-mb
  "Force GC twice and return the live-set MB (best-effort retained heap)."
  []
  (dotimes [_ 3] (System/gc) (Thread/sleep 250))
  (let [r (Runtime/getRuntime)]
    (/ (double (- (.totalMemory r) (.freeMemory r))) 1048576.0)))

(defn standalone-ctx
  "A GraalPy context on its OWN implicit engine (no shared engine), with a few
   stdlib imports to allocate a realistic per-context heap."
  ^Context []
  (let [c (-> (Context/newBuilder (into-array String ["python"]))
            (.allowExperimentalOptions true)
            (.option "engine.WarnVirtualThreadSupport" "false")
            (.allowAllAccess true)
            (.build))]
    (.eval c "python" "import json, re, base64, textwrap, collections, functools, itertools\nx = [str(i) for i in range(2000)]")
    c))

(defn measure-standalone []
  (let [base (live-mb)]
    (println (format "\n[STANDALONE ENGINE test]\nbaseline: %.1f MB" base))
    (let [cs (mapv (fn [_] (standalone-ctx)) (range 6))
          peak (live-mb)]
      (println (format "6 standalone contexts: %.1f MB (+%.1f)" peak (- peak base)))
      (doseq [^Context c cs] (.close c true))
      (let [closed (live-mb)]
        (println (format "after close (fresh engines): %.1f MB (reclaimed %.1f of %.1f)"
                   closed (- peak closed) (- peak base)))))))

(defn -main [& _]
  (let [base (live-mb)]
    (println (format "baseline live-set:            %.1f MB" base))
    ;; First context pays one-time engine + printer/parser warmup; later ones
    ;; are the marginal per-session cost.
    (let [ctxs (atom [])]
      (dotimes [i 6]
        (let [m (env/create-python-context {})]
          (swap! ctxs conj (:python-context m)))
        (println (format "after %d context(s):           %.1f MB  (+%.1f from base)"
                   (inc i) (live-mb) (- (live-mb) base))))
      (let [n (count @ctxs)
            with-all (live-mb)]
        (println (format "--- %d contexts add %.1f MB total (~%.1f MB each after the first) ---"
                   n (- with-all base) (/ (- with-all base) n)))
        (doseq [^Context c @ctxs] (try (.close c true) (catch Throwable _ nil)))
        (println (format "after closing ALL (refs held): %.1f MB  (reclaimed %.1f MB)"
                   (live-mb) (- with-all (live-mb))))
        ;; Drop the Context refs too — if THIS is what finally reclaims, the
        ;; heap was held by the Context objects; if it still doesn't, a global
        ;; (engine cache / a Clojure registry atom) is retaining the Python heap.
        (reset! ctxs [])
        (let [after-drop (live-mb)]
          (println (format "after dropping refs too:      %.1f MB  (reclaimed %.1f MB of %.1f)"
                     after-drop (- with-all after-drop) (- with-all base))))))
    (measure-standalone)))
