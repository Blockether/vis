(ns com.blockether.vis.heap-ceiling-test
  "The `:vis` alias pins ONE heap ceiling for every JVM Vis launches — TUI and
   gateway alike.

   It used to be a PERCENTAGE (`-XX:MaxRAMPercentage=75.0`), which scaled the
   ceiling with the host: ~27 GiB on a 48 GB machine. At that size the
   free-ratio + periodic-GC flags beside it never uncommit (a 3 GiB live set is
   noise against 27 GiB), and `internal.loop`'s memory-pressure gates — the heap
   watermark percent especially — can never trip, so a long-running gateway
   parks at multi-gigabyte RSS and looks like a leak. Nothing else in the tree
   asserts these opts, so a well-meaning re-introduction of a percentage would
   be invisible until the next time someone asks why the gateway is huge."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- vis-jvm-opts
  "The `:vis` alias `:jvm-opts` from the root deps.edn — the options every
   `clojure -M:vis` process (TUI and `gateway start`) runs with."
  []
  (get-in (edn/read-string (slurp (io/file "deps.edn"))) [:aliases :vis :jvm-opts]))

(defdescribe vis-alias-heap-ceiling-test
             (it "pins an explicit 5 GiB ceiling instead of a share of host RAM"
                 (let [opts (vis-jvm-opts)]
                   (expect (seq opts))
                   (expect (some #{"-Xmx5g"} opts)
                           (str "expected an explicit -Xmx5g, got " (pr-str opts)))
                   (doseq [opt opts]
                     (expect (not (str/includes? opt "MaxRAMPercentage"))
                             (str opt " scales the heap ceiling with host RAM"))
                     (expect (not (str/includes? opt "MaxRAM="))
                             (str opt " scales the heap ceiling with host RAM")))))
             (it "keeps the uncommit flags that make the ceiling shrinkable"
                 ;; -Xmx alone caps growth; it never hands pages back. The periodic
                 ;; concurrent cycle plus the free ratios are what return them to the OS.
                 (let [opts (set (vis-jvm-opts))]
                   (expect (contains? opts "-XX:+G1PeriodicGCInvokesConcurrent"))
                   (expect (contains? opts "-XX:MinHeapFreeRatio=10"))
                   (expect (contains? opts "-XX:MaxHeapFreeRatio=25")))))
