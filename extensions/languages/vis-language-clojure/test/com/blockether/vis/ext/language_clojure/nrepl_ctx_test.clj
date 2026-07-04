(ns com.blockether.vis.ext.language-clojure.nrepl-ctx-test
  "Unit tests for the `:ext/ctx-fn` nREPL contribution. Discovery and liveness
   are stubbed so the assertions are deterministic and offline; the real
   `describe`/eval round-trip is covered in `nrepl-client-test`."
  (:require
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
   [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nx]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.repl-manager :as rm]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- reset-cache! []
  (reset! @#'nx/liveness-cache {:key nil :statuses {}}))

(defn- nrepl-of [contribution]
  ;; The contribution is STRING-keyed from the top: the contract key is
  ;; "session_env" (ctx_loop drops a keyword :session/env), and everything under
  ;; it crosses the strings-only boundary.
  (get-in contribution ["session_env" "languages" "clojure" "nrepl"]))

(defdescribe contribute-shape-test
  (it "nests live nREPL state under :session/env :languages :clojure with via/status/dialect/cwd"
    (reset-cache!)
    (with-redefs [ports/discover-all (fn [_] [{:port 7001 :source "/proj/.nrepl-port"}])
                  rm/managed-ports   (fn [] {})
                  nc/probe!          (fn [_] {:status :up
                                              :versions {:clojure "1.12.4"}
                                              :dialect :clj
                                              :cwd "/proj"})]
      (let [b (nrepl-of (nx/contribute {:workspace/root "/proj"
                                        :ctx-atom (atom {:session/turn 1})}))
            p (first (get b "ports"))]
        (expect (= 7001 (get b "default")))
        (expect (= "project" (get p "via")))
        (expect (= "up" (get p "status")))
        (expect (= "clj" (get p "dialect")))
        (expect (= "/proj" (get p "cwd")))
        (expect (= {"clojure" "1.12.4"} (get p "versions")))
        ;; not vis-managed (no managed processes) → managed false, no handle
        (expect (false? (get p "managed")))
        (expect (nil? (get p "tool"))))))

  (it "derives :via per source and falls back :cwd to the source dir for project ports"
    (reset-cache!)
    (with-redefs [ports/discover-all (fn [_] [{:port 1 :source "/a/.nrepl-port"}
                                              {:port 2 :source "/h/.lein/repl-port"}
                                              {:port 3 :source "/h/.clojure/.nrepl-port"}])
                  rm/managed-ports   (fn [] {})
                  nc/probe!          (fn [_] {:status :up})] ; no :cwd from server
      (let [ports   (get (nrepl-of (nx/contribute {:workspace/root "/a"
                                                   :ctx-atom (atom {:session/turn 2})})) "ports")
            by-port (into {} (map (juxt #(get % "port") identity)) ports)]
        (expect (= "project"      (get (by-port 1) "via")))
        (expect (= "lein-home"    (get (by-port 2) "via")))
        (expect (= "clojure-home" (get (by-port 3) "via")))
        ;; project port falls back to its source dir; non-project leave cwd absent
        (expect (= "/a" (get (by-port 1) "cwd")))
        (expect (nil? (get (by-port 2) "cwd")))
        (expect (nil? (get (by-port 3) "cwd"))))))

  (it "returns {} when no workspace root is on env"
    (expect (= {} (nx/contribute {}))))

  (it "emits an empty block when nothing is discovered"
    (reset-cache!)
    (with-redefs [ports/discover-all (fn [_] [])
                  rm/managed-ports   (fn [] {})]
      (let [b (nrepl-of (nx/contribute {:workspace/root "/proj"
                                        :ctx-atom (atom {:session/turn 3})}))]
        (expect (nil? (get b "default")))
        (expect (= [] (get b "ports"))))))

  (it "never throws — degrades to {} when discovery blows up"
    (reset-cache!)
    (with-redefs [ports/discover-all (fn [_] (throw (ex-info "boom" {})))]
      (expect (= {} (nx/contribute {:workspace/root "/proj"
                                    :ctx-atom (atom {:session/turn 4})}))))))

(defdescribe managed-cross-reference-test
  (it "stamps :managed + :tool/:pid/:aliases on a vis-managed port"
    (reset-cache!)
    (with-redefs [ports/discover-all (fn [_] [{:port 7001 :source "/proj/.nrepl-port"}])
                  rm/managed-ports   (fn [] {7001 {:managed true :tool :clj
                                                   :pid 4242 :aliases [:dev]
                                                   :dir "/proj"}})
                  nc/probe!          (fn [_] {:status :up})]
      (let [p (first (get (nrepl-of (nx/contribute {:workspace/root "/proj"
                                                    :ctx-atom (atom {:session/turn 8})})) "ports"))]
        (expect (true? (get p "managed")))
        (expect (= "clj" (get p "tool")))
        (expect (= 4242 (get p "pid")))
        (expect (= ["dev"] (get p "aliases"))))))

  (it "surfaces a vis-managed subdir REPL that workspace-root discovery misses"
    (reset-cache!)
    (with-redefs [ports/discover-all (fn [_] [])  ; nothing in the project tree
                  rm/managed-ports   (fn [] {6543 {:managed true :tool :clj
                                                   :dir "/proj/extensions/foo"}})
                  nc/probe!          (fn [_] {:status :up})]
      (let [ports (get (nrepl-of (nx/contribute {:workspace/root "/proj"
                                                 :ctx-atom (atom {:session/turn 9})})) "ports")
            p     (first ports)]
        (expect (= 1 (count ports)))
        (expect (= 6543 (get p "port")))
        (expect (true? (get p "managed")))
        (expect (= "/proj/extensions/foo/.nrepl-port" (get p "source")))))))

(defdescribe per-turn-cache-test
  (it "probes once per (turn, port-set); re-probes when the turn advances"
    (reset-cache!)
    (let [calls (atom 0)]
      (with-redefs [ports/discover-all (fn [_] [{:port 9999 :source "/p/.nrepl-port"}])
                    rm/managed-ports   (fn [] {})
                    nc/probe!          (fn [_] (swap! calls inc) {:status :up})]
        (let [e1 {:workspace/root "/p" :ctx-atom (atom {:session/turn 5})}
              e2 {:workspace/root "/p" :ctx-atom (atom {:session/turn 6})}]
          (nx/contribute e1) (nx/contribute e1) (nx/contribute e1)
          (expect (= 1 @calls))
          (nx/contribute e2)
          (expect (= 2 @calls))))))

  (it "re-probes within the same turn when the discovered port-set changes"
    (reset-cache!)
    (let [calls  (atom 0)
          ports* (atom [{:port 1 :source "/p/.nrepl-port"}])]
      (with-redefs [ports/discover-all (fn [_] @ports*)
                    rm/managed-ports   (fn [] {})
                    nc/probe!          (fn [_] (swap! calls inc) {:status :up})]
        (let [e {:workspace/root "/p" :ctx-atom (atom {:session/turn 7})}]
          (nx/contribute e)
          (expect (= 1 @calls))
          (reset! ports* [{:port 1 :source "/p/.nrepl-port"}
                          {:port 2 :source "/p/sub/.nrepl-port"}])
          (nx/contribute e)
          ;; new port-set → cache miss → both ports re-probed (1 + 2 = 3)
          (expect (= 3 @calls)))))))
