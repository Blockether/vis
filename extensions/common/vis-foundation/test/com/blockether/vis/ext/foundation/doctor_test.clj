(ns com.blockether.vis.ext.foundation.doctor-test
  "Unit tests for foundation's `:ext/doctor-checks` entries:
   ::system, ::agents-md, ::skills, ::scan-warnings.

   Plan §6: each check returns expected message shapes for every
   input scenario."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.doctor :as doctor]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- run-check [check-map env]
  ;; Mirror what the aggregator does to a single check fn: invoke
  ;; with env, collect the returned messages.
  (let [r ((:check/run-fn check-map) env)]
    (if (map? r) [r] (vec r))))

;; ---------------------------------------------------------------------------
;; ::system
;; ---------------------------------------------------------------------------

(defdescribe system-check-test
  (it "emits 5 :info messages with OS / Java / Clojure / Memory / DB path"
    (let [msgs (run-check doctor/system-check {})
          msg-text (mapv :message msgs)]
      (expect (= 5 (count msgs)))
      (expect (every? #(= :info (:level %)) msgs))
      (expect (some #(str/starts-with? % "OS:") msg-text))
      (expect (some #(str/starts-with? % "Java:") msg-text))
      (expect (some #(str/starts-with? % "Clojure:") msg-text))
      (expect (some #(str/starts-with? % "Memory:") msg-text))
      (expect (some #(str/starts-with? % "DB path:") msg-text))))

  (it "DB path message says '(no DB)' when env has no :db-info"
    (let [msgs (run-check doctor/system-check {})
          db   (some #(when (str/starts-with? (:message %) "DB path:") %) msgs)]
      (expect (str/includes? (:message db) "(no DB)"))))

  (it "DB path message includes the path when :db-info :path present"
    (let [msgs (run-check doctor/system-check {:db-info {:path "/tmp/test.db"}})
          db   (some #(when (str/starts-with? (:message %) "DB path:") %) msgs)]
      (expect (str/includes? (:message db) "/tmp/test.db")))))

;; ---------------------------------------------------------------------------
;; ::agents-md
;; ---------------------------------------------------------------------------

(defdescribe agents-md-check-test
  ;; This check reads from the agents/ scanner, which is cwd-cached.
  ;; In the test JVM, cwd is the repo root which DOES have AGENTS.md.
  ;; So we exercise the present case with the live data; the absent
  ;; case is covered indirectly by the agents scanner tests.

  (it "emits one :info message when AGENTS.md found"
    (let [msgs (run-check doctor/agents-md-check {})]
      (expect (= 1 (count msgs)))
      (let [m (first msgs)]
        (expect (= :info (:level m)))
        (expect (str/includes? (:message m) "AGENTS.md"))
        (expect (str/includes? (:message m) "source: repo"))))))

;; ---------------------------------------------------------------------------
;; ::skills
;; ---------------------------------------------------------------------------

(defdescribe skills-check-test
  (it "emits one :info message summarising count + breakdown"
    (let [msgs (run-check doctor/skills-check {})]
      (expect (= 1 (count msgs)))
      (let [m (first msgs)]
        (expect (= :info (:level m)))
        ;; The repo has 5 repo skills + 1 user-global skill.
        (expect (re-find #"\d+ skills? loaded" (:message m)))))))

;; ---------------------------------------------------------------------------
;; ::scan-warnings
;; ---------------------------------------------------------------------------

(defdescribe scan-warnings-check-test
  (it "emits zero messages when nothing malformed (clean repo)"
    (let [msgs (run-check doctor/scan-warnings-check {})]
      (expect (empty? msgs)))))

;; ---------------------------------------------------------------------------
;; Bundled vec sanity
;; ---------------------------------------------------------------------------

(defdescribe all-checks-shape-test
  (it "all-checks vec contains the four documented entries in order"
    (expect (= 4 (count doctor/all-checks)))
    (expect (= [:com.blockether.vis.ext.foundation.doctor/system
                :com.blockether.vis.ext.foundation.doctor/agents-md
                :com.blockether.vis.ext.foundation.doctor/skills
                :com.blockether.vis.ext.foundation.doctor/scan-warnings]
              (mapv :check/id doctor/all-checks))))

  (it "every entry has the four required fields"
    (doseq [c doctor/all-checks]
      (expect (keyword? (:check/id c)))
      (expect (string?  (:check/name c)))
      (expect (string?  (:check/description c)))
      (expect (fn?      (:check/run-fn c))))))
