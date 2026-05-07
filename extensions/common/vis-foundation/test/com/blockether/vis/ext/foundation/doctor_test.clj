(ns com.blockether.vis.ext.foundation.doctor-test
  "Unit tests for foundation's `:ext/doctor-check-fn` sections:
   ::system, ::agents-md, ::skills, ::scan-warnings.

   Plan §6: each section returns expected message shapes for every
   input scenario; the composite `check-fn` stamps the right
   `:check-id` on every message."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.doctor :as doctor]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- section-msgs
  "Run the composite `check-fn` against `env`, then keep only the
   messages stamped with the given `:check-id` — the test sees just
   the section it cares about."
  [check-id env]
  (->> (doctor/check-fn env)
    (filter #(= check-id (:check-id %)))
    vec))

;; ---------------------------------------------------------------------------
;; ::system
;; ---------------------------------------------------------------------------

(defdescribe system-check-test
  (it "emits 5 :info messages with OS / Java / Clojure / Memory / DB path"
    (let [msgs     (section-msgs ::doctor/system {})
          msg-text (mapv :message msgs)]
      (expect (= 5 (count msgs)))
      (expect (every? #(= :info (:level %)) msgs))
      (expect (some #(str/starts-with? % "OS:") msg-text))
      (expect (some #(str/starts-with? % "Java:") msg-text))
      (expect (some #(str/starts-with? % "Clojure:") msg-text))
      (expect (some #(str/starts-with? % "Memory:") msg-text))
      (expect (some #(str/starts-with? % "DB path:") msg-text))))

  (it "DB path message says '(no DB)' when env has no :db-info"
    (let [msgs (section-msgs ::doctor/system {})
          db   (some #(when (str/starts-with? (:message %) "DB path:") %) msgs)]
      (expect (str/includes? (:message db) "(no DB)"))))

  (it "DB path message includes the path when :db-info :path present"
    (let [msgs (section-msgs ::doctor/system {:db-info {:path "/tmp/test.db"}})
          db   (some #(when (str/starts-with? (:message %) "DB path:") %) msgs)]
      (expect (str/includes? (:message db) "/tmp/test.db")))))

;; ---------------------------------------------------------------------------
;; ::agents-md
;; ---------------------------------------------------------------------------

(defdescribe agents-md-check-test
  ;; This section reads from the agents/ scanner, which is cwd-cached.
  ;; In the test JVM, cwd is the repo root which DOES have AGENTS.md.
  ;; So we exercise the present case with the live data; the absent
  ;; case is covered indirectly by the agents scanner tests.

  (it "emits one :info message when AGENTS.md found"
    (let [msgs (section-msgs ::doctor/agents-md {})]
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
    (let [msgs (section-msgs ::doctor/skills {})]
      (expect (= 1 (count msgs)))
      (let [m (first msgs)]
        (expect (= :info (:level m)))
        ;; The repo has 5 repo skills + 1 user-global skill.
        (expect (re-find #"\d+ skills? loaded" (:message m)))))))

;; ---------------------------------------------------------------------------
;; ::voice
;; ---------------------------------------------------------------------------

(defdescribe voice-check-test
  (it "emits voice extension, ffmpeg, and Piper espeak-ng-data diagnostics"
    (let [msgs     (section-msgs ::doctor/voice {})
          msg-text (mapv :message msgs)]
      (expect (= 3 (count msgs)))
      (expect (some #(str/starts-with? % "Voice:") msg-text))
      (expect (some #(str/starts-with? % "ffmpeg:") msg-text))
      (expect (some #(str/starts-with? % "Piper espeak-ng-data:") msg-text))
      (expect (every? #{:info :warn :error} (mapv :level msgs))))))

;; ---------------------------------------------------------------------------
;; ::scan-warnings
;; ---------------------------------------------------------------------------

(defdescribe scan-warnings-check-test
  (it "emits zero messages when nothing malformed (clean repo)"
    (expect (empty? (section-msgs ::doctor/scan-warnings {})))))

;; ---------------------------------------------------------------------------
;; Composite check-fn shape
;; ---------------------------------------------------------------------------

(defdescribe check-fn-shape-test
  (it "check-fn is a function suitable for `:ext/doctor-check-fn`"
    (expect (fn? doctor/check-fn)))

  (it "every emitted message carries one of the five documented :check-ids in section order"
    (let [msgs (doctor/check-fn {})
          ids  (distinct (mapv :check-id msgs))]
      (expect (every? #{::doctor/system
                        ::doctor/agents-md
                        ::doctor/skills
                        ::doctor/voice
                        ::doctor/scan-warnings}
                ids))
      ;; Sections appear in the documented order — system, agents-md, skills,
      ;; voice, scan-warnings. Any present subset preserves that ordering.
      (let [section-order [::doctor/system ::doctor/agents-md
                           ::doctor/skills ::doctor/voice ::doctor/scan-warnings]
            present (filter (set ids) section-order)]
        (expect (= present ids))))))
