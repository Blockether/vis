(ns com.blockether.vis.ext.channel-tui.screen-test
  "Tests for the TUI channel entry point. The bulk of the namespace
   is Lanterna-bound and exercised by the integration smoke + render
   benchmark; this suite focuses on the pure helpers — currently the
   `--conversation-id` / `--resume` argument parser, where a silent
   accept of unknown flags previously masked typos like
   `--conversations-id`."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.channel-tui.chat :as chat]
   [com.blockether.vis.ext.channel-tui.input :as input]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.screen :as screen]
   [com.blockether.vis.ext.channel-tui.selection :as selection]
   [com.blockether.vis.internal.external-opener :as opener]
   [com.blockether.vis.ext.channel-tui.state :as state]
   [lazytest.core :refer [defdescribe it expect]]))

(def ^:private parse-args
  (deref #'screen/parse-args))

(def ^:private current-hint
  (deref #'screen/current-hint))

(def ^:private submit-input!
  (deref #'screen/submit-input!))

(def ^:private copy-conversation-id!
  (deref #'screen/copy-conversation-id!))

(def ^:private copy-selection!
  (deref #'screen/copy-selection!))

(def ^:private copy-bubble!
  (deref #'screen/copy-bubble!))

(def ^:private copy-conversation-as-markdown!
  (deref #'screen/copy-conversation-as-markdown!))

(def ^:private open-click-target!
  (deref #'screen/open-click-target!))

(def ^:private provenance-ref-from-url
  (deref #'screen/provenance-ref-from-url))

(def ^:private provenance-dialog-lines
  (deref #'screen/provenance-dialog-lines))

(def ^:private bubble-selectable-ranges
  (deref #'screen/bubble-selectable-ranges))

(def ^:private bubble-copy-regions
  (deref #'screen/bubble-copy-regions))

(def ^:private bubble-copy-hit
  (deref #'screen/bubble-copy-hit))

(def ^:private input-selectable-ranges
  (deref #'screen/input-selectable-ranges))

(def ^:private conversation-summary
  (deref #'screen/conversation-summary))

(def ^:private latest-modified-first
  (deref #'screen/latest-modified-first))

(def ^:private conversation-sort-key
  (deref #'screen/conversation-sort-key))

(def ^:private pre-resolve-conversation-id!
  (deref #'screen/pre-resolve-conversation-id!))

(defn- user-error?
  "True when `f` throws an ex-info carrying the `:vis/user-error` flag —
   the contract the channel entry point relies on to print a clean
   `vis: <msg>` line and exit 2 instead of a Java stack trace."
  [f]
  (try (f) false
    (catch clojure.lang.ExceptionInfo e
      (true? (:vis/user-error (ex-data e))))))

(defdescribe provenance-click-test
  (it "extracts the canonical ref from a TUI-internal provenance URL"
    (expect (= "turn/8fc00e9d/iteration/6/block/4"
              (provenance-ref-from-url "vis-provenance://turn/8fc00e9d/iteration/6/block/4"))))

  (it "formats a provenance event for the internal viewer"
    (let [lines (provenance-dialog-lines
                  "cid"
                  "turn/8fc00e9d/iteration/6/block/4"
                  {:ref "turn/8fc00e9d/iteration/6/block/4"
                   :status :done
                   :kind :eval
                   :op :sci/eval
                   :duration-ms 5
                   :turn-id "turn-id"
                   :iteration 6
                   :form-position 4
                   :code "(+ 1 1)"})]
      (expect (some #(= "Ref: turn/8fc00e9d/iteration/6/block/4" %) lines))
      (expect (some #(= "Status: :done" %) lines))
      (expect (some #(= "Code: (+ 1 1)" %) lines)))))

(defdescribe hint-test
  (it "empty input advertises arrow-key history instead of removed Ctrl+P/N chords"
    (let [hint (current-hint {:input (input/empty-input)})]
      (expect (re-find #"↑↓ history" hint))
      (expect (not (re-find #"Ctrl\+P/N" hint)))))

  (it "idle hints leave model/reasoning/verbosity shortcuts to the footer"
    (let [empty-hint (current-hint {:input (input/empty-input)})
          typed-hint (current-hint {:input (input/paste-text (input/empty-input) "hello")})]
      (expect (not (re-find #"Ctrl\+R reasoning" empty-hint)))
      (expect (not (re-find #"Ctrl\+L verbosity" empty-hint)))
      (expect (not (re-find #"Ctrl\+T model" empty-hint)))
      (expect (not (re-find #"Ctrl\+R reasoning" typed-hint)))
      (expect (not (re-find #"Ctrl\+L verbosity" typed-hint)))
      (expect (not (re-find #"Ctrl\+T model" typed-hint))))))

(defdescribe startup-resume-test
  (it "--conversation-id sweeps orphaned running turns before rebuilding history"
    (let [calls   (atom [])
          resumed {:id "c1" :history [{:role :assistant :text "interrupted"}]}]
      (with-redefs [vis/db-info (fn []
                                  (swap! calls conj :db-info)
                                  :db)
                    vis/db-sweep-orphaned-running-turns! (fn [db]
                                                           (swap! calls conj [:sweep db])
                                                           (expect (= :db db))
                                                           1)
                    chat/resume-conversation (fn [cid]
                                               (swap! calls conj [:resume cid])
                                               (expect (= "c1" cid))
                                               resumed)]
        (expect (= resumed (pre-resolve-conversation-id! {:conversation-id "c1"})))
        (expect (= [:db-info [:sweep :db] [:resume "c1"]] @calls))))))

(defdescribe conversation-switcher-data-test
  (it "uses latest turn creation time as modification time and sorts newest first"
    (with-redefs [vis/db-list-conversation-turns (fn [_db-info conversation-id]
                                                   (case conversation-id
                                                     "old" [{:created-at #inst "2024-01-04T00:00:00.000-00:00"}]
                                                     "new" [{:created-at #inst "2024-01-02T00:00:00.000-00:00"}
                                                            {:created-at #inst "2024-01-08T00:00:00.000-00:00"}]
                                                     []))]
      (let [old-summary (conversation-summary :db {:id "old"
                                                   :created-at #inst "2024-01-01T00:00:00.000-00:00"})
            new-summary (conversation-summary :db {:id "new"
                                                   :created-at #inst "2024-01-03T00:00:00.000-00:00"})]
        (expect (= 1 (:turn-count old-summary)))
        (expect (= 2 (:turn-count new-summary)))
        (expect (= #inst "2024-01-08T00:00:00.000-00:00"
                  (:modified-at new-summary)))
        (expect (= ["new" "old"]
                  (mapv :id (latest-modified-first [old-summary new-summary]))))))))

(defdescribe submit-input-test
  (it "dispatches send before reset so paste placeholders can expand"
    (let [events      (atom [])
          payload     "therapy line 1\ntherapy line 2"
          token       (input/format-paste-placeholder {:id 1 :content payload})
          input-state (input/paste-text (input/empty-input) (str "context " token))]
      (with-redefs [state/dispatch (fn [event]
                                     (swap! events conj event))]
        (submit-input! {:conversation {:id "c1"}
                        :loading? false}
          input-state)
        (expect (= [[:send-message (str "context " token)]
                    [:reset-input]]
                  @events))))))

(defdescribe selectable-ranges-test
  (it "clips transcript selection to message content rows only"
    (expect (= [{:row 4 :col 4 :width 11}
                {:row 5 :col 4 :width 11}]
              (bubble-selectable-ranges
                {:visible [{:top -1
                            :height 4
                            :projected {:role :assistant
                                        :prewrapped-lines ["first" "second"]}}
                           {:top 4
                            :height 3
                            :projected {:role :assistant
                                        :prewrapped-lines ["below viewport"]}}]}
                4 5 20))))

  (it "does not mark role banners, padding, provider footers, or gap rows as selectable"
    (expect (= [{:row 6 :col 4 :width 11}]
              (bubble-selectable-ranges
                {:visible [{:top 0
                            :height 5
                            :projected {:role :user
                                        :text "siema"}}]}
                4 6 20))))

  (it "sorts conversations by real turns, latest modified time, then turn count by default"
    (let [old-with-turns {:id :old
                          :turn-count 1
                          :modified-at #inst "2024-01-02T00:00:00.000-00:00"
                          :created-at #inst "2024-01-01T00:00:00.000-00:00"}
          latest-empty {:id :empty
                        :turn-count 0
                        :modified-at #inst "2024-01-10T00:00:00.000-00:00"
                        :created-at #inst "2024-01-10T00:00:00.000-00:00"}
          latest-with-turns {:id :latest
                             :turn-count 2
                             :modified-at #inst "2024-01-03T00:00:00.000-00:00"
                             :created-at #inst "2024-01-01T00:00:00.000-00:00"}
          same-latest-more-turns {:id :more-turns
                                  :turn-count 5
                                  :modified-at #inst "2024-01-03T00:00:00.000-00:00"
                                  :created-at #inst "2024-01-01T00:00:00.000-00:00"}]
      (expect (= [1 1704240000000 2]
                (conversation-sort-key latest-with-turns)))
      (expect (= [:more-turns :latest :old :empty]
                (mapv :id
                  (latest-modified-first
                    [old-with-turns latest-empty latest-with-turns same-latest-more-turns]))))))

  (it "copies transcript content without role labels, answer separators, or model metadata"
    (let [ranges (bubble-selectable-ranges
                   {:visible [{:top 0
                               :height 6
                               :projected {:role :assistant
                                           :prewrapped-lines ["(answer (v/p \"hi\"))"
                                                              (str p/MARKER_ANSWER_SEP "")
                                                              (str p/MARKER_ANSWER_TXT "hi there")]}}]}
                   0 6 30)
          rows   ["  Vis                         "
                  "    (answer (v/p \"hi\"))    "
                  "──────────────────────────────"
                  "    hi there                  "
                  "          zai/glm · 1 iter    "
                  "                              "]]
      (expect (= "(answer (v/p \"hi\"))\nhi there"
                (selection/selected-text
                  rows
                  {:anchor (selection/point 0 0)
                   :focus  (selection/point 29 5)}
                  ranges)))))

  (it "marks whole visible bubble rectangles for single-click copy"
    (let [regions (bubble-copy-regions
                    {:visible [{:idx 0 :top 0 :height 5
                                :projected {:role :user
                                            :prewrapped-lines ["hello"]}}]}
                    [{:role :user :text "hello **markdown**"}]
                    4 6 20)]
      (expect (= [{:row 4 :col 2 :width 15
                   :height 4 :text "hello **markdown**"}]
                regions))
      (expect (= "hello **markdown**"
                (:text (bubble-copy-hit (selection/point 4 5) regions))))
      (expect (nil? (bubble-copy-hit (selection/point 1 5) regions)))
      (expect (nil? (bubble-copy-hit (selection/point 4 8) regions)))))

  (it "excludes turn separator rows from whole-bubble click copy hitboxes"
    (let [regions (bubble-copy-regions
                    {:visible [{:idx 0 :top 0 :height 7
                                :projected {:role :user
                                            :turn-separator? true
                                            :prewrapped-lines ["hello"]}}]}
                    [{:role :user :text "hello"}]
                    4 9 20)]
      (expect (= [{:row 6 :col 2 :width 15
                   :height 4 :text "hello"}]
                regions))
      (expect (nil? (bubble-copy-hit (selection/point 4 4) regions)))
      (expect (nil? (bubble-copy-hit (selection/point 4 5) regions)))
      (expect (= "hello"
                (:text (bubble-copy-hit (selection/point 4 6) regions))))))

  (it "marks input text rows as selectable without input padding"
    (expect (= [{:row 11 :col 2 :width 16}
                {:row 12 :col 2 :width 16}]
              (input-selectable-ranges 10 2 20)))))

(defdescribe clipboard-copy-actions-test
  (it "conversation-id copy uses the same icon-era notification TTL"
    (let [copied   (promise)
          notified (atom nil)]
      (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                 (deliver copied text)
                                                 true)
                       #'vis/notify!           (fn [text & kvs]
                                                 (reset! notified [text kvs]))}
        (fn []
          (copy-conversation-id! "123e4567-e89b-12d3-a456-426614174000")
          (expect (= "123e4567-e89b-12d3-a456-426614174000"
                    (deref copied 1000 ::timeout)))
          (expect (= ["✓ Copied conversation ID" [:level :success :ttl-ms 1500]]
                    @notified))))))

  (it "mouse selection copy uses the shared success notification contract"
    (let [copied   (promise)
          notified (atom nil)]
      (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                 (deliver copied text)
                                                 true)
                       #'vis/notify!           (fn [text & kvs]
                                                 (reset! notified [text kvs]))}
        (fn []
          (copy-selection! "selected text")
          (expect (= "selected text" (deref copied 1000 ::timeout)))
          (expect (= ["✓ Copied selection" [:level :success :ttl-ms 1500]]
                    @notified))))))

  (it "single-click bubble copy uses the shared success notification contract"
    (let [copied   (promise)
          notified (atom nil)]
      (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                 (deliver copied text)
                                                 true)
                       #'vis/notify!           (fn [text & kvs]
                                                 (reset! notified [text kvs]))}
        (fn []
          (copy-bubble! "whole bubble")
          (expect (= "whole bubble" (deref copied 1000 ::timeout)))
          (expect (= ["✓ Copied bubble" [:level :success :ttl-ms 1500]]
                    @notified))))))

  (it "input mouse selection copy names the input in the notification"
    (let [copied   (promise)
          notified (atom nil)]
      (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                 (deliver copied text)
                                                 true)
                       #'vis/notify!           (fn [text & kvs]
                                                 (reset! notified [text kvs]))}
        (fn []
          (copy-selection! "typed mistake" :input)
          (expect (= "typed mistake" (deref copied 1000 ::timeout)))
          (expect (= ["✓ Copied input selection" [:level :success :ttl-ms 1500]]
                    @notified))))))

  (it "conversation Markdown copy exports through the host projection"
    (let [copied   (promise)
          notified (promise)]
      (with-redefs-fn {#'vis/env-for (fn [conversation-id]
                                       (expect (= "conversation-1" conversation-id))
                                       {:db-info :db})
                       #'vis/conversation->markdown (fn [db-info conversation-id]
                                                      (expect (= :db db-info))
                                                      (expect (= "conversation-1" conversation-id))
                                                      "# Conversation")
                       #'input/clipboard-copy! (fn [text]
                                                 (deliver copied text)
                                                 true)
                       #'vis/notify! (fn [text & kvs]
                                       (deliver notified [text kvs]))}
        (fn []
          (copy-conversation-as-markdown! "conversation-1")
          (expect (= "# Conversation" (deref copied 1000 ::timeout)))
          (expect (= ["✓ Copied conversation as Markdown" [:level :success :ttl-ms 1500]]
                    (deref notified 1000 ::timeout)))))))

  (it "file click targets open through the editor path, not the generic URL opener"
    (let [editor-opened (promise)
          url-opened    (promise)]
      (with-redefs-fn {#'opener/open-file-in-editor! (fn [target]
                                                       (deliver editor-opened target)
                                                       {:status :ok})
                       #'opener/open!                (fn [target]
                                                       (deliver url-opened target)
                                                       {:status :ok})}
        (fn []
          (open-click-target! {:kind :file :url "deps.edn#L42"})
          (expect (= "deps.edn#L42" (deref editor-opened 1000 ::timeout)))
          (expect (= ::timeout (deref url-opened 100 ::timeout)))))))

  (it "URL click targets keep using the generic opener"
    (let [url-opened (promise)]
      (with-redefs-fn {#'opener/open! (fn [target]
                                        (deliver url-opened target)
                                        {:status :ok})}
        (fn []
          (open-click-target! {:kind :url :url "https://example.com"})
          (expect (= "https://example.com" (deref url-opened 1000 ::timeout))))))))

(defdescribe parse-args-test
  (it "no args -> empty opts map"
    (expect (= {} (parse-args []))))

  (it "--resume sets :resume true"
    (expect (= {:resume true} (parse-args ["--resume"]))))

  (it "--conversation-id captures the next token as the id"
    (expect (= {:conversation-id "abc123"}
              (parse-args ["--conversation-id" "abc123"]))))

  (it "--conversation-id + --resume coexist (caller decides precedence)"
    (expect (= {:conversation-id "abc123" :resume true}
              (parse-args ["--conversation-id" "abc123" "--resume"]))))

  (it "unknown flag throws :vis/user-error (regression: typo silently swallowed)"
    ;; `vis channels tui --conversations-id <uuid>` used to succeed
    ;; silently and start a fresh conversation. The user reported it
    ;; explicitly: the flag with a stray "s" must blow up.
    (expect (user-error?
              #(parse-args ["--conversations-id" "d8aff512-d60d-42b6-a009-041f1bec3891"]))))

  (it "unknown flag error message names the bad flag and shows usage"
    (try (parse-args ["--conversations-id" "x"])
      (expect false "expected ex-info")
      (catch clojure.lang.ExceptionInfo e
        (let [msg (.getMessage e)]
          (expect (re-find #"--conversations-id" msg))
          (expect (re-find #"Usage:" msg))))))

  (it "--conversation-id without a value -> :vis/user-error"
    (expect (user-error? #(parse-args ["--conversation-id"]))))

  (it "--conversation-id followed by another flag -> :vis/user-error (no value)"
    ;; Catches the case where the user types `--conversation-id --resume`
    ;; and `--resume` would otherwise be silently treated as the id.
    (expect (user-error? #(parse-args ["--conversation-id" "--resume"]))))

  (it "non-flag positional arg also errors (no positional API today)"
    (expect (user-error? #(parse-args ["stray-positional"])))))
