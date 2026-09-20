(ns com.blockether.vis.tui.annotator
  "Read and comment on versioned text artifacts without leaving the terminal."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.annotations :as annotations]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.contract.plan :as plan]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.external-opener :as opener]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.markdown-layout :as layout]
            [com.blockether.vis.tui.presentation :as ir]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [java.io File]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.security MessageDigest]))

(defn draft-file
  "One private draft per session/document, independent of the iteration that saved it."
  ^java.io.File [session-id filename]
  (let [digest
        (.digest (MessageDigest/getInstance "SHA-256")
                 (.getBytes (pr-str [session-id filename]) StandardCharsets/UTF_8))

        name
        (apply str (map #(format "%02x" (bit-and 255 (long %))) digest))]

    (io/file (System/getProperty "user.home") ".vis" "tui" "annotation-drafts" (str name ".edn"))))

(defn read-draft
  [session-id filename]
  (let [file (draft-file session-id filename)]
    (when (Files/isRegularFile (.toPath file) (make-array java.nio.file.LinkOption 0))
      (edn/read-string (slurp file :encoding "UTF-8")))))

(defn keep-draft!
  "Atomically keep committed remarks; saving the server revision consumes the draft."
  [session-id filename state]
  (let [file (draft-file session-id filename)]
    (if (:dirty? state)
      (do (io/make-parents file)
          (let [temporary (Files/createTempFile (.toPath (.getParentFile file))
                                                "draft-"
                                                ".tmp"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))]
            (try (spit (.toFile temporary)
                       (pr-str (select-keys state [:base-text :comments]))
                       :encoding
                       "UTF-8")
                 (Files/move temporary
                             (.toPath file)
                             (into-array java.nio.file.CopyOption
                                         [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
                 (finally (Files/deleteIfExists temporary)))))
      (Files/deleteIfExists (.toPath file)))))

(defn- passage-text [node] (annotations/quote-of (ir/search-text [:ast {} node])))

(defn document-blocks
  "Render whole canonical blocks; lists and tables offer individual items/cells for quoting."
  [body plain?]
  (mapv (fn [node]
          {:node node
           :quote (passage-text node)
           :passages (into []
                           (comp (filter #(and (vector? %) (#{:li :td :th} (first %))))
                                 (map passage-text)
                                 (remove str/blank?))
                           (tree-seq vector? #(drop 2 %) node))})
        (if plain?
          (map #(vector :p {} %) (str/split body #"\n" -1))
          (drop 2 (ir/markdown->ast body)))))

(defn- patch-file
  "The file a unified-diff header line names, relative to the tree the patch was
   taken in — nil for every other line. `+++` and `diff --git` name the file AFTER
   the change; `---` names it for a deletion, where the `+++` side is /dev/null.
   A git-quoted name (spaces, control characters) and anything that is not inside
   the tree — /dev/null included — are left alone rather than guessed at."
  [line]
  (when-let [named (or (second (re-matches #"diff --git a/\S+ b/(\S+)" (str line)))
                       (second (re-matches #"(?:\+\+\+|---) ([^\t]+)(?:\t.*)?" (str line))))]
    (let [path (str/replace-first named #"^[ab]/" "")]
      (when-not (or (str/blank? path)
                    (re-find #"\s" named)
                    (str/starts-with? path "/")
                    (some #{".."} (str/split path #"/")))
        path))))

(defn- named-file
  "The file the selected block names, for a patch snapshot only: every line of a
   patch is its own block, so the selection already IS the header the reader is
   standing on."
  [state plain?]
  (when (:diff state)
    (let [node
          (:node (nth (document-blocks (:body state) plain?) (:selected state) nil))

          text
          (when (vector? node) (nth node 2 nil))]

      (when (string? text) (patch-file text)))))

(defn initial-state
  [text version draft]
  (let [{:keys [body comments]}
        (annotations/parse-annotated text)

        restored?
        (and draft (= text (:base-text draft)) (vector? (:comments draft)))]

    {:body body
     :base-text text
     :comments (if restored? (:comments draft) comments)
     :version version
     :dirty? (boolean (and restored? (not= comments (:comments draft))))
     :selected 0
     :comment-index 0
     :focus :document
     :scroll 0
     :jump? true
     :note ""}))

(defn artifact-state
  "Read one version with explicit capability; malformed diffs fail closed."
  [row text draft]
  (let [commentable
        (true? (:commentable row))

        draft
        (when commentable draft)]

    (if (= diff/media-type (:media-type row))
      (try (let [envelope
                 (diff/parse! text)

                 comments
                 (mapv #(hash-map :quote (get % "quote") :body (get % "body"))
                       (get envelope "comments"))

                 restored?
                 (and draft (= text (:base-text draft)) (vector? (:comments draft)))]

             (assoc (initial-state "" (:version row) nil)
               :body (get envelope "patch")
               :base-text text
               :diff envelope
               :commentable commentable
               :comments (if restored? (:comments draft) comments)
               :dirty? (boolean (and restored? (not= comments (:comments draft))))))
           (catch clojure.lang.ExceptionInfo _
             (assoc (initial-state "This diff attachment is malformed. Ask for a new version."
                                   (:version row)
                                   nil)
               :commentable false
               :invalid-diff? true)))
      (assoc (initial-state text (:version row) draft) :commentable commentable))))

(defn- pending-review?
  [state]
  (or (:dirty? state) (:revision-pending? state) (seq (:comments state))))

(defn- review-actions
  [info state]
  (if (:diff state)
    (if (pending-review? state) [:revise] [])
    (plan/available-actions info (pending-review? state))))

(defn modal-component
  "Read and optionally review an explicitly commentable artifact with production rendering."
  [filename plain? plans? initial]
  (let [blocks
        (document-blocks (if (and (:diff initial) (empty? (:body initial)))
                           "No changes in this diff."
                           (:body initial))
                         plain?)

        commentable
        (true? (:commentable initial))

        info
        (if (:diff initial)
          {:kind :diff
           :status (str/join " · "
                             (remove nil?
                               [(get-in initial [:diff "source" "type"])
                                (get-in initial [:diff "source" "backend"])
                                (get-in initial [:diff "source" "label"])]))}
          (when plans? (plan/document-info filename (:body initial))))]

    {:init initial
     :measure (fn [state cols rows]
                (let [content-w
                      (dlg/default-content-width cols)

                      bounds
                      (dlg/dialog-bounds cols rows content-w (max 5 (- (long rows) 7)))

                      geom
                      (dlg/dialog-layout bounds)

                      width
                      (max 1 (- (long (:inner-w bounds)) 4))

                      lines
                      (vec (mapcat (fn [index block]
                                     (map #(assoc % :block index)
                                          (layout/ast->lines [:ast {} (:node block)] width)))
                                   (range)
                                   blocks))

                      notes-h
                      (min 3 (count (:comments state)))

                      doc-h
                      (max 1 (- (long (:content-h geom)) notes-h (if info 4 3)))]

                  (merge bounds
                         geom
                         {:cols cols
                          :rows rows
                          :content-w content-w
                          :height (max 5 (- (long rows) 7))
                          :lines lines
                          :doc-h doc-h
                          :notes-h notes-h
                          :blocks blocks})))
     :reconcile
     (fn [state {:keys [lines doc-h]}]
       (let [selected
             (p/clamp (long (:selected state)) 0 (max 0 (dec (count blocks))))

             start
             (or (first (keep-indexed #(when (= selected (:block %2)) %1) lines)) 0)

             scroll
             (if (and (:jump? state) (= :document (:focus state))) start (:scroll state))]

         (assoc state
           :selected selected
           :jump? false
           :comment-index
           (p/clamp (long (:comment-index state)) 0 (max 0 (dec (count (:comments state)))))
           :scroll (p/clamp (long scroll) 0 (max 0 (- (count lines) (long doc-h)))))))
     :paint (fn [g state
                 {:keys [cols rows content-w height left inner-w content-top hint-row lines doc-h
                         notes-h]}]
              (dlg/draw-dialog-chrome! g
                                       cols
                                       rows
                                       (case (:kind info)
                                         :plan
                                         "Specification"

                                         :implementation
                                         "Implementation record"

                                         :diff
                                         "Code changes"

                                         filename)
                                       content-w
                                       height)
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (when info
                (p/put-str! g
                            (+ (long left) 2)
                            content-top
                            (p/ellipsize (str filename " · v" (:version state))
                                         (- (long inner-w) 3))))
              (p/put-str! g
                          (+ (long left) 2)
                          (+ (long content-top) (if info 1 0))
                          (p/ellipsize
                            (str (if info (:status info) (str "v" (:version state)))
                                 (if commentable
                                   (str " · "
                                        (count (:comments state))
                                        (if (= 1 (count (:comments state))) " comment" " comments")
                                        (when (:dirty? state) " · Unsaved draft"))
                                   " · Read only"))
                            (- (long inner-w) 3)))
              (doseq [[i line] (map-indexed vector (take doc-h (drop (:scroll state) lines)))]
                (let [row (+ (long content-top) (if info 2 1) (long i))]
                  (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                  (p/put-str! g
                              (inc (long left))
                              row
                              (if (and (or commentable (:diff state))
                                       (= :document (:focus state))
                                       (= (:selected state) (:block line)))
                                ">"
                                " "))
                  (reduce (fn [x run]
                            (dlg/md-run-paint! g x row run))
                          (+ (long left) 3)
                          (:runs line))))
              (let [first-comment (max 0 (inc (- (long (:comment-index state)) (long notes-h))))]
                (doseq [[i comment]
                        (map-indexed vector (take notes-h (drop first-comment (:comments state))))]
                  (dlg/draw-selectable-row!
                    g
                    left
                    (+ (long content-top) (if info 2 1) (long doc-h) (long i))
                    inner-w
                    (and (= :comments (:focus state))
                         (= (:comment-index state) (+ first-comment (long i))))
                    (str (inc (+ first-comment (long i)))
                         ". " (if (str/blank? (:quote comment)) "Whole document" (:quote comment))
                         " — " (:body comment)))))
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g
                          (+ (long left) 2)
                          (- (long hint-row) 1)
                          (p/ellipsize (or (not-empty (:note state))
                                           (when commentable
                                             (if info
                                               (when-not (:sent? state)
                                                 (case (first (review-actions info state))
                                                   :approve
                                                   "a Approve and start"

                                                   :revise
                                                   "r Send for revision"

                                                   "Add comments to request changes"))
                                               "s Save · w Whole")))
                                       (- (long inner-w) 3)))
              (dlg/draw-hint-bar! g
                                  left
                                  hint-row
                                  inner-w
                                  (cond-> (if commentable
                                            [["Enter" "comment"] ["Tab" "notes"] ["w" "whole"]
                                             ["?" "help"]]
                                            [["↑↓" "scroll"] ["Esc" "close"]])
                                    (:diff state)
                                    (conj ["o" "open file"])))
              nil)
     :on-key
     (fn [state ^KeyStroke key {:keys [doc-h]}]
       (let [done
             (fn [action]
               {::dlg/done {:action action :state state}})

             selection
             (if (= :comments (:focus state)) :comment-index :selected)

             action
             (case (.getCharacter key)
               \r
               :revise

               \a
               :approve

               nil)]

         (cond (= KeyType/Escape (.getKeyType key)) (done :close)
               (and (= \o (.getCharacter key)) (named-file state plain?)) (done :open)
               (and (not commentable)
                    (not (#{KeyType/ArrowUp KeyType/ArrowDown KeyType/PageUp KeyType/PageDown}
                          (.getKeyType key))))
               state
               (= KeyType/Tab (.getKeyType key))
               (assoc state :focus (if (= :document (:focus state)) :comments :document))
               (= KeyType/ArrowUp (.getKeyType key)) (-> state
                                                         (update selection #(dec (long %)))
                                                         (assoc :jump? true))
               (= KeyType/ArrowDown (.getKeyType key)) (-> state
                                                           (update selection #(inc (long %)))
                                                           (assoc :jump? true))
               (= KeyType/PageUp (.getKeyType key))
               (update state :scroll #(- (long %) (long doc-h)))
               (= KeyType/PageDown (.getKeyType key))
               (update state :scroll #(+ (long %) (long doc-h)))
               (= KeyType/Enter (.getKeyType key))
               (done (if (= :comments (:focus state)) :edit :comment))
               (= KeyType/Delete (.getKeyType key))
               (if (= :comments (:focus state)) (done :delete) state)
               (= \w (.getCharacter key)) (done :whole)
               (= \s (.getCharacter key)) (if (and (nil? info) (:dirty? state)) (done :save) state)
               (= \? (.getCharacter key)) (done :help)
               (and action (not (:sent? state)) (some #{action} (review-actions info state)))
               (done action)
               :else state)))}))

(defn save-state!
  "Only a confirmed saved version clears dirty state."
  [session-id row state]
  (when-not (true? (:commentable row))
    (throw (ex-info "This attachment is read only" {:type :attachment/read-only})))
  (let [text
        (if-let [envelope (:diff state)]
          (diff/render (diff/with-comments envelope (:comments state)))
          (annotations/render-annotated (:body state) (:comments state)))

        saved
        (vis/save-artifact-text! session-id
                                 (:iteration-id row)
                                 (:filename row)
                                 (:media-type row)
                                 text)]

    (assoc state
      :dirty? false
      :base-text text
      :commentable true
      :version (get saved "version")
      :note (str "Saved as v" (get saved "version")))))

(defn send-state!
  "Save before revision; retain that version on a failed send so retry never saves twice."
  [session-id row state action]
  (when-not (and (true? (:commentable row))
                 (or (:diff state) (vis/toggle-enabled? "plans"))
                 (not (:sent? state))
                 (some #{action}
                       (review-actions (plan/document-info (:filename row) (:body state)) state)))
    (throw (ex-info "This review action is not available" {})))
  (let [saved (cond-> (if (:dirty? state) (save-state! session-id row state) state)
                (= :revise action)
                (assoc :revision-pending? true))]
    (try (let [result (vis/submit-turn!
                        session-id
                        {:request
                         (if (:diff saved)
                           (diff/review-request (:filename row) (:version saved))
                           (plan/action-request (:filename row) (:version saved) action))})]
           (if (get-in result [:turn "turn_id"])
             (assoc saved
               :sent? true
               :note (str "Sent v" (:version saved) " to agent"))
             (assoc saved :note "Could not send. Retry when the session is ready.")))
         (catch Exception _
           (assoc saved :note "Could not send. Saved version kept; retry when ready.")))))

(defn- edit-comment!
  [screen state quote index]
  (if-let [body (dlg/text-input-dialog! screen
                                        (if (some? index) "Edit comment" "Add comment")
                                        "Comment"
                                        :body (if (str/blank? quote) "Whole document" quote)
                                        :initial (if (some? index)
                                                   (:body (nth (:comments state) index))
                                                   ""))]
    (if (str/blank? body)
      state
      (-> state
          (update :comments
                  #(if (some? index)
                     (assoc % index {:quote quote :body body})
                     (conj % {:quote quote :body body})))
          (assoc :dirty? true
                 :sent? false
                 :note "")))
    state))

(defn- open-named-file!
  "Open the file the selected patch header names, in the editor on THIS machine.
   A patch is a snapshot of a tree the session works in, and its headers are the
   only address it carries, so the path is resolved against that session's own
   workspace root. Says what happened in the note line; never throws."
  [session-id state plain?]
  (if-let [named (named-file state plain?)]
    (let [root (try (get (vis/session-workspace-info session-id) "root") (catch Exception _ nil))
          ^File target
          (if (str/blank? (str root)) (File. (str named)) (File. (str root) (str named)))]

      (if-not (.isFile target)
        (assoc state :note (str "No such file: " named))
        (let [{:keys [status error]} (opener/open-file-in-editor! (.getPath target))]
          (assoc state
            :note
            (if (= :ok status) (str "Opening " named) (or error "File could not be opened"))))))
    state))

(defn show!
  "Fetch, review, comment and send a durable artifact through the existing gateway routes."
  [^TerminalScreen screen session-id row]
  (if-let [bytes
           (vis/gateway-iteration-attachment-bytes session-id (:iteration-id row) (:index row))]
    (let [text (String. ^bytes bytes StandardCharsets/UTF_8)
          filename (:filename row)
          plain? (not (or (= "text/markdown" (:media-type row)) (str/ends-with? filename ".md")))
          draft (when (true? (:commentable row))
                  (try (read-draft session-id filename) (catch Exception _ ::unreadable)))
          stale? (and draft (not= text (:base-text draft)))
          open? (or (not stale?)
                    (= :discard
                       (:action (dlg/select-dialog!
                                  screen
                                  "Draft needs review"
                                  [{:label "Keep existing draft and close" :action :keep}
                                   {:label "Discard old draft and open this version"
                                    :action :discard}]))))]

      (when open?
        (loop [state (artifact-state row text (when-not stale? draft))]
          (let
            [component (modal-component filename plain? (vis/toggle-enabled? "plans") state)
             {:keys [action state]} (dlg/run-modal! screen component)
             _ (when (#{:save :revise :approve} action)
                 (let [size (.getTerminalSize screen)
                       busy (assoc state
                              :note (if (= :save action)
                                      "Saving changes…"
                                      "Saving and sending this version…"))
                       geom ((:measure component) busy (.getColumns size) (.getRows size))]

                   ((:paint component)
                     (frame/surface-graphics screen (.getColumns size) (.getRows size))
                     busy
                     geom)
                   (.refresh screen)))
             next-state
             (try
               (case action
                 :comment
                 (let [{:keys [quote passages]}
                       (nth (document-blocks (:body state) plain?) (:selected state) nil)
                       quote (if (seq passages)
                               (:label (dlg/select-dialog! screen
                                                           "Select passage"
                                                           (mapv #(hash-map :label %) passages)))
                               quote)]

                   (if quote (edit-comment! screen state quote nil) state))

                 :whole
                 (edit-comment! screen state "" nil)

                 :edit
                 (if-let [comment (nth (:comments state) (:comment-index state) nil)]
                   (edit-comment! screen state (:quote comment) (:comment-index state))
                   state)

                 :delete
                 (if (nth (:comments state) (:comment-index state) nil)
                   (assoc state
                     :comments (into []
                                     (keep-indexed #(when (not= %1 (:comment-index state)) %2))
                                     (:comments state))
                     :dirty? true
                     :sent? false
                     :note "")
                   state)

                 :open
                 (open-named-file! session-id state plain?)

                 :save
                 (save-state! session-id row state)

                 (:revise :approve)
                 (send-state! session-id row state action)

                 :help
                 (do
                   (dlg/text-viewer-dialog!
                     screen
                     "Document keys"
                     (str
                       "Up/Down: choose a block or comment\nPage Up/Down: scroll\nEnter: quote a block or edit a comment\nTab: switch document/comments\nw: whole-document comment\nDelete: remove selected comment\n"
                       (if (:diff state)
                         "o: open the file this line names\nr: send comments for revision\n"
                         (if (and (vis/toggle-enabled? "plans")
                                  (plan/document-info filename (:body state)))
                           "r: send comments for revision\na: approve this specification and start implementation\n"
                           "s: save as a new version\n"))
                       "Esc: close and keep draft"))
                   state)

                 state)
               (catch Exception _ (assoc state :note "Could not save. Your draft is kept; retry.")))
             persisted
             (or (not (true? (:commentable state)))
                 (try (keep-draft! session-id filename next-state) true (catch Exception _ false)))]

            (when (or (not= action :close) (not persisted))
              (recur (cond-> next-state
                       (not persisted)
                       (assoc :note "Draft could not be kept. Save before closing."))))))))
    (dlg/text-viewer-dialog! screen
                             "Artifact unavailable"
                             "Could not load this version. Close and retry.")))
