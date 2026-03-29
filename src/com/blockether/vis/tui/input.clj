(ns com.blockether.vis.tui.input
  (:require [clojure.string :as str])
  (:import [com.googlecode.lanterna.input
            CharacterPattern CharacterPattern$Matching
            KeyDecodingProfile KeyStroke KeyType
            MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.ansi UnixTerminal]
           [java.awt Toolkit]
           [java.awt.datatransfer DataFlavor StringSelection]))

;;; ── Custom input patterns ──────────────────────────────────────────────────
;; Lanterna's AltAndCharacterPattern rejects ISO control chars (\n, \r, \t).
;; Alt+Enter sends ESC + '\n' which gets rejected. We fix it here.

(def ^:private ESC_CHAR (Character. (char 0x1b)))

(def alt-enter-pattern
  (reify CharacterPattern
    (match [_ seq]
      (let [size (.size seq)]
        (cond
          (and (= size 2)
               (= (.get seq 0) ESC_CHAR)
               (let [c (.get seq 1)]
                 (or (= c (Character. \newline))
                     (= c (Character. \return)))))
          (CharacterPattern$Matching.
           (KeyStroke. KeyType/Enter false true))

          (and (= size 1)
               (= (.get seq 0) ESC_CHAR))
          CharacterPattern$Matching/NOT_YET

          :else nil)))))

(defn register-custom-patterns!
  "Register Alt+Enter pattern on terminal's input decoder."
  [^UnixTerminal terminal]
  (.addProfile (.getInputDecoder terminal)
               (reify KeyDecodingProfile
                 (getPatterns [_] [alt-enter-pattern]))))

;;; ── Clipboard (java.awt.datatransfer) ──────────────────────────────────────

(defn clipboard-paste []
  (try
    (let [cb   (.getSystemClipboard (Toolkit/getDefaultToolkit))
          data (.getContents cb nil)]
      (when (and data (.isDataFlavorSupported data DataFlavor/stringFlavor))
        (let [text (.getTransferData data DataFlavor/stringFlavor)]
          (when (seq text) text))))
    (catch Exception _ nil)))

(defn clipboard-copy! [^String text]
  (try
    (let [cb  (.getSystemClipboard (Toolkit/getDefaultToolkit))
          sel (StringSelection. text)]
      (.setContents cb sel nil))
    (catch Exception _ nil)))

;;; ── Input buffer state ─────────────────────────────────────────────────────

(defn empty-input []
  {:lines [""] :crow 0 :ccol 0})

(defn input->text [{:keys [lines]}]
  (str/join "\n" lines))

(defn insert-char [{:keys [lines crow ccol] :as st} ch]
  (let [line     (nth lines crow)
        new-line (str (subs line 0 ccol) ch (subs line ccol))]
    (-> st
        (assoc-in [:lines crow] new-line)
        (update :ccol inc))))

(defn insert-newline [{:keys [lines crow ccol] :as st}]
  (let [line   (nth lines crow)
        before (subs line 0 ccol)
        after  (subs line ccol)]
    (-> st
        (assoc :lines (into (conj (subvec lines 0 crow) before)
                            (cons after (subvec lines (inc crow)))))
        (assoc :crow (inc crow))
        (assoc :ccol 0))))

(defn delete-backward [{:keys [lines crow ccol] :as st}]
  (cond
    (pos? ccol)
    (let [line (nth lines crow)]
      (-> st
          (assoc-in [:lines crow] (str (subs line 0 (dec ccol)) (subs line ccol)))
          (update :ccol dec)))

    (pos? crow)
    (let [prev (nth lines (dec crow))
          curr (nth lines crow)]
      (-> st
          (assoc :lines (into (conj (subvec lines 0 (dec crow)) (str prev curr))
                              (subvec lines (inc crow))))
          (assoc :crow (dec crow))
          (assoc :ccol (count prev))))

    :else st))

(defn move-left [{:keys [lines crow ccol] :as st}]
  (cond
    (pos? ccol)  (update st :ccol dec)
    (pos? crow)  (-> st (update :crow dec) (assoc :ccol (count (nth lines (dec crow)))))
    :else        st))

(defn move-right [{:keys [lines crow ccol] :as st}]
  (let [line (nth lines crow)]
    (cond
      (< ccol (count line))         (update st :ccol inc)
      (< crow (dec (count lines)))  (-> st (update :crow inc) (assoc :ccol 0))
      :else                         st)))

(defn move-up [{:keys [lines crow ccol] :as st}]
  (if (pos? crow)
    (-> st (update :crow dec) (assoc :ccol (min ccol (count (nth lines (dec crow))))))
    st))

(defn move-down [{:keys [lines crow ccol] :as st}]
  (if (< crow (dec (count lines)))
    (-> st (update :crow inc) (assoc :ccol (min ccol (count (nth lines (inc crow))))))
    st))

(defn paste-text [{:keys [lines crow ccol] :as st} text]
  (let [paste-lines  (str/split text #"\r?\n" -1)
        current-line (nth lines crow)
        before       (subs current-line 0 ccol)
        after        (subs current-line ccol)]
    (if (= 1 (count paste-lines))
      (-> st
          (assoc-in [:lines crow] (str before (first paste-lines) after))
          (assoc :ccol (+ ccol (count (first paste-lines)))))
      (let [first-l  (str before (first paste-lines))
            last-l   (str (last paste-lines) after)
            mid      (subvec (vec paste-lines) 1 (dec (count paste-lines)))
            new-crow (+ crow (dec (count paste-lines)))]
        (-> st
            (assoc :lines (into (conj (subvec lines 0 crow) first-l)
                                (concat mid [last-l] (subvec lines (inc crow)))))
            (assoc :crow new-crow)
            (assoc :ccol (count (last paste-lines))))))))

;;; ── Key handling ───────────────────────────────────────────────────────────

(defn handle-key
  "Process keystroke. Returns {:action kw, :state s}."
  [key state]
  (let [ktype (.getKeyType key)]
    (condp = ktype
      KeyType/Escape
      {:action :continue :state state}

      KeyType/Character
      (let [c    (.getCharacter key)
            ctrl (.isCtrlDown key)]
        (cond
          (and ctrl (= c \c)) {:action :quit :state state}

          (and ctrl (= c \v)) (if-let [t (clipboard-paste)]
                                {:action :continue :state (paste-text state t)}
                                {:action :continue :state state})

          (and ctrl (= c \x)) {:action :send :state state}

          (and ctrl (= c \p)) {:action :show-provider :state state}

          (and ctrl (= c \y)) {:action :show-copy :state state}

          :else {:action :continue :state (insert-char state c)}))

      KeyType/Enter
      (if (.isAltDown key)
        {:action :continue :state (insert-newline state)}
        {:action :send :state state})

      KeyType/Backspace  {:action :continue :state (delete-backward state)}
      KeyType/ArrowLeft  {:action :continue :state (move-left state)}
      KeyType/ArrowRight {:action :continue :state (move-right state)}
      KeyType/ArrowUp    {:action :continue :state (move-up state)}
      KeyType/ArrowDown  {:action :continue :state (move-down state)}
      KeyType/PageUp     {:action :scroll-up :state state}
      KeyType/PageDown   {:action :scroll-down :state state}

      KeyType/MouseEvent
      (let [^MouseAction mouse key]
        (condp = (.getActionType mouse)
          MouseActionType/SCROLL_UP   {:action :scroll-up :state state}
          MouseActionType/SCROLL_DOWN {:action :scroll-down :state state}
          {:action :continue :state state}))

      {:action :continue :state state})))

;;; ── Message formatting ─────────────────────────────────────────────────────

(defn format-message [text]
  (let [ls (str/split-lines text)]
    (into [(str "you: " (first ls))]
          (map #(str "     " %) (rest ls)))))
