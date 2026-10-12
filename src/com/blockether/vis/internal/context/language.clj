(ns com.blockether.vis.internal.context.language
  "Reply language for user requests and progress notes.

   The CORE prompt says that the reply language is the language of the user's
   latest request. Some models still drift to another language in a long turn.
   A drift sticks, because the next note copies the language of the last note.
   The host therefore names the reply language at each turn boundary, and it
   adds one correction line to the tool result of an iteration whose note left
   that language.

   Detection is a small and conservative profile match. Non-Latin text matches
   by script. Latin text matches by common function words and by letters that
   only one profiled language uses. Short or mixed text gives nil, and the
   caller keeps the earlier language."
  (:require [clojure.string :as str]))

(def ^:private language-names
  {"ar" "Arabic"
   "cs" "Czech"
   "de" "German"
   "el" "Greek"
   "en" "English"
   "es" "Spanish"
   "fr" "French"
   "he" "Hebrew"
   "hi" "Hindi"
   "it" "Italian"
   "ja" "Japanese"
   "ko" "Korean"
   "nl" "Dutch"
   "pl" "Polish"
   "pt" "Portuguese"
   "ru" "Russian"
   "th" "Thai"
   "uk" "Ukrainian"
   "zh" "Chinese"})

(def ^:private name->code
  (into {}
        (map (fn [[code language]]
               [language code]))
        language-names))

(def ^:private function-words
  "Common words of each profiled Latin language. A word that two profiled
   languages use often, such as `to`, `i` or `do`, is in neither list."
  {"cs" #{"je" "jsem" "není" "nebo" "když" "jsou" "také" "jsme" "jste" "který" "protože" "však"
          "ještě" "pro" "tady"}
   "de" #{"und" "nicht" "ich" "ist" "das" "die" "der" "den" "dem" "ein" "eine" "mit" "für" "auch"
          "noch" "jetzt" "wird" "sind" "bitte" "warum" "wie" "kann" "soll" "schon" "aber" "oder"
          "wenn" "zuerst" "danach" "prüfe" "lese"}
   "en" #{"the" "and" "is" "are" "of" "in" "it" "this" "that" "with" "for" "not" "you" "what" "how"
          "why" "can" "please" "should" "would" "there" "we" "now" "then" "next" "first" "will"
          "from" "these" "after" "before"}
   "es" #{"el" "los" "las" "que" "por" "para" "con" "una" "pero" "lo" "del" "como" "esta" "este"
          "muy" "también"}
   "fr" #{"le" "les" "et" "est" "que" "pour" "avec" "une" "des" "pas" "je" "vous" "il" "elle" "mais"
          "dans" "sur" "ce" "cette" "qui"}
   "it" #{"il" "lo" "gli" "che" "non" "per" "con" "una" "sono" "della" "questo" "questa" "come" "ma"
          "anche"}
   "nl" #{"het" "een" "en" "van" "niet" "dat" "met" "voor" "zijn" "maar" "ook" "dit" "wat"}
   "pl" #{"nie" "jest" "sie" "się" "ze" "że" "na" "w" "z" "co" "jak" "tak" "czy" "juz" "już" "ale"
          "kiedy" "jeszcze" "tego" "mi" "sobie" "mam" "zeby" "żeby" "bo" "tu" "teraz" "masz" "jakie"
          "jaki" "wiec" "więc" "ten" "oraz" "dla" "przez" "po" "od" "tylko" "tez" "też" "moze"
          "może" "trzeba" "dobra" "mamy" "sprawdzam" "czytam"}
   "pt" #{"os" "que" "não" "nao" "para" "com" "uma" "mas" "isso" "este" "esta" "como" "você" "voce"
          "também"}})

(def ^:private marker-letters
  "Letters that only one profiled Latin language uses. Each letter adds one point."
  {"cs" #"[ěřůť]" "de" #"[äöüß]" "pl" #"[ąćęłńśźż]"})

(def ^:private scripts
  "Non-Latin scripts in match order. Kana comes before Han, because Japanese
   text also uses Han characters."
  [["ja" #"[\u3040-\u30ff]"] ["ko" #"[\uac00-\ud7af]"] ["zh" #"[\u4e00-\u9fff]"]
   ["ru" #"[\u0400-\u04ff]"] ["el" #"[\u0370-\u03ff]"] ["ar" #"[\u0600-\u06ff]"]
   ["he" #"[\u0590-\u05ff]"] ["th" #"[\u0e00-\u0e7f]"] ["hi" #"[\u0900-\u097f]"]])

(def ^:private min-script-chars
  "Characters of one script that make non-Latin text count as that script."
  8)

(def ^:private min-score "Points that the best Latin profile needs." 2)

(defn- prose
  "Text without fenced blocks, inline code and links: the words a person wrote."
  [text]
  (-> (str text)
      (str/replace #"(?ms)^[ \t]*(`{3,}|~{3,}).*?^[ \t]*\1[ \t]*$" " ")
      (str/replace #"`[^`\n]*`" " ")
      (str/replace #"https?://\S+" " ")))

(defn- script-scores
  [text]
  (into {}
        (keep (fn [[code re]]
                (let [n (count (re-seq re text))]
                  (when (pos? n) [code n]))))
        scripts))

(defn- latin-scores
  [text]
  (let [lower
        (str/lower-case text)

        words
        (re-seq #"\p{L}+" lower)]

    (into {}
          (map (fn [[code vocabulary]]
                 [code
                  (+ (count (filter vocabulary words))
                     (long (if-let [re (get marker-letters code)]
                             (count (re-seq re lower))
                             0)))]))
          function-words)))

(defn- script-language
  [text scores]
  (let [[code n]
        (first (sort-by (comp - val) scores))

        latin
        (count (re-seq #"[A-Za-z]" text))]

    (when (and code (>= (long n) (long min-script-chars)) (>= (long n) latin))
      (if (and (= "ru" code) (re-find #"[іїєґІЇЄҐ]" text)) "uk" code))))

(defn detect
  "Language of `text` as `{:code \"pl\" :name \"Polish\" :scores {...}}`, or nil when
   the text is short, mixed or unknown. `:scores` holds the points of each
   profile, so a caller can see if another language is also present."
  [text]
  (let [text
        (prose text)

        by-script
        (script-scores text)]

    (if-let [code (script-language text by-script)]
      {:code code :name (get language-names code) :scores by-script}
      (let [scores (latin-scores text)
            [[best best-score] [_ runner-up]] (sort-by (comp - val) scores)]

        (when (and (>= (long best-score) (long min-score))
                   (>= (long best-score) (* 2 (long runner-up))))
          {:code best :name (get language-names best) :scores scores})))))

(defn reply-language
  "Reply language after the user's `requests`, oldest first, as a language name,
   or nil when no request shows one. A short request keeps the earlier
   language. A mixed request that still uses the earlier language also keeps it.
   This follows the reply-language rule of the CORE prompt."
  [requests]
  (some-> (reduce (fn [current request]
                    (let [{:keys [code scores]} (detect request)]
                      (cond (nil? code) current
                            (nil? current) code
                            (= current code) current
                            (>= (long (get scores current 0)) (long min-score)) current
                            :else code)))
                  nil
                  requests)
          language-names))

(defn drift-line
  "One correction line when the progress note `note` is not in `reply-language`
   (a language name), else nil. A note that also uses the reply language is not
   a drift."
  [reply-language note]
  (when-let [reply-code (get name->code reply-language)]
    (let [{:keys [code scores] note-language :name} (detect note)]
      (when (and code (not= code reply-code) (< (long (get scores reply-code 0)) (long min-score)))
        (str "# Reply language: "
             reply-language
             ". Your last note was in "
             note-language
             ". Write the next notes and the final answer in "
             reply-language
             ".")))))
