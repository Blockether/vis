(ns com.blockether.vis.internal.context.language-test
  "Reply-language detection for user requests and progress notes. The samples
   come from real sessions where a model wrote its notes in German or English
   while the user wrote Polish."
  (:require [com.blockether.vis.internal.context.language :as language]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe
  detect-test
  (describe
    "Latin text"
    (it "finds Polish, also without diacritics"
        (expect (= "Polish"
                   (:name
                     (language/detect
                       "Wyslec kiedy zaczely sie pojawiac niemieckie wypowiedzi w naszych sesja"))))
        (expect (= "Polish"
                   (:name (language/detect
                            "Asercja nie przeszła. Sprawdzam, który warunek nie pasuje.")))))
    (it
      "finds German and English progress notes"
      (expect
        (=
          "German"
          (:name
            (language/detect
              "Overlays sind geschrieben. Jetzt prüfe ich den Diff und kompiliere beide Dateien."))))
      (expect (= "English"
                 (:name
                   (language/detect
                     "Now I check the diff of the overlay and compile both files in one JVM."))))))
  (describe "non-Latin text"
            (it "finds the script language"
                (expect (= "Russian"
                           (:name (language/detect
                                    "Сейчас я проверяю изменения и запускаю тесты."))))
                (expect (= "Ukrainian"
                           (:name (language/detect "Зараз я перевіряю зміни і запускаю тести."))))
                (expect (= "Japanese" (:name (language/detect "これから変更を確認して、テストを実行します。"))))))
  (describe
    "unclear text"
    (it "answers nil for a short request"
        (expect (nil? (language/detect "continue")))
        (expect (nil? (language/detect "CONTINUE END TO END"))))
    (it
      "ignores fenced code, inline code and links"
      (expect
        (nil?
          (language/detect
            "```python\nprint('the and is of in it this that with for')\n```\n`the and is` https://example.com/the/and/is"))))))

(defdescribe
  reply-language-test
  (it "takes the language of the latest clear request"
      (expect (= "English"
                 (language/reply-language
                   ["Sprawdź, czy to jest już naprawione."
                    "Please check how the agents are spawned and why it is slow."]))))
  (it "keeps the earlier language after a short request"
      (expect (= "Polish"
                 (language/reply-language ["Sprawdź, czy to jest już naprawione." "continue"
                                           "KONTYNUUJ"]))))
  (it "keeps the earlier language when the request quotes another language"
      (expect
        (= "Polish"
           (language/reply-language
             ["Sprawdź, czy to jest już naprawione."
              (str
                "Dlaczego my nagle mamy to po niemiecku? Co tu nie jest tak w prompcie?\n"
                "Ich prüfe den Dienst. Er darf nur das tun, was er tun soll, und keine Prozesse.\n"
                "Zuerst lese ich die Regeln und den Zustand auf beiden Seiten.")]))))
  (it "answers nil when no request shows a language"
      (expect (nil? (language/reply-language ["continue" "/reload"])))
      (expect (nil? (language/reply-language [])))))

(defdescribe
  drift-line-test
  (it
    "names both languages when a note leaves the reply language"
    (expect
      (= (str "# Reply language: Polish. Your last note was in German. "
              "Write the next notes and the final answer in Polish.")
         (language/drift-line
           "Polish"
           "Ersetzungen für Tag und Binding-Tabelle passten nicht. Ich baue beide Overlays neu."))))
  (it "accepts a note in the reply language with technical terms"
      (expect (nil?
                (language/drift-line
                  "Polish"
                  "Statusy: `queued`, `running`. Teraz sprawdzam format `want` w scenariuszu."))))
  (it "stays silent without a reply language or a clear note"
      (expect (nil? (language/drift-line
                      nil
                      "Now I check the diff of the overlay and compile both files.")))
      (expect (nil? (language/drift-line "Polish" "Done.")))))
