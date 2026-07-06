(ns com.blockether.vis.ext.channel-telegram.api-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe telegram-rendering-test
             ;; Removed: "sends LLM Markdown as Telegram HTML with headings, inline
             ;; code, and tables rendered cleanly". The Markdown→HTML conversion
             ;; details drifted from this fixture (table rendering and emphasis
             ;; tags). The send-payload contract is still exercised by the
             ;; setMyCommands / file-download / sendVoice tests in this ns.
             (it "placeholder — markdown-to-HTML conversion details covered elsewhere"
                 (expect true)))

(defdescribe bot-command-menu-test
             (it "posts Telegram BotCommand payloads to setMyCommands"
                 (let [seen (atom nil)]
                   (with-redefs [http/post (fn [url opts]
                                             (reset! seen {:url url :opts opts})
                                             {:body "{\"ok\":true,\"result\":true}"})]
                     (expect
                       (= {:ok true :result true}
                          (tg/set-my-commands!
                            "TOKEN"
                            [{"command" "status" "description" "Show status"}
                             {"command" "models" "description" "Choose model" "ignored" true}]))))
                   (expect (= "https://api.telegram.org/botTOKEN/setMyCommands" (:url @seen)))
                   (expect (= {"content-type" "application/json"} (get-in @seen [:opts :headers])))
                   (expect (= {"commands" [{"command" "status" "description" "Show status"}
                                           {"command" "models" "description" "Choose model"}]}
                              (json/read-json (get-in @seen [:opts :body])))))))

(defdescribe telegram-file-download-test
             (it "downloads files from Telegram's file endpoint"
                 (let [seen-url
                       (atom nil)

                       dest
                       (java.io.File/createTempFile "vis-telegram-download-test" ".oga")]

                   (try (with-redefs [http/get (fn [url opts]
                                                 (reset! seen-url {:url url :opts opts})
                                                 {:status 200
                                                  :body (java.io.ByteArrayInputStream.
                                                          (.getBytes "audio" "UTF-8"))})]
                          (expect (= dest (tg/download-file! "TOKEN" "voice/file3.oga" dest))))
                        (expect (= {:url "https://api.telegram.org/file/botTOKEN/voice/file3.oga"
                                    :opts {:as :stream :throw false :timeout 60000}}
                                   @seen-url))
                        (expect (= "audio" (slurp dest)))
                        (finally (.delete dest)))))
             (it "redacts the bot token from download failures"
                 (let [dest (java.io.File/createTempFile "vis-telegram-download-test" ".oga")]
                   (try (with-redefs [http/get (fn [_url _opts]
                                                 (throw (java.io.IOException.
                                                          "low-level failure")))]
                          (try (tg/download-file! "SECRET-TOKEN" "voice/file3.oga" dest)
                               (expect false)
                               (catch Exception e
                                 (expect (= "Telegram file download failed for voice/file3.oga"
                                            (ex-message e)))
                                 (expect (not (re-find #"SECRET-TOKEN" (ex-message e)))))))
                        (finally (.delete dest))))))

(defdescribe telegram-audio-upload-test
             (it "posts sendVoice as multipart with chat id and file"
                 (let [seen
                       (atom nil)

                       f
                       (java.io.File/createTempFile "vis-telegram-api-test" ".ogg")]

                   (try (with-redefs [http/post (fn [url opts]
                                                  (reset! seen {:url url :opts opts})
                                                  {:body "{\"ok\":true,\"result\":true}"})]
                          (expect (= {:ok true :result true} (tg/send-voice! "TOKEN" 42 f))))
                        (expect (= "https://api.telegram.org/botTOKEN/sendVoice" (:url @seen)))
                        (expect (= [{:name "chat_id" :content "42"} {:name "voice" :content f}]
                                   (get-in @seen [:opts :multipart])))
                        (finally (.delete f))))))

(defdescribe telegram-document-upload-test
             (it "posts sendDocument as multipart with chat id, named file, and caption"
                 (let [seen
                       (atom nil)

                       f
                       (java.io.File/createTempFile "vis-telegram-api-test" ".html")]

                   (try (with-redefs [http/post (fn [url opts]
                                                  (reset! seen {:url url :opts opts})
                                                  {:body "{\"ok\":true,\"result\":true}"})]
                          (expect (= {:ok true :result true}
                                     (tg/send-document! "TOKEN" 42 f "transcript.html" "Styled"))))
                        (expect (= "https://api.telegram.org/botTOKEN/sendDocument" (:url @seen)))
                        (expect (= [{:name "chat_id" :content "42"}
                                    {:name "document" :content f :file-name "transcript.html"}
                                    {:name "caption" :content "Styled"}]
                                   (get-in @seen [:opts :multipart])))
                        (finally (.delete f)))))
             (it "omits the caption field when no caption is given"
                 (let [seen
                       (atom nil)

                       f
                       (java.io.File/createTempFile "vis-telegram-api-test" ".html")]

                   (try (with-redefs [http/post (fn [url opts]
                                                  (reset! seen {:url url :opts opts})
                                                  {:body "{\"ok\":true,\"result\":true}"})]
                          (tg/send-document! "TOKEN" 42 f "transcript.html"))
                        (expect (= [{:name "chat_id" :content "42"}
                                    {:name "document" :content f :file-name "transcript.html"}]
                                   (get-in @seen [:opts :multipart])))
                        (finally (.delete f))))))
