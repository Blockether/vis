(ns com.blockether.vis.ext.channel-telegram.api-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe markdown-escape-test
  (it "escapes Telegram MarkdownV2 specials in plain text"
    (expect (= "a\\_b\\! \\(x\\)"
              (tg/escape-markdown-v2 "a_b! (x)")))))

(defdescribe bot-command-menu-test
  (it "posts Telegram BotCommand payloads to setMyCommands"
    (let [seen (atom nil)]
      (with-redefs [http/post (fn [url opts]
                                (reset! seen {:url url :opts opts})
                                {:body "{\"ok\":true,\"result\":true}"})]
        (expect (= {:ok true :result true}
                  (tg/set-my-commands! "TOKEN"
                    [{"command" "status" "description" "Show status"}
                     {"command" "models" "description" "Choose model" "ignored" true}]))))
      (expect (= "https://api.telegram.org/botTOKEN/setMyCommands" (:url @seen)))
      (expect (= {"content-type" "application/json"}
                (get-in @seen [:opts :headers])))
      (expect (= {"commands" [{"command" "status" "description" "Show status"}
                              {"command" "models" "description" "Choose model"}]}
                (json/read-json (get-in @seen [:opts :body])))))))
