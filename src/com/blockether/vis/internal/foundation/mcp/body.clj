(ns com.blockether.vis.internal.foundation.mcp.body
  "Fixed-length request bodies for babashka.http-client."
  (:import (java.io IOException)
           (java.net.http HttpRequest$BodyPublisher HttpRequest$BodyPublishers)
           (java.util.concurrent Flow$Subscriber Flow$Subscription)))

(defn tracked-body
  "Call `sent!` after body handoff. A mutation body can be published only once."
  [body begin! sent! single?]
  (let [^HttpRequest$BodyPublisher publisher (HttpRequest$BodyPublishers/ofString body)
        used? (atom false)]
    (reify HttpRequest$BodyPublisher
      (contentLength [_] (.contentLength publisher))
      (subscribe [_ subscriber]
        (if (and (or (not single?) (compare-and-set! used? false true))
                 (or (not begin!) (begin!)))
          (.subscribe publisher
                      (reify Flow$Subscriber
                        (onSubscribe [_ subscription] (.onSubscribe ^Flow$Subscriber subscriber subscription))
                        (onNext [_ item] (.onNext ^Flow$Subscriber subscriber item))
                        (onError [_ error] (.onError ^Flow$Subscriber subscriber error))
                        (onComplete [_]
                          (.onComplete ^Flow$Subscriber subscriber)
                          (sent!))))
          (do (.onSubscribe ^Flow$Subscriber subscriber
                            (reify Flow$Subscription (request [_ _]) (cancel [_])))
              (.onError ^Flow$Subscriber subscriber (IOException. "MCP request body is not available."))))))))
