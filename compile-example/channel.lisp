(module ChannelModule)

(def channel (chan))

(fn producer (n)
    (if (<= n 0) nil
        (do (send channel "Hello")
            (wait 200)
            return ($ (- n 1)))))

(fn consumer ()
    (let ((message (receive channel)))
      (if (nil? message) nil
          (do (println message)
              return ($)))))

(def sender (async (^ () (producer 10))))

(def receiver (async consumer))

(await sender)
(println "Closed sender")
(close channel)
(await receiver)
(println "Received finished since the channel was closed")

(exit 0)
