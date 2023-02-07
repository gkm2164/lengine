(module ChannelModule)

(def channel (chan))

(fn producer ()
    (do (send channel "Hello")
        (wait 1000)
        return ($)))

(fn consumer ()
    (let ((message (receive channel)))
         (do (println message)
             return ($))))

(def sender (async producer))
(def receiver (async consumer))

(wait 10000)

(exit 0)
