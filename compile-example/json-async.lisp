(module JsonAsyncModule)

(import Json.from-json)

(def json-file-stream (read-file-seq "./compile-example/json-example.json"))

(def json-chs (fold json-file-stream "" +))

(fn run ()
  (let ((start (now))
        (json-obj (from-json json-chs)))
       (do (wait 200)
           (println "Processed!")
           return [(- (now) start) json-obj])))

(def result (loop for x in (=range 1 10)
   (async run)))

(wait 100)
(println "Should be shown earlier than result")

(loop for x in result
    (printf "%dms elapsed: %s\n" (await x)))

(exit 0)
