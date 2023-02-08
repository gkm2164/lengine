(module HttpServerModule)

(import Json.to-json)
(import Json.from-json)
;;; Now we do some fun with Lengine...

;;; Though, this backend implementation is Java, but, for now, this would be the interface.

;;; Let's define handler!
;;; This is going to be used as "/" route, and especially GET method.
(fn home (req res)
  (do (println "Process request / GET")
      ;;; "res" object is Map type, and has entry with lambda function values.
      ;;; Below is setting up status code
      ((:set-status-code res) 200)
      ;;; What about headers?
      ((:set-headers res) { :Content-Type "text/html" })
      ;;; And now write the values to the output stream.
      ((:writer res) "<html><h1>Welcome!</h1></html>")
      ;;; This is nothing, but, just saying "Unit"
      return res))

(fn debug (obj)
    (do (printf "[%s]\n" [obj])
        return obj))

(fn escape (stream)
    (fold stream "" (lambda (ret ch)
        (case ((= ch #\") (+ (+ ret #\\) #\"))
              ((= ch #\Return) (+ (+ ret #\\) #\r))
              ((= ch #\Linefeed) (+ (+ ret #\\) #\n))
              default (+ ret ch)))))

(debug (escape (seq "abcdefg\" \n")))

;;; Now, testing whether our to-json is working as expected.
;;; It takes the user's headers and information retrieved from requests, and converting it to json responses.
;;; Send payload here with {"name": "Your name"}
(fn home-post (req res)
  ;;; Similar, but, processing POST.
  (do (println "Process request / POST")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "application/json" })
      (let ((body-txt (:request-body req))
            (body (from-json (fold body-txt "" +))))
          ((:writer res) (debug (to-json {
            :id "id-1234"
            :title "Hello"
            :message (format "Hello, %s!" [(:name body)])
            :request {
              :headers (:headers req)
              :path (:path req)
              :query (:query req)
              :method (:method req)
              :body body
            }
           }))))
      return res))

(fn about (req res)
  ;;; This will handler ALL methods.
  (do (println "Process request /about for ALL methods")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "text/html" })
      ((:writer res) "<html><h1>This is About</h1></html>")
      return res))

(def closer (listen {
  :host "127.0.0.1"
  :port 8080
  :handlers {
    ;;; Here's how to define routes.
    ;;; Each key hold context path
    ;;; And inside, need to define map saying ":METHOD" => HANDLER mapping.
    :/home            { :GET  home :POST home-post }
    :/about           { :POST about }
    ;;; ALL is for define accepting all methods
    :/different-about { :ALL about }
  }
}))

;;; We need to hold manually, otherwise it will fall behind.
;;; This is actually, converted into while loop.
(fn recursion ()
    ($))

(recursion)

;;; Will never reach here, but, if reaches...
(println "Closing server...")
;;; here how we can close the server.
(closer)
