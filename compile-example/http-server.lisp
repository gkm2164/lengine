(module HttpServerModule)

(fn home (req res)
  (do (println "Process request / GET")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "text/html" })
      ((:writer res) "<html><h1>Welcome!</h1></html>")
      return res))

(fn home-post (req res)
  (do (println "Process request / POST")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "application/json" })
      ((:writer res) "{}")
      return res))

(fn about (req res)
  (do (println "Process request /about for ALL methods")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "text/html" })
      ((:writer res) "<html><h1>This is About</h1></html>")
      return res))

(def closer (listen {
  :host "127.0.0.1"
  :port 8080
  :handlers {
    :/                { :GET  home :POST home-post }
    :/about           { :POST about }
    :/different-about { :ALL about }
  }
}))

(fn recursion ()
    ($))

(recursion)

(println "Closing server...")
(closer)
