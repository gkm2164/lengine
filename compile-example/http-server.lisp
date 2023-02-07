(module HttpServerModule)

(fn home (req res)
  (do (println "Process request /")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "text/html" })
      ((:writer res) "<html><h1>Welcome!</h1></html>")
      return res))

(fn about (req res)
  (do (println "Process request /about")
      ((:set-status-code res) 200)
      ((:set-headers res) { :Content-Type "text/html" })
      ((:writer res) "<html><h1>This is About</h1></html>")
      return res))

(listen {
  :host "127.0.0.1"
  :port 8080
  :handlers {
    :/      home
    :/about about
  }
})
