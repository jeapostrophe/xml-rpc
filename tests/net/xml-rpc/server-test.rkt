#lang racket/base
(require rackunit
         racket/match
         net/url
         web-server/servlet-env
         net/xml-rpc/client
         net/xml-rpc/server)

(provide server-tests)

(define betty 
  (xml-rpc-server
   (string->url "http://betty.userland.com/RPC2")))

(define get-state-name (betty 'examples.getStateName))

(define server-tests
  (test-suite
   "All tests for server"
   
   (test-case
    "Server works with client"
    
    (define start
      (make-handle-xml-rpc
       (hasheq 'math.+ +
               'math.plus (λ (x y) (+ x y)))))
    
    (define-values (pipe-read-p pipe-write-p)
      (make-pipe))
    
    (define server-t
      (thread
       (λ ()
         (parameterize ([current-output-port pipe-write-p])
           (serve/servlet start
                          #:launch-browser? #f
                          #:quit? #f
                          #:listen-ip #f
                          #:port 0
                          #:servlet-path "/")))))
      
      ; Wait for server to start
      (define port-embedded-line (read-line pipe-read-p))
      (match-define (regexp #rx"Your Web application is running at http://localhost:([0-9]+)\\." 
                            (list _ port-string))
                    port-embedded-line)
      (define port (string->number port-string))
      
      (define url
        (string->url (format "http://localhost:~a/" port)))
      
      (define server
        (xml-rpc-server url))
      
      (check = ((server 'math.+) 3 4) 7)
      (check = ((server 'math.plus) 3 4) 7)
      (check = ((server 'math.+) 3 4 5) 12)
      
      (check-exn exn:xml-rpc:fault?
                 (λ () ((server 'math.add) 3 4)))
     
      (check-exn exn:xml-rpc:fault?
                 (λ () ((server 'math.plus))))
      (check-exn exn:xml-rpc:fault?
                 (λ () ((server 'math.plus) 1)))
      (check-exn exn:xml-rpc:fault?
                 (λ () ((server 'math.plus) 1 2 3)))
      
      (check-exn exn:xml-rpc:fault? 
                 (λ () ((server 'math.+) "Foo"))))))
