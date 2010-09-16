#lang racket

(require net/url
         "serialise.rkt"
         "protocol.rkt")

(provide xml-rpc-server)

;; xmlrpc-server : string integer string -> (string -> (any ... -> any))
;;               : url -> (string -> (any ... -> any))
(define xml-rpc-server 
  (case-lambda
    [(host port path)
     (let ([url (string->url
                 (format "http://~a:~a/~a" host port path))])
       (xml-rpc-server url))]
    [(url)
     (lambda (method-name)
       (lambda args
         ;; This port used to go unclosed. Now, I close it.
         ;; However, this is on the client-side. So while this
         ;; does clean up a leak, it doesn't fix the server-leak.
         (let* ([impure-port
                 (make-xmlrpc-call
                  url
                  (apply encode-xmlrpc-call method-name args))]
                [result (decode-xmlrpc-response impure-port)])
           (close-input-port impure-port)
           result)))]))