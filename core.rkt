#lang racket
(require net/url
         "protocol.rkt")

(provide/contract
 [xml-rpc-server
  (url? . -> . (symbol? . -> . procedure?))])

(define (xml-rpc-server url)
  (lambda (method-name)
    (lambda args
      ;; This port used to go unclosed. Now, I close it.
      ;; However, this is on the client-side. So while this
      ;; does clean up a leak, it doesn't fix the server-leak.
      (let* ([impure-port
              (make-xml-rpc-call
               url
               (apply encode-xml-rpc-call (symbol->string method-name) args))]
             [result (decode-xml-rpc-response impure-port)])
        (close-input-port impure-port)
        result))))