#lang racket/base
(require xml
         net/url
         "base.rkt"
         "serialise.rkt")

(provide encode-xml-rpc-call
         write-xml-rpc-call
         make-xml-rpc-call
         read-xml-rpc-response
         decode-xml-rpc-response
         ;; Server-side
         decode-xml-rpc-call
         (struct-out rpc-call))

;; http-200? : string -> (U #t #f)
(define (http-200? headers)
  (if (regexp-match #rx"^HTTP/[0-9]*\\.[0-9]* 200" headers)
      #t
      #f))

;; http-404? : string -> (U #t #f)
(define (http-404? headers)
  (if (regexp-match #rx"^HTTP/[0-9]*\\.[0-9]* 404" headers)
      #t
      #f))

;; encode-xml-rpc-call : string any ... -> sxml
(define (encode-xml-rpc-call method-name . args)
  `(methodCall
    (methodName ,method-name)
    (params
     ,@(map (lambda (val)
              `(param ,(serialise val)))
            args))))

;; write-xml-rpc-call-headers : sxml output-port -> #t
(require racket/pretty)
(define (write-xml-rpc-call call op)
  (parameterize
   ; XXX
      (#;(xml-double-quotes-mode #t))
    (let ([result
           (pretty-print (list '(*pi* xml (version "1.0"))
                                      call) op)])
      ;; We don't need to close this port; it's an
      ;; 'ouput-bytes' port. Oops. Closing this breaks things.
      ;;(close-output-port op)
      result)))

;; WARNING 20060711 MCJ
;; Given a bad hostname, make-xml-rpc-call could fail. Should we 
;; catch that and pass it on as an XML-RPC exception, 
;; or leave it to the developer?
#| tcp-connect: connection to locahost, port 8080 failed; host not found (at step 1: No address associated with nodename; errno=7) |#
;; make-xml-rpc-call : url sxml -> impure-port
(define (make-xml-rpc-call url call)
  (let ((op (open-output-bytes)))
    (write-xml-rpc-call call op)
    (post-impure-port url
                      (get-output-bytes op)
                      '("Content-Type: text/xml"
                        "User-Agent: Racket"))))

;; read-xml-rpc-response : input-port -> sxml
(define (read-xml-rpc-response ip)
  (let ((headers (purify-port ip)))
    ;; Expanding the quality of error message supplied to the 
    ;; programmer developing with the XML-RPC library.
    (cond
      [(http-404? headers)
       (raise-exn:xml-rpc "Server responded with a 404: File not found")]
      [(not (http-200? headers))
       (raise-exn:xml-rpc  
        (format "Server did not respond with an HTTP 200~nHeaders:~n~a~n"
                headers))])
    ;; 20060731 MCJ
    ;; This input port doesn't seem to get closed. Or, 
    ;; if it does, I don't know where. We'll find out.
    (let ([response (read-xml ip)])
      (close-input-port ip)
      response) ))

;; decode-xml-rpc-response : input-port -> any
(define (decode-xml-rpc-response ip)
  (let ((resp (read-xml-rpc-response ip)))
    (error 'decode-xml-rpc-response "~v" resp)
    #;(xml-match (xml-document-content resp)
               [(methodResponse (params (param ,value)))
                (deserialise value)]
               [(methodResponse (fault ,value))
                (let ((h (deserialise value)))
                  (raise
                   (exn:xml-rpc:fault
                    (string->immutable-string
                     (hash-ref h 'faultString))
                    (current-continuation-marks)
                    (hash-ref h 'faultCode))))]
               [,else
                (raise-exn:xml-rpc
                 (format "Received invalid XMLRPC response ~a\n" else))])))


;; Server-side
;; extract-parameter-values : (list-of `(param ,v)) -> any
(define (extract-parameter-values param*)
  (map (lambda (p)
         (error 'xml-match "~v" p)
         #;(xml-match p
                    [(param ,value) (deserialise value)]
                    [,else
                     (raise-exn:xml-rpc
                      (format "Bad parameter in methodCall: ~a~n" p))]))
       param*))

;; read-xml-rpc-response : string -> sxml
(define (read-xml-rpc-call str)
  (let* ([call-ip (open-input-string str)]
         [result (read-xml call-ip)])
    (close-input-port call-ip)
    result))

;; decode-xml-rpc-call : string -> any
(define-struct rpc-call (name args))
(define (decode-xml-rpc-call str)
  (let ([docu (read-xml-rpc-call str)])
    (error 'xml-match "~v" docu)
    #;(xml-match (xml-document-content docu)
               [(methodCall (methodName ,name) (params ,param* ...))
                (let ([value* (extract-parameter-values param*)])
                  (make-rpc-call (string->symbol name) value*))]
               [,else
                (raise-exn:xml-rpc
                 (format "Cannot parse methodCall: ~a~n" else))])))
