#lang racket
(require web-server/servlet
         "serialise.rkt"
         "protocol.rkt"
         xml
         web-server/http)
(provide (all-defined-out))

;; handler-exists? : symbol -> (U #t #f)
;; Checks to see if the requisite handler is bound in the environment.
(define (handler-exists? environment id)
  (hash-ref environment id #f))

;; invoke-handler : sym (list-of any) -> methodResponse
;; Invokes the given handler on the data passed in from
;; the call if the handler exists. 
;; 
;; There might be other checks we could do at this point
;; to keep things from falling over in an ugly way; for 
;; the moment, I do an arity check, which is more than the 
;; spec calls for, I suspect.
(define (invoke-handler environment name args)
  (let* ([fun (hash-ref environment name)]
         [arg-length (length args)])
    (if (procedure-arity-includes? fun arg-length)
        (with-handlers
            ([exn?
              (λ (x)
                (make-handler-fault 
                 (format "Error during handler evaluation: ~a"
                         (exn-message x))
                 101))])
          (let* ([result (apply fun args)]
                 [serialised-result (serialise result)])
            (make-response serialised-result)))
        (make-handler-fault 
         (format "You invoked '~a' with ~a parameters; '~a'"
                 name arg-length name)
         101))))

(define (make-response serialised-result)
  (let* ([response 
          `(methodResponse
            (params 
             (param
              ;; Is there an inconsistent wrapping of 'value'
              ;; around this?
              ,serialised-result)))]
         [output (string->bytes/utf-8 (xexpr->string response))])
    (make-response/full 
     200 #"Okay" (current-seconds) 
     #"text/xml" '() 
     (list output))))


;; make-handler-fault : string num -> methodResponse
;; Makes the XML-RPC 'fault' method response. 
;; The error codes thrown by this library should be chosen
;; in a less arbitrary way, and documented.
(define (make-handler-fault string code)
  (let ([errorHash (make-hash)])
    (hash-set! 
     errorHash 'faultString string)
    (hash-set! 
     errorHash 'faultCode code)
    `(methodResponse (fault ,(serialise errorHash)))))

;; extract-xml-rpc-bindings : request -> string
;; The bindings come in all kinds of messed up, it seems.
;; This *must* be tested against clients other than ours
;; to decide whether this is a sensible way to handle the bindings
;; or not.
(define (extract-xml-rpc-bindings request)
  ;; struct:request looks like:
  ;;   method uri headers/raw bindings/raw
  ;;   host-ip host-port client-ip
  (bytes->string/utf-8 (request-post-data/raw request)))

(define ((make-handle-xml-rpc environment) request)
  (let ([call (decode-xml-rpc-call
               (extract-xml-rpc-bindings request))])
    (let ([name (rpc-call-name call)]
          [args (rpc-call-args call)])
      (if (handler-exists? environment name)
          (invoke-handler environment name args)
          (make-handler-fault 
           (format "No handler found on server for '~a'" name)
           100)))))

