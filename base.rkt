#lang racket
(provide raise-exn:xml-rpc)

(struct exn:xml-rpc exn ())

(struct exn:xml-rpc:fault exn:xml-rpc (code))

(define-syntax raise-exn:xml-rpc
  (syntax-rules ()
    ((_ message)
     (raise
      (exn:xml-rpc
       (string->immutable-string message)
       (current-continuation-marks))))))

(provide/contract
 [struct (exn:xml-rpc exn) 
         ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:xml-rpc:fault exn:xml-rpc) 
         ([message string?] [continuation-marks continuation-mark-set?] [code integer?])])