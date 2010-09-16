#lang racket
(provide (all-defined-out))

;; struct exn:xmlrpc:base : ()
(struct exn:xmlrpc exn ())

;; struct (exn:xmlrpc:base exn:xmlrpc) : integer
(struct exn:xmlrpc:fault exn:xmlrpc (code))

(define-syntax raise-exn:xmlrpc
  (syntax-rules ()
    ((_ message)
     (raise
      (exn:xmlrpc
       (string->immutable-string message)
       (current-continuation-marks))))))