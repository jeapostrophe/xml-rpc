#lang setup/infotab

(define name "xml-rpc")
(define required-core-version "5.0.1")
(define compile-omit-files '("tests"))

(define blurb '("Implementation of the XML-RPC protocol."))
(define release-notes 
  '((p "Rackety")))

(define categories '(net))
(define repositories '("4.x"))
(define scribblings '(("xml-rpc.scrbl" ())))
(define primary-file "main.rkt")

