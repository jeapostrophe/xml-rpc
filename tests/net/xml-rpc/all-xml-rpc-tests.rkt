#lang racket/base
(require rackunit
         "serialise-test.rkt"
         "protocol-test.rkt"
         "server-test.rkt"
         "core-test.rkt")
(provide all-xml-rpc-tests)

(define all-xml-rpc-tests
  (test-suite 
   "all-xml-rpc-tests"
   serialise-tests
   protocol-tests
   core-tests
   server-tests))
