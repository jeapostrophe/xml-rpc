#lang racket
(require rackunit
         "serialise-test.rkt"
         "protocol-test.rkt"
         "core-test.rkt")
(provide all-xmlrpc-tests)

(define all-xmlrpc-tests
  (test-suite 
   "all-xmlrpc-tests"
   serialise-tests
   protocol-tests
   core-tests))