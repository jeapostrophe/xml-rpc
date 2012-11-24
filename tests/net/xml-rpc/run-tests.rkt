#lang racket/base
(require rackunit/text-ui)
(require "all-xml-rpc-tests.rkt")

(print-struct #t)

(run-tests all-xml-rpc-tests)
