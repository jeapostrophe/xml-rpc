#lang racket
(require rackunit/text-ui)
(require "all-xml-rpc-tests.ss")

(print-struct #t)

(run-tests all-xml-rpc-tests)
