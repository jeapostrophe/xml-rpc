#lang racket
(require rackunit/text-ui)
(require "all-xmlrpc-tests.ss")

(print-struct #t)

(test/text-ui all-xmlrpc-tests)
