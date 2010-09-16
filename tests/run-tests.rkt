#lang racket
(require rackunit/text-ui)
(require "all-xmlrpc-tests.ss")

(print-struct #t)

(run-tests all-xmlrpc-tests)
