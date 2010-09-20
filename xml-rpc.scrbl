#lang scribble/doc
@(require unstable/scribble
          (for-label "main.rkt"
                     net/url
                     web-server/http
                     web-server/servlet-env
                     racket)
          scribble/eval
          scribble/manual)

@(define xml-rpc-eval
   (make-base-eval))
@(xml-rpc-eval '(require "main.rkt"))
@(xml-rpc-eval '(require net/url))

@title{XML-RPC}
@author{@(author+email "Jay McCarthy" "jay@racket-lang.org")}

@defmodule/this-package[]

@margin-note{This package is based on the XML-RPC package by Matt Jadud and Noel Welsh of Untyped}

This library implements the XML-RPC protocol, a popular protocol for interface to Internet
services such as blog engines, as specified at
@link["http://www.xmlrpc.com/spec"]{http://www.xmlrpc.com/spec}.


@section{Clients}

@defmodule/this-package[client]

The client library provides a few forms for defining servers and
their supported endpoints.

@defproc[((xml-rpc-server [url url?]) [method symbol?])
         procedure?]{
                                                     
Returns a function configured to make XML-RPC requests to the given
URL.  The function accepts a string, the name of the method, and
returns a function of any arguments which calls the method with the
given arguments.

This example calls the @litchar{examples.getStateName} method on the server
@litchar{betty.userland.com}:

@defexamples[#:eval xml-rpc-eval
 (define betty 
   (xml-rpc-server (string->url "http://betty.userland.com/RPC2")))
 (define get-state-name (betty 'examples.getStateName))
 (get-state-name 42)]

}

@section{Servers}

@defmodule/this-package[server]

The server library provides 

@defproc[(make-handle-xml-rpc [exports (hash/c symbol? procedure?)])
         (request? . -> . response/c)]{

Returns a function from HTTP requests encoding XML-RPC queries to HTTP responses encoding XML-RPC answers
that computes answers by calling the procedure associated with the XML-RPC method name in the @racket[exports]
hash table.

@defexamples[#:eval xml-rpc-eval
 (define (add x y) (+ x y))
 (define xml-rpc-adder
   (make-handle-xml-rpc 
    (hasheq 'math.add add
            'math.+ +
            'addFun add)))
 (define (run-server!)
   (serve/servlet xml-rpc-adder
                  #:port 8080
                  #:servlet-path "/"
                  #:command-line? #t))
 (define (test-server)
   (define adder 
     (xml-rpc-server (string->url "http://localhost:8080/")))
   (define math.add (adder 'math.add))
   (math.add 3 4))]

}

@section{Errors}

@defstruct*[(exn:xml-rpc exn) ([message string?] [continuation-marks continuation-mark-set?])]

A subtype of @racket[exn], this exception is raised whenever the XML-RPC library
encounters an error.

@defstruct*[(exn:xml-rpc:fault exn:xml-rpc) ([message string?] [continuation-marks continuation-mark-set?] [code integer?])]{
  
A subtype of @racket[exn:xml-rpc], this exception is raised when the XML-RPC
server responds to the client with a fault.  The @racket[code] is an integer
containing the fault code returned by the server.  The fault message
returned by the server is contained in the exception message.

}