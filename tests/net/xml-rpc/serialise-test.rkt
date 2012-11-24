#lang racket/base
(require rackunit
         net/xml-rpc/base
         net/xml-rpc/serialise
         "util.rkt")

;; 20071020 MCJ
;; I don't quite know how to handle timezones. More specifically, I don't know
;; that our date handling properly handles timezones. I was unable to run the
;; test suite successfully here in EST, as Racket and I both have different
;; notions of what timezone we're in. This could be because my Mac, recently
;; recussitated, is confused... or something else is going on.
;;
;; I've forced the issue. Our library currently forces the timezone to the
;; local, here-and-now, timezone. This is badness, but I need more time to
;; test and explore what is going on.
(define current-timezone-offset
  (date-time-zone-offset (seconds->date (current-seconds))))

(provide serialise-tests)

;; check-invertible : any sxml
;;
;; Assert that serialisation and deserialisation produces
;; exactly the same result
(define (check-invertible scheme xml)
  (test-equal? (format "serialize ~v" scheme)
               (serialise scheme)
               xml)
  (test-equal? (format "deserialize ~v" xml)
               (deserialise xml)
               scheme))

(define-check (check-invertible-hash scheme xml)
  (check-serialised-hash-table-equal? (serialise scheme)
                                      xml)
  (check-hash-table-equal? (deserialise xml)
                           scheme))


(define invertible-tests
  (test-suite
   "All tests for invertible serialisation"

   (test-case
    "Normal integers serialised correctly"
    (check-invertible 1 '(value (int "1")))
    (check-invertible 123456 '(value (int "123456")))
    (check-invertible -1234 '(value (int "-1234")))
    (check-invertible (expt 2 31) '(value (int "2147483648")))
    (check-invertible (* -1 (expt 2 31))
                      '(value (int "-2147483648"))))

   (test-case
    "Boolean serialised correctly"
    (check-invertible #t '(value (boolean "1")))
    (check-invertible #f '(value (boolean "0"))))

   (test-case
    "String serialised correctly"
    (check-invertible "hello world"
                      '(value (string "hello world"))))

   ;; Test for client shortcut for strings
   ;; 20061201 MCJ
   ;; We don't have a shortcut serialisation for strings. Therefore,
   ;; this test will never pass. When strings are encountered,
   ;; they are encoded as above.
   #;(test-case
   "Shortcut string serialised correctly"
   (check-invertible "hello world"
                     '(value "hello world")))

  (test-case
   "Empty string serialised correctly"
   (check-invertible "" '(value (string ""))))

  (test-case
   "Doubles serialised correctly"
   (check-invertible 1.2 '(value (double "1.2"))))

  (test-case
   "Date serialised correctly"
   (check-invertible
    (make-date 35 27 9 7 12 2005 3 340 #f current-timezone-offset)
    '(value (dateTime.iso8601 "20051207T09:27:35"))))

  (test-case
   "Date components are padded to two digits"
   (check-invertible
    (make-date 5 4 3 2 1 2005 0 1 #f current-timezone-offset)
    '(value (dateTime.iso8601 "20050102T03:04:05"))))

  (test-case
   "Hash-table serialised correctly"
   (check-invertible-hash
    #hash((a . 1) (b . "2") (c . 3.0))
    '(value (struct
             (member (name "b") (value (string "2")))
             (member (name "a") (value (int "1")))
             (member (name "c") (value (double "3.0")))))))

  (test-case
   "Hash-table serialised correctly, different order"
   (check-invertible-hash
    #hash((a . 1) (b . "2") (c . 3.0))
    '(value (struct
             (member (name "b") (value (string "2")))
             (member (name "c") (value (double "3.0")))
             (member (name "a") (value (int "1")))
             ))))

  ;; Failure test
  (test-case
   "Hash table correctly found to be smaller than serialised form."
   (check-fail
    (check-invertible-hash
     #hash((a . 1) (b . "2"))
     '(value (struct
              (member (name "b") (value (string "2")))
              (member (name "a") (value (int "1")))
              (member (name "c") (value (double "3.0"))))))))

  ;; Failure test
  (test-case
   "Hash table correctly found to be longer than serialised form."
   (check-fail
    (check-invertible-hash
     #hash((a . 1) (b . "2") (c . 3.0))
     '(value (struct
              (member (name "b") (value (string "2")))
              (member (name "a") (value (int "1")))
              )))))

  ;; Failure test
  (test-case
   "Different key names in hash tables fail to pass serialise."
   (check-fail
    (check-invertible-hash
     #hash((a . 1) (bogo . "2") (c . 3.0))
     '(value (struct
              (member (name "b") (value (string "2")))
              (member (name "c") (value (double "3.0")))
              (member (name "a") (value (int "1")))
              )))))

  (test-case
   "Recursive hash-table serialised correctly"
   (check-invertible-hash
    #hash((a . #hash((b . 2))))
    '(value (struct
             (member (name "a")
                     (value (struct
                             (member (name "b")
                                     (value (int "2"))))))))))
  (test-case
   "List serialised as array"
   (check-invertible '(1 2 3 4)
                     `(value (array (data (value (int "1"))
                                          (value (int "2"))
                                          (value (int "3"))
                                          (value (int "4")))))))
  (test-case
   "Hetergenous list serialised correctly"
   (check-invertible
    '(1 2.0 "3")
    `(value (array (data (value (int "1"))
                         (value (double "2.0"))
                         (value (string "3")))))))
  (test-case
   "Recursive list serialised correctly"
   (check-invertible
    '((1 (2)))
    '(value (array
             (data
              (value
               (array
                (data
                 (value (int "1"))
                 (value
                  (array
                   (data (value (int "2")))))))))))))

  (test-case
   "(list) array encoded correctly"
   (check-invertible '() '(value (array (data))))
   (check-invertible
    '(())
    '(value (array (data (value (array (data)))))))

   (check-invertible '(1 2 3 4 5)
                     '(value (array
                              (data
                               (value (int "1"))
                               (value (int "2"))
                               (value (int "3"))
                               (value (int "4"))
                               (value (int "5"))))))
   (check-invertible '(1 "two" 3.3)
                     '(value (array
                              (data
                               (value (int "1"))
                               (value (string "two"))
                               (value (double "3.3"))))))
   (check-invertible
    '("")
    '(value (array (data (value (string "")))))))

  ;; 20061201 MCJ
  ;; Our double encoding breaks; our upstream XML library encodes these
  ;; entities for us, so we don't need to double-encode them. Patch
  ;; by Danny Yoo.
  (test-case
   "String containing < and & is encoded correctly"
   (check-invertible "<&" '(value (string "<&"))))

  ;; 20061201 MCJ
  ;; Our double encoding breaks; our upstream XML library encodes these
  ;; entities for us, so we don't need to double-encode them. Patch
  ;; by Danny Yoo.
  (test-case
   "All entity instances are encoded"
   (check-invertible
    "<hello&<there&< <&&"
    '(value (string "<hello&<there&< <&&"))))

  (test-case
   "base64 encoded correctly"
   (check-invertible
    #"Racket Rules!"
    '(value (base64 "UmFja2V0IFJ1bGVzIQ=="))))

  ))
;; end of invertible-tests

(define serialisation-tests
  (test-suite
   "Serialisation tests"

   (test-case
    "Out-of-range doubles throw exception"
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise +inf.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise -inf.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise +nan.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise -nan.0))))

   (test-case
    "Out of range integer throws exception"
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise +inf.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise -inf.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise +nan.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise -nan.0)))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise (expt 2 32))))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (serialise (- (expt 2 40))))))

   (test-case
    "(vector) array encoded correctly"
    (check-equal? (serialise (make-vector 0))
                  '(value (array (data))))

    (check-equal? (serialise (list->vector '(1 2 3 4 5)))
                  '(value (array
                           (data
                            (value (int "1"))
                            (value (int "2"))
                            (value (int "3"))
                            (value (int "4"))
                            (value (int "5"))))))
    (check-equal? (serialise (list->vector '(1 "two" 3.3)))
                  '(value (array
                           (data
                            (value (int "1"))
                            (value (string "two"))
                            (value (double "3.3"))))))
    (check-equal? (serialise (list->vector '("")))
                  '(value (array (data (value (string "")))))))

   ;; 20061201 MCJ
   ;; Our double encoding breaks; our upstream XML library encodes these
   ;; entities for us, so we don't need to double-encode them. Patch
   ;; by Danny Yoo.
   ;;
   ;; It would seem his patch was unnecessary, and we could have simply
   ;; defaulted the parameterization. However, for now, I'm going to
   ;; leave the parameterization and change the test.
   (test-case
    "String encoding is parameterised"
    (parameterize
        ((encode-string #f))
      (check-equal? (serialise "<&")
                    '(value (string "<&"))))
    (parameterize
        ((encode-string #t))
      (check-equal? (serialise "<&")
                    '(value (string "<&")))))
   ))
;; end of serialisation tests

(define deserialisation-tests
  (test-suite
   "Deserialisation tests"

   (test-case
    "Deserialisation default to string"
    (check-equal? (deserialise '(value "Foo"))
                  "Foo"))

   (test-case
    "Deserialisation of empty value defaults to empty string"
    (check-equal? (deserialise '(value))
                  ""))

   (test-case
    "Deserialisation of dateTime raises exn:xml-rpc on badly formatted data"
    (check-exn
     exn:xml-rpc?
     (lambda ()
       (deserialise
        '(value (dateTime.iso8601 "990101T09:27:35"))))))

   (test-case
    "Deserialisation raises exn:xml-rpc on error"
    (check-exn exn:xml-rpc?
               (lambda ()
                 (deserialise '(some crap)))))

   (test-case
    "Incorrect struct raises exn:xml-rpc"
    (check-exn exn:xml-rpc?
               (lambda ()
                 (deserialise
                  '(value (struct
                           (member (name "a")))))))
    (check-exn exn:xml-rpc?
               (lambda ()
                 (deserialise
                  '(value (struct
                           (member (value "2"))))))))

   (test-case
    "Deserialisation of empty string is correct"
    (check-equal? (deserialise '(value (string)))
                  ""))

   ;; 20061201 MCJ
   ;; Our double encoding breaks; our upstream XML library encodes these
   ;; entities for us, so we don't need to double-encode them. Patch
   ;; by Danny Yoo.
   ;;
   ;; It would seem his patch was unnecessary, and we could have simply
   ;; defaulted the parameterization. However, for now, I'm going to
   ;; leave the parameterization and change the test.
   (test-case
    "String decoding is parameterised"
    (parameterize
        ((decode-string #f))
      (check-equal? (deserialise '(value (string "<&")))
                    "<&"))
    (parameterize
        ((decode-string #t))
      (check-equal? (deserialise '(value (string "<&")))
                    "<&")))

   (test-case
    "All entity instances are decoded"
    (check-equal?
     (deserialise
      '(value
        ;;(string "&lt;hello&amp;&lt;there&amp;&lt; &lt;&amp;&amp;")
        (string "<hello&<there&< <&&")
        ))
     "<hello&<there&< <&&"))
   ))
;; end of deserialisation-tests

(define serialise-tests
  (test-suite
   "Serialise tests"
   invertible-tests
   serialisation-tests
   deserialisation-tests))
