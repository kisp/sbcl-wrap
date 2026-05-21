;;; End-to-end test — exercises loading alexandria and puri via ASDF.
(defpackage #:sbcl-wrap-test
  (:use #:cl #:alexandria))

(in-package #:sbcl-wrap-test)

;;; alexandria: permutations of a short list
(let ((perms '()))
  (map-permutations (lambda (x) (push x perms)) '(1 2 3))
  (assert (= (length perms) 6) ()
          "Expected 6 permutations, got ~A" (length perms)))

;;; puri: round-trip a URI
(let ((uri (puri:parse-uri "http://example.com/path")))
  (assert (string= (puri:uri-host uri) "example.com") ()
          "Expected host example.com, got ~A" (puri:uri-host uri)))

(format t "libs-ok~%")
