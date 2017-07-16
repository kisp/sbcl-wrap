[![Build Status](https://travis-ci.org/kisp/sbcl-wrap.svg?branch=master)](https://travis-ci.org/kisp/sbcl-wrap)

# sbcl-wrap
Wrapper around sbcl for fast script execution with (on demand compiled and cached) libraries.

See also https://github.com/kisp/clisp-wrap.

# Building
```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build   
```
Then install ```dist/build/sbcl-wrap/sbcl-wrap``` to a convenient location, e.g. ```/usr/local/bin/sbcl-wrap```.

# Usage example
Given a script foo.lisp:
```
#!/usr/local/bin/sbcl-wrap alexandria puri --

(defpackage #:foo
  (:use #:cl #:alexandria))

(in-package #:foo)

(let ((list '(1 2 3)))
  (format t "Getting permutations of ~S via alexandria:~%"
          list)
  (map-permutations (lambda (x) (format t "~S~%" x)) list)
  (terpri))

(format t "Parsing a url via puri: ~S~%" (puri:parse-uri "http://example.com/"))
```

calling it for the first time will be slow:

```
$ time ./foo.lisp 
Getting permutations of (1 2 3) via alexandria:
(1 2 3)
(2 1 3)
(3 1 2)
(1 3 2)
(2 3 1)
(3 2 1)

Parsing a url via puri: #<PURI:URI http://example.com/>

real    0m6.040s
user    0m4.517s
sys     0m0.600s
```

but then:

```
$ time ./foo.lisp 
Getting permutations of (1 2 3) via alexandria:
(1 2 3)
(2 1 3)
(3 1 2)
(1 3 2)
(2 3 1)
(3 2 1)

Parsing a url via puri: #<PURI:URI http://example.com/>

real    0m0.048s
user    0m0.030s
sys     0m0.017s
```
The image is stored in ~/.cache/sbcl-wrap.
