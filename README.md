[![CI](https://github.com/kisp/sbcl-wrap/actions/workflows/ci.yml/badge.svg)](https://github.com/kisp/sbcl-wrap/actions/workflows/ci.yml)

# sbcl-wrap
Wrapper around sbcl for fast script execution with (on demand compiled and cached) libraries.

See also https://github.com/kisp/clisp-wrap.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Building](#building)
    - [Nix](#nix)
    - [Cabal](#cabal)
- [Development shell (devenv.sh)](#development-shell-devenvsh)
- [Usage example](#usage-example)
- [Using without installing sbcl-wrap](#using-without-installing-sbcl-wrap)

<!-- markdown-toc end -->

## Building

### Nix

No Haskell toolchain needed on the host.

```
# Build the binary
make build
# or: nix build
./result/bin/sbcl-wrap  # prints version info and exits 88 (no args)

# Install into your Nix profile
make nix-install
# or: nix profile install .

# Run directly without installing
nix run github:kisp/sbcl-wrap

# Drop into a dev shell (GHC, cabal, HLS, sbcl)
nix develop
```

### Cabal

```
cabal build
cabal install
```


## Development shell (devenv.sh)

[devenv.sh](https://devenv.sh) provides an alternative to `nix develop` using `devenv.nix`.

No need to install devenv globally. Run it directly via Nix:

```
nix run github:cachix/devenv -- shell
```

This always fetches the latest devenv. If you have devenv installed locally:

```
# Enter the dev shell
devenv shell

# Or activate automatically with direnv
direnv allow
```

The shell provides GHC, cabal, stack, sbcl, and rlwrap. On entry it prints a
quick reference of the common build commands.

## Usage example
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

## Using without installing sbcl-wrap

On any Nix-enabled system you can write scripts that use sbcl-wrap via `nix run`,
with no prior installation required. Use the same `#!/bin/sh` polyglot shebang
as usual, but call `nix run github:kisp/sbcl-wrap` instead:

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec nix run github:kisp/sbcl-wrap -- cl-ppcre -- "$0" "$@"
|#

;;; Read stdin, print the 10 most common words.

(defpackage #:word-freq (:use #:cl))
(in-package #:word-freq)

(let ((tally (make-hash-table :test #'equal)))
  (loop for line = (read-line *standard-input* nil)
        while line
        do (dolist (word (cl-ppcre:all-matches-as-strings "[a-z]+" (string-downcase line)))
             (incf (gethash word tally 0))))
  (let* ((pairs (loop for k being each hash-key of tally
                      using (hash-value v) collect (cons k v)))
         (sorted (sort pairs #'> :key #'cdr)))
    (loop for (word . n) in (subseq sorted 0 (min 10 (length sorted)))
          do (format t "~5d  ~a~%" n word))))
```

Make the script executable and run it:

```
$ chmod +x word-freq.lisp
$ cat /usr/share/doc/sbcl/README | ./word-freq.lisp
   52  the
   28  sbcl
   27  to
   20  of
   ...
```

The first run fetches and builds sbcl-wrap from source via Nix (slow, cached
afterwards). To pin a specific release for reproducibility:

```lisp
exec nix run github:kisp/sbcl-wrap/0.0.13 -- cl-ppcre -- "$0" "$@"
```
