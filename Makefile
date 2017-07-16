prefix=/usr/local

install:
	install -m 0755 dist/build/sbcl-wrap/sbcl-wrap $(prefix)/bin
