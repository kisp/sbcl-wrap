default: package

prefix=/usr/local

install:
	install -m 0755 dist/build/sbcl-wrap/sbcl-wrap $(prefix)/bin

BINARY = sbcl-wrap
PACKAGE = sbcl-wrap-binary-linux-x86_64.tar.gz
LOCAL_BIN = $(HOME)/.local/bin/$(BINARY)

$(LOCAL_BIN): src/Main.hs sbcl-wrap.cabal stack.yaml stack.yaml.lock
	stack build
	stack install

$(PACKAGE): $(LOCAL_BIN)
	tar -czf $(PACKAGE) -C $(dir $(LOCAL_BIN)) $(BINARY)

package: $(PACKAGE)
