TARGET=target
EXE=$(TARGET)/kms-s3
DIST_EXE=$(EXE)-$(shell uname -s)-$(shell uname -m)
DIST_EXE_SIG=$(DIST_EXE).sig

build:
	stack build kms-s3

build-prof:
	stack build --profile --ghc-options="-rtsopts" kms-s3

# Requires dockerized toolchain: https://github.com/andreyk0/docker-haskell-platform-alpine
build-linux-static:
	docker-haskell-build-linux-static

install:
	stack install kms-s3

bindist:
	mkdir -p $(TARGET)
	stack --local-bin-path $(TARGET) install $(STACK_OPTS) kms-s3
	upx --best $(EXE)
	mv $(EXE) $(DIST_EXE)
	gpg --output $(DIST_EXE_SIG) --detach-sign $(DIST_EXE)

clean:
	stack clean
	rm -rf target

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies


.PHONY: build build-prof clean tags sources

