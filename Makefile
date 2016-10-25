build:
	stack build kms-s3

build-prof:
	stack build --profile --ghc-options="-rtsopts" kms-s3 

install:
	stack install kms-s3

clean:
	stack clean

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

.PHONY: build build-prof clean tags sources

