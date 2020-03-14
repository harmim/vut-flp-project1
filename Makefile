OUT := simplify-bkg
ARGS := -i

PACK := flp-fun-xharmi00


.PHONY: build
build: $(OUT)

$(OUT): *.hs
	ghc --make -o $@ $^


.PHONY: run
run: $(OUT)
	./$< $(ARGS)


.PHONY: pack
pack: $(PACK).zip

$(PACK).zip: Makefile README.md *.hs
	zip -r $@ $^


.PHONY: clean
clean:
	rm -f $(PACK).zip *.hi *.o $(OUT)
