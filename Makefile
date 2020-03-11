OUT := simplify-bkg
ARGS := -i

PACK := flp-fun-xharmi00


.PHONY: buildm
build: $(OUT)

$(OUT):
	ghc --make -o $@ *.hs


.PHONY: run
run: $(OUT)
	./$< $(ARGS)


.PHONY: tests
tests: $(OUT)


.PHONY: pack
pack: $(PACK).zip

$(PACK).zip:
	zip -r $@ Makefile README.md *.hs


.PHONY: clean
clean:
	rm -f $(PACK).zip *.hi *.o $(OUT)
