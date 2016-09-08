
all: bin/HSlambdamon-dll.o

bin/HSlambdamon-dll.o: *.hs stack.yaml lambdamon-dll.cabal
	@stack build 

	$(eval LIB=$(shell find .stack-work/dist | grep -e "\.a"))
	@echo lib: $(LIB)

	@# First match the fingerprint, then escape z and Z by zz and ZZ as GHC does (z-encoding)
	$(eval FINGERPRINT=$(shell echo $(LIB) | sed -E "s/^.*-([^-]+)\.a/\1/;s/z/zz/;s/Z/ZZ/"))
	@echo z-encoded fingerprint: $(FINGERPRINT)

	@mkdir -p bin/ build/
	@cd build && ar -x ../$(LIB)
	@ld -r build/*.o -o build/HSlambdamon-dll.o

	@# Now find those symbols containing the fingerprint for renaming
	@# ... for my eyes only ...
	@cd build && for obj in *.o; do nm -j $$obj | grep $(FINGERPRINT) | sed -E "s/(^(.*)_[0-9a-zA-Z]+$(FINGERPRINT)(.*)$$)/\1 \2\3/" > rename_$$obj.txt && gobjcopy $$obj --redefine-syms rename_$$obj.txt; done
	@nm -j $(LIB) | grep -v "/" | grep -e "$(FINGERPRINT)" | sed -E "s/(^(.*)_[0-9a-zA-Z]+$(FINGERPRINT)(.*)$$)/\1 \2\3/" > build/rename_lib.txt
	@gobjcopy $(LIB) --redefine-syms build/rename_lib.txt

	@cp build/*.o bin/
	@cp $(LIB) bin/


# Problem: stack build has a special package prefix which contains the 
#          GhcPkgId, which makes it really hard to get it loading

clean:
	@rm -rf bin/ build/