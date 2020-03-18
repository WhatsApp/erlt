all:
	$(MAKE) -C erlbuild/src
	$(MAKE) -C erl2c/src

clean:
	$(MAKE) -C erlbuild/src clean
	$(MAKE) -C erl2c/src clean
