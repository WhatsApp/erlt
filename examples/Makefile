all:
	$(MAKE) -C dev/src
	$(MAKE) -C elm-core/src
	$(MAKE) -C dev_dots/src
	$(MAKE) -C dev_enum/src

clean:
	$(MAKE) -C dev/src clean
	$(MAKE) -C elm-core/src clean
	$(MAKE) -C dev_dots/src clean
	$(MAKE) -C dev_enum/src clean

ir:
	$(MAKE) -C dev/src ir
	$(MAKE) -C elm-core/src ir
	$(MAKE) -C dev_dots/src ir
	$(MAKE) -C dev_enum/src ir

test-ir:
	$(MAKE) -C dev/src test-ir
	$(MAKE) -C elm-core/src test-ir
	$(MAKE) -C dev_dots/src ir
	$(MAKE) -C dev_enum/src ir

update-ir-spec:
	$(MAKE) -C dev/src update-ir-spec
	$(MAKE) -C elm-core/src update-ir-spec
	$(MAKE) -C dev_dots/src update-ir-spec
	$(MAKE) -C dev_enum/src update-ir-spec
