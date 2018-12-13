DAYS = 01 02 03 04

rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))

.PHONY: all clean $(DAYS)

all: $(DAYS) ;

.stamps/build: .stamps/dependencies $(call rwildcard, src/, *.purs)
	psc-package build
	touch .stamps/build

.stamps/dependencies: psc-package.json | .stamps
	psc-package install
	touch .stamps/dependencies

$(DAYS): .stamps/build
	node runner.js AoC.Day$@

.stamps:
	mkdir .stamps

clean:
	rm -rf output
	rm -rf .psc-package
	rm -rf .stamps
