DAYS = 01 02 03

.PHONY: all clean $(DAYS)

all: $(DAYS) ;

.stamps/build: .stamps/dependencies src/*.purs
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
