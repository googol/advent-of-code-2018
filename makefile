.PHONY: clean

.stamps/build: .stamps/dependencies src/*.purs
	psc-package build
	touch .stamps/build

.stamps/dependencies: psc-package.json | .stamps
	psc-package install
	touch .stamps/dependencies

.stamps:
	mkdir .stamps

clean:
	rm -rf output
	rm -rf psc-package
	rm -rf .stamps
