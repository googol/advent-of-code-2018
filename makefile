.PHONY: clean 01

.stamps/build: .stamps/dependencies src/*.purs
	psc-package build
	touch .stamps/build

.stamps/dependencies: psc-package.json | .stamps
	psc-package install
	touch .stamps/dependencies

01: .stamps/build
	node runner.js AoC.Day01

.stamps:
	mkdir .stamps

clean:
	rm -rf output
	rm -rf psc-package
	rm -rf .stamps
