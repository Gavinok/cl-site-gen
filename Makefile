all: csg

.qlot:
	qlot install

csg: .qlot src/main.lisp
	qlot exec ros -l build.lisp

gen: ## generate the example project
	qlot exec ./example/ex.ros

cli: csg
	./csg example example-res

