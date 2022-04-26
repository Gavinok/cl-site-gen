all: csg

.qlot:
	qlot install

csg: .qlot src/main.lisp
	qlot exec ros -l build.lisp

gen: csg
	./csg example example-res

