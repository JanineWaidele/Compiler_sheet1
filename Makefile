.phony: all

ASDL2PY = ./scripts/asdl2py

all: src/lang_var/var_ast.py src/lang_loop/loop_ast.py \
	src/lang_array/array_astCommon.py \
	src/lang_array/array_ast.py \
	src/lang_array/array_astAtom.py \
	src/lang_fun/fun_astCommon.py \
	src/lang_fun/fun_ast.py \
	src/lang_fun/fun_astAtom.py \
	src/parsers/lang_simple/simple_ast.py \
	src/assembly/tac_ast.py \
	src/assembly/tacSpill_ast.py \
	src/assembly/mips_ast.py

%.py: %.asdl $(wildcard src/asdl/*.py)
	$(ASDL2PY) --out $@ $<

src/lang_array/array_ast.py: src/lang_array/array_ast.asdl
	$(ASDL2PY) --out src/lang_array/array_ast.py --common lang_array.array_astCommon \
		src/lang_array/array_ast.asdl

src/lang_array/array_astAtom.py: src/lang_array/array_astAtom.asdl
	$(ASDL2PY) --out src/lang_array/array_astAtom.py --common lang_array.array_astCommon \
		src/lang_array/array_astAtom.asdl

src/lang_fun/fun_ast.py: src/lang_fun/fun_ast.asdl
	$(ASDL2PY) --out src/lang_fun/fun_ast.py --common lang_fun.fun_astCommon \
		src/lang_fun/fun_ast.asdl

src/lang_fun/fun_astAtom.py: src/lang_fun/fun_astAtom.asdl
	$(ASDL2PY) --out src/lang_fun/fun_astAtom.py --common lang_fun.fun_astCommon \
		src/lang_fun/fun_astAtom.asdl
