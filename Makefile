ifeq ($(OS),Windows_NT)
	.DEFAULT_GOAL := compiler.exe
else
	.DEFAULT_GOAL := compiler
endif

clean:
	rm -f compiler.exe run.exe run.asm run.obj compiler run run.s run.o
	rm -rf obj

run.exe: run.obj
	gcc -o run.exe run.obj

run.obj: run.asm
	nasm -fwin32 run.asm

run.asm: run.c compiler.exe
	./compiler.exe run.c run.asm

compiler.exe: src/*.hs
	ghc -outputdir obj -o compiler.exe -isrc src/Main.hs

run: run.o
	gcc -m32 -no-pie -o run run.o

run.o: run.s
	nasm -felf32 run.s

run.s: run.c compiler
	./compiler run.c run.s

compiler: src/*.hs
	stack exec -- ghc -outputdir obj -o compiler -isrc src/Main.hs
