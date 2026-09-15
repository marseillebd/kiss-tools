all: main-x64-linux

main-x64-linux: forth_rt-x64.s main-x64-linux.c
	gcc -fPIC -g -o $@ $^

run: main-x64-linux
	./main-x64-linux

gdb: main-x64-linux
	gdb --tui ./main-x64-linux

clean:
	rm -f main-x64-linux

.PHONY: all clean
