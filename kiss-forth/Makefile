all: main-x64-linux

main-x64-linux: forth_rt-x64.s main-x64-linux.c
	gcc -g -o $@ $^

clean:
	rm -f main-x64-linux

.PHONY: all clean
