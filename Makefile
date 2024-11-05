CC=gcc
CFLAGS=-g -std=gnu99 -Wall -Werror -pedantic
RM=rm -f
SRC=main.c vm.c gui.c

make: format
	$(CC) $(CFLAGS) $(SRC) -I/usr/include/tcl8.6 -ltcl -ltk

format:
	find . \( -name "*.c" -o -name "*.h" \) -exec clang-format -i {} \;