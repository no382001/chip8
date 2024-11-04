CC=gcc
CFLAGS=-g -std=gnu99 -Wall -Werror -pedantic -Wno-error=unused-variable
RM=rm -f
SRC=main.c

make: format
	$(CC) $(CFLAGS) $(SRC) -lncurses -I/usr/include/tcl8.6 -ltcl -ltk

format:
	find . \( -name "*.c" -o -name "*.h" \) -exec clang-format -i {} \;