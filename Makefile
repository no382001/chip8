CC=gcc
CFLAGS=-g -std=gnu99 -Wall -Werror -pedantic
RM=rm -f
SRC=main.c

make: format
	$(CC) $(CFLAGS) $(SRC) -lncurses

format:
	find . \( -name "*.c" -o -name "*.h" \) -exec clang-format -i {} \;