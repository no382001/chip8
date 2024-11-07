CC=gcc
CFLAGS=-g -std=gnu99 -Wall -Werror -pedantic -Wunused
RM=rm -f
SRC=main.c vm.c gui.c
INC=-I/usr/include/tcl8.6 -Iminifb/include
LINK=-ltcl -ltk -Lminifb -lminifb -lX11 -lXrandr -lGL -lGLX

make: format
	$(CC) $(CFLAGS) $(SRC) $(INC) $(LINK)

format:
	find . \( -name "*.c" -o -name "*.h" \) -exec clang-format -i {} \;