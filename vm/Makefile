CC=gcc
CFLAGS=-g -std=gnu99 -Wall -Werror -pedantic -Wunused
RM=rm -f
SRC=main.c vm.c gui.c
INC=-I/usr/include/tcl8.6 -Iminifb/include
LINK=-ltcl -ltk -Lminifb -lminifb -lX11 -lXrandr -lGL -lGLX

make: format
	$(CC) $(CFLAGS) $(SRC) $(INC) $(LINK)

format:
	@changed_files=$$(git diff --name-only -- '*.c' '*.h'); \
	if [ -n "$$changed_files" ]; then \
		echo "$$changed_files" | xargs clang-format -i; \
	else \
		echo "no changes to format."; \
	fi