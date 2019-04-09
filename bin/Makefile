# --------------------------------------------------
# Makefile for Heterogeneous Linked List in Fortran
# --------------------------------------------------

# Files Folder
src = ../src

# Exe name
exe = test

# Compiler
Fort = gfortran-7
# Flags
W_flags = -Wall -Wextra -Werror
Opt_flags = -O3 -march=native -flto=8 -fno-fat-lto-objects -fno-strict-aliasing
All_flags = -g -ffree-line-length-none

# Valgrind debug
Val_flags = --leak-check=full --show-reachable=yes --track-origins=yes

# Concatenate flags and compiler into one command
Fort_Mod = $(Fort) -c $(All_flags) $(W_flags) $(Opt_flags)
Fort_Lin = $(Fort) -g $(Opt_flags)

#List of objects to compile
File_list = test_link.o linked_list.o

#------------------------
# Build
all: $(File_list)
	$(Fort_Lin) *.o -o $(exe)
#------------------------

# Generic rules
%.mod: %.o
	@true

%.o: $(src)/%.f08
	$(Fort_Mod)  $<

# Objects list and dependencies
linked_list.o: $(src)/linked_list.f08
test_link.o: $(src)/test_link.f08 linked_list.mod



#------------------------
# Clean make dir
clean:
	rm *.o *.mod $(exe)

#------------------------
# Run valgrind to debug memory
mem-debug:
	valgrind $(Val_flags) ./$(exe)
