# --------------------------------------------------
# Makefile for Heterogeneous Linked List in Fortran
# --------------------------------------------------

# Files Folder
src = ../src
example = ../examples

# Exe name
exe = test

# Compiler
Fort = gfortran
# Flags
W_flags = -Wall -Wextra -Werror
Opt_flags = -O3 -march=native -flto=8 -fno-fat-lto-objects -fno-strict-aliasing
All_flags = -g -ffree-line-length-none -fcheck=all -std=f2008

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

%.o: $(src)/%.f90
	$(Fort_Mod)  $<

%.o: $(example)/%.f90
	$(Fort_Mod)  $<

# Objects list and dependencies
linked_list.o: $(src)/linked_list.f90
test_link.o: $(example)/test_link.f90 linked_list.mod



#------------------------
# Clean make dir
clean:
	rm *.o *.mod $(exe)

#------------------------
# Run valgrind to debug memory
mem-debug:
	valgrind $(Val_flags) ./$(exe)
