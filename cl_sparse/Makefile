CC ?= gcc
CFLAGS ?= -Wall -Wno-unused-function #-std=c99 -Wall -Wextra -pedantic
CFLAGS += -g -ggdb3 #-O2

CL_DIR = ../predator_cl/cl_build
CL = cl
CL_LIB = $(CL_DIR)/lib$(CL).a

SPARSE_DIR = ../sparse_devel
SPARSE = sparse
SPARSE_LIB = $(SPARSE_DIR)/lib$(SPARSE).a

PROGRAM_LDFLAGS = -L$(SPARSE_DIR) -l$(SPARSE) -L$(CL_DIR) -l$(CL)

PROGRAM = cl_sparse
MAIN_SRC = $(PROGRAM).c
SRCS = hash_table.c type_enumerator.c

INC_DIR = include


.PHONY: all clean check clean-check-prev clean-check


all: $(PROGRAM)


$(PROGRAM): $(MAIN_SRC:.c=.o) $(SRCS:.c=.o) $(SPARSE_LIB) $(CL_LIB)
	g++ -I$(INC_DIR) -o $@ $(CFLAGS)  $^ $(PROGRAM_LDFLAGS)

$(MAIN_SRC:.c=.o) $(SRCS:.c=.o): %.o : %.c
	$(CC) -I$(INC_DIR) -o $@ $(CFLAGS) -fPIC -c $<

$(SPARSE_LIB):
	make -C $(SPARSE_DIR) $(shell basename $(SPARSE_LIB))

$(CL_LIB):
	@echo Not implemented yet, prepare $@ manually

check: $(PROGRAM)
	./tests.sh

clean: clean-check
	$(RM) -f $(MAIN_SRC:.c=.o) $(SRCS:.c=.o) core vgcore.*

clean-check-prev:
	./tests.sh clean_prev

clean-check:
	./tests.sh clean
