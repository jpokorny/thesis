CC ?= gcc
CFLAGS ?= #-std=c99 -Wall -Wextra -pedantic
CFLAGS += -g -ggdb3 #-O2

CL_DIR = ../predator_cl/cl_build
CL = cl
CL_LIB = $(CL_DIR)/lib$(CL).a

SPARSE_DIR = ../sparse_devel
SPARSE = sparse
SPARSE_LIB = $(SPARSE_DIR)/lib$(SPARSE).a

PROGRAM_LDFLAGS = -L$(SPARSE_DIR) -l$(SPARSE) -L$(CL_DIR) -l$(CL)

PROGRAM = test
MAIN_SRC = $(PROGRAM).c
SRCS = hash_table.c type_enumerator.c

INC_DIR = include


.PHONY: all clean check clean-check


all: $(PROGRAM)


$(PROGRAM): $(MAIN_SRC:.c=.o) $(SRCS:.c=.o) $(SPARSE_LIB) $(CL_LIB)
	g++ -I$(INC_DIR) -o $@ $(CFLAGS)  $^ $(PROGRAM_LDFLAGS)

$(MAIN_SRC:.c=.o) $(SRCS:.c=.o): %.o : %.c
	$(CC) -I$(INC_DIR) -o $@ $(CFLAGS) -fPIC -c $<

$(SPARSE_LIB):
	make -C $(SPARSE_DIR) $(basename SPARSE_LIB)

$(CL_LIB):
	@echo Not implemented yet, prepare $@ manually

check: $(PROGRAM)
	./tests.sh

clean: clean-check
	$(RM) -f $(MAIN_SRC:.c=.o) $(SRCS:.c=.o)

clean-check:
	./tests.sh clean
