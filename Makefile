CC ?= gcc
CFLAGS ?= -Wall -DTRAP="do { } while (0)" # -Wextra # -pedantic 
CFLAGS += -g #-O2

CL = cl
CL_LIB = lib$(CL).a
CL_DIR = .

CL_LIBS = -lboost_iostreams -lboost_system

SPARSE = sparse
SPARSE_LIB = lib$(SPARSE).a
SPARSE_DIR = .

PROGRAM = test
MAIN_SRC = $(PROGRAM).c
SRCS = hash_table.c type_enumerator.c
PROGRAM_LDFLAGS = -L$(SPARSE_DIR) -l$(SPARSE) -L$(CL_DIR) -l$(CL)

INC_DIR = include


.PHONY: all clean


all: $(PROGRAM)


$(PROGRAM): $(MAIN_SRC:.c=.o) $(SRCS:.c=.o)
	g++ -I$(INC_DIR) -o $@ $(CFLAGS) $^ $(CL_LIBS) $(PROGRAM_LDFLAGS) 

$(MAIN_SRC:.c=.o) $(SRCS:.c=.o): %.o : %.c
	$(CC) -I$(INC_DIR) -o $@ $(CFLAGS) -fPIC -c $<

clean:
	$(RM) -fr $(PROGRAM)
