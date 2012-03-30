CC       = c99
CFLAGS   = -Wall -Wextra -pedantic
CFLAGS  += -pipe
LDFLAGS  = -Wl,--as-needed
LDFLAGS += -ldl # dlopen,...

HAS_CL  = 1
RELEASE = 0

ifeq (1,$(RELEASE))
  CFLAGS  = -O2 -march=native -mtune=native
  LDFLAGS = -Wl,-O1
else
  CFLAGS  += -g -ggdb3
endif

# sparse
SPARSE_DIR      = ../sparse-llvm
SPARSE_LIB      = sparse
SPARSE_LIB_PATH = $(SPARSE_DIR)/lib$(SPARSE_LIB).a
LDFLAGS         += -L$(SPARSE_DIR) -l$(SPARSE_LIB)

# Code Listener
PREDATOR_DIR = ../predator_cl
CL_DIR       = $(PREDATOR_DIR)/cl
CL_BUILD_DIR = $(PREDATOR_DIR)/cl_build
CL_LIB       = cl
CL_LIB_PATH  = $(CL_BUILD_DIR)/lib$(CL_LIB).a
# we reuse cl_test object built when building static cl library,
# but we could also build on our own (problem of "correct" flags)
#CL_TEST     = $(CL_DIR)/cl/cl_test.o
CL_TEST      = $(CL_BUILD_DIR)/CMakeFiles/cl_test.dir/cl_test.cc.o

ifeq (1,$(HAS_CL))
  # unresolved symbols are those provided by GCC when used as its plugin
  LDFLAGS += -L$(CL_BUILD_DIR) -l$(CL_LIB)
  CFLAGS  += -DHAS_CL
endif

PROGRAM  = clsp
OBJECTS  = clsp.o   \
  clsp_options.o    \
  clsp_proceed.o    \
  clsp_types.o      \
  type_enumerator.o \
  hash_table.o

# do not warn about C99 and similar issues in included headers
INC_DIR =  $(SPARSE_DIR) $(PREDATOR_DIR)/include/cl $(CL_DIR)
INC     =  $(foreach dir,$(INC_DIR),-isystem $(dir))
#CFLAGS +=  -Wno-system-headers


all: $(PROGRAM)

# solves both cl_test object and libcl.a (not clean, but parallel make ready)
$(CL_TEST) $(CL_LIB_PATH):
	make -C $(CL_DIR) CXX_DEFINES="-D__CL_IN"

$(PROGRAM): $(OBJECTS) $(CL_TEST) $(CL_LIB_PATH) $(SPARSE_LIB_PATH)
	g++ $(CFLAGS) $(INC) -fPIC -o $@ $(OBJECTS) $(CL_TEST) $(LDFLAGS)

$(OBJECTS):: %.o : %.c
	$(CC) $(CFLAGS) $(INC) -fPIC -o $@ -c $<

$(SPARSE_LIB_PATH):
	make -C $(SPARSE_DIR) $(notdir $@)

check: $(PROGRAM)
	./tests.sh

clean: clean-check
	$(RM) -f -- $(OBJECTS) core vgcore.*

clean-check-prev:
	./tests.sh clean_prev

clean-check:
	./tests.sh clean

mrproper: clean
	make -C $(SPARSE_DIR) clean
	make -C $(CL_DIR) clean

.PHONY: all clean check clean-check-prev clean-check mrproper
