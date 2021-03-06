###############################################################################
#
# Name:    DLang Scheme
# Type:    Application
# Author:  Mike Lowis
# License: BSD 2-Clause
#
###############################################################################

# Utility Function Definitions
#-----------------------------
# Function for generating an file list
flist = $(shell env find $(1) -name *.$(strip $(2)) -print)

# Project and Artifact Names
#---------------------------
PROJ_NAME   = dlang
TEST_RUNNER = test_runner

# File and Directory Settings
#----------------------------
# Root Directories
SRC_ROOT = source/

# File Extensions
SRC_EXT = scm

# Source File Lists
SRC_FILES = $(call flist, $(SRC_ROOT), $(SRC_EXT))

# Object File Lists
SRC_OBJS = $(SRC_FILES:%.$(SRC_EXT)=%.o)

# Compiler and Linker Options
#----------------------------
CSC = csc
CSCFLAGS = -c

# Build Rules
#------------

# List all rules not named for files/folders on disk
.PHONY: all release

all: release

release: $(PROJ_NAME)

# Binaries
$(PROJ_NAME): $(SRC_OBJS)
	@echo Linking $@...
	@$(CSC) -o $@ $(SRC_OBJS) $(LIBS)

# Object Files
$(SRC_OBJS): %.o : %.$(SRC_EXT)
	@echo $<
	@$(CSC) $(CSCFLAGS) -o $@ $<

# Cleanup
clean:
	@$(RM) $(SRC_OBJS)
	@$(RM) $(PROJ_NAME)*

