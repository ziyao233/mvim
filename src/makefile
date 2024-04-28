# mVim
# File: /src/makefile
# Date: 2024.04.28
# By 2-BSD License.
# Copyright(C) 2022-2024 Ziyao.

# Tools
CC		?= c99
INSTALL		?= install
REMOVE		?= rm
ECHO		?= echo
CCLD		?= $(CC)

# Directories
DIR_INSTALL	= /usr/local
DIR_BIN		= $(DIR_INSTALL)/bin

# Files
FILE_OBJS	:= mvim.o
FILE_OUTPUT	:= mvim

ifeq ($(RELEASE), 1)
	CFLAGS_DBG	:= -O2
else
	CFLAGS_DBG	:= -O0 -g
endif

# Flags
FLAG_CC		?= -Wextra -Werror -Wall -pedantic $(CFLAGS_DBG)
FLAG_LD		?=

default: build

%.o: %.c mvim.conf.h
	@$(ECHO) [CC] $< $@
	@$(CC) $< -c $(FLAG_CC)

mvim.o: mvim.c mvim.conf.h keywords.h
	@$(ECHO) [CC] $< $@
	@$(CC) $< -c $(FLAG_CC)

build: $(FILE_OBJS)
	@$(ECHO) [LD] $(FILE_OBJS)
	@$(CCLD) $(FILE_OBJS) -o $(FILE_OUTPUT)

clean:
	@$(ECHO) [REMOVE] $(FILE_OBJS)
	-@$(REMOVE) $(FILE_OBJS)

disbuild:
	@$(ECHO) [REMOVE] $(FILE_OBJS)
	-@$(REMOVE) $(FILE_OBJS)
	@$(ECHO) [REMOVE] $(FILE_OUTPUT)
	-@$(REMOVE) $(FILE_OUTPUT)

install: build
	@$(ECHO) [INSTALL] $(FILE_OUTPUT) $(DIR_INSTALL)
	@$(INSTALL) -m0755 -D -p $(FILE_OUTPUT) $(DIR_BIN)
