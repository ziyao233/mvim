# mVim
# File: /src/makefile
# Date: 2023.03.22
# By 2-BSD License.
# Copyright(C) 2022-2023 Ziyao.

# Tools
CC		= c99
INSTALL		= install
REMOVE		= rm
ECHO		= echo
LD		= c99

# Directories
DIR_INSTALL	= /usr/local
DIR_BIN		= $(DIR_INSTALL)/bin

# Files
FILE_OBJS	:= mvim.o
FILE_OUTPUT	:= mvim

# Flags
FLAG_CC		= -O0 -Wextra -Werror -Wall -g -pedantic
FLAG_LD		=

default: build

%.o: %.c mvim.conf.h
	@$(ECHO) [CC] $< $@
	@$(CC) $< -c $(FLAG_CC)

mvim.o: mvim.c mvim.conf.h keywords.h
	@$(ECHO) [CC] $< $@
	@$(CC) $< -c $(FLAG_CC)

build: $(FILE_OBJS)
	@$(ECHO) [LD] $(FILE_OBJS)
	@$(LD) $(FILE_OBJS) -o $(FILE_OUTPUT)

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
