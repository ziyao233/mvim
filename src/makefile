# mVim
# File:/src/makefile
# Date:2022.12.23
# By 2-BSD License.
# Copyright(C) 2022 Ziyao. All rights reserved.

# Tools
CC		:= c99
INSTALL		:= install
REMOVE		:= rm
ECHO		:= echo
LD		:= c99

# Directories
DIR_INSTALL	:= /usr/local/bin

# Files
FILE_OBJS	:= mvim.o
FILE_OUTPUT	:= mvim

# Flags
FLAG_CC		:= -O0 -Wextra -Werror -Wall -g
FLAG_LD		:=

default: build

%.o: %.c
	@$(ECHO) [C99] $< $@
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
	@$(COPY) -D 0755 -p $(FILE_OUTPUT) $(DIR_INSTALL)
