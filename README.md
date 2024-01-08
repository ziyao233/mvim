# mVim

A __MICRO__ copy of Vim

## Features

- Tiny, single-filed, no dependency
- Vim-like commands
- Support UTF-8 and wide characters
- Copy, cut and paste
- History Tracing
- Keyword highlighting
- Position Stack

## Compilation and Installation

Make sure you have `c99` on your system, which should refer to a
C99-compatible compiler. This project uses POSIX features and VT100 control
sequences to draw the screen, so check that your terminal supports it.

```shell
$ cd src && make install
```

Default installation path is specified in `/src/makefile` as `DIR_INSTALL`,
which could be modified at command line, e.g.:

```shell
$ make install DIR_INSTALL=$(HOME)/.local/bin
```

## Configuration

Default configuration is specified at compile time, and some of them could be
modified at runtime with `:set` command.

Here is a list of configuration options:

- `tabsize`: int, Width of a TAB character.
- `outputBuffersize` (Compile time only): int, the size of stdout's buffer
- `historySize` (Compile time only): int, how many changes could be traced.
- `highlightTrailingSpace`: bool, whether to highlight trailing spaces.
- `highlightKeywordColor`: int, color used to highlight keywords
(See Color List)
- `positionStackSize`: int, the size of Position Stack (See Position Stack)

## Color List

- `COLOR_BLACK`: 0
- `COLOR_RED`: 1
- `COLOR_GREEN`: 2
- `COLOR_YELLOW`: 3
- `COLOR_BLUE`: 4
- `COLOR_MAGENTA`: 5
- `COLOR_CYAN`: 6
- `COLOR_WHITE`: 7

## Position Stack

A stack is provided to ease browsing long files. There are two corresponding
commands for manipulate it.

- `push` (or `pu` for short): Push current position of cursor onto the position
  stack
- `pop` (or `po` for short): Move cursor to the position the last element on
  position stack specifies and pop it from the stack.

## Keyword Highlight

You could specify keywords in `/src/keywords.h`.

Keywords of C, Lua, C++, Golang and Elm have already been included.

## Contribution

This project is tailored for my own usage, but I have make some efforts to make
it adapt to wider requirements. Pull requests and patches will not be accepted.
If you want a feature to be added, make sure it isn't in the unsupported list
(``/UNSUPPORTED_LIST``), then give me an issue.

## LICENSE

By BSD-2-Clause License. This project is derived from kilo project. See
`/src/mvim.c` for details. The original license is `/LICENSE_KILO`.

## Sponsor

Help poor children in Uganda!

Consider donating to [ICCF](https://www.iccf.nl).

[Click here to give me a cup of coffee](https://afdian.net/a/ziyao)
