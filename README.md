# mVim

A __MICRO__ copy of Vim

## Features

- Tiny, single-filed, no dependency
- Vim-like commands
- Support UTF-8 and wide characters

## Compilation and Installation

Make sure you have ``c99`` on your system, which should refer to a
C99-compatible compiler. This project uses POSIX features and VT100 control
sequences to draw the screen, so check that your terminal supports it.

```shell
$ cd src && make install
```

Default installation path is specified in ``/src/makefile``, feel free to modify
that.

## Contribution

This project is tailored for my own usage, but I have make some efforts to make
it adapt to wider requirements. Pull requests and patches will not be accepted.
If you want a feature to be added, make sure it isn't in the unsupported list
(``/UNSUPPORTED_LIST``), then give me an issue.

## LICENSE

By BSD-2-Clause License. This project is derived from kilo project. See
``/src/mvim.c`` for details.

## Sponsor

[Click here to give me a cup of coffee](https://afdian.net/a/ziyao)
