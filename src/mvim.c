/* -----------------------------------------------------------------------
 *
 * Copyright (c) 2022-2024 Yao Zi <ziyao at disroot dot com>.
 * Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef __linux__
#define _POSIX_C_SOURCE 200809L
#endif
#define _XOPEN_SOURCE

#include<assert.h>
#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include<errno.h>
#include<string.h>
#include<ctype.h>
#include<time.h>
#include<stdarg.h>
#include<wchar.h>
#include<locale.h>
#include<limits.h>
#include<wctype.h>
#include<stdbool.h>

#include<termios.h>
#include<sys/types.h>
#include<sys/ioctl.h>
#include<sys/time.h>
#include<sys/wait.h>
#include<unistd.h>
#include<fcntl.h>
#include<signal.h>

#include"mvim.conf.h"

typedef enum {
	COLOR_BLACK	= 0, COLOR_RED, COLOR_GREEN, COLOR_YELLOW, COLOR_BLUE,
	COLOR_MAGENTA, COLOR_CYAN, COLOR_WHITE
} Color;

typedef struct {
	char *suffix;
	wchar_t **keywords;
} Keyword_Class;

#include"keywords.h"

typedef struct {
	unsigned int unused	: 1;	// To simplify drawRowAt()
	unsigned int bold	: 1;	// [1m
	unsigned int italic	: 1;	// [3m
	unsigned int underline	: 1;	// [4m
	unsigned int reverse	: 1;	// [7m
	unsigned int color	: 3;
} Char_Attr;

/* This structure represents a single line of the file we are editing. */
typedef struct {
	int idx;		// Row index in the file, zero-based.
	int size;		// Size of the row, excluding the null term.
	wchar_t *chars;		// Row content.
	int asize;		// Size of attr
	Char_Attr *attr;	// Character attributes
} erow;

typedef struct {
	wchar_t *old, *new;
	size_t oldLines, newLines;
	int pos;
} Change;

static struct editorConfig {
	/*	Display	Status	*/
	int cx,cy;		// Cursor x and y position on screen
	int rowoff;		// Offset of row displayed.
	int rowBottom;		// Index of the row in the bottom
	enum {
		MODE_NORMAL, MODE_INSERT, MODE_VISUAL
	} mode;
	int isScreenFull;	// The screen is fully used (no '~')
	int sx, sy;		// Selected x and y, the beginnning position
				// of select

	/*	Screen Info	*/
	int screenrows;		// Number of rows that we can show
	int screencols;		// Number of cols that we can show
	int numrows;		// Number of rows
	int rawmode;		// Is terminal raw mode enabled?

	/*	File Info	*/
	erow *row;		// Rows
	int version;		// Timestamp
	char *filename;		// Currently open filename

	/*	Copy		*/
	wchar_t *copyBuffer;

	/*	History		*/
	Change *history;
	int newest, oldest;

	/*	Search		*/
	wchar_t *keyword;
	int lastMatchX, lastMatchY;

	/*	Keyword Highlight	*/
	wchar_t **keywords;

	/*	Position Stack		*/
	int *posStack;
	int posTop;
} E;

static struct editorConfig E;

enum KEY_ACTION {
	KEY_NULL	= 0,
	CTRL_B		= 2,
	CTRL_C		= 3,
	CTRL_D		= 4,
	CTRL_F		= 6,
	CTRL_BACKSPACE	= 8,
	TAB		= 9,
	CTRL_L		= 12,
	ENTER		= 13,
	CTRL_Q		= 17,
	CTRL_R		= 18,
	CTRL_S		= 19,
	CTRL_U		= 21,
	CTRL_Z		= 26,
	ESC		= 27,
	BACKSPACE	= 127,
/*
 * The following are just soft codes, not really reported by the
 * terminal directly
 */
	ARROW_LEFT	= 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/

void
disableRawMode(void) {
	if (E.rawmode) {
		tcsetattr(STDIN_FILENO, TCSANOW, &orig_termios);
		tcflush(STDIN_FILENO, TCIOFLUSH);
		E.rawmode = 0;
	}
}

/* Called at exit to avoid remaining in raw mode. */
void
editorAtExit(void)
{
	disableRawMode();

	for (int i = 0; i < E.numrows; i++) {
		free(E.row[i].chars);
		free(E.row[i].attr);
	}
	free(E.row);
	free(E.filename);
	free(E.copyBuffer);

	for (int i = 0; i < C.historySize; i++) {
		if (E.history[i].new)
			free(E.history[i].new);
		if (E.history[i].old)
			free(E.history[i].old);
	}
	free(E.history);

	free(E.posStack);

	/*	Reset cursor position	*/
	printf("\x1b[%d;0H\x1b[0K", E.screenrows + 1);
	return;
}

/* Raw mode: 1960 magic shit. */
int
enableRawMode(void) {
	struct termios raw;

	if (E.rawmode)
		return 0;
	if (!isatty(STDIN_FILENO))
		goto fatal;

	if (tcgetattr(STDIN_FILENO, &orig_termios) == -1)
		goto fatal;

	raw = orig_termios;  /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* output modes - disable post processing */
	raw.c_oflag &= ~(OPOST);
    /* control modes - set 8 bit chars */
	raw.c_cflag |= (CS8);
    /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer. */
	raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout. */
	raw.c_cc[VTIME] = 1;

    /* put terminal in raw mode after flushing */
	if (tcsetattr(STDIN_FILENO, TCSANOW, &raw) < 0)
		goto fatal;

	if (tcflush(STDIN_FILENO, TCIOFLUSH))
		goto fatal;
	E.rawmode = 1;
	return 0;


fatal:
	errno = ENOTTY;
	return -1;
}

/*
 *	Make stdout block-buffered for higher performance
 */
int
enableStdoutBuffer(void)
{
	char *buf = malloc(C.outputBufferSize);
	if (!buf)
		return -1;

	setvbuf(stdout, buf, _IOFBF, C.outputBufferSize);
	return 0;
}

/*	puts() writes an extra '\n', fputs() does not	*/
void
writeString(const char *s)
{
	fputs(s, stdout);
	return;
}

wchar_t *
wcsndup(const wchar_t *s, size_t n)
{
	wchar_t *copy = malloc(sizeof(wchar_t) * (n + 1));

	if (copy) {
		copy[n] = L'\0';
		wcsncpy(copy, s, n);
	}

	return copy;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int
editorReadKey(int fd) {
    int nread;
    char c, seq[3];
    while ((nread = read(fd, &c, 1)) == 0);
    if (nread == -1) exit(1);

    while(1) {
        switch(c) {
        case ESC:    /* escape sequence */
            /* If this is just an ESC, we'll timeout here. */
            if (read(fd, seq, 1) == 0) return ESC;
            if (read(fd, seq+1, 1) == 0) return ESC;

            /* ESC [ sequences. */
            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape, read additional byte. */
                    if (read(fd, seq+2, 1) == 0) return ESC;
                    if (seq[2] == '~') {
                        switch(seq[1]) {
                        case '3': return DEL_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        }
                    }
                } else {
                    switch(seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                    }
                }
            }

            /* ESC O sequences. */
            else if (seq[0] == 'O') {
                switch(seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
                }
            }
            break;
        default:
            return c;
        }
    }
}

/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor is stored at *rows and *cols and 0 is returned. */
int
getCursorPosition(int ifd, int ofd, int *rows, int *cols)
{
	char buf[32];
	unsigned int i = 0;

	/* Report cursor location */
	if (write(ofd, "\x1b[6n", 4) != 4)
		return -1;

	/* Read the response: ESC [ rows ; cols R */
	while (i < sizeof(buf) - 1) {
		if (read(ifd, buf + i, 1) != 1)
			break;
		if (buf[i] == 'R')
			break;
		i++;
	}
	buf[i] = '\0';

	/* Parse it. */
	if (buf[0] != ESC || buf[1] != '[')
		return -1;
	if (sscanf(buf+2,"%d;%d", rows, cols) != 2)
		return -1;

	return 0;
}

/* Try to get the number of columns in the current terminal. If the ioctl()
 * call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error. */
int
getWindowSize(int ifd, int ofd, int *rows, int *cols)
{
	struct winsize ws;

	if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
		/* ioctl() failed. Try to query the terminal itself. */
		int orig_row, orig_col, retval;

		/* Get the initial position so we can restore it later. */
		retval = getCursorPosition(ifd, ofd, &orig_row, &orig_col);
		if (retval == -1)
			goto failed;

		/* Go to right/bottom margin and get position. */
		if (write(ofd, "\x1b[999C\x1b[999B", 12) != 12)
			goto failed;
		retval = getCursorPosition(ifd, ofd, rows, cols);
		if (retval == -1)
			goto failed;

		/* Restore position. */
		char seq[32];
		snprintf(seq, 32, "\x1b[%d;%dH", orig_row, orig_col);
		if (write(ofd, seq, strlen(seq)) == -1)
			return -1;
		return 0;
	} else {
		*cols = ws.ws_col;
		*rows = ws.ws_row;
		return 0;
	}

failed:
	return -1;
}

/* ======================= Editor rows implementation ======================= */

static void
getSelectedRange(int *sx, int *sy, int *ex, int *ey)
{
	int cy = E.rowoff + E.cy;
	if (cy > E.sy) {
		*ey = cy;
		*ex = E.cx;
		*sy = E.sy;
		*sx = E.sx;
	} else if (cy == E.sy) {
		*sy = *ey = cy;
		*sx = E.cx > E.sx ? E.sx : E.cx;
		*ex = E.cx > E.sx ? E.cx : E.sx;
	} else {
		*ey = E.sy;
		*ex = E.sx;
		*sy = cy;
		*sx = E.cx;
	}
	return;
}

static void
renderSelect(erow *row, int y)
{
	if (!row->size)
		return;

	int sy,sx,ey,ex;
	getSelectedRange(&sx, &sy, &ex, &ey);

	if (y < sy || y > ey)
		return;

	for (int i = (y == sy ? sx : 0);i <= (y == ey ? ex : row->asize - 1);i++)
		row->attr[i].reverse = !row->attr[i].reverse;

	return;
}

static void
renderTrailingSpace(erow *row)
{
	if (!row->size)
		return;

	for (int i = row->size - 1; isspace(row->chars[i]); i--)
		row->attr[i] = (Char_Attr) {
						.reverse	= 1,
						.color		= COLOR_RED,
					    };
}

static inline int
isSeperator(wchar_t *p)
{
	return !iswalnum(*p) && *p != '_' && *p != '-';
}

static void
renderKeywords(erow *row)
{
	if (!row->size)
		return;

	for (wchar_t **keyword = E.keywords; *keyword; keyword++) {
		size_t len = wcslen(*keyword);
		for (wchar_t *p = wcsstr(row->chars, *keyword); p;
		     p = wcsstr(p, *keyword)) {
		     	if ((p != row->chars && !isSeperator(p - 1)) ||
			    !isSeperator(p + len)) {
				p += len;
				continue;
			}
			for (size_t i = 0; i < len; i++)
				row->attr[p - row->chars + i].color =
					C.highlightKeywordColor;
			p += len;
		}
	}
	return;
}

/*
 *	Render character attributes
 */
void
editorUpdateRow(erow *row)
{
	if (row->size != row->asize) {
		row->attr = realloc(row->attr,sizeof(Char_Attr) * row->size);
		row->asize = row->size;
	}

	for (int i = 0;i < row->asize;i++) {
		row->attr[i] = (Char_Attr) {
						.color	= COLOR_WHITE,
					   };
	}

	int y = row - E.row;

	if (E.keywords)
		renderKeywords(row);

	if (E.mode == MODE_VISUAL)
		renderSelect(row, y);

	if (C.highlightTrailingSpace)
		renderTrailingSpace(row);

	return;
}

void
editorUpdateRange(int yStart, int yEnd)
{
	for (int i = yStart;i <= yEnd;i++)
		editorUpdateRow(E.row + i);
	return;
}

/* Insert a row at the specified position, shifting the other rows on the bottom
 * if required. */
void
editorInsertRow(int at, const wchar_t *s, size_t len)
{
	if (at > E.numrows)
	    return;
	E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
	if (at != E.numrows) {
		memmove(E.row + at + 1, E.row + at,
			sizeof(E.row[0]) * (E.numrows - at));
		for (int j = at + 1; j <= E.numrows; j++)
			E.row[j].idx++;
	}

	E.row[at].size	= len;
	E.row[at].attr	= NULL;
	E.row[at].asize	= 0;
	E.row[at].chars	= malloc(sizeof(wchar_t) * (len + 1));
	wcsncpy(E.row[at].chars, s, len);
	E.row[at].chars[len]	= L'\0';
	E.row[at].idx		= at;
	editorUpdateRow(E.row + at);
	E.numrows++;
}

static
wchar_t *convertToWideCharFallback(const char *mbs)
{
	wchar_t *s = malloc(sizeof(wchar_t) * (strlen(mbs) + 1));
	size_t i = 0;
	for (; i < strlen(mbs); i++)
		s[i] = mbs[i] & 0x80 ? mbs[i] : L'?';

	s[i] = L'\0';
	return s;
}

void
editorInsertRowMb(int at, const char *mbs)
{
	size_t charNum = mbstowcs(NULL, mbs, 0);
	wchar_t *s;
	if (charNum == (size_t)(-1)) {
		s = convertToWideCharFallback(mbs);
		charNum = wcslen(s);
	} else {
		s = malloc(sizeof(wchar_t) * (charNum + 1));
		mbstowcs(s, mbs, charNum + 1);
	}
	editorInsertRow(at, s, charNum);
	free(s);
	return;
}

/* Free row's heap allocated stuff. */
void
editorFreeRow(erow *row)
{
	free(row->chars);
	free(row->attr);
	return;
}

/*
 * Remove the row at the specified position, shifting the remainign on the
 * top
 */
void
editorDelRow(int at)
{
	if (at >= E.numrows)
		return;
	erow *row = E.row + at;
	editorFreeRow(row);
	memmove(E.row + at, E.row + at + 1,
		sizeof(E.row[0]) * (E.numrows - at - 1));

	for (int j = at; j < E.numrows - 1; j++)
		E.row[j].idx++;
	E.numrows--;
	return;
}

/* Turn the editor rows into a single heap-allocated string.
 * Returns the pointer to the heap-allocated string and populate the
 * integer pointed by 'buflen' with the size of the string, escluding
 * the final nulterm. */
char *
editorRowsToString(int *buflen)
{
	char *buf = NULL,*p;
	int totlen = 0;

    /* Compute count of bytes */
	for (int j = 0; j < E.numrows; j++)
		totlen += wcstombs(NULL, E.row[j].chars, 0) + 1;	// for '\n'
	*buflen = totlen;
	totlen++;			// '\0'

	p = buf = malloc(totlen);
	for (int j = 0; j < E.numrows; j++) {
		p += wcstombs(p, E.row[j].chars, totlen);
		*p = '\n';
		p++;
	}
	*p = '\0';
	return buf;
}

/* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed. */
void
editorRowInsertChar(erow *row, int at, int c)
{
	if (at > row->size) {
/* Pad the string with spaces if the insert location is outside the
 * current length by more than a single character. */
	        int padlen = at - row->size;
/* In the next line +2 means: new char and null term. */
		row->chars = realloc(row->chars, sizeof(wchar_t) *
						 (row->size + padlen + 2));
		for (int i = 0;i < padlen;i++)
			row->chars[row->size + i] = L' ';
		row->chars[row->size + padlen + 1] = '\0';
		row->size += padlen + 1;
	} else {
        /* If we are in the middle of the string just make space for 1 new
         * char plus the (already existing) null term. */
		row->chars = realloc(row->chars, sizeof(wchar_t) *
						 (row->size + 2));
		memmove(row->chars + at + 1, row->chars + at,
			sizeof(wchar_t) * (row->size - at + 1));
		row->size++;
	}
	row->chars[at] = c;
	editorUpdateRow(row);
	return;
}

/* Append the string 's' at the end of a row */
void
editorRowAppendString(erow *row, wchar_t *s, size_t len)
{
	row->chars = realloc(row->chars, sizeof(wchar_t) *
					 (row->size + len + 1));
	wcsncat(row->chars, s, len);
	row->size += len;
	editorUpdateRow(row);
	return;
}

/* Delete the character at offset 'at' from the specified row. */
void
editorRowDelChar(erow *row, int at)
{
	if (row->size <= at)
		return;
	memmove(row->chars + at,row->chars + at + 1,sizeof(wchar_t) *
						    (row->size - at));
	row->size--;
	row->asize--;
	editorUpdateRow(row);
	return;
}

/* Insert the specified char at the current prompt position. */
void
editorInsertChar(int c)
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

    /* If the row where the cursor is currently located does not exist in our
     * logical representaion of the file, add enough empty rows as needed. */
	if (!row) {
		while(E.numrows <= filerow)
			editorInsertRow(E.numrows, L"", 0);
	}
	row = &E.row[filerow];
	editorRowInsertChar(row, E.cx, c);
	E.cx++;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed. */
void
editorInsertNewline(void)
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

	if (!row) {
		if (filerow == E.numrows) {
			editorInsertRow(filerow, L"", 0);
			goto fixcursor;
		}
		return;
	}
/*
 * If the cursor is over the current line size, we want to conceptually
 * think it's just over the last character.
 */
	if (E.cx >= row->size)
		E.cx = row->size;

	if (E.cx == 0) {
		editorInsertRow(filerow, L"", 0);
	} else {
	/* We are in the middle of a line. Split it between two rows. */
		editorInsertRow(filerow + 1, row->chars + E.cx,
				row->size - E.cx);
		row = &E.row[filerow];
		row->chars[E.cx] = L'\0';
		row->size = E.cx;
		editorUpdateRow(row);
	}

fixcursor:
	if (E.cy == E.rowBottom && E.isScreenFull)
		E.rowoff++;
	else
		E.cy++;

	E.cx = 0;
	return;
}

void editorStartChange(int sy, int ey);
void editorCommitChange(int sy, int ey);

/* Delete the char at the current prompt position. */
static void
editorDelChar(void)
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

	if (!row || (E.cx == 0 && filerow == 0))
		return;

	if (!E.cx) {
        /* Handle the case of column 0, we need to move the current line
         * on the right of the previous one. */
		editorCommitChange(filerow, filerow);
		int newX = E.cx + E.row[filerow - 1].size;
		editorRowAppendString(&E.row[filerow - 1], row->chars,
				      row->size);
		editorDelRow(filerow);
		row = NULL;
		if (E.cy == 0) {
			E.rowoff--;
		} else {
			E.cy--;
		}
		E.cx = newX;
		editorStartChange(E.cy + E.rowoff, E.cy + E.rowoff - 1);
	} else {
		editorRowDelChar(row, E.cx - 1);
		E.cx--;
	}

	if (row)
		editorUpdateRow(row);
}

wchar_t *
editorCopyRange(int sx, int sy, int ex, int ey)
{
	int size = 1;				// '\0';
	for (int i = sy;i <= ey;i++)
		size += E.row[i].size + 1;	// '\n'

	wchar_t *copy = malloc(sizeof(wchar_t) * size);
	assert(copy);
	copy[0] = '\0';

	for (int i = sy;i <= ey;i++) {
		if (E.row[i].chars)
			wcsncat(copy, E.row[i].chars + (i == sy ? sx : 0),
				i == ey ? (ex + 1 - (i == sy ? sx : 0)) :
					  E.row[i].size);
		wcscat(copy,L"\n");
	}
	return copy;
}

void
editorPaste(wchar_t *s)
{
	for (int i = 0;s[i];i++) {
		if (s[i] == L'\n') {
			editorInsertNewline();
		} else {
			editorInsertChar(s[i]);
		}
	}
}

/* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error. */
int
editorOpen(const char *filename)
{
	FILE *fp;

	E.version = 0;
	free(E.filename);
	size_t fnlen = strlen(filename);
	E.filename = strdup(filename);

	const char *suffix = filename + fnlen;
	while (suffix > filename && *suffix != '.')
		suffix--;
	for (Keyword_Class *p = K; p->suffix; p++) {
		if (!strcmp(p->suffix, suffix)) {
			E.keywords = p->keywords;
			break;
		}
	}

	fp = fopen(filename, "r");
	if (!fp) {
		if (errno != ENOENT) {
			perror("Opening file");
			exit(1);
		}
		return -1;
	}

	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	while((linelen = getline(&line, &linecap, fp)) != -1) {
		if (linelen && (line[linelen - 1] == '\n' ||
		    line[linelen - 1] == '\r'))
			line[--linelen] = '\0';
		editorInsertRowMb(E.numrows, line);
	}
	free(line);
	fclose(fp);
	E.version = 0;

	return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int
editorSave(void) {
	int len;
	char *buf = editorRowsToString(&len);
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
	if (fd == -1)
		goto writeerr;

    /* Use truncate + a single write(2) call in order to make saving
     * a bit safer, under the limits of what we can do in a small editor. */
	if (ftruncate(fd, len) == -1)
		goto writeerr;
	if (write(fd, buf, len) != len)
		goto writeerr;

	close(fd);
	free(buf);
	E.version	= 0;
	E.newest	= 0;
	E.oldest	= 0;
	return 0;

writeerr:
	free(buf);
	if (fd != -1)
		close(fd);
	return 1;
}

/*
 *	|<---------------------------------->_
 *		editorWidthFrom()	    Cursor
 */
int
editorWidthFrom(int start)
{
	int width = 0;
	erow *row = E.row + E.rowoff + E.cy;
	for (int i = start;i - start < E.cx;i++) {
		int t = wcwidth(row->chars[i]);
		width += t >= 0			? t			    :
			 row->chars[i] == TAB	? C.tabsize - C.tabsize % 8 :
						  1;
	}
	return width;
}

/* ============================= Terminal update ============================ */

static inline void
switchAttr(Char_Attr *old, Char_Attr *new)
{
	if (old->color != new->color)
		printf("\x1b[3%um", (unsigned int)new->color);
	old->color = new->color;

	if (*(uint8_t*)old == *(uint8_t*)new)
		return;

	writeString("\x1b[0m");
	if (new->bold)
		writeString("\x1b[1m");
	if (new->italic)
		writeString("\x1b[3m");
	if (new->underline)
		writeString("\x1b[4m");
	if (new->reverse)
		writeString("\x1b[7m");
	printf("\x1b[3%um", (unsigned int)new->color);

	return;
}

static inline int
drawRowAt(int at, int remainSpace, int write)
{
	erow *row = E.row + at;
	int line = 0;

	Char_Attr lastAttr = (Char_Attr) {
						.unused = 1,
					 };
	for (int i = 0,width = 0;i < row->size;i++) {
		int t = wcwidth(row->chars[i]);
		// Normal characters, TAB or control charaters
		t = t >= 0		 ? t				 :
		    row->chars[i] == TAB ? C.tabsize - width % C.tabsize :
					   1;

		/*	Wrapping	*/
		if (width + t > E.screencols) {
			width = 0;
			if (write)
				writeString("\x1b[0K\r\n");
			line++;
			if (line >= remainSpace)
				return 0;
		}

		if (*(uint8_t*)(row->attr + i) != *(uint8_t*)&lastAttr) {
			switchAttr(&lastAttr, row->attr + i);
			lastAttr = row->attr[i];
		}
		if (write) {
			if (row->chars[i] == TAB) {
				/*	Handle TABs	*/
				for (int i = 0;i < t;i++)
					putchar(' ');
			} else if (!iswprint(row->chars[i])) {
				/*	Control characters	*/
				putchar('?');
			} else {
				/*	Normal ones		*/
				char s[MB_LEN_MAX];
				s[wctomb(s, row->chars[i])] = '\0';
				writeString(s);
			}
		}
		width += t;
	}

	return line + 1;
}

/* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'. */
void
editorRefreshScreen(int write)
{
	writeString("\x1b[?25l");	// Hide cursor.
	writeString("\x1b[H");		// Go home.

	int printedLine = 0, y = 0, cursorY = 0;
	E.isScreenFull = true;
	for (int i = 0 ;y < E.screenrows; y += printedLine, i++) {
		int filerow = E.rowoff + i;

		if (filerow >= E.numrows) {
			E.isScreenFull = false;
			if (!write)
				continue;
			if (!E.numrows && printedLine == E.screenrows / 2) {
				char welcome[80];
				int wellen = snprintf(welcome, sizeof(welcome),
						      "mVim\x1b[0K\r\n");
				int padding = (E.screencols - wellen) / 2;
				if (padding) {
					putchar('~');
					padding--;
				}
				while(padding--)
					putchar(' ');

				writeString(welcome);
			} else {
				writeString("~\x1b[0K\r\n");
			}
			printedLine = 1;
			continue;
		}

		printedLine = drawRowAt(filerow, E.screenrows - y, write);

		if (!printedLine)
			break;

		if (i == E.cy)
			cursorY = y;
		if (filerow < E.numrows)
			E.rowBottom = i;

		if (write) {
			writeString("\x1b[0m");
			writeString("\x1b[37m");
			writeString("\x1b[0K");
			writeString("\r\n");
		}
	}

	if (!write)
		return;

	/*
	 * Put cursor at its current position. Note that the horizontal
	 * position at which the cursor is displayed may be different
	 * compared to 'E.cx' because of TABs and wide characters
	 */
	int cx = 0;
	int filerow = E.rowoff + E.cy;
	int t = C.tabsize;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
	if (row) {
		for (int i = 0;i < E.cx && i < row->size;i++) {
			int width = wcwidth(row->chars[i]);
			width = width >= 0		? width		:
			        row->chars[i] == TAB	? t - cx % t	:
							  1;
			cursorY += cx + width < E.screencols ? 0 : 1;
			cx = cx + width < E.screencols	? cx + width	:
			     row->chars[i] == TAB	? cx + width -
							  E.screencols	:
							  0;
		}
	}

	/* Create a one row status. */
	char status[80],rstatus[80];
	int len = snprintf(status, sizeof(status),"%s",
			   E.mode == MODE_INSERT ? "-- INSERT --" :
			   E.mode == MODE_VISUAL ? "-- VISUAL --" :"");
	int rlen = snprintf(rstatus, sizeof(rstatus), "%d,%d-%d    %d%%",
			    E.rowoff + E.cy + 1, E.cx + 1, cx + 1,
			    E.numrows ? E.rowoff * 100 / E.numrows : 100);
	if (len > E.screencols)
		len = E.screencols;

	fwrite(status, 1, len, stdout);
	while(len < E.screencols) {
		if (E.screencols - len == rlen) {
			writeString(rstatus);
			break;
		} else {
			putchar(' ');
			len++;
		}
	}

	printf("\x1b[%d;%dH", cursorY + 1, cx + 1);
	writeString("\x1b[?25h");		// Show cursor
	fflush(stdout);				// stdout is block-buffered
	return;
}

/* ========================= Change Tracing ================================  */

void
editorStartChange(int sy, int ey)
{
	Change *this = E.history + (E.version + 1) % C.historySize;
	if (this->old)
		free(this->old);
	if (this->new)
		free(this->new);

	E.history[(E.version + 1) % C.historySize] = (Change) {
			.pos		= sy,
			.oldLines	= ey - sy + 1,
			.old		= ey < sy ?
				NULL					:
				editorCopyRange(0, sy,
						E.row[ey].size, ey),
		};
}

void
editorCommitChange(int sy, int ey)
{
	E.version++;
	Change *this	= E.history + E.version % C.historySize;
	this->newLines	= ey - sy + 1;
	this->new	= ey < sy ?
		NULL : editorCopyRange(0, sy, E.row[ey].size, ey);

	if (E.oldest + C.historySize == E.newest)
		E.oldest++;
	E.newest = E.version;
}

void editorMoveCursorTo(int y, int x);

/*
 *	Replace n lines at pos with new
 */
static void
editorReplace(int pos, size_t n, wchar_t *new)
{
	for (size_t i = 0; i < n; i++)
		editorDelRow(pos);
	editorMoveCursorTo(pos, 0);
	if (new)
		editorPaste(new);
}

static void
editorUndoChange(void)
{
	if (E.version == E.oldest)
		return;

	Change *this = E.history + E.version % C.historySize;
	editorReplace(this->pos, this->newLines, this->old);
	E.version--;
}

static void
editorRedoChange(void)
{
	if (E.version == E.newest)
		return;

	Change *this = E.history + (E.version + 1) % C.historySize;
	editorReplace(this->pos, this->oldLines, this->new);
	E.version++;
}

/* ========================= Editor events handling  ======================== */

void
editorMoveCursor(int key)
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
	int leftWidth = editorWidthFrom(0);

	switch(key) {
	case ARROW_LEFT:
		if (E.cx)
			E.cx--;
		return;
	case ARROW_RIGHT:
		if (row && E.cx < row->size - (E.mode != MODE_INSERT))
			E.cx++;
		return;
	case ARROW_UP:
		if (E.cy) {
			E.cy--;
		} else {
			if (E.rowoff)
				E.rowoff--;
		}
		break;
	case ARROW_DOWN:
		if (filerow < E.numrows - 1) {
			if (E.cy == E.rowBottom && E.isScreenFull) {
				E.rowoff++;
				editorRefreshScreen(false);
				E.cy = E.rowBottom;
			} else {
				E.cy++;
			}
		}
		break;
	}

	/*
	 *	When moving the cursor up/down,keeping the horizontal position
	 *	approximately equal (precisely the same may not be possible,
	 *	as wide characters (CJK) cannot be splitted)
	 */
	row = E.row + E.cy + E.rowoff;
	int cx = 0,width = 0;
	while (cx < row->size && width <= leftWidth) {
		int t = wcwidth(row->chars[cx]);
		width += t >= 0		       ? t			       :
			 row->chars[cx] == TAB ? C.tabsize - width % C.tabsize :
						 1;
		cx++;
	}
	E.cx = cx > 0 ? cx - 1 : 0;

	return;
}

void
editorMoveCursorTo(int y, int x)
{
	/*	Do the moving only the position is not on screen	*/
	if (E.rowBottom + E.rowoff >= y &&
	    E.rowoff <= y) {
		E.cy = y - E.rowoff;
		E.cx = x;
		return;
	}

	E.rowoff	= y;
	E.cy		= 0;
	E.cx		= x;
	do {
		E.rowoff--;
		E.cy++;
		editorRefreshScreen(false);
	} while (E.rowBottom + E.rowoff == y);
	E.rowoff++;
	E.cy--;
	return;
}

void
editorReplaceChar(int y, int x, int new)
{
	if (E.row[y].size) {
		editorStartChange(y, y);
		E.row[y].chars[x] = new;
		editorCommitChange(y, y);
		editorUpdateRow(E.row + y);
	}
	return;
}

static void
exitRawMode(char promot)
{
	disableRawMode();

	// Overwrite the status line
	printf("\x1b[%d;%dH\x1b[0K%c", E.screenrows + 1, 0, promot);
	fflush(stdout);			// stdout is block-buffered
	return;
}

static void
normalModeError(const char *s)
{
	enableRawMode();
	writeString(s);
	fflush(stdout);
	editorReadKey(STDOUT_FILENO);
	return;
}

static inline int
isCmd(const char *s, const char *cmd)
{
	size_t len = strlen(cmd);
	return !strncmp(s, cmd, len) && (isspace(s[len]) || !s[len]) ?
			len : 0;
}

static Mvim_Conf_Entry *
getConfEntry(const char *name)
{
	for (unsigned int i = 0;
	     i < sizeof(gConfList) / sizeof(Mvim_Conf_Entry);
	     i++) {
		if (!strcmp(name, gConfList[i].name))
			return gConfList + i;
	}
	return NULL;
}

static int
isNumber(const char *p)
{
	if (!*p)
		return 0;
	while (*p) {
		if (!isdigit(*p))
			return 0;
		p++;
	}
	return 1;
}

static void
expandTab(void)
{
	editorStartChange(0, E.numrows - 1);
	for (int y = 0; y < E.numrows; y++) {
		erow *row = E.row + y;
		for (int x = 0; x < row->size; x++) {
			if (row->chars[x] == TAB) {
				editorRowDelChar(row, x);
				for (int i = C.tabsize - x % C.tabsize;
				     i;
				     i--) {
					editorRowInsertChar(row, x, L' ');
					x++;
				}
				x--;
			}
		}
	}
	editorCommitChange(0, E.numrows - 1);
	return;
}

static int
isPosStackOverflow(int top)
{
	return top < 0 || top > C.positionStackSize;
}

static inline void
pushPosition(void)
{
	if (isPosStackOverflow(E.posTop + 1)) {
		normalModeError("Position stack overflow");
		return;
	}
	E.posStack[E.posTop] = E.rowoff + E.cy;
	E.posTop++;
	return;
}

static inline void
popPosition(void)
{
	if (isPosStackOverflow(E.posTop - 1)) {
		normalModeError("Position stack overflow");
		return;
	}
	E.posTop--;
	int line = E.posStack[E.posTop];
	editorMoveCursorTo(line, 0);
	return;
}

static inline void
commandWriteFile(const char *cmd)
{
	const char *p = cmd + 1;
	while (isspace(*p))
		p++;

	int ret = 0;

	if (*p) {
		free(E.filename);
		E.filename = strdup(p);
		ret = editorSave();
	} else {
		if (E.version)
			ret = editorSave();
	}

	if (ret)
		normalModeError("Cannot save file");

	return;
}

static inline void
commandMode(void)
{
	exitRawMode(':');

	char *cmd = NULL;
	size_t size = 0;
	ssize_t length = getline(&cmd, &size, stdin);
	/*
	 *	cmd will get leaked if exit() is called when "q", "wq" or "q!"
	 *	is used and the editor exits. It doesn't matter.
	 */
	if (length < 0)
		goto end;
	cmd[length - 1] = '\0';

	int offset = 0;
	if (!strcmp(cmd, "q")) {
		if (E.version) {
			normalModeError("No write since last change");
		} else {
			free(cmd);
			exit(0);
		}
	} else if (!strcmp(cmd, "q!")) {
		exit(0);
	} else if (isCmd(cmd, "w")) {
		commandWriteFile(cmd);
		goto end;
	} else if (!strcmp(cmd, "wq")) {
		if (E.version && editorSave()) {
			normalModeError("Cannot save file");
			goto end;
		}
		free(cmd);
		exit(0);
	} else if ((offset = isCmd(cmd, "set"))) {
		char *name = malloc(length);
		int value = 0;
		if (sscanf(cmd + offset,"%s %d",name,&value) != 2) {
			normalModeError("Wrong usage: set key value");
			goto freeName;
		}

		Mvim_Conf_Entry *entry = getConfEntry(name);
		if (!entry) {
			normalModeError("Invalid key.");
			goto freeName;
		}

		*(entry->value) = value;
freeName:
		free(name);
		for (int i = 0; i < E.numrows; i++)
			editorUpdateRow(E.row + i);
	} else if (isCmd(cmd, "expandTab")) {
		expandTab();
	} else if (isCmd(cmd, "push") || isCmd(cmd, "pu")) {
		pushPosition();
	} else if (isCmd(cmd, "pop") || isCmd(cmd, "po")) {
		popPosition();
	} else if (isNumber(cmd)) {
		int line = atoi(cmd);
		if (line > 0 && line <= E.numrows)
			editorMoveCursorTo(line - 1, 0);
		else
			normalModeError("Out of range.");
	} else if (!*cmd) {
		goto end;
	} else if (isCmd(cmd, "iccf")) {
		normalModeError("Help poor children in Uganda\r\n"
				"See https://www.iccf.nl for more information.");
	} else {
		normalModeError("Unknown command");
	}

end:
	free(cmd);

	enableRawMode();
	return;
}

static wchar_t
readWideChar(int startByte)
{
	int length = 0;
	char tmp[16] = {startByte};
	while (startByte & 0x80) {
		length++;
		startByte <<= 1;
	}
	if (!length)
		return startByte;

	// No fread()! The inner buffer's state is unknown
	read(STDIN_FILENO, tmp + 1, length - 1);
	wchar_t wideChar = 0;
	if (mbtowc(&wideChar, tmp, length) < 0)
		return L' ';
	return wideChar;
}

static inline void
deleteRange(int y, int x, int length)
{
	for (int i = 0;i < length;i++)
		editorRowDelChar(E.row + y, x);
	return;
}

static void
enterInsertMode(int y)
{
	E.mode = MODE_INSERT;
	editorStartChange(y, y);
}

static void
searchFor(void)
{
	int y = 0;
	wchar_t *p = NULL;

	wchar_t *keyword = E.keyword;
	if (keyword[0] == L'^' && keyword[1] != L'^') {
		keyword++;
		for (y = 1; y <= E.numrows; y++) {
			erow *row = E.row + (E.rowoff + E.cy + y) % E.numrows;
			if (!wcsncmp(row->chars, keyword,
				     wcslen(keyword))) {
				p = row->chars;
				break;
			}
		}
	} else {
		if (wcsncmp(E.keyword, L"^^", 2))
			keyword++;
		for (y = 0; y <= E.numrows; y++) {
			p = wcsstr(E.row[(E.rowoff + E.cy + y) %
						E.numrows].chars +
				   !y * E.cx,
				   E.keyword);
			if (p)
				break;
		}
	}

	if (p) {
		enableRawMode();
		y = (E.rowoff + E.cy + y) % E.numrows;
		editorMoveCursorTo(y, p - E.row[y].chars + wcslen(keyword));
	} else {
		normalModeError("Cannot find the keyword.");
		enableRawMode();
	}

	return;
}

static void
searchMode(void)
{
	exitRawMode('/');

	char *keyword = NULL;
	size_t size = 0;
	ssize_t length = getline(&keyword, &size, stdin);
	keyword[length - 1] = '\0';

	if (strlen(keyword) && !(strlen(keyword) == 1 && keyword[0] == '^')) {
		if (E.keyword)
			free(E.keyword);

		size_t length = mbstowcs(NULL, keyword, 0) + 1;
		wchar_t *wKeyword = malloc(length * sizeof(wchar_t));
		mbstowcs(wKeyword, keyword, length);
		E.keyword = wKeyword;

		searchFor();
	} else {
		free(keyword);
		enableRawMode();
	}

	return;
}

void
editorAutoIndent(void)
{
	if (!C.autoIndent)
		return;

	int width = 0;
	for (wchar_t *p = E.row[E.rowoff + E.cy - 1].chars;
	     iswspace(*p);
	     p++) {
		width += *p == TAB ? C.tabsize : 1;	// FIXME: wider space?
	}

	while (width >= C.tabsize) {
		editorInsertChar(L'\t');
		width -= C.tabsize;
	}
	for (; width; width--)
		editorInsertChar(L' ');
	return;
}

static void
scrollLines(int direction, int count)
{
	for (int i = 0; i < count; i++)
		editorMoveCursor(direction);
	return;
}

char *
getKeywordUnderCursor(void)
{
	int start = E.cx + 1, end = E.cx;
	int cy = E.rowoff + E.cy;
	const wchar_t *line = E.row[cy].chars;

	while (start > 0 && iswalnum(line[start - 1]))
		start--;

	while (end <= E.row[cy].size && iswalnum(line[end]))
		end++;

	if (end < start)
		return NULL;

	wchar_t *wcs = wcsndup(line + start, end - start);
	size_t len = wcstombs(NULL, wcs, 0);
	if (len == (size_t)-1)
		return NULL;

	char *mbs = malloc(len + 1);
	wcstombs(mbs, wcs, len + 1);

	free(wcs);

	return mbs;
}

static inline void
getManual(void)
{
	exitRawMode('\0');

	char *keyword = getKeywordUnderCursor();
	if (!keyword) {
		normalModeError("No keyword under cursor");
		enableRawMode();
		return;
	}

	char *manCommand = malloc(4 + strlen(keyword) + 1);
	strcpy(manCommand, "man ");
	strcat(manCommand, keyword);

	int pid = fork();
	if (!pid)
		execlp("/bin/sh", "sh", "-c", manCommand, NULL);
	else
		waitpid(pid, NULL, 0);

	free(manCommand);
	free(keyword);

	normalModeError("Press any key to continue");
	enableRawMode();
	return;
}

static void
processKeyNormal(int fd, int key)
{
	int y = E.cy + E.rowoff;
	switch (key) {
	case 'd':
		key = editorReadKey(fd);
		if (key == 'd') {
			editorStartChange(y, y);
			free(E.copyBuffer);
			E.copyBuffer = editorCopyRange(0, y, E.row[y].size, y);
			editorDelRow(y);
			if (!E.numrows) {
				editorInsertRow(0, L"", 0);
			} else if (E.numrows == y) {
				editorMoveCursor(ARROW_UP);
			}
		} else if (key == 'w') {
			editorStartChange(y, y);
			int x = E.cx;
			int isSpace = iswspace(E.row[y].chars[x]);
			while (iswspace(E.row[y].chars[x]) == isSpace)
				x++;
			deleteRange(y, E.cx, x - E.cx);
		} else if (key == '$') {
			editorStartChange(y, y);
			deleteRange(y, E.cx, E.row[y].size - E.cx);
		} else if (key == '0') {
			editorStartChange(y, y);
			deleteRange(y, 0, E.cx);
			E.cx = 0;
		} else {
			break;
		}

		editorCommitChange(y, key == 'd' ? y - 1 : y);
		break;
	case '$':
	case END_KEY:
		E.cx = E.row[y].size - 1;
		break;
	case '0':
	case HOME_KEY:
		E.cx = 0;
		break;
	case 'o':
		editorStartChange(y + 1, y);
		editorInsertRow(y + 1, L"", 0);
		enterInsertMode(y + 1);
		editorRefreshScreen(false);
		editorMoveCursor(ARROW_DOWN);
		editorAutoIndent();
		editorCommitChange(y + 1, y + 1);
		break;
	case 'a':
		enterInsertMode(y);
		editorMoveCursor(ARROW_RIGHT);
		break;
	case 'i':
		enterInsertMode(y);
		break;
	case 'v':
		E.mode	= MODE_VISUAL;
		E.sy	= y;
		E.sx	= E.cx;
		break;
	case 'h':
	case ARROW_LEFT:
	case CTRL_BACKSPACE:
	case BACKSPACE:
		editorMoveCursor(ARROW_LEFT);
		break;
	case 'l':
	case ARROW_RIGHT:
		editorMoveCursor(ARROW_RIGHT);
		break;
	case 'j':
	case ARROW_DOWN:
	case ENTER:
		editorMoveCursor(ARROW_DOWN);
		break;
	case 'k':
	case ARROW_UP:
		editorMoveCursor(ARROW_UP);
		break;
	case 'g':
		key = editorReadKey(fd);
		if (key == 'g') {
			E.rowoff = 0;
			E.cx	 = 0;
			E.cy	 = 0;
		}
		break;
	case 'G':
		editorMoveCursorTo(E.numrows - 1, 0);
		break;
	case 'x':
		if (E.row[y].size) {
			editorStartChange(y, y);
			editorRowDelChar(E.row + y, E.cx);
			editorCommitChange(y, y);
		}
		break;
	case 'r':
		key = editorReadKey(fd);
		editorReplaceChar(y, E.cx, readWideChar(key));
		break;
	case ':':
		commandMode();
		break;
	case 'p':
		if (E.copyBuffer) {
			editorStartChange(y, y - 1);
			editorPaste(E.copyBuffer);
			editorCommitChange(y, E.rowoff + E.cy);
		}
		break;
	case 'u':
		editorUndoChange();
		break;
	case CTRL_R:
		editorRedoChange();
		break;
	case '/':
		searchMode();
		break;
	case 'n':
		exitRawMode('\0');
		if (E.keyword)
			searchFor();
		break;
	case CTRL_D:
		scrollLines(ARROW_DOWN, E.screenrows / 2);
		break;
	case CTRL_U:
		scrollLines(ARROW_UP, E.screenrows / 2);
		break;
	case CTRL_Z:
		exitRawMode('\0');
		raise(SIGSTOP);
		enableRawMode();
		raise(SIGWINCH);
		break;
	case 'K':
		getManual();
		break;
	default:
		break;
	}
	return;
}

static inline void
processKeyInsert(int fd, int key)
{
	(void)fd;
	int y = E.rowoff + E.cy;
	switch (key) {
	case ESC:
		editorMoveCursor(ARROW_LEFT);
		E.mode = MODE_NORMAL;
		editorCommitChange(y, y);
		break;
	case ENTER:
		editorCommitChange(y, y);
		editorInsertNewline();
		editorAutoIndent();
		editorStartChange(y + 1, y);
		break;
	case CTRL_BACKSPACE:
	case BACKSPACE:
		editorDelChar();
		break;
	default:
		editorInsertChar(readWideChar(key));
		break;
	}
	return;
}

static void
exitVisualMode(int sy, int ey)
{
	E.mode = MODE_NORMAL;
	editorUpdateRange(sy, ey);
	return;
}

static void
visualCut(int sx, int sy, int ex, int ey)
{
	int li = E.row[ey].size - 1;		// Last index of the last line
	/*	One row only	*/
	if (sy == ey) {
		if (!sx && ex == li)
			editorDelRow(sy);
		else
			deleteRange(sy, sx, ex - sx + 1);
		return;
	}

	/*	The first/last line is fully selected, delete it	*/
	if (sx) {
		deleteRange(sy, sx, E.row[sy].size - E.sx);
		sy++;
	} else {
		editorDelRow(sy);
		ey--;
	}

	if (ex == li) {
		editorDelRow(ey);
	} else {
		deleteRange(ey, 0, ex + 1);
	}

	for (int i = sy; i < ey; i++)
		editorDelRow(sy);
}

static void
processKeyVisual(int fd, int key)
{
	int y = E.rowoff + E.cy;
	int sx, sy, ex, ey;
	getSelectedRange(&sx, &sy, &ex, &ey);

	switch (key) {
	case 'v':
	case ESC:
		exitVisualMode(sy, ey);
		break;
	case '$':
	case END_KEY:
		E.cx = E.row[E.cy + E.rowoff].size - 1;
		break;
	case '0':
	case HOME_KEY:
		E.cx = 0;
		break;
	case 'h':
	case ARROW_LEFT:
	case BACKSPACE:
		editorMoveCursor(ARROW_LEFT);
		break;
	case 'l':
	case ARROW_RIGHT:
		editorMoveCursor(ARROW_RIGHT);
		break;
	case 'j':
	case ARROW_DOWN:
	case ENTER:
		editorMoveCursor(ARROW_DOWN);
		editorUpdateRange(y, E.rowoff + E.cy);
		break;
	case 'k':
	case ARROW_UP:
		editorMoveCursor(ARROW_UP);
		editorUpdateRange(E.rowoff + E.cy, y);
		break;
	case 'g':
		key = editorReadKey(fd);
		if (key == 'g') {
			E.rowoff = 0;
			E.cx	 = 0;
			E.cy	 = 0;
			editorUpdateRange(0, y);
		}
		break;
	case 'G':
		E.rowoff	= E.numrows - 1;
		E.cy		= 0;
		do {
			E.rowoff--;
			E.cy++;
			editorRefreshScreen(false);
		} while (E.rowBottom + E.rowoff == E.numrows - 1);
		E.rowoff++;
		E.cy--;
		editorUpdateRange(y, E.numrows);
		break;
	case 'y':	/*	Yank	*/
		free(E.copyBuffer);
		E.copyBuffer = editorCopyRange(sx, sy, ex, ey);
		exitVisualMode(sy, ey);
		break;
	case 'x':
	case 'd':	/*	Cut	*/
		free(E.copyBuffer);

		editorStartChange(sy, ey);
		editorCommitChange(sy, sy + (sx == 0) +
				       (ex == E.row[ey].size - 1));

		E.copyBuffer = editorCopyRange(sx, sy, ex, ey);

		exitVisualMode(sy, ey);

		editorMoveCursorTo(sy, sx);
		visualCut(sx, sy, ex, ey);
		break;
	default:
		break;
	}
	editorUpdateRow(E.row + y);
	return;
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
void
editorProcessKeypress(int fd) {
	int key = editorReadKey(fd);
	if (E.mode == MODE_NORMAL)
		processKeyNormal(fd,key);
	else if (E.mode == MODE_INSERT)
		processKeyInsert(fd,key);
	else if (E.mode == MODE_VISUAL)
		processKeyVisual(fd,key);
	return;
}

int
editorFileWasModified(void) {
    return E.version;
}

void
updateWindowSize(void) {
    if (getWindowSize(STDIN_FILENO, STDOUT_FILENO,
                      &E.screenrows, &E.screencols) == -1) {
        perror("Unable to query the screen for size (columns / rows)");
        exit(1);
    }
    E.screenrows--;		// Get room for status line
}

void
handleSigWinCh(int unused)
{
	(void)unused;
	updateWindowSize();
	if (E.cy > E.screenrows)
		E.cy = E.screenrows - 1;
	if (E.cx > E.screencols)
		E.cx = E.screencols - 1;
	editorRefreshScreen(true);
}

void
initEditor(void)
{
	E.cx		= 0;
	E.cy		= 0;
	E.rowBottom	= 0;
	E.rowoff	= 0;
	E.mode		= MODE_NORMAL;
	E.isScreenFull	= false;

	E.numrows	= 0;
	E.row		= NULL;
	E.filename	= NULL;

	E.version	= 0;
	E.newest	= 0;
	E.oldest	= 0;
	E.history	= malloc(sizeof(Change) * C.historySize);
	memset(E.history, 0, sizeof(Change) * C.historySize);

	E.copyBuffer	= NULL;

	E.keyword	= NULL;
	E.lastMatchX	= -1;
	E.lastMatchY	= -1;

	E.keywords	= NULL;

	E.posStack	= malloc(sizeof(int) * C.positionStackSize);
	E.posTop	= 0;

	updateWindowSize();
	signal(SIGWINCH, handleSigWinCh);
}

static void
usage(const char *prog)
{
	fprintf(stderr, "Usage: %s <filename>\n", prog);
	return;
}

int
main(int argc, const char *argv[])
{
	if (argc != 2) {
		usage(argv[0]);
		return -1;
	}

	setlocale(LC_ALL, "");
	initEditor();
	editorOpen(argv[1]);

	/*	Make sure there is at last one row	*/
	if (!E.numrows) {
		editorInsertRow(0, L"", 0);
		E.version = 0;
	}

	enableRawMode();
	atexit(editorAtExit);
	while(1) {
		editorRefreshScreen(true);
		editorProcessKeypress(STDIN_FILENO);
	}

	return 0;
}
