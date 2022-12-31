/* -----------------------------------------------------------------------
 *
 * Copyright (c) 2022 Ziyao.
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

#define MVIM_STDOUT_BUFFER_SIZE		(64 * 1024)

#ifdef __linux__
#define _POSIX_C_SOURCE 200809L
#endif
#define _XOPEN_SOURCE

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
#include<unistd.h>
#include<fcntl.h>
#include<signal.h>

/* This structure represents a single line of the file we are editing. */
typedef struct erow {
	int idx;		// Row index in the file, zero-based.
	int size;		// Size of the row, excluding the null term.
	wchar_t *chars;		// Row content.
} erow;

static struct editorConfig {
	int cx,cy;		// Cursor x and y position on screen
	int rowoff;		// Offset of row displayed.
	int screenrows;		// Number of rows that we can show
	int screencols;		// Number of cols that we can show
	int numrows;		// Number of rows
	int rowBottom;		// Index of the row in the bottom
	int rawmode;		// Is terminal raw mode enabled?
	erow *row;		// Rows
	int dirty;		// File modified but not saved.
	char *filename;		// Currently open filename
	enum {
		MODE_NORMAL,MODE_INSERT,MODE_VISUAL
	} mode;
	bool isScreenFull;	// The screen is fully used (no '~')
} E;

static struct editorConfig E;

enum KEY_ACTION {
	KEY_NULL	= 0,
	CTRL_B		= 2,
	CTRL_C		= 3,
	CTRL_D		= 4,
	CTRL_F		= 6,
	CTRL_H		= 8,
	TAB		= 9,
	CTRL_L		= 12,
	ENTER		= 13,
	CTRL_Q		= 17,
	CTRL_S		= 19,
	CTRL_U		= 21,
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

void editorSetStatusMessage(const char *fmt, ...);

/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/

void disableRawMode(int fd) {
	/* Don't even check the return value as it's too late. */
	if (E.rawmode) {
		tcsetattr(fd,TCSAFLUSH,&orig_termios);
		E.rawmode = 0;
	}
}

/* Called at exit to avoid remaining in raw mode. */
void editorAtExit(void)
{
	disableRawMode(STDIN_FILENO);

	for (int i = 0;i < E.numrows;i++)
		free(E.row[i].chars);
	free(E.row);
	free(E.filename);

	/*	Reset cursor position	*/
	printf("\x1b[%d;0H\x1b[0K",E.screenrows + 1);
	return;
}

/* Raw mode: 1960 magic shit. */
int enableRawMode(int fd) {
	struct termios raw;

	if (E.rawmode)
		return 0;
	if (!isatty(STDIN_FILENO))
		goto fatal;

	if (tcgetattr(fd,&orig_termios) == -1)
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
	raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */

    /* put terminal in raw mode after flushing */
	if (tcsetattr(fd,TCSAFLUSH,&raw) < 0)
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
int enableStdoutBuffer(void)
{
	char *buf = malloc(MVIM_STDOUT_BUFFER_SIZE);
	if (!buf)
		return -1;

	setvbuf(stdout,buf,_IOFBF,MVIM_STDOUT_BUFFER_SIZE);
	return 0;
}

/*	puts() writes an extra '\n', fputs() does not	*/
void writeString(const char *s)
{
	fputs(s,stdout);
	return;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int editorReadKey(int fd) {
    int nread;
    char c, seq[3];
    while ((nread = read(fd,&c,1)) == 0);
    if (nread == -1) exit(1);

    while(1) {
        switch(c) {
        case ESC:    /* escape sequence */
            /* If this is just an ESC, we'll timeout here. */
            if (read(fd,seq,1) == 0) return ESC;
            if (read(fd,seq+1,1) == 0) return ESC;

            /* ESC [ sequences. */
            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape, read additional byte. */
                    if (read(fd,seq+2,1) == 0) return ESC;
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
int getCursorPosition(int ifd, int ofd, int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    /* Report cursor location */
    if (write(ofd, "\x1b[6n", 4) != 4) return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < sizeof(buf)-1) {
        if (read(ifd,buf+i,1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[') return -1;
    if (sscanf(buf+2,"%d;%d",rows,cols) != 2) return -1;
    return 0;
}

/* Try to get the number of columns in the current terminal. If the ioctl()
 * call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error. */
int getWindowSize(int ifd, int ofd, int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        /* ioctl() failed. Try to query the terminal itself. */
        int orig_row, orig_col, retval;

        /* Get the initial position so we can restore it later. */
        retval = getCursorPosition(ifd,ofd,&orig_row,&orig_col);
        if (retval == -1) goto failed;

        /* Go to right/bottom margin and get position. */
        if (write(ofd,"\x1b[999C\x1b[999B",12) != 12) goto failed;
        retval = getCursorPosition(ifd,ofd,rows,cols);
        if (retval == -1) goto failed;

        /* Restore position. */
        char seq[32];
        snprintf(seq,32,"\x1b[%d;%dH",orig_row,orig_col);
        if (write(ofd,seq,strlen(seq)) == -1) {
            /* Can't recover... */
        }
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

/*
 *	Empty now, later would be used to render color attributes
 */
void editorUpdateRow(erow *row)
{
	(void)row;
	return;
}

/* Insert a row at the specified position, shifting the other rows on the bottom
 * if required. */
void editorInsertRow(int at,const wchar_t *s,size_t len)
{
	if (at > E.numrows)
	    return;
	E.row = realloc(E.row,sizeof(erow) * (E.numrows + 1));
	if (at != E.numrows) {
		memmove(E.row + at + 1,E.row + at,
			sizeof(E.row[0]) * (E.numrows - at));
		for (int j = at + 1; j <= E.numrows; j++)
			E.row[j].idx++;
	}

	E.row[at].size	= len;
	E.row[at].chars	= malloc(sizeof(wchar_t) * (len + 1));
	wcsncpy(E.row[at].chars,s,len);
	E.row[at].chars[len]	= L'\0';
	E.row[at].idx		= at;
	editorUpdateRow(E.row + at);
	E.numrows++;
	E.dirty++;
}

void editorInsertRowMb(int at,const char *mbs)
{
	size_t charNum = mbstowcs(NULL,mbs,0);
	if (charNum == (size_t)(-1))
		perror("Invalid multibyte text");
	wchar_t *s = malloc(sizeof(wchar_t) * (charNum + 1));
	mbstowcs(s,mbs,charNum + 1);
	editorInsertRow(at,s,charNum);
	free(s);
}

/* Free row's heap allocated stuff. */
void editorFreeRow(erow *row)
{
	free(row->chars);
	return;
}

/*
 * Remove the row at the specified position, shifting the remainign on the
 * top
 */
void editorDelRow(int at)
{
	if (at >= E.numrows)
		return;
	erow *row = E.row + at;
	editorFreeRow(row);
	memmove(E.row + at,E.row + at + 1,
		sizeof(E.row[0]) * (E.numrows - at - 1));

	for (int j = at; j < E.numrows - 1; j++)
		E.row[j].idx++;
	E.numrows--;
	E.dirty++;
	return;
}

/* Turn the editor rows into a single heap-allocated string.
 * Returns the pointer to the heap-allocated string and populate the
 * integer pointed by 'buflen' with the size of the string, escluding
 * the final nulterm. */
char *editorRowsToString(int *buflen)
{
	char *buf = NULL,*p;
	int totlen = 0;

    /* Compute count of bytes */
	for (int j = 0; j < E.numrows; j++)
		totlen += wcstombs(NULL,E.row[j].chars,0);
	*buflen = totlen;
	totlen++;			// '\0'

	p = buf = malloc(totlen);
	for (int j = 0; j < E.numrows; j++) {
		p += wcstombs(p,E.row[j].chars,totlen);
		*p = '\n';
		p++;
	}
	*p = '\0';
	return buf;
}

/* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed. */
void editorRowInsertChar(erow *row, int at, int c)
{
	if (at > row->size) {
/* Pad the string with spaces if the insert location is outside the
 * current length by more than a single character. */
	        int padlen = at - row->size;
/* In the next line +2 means: new char and null term. */
		row->chars = realloc(row->chars,sizeof(wchar_t) *
						(row->size + padlen + 2));
		for (int i = 0;i < padlen;i++)
			row->chars[row->size + i] = L' ';
		row->chars[row->size + padlen + 1] = '\0';
		row->size += padlen + 1;
	} else {
        /* If we are in the middle of the string just make space for 1 new
         * char plus the (already existing) null term. */
		row->chars = realloc(row->chars,sizeof(wchar_t) *
						(row->size + 2));
		memmove(row->chars + at + 1,row->chars + at,
			sizeof(wchar_t) * (row->size - at + 1));
		row->size++;
	}
	row->chars[at] = c;
	editorUpdateRow(row);
	E.dirty++;
	return;
}

/* Append the string 's' at the end of a row */
void editorRowAppendString(erow *row,wchar_t *s, size_t len)
{
	row->chars = realloc(row->chars,sizeof(wchar_t) *
					(row->size + len + 1));
	wcsncat(row->chars,s,len);
	row->size += len;
	editorUpdateRow(row);
	E.dirty++;
	return;
}

/* Delete the character at offset 'at' from the specified row. */
void editorRowDelChar(erow *row, int at)
{
	if (row->size <= at)
		return;
	memmove(row->chars + at,row->chars + at + 1,sizeof(wchar_t) *
						    (row->size - at));
	row->size--;
	editorUpdateRow(row);
	E.dirty++;
	return;
}

/* Insert the specified char at the current prompt position. */
void editorInsertChar(int c)
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

    /* If the row where the cursor is currently located does not exist in our
     * logical representaion of the file, add enough empty rows as needed. */
	if (!row) {
		while(E.numrows <= filerow)
			editorInsertRow(E.numrows,L"",0);
	}
	row = &E.row[filerow];
	editorRowInsertChar(row,E.cx,c);
	E.cx++;
	E.dirty++;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed. */
void editorInsertNewline(void)
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

	if (!row) {
		if (filerow == E.numrows) {
			editorInsertRow(filerow,L"",0);
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
		editorInsertRow(filerow,L"",0);
	} else {
	/* We are in the middle of a line. Split it between two rows. */
		editorInsertRow(filerow + 1,row->chars + E.cx,row->size - E.cx);
		row = &E.row[filerow];
		row->chars[E.cx] = L'\0';
		row->size = E.cx;
		editorUpdateRow(row);
	}

fixcursor:
	if (E.cy == E.rowBottom && E.isScreenFull) {
		E.rowoff++;
	else
		E.cy++;

	E.cx = 0;
	return;
}

/* Delete the char at the current prompt position. */
void editorDelChar()
{
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

	if (!row || (E.cx == 0 && filerow == 0))
		return;

	if (!E.cx) {
        /* Handle the case of column 0, we need to move the current line
         * on the right of the previous one. */
		int newX = E.cx + E.row[filerow - 1].size;
		editorRowAppendString(&E.row[filerow - 1],row->chars,row->size);
		editorDelRow(filerow);
		row = NULL;
		if (E.cy == 0) {
			E.rowoff--;
		} else {
			E.cy--;
		}
		E.cx = newX;
	} else {
		editorRowDelChar(row,E.cx - 1);
		E.cx--;
	}

	if (row)
		editorUpdateRow(row);

	E.dirty++;

	return;
}

/* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error. */
int editorOpen(char *filename)
{
	FILE *fp;

	E.dirty = 0;
	free(E.filename);
	size_t fnlen = strlen(filename) + 1;
	E.filename = malloc(fnlen);
	memcpy(E.filename,filename,fnlen);

	fp = fopen(filename,"r");
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
	while((linelen = getline(&line,&linecap,fp)) != -1) {
		if (linelen && (line[linelen - 1] == '\n' ||
		    line[linelen - 1] == '\r'))
			line[--linelen] = '\0';
		editorInsertRowMb(E.numrows,line);
	}
	free(line);
	fclose(fp);
	E.dirty = 0;
	return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editorSave(void) {
	int len;
	char *buf = editorRowsToString(&len);
	int fd = open(E.filename,O_RDWR|O_CREAT,0644);
	if (fd == -1)
		goto writeerr;

    /* Use truncate + a single write(2) call in order to make saving
     * a bit safer, under the limits of what we can do in a small editor. */
	if (ftruncate(fd,len) == -1)
		goto writeerr;
	if (write(fd,buf,len) != len)
		goto writeerr;

	close(fd);
	free(buf);
	E.dirty = 0;
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
int editorWidthFrom(int start)
{
	int width = 0;
	erow *row = E.row + E.rowoff + E.cy;
	for (int i = start;i - start < E.cx;i++) {
		int t = wcwidth(row->chars[i]);
		width += t >= 0			? t			:
			 row->chars[i] == TAB	? 8 - width % 8		:
						  1;
	}
	return width;
}

/* ============================= Terminal update ============================ */

static inline int drawRowAt(int at,int remainSpace,bool write)
{
	erow *row = E.row + at;
	int line = 0;

	for (int i = 0,width = 0;i < row->size;i++) {
		int t = wcwidth(row->chars[i]);
		// Normal characters, TAB or control charaters
		t = t >= 0			? t			:
		    row->chars[i] == TAB	? 8 - width % 8	:
						  1;

		/*	Wrapping	*/
		if (width + t > E.screencols) {
			width = 0;
			if (write)
				writeString("\x1b[0K\r\n");
			line++;
			if (line > remainSpace)
				break;
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
				s[wctomb(s,row->chars[i])] = '\0';
				writeString(s);
			}
		}
		width += t;
	}

	return line + 1;
}

/* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'. */
void editorRefreshScreen(bool write)
{
	writeString("\x1b[?25l");	// Hide cursor.
	writeString("\x1b[H");		// Go home.

	int printedLine = 0,y = 0,cursorY = 0;
	E.isScreenFull = true;
	for (int i = 0;y < E.screenrows;y += printedLine,i++) {
		int filerow = E.rowoff + i;

		if (filerow >= E.numrows) {
			E.isScreenFull = false;
			if (!write)
				continue;
			if (!E.numrows && printedLine == E.screenrows / 2) {
				char welcome[80];
				int wellen = snprintf(welcome,sizeof(welcome),
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

		printedLine = drawRowAt(filerow,E.screenrows - y,write);

		if (i == E.cy)
			cursorY = y;
		if (filerow < E.numrows)
			E.rowBottom = i;

		if (write) {
			writeString("\x1b[39m");
			writeString("\x1b[0K");
			writeString("\r\n");
		}
	}

	if (!write)
		return;

	/* Create a one row status. */
	char status[80],rstatus[80];
	int len = snprintf(status,sizeof(status),"%s",
			   E.mode == MODE_INSERT ? "-- INSERT --" :
			   E.mode == MODE_VISUAL ? "-- VISUAL --" :"");
	int rlen = snprintf(rstatus,sizeof(rstatus),"%d,%d    %d%%",
			    E.rowoff + E.cy + 1,E.cx + 1,
			    E.numrows ? E.rowoff * 100 / E.numrows : 100);
	if (len > E.screencols)
		len = E.screencols;

	fwrite(status,1,len,stdout);
	while(len < E.screencols) {
		if (E.screencols - len == rlen) {
			writeString(rstatus);
			break;
		} else {
			putchar(' ');
			len++;
		}
	}

    /*
     * Put cursor at its current position. Note that the horizontal position
     * at which the cursor is displayed may be different compared to 'E.cx'
     * because of TABs and wide characters
     */
	int cx = 0;
	int filerow = E.rowoff + E.cy;
	erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
	if (row) {
		for (int i = 0;i < E.cx && i < row->size;i++) {
			int width = wcwidth(row->chars[i]);
			width = width >= 0		? width		:
			        row->chars[i] == TAB	? 8 - cx % 8	:
							  1;
			cursorY += cx + width < E.screencols ? 0 : 1;
			cx = cx + width < E.screencols	? cx + width	:
			     row->chars[i] == TAB	? cx + width -
							  E.screencols	:
							  0;
		}
	}


	printf("\x1b[%d;%dH",cursorY + 1,cx + 1);
	writeString("\x1b[?25h");		// Show cursor
	fflush(stdout);				// stdout is block-buffered
	return;
}

/* ========================= Editor events handling  ======================== */

void editorMoveCursor(int key)
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
		width += t >= 0				? t		:
			 row->chars[cx] == TAB		? 8 - width % 8	:
							  1;
		cx++;
	}
	E.cx = cx > 0 ? cx - 1 : 0;

	return;
}

void editorReplaceChar(int y,int x,int new)
{
	if (E.row[y].size)
		E.row[y].chars[x] = new;
	E.dirty++;
	editorUpdateRow(E.row + y);
	return;
}

static inline void commandModeError(int fd,const char *s)
{
	enableRawMode(fd);
	writeString(s);
	fflush(stdout);
	editorReadKey(fd);
	return;
}

static inline void commandMode(int fd)
{
	disableRawMode(fd);

	// Overwrite the status line
	printf("\x1b[%d;%dH\x1b[0K:",E.screenrows + 1,0);
	fflush(stdout);			// stdout is block-buffered

	char *cmd = NULL;
	size_t size = 0;
	ssize_t length = getline(&cmd,&size,stdin);
	if (length < 0)
		goto end;
	cmd[length - 1] = '\0';

	if (!strcmp(cmd,"q")) {
		if (E.dirty) {
			commandModeError(fd,"No write since last change");
		} else {
			exit(0);
		}
	} else if (!strcmp(cmd,"q!")) {
		exit(0);
	} else if (!strcmp(cmd,"w")) {
		if (E.dirty) {
			if (editorSave())
				commandModeError(fd,"Cannot save file");
		}
	} else if (!strcmp(cmd,"wq")) {
		if (E.dirty) {
			if (editorSave()) {
				commandModeError(fd,"Cannot save file");
				goto end;
			}
		}
		exit(0);
	} else if (!*cmd) {
		goto end;
	} else {
		commandModeError(fd,"Unknown command");
	}

end:
	free(cmd);

	enableRawMode(fd);
	return;
}

static wchar_t readWideChar(int startByte)
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
	read(STDIN_FILENO,tmp + 1,length - 1);
	wchar_t wideChar = 0;
	if (mbtowc(&wideChar,tmp,length) < 0)
		return L' ';
	return wideChar;
}

static inline void deleteRange(int y,int x,int length)
{
	for (int i = 0;i < length;i++)
		editorRowDelChar(E.row + y,x);
	return;
}

static inline void processKeyNormal(int fd,int key)
{
	int y = E.cy + E.rowoff;
	switch (key) {
	case 'd':
		key = editorReadKey(fd);
		if (key == 'd') {
			editorDelRow(y);
			if (!E.numrows)
				editorInsertRow(0,L"",0);
		} else if (key == '$') {
			deleteRange(y,E.cx,E.row[y].size - E.cx);
		} else if (key == '0') {
			deleteRange(y,0,E.cx);
			E.cx = 0;
		}
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
		editorInsertRow(y + 1,L"",0);
		E.mode = MODE_INSERT;
		editorRefreshScreen(false);
		editorMoveCursor(ARROW_DOWN);
		break;
	case 'a':
		E.mode = MODE_INSERT;
		editorMoveCursor(ARROW_RIGHT);
		break;
	case 'i':
		E.mode = MODE_INSERT;
		break;
	case 'v':
		E.mode = MODE_VISUAL;
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
		E.rowoff	= E.numrows - 1;
		E.cy		= 0;
		do {
			E.rowoff--;
			E.cy++;
			editorRefreshScreen(false);
		} while (E.rowBottom + E.rowoff == E.numrows - 1);
		E.rowoff++;
		E.cy--;
		break;
	case 'x':
		if (E.row[y].size)
			editorRowDelChar(E.row + y,E.cx);
		break;
	case 'r':
		key = editorReadKey(fd);
		editorReplaceChar(y,E.cx,readWideChar(key));
		break;
	case ':':
		commandMode(fd);
		break;
	default:
		break;
	}
	return;
}

static inline void processKeyInsert(int key)
{
	switch (key) {
	case ESC:
		editorMoveCursor(ARROW_LEFT);
		E.mode = MODE_NORMAL;
		break;
	case ENTER:
		editorInsertNewline();
		break;
	case BACKSPACE:
		editorDelChar();
		break;
	case ARROW_LEFT:
		editorMoveCursor(ARROW_LEFT);
		break;
	case ARROW_RIGHT:
		editorMoveCursor(ARROW_RIGHT);
		break;
	case ARROW_UP:
		editorMoveCursor(ARROW_UP);
		break;
	case ARROW_DOWN:
		editorMoveCursor(ARROW_DOWN);
		break;
	default:
		editorInsertChar(readWideChar(key));
		break;
	}
	return;
}

static inline void processKeyVisual(int key)
{
	switch (key) {
	case ESC:
		E.mode = MODE_NORMAL;
		break;
	default:
		break;
	}
	return;
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
void editorProcessKeypress(int fd) {
	int key = editorReadKey(fd);
	switch (E.mode) {
	if (E.mode == MODE_NORMAL)
		processKeyNormal(fd,key);
	else if (E.mode == MODE_INSERT)
		processKeyInsert(key);
	else if (E.mode == MODE_VISUAL)
		processKeyVisual(key);
	return;
}

int editorFileWasModified(void) {
    return E.dirty;
}

void updateWindowSize(void) {
    if (getWindowSize(STDIN_FILENO,STDOUT_FILENO,
                      &E.screenrows,&E.screencols) == -1) {
        perror("Unable to query the screen for size (columns / rows)");
        exit(1);
    }
    E.screenrows --;		// Get room for status line
}

void handleSigWinCh(int unused)
{
	(void)unused;
	updateWindowSize();
	if (E.cy > E.screenrows)
		E.cy = E.screenrows - 1;
	if (E.cx > E.screencols)
		E.cx = E.screencols - 1;
	editorRefreshScreen(true);
}

void initEditor(void)
{
	E.cx		= 0;
	E.cy		= 0;
	E.rowBottom	= 0;
	E.rowoff	= 0;
	E.numrows	= 0;
	E.row		= NULL;
	E.dirty		= 0;
	E.filename	= NULL;
	E.mode		= MODE_NORMAL;
	E.isScreenFull	= false;
	updateWindowSize();
	signal(SIGWINCH, handleSigWinCh);
}

int main(int argc, char **argv)
{
	if (argc != 2) {
		fprintf(stderr,"Usage: mvim <filename>\n");
		exit(1);
	}

	setlocale(LC_ALL,"");
	initEditor();
	editorOpen(argv[1]);

	/*	Make sure there is at last one row	*/
	if (!E.numrows)
		editorInsertRow(0,L"",0);

	enableRawMode(STDIN_FILENO);
	atexit(editorAtExit);
	while(1) {
		editorRefreshScreen(true);
		editorProcessKeypress(STDIN_FILENO);
	}

	return 0;
}
