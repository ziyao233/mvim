/*
 *	mvim
 *	Default configuration for mVim
 */

#ifndef __MVIM_CONF_H_INC__
#define __MVIM_CONF_H_INC__

#include<stdbool.h>
#include<stdint.h>

#define ENTRY(name) { #name, &(C.name) }

struct Mvim_Conf {
	int tabsize;
	int outputBufferSize;
	int historySize;
	int highlightTrailingSpace;
	int highlightKeywordColor;
	int autoIndent;
	int positionStackSize;
} C = {
	.tabsize		= 8,
	.outputBufferSize	= 64 * 1024,
	.historySize		= 64,
	.highlightTrailingSpace	= 1,
	.highlightKeywordColor	= 2,
	.autoIndent		= 1,
	.positionStackSize	= 16,
};

/*
 *	DO NOT MODIFY
 */
typedef struct {
	const char *name;
	int *value;
} Mvim_Conf_Entry;
Mvim_Conf_Entry gConfList[] = {
	ENTRY(tabsize),
	ENTRY(highlightTrailingSpace),
	ENTRY(highlightKeywordColor),
	ENTRY(autoIndent),
};

#undef ENTRY

#endif	// __MVIM_CONF_H_INC__
