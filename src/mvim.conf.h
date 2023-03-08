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
} C = {
	.tabsize		= 8,
	.outputBufferSize	= 64 * 1024,
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
};

#endif	// __MVIM_CONF_H_INC__
