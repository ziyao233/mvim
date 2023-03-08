/*
 *	mvim
 *	Default configuration for mVim
 */

#ifndef __MVIM_CONF_H_INC__
#define __MVIM_CONF_H_INC__

#include<stdbool.h>
#include<stdint.h>

struct Mvim_Conf {
	int tabsize;
} C = {
	.tabsize	= 8,
};



#endif	// __MVIM_CONF_H_INC__
