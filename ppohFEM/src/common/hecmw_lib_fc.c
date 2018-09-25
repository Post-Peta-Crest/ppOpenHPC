/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohFEM                                           *
 *         Version : 1.0                                               *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppohFEM.                                   *
 *     ppohFEM is a free software, you can use it under the terms      *
 *     of The MIT License (MIT). See LICENSE file and User's guide     *
 *     for more details.                                               *
 *                                                                     *
 *   ppOpen-HPC project:                                               *
 *     Open Source Infrastructure for Development and Execution of     *
 *     Large-Scale Scientific Applications on Post-Peta-Scale          *
 *     Supercomputers with Automatic Tuning (AT).                      *
 *                                                                     *
 *   Organizations:                                                    *
 *     The University of Tokyo                                         *
 *       - Information Technology Center                               *
 *       - Atmosphere and Ocean Research Institute (AORI)              *
 *       - Interfaculty Initiative in Information Studies              *
 *         /Earthquake Research Institute (ERI)                        *
 *       - Graduate School of Frontier Science                         *
 *     Kyoto University                                                *
 *       - Academic Center for Computing and Media Studies             *
 *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
 *                                                                     *
 *   Sponsorship:                                                      *
 *     Japan Science and Technology Agency (JST), Basic Research       *
 *     Programs: CREST, Development of System Software Technologies    *
 *     for post-Peta Scale High Performance Computing.                 *
 *                                                                     *
 *   Copyright (c) 2015 The University of Tokyo                        *
 *                       - Graduate School of Frontier Science         *
 *                                                                     *
 *=====================================================================*/




#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "hecmw_util.h"

char *
HECMW_strcpy_f2c(const char *fstr, int flen)
{
	int i,len;
	char *s;

	if(fstr == NULL) return NULL;
	if(flen <= 0) return NULL;

	len = 0;
	for(i=flen-1; i >= 0; i--) {
		if(fstr[i] != ' ') {
			len = i+1;
			break;
		}
	}

	if(len == 0) {
		s = HECMW_strdup("");
		if(s == NULL) {
			HECMW_set_error(errno, "");
			return NULL;
		}
		return s;
	}

	s = HECMW_malloc(len+1);
	if(s == NULL) {
		HECMW_set_error(errno, "");
		return NULL;
	}
	strncpy(s, fstr, len);
	s[len] = '\0';
	return s;
}

char *
HECMW_strcpy_f2c_r(const char *fstr, int flen, char *buf, int bufsize)
{
	int i,len;

	if(fstr == NULL) return NULL;
	if(flen <= 0) return NULL;
	if(buf == NULL) return NULL;
	if(bufsize <= 0) return NULL;

	len = 0;
	for(i=flen-1; i >= 0; i--) {
		if(fstr[i] != ' ') {
			len = i+1;
			break;
		}
	}
	if(len == 0) {
		buf[0] = '\0';
		return buf;
	}
	if(len > bufsize-1) {
		len = bufsize-1;
	}
	strncpy(buf, fstr, len);
	buf[len] = '\0';
	return buf;
}

int
HECMW_strcpy_c2f(const char *cstr, char *fstr, int flen)
{
	int clen;

	if(fstr == NULL) return 0;
	if(flen <= 0) return 0;

	if(cstr == NULL) {
		clen = 0;
	} else {
		clen = strlen(cstr);
	}
	if(clen > flen) {
		clen = flen;
	}
	memset(fstr, ' ', flen);
	strncpy(fstr, cstr, clen);
	return flen;
}

