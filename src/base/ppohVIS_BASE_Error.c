/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_base                                      *
 *         Version : 0.2.0                                             *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppohVIS_FDM3D                              *
 *     ppohVIS_FDM3D is a free software, you can use it under the      *
 *     termas of The MIT License (MIT). See LICENSE file and User's    *
 *     guide for more details.                                         *
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
 *       - Graduate School of Interdisciplinary Information Studies    *
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
 *   Copyright (c) 2013 <Kengo Nakajima, The University of Tokyo       *
 *                       nakajima(at)cc.u-tokyo.ac.jp           >      *
 *=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#include "ppohVIS_BASE_Error.h"

static int ppohVIS_BASE_errno;
static char ppohVIS_BASE_errmsg[PPOHVIS_BASE_ERRMSG_LEN+1];

extern char *
ppohVIS_BASE_GetErrmsg(int ErrorCode)
{
	int ErrorCodeBase;
	ErrorCodeBase = ppohVIS_BASE_ErrorCode_NormalEnd;

	if(ErrorCode < ErrorCodeBase) {
		return strerror(errno);
	} else if(ErrorCode >= ppohVIS_BASE_ErrorCode_Error) {
		return ppohVIS_BASE_ErrorMsg[
		           ppohVIS_BASE_ErrorCode_Error-ErrorCodeBase];
	} else {
		return ppohVIS_BASE_ErrorMsg[ErrorCode-ErrorCodeBase-1];
	};
};


extern int
ppohVIS_BASE_SetErrorv(int ErrorCode, const char *fmt, va_list ap)
{
	char error_msg[PPOHVIS_BASE_ERRMSG_LEN+1];

        ppohVIS_BASE_errno = ErrorCode;

	snprintf(ppohVIS_BASE_errmsg, sizeof(ppohVIS_BASE_errmsg),
	         "%s", ppohVIS_BASE_GetErrmsg(ErrorCode));
	vsnprintf(error_msg, sizeof(error_msg), fmt, ap);

	if(strlen(error_msg) > 0) {
		snprintf(ppohVIS_BASE_errmsg+strlen(ppohVIS_BASE_errmsg),
		         sizeof(ppohVIS_BASE_errmsg)-strlen(error_msg),
		         " (%s)", error_msg);
	};

	return 0;
};


extern int
ppohVIS_BASE_SetError(int ErrorCode, const char *fmt, ...)
{
	int rc;
	va_list ap;

	rc = ppohVIS_BASE_ErrorCode_NormalEnd;

	va_start(ap, fmt);
	rc = ppohVIS_BASE_SetErrorv(ErrorCode, fmt, ap);
	va_end(ap);

	return rc;
};


extern int
ppohVIS_BASE_GetError(char **error_msg)
{
	if(error_msg != NULL) {
		*error_msg = ppohVIS_BASE_errmsg;
	};

	return ppohVIS_BASE_errno;
};


extern void
ppohVIS_BASE_PrintError(FILE *fp)
{
	char *error_msg;
	int error_no;

	error_no = ppohVIS_BASE_GetError(&error_msg);

	fprintf(fp, "ppohVIS_Error [%d]: %s\n", error_no, error_msg);
};
