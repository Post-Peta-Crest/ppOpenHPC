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
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "hecmw_util.h"
#include "hecmw_error.h"


static int hecmw_errno;


static char hecmw_errmsg[HECMW_MSG_LEN+1];


int
HECMW_set_verror(int errorno, const char *fmt, va_list ap)
{
	char errmsg[HECMW_MSG_LEN+1];

	hecmw_errno = errorno;

	HECMW_snprintf(hecmw_errmsg, sizeof(hecmw_errmsg), "%s", HECMW_strmsg(errorno));
	HECMW_vsnprintf(errmsg, sizeof(errmsg), fmt, ap);

	if(strlen(errmsg) > 0) {
		HECMW_snprintf(hecmw_errmsg+strlen(hecmw_errmsg), sizeof(hecmw_errmsg)-strlen(hecmw_errmsg), " (%s)", errmsg);
	}

	HECMW_print_error();

	return 0;
}


int
HECMW_set_error(int errorno, const char *fmt, ...)
{
	int rc;
	va_list ap;

	rc = 0;
	va_start(ap, fmt);
	rc = HECMW_set_verror(errorno, fmt, ap);
	va_end(ap);

	return rc;
}


int
HECMW_get_error(char **errmsg)
{
	if(errmsg) {
		*errmsg = hecmw_errmsg;
	}
	return hecmw_errno;
}


int
HECMW_get_errno(void)
{
	return hecmw_errno;
}


char *
HECMW_get_errmsg(void)
{
	return hecmw_errmsg;
}

