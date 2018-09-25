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

#include "ppohVIS_BASE_F2CUtil.h"


/*******************************************************************************
 * copy string allocated by 'FORTRAN' to 'C' region
 ******************************************************************************/
extern char *
ppohVIS_BASE_StrCpyF2C(
	const char *cFStr, int iFStrLen, char *cCBuf, int iCBufLen)
{
	int i, iLen;

	if(cFStr == NULL) return NULL;
	if(cCBuf == NULL) return NULL;
	if(iFStrLen <= 0) return NULL;
	if(iCBufLen <= 0) return NULL;

	iLen = 0;
	for(i=iFStrLen-1; i>=0; i--) {
		if(cFStr[i] != ' ') {
			iLen = i+1;
			break;
		};
	};

	if(iLen == 0) {
		cCBuf[0] = '\0';
		return cCBuf;
	};
	if(iLen >= iCBufLen) {
		iLen = iCBufLen-1;
	};

	strncpy(cCBuf, cFStr, iLen);
	cCBuf[iLen] = '\0';

	return cCBuf;
};
