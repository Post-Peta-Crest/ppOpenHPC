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
#include <ctype.h>

#include "ppohVIS_BASE_Lib.h"

/*******************************************************************************
 * convert space-splitting-string to comma-splitting-string
 ******************************************************************************/
extern char *
ppohVIS_BASE_StrSSV2CSV(char *s)
{
	int i, Flag;

	Flag = 0;
	for(i=0; i<strlen(s); i++) {
		if(s[i] == '\0') break;

		if(s[i] == ' ') {
			if(Flag) {
				s[i] = ',';
				Flag = 0;
			};
		} else {
			Flag = 1;
		};
	};

	return s;
};


/*******************************************************************************
 * change to lower case
 ******************************************************************************/
extern char *
ppohVIS_BASE_ToLower(char *s)
{
	int i;

	for(i=0; i<strlen(s); i++) {
		s[i] = tolower(s[i]);
	};

	return s;
};


/*******************************************************************************
 * create distributed file name
 ******************************************************************************/
extern char *
ppohVIS_BASE_GetDistFileName(char *cSrc, int iRank, char *cDst, int iDstLen)
{
#define CRANK_LENGTH 80
	char cRank[CRANK_LENGTH];
	int iSrcLen, iRankLen;

	memset(cRank, '\0', CRANK_LENGTH);
	sprintf(cRank, ".%d", iRank);

	iSrcLen = strlen(cSrc);
	iRankLen = strlen(cRank);

	memset(cDst, '\0', iDstLen);
	if(iSrcLen < iDstLen) {
		strncpy(cDst, cSrc, iSrcLen);
		if(iSrcLen+iRankLen < iDstLen) {
			strncat(cDst, cRank, iRankLen);
		} else {
			strncat(cDst, cRank, iDstLen-(iSrcLen+iRankLen)-1);
		};
	} else {
		strncpy(cDst, cSrc, iDstLen-1);
	};

	return cDst;
};

