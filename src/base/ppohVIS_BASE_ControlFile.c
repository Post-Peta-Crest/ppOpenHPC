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
#include <errno.h>
#include <ctype.h>

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_ControlFile.h"


/*******************************************************************************
 *
 ******************************************************************************/
static int
ScanRefineControl(
	struct ppohVIS_BASE_stControl *pControl, char *cKey, char *cVal)
{
	double dVal;
	int iVal;
	int iRead;

	if(strncmp(cKey, "availablememory", 15) == 0) {
		iRead = sscanf(cVal, "%le", &dVal);
		if(iRead == 1) {
			pControl->Refine.AvailableMemory = dVal;
		};

	} else if(strncmp(cKey, "maxvoxelcount", 13) == 0) {
		iRead = sscanf(cVal, "%d", &iVal);
		if(iRead == 1) {
			pControl->Refine.MaxVoxelCount = iVal;
		};

	} else if(strncmp(cKey, "maxrefinelevel", 14) == 0) {
		iRead = sscanf(cVal, "%d", &iVal);
		if(iRead == 1) {
			pControl->Refine.MaxRefineLevel = iVal;
		};

	};

	return 0;
};


static int
ScanSimpleControl(
	struct ppohVIS_BASE_stControl *pControl, char *cKey, char *cVal)
{
	double dVal;
	int iRead;

	if(strncmp(cKey, "reductionrate", 13) == 0) {
		iRead = sscanf(cVal, "%le", &dVal);
		if(iRead == 1) {
			pControl->Simple.ReductionRate = dVal;
		};

	};

	return 0;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stControl *
ppohVIS_BASE_ScanControl(FILE *fp)
{
	struct ppohVIS_BASE_stControl *pControl = NULL;
	char cLine[PPOHVIS_BASE_BUFFER_LEN];
	char cBuf[PPOHVIS_BASE_BUFFER_LEN];
	char cKey[PPOHVIS_BASE_BUFFER_LEN];
	char cVal[PPOHVIS_BASE_BUFFER_LEN];
	int bRefine, bSimple;
	int i, iS, iE;

	pControl = ppohVIS_BASE_AllocateControl();
	if(pControl == NULL) {
		goto Error;
	};

	bRefine = 0;
	bSimple = 0;

	while(fgets(cLine, PPOHVIS_BASE_BUFFER_LEN, fp) != NULL) {
		memset(cBuf, '\0', PPOHVIS_BASE_BUFFER_LEN);
		for(i=0; i<PPOHVIS_BASE_BUFFER_LEN; i++) {
			if(cLine[i] != ' ') break;
		};
		iS = i;
		for(i=iS; i<PPOHVIS_BASE_BUFFER_LEN; i++) {
			cBuf[i-iS] = cLine[i];
		};

		memset(cKey, '\0', PPOHVIS_BASE_BUFFER_LEN);
		memset(cVal, '\0', PPOHVIS_BASE_BUFFER_LEN);
		if(cLine[0] == '[') {   /**************************** Section */
			for(i=0; i<PPOHVIS_BASE_BUFFER_LEN; i++) {
				if(cLine[i] == ']') {
					strncpy(cKey, cLine+1, i-1);
					break;
				};
			};
			if(strlen(cKey) <= 0) continue;

			ppohVIS_BASE_ToLower(cKey);

			if(strncmp(cKey, "refine", 6) == 0) {
				bRefine = 1;
				bSimple = 0;
			} else if(strncmp(cKey, "simple", 6) == 0) {
				bRefine = 0;
				bSimple = 1;
			};

		} else {   /************************************* Key & Value */
			iE = -1;
			for(i=0; i<PPOHVIS_BASE_BUFFER_LEN; i++) {
				if(cLine[i] == '=') {
					iE = i;
					break;
				};
			};
			if(iE < 0) continue;

			for(i=iE-1; i>=0; i--) {
				if(cLine[i] != ' ') {
					strncpy(cKey, cLine, i+1);
					break;
				};
			};
			if(strlen(cKey) <= 0) continue;

			iS = iE+1;
			for(i=iE+1; i<PPOHVIS_BASE_BUFFER_LEN; i++) {
				if(cLine[i] != ' ') {
					iS = i;
					break;
				};
			};
			for(i=PPOHVIS_BASE_BUFFER_LEN-1; i>iS; i--) {
				if(cLine[i] != '\0') {
					strncpy(cVal, cLine+iS, i-iS+1);
					break;
				};
			};

			ppohVIS_BASE_ToLower(cKey);
			ppohVIS_BASE_ToLower(cVal);

			if(bRefine) {
				if(ScanRefineControl(pControl, cKey, cVal)) {
					goto Error;
				};
			} else if(bSimple) {
				if(ScanSimpleControl(pControl, cKey, cVal)) {
					goto Error;
				};
			};
		};
	};

	return pControl;

Error:
	ppohVIS_BASE_FreeControl(pControl);
	return NULL;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stControl *
ppohVIS_BASE_GetControl(char *cFileName)
{
	struct ppohVIS_BASE_stControl *pControl = NULL;
	FILE *fp = NULL;

	if((fp = fopen(cFileName, "r")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError, "%s [%s]",
			strerror(errno), "control file");
		goto Error;
	};

	pControl = ppohVIS_BASE_ScanControl(fp);
	if(pControl == NULL) goto Error;

	if(fclose(fp)) goto Error;

	return pControl;

Error:
	if(fp) fclose(fp);
	return NULL;
};

