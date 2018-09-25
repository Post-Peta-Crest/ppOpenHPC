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

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Result.h"


/*******************************************************************************
 * Allocate
 ******************************************************************************/
extern struct ppohVIS_BASE_stResult *
ppohVIS_BASE_AllocateResult(void)
{
	struct ppohVIS_BASE_stResult *pResult = NULL;
	size_t BufSize;

	/* allocate */
	BufSize = sizeof(struct ppohVIS_BASE_stResult);
	pResult = (struct ppohVIS_BASE_stResult *)malloc(BufSize);
	if(pResult == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "result info");
		goto Error;
	};

	/* initialize */
	pResult->ItemCount = 0;
	pResult->EntityType = ppohVIS_BASE_ResultNode;
	pResult->FreedomCount = 0;
	memset(pResult->Label, '\0', PPOHVIS_BASE_LABEL_LEN);
	pResult->Value = NULL;

	return pResult;

Error:
	ppohVIS_BASE_FreeResult(pResult);
	return NULL;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stResultCollection *
ppohVIS_BASE_AllocateResultCollection(void)
{
	struct ppohVIS_BASE_stResultCollection *pResults = NULL;
	size_t BufSize;

	/* allocate */
	BufSize = sizeof(struct ppohVIS_BASE_stResultCollection);
	pResults = (struct ppohVIS_BASE_stResultCollection *)malloc(BufSize);
	if(pResults == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "result collection info");
		goto Error;
	};

	/* initialize */
	pResults->ListCount = 0;
	pResults->Results = NULL;

	return pResults;

Error:
	ppohVIS_BASE_FreeResultCollection(pResults);
	return NULL;
};


/*******************************************************************************
 * Initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_InitResult(
	struct ppohVIS_BASE_stResult *pResult, int ItemCount,
	enum ppohVIS_BASE_eResultEntityType Type, int FreedomCount)
{
	int BufCount, i;
	size_t BufSize;

	pResult->ItemCount = ItemCount;
	pResult->EntityType = Type;
	pResult->FreedomCount = FreedomCount;

	memset(pResult->Label, '\0', PPOHVIS_BASE_LABEL_LEN);

	if(ItemCount > 0 && FreedomCount > 0) {
		BufCount = ItemCount * FreedomCount;
		BufSize = sizeof(double) * BufCount;
		pResult->Value = (double *)malloc(BufSize);
		if(pResult->Value == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "result value");
			goto Error;
		};

		for(i=0; i<BufCount; i++) {
			pResult->Value[i] = 0.0;
		};
	};

	return 0;

Error:
	return -1;
};


/*============================================================================*/
extern int
ppohVIS_BASE_InitResultCollection(
	struct ppohVIS_BASE_stResultCollection *pResults, int Count)
{
	int i;
	size_t BufSize;

	if(pResults == NULL) return 0;

	pResults->ListCount = Count;
	if(pResults->ListCount <= 0) return 0;

	BufSize = sizeof(struct ppohVIS_BASE_stResult *) * pResults->ListCount;
	pResults->Results = (struct ppohVIS_BASE_stResult **)malloc(BufSize);
	if(pResults->Results == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "result list");
		goto Error;
	};
	for(i=0; i<pResults->ListCount; i++) {
		pResults->Results[i] = NULL;
	};

	return 0;

Error:
	return -1;
};


/*******************************************************************************
 * Free
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeResult(
	struct ppohVIS_BASE_stResult *pResult)
{
	if(pResult == NULL) return;

	free(pResult);
	pResult = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeResultCollection(
	struct ppohVIS_BASE_stResultCollection *pResults)
{
	int i;

	if(pResults == NULL) return;

	if(pResults->Results) {
		for(i=0; i<pResults->ListCount; i++) {
			if(pResults->Results[i]) {
				ppohVIS_BASE_FreeResult(pResults->Results[i]);
			};
		};
		free(pResults->Results);
	};

	free(pResults);
	pResults = NULL;
};


/*******************************************************************************
 * Print result information
 ******************************************************************************/
extern void
ppohVIS_BASE_PrintResult(
	FILE *fp, struct ppohVIS_BASE_stResult *pResult)
{
	int i, j;

	if(pResult == NULL) {
		return;
	}

	fprintf(fp, "%d\n", pResult->ItemCount);
	fprintf(fp, "%d\n", pResult->EntityType);
	fprintf(fp, "%d\n", pResult->FreedomCount);
	fprintf(fp, "%s\n", pResult->Label);

	for(i=0; i<pResult->ItemCount; i++) {
		fprintf(fp, "%10d", i+1);
		for(j=0; j<pResult->FreedomCount; j++) {
			fprintf(fp, "%15.7E",
			        pResult->Value[pResult->FreedomCount*i+j]);
		};
		fprintf(fp, "\n");
	};

	return;
};


/*******************************************************************************
 * Print result information to a file
 ******************************************************************************/
extern int
ppohVIS_BASE_PutResult(
	char *FileName, struct ppohVIS_BASE_stResult *pResult, int Append)
{
	FILE *fp = NULL;
	int iMyRank;
	char cFileName[PPOHVIS_BASE_FILE_NAME_LEN];

	iMyRank = ppohVIS_BASE_GetCommRank();
	ppohVIS_BASE_GetDistFileName(FileName, iMyRank,
				cFileName, PPOHVIS_BASE_FILE_NAME_LEN);

	/* Open file */
	if(Append == 0) {
		if((fp = fopen(cFileName, "w")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "result info.");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "result info.");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintResult(fp, pResult);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "result info.");
		goto error;
	};

	return 0;

error:
	return -1;
};
