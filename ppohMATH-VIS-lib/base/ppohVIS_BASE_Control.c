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
#include "ppohVIS_BASE_Control.h"


/*******************************************************************************
 * allocate
 ******************************************************************************/
extern struct ppohVIS_BASE_stControl *
ppohVIS_BASE_AllocateControl(void)
{
	struct ppohVIS_BASE_stControl *pControl = NULL;
	size_t BufSize;

	/* allocate */
	BufSize = sizeof(struct ppohVIS_BASE_stControl);
	pControl = (struct ppohVIS_BASE_stControl *)malloc(BufSize);
	if(pControl == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "control info");
		goto Error;
	};

	/* initialize */

	return pControl;

Error:
	ppohVIS_BASE_FreeControl(pControl);
	return NULL;
};


/*******************************************************************************
 * initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_InitRefineControl(
	struct ppohVIS_BASE_stRefineControl *pRefineCtrl)
{
	if(pRefineCtrl == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound, "%s",
			"control info for refinement");
		goto Error;
	};

	pRefineCtrl->AvailableMemory = 0.0;
	pRefineCtrl->MaxRefineLevel = 0;
	pRefineCtrl->MaxVoxelCount = 0;

	return 0;

Error:
	return 1;
};


extern int
ppohVIS_BASE_InitSimpleControl(
	struct ppohVIS_BASE_stSimpleControl *pSimpleCtrl)
{
	if(pSimpleCtrl == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound, "%s",
			"control info for simplification");
		goto Error;
	};

	pSimpleCtrl->ReductionRate = 0.0;

	return 0;

Error:
	return 1;
};


/*============================================================================*/
extern int
ppohVIS_BASE_InitControl(
	struct ppohVIS_BASE_stControl *pControl)
{
	if(pControl == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound, "%s",
			"control info");
		goto Error;
	};

	ppohVIS_BASE_InitRefineControl(&(pControl->Refine));
	ppohVIS_BASE_InitSimpleControl(&(pControl->Simple));

	return 0;

Error:
	return 1;
};


/*******************************************************************************
 * free
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeControl(
	struct ppohVIS_BASE_stControl *pControl)
{
	if(pControl) {
		free(pControl);
	};
	pControl = NULL;
};


/*******************************************************************************
 * I/O
 ******************************************************************************/
static int
PrintRefineControl(
	FILE *fp, struct ppohVIS_BASE_stRefineControl *pRefineCtrl)
{
	fprintf(fp, "\n### refinement information\n");

	if(pRefineCtrl == NULL) {
		fprintf(fp, "Error: not allocated\n");
		return -1;
	};

	fprintf(fp, "AvailableMemory=%le\n", pRefineCtrl->AvailableMemory);
	fprintf(fp, "MaxRefineLevel =%d\n", pRefineCtrl->MaxRefineLevel);
	fprintf(fp, "MaxVoxelCount  =%d\n", pRefineCtrl->MaxVoxelCount);

	return 0;
};


static int
PrintSimpleControl(
	FILE *fp, struct ppohVIS_BASE_stSimpleControl *pSimpleCtrl)
{
	fprintf(fp, "\n### simplification information\n");

	if(pSimpleCtrl == NULL) {
		fprintf(fp, "Error: not allocated\n");
		return -1;
	};

	fprintf(fp, "ReductionRate=%le\n", pSimpleCtrl->ReductionRate);

	return 0;
};


/*============================================================================*/
extern int
ppohVIS_BASE_PrintControl(
	FILE *fp, struct ppohVIS_BASE_stControl *pControl)
{
	fprintf(fp, "### control information\n");

	if(pControl == NULL) {
		fprintf(fp, "Error: not allocated\n");
		return -1;
	};

	PrintRefineControl(fp, &(pControl->Refine));
	PrintSimpleControl(fp, &(pControl->Simple));

	return 0;
};


/*============================================================================*/
extern int
ppohVIS_BASE_PutControl(
	char *cFileName, struct ppohVIS_BASE_stControl *pControl)
{
	FILE *fp = NULL;

	if((fp = fopen(cFileName, "w")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError, "%s [%s]",
			"control file", cFileName);
		goto Error;
	};

	if(ppohVIS_BASE_PrintControl(fp, pControl)) goto Error;

	if(fclose(fp)) goto Error;

	return 0;

Error:
	if(fp) fclose(fp);
	return 1;
};

