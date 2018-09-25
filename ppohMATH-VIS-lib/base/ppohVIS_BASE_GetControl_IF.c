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
#include "ppohVIS_BASE_F2CUtil.h"
#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_Control_C2F.h"
#include "ppohVIS_BASE_ControlFile.h"


static struct ppohVIS_BASE_stControl *ptrControl;


/*******************************************************************************
 * initialize
 ******************************************************************************/
extern void
ppohvis_base_get_control_init_if(int *iErr)
{
	*iErr = 0;
	return;
};

extern void
ppohvis_base_get_control_init_if_(int *iErr)
{
	ppohvis_base_get_control_init_if(iErr);
};

extern void
ppohvis_base_get_control_init_if__(int *iErr)
{
	ppohvis_base_get_control_init_if(iErr);
};

extern void
PPOHVIS_BASE_GET_CONTROL_INIT_IF(int *iErr)
{
	ppohvis_base_get_control_init_if(iErr);
};


/*******************************************************************************
 * main procedure
 ******************************************************************************/
extern void
ppohvis_base_get_control_if(char *cFileName, int *iErr, int iLen)
{
	char cFileNameC[PPOHVIS_BASE_FILE_NAME_LEN];
	char *cErrMsg;

	/* file name */
	if(ppohVIS_BASE_StrCpyF2C(cFileName, iLen,
                                  cFileNameC, sizeof(cFileNameC)) == NULL) {
		goto Error;
	};

	/* c function */
	ptrControl = ppohVIS_BASE_GetControl(cFileNameC);
	if(ptrControl == NULL) {
		goto Error;
	};

	if(ppohVIS_BASE_ControlC2FInit(ptrControl)) {
		goto Error;
	};

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_base_get_control_if_(char *cFileName, int *iErr, int iLen)
{
	ppohvis_base_get_control_if(cFileName, iErr, iLen);
};

extern void
ppohvis_base_get_control_if__(char *cFileName, int *iErr, int iLen)
{
	ppohvis_base_get_control_if(cFileName, iErr, iLen);
};

extern void
PPOHVIS_BASE_GET_CONTROL_IF(char *cFileName, int *iErr, int iLen)
{
	ppohvis_base_get_control_if(cFileName, iErr, iLen);
};

/*============================================================================*/
extern void
ppohvis_base_get_control_get_refinecontrol_if(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_BASE_ControlC2FGetRefineControl(
		AvailableMemory, MaxRefineLevel, MaxVoxelCount)) {
		goto Error;
	};

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_base_get_control_get_refinecontrol_if_(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	ppohvis_base_get_control_get_refinecontrol_if(
		AvailableMemory, MaxRefineLevel, MaxVoxelCount, iErr);
};

extern void
ppohvis_base_get_control_get_refinecontrol_if__(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	ppohvis_base_get_control_get_refinecontrol_if(
		AvailableMemory, MaxRefineLevel, MaxVoxelCount, iErr);
};

extern void
PPOHVIS_BASE_GET_CONTROL_GET_REFINECONTROL_IF(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	ppohvis_base_get_control_get_refinecontrol_if(
		AvailableMemory, MaxRefineLevel, MaxVoxelCount, iErr);
};


/*============================================================================*/
extern void
ppohvis_base_get_control_get_simplecontrol_if(
	double *ReductionRate, int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_BASE_ControlC2FGetSimpleControl(ReductionRate)) {
		goto Error;
	};

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_base_get_control_get_simplecontrol_if_(
	double *ReductionRate, int *iErr)
{
	ppohvis_base_get_control_get_simplecontrol_if(ReductionRate, iErr);
};

extern void
ppohvis_base_get_control_get_simplecontrol_if__(
	double *ReductionRate, int *iErr)
{
	ppohvis_base_get_control_get_simplecontrol_if(ReductionRate, iErr);
};

extern void
PPOHVIS_BASE_GET_CONTROL_GET_SIMPLECONTROL_IF(
	double *ReductionRate, int *iErr)
{
	ppohvis_base_get_control_get_simplecontrol_if(ReductionRate, iErr);
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern void
ppohvis_base_get_control_finalize_if(int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_BASE_ControlC2FFinalize()) {
		goto Error;
	};

	free(ptrControl);
	ptrControl = NULL;

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_base_get_control_finalize_if_(int *iErr)
{
	ppohvis_base_get_control_finalize_if(iErr);
};

extern void
ppohvis_base_get_control_finalize_if__(int *iErr)
{
	ppohvis_base_get_control_finalize_if(iErr);
};

extern void
PPOHVIS_BASE_GET_CONTROL_FINALIZE_IF(int *iErr)
{
	ppohvis_base_get_control_finalize_if(iErr);
};
