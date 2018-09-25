/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_FDM3D                                     *
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
#include "ppohVIS_BASE_Control_F2C.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_Result_F2C.h"
#include "ppohVIS_FDM3D_Extern.h"


static struct ppohVIS_BASE_stControl *ptrControl;
static struct ppohVIS_BASE_stResultCollection *ptrResultNode;
static struct ppohVIS_BASE_stResultCollection *ptrResultElem;

/*******************************************************************************
 * initialize
 ******************************************************************************/
extern void
ppohvis_fdm3d_visualize_init_if(int *iErr)
{
	char *cErrMsg;

	/* initialization : control */
	ptrControl = ppohVIS_BASE_AllocateControl();
	if(ptrControl == NULL) goto Error;

	if(ppohVIS_BASE_ControlF2CInit(ptrControl)) {
		goto Error;
	};

	/* initialization : node result */
	ptrResultNode = ppohVIS_BASE_AllocateResultCollection();
	if(ptrResultNode == NULL) goto Error;

	if(ppohVIS_BASE_ResultF2CInit(ppohVIS_BASE_ResultNode,
	                               ptrResultNode)) {

	};

	/* initialization : element result */
	ptrResultElem = ppohVIS_BASE_AllocateResultCollection();
	if(ptrResultElem == NULL) goto Error;

	if(ppohVIS_BASE_ResultF2CInit(ppohVIS_BASE_ResultElement,
	                               ptrResultElem)) {
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
ppohvis_fdm3d_visualize_init_if_(int *iErr)
{
	ppohvis_fdm3d_visualize_init_if(iErr);
};

extern void
ppohvis_fdm3d_visualize_init_if__(int *iErr)
{
	ppohvis_fdm3d_visualize_init_if(iErr);
};

extern void
PPOHVIS_FDM3D_VISUALIZE_INIT_IF(int *iErr)
{
	ppohvis_fdm3d_visualize_init_if(iErr);
};


/*******************************************************************************
 * main procedures
 ******************************************************************************/
extern void
ppohvis_fdm3d_visualize_set_refinecontrol_if(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_BASE_ControlF2CSetRefineControl(
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
ppohvis_fdm3d_visualize_set_refinecontrol_if_(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	ppohvis_fdm3d_visualize_set_refinecontrol_if(
			AvailableMemory, MaxRefineLevel, MaxVoxelCount, iErr);
};

extern void
ppohvis_fdm3d_visualize_set_refinecontrol_if__(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	ppohvis_fdm3d_visualize_set_refinecontrol_if(
			AvailableMemory, MaxRefineLevel, MaxVoxelCount, iErr);
};

extern void
PPOHVIS_FDM3D_VISUALIZE_SET_REFINECONTROL_IF(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount,
	int *iErr)
{
	ppohvis_fdm3d_visualize_set_refinecontrol_if(
			AvailableMemory, MaxRefineLevel, MaxVoxelCount, iErr);
};

/*============================================================================*/
extern void
ppohvis_fdm3d_visualize_set_resultcollection_listcount_if(
	int *EntityType, int *ListCount, int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_BASE_ResultF2CSetListCount(
			(enum ppohVIS_BASE_eResultEntityType)(*EntityType),
			ListCount)) {
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
ppohvis_fdm3d_visualize_set_resultcollection_listcount_if_(
	int *EntityType, int *ListCount, int *iErr)
{
	ppohvis_fdm3d_visualize_set_resultcollection_listcount_if(
			EntityType, ListCount, iErr);
};

extern void
ppohvis_fdm3d_visualize_set_resultcollection_listcount_if__(
	int *EntityType, int *ListCount, int *iErr)
{
	ppohvis_fdm3d_visualize_set_resultcollection_listcount_if(
			EntityType, ListCount, iErr);
};

extern void
PPOHVIS_FDM3D_VISUALIZE_SET_RESULTCOLLECTION_LISTCOUNT_IF(
	int *EntityType, int *ListCount, int *iErr)
{
	ppohvis_fdm3d_visualize_set_resultcollection_listcount_if(
			EntityType, ListCount, iErr);
};


/*============================================================================*/
extern void
ppohvis_fdm3d_visualize_set_resultcollection_result_if(
	int *EntityType, int *Index, int *ItemCount, int *FreedomCount,
	char *Label, double *Value, int *iErr, int iLen)
{
	char cLabelC[PPOHVIS_BASE_LABEL_LEN];
	char *cErrMsg;
	enum ppohVIS_BASE_eResultEntityType iEntityType;

	/* copy LABEL from 'FORTRAN' region to 'C' region */
	if(ppohVIS_BASE_StrCpyF2C(
			Label, iLen, cLabelC, sizeof(cLabelC)) == NULL) {
		goto Error;
	};

	/* set result info. from 'FORTRAN' region to 'C' region */
	iEntityType = (enum ppohVIS_BASE_eResultEntityType)(*EntityType);
	if(ppohVIS_BASE_ResultF2CSetResult(
			iEntityType, (*Index)-1, ItemCount,
			FreedomCount, cLabelC, Value)) {
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
ppohvis_fdm3d_visualize_set_resultcollection_result_if_(
	int *EntityType, int *Index, int *ItemCount, int *FreedomCount,
	char *Label, double *Value, int *iErr, int iLen)
{
	ppohvis_fdm3d_visualize_set_resultcollection_result_if(
			EntityType, Index, ItemCount, FreedomCount,
			Label, Value, iErr, iLen);
};

extern void
ppohvis_fdm3d_visualize_set_resultcollection_result_if__(
	int *EntityType, int *Index, int *ItemCount, int *FreedomCount,
	char *Label, double *Value, int *iErr, int iLen)
{
	ppohvis_fdm3d_visualize_set_resultcollection_result_if(
			EntityType, Index, ItemCount, FreedomCount,
			Label, Value, iErr, iLen);
};

extern void
PPOHVIS_FDM3D_VISUALIZE_SET_RESULTCOLLECTION_RESULT_IF(
	int *EntityType, int *Index, int *ItemCount, int *FreedomCount,
	char *Label, double *Value, int *iErr, int iLen)
{
	ppohvis_fdm3d_visualize_set_resultcollection_result_if(
			EntityType, Index, ItemCount, FreedomCount,
			Label, Value, iErr, iLen);
};


/*============================================================================*/
extern void
ppohvis_fdm3d_visualize_if(char *cFileHeader, int *iStep, int *iErr, int iLen)
{
	char cFileHeaderC[PPOHVIS_BASE_FILE_NAME_LEN];
	char *cErrMsg;

	/* file name */
	if(ppohVIS_BASE_StrCpyF2C(
			cFileHeader, iLen,
			cFileHeaderC, sizeof(cFileHeaderC)) == NULL) {
		goto Error;
	};

	/* c function */
	if(ppohVIS_FDM3D_Visualize(
			ptrResultNode, ptrResultElem, ptrControl,
			cFileHeaderC, *iStep)) {
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
ppohvis_fdm3d_visualize_if_(char *cFileHeader, int *iStep, int *iErr, int iLen)
{
	ppohvis_fdm3d_visualize_if(cFileHeader, iStep, iErr, iLen);
};

extern void
ppohvis_fdm3d_visualize_if__(char *cFileHeader, int *iStep, int *iErr, int iLen)
{
	ppohvis_fdm3d_visualize_if(cFileHeader, iStep, iErr, iLen);
};

extern void
PPOHVIS_FDM3D_VISUALIZE_IF(char *cFileHeader, int *iStep, int *iErr, int iLen)
{
	ppohvis_fdm3d_visualize_if(cFileHeader, iStep, iErr, iLen);
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern void
ppohvis_fdm3d_visualize_finalize_if(int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_BASE_ControlF2CFinalize()) {
		goto Error;
	};
	ppohVIS_BASE_FreeControl(ptrControl);
	ptrControl = NULL;

	if(ppohVIS_BASE_ResultF2CFinalize(ppohVIS_BASE_ResultNode)) {
		goto Error;
	};
	ppohVIS_BASE_FreeResultCollection(ptrResultNode);
	ptrResultNode = NULL;

	if(ppohVIS_BASE_ResultF2CFinalize(ppohVIS_BASE_ResultElement)) {
		goto Error;
	};
	ppohVIS_BASE_FreeResultCollection(ptrResultElem);
	ptrResultElem = NULL;

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_fdm3d_visualize_finalize_if_(int *iErr)
{
	ppohvis_fdm3d_visualize_finalize_if(iErr);
};

extern void
ppohvis_fdm3d_visualize_finalize_if__(int *iErr)
{
	ppohvis_fdm3d_visualize_finalize_if(iErr);
};

extern void
PPOHVIS_FDM3D_VISUALIZE_FINALIZE_IF(int *iErr)
{
	ppohvis_fdm3d_visualize_finalize_if(iErr);
};
