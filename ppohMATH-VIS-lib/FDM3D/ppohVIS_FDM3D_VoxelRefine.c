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
#include <math.h>

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_VoxelCount.h"
#include "ppohVIS_BASE_VoxelUtil.h"
#include "ppohVIS_BASE_VoxelUtilSetValue.h"
#include "ppohVIS_BASE_VoxelUtilSetChildProp.h"
#include "ppohVIS_BASE_VoxelCost.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_ParallelVoxel.h"


/*******************************************************************************
 * Refine voxel for node value
 ******************************************************************************/
static int
RefineVoxelByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult,
	struct ppohVIS_BASE_stRefineControl *pRefineCtrl,
	ppohVIS_BASE_Comm comm)
{
	int RefineCount, MaxVoxelCount, MaxLevel, iMyRank, iRefineRank;
	int iVoxelCountLocal, iVoxelCountGlobal;
	struct ppohVIS_BASE_stVoxelItem *pRefineVoxel;

	iMyRank = ppohVIS_BASE_GetCommRank();

	/*======================================================================
	 * calculate voxel value and cost
	 *====================================================================*/
	ppohVIS_BASE_SetVoxelValuesByNode(pIniVoxel, pMesh, pResult);
	ppohVIS_BASE_SetVoxelCostsByNode(pIniVoxel, pResult);

	/*======================================================================
	 * control data for refinement
	 *====================================================================*/
	MaxLevel = pRefineCtrl->MaxRefineLevel;
	MaxVoxelCount = pRefineCtrl->MaxVoxelCount;

	/*======================================================================
	 * refine voxel
	 *====================================================================*/
	RefineCount = 0;
	iRefineRank = iMyRank;
	for(;;) {
		/*--------------------------------------------------------------
		 * search maximum cost voxel
		 *------------------------------------------------------------*/
		if(iRefineRank == iMyRank) {
			pRefineVoxel = NULL;
			pRefineVoxel = ppohVIS_BASE_SearchCostliestVoxel(pIniVoxel, MaxLevel);
		};

		iRefineRank = ppohVIS_BASE_GetMaxCostRank(pRefineVoxel, comm);
		if(iRefineRank < 0) {
			break;
		};

		/*--------------------------------------------------------------
		 * refine voxel and set voxel property to children
		 *------------------------------------------------------------*/
		if(iMyRank == iRefineRank) {
			ppohVIS_BASE_SetChildVoxelPropertyByNode(
				pIniVoxel, pRefineVoxel, pMesh, pFreeSurf, pResult);
		};

		RefineCount += 1;

		/*--------------------------------------------------------------
		 * count voxels and reduce them
		 *------------------------------------------------------------*/
		iVoxelCountLocal = ppohVIS_BASE_GetVoxelCount(pIniVoxel);
		ppohVIS_BASE_ReduceVoxelCount(iVoxelCountLocal, comm, &iVoxelCountGlobal);

		if(iVoxelCountGlobal > MaxVoxelCount) {
			break;
		};
	};

	if(iMyRank == 0) {
		fprintf(stderr, "[ppohVIS] finished to refine voxels\n");
		fprintf(stderr, "[ppohVIS]   refinement : %d\n", RefineCount);
		fprintf(stderr, "[ppovVIS]   # of voxels: %d\n", iVoxelCountGlobal);
	};

	return 0;
};


/*******************************************************************************
 * Refine voxel for element value
 ******************************************************************************/
static int
RefineVoxelByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult,
	struct ppohVIS_BASE_stRefineControl *pRefineCtrl,
	ppohVIS_BASE_Comm comm)
{
	int RefineCount, MaxVoxelCount, MaxLevel, iMyRank, iRefineRank, iRc;
	int iVoxelCountLocal, iVoxelCountGlobal;
	struct ppohVIS_BASE_stVoxelItem *pRefineVoxel;

	iMyRank = ppohVIS_BASE_GetCommRank();

	/*============*
	 * initialize *
	 *============*/
	/* voxel value */
	iRc = ppohVIS_BASE_SetVoxelValuesByElement(pIniVoxel, pMesh, pResult);
	if(iRc != 0) goto Error;

	/* voxel cost */
	iRc = ppohVIS_BASE_SetVoxelCostsByElement(pIniVoxel, pResult);
	if(iRc != 0) goto Error;

	/*========*
	 * refine *
	 *========*/
	MaxLevel = pRefineCtrl->MaxRefineLevel;
	MaxVoxelCount = pRefineCtrl->MaxVoxelCount;

	RefineCount = 0;
	iRefineRank = iMyRank;
	for(;;) {
		if(iRefineRank == iMyRank) {
			pRefineVoxel = NULL;
			pRefineVoxel = ppohVIS_BASE_SearchCostliestVoxel(pIniVoxel, MaxLevel);
		};

		iRefineRank = ppohVIS_BASE_GetMaxCostRank(pRefineVoxel, comm);

		if(iRefineRank < 0) {
			break;
		};

		if(iMyRank == iRefineRank) {
			ppohVIS_BASE_SetChildVoxelPropertyByElement(
				pIniVoxel, pRefineVoxel, pMesh, pFreeSurf, pResult);
		};

		RefineCount += 1;

		iVoxelCountLocal = ppohVIS_BASE_GetVoxelCount(pIniVoxel);
		ppohVIS_BASE_ReduceVoxelCount(iVoxelCountLocal, comm, &iVoxelCountGlobal);

		if(iVoxelCountGlobal > MaxVoxelCount) {
			break;
		};
	};

	if(iMyRank == 0) {
		fprintf(stderr, "[ppohVIS] finished to refine voxels\n");
		fprintf(stderr, "[ppohVIS]   refinement : %d\n", RefineCount);
		fprintf(stderr, "[ppovVIS]   # of voxels: %d\n", iVoxelCountGlobal);
	};

	return 0;

Error:
	return -1;
};


/*******************************************************************************
 * Refine voxel
 ******************************************************************************/
extern int
ppohVIS_FDM3D_RefineVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult,
	struct ppohVIS_BASE_stControl *pControl,
	ppohVIS_BASE_Comm comm)
{
	struct ppohVIS_BASE_stRefineControl *pRefineCtrl;

	/*======================================================================
	 * check data
	 *====================================================================*/
	if(pIniVoxel == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "voxel info");
		goto Error;
	};
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh info");
		goto Error;
	};
	if(pFreeSurf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "free surface info");
		goto Error;
	};
	if(pResult == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "result info");
		goto Error;
	};
	if(pControl == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "control info");
		goto Error;
	};

	pRefineCtrl = &(pControl->Refine);

	/*======================================================================
	 * refine
	 *====================================================================*/
	if(pResult->EntityType == ppohVIS_BASE_ResultNode) {
		if(RefineVoxelByNode(pIniVoxel, pMesh, pFreeSurf,
		                     pResult, pRefineCtrl, comm)) {
			goto Error;
		};

	} else if(pResult->EntityType == ppohVIS_BASE_ResultElement) {
		if(RefineVoxelByElement(pIniVoxel, pMesh, pFreeSurf,
		                        pResult, pRefineCtrl, comm)) {
			goto Error;
		};

	} else {
		ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_UnknownEntityType,
			"%d", pResult->EntityType);
		goto Error;
	};

	return 0;

Error:
	return -1;
};
