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

#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_UCD.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelUtilGetCoord.h"
#include "ppohVIS_BASE_VoxelValue.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_ParallelVoxel.h"
#include "ppohVIS_FDM3D_InitialVoxel.h"
#include "ppohVIS_FDM3D_VoxelRefine.h"


/*******************************************************************************
 * visualization for FDM3D
 ******************************************************************************/
static int
ppohVIS_FDM3D_VisualInner(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stMesh *pFreeSurfMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult,
	struct ppohVIS_BASE_stControl *pControl,
	char *cFileHeader,
	int iStep)
{
	struct ppohVIS_BASE_stMeshNode *pInnerVoxelNode = NULL;
	struct ppohVIS_BASE_stVoxelValue *pVoxelValueGlobal = NULL;
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue = NULL;
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	char sUCDFileName[PPOHVIS_BASE_FILE_NAME_LEN];
	int iRc;
	ppohVIS_BASE_Comm comm;

//debug
//	char sTMPFileName[PPOHVIS_BASE_FILE_NAME_LEN];
//debug

	comm = ppohVIS_BASE_GetCommunicator();

//debug
//	memset(sTMPFileName, '\0', PPOHVIS_BASE_FILE_NAME_LEN);
//	sprintf(sTMPFileName, "debug_result.%d", iStep);
//	if(ppohVIS_BASE_GetCommRank() == 0) {
//		ppohVIS_BASE_PutResult(sTMPFileName, pResult, 0);
//	};
//debug

	/*==============================*
	 * initialize voxel information *
	 *==============================*/
	iRc = ppohVIS_FDM3D_InitInitialVoxel(pMesh, pFreeSurf, pIniVoxel);
	if(iRc != 0) {
		goto Error;
	};

	/*===============*
	 * refine voxels *
	 *===============*/
	iRc = ppohVIS_FDM3D_RefineVoxel(pIniVoxel, pMesh, pFreeSurf,
	                                pResult, pControl, comm);
	if(iRc != 0) {
		goto Error;
	};

	pVoxelValue = ppohVIS_BASE_InitialVoxelToVoxelValue(pIniVoxel, pResult);
	if(pVoxelValue == NULL) {
		goto Error;
	};

	/*=======================*
	 * get voxel information *
	 *=======================*/
	pInnerVoxelNode = ppohVIS_BASE_GetInnerVoxelCoordList(pIniVoxel);
	if(pInnerVoxelNode == NULL) {
		goto Error;
	};

	pVoxelValueGlobal = ppohVIS_BASE_GatherVoxelValue(pVoxelValue, comm);
	if(ppohVIS_BASE_GetCommRank() == 0) {
		pUCDData = ppohVIS_BASE_ConvertVoxelValue2UCD2(pVoxelValueGlobal);

		memset(sUCDFileName, '\0', PPOHVIS_BASE_FILE_NAME_LEN);
		sprintf(sUCDFileName, "%s_%s.%d.inp",
				cFileHeader, pResult->Label, iStep);

		ppohVIS_BASE_PutUCD(sUCDFileName, pUCDData);

		if(pUCDData != NULL) {
			ppohVIS_BASE_FreeUCD(pUCDData);
			pUCDData = NULL;
		};
	};

	return 0;

Error:
	return -1;
};


/******************************************************************************/
extern int
ppohVIS_FDM3D_Visual(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResultCollection *pResultNode,
	struct ppohVIS_BASE_stResultCollection *pResultElement,
	struct ppohVIS_BASE_stControl *pControl,
	char *cFileHeader,
	int iStep)
{
	struct ppohVIS_BASE_stMesh *pFreeSurfMesh = NULL;
	struct ppohVIS_BASE_stResult *pResult = NULL;
	int iRc, i;

	/*======================*
	 * mesh of free surface *
	 *======================*/
	pFreeSurfMesh = ppohVIS_BASE_ConvertGeometry2Mesh(pMesh->Node, pFreeSurf);
	if(pFreeSurfMesh == NULL) {
		goto Error;
	};

	if(pResultNode) {
		for(i=0; i<pResultNode->ListCount; i++) {
			pResult = pResultNode->Results[i];

			iRc = ppohVIS_FDM3D_VisualInner(
					pMesh, pFreeSurfMesh,
					pFreeSurf, pIniVoxel,
					pResult, pControl, cFileHeader, iStep);
			if(iRc != 0) {
				goto Error;
			};
		};
	};

	if(pResultElement) {
		for(i=0; i<pResultElement->ListCount; i++) {
			pResult = pResultElement->Results[i];

			iRc = ppohVIS_FDM3D_VisualInner(
					pMesh, pFreeSurfMesh,
					pFreeSurf, pIniVoxel,
					pResult, pControl, cFileHeader, iStep);
			if(iRc != 0) {
				goto Error;
			};
		};
	};

	if(pFreeSurfMesh != NULL) {
		ppohVIS_BASE_FreeMesh(pFreeSurfMesh);
		pFreeSurfMesh = NULL;
	};

	return 0;

Error:
	if(pFreeSurfMesh != NULL) {
		ppohVIS_BASE_FreeMesh(pFreeSurfMesh);
		pFreeSurfMesh = NULL;
	};

	return -1;
};
