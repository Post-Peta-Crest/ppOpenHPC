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

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_ParallelBoundingBox.h"
#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_ControlFile.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_FDM3D_StrGrid.h"
#include "ppohVIS_BASE_BoundingBox.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_Edge.h"
#include "ppohVIS_BASE_TriangularSurface.h"
#include "ppohVIS_BASE_QuadrilateralSurface.h"
#include "ppohVIS_BASE_FreeSurface.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_FDM3D_InitialVoxel.h"
#include "ppohVIS_FDM3D_VoxelRefine.h"
#include "ppohVIS_BASE_VoxelValue.h"
#include "ppohVIS_FDM3D_VoxelVisual.h"
#include "ppohVIS_FDM3D_Extern.h"


static struct ppohVIS_BASE_stMesh *pMeshCurrent;
static struct ppohVIS_FDM3D_stStrGrid *pStrGridCurrent ;

static struct ppohVIS_BASE_stBoundingBox *pBBoxCurrent;
static struct ppohVIS_BASE_stBoundingBox *pBBoxGlobal;

static struct ppohVIS_BASE_stGeometryItem *pEdgeCurrent;
static struct ppohVIS_BASE_stGeometryItem *pTsufCurrent;
static struct ppohVIS_BASE_stGeometryItem *pQsufCurrent;
static struct ppohVIS_BASE_stGeometryItem *pFreeSurfCurrent;

static struct ppohVIS_BASE_stInitialVoxel *pIniVoxelCurrent;

//static enum ppohVIS_BASE_eAnalysisType iAnalysisType;


/*******************************************************************************
 * Initialization
 ******************************************************************************/
extern int
ppohVIS_FDM3D_Init(ppohVIS_BASE_Comm comm)
{
	ppohVIS_BASE_InitParallel(comm);

//	iAnalysisType = ppohVIS_BASE_AnalysisUnknown;

	pBBoxGlobal = NULL;
	pMeshCurrent = NULL;
	pStrGridCurrent = NULL;
	pEdgeCurrent = NULL;
	pTsufCurrent = NULL;
	pQsufCurrent = NULL;
	pFreeSurfCurrent = NULL;

	return 0;
};


/*******************************************************************************
 * Get control
 ******************************************************************************/
extern struct ppohVIS_BASE_stControl *
ppohVIS_FDM3D_GetControl(char *cFileName)
{
	return ppohVIS_BASE_GetControl(cFileName);
};


/*******************************************************************************
 * Set mesh / grid information
 ******************************************************************************/
extern int
ppohVIS_FDM3D_SetMesh(struct ppohVIS_BASE_stMesh *pMesh)
{
	ppohVIS_BASE_Comm comm;
	int iRc;

	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(ppohVIS_BASE_ErrorCode_NullPointerFound,
		           "%s", "mesh information");
		goto Error;
	}

	comm = ppohVIS_BASE_GetCommunicator();

	/*==================*
	 * mesh information *
	 *==================*/
//	iAnalysisType = ppohVIS_BASE_AnalysisFEM;
	pMeshCurrent = pMesh;

	iRc = ppohVIS_BASE_CalculateGravity(pMeshCurrent);
	if(iRc != 0) {
		goto Error;
	};

	/*==============*
	 * bounding box *
	 *==============*/
	pBBoxCurrent = ppohVIS_BASE_GetBoundingBox(pMeshCurrent);
	if(pBBoxCurrent == NULL) {
		return -1;
	};

	pBBoxGlobal = ppohVIS_BASE_GetGlobalBoundingBox(pBBoxCurrent, comm);
	if(pBBoxGlobal == NULL) {
		return -1;
	};

	/*==========*
	 * geometry *
	 *==========*/
	pEdgeCurrent = ppohVIS_BASE_SearchEdge(pMeshCurrent->Element);
	if(pEdgeCurrent == NULL) {
		return -1;
	};

	pTsufCurrent = ppohVIS_BASE_SearchTriangularSurface(pMeshCurrent->Element);
	if(pTsufCurrent == NULL) {
		return -1;
	};

	pQsufCurrent = ppohVIS_BASE_SearchQuadrilateralSurface(pMeshCurrent->Element);
	if(pQsufCurrent == NULL) {
		return -1;
	};

	pFreeSurfCurrent = ppohVIS_BASE_GetFreeSurface(pTsufCurrent, pQsufCurrent);
	if(pFreeSurfCurrent == NULL) {
		return -1;
	};

	/*===============*
	 * initial voxel *
	 *===============*/
	pIniVoxelCurrent = ppohVIS_FDM3D_CreateInitialVoxel(
	                              pMeshCurrent, pBBoxGlobal, pEdgeCurrent);
	if(pIniVoxelCurrent == NULL) {
		return -1;
	};

	return 0;

Error:
	ppohVIS_BASE_FreeBoundingBox(pBBoxCurrent);
	ppohVIS_BASE_FreeBoundingBox(pBBoxGlobal);

	ppohVIS_BASE_FreeGeometryItem(pEdgeCurrent);
	ppohVIS_BASE_FreeGeometryItem(pTsufCurrent);
	ppohVIS_BASE_FreeGeometryItem(pQsufCurrent);
	ppohVIS_BASE_FreeGeometryItem(pFreeSurfCurrent);

	return -1;
};


/******************************************************************************/
extern int
ppohVIS_FDM3D_SetStrGrid(struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	struct ppohVIS_BASE_stMesh *pMesh = NULL;

	if(pGrid == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "structured grid");
		goto Error;
	};

	/*=============================*
	 * structured grid information *
	 *=============================*/
//	iAnalysisType = ppohVIS_BASE_AnalysisFDM;
	pStrGridCurrent = pGrid;

	pMesh = ppohVIS_FDM3D_ConvertStrGridToMesh(pStrGridCurrent);
	if(pMesh == NULL) {
		goto Error;
	};

	if(ppohVIS_FDM3D_SetMesh(pMesh)) {
		goto Error;
	};


	return 0;

Error:
	return -1;
};


/*******************************************************************************
 * Visualization
 ******************************************************************************/
extern int
ppohVIS_FDM3D_Visualize(
	struct ppohVIS_BASE_stResultCollection *pResultNode,
	struct ppohVIS_BASE_stResultCollection *pResultElement,
	struct ppohVIS_BASE_stControl *pControl,
	char *cFileHeader,
	int iStep)
{
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel = NULL;
/*
	pIniVoxel = ppohVIS_FDM3D_CopyInitialVoxel(pIniVoxel);
	if(pIniVoxel == NULL) {
		goto Error;
	};
*/
/*
	pIniVoxel = pIniVoxelCurrent;
*/
	pIniVoxel = ppohVIS_BASE_CopyInitialVoxel(pIniVoxelCurrent);
	if(pIniVoxel == NULL) {
		goto Error;
	};

	return ppohVIS_FDM3D_Visual(
		pMeshCurrent, pFreeSurfCurrent, pIniVoxel,
		pResultNode, pResultElement, pControl, cFileHeader, iStep);

Error:
	return -1;
};


/*******************************************************************************
 * Finalization
 ******************************************************************************/
extern int
ppohVIS_FDM3D_Finalize(void)
{
	ppohVIS_BASE_Barrier(ppohVIS_BASE_GetCommunicator());

#if 0
	pMeshCurrent = NULL;

	ppohVIS_FDM3D_FreeBoundingBox(pBBoxCurrent);
	ppohVIS_FDM3D_FreeBoundingBox(pBBoxGlobal);
	pBBoxCurrent = NULL;
	pBBoxGlobal = NULL;

	ppohVIS_FDM3D_FreeGeometryItem(pEdgeCurrent);
	ppohVIS_FDM3D_FreeGeometryItem(pTsufCurrent);
	ppohVIS_FDM3D_FreeGeometryItem(pQsufCurrent);
	ppohVIS_FDM3D_FreeGeometryItem(pFreeSurfCurrent);
	pEdgeCurrent = NULL;
	pTsufCurrent = NULL;
	pQsufCurrent = NULL;
	pFreeSurfCurrent = NULL;

	ppohVIS_FDM3D_FreeInitialVoxel(pIniVoxelCurrent);
	pIniVoxelCurrent = NULL;

	iAnalysisType = ppohVIS_FDM3D_AnalysisUnknown;
#endif

	return 0;
};

