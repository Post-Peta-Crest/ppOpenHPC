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
#include <math.h>
#include <omp.h>

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_VoxelUtil.h"
#include "ppohVIS_BASE_VoxelCost.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelUtilSetValue.h"
#include "ppohVIS_BASE_VoxelUtilSetChildProp.h"


/*******************************************************************************
 * Set properties of Child Voxels
 ******************************************************************************/
static int
SetChildVoxelPropertyInit(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int i, iX, iY, iZ;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem *)*8;
	pVoxel->Children = (struct ppohVIS_BASE_stVoxelItem **)malloc(BufSize);
	if(pVoxel->Children == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "child voxels");
		goto error;
	} else {
		for(i=0; i<8; i++) {
			pVoxel->Children[i] = NULL;
		};
	};

	for(iZ=0; iZ<2; iZ++) {
	for(iY=0; iY<2; iY++) {
	for(iX=0; iX<2; iX++) {
		i = 4 * iZ + 2 * iY + iX;

		BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem);
		pVoxel->Children[i] = (struct ppohVIS_BASE_stVoxelItem *)malloc(BufSize);
		if(pVoxel->Children[i] == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "child voxel");
			goto error;
		};

		pVoxel->Children[i]->Level = pVoxel->Level + 1;
		pVoxel->Children[i]->IndexX = pVoxel->IndexX * 2 + iX;
		pVoxel->Children[i]->IndexY = pVoxel->IndexY * 2 + iY;
		pVoxel->Children[i]->IndexZ = pVoxel->IndexZ * 2 + iZ;

		pVoxel->Children[i]->Type = pVoxel->Type;

		pVoxel->Children[i]->DX = pVoxel->DX * 0.5;
		pVoxel->Children[i]->DY = pVoxel->DY * 0.5;
		pVoxel->Children[i]->DZ = pVoxel->DZ * 0.5;
		pVoxel->Children[i]->OX = pVoxel->OX + pVoxel->DX * 0.5 * (double)(iX);
		pVoxel->Children[i]->OY = pVoxel->OY + pVoxel->DY * 0.5 * (double)(iY);
		pVoxel->Children[i]->OZ = pVoxel->OZ + pVoxel->DZ * 0.5 * (double)(iZ);

		pVoxel->Children[i]->NodeCount = 0;
		pVoxel->Children[i]->NodeID = NULL;
		pVoxel->Children[i]->ElementCount = 0;
		pVoxel->Children[i]->ElementID = NULL;
		pVoxel->Children[i]->FreeSurfaceCount = 0;
		pVoxel->Children[i]->FreeSurfaceID = NULL;
		pVoxel->Children[i]->Parent = pVoxel;
		pVoxel->Children[i]->Children = NULL;
		pVoxel->Children[i]->Value = NULL;
		pVoxel->Children[i]->Cost = 0.0;
	};
	};
	};

	return 0;

error:
	return -1;
};


/******************************************************************************/
static int
SetChildVoxelNodeInfo(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh)
{
	int *NodeMask = NULL;
	double X, Y, Z;
	int Node;
	int i, j, iX, iY, iZ, iNode;
	int ThreadCount;
	int BufCount;

	ThreadCount = ppohVIS_BASE_GetThreadSize();

	NodeMask = (int *)calloc(pVoxel->NodeCount, sizeof(int));
	if(NodeMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask of node");
		goto error;
	};

	for(iNode=0; iNode<pVoxel->NodeCount; iNode++) {
		Node = pVoxel->NodeID[iNode];

		X = pMesh->Node->Coords[3*Node-3];
		Y = pMesh->Node->Coords[3*Node-2];
		Z = pMesh->Node->Coords[3*Node-1];

		if(X < pVoxel->OX + pVoxel->DX * 0.5) {
			iX = 0;
		} else {
			iX = 1;
		};
		if(Y < pVoxel->OY + pVoxel->DY * 0.5) {
			iY = 0;
		} else {
			iY = 1;
		};
		if(Z < pVoxel->OZ + pVoxel->DZ * 0.5) {
			iZ = 0;
		} else {
			iZ = 1;
		};

		i = 4 * iZ + 2 * iY + iX;
		pVoxel->Children[i]->NodeCount += 1;
	};

	for(i=0; i<8; i++) {
		BufCount = pVoxel->Children[i]->NodeCount;
		pVoxel->Children[i]->NodeID = (int *)calloc(BufCount, sizeof(int));
		if(pVoxel->Children[i]->NodeID == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]",
				strerror(errno), "node list in voxel");
			goto error;
		};

		pVoxel->Children[i]->NodeCount = 0;
	};

	for(iNode=0; iNode<pVoxel->NodeCount; iNode++) {
		Node = pVoxel->NodeID[iNode];

		X = pMesh->Node->Coords[3*Node-3];
		Y = pMesh->Node->Coords[3*Node-2];
		Z = pMesh->Node->Coords[3*Node-1];

		if(X < pVoxel->OX + pVoxel->DX * 0.5) {
			iX = 0;
		} else {
			iX = 1;
		};
		if(Y < pVoxel->OY + pVoxel->DY * 0.5) {
			iY = 0;
		} else {
			iY = 1;
		};
		if(Z < pVoxel->OZ + pVoxel->DZ * 0.5) {
			iZ = 0;
		} else {
			iZ = 1;
		};

		i = 4 * iZ + 2 * iY + iX;
		j = pVoxel->Children[i]->NodeCount;

		pVoxel->Children[i]->NodeCount += 1;
		pVoxel->Children[i]->NodeID[j] = Node;
	};

	return 0;

error:
	if(NodeMask) {
		free(NodeMask);
		NodeMask = NULL;
	};

	return -1;
};


/******************************************************************************/
static int
SetChildVoxelElementInfo(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh)
{
	int *ElemMask = NULL;
	double X, Y, Z;
	int Elem;
	int i, j, iX, iY, iZ, iElem;
	int ThreadCount;
	int BufCount;

	ThreadCount = ppohVIS_BASE_GetThreadSize();

	ElemMask = (int *)calloc(pVoxel->ElementCount, sizeof(int));
	if(ElemMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask of node");
		goto error;
	};

//#pragma omp parallel for private(iElem,Elem,X,Y,Z,iX,iY,iZ,i)
	for(iElem=0; iElem<pVoxel->ElementCount; iElem++) {
		Elem = pVoxel->ElementID[iElem];

		X = pMesh->Element->Gravity[3*Elem-3];
		Y = pMesh->Element->Gravity[3*Elem-2];
		Z = pMesh->Element->Gravity[3*Elem-1];

		if(X < pVoxel->OX + pVoxel->DX * 0.5) {
			iX = 0;
		} else {
			iX = 1;
		};
		if(Y < pVoxel->OY + pVoxel->DY * 0.5) {
			iY = 0;
		} else {
			iY = 1;
		};
		if(Z < pVoxel->OZ + pVoxel->DZ * 0.5) {
			iZ = 0;
		} else {
			iZ = 1;
		};

		i = 4 * iZ + 2 * iY + iX;
		pVoxel->Children[i]->ElementCount += 1;
	};

	for(i=0; i<8; i++) {
		BufCount = pVoxel->Children[i]->ElementCount;
		pVoxel->Children[i]->ElementID = (int *)calloc(BufCount, sizeof(int));
		if(pVoxel->Children[i]->ElementID == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]",
				strerror(errno), "node list in voxel");
			goto error;
		};

		pVoxel->Children[i]->ElementCount = 0;
	};

//#pragma omp parallel for private(iElem,Elem,X,Y,Z,iX,iY,iZ,i,j)
	for(iElem=0; iElem<pVoxel->ElementCount; iElem++) {
		Elem = pVoxel->ElementID[iElem];

		X = pMesh->Element->Gravity[3*Elem-3];
		Y = pMesh->Element->Gravity[3*Elem-2];
		Z = pMesh->Element->Gravity[3*Elem-1];

		if(X < pVoxel->OX + pVoxel->DX * 0.5) {
			iX = 0;
		} else {
			iX = 1;
		};
		if(Y < pVoxel->OY + pVoxel->DY * 0.5) {
			iY = 0;
		} else {
			iY = 1;
		};
		if(Z < pVoxel->OZ + pVoxel->DZ * 0.5) {
			iZ = 0;
		} else {
			iZ = 1;
		};

		i = 4 * iZ + 2 * iY + iX;
		j = pVoxel->Children[i]->ElementCount;

		pVoxel->Children[i]->ElementCount += 1;
		pVoxel->Children[i]->ElementID[j] = Elem;
	};

	return 0;

error:
	if(ElemMask) {
		free(ElemMask);
		ElemMask = NULL;
	};

	return -1;
};


/******************************************************************************/
static int
SetChildVoxelTriangularSurfaceInfo(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult)
{
	int *TsufMask = NULL;

	TsufMask = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(TsufMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]",
			strerror(errno), "mask of triangular surface");
		goto error;
	};

	ppohVIS_BASE_SearchFreeSurfaceInsideByParent(pVoxel, pMesh, pFreeSurf, TsufMask);

	if(TsufMask) {
		free(TsufMask);
		TsufMask = NULL;
	};
	return 0;

error:
	if(TsufMask) {
		free(TsufMask);
		TsufMask = NULL;
	};
	return -1;
};


/******************************************************************************/
extern int
ppohVIS_BASE_SetChildVoxelPropertyByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult)
{
	int i;

	if(SetChildVoxelPropertyInit(pVoxel)) goto error;

	if(pVoxel->NodeCount > 0) {
		if(SetChildVoxelNodeInfo(pVoxel, pMesh)) goto error;
	};

	if(pVoxel->ElementCount > 0) {
		if(SetChildVoxelElementInfo(pVoxel, pMesh)) goto error;
	};

	if(pVoxel->Type == ppohVIS_BASE_VoxelBoundary) {
		if(SetChildVoxelTriangularSurfaceInfo(pVoxel, pMesh, pFreeSurf, pResult)) {
			goto error;
		};
	};

	for(i=0; i<8; i++) {
		ppohVIS_BASE_SetVoxelTypeSimple(pVoxel->Children[i]);
		ppohVIS_BASE_SetVoxelValueByNode(pVoxel->Children[i], pMesh, pResult);
		ppohVIS_BASE_SetVoxelCostByNode(pVoxel->Children[i], pResult);
	};

	return 0;

error:
	return -1;
};


/******************************************************************************/
extern int
ppohVIS_BASE_SetChildVoxelPropertyByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult)
{
	int i;

	if(SetChildVoxelPropertyInit(pVoxel)) goto error;

	if(pVoxel->NodeCount > 0) {
		if(SetChildVoxelNodeInfo(pVoxel, pMesh)) goto error;
	};

	if(pVoxel->ElementCount > 0) {
		if(SetChildVoxelElementInfo(pVoxel, pMesh)) goto error;
	};

	if(pVoxel->Type == ppohVIS_BASE_VoxelBoundary) {
		if(SetChildVoxelTriangularSurfaceInfo(pVoxel, pMesh, pFreeSurf, pResult)) {
			goto error;
		};
	};

	for(i=0; i<8; i++) {
		ppohVIS_BASE_SetVoxelTypeSimple(pVoxel->Children[i]);
		ppohVIS_BASE_SetVoxelValueByElement(pVoxel->Children[i], pMesh, pResult);
		ppohVIS_BASE_SetVoxelCostByElement(pVoxel->Children[i], pResult);
	};

	return 0;

error:
	return -1;
};

