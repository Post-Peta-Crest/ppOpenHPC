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
#include <omp.h>

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_FDM3D_StrGrid.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_BoundingBox.h"
#include "ppohVIS_BASE_Edge.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelUtil.h"
#include "ppohVIS_BASE_VoxelUtilGetCoord.h"
#include "ppohVIS_BASE_VoxelUtilSearchInside.h"
#include "ppohVIS_BASE_VoxelItem.h"
#include "ppohVIS_BASE_VoxelCount.h"
#include "ppohVIS_FDM3D_InitialVoxel.h"

#define MINIMUM_VOXEL_COUNT 4
//#define EXPAND_FACTOR (0.5)
#define EXPAND_FACTOR (0.0)
#define INITIAL_FACTOR (100.0)


/*******************************************************************************
 * Create initial voxels
 ******************************************************************************/
extern struct ppohVIS_BASE_stInitialVoxel *
ppohVIS_FDM3D_CreateInitialVoxel(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stBoundingBox *pBBox,
	struct ppohVIS_BASE_stGeometryItem *pEdge)
{
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel = NULL;
	double MinLen, MaxLen;
	double XMin, YMin, ZMin;
	double XMax, YMax, ZMax;
	double XLen, YLen, ZLen;
	int NX, NY, NZ;
	int iRc;
	size_t BufSize;

	/* allocate */
	pIniVoxel = ppohVIS_BASE_AllocateInitialVoxel();
	if(pIniVoxel == NULL) {
		goto Error;
	};

	/* set region */
	ppohVIS_BASE_GetEdgeLengthMinMax(pMesh->Node, pEdge, &MinLen, &MaxLen);

	XMin = pBBox->XMin - MinLen * EXPAND_FACTOR;
	YMin = pBBox->YMin - MinLen * EXPAND_FACTOR;
	ZMin = pBBox->ZMin - MinLen * EXPAND_FACTOR;
	XMax = pBBox->XMax + MinLen * EXPAND_FACTOR;
	YMax = pBBox->YMax + MinLen * EXPAND_FACTOR;
	ZMax = pBBox->ZMax + MinLen * EXPAND_FACTOR;
	XLen = XMax - XMin;
	YLen = YMax - YMin;
	ZLen = ZMax - ZMin;

	if(XLen < 0.0) XLen = MaxLen;
	if(YLen < 0.0) YLen = MaxLen;
	if(ZLen < 0.0) ZLen = MaxLen;

	NX = floor(XLen / (INITIAL_FACTOR * MaxLen));
	NY = floor(YLen / (INITIAL_FACTOR * MaxLen));
	NZ = floor(ZLen / (INITIAL_FACTOR * MaxLen));

	if(NX < MINIMUM_VOXEL_COUNT) NX = MINIMUM_VOXEL_COUNT;
	if(NY < MINIMUM_VOXEL_COUNT) NY = MINIMUM_VOXEL_COUNT;
	if(NZ < MINIMUM_VOXEL_COUNT) NZ = MINIMUM_VOXEL_COUNT;

	/* initialize member variables */
	pIniVoxel->OX = XMin;
	pIniVoxel->OY = YMin;
	pIniVoxel->OZ = ZMin;
	pIniVoxel->DX = XLen / (double)(NX);
	pIniVoxel->DY = YLen / (double)(NY);
	pIniVoxel->DZ = ZLen / (double)(NZ);
	pIniVoxel->NX = NX;
	pIniVoxel->NY = NY;
	pIniVoxel->NZ = NZ;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem)*NX*NY*NZ;
	pIniVoxel->Voxels = (struct ppohVIS_BASE_stVoxelItem *)malloc(BufSize);
	if(pIniVoxel->Voxels == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "initial voxels");
		return NULL;
	};

	iRc = ppohVIS_BASE_InitVoxelItems(pIniVoxel);
	if(iRc != 0) goto Error;

	return pIniVoxel;

Error:
	if(pIniVoxel) {
		ppohVIS_BASE_FreeInitialVoxel(pIniVoxel);
		pIniVoxel = NULL;
	};
	return NULL;
};


/*=====*
 * FDM *
 *=====*/
extern struct ppohVIS_BASE_stInitialVoxel *
ppohVIS_FDM3D_CreateInitialVoxelGrid(
	struct ppohVIS_FDM3D_stStrGrid *pGrid,
	struct ppohVIS_BASE_stBoundingBox *pBBox)
{
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel = NULL;
	double LenX, LenY, LenZ;
	int NX, NY, NZ, iRc;
	size_t BufSize;

	pIniVoxel = ppohVIS_BASE_AllocateInitialVoxel();
	if(pIniVoxel == NULL) {
		goto Error;
	};

	NX = MINIMUM_VOXEL_COUNT;
	NY = MINIMUM_VOXEL_COUNT;
	NZ = MINIMUM_VOXEL_COUNT;
	LenX = pGrid->DeltaX * (double)(pGrid->NumX);
	LenY = pGrid->DeltaY * (double)(pGrid->NumY);
	LenZ = pGrid->DeltaZ * (double)(pGrid->NumZ);

	pIniVoxel->OX = pGrid->OriginX;
	pIniVoxel->OY = pGrid->OriginY;
	pIniVoxel->OZ = pGrid->OriginZ;
	pIniVoxel->NX = NX;
	pIniVoxel->NY = NY;
	pIniVoxel->NZ = NZ;
	pIniVoxel->DX = LenX / (double)(pIniVoxel->NX);
	pIniVoxel->DY = LenY / (double)(pIniVoxel->NY);
	pIniVoxel->DZ = LenZ / (double)(pIniVoxel->NZ);

	pIniVoxel->Rank = (int *)calloc(NX*NY*NZ, sizeof(int));
	if(pIniVoxel->Rank == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "rank of voxel");
		return NULL;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem) * NX * NY * NZ;
	pIniVoxel->Voxels = (struct ppohVIS_BASE_stVoxelItem *)malloc(BufSize);
	if(pIniVoxel->Voxels == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "initial voxels");
		return NULL;
	};

	iRc = ppohVIS_BASE_InitVoxelItems(pIniVoxel);
	if(iRc != 0) goto Error;

	return pIniVoxel;

Error:
	if(pIniVoxel) {
		ppohVIS_BASE_FreeInitialVoxel(pIniVoxel);
		pIniVoxel = NULL;
	};
	return NULL;
};


/*******************************************************************************
 * Initialize Initial Voxels
 ******************************************************************************/
extern int
ppohVIS_FDM3D_InitInitialVoxel(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	double OX, OY, OZ, DX, DY, DZ, X, Y, Z;
	int *NodeMask = NULL;
	int *ElemMask = NULL;
	int *SurfMask = NULL;
	int NX, NY, NZ;
	int iNode, iElem, iTsuf, iX, iY, iZ, i, j;
	int MyRank;
	struct ppohVIS_BASE_stVoxelItem *p;

	MyRank = ppohVIS_BASE_GetCommRank();

	/* data check */
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh info");
		goto error;
	};
	if(pIniVoxel == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "initial voxel info");
		goto error;
	};

	/* initialize */
	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;
	OX = pIniVoxel->OX;
	OY = pIniVoxel->OY;
	OZ = pIniVoxel->OZ;
	DX = pIniVoxel->DX;
	DY = pIniVoxel->DY;
	DZ = pIniVoxel->DZ;

	NodeMask = (int *)calloc(pMesh->Node->Count, sizeof(int));
	if(NodeMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node mask array");
		goto error;
	};
	ElemMask = (int *)calloc(pMesh->Element->Count, sizeof(int));
	if(ElemMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element mask array");
		goto error;
	};
	SurfMask = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(SurfMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "surface mask array");
		goto error;
	};

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;

		ppohVIS_BASE_InitVoxelItem(&pIniVoxel->Voxels[i]);
	};
	};
	};
	//ppohVIS_BASE_InitVoxelItems(pIniVoxel);
	ppohVIS_BASE_InitVoxelItems(pIniVoxel);

	for(iNode=0; iNode<pMesh->Node->Count; iNode++) {
		X = pMesh->Node->Coords[3*iNode];
		Y = pMesh->Node->Coords[3*iNode+1];
		Z = pMesh->Node->Coords[3*iNode+2];

		iX = floor((X - OX) / DX);
		iY = floor((Y - OY) / DY);
		iZ = floor((Z - OZ) / DZ);
		if(iX >= NX) { iX = NX-1; };
		if(iY >= NY) { iY = NY-1; };
		if(iZ >= NZ) { iZ = NZ-1; };

		i = NX * NY * iZ + NX * iY + iX;
		NodeMask[iNode] = i + 1;

		pIniVoxel->Voxels[i].NodeCount += 1;
	};

	for(iElem=0; iElem<pMesh->Element->Count; iElem++) {
		X = pMesh->Element->Gravity[3*iElem];
		Y = pMesh->Element->Gravity[3*iElem+1];
		Z = pMesh->Element->Gravity[3*iElem+2];

		iX = floor((X - OX) / DX);
		iY = floor((Y - OY) / DY);
		iZ = floor((Z - OZ) / DZ);
		if(iX >= NX) { iX = NX-1; };
		if(iY >= NY) { iY = NY-1; };
		if(iZ >= NZ) { iZ = NZ-1; };

		i = NX * NY * iZ + NX * iY + iX;
		ElemMask[iElem] = i + 1;

		pIniVoxel->Voxels[i].ElementCount += 1;
	};

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		p = &pIniVoxel->Voxels[i];

		pIniVoxel->Rank[i] = -1;

		if(p->NodeCount > 0) {
			p->NodeID = (int *)calloc(p->NodeCount, sizeof(int));
			pIniVoxel->Rank[i] = MyRank;
		};
		p->NodeCount = 0;

		if(p->ElementCount > 0) {
			p->ElementID = (int *)calloc(p->ElementCount, sizeof(int));
			pIniVoxel->Rank[i] = MyRank;
		};
		p->ElementCount = 0;

		p->FreeSurfaceCount = 0;
	};
	};
	};

	for(iNode=0; iNode<pMesh->Node->Count; iNode++) {
		i = NodeMask[iNode] - 1;
		j = pIniVoxel->Voxels[i].NodeCount;
		pIniVoxel->Voxels[i].NodeID[j] = iNode+1;
		pIniVoxel->Voxels[i].NodeCount += 1;
	};

	for(iElem=0; iElem<pMesh->Element->Count; iElem++) {
		i = ElemMask[iElem] - 1;
		j = pIniVoxel->Voxels[i].ElementCount;
		pIniVoxel->Voxels[i].ElementID[j] = iElem+1;
		pIniVoxel->Voxels[i].ElementCount += 1;
	};

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;

		for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
			SurfMask[iTsuf] = 1;
		};

		ppohVIS_BASE_SearchFreeSurfaceInside(
			&pIniVoxel->Voxels[i], pMesh, pFreeSurf, SurfMask);
	};
	};
	};

	ppohVIS_BASE_SetVoxelsTypeSimple(pIniVoxel);

	/* Finalization */
	if(NodeMask) {
		free(NodeMask);
		NodeMask = NULL;
	};
	if(ElemMask) {
		free(ElemMask);
		ElemMask = NULL;
	};
	if(SurfMask) {
		free(SurfMask);
		SurfMask = NULL;
	};
	return 0;

error:
	if(NodeMask) {
		free(NodeMask);
		NodeMask = NULL;
	};
	if(ElemMask) {
		free(ElemMask);
		ElemMask = NULL;
	};
	if(SurfMask) {
		free(SurfMask);
		SurfMask = NULL;
	};
	return -1;
};
