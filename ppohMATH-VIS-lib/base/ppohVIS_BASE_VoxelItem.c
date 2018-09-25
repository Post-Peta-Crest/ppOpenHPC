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

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_VoxelItem.h"

/*******************************************************************************
 * Allocate
 ******************************************************************************/
extern struct ppohVIS_BASE_stVoxelItem *
ppohVIS_BASE_AllocateVoxelItem(void)
{
	struct ppohVIS_BASE_stVoxelItem *pVoxel;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem);
	pVoxel = (struct ppohVIS_BASE_stVoxelItem *)malloc(BufSize);
	if(pVoxel == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "voxel item");
		return NULL;
	};

	pVoxel->Level = 0;
	pVoxel->IndexX = -1;
	pVoxel->IndexY = -1;
	pVoxel->IndexZ = -1;

	pVoxel->Type = ppohVIS_BASE_VoxelUnknown;
	pVoxel->OX = 0.0;
	pVoxel->OY = 0.0;
	pVoxel->OZ = 0.0;
	pVoxel->DX = 0.0;
	pVoxel->DY = 0.0;
	pVoxel->DZ = 0.0;

	pVoxel->NodeCount = 0;
	pVoxel->NodeID = NULL;
	pVoxel->ElementCount = 0;
	pVoxel->ElementID = NULL;
	pVoxel->FreeSurfaceCount = 0;
	pVoxel->FreeSurfaceID = NULL;

	pVoxel->Value = NULL;
	pVoxel->Cost = 0.0;
	pVoxel->CoefN = 0.0;
	pVoxel->CoefD = 0.0;

	pVoxel->Parent = NULL;
	pVoxel->Children = NULL;

	return pVoxel;
};


/*******************************************************************************
 * Create children
 ******************************************************************************/
extern int
ppohVIS_BASE_CreateChildVoxelItem(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	size_t BufSize;
	int iX, iY, iZ, i;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem *) * 8;
	pVoxel->Children = (struct ppohVIS_BASE_stVoxelItem **)malloc(BufSize);
	if(pVoxel->Children == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "child voxels");
		return -1;
	} else {
		for(i=0; i<8; i++) {
			pVoxel->Children[i] = NULL;
		};
	};;

	for(iZ=0; iZ<2; iZ++) {
	for(iY=0; iY<2; iY++) {
	for(iX=0; iX<2; iX++) {
		i = 4 * iZ + 2 * iY + iX;
		pVoxel->Children[i] = ppohVIS_BASE_AllocateVoxelItem();

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
	};
	};
	};

	return 0;
};


/*******************************************************************************
 * Free
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeVoxelItem(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	if(pVoxel == NULL) return;

	if(pVoxel->NodeID) {
		free(pVoxel->NodeID);
		pVoxel->NodeID = NULL;
	};
	if(pVoxel->ElementID) {
		free(pVoxel->ElementID);
		pVoxel->ElementID = NULL;
	};
	if(pVoxel->FreeSurfaceID) {
		free(pVoxel->FreeSurfaceID);
		pVoxel->FreeSurfaceID = NULL;
	};

	if(pVoxel->Value) {
		free(pVoxel->Value);
		pVoxel->Value = NULL;
	};

	pVoxel->Parent = NULL;
	if(pVoxel->Children) {
		free(pVoxel->Children);
		pVoxel->Children = NULL;
	};

	free(pVoxel);
	pVoxel = NULL;

	return;
};


/******************************************************************************/
extern void
ppohVIS_BASE_FreeVoxelItems(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int i;

	if(pVoxel == NULL) return;

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			ppohVIS_BASE_FreeVoxelItems(pVoxel->Children[i]);
		};
	};

	ppohVIS_BASE_FreeVoxelItem(pVoxel);

	return;
};


/*******************************************************************************
 * Initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_InitVoxelItem(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	if(pVoxel == NULL) return 0;

	pVoxel->Level = 0;
	pVoxel->IndexX = -1;
	pVoxel->IndexY = -1;
	pVoxel->IndexZ = -1;

	pVoxel->Type = ppohVIS_BASE_VoxelUnknown;
	pVoxel->OX = 0.0;
	pVoxel->OY = 0.0;
	pVoxel->OZ = 0.0;
	pVoxel->DX = 0.0;
	pVoxel->DY = 0.0;
	pVoxel->DZ = 0.0;

	pVoxel->NodeCount = 0;
	if(pVoxel->NodeID) {
		free(pVoxel->NodeID);
		pVoxel->NodeID = NULL;
	};

	pVoxel->ElementCount = 0;
	if(pVoxel->ElementID) {
		free(pVoxel->ElementID);
		pVoxel->ElementID = NULL;
	};

	pVoxel->FreeSurfaceCount = 0;
	if(pVoxel->FreeSurfaceID) {
		free(pVoxel->FreeSurfaceID);
		pVoxel->FreeSurfaceID = NULL;
	};

	if(pVoxel->Value) {
		free(pVoxel->Value);
		pVoxel->Value = NULL;
	};
	pVoxel->Cost = 0.0;
	pVoxel->CoefN = 0.0;
	pVoxel->CoefD = 0.0;

	pVoxel->Parent = NULL;
	if(pVoxel->Children) {
		free(pVoxel->Children);
		pVoxel->Children = NULL;
	};

	return 0;
};


/*******************************************************************************
 * Information function
 ******************************************************************************/
extern int
ppohVIS_BASE_GetLocalIndex(int IndexGlobal)
{
	return IndexGlobal % 2;
};

extern int
ppohVIS_BASE_GetParentIndex(int IndexCurrent)
{
	int iMod;

	iMod = IndexCurrent % 2;
	return (IndexCurrent - iMod) / 2;
};

extern int
ppohVIS_BASE_GetRootIndex(int SrcLevel, int DstLevel, int IndexCurrent)
{
	int iBase, iMod, i;

	if(SrcLevel < DstLevel) return -1;

	iBase = 1;
	for(i=0; i<SrcLevel-DstLevel; i++) {
		iBase *= 2;
	};

	iMod = IndexCurrent % iBase;
	return (IndexCurrent - iMod) / iBase;
};


