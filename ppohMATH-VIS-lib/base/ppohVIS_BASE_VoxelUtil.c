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

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelUtil.h"


/*******************************************************************************
 * Set voxel type (simple version)
 ******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelTypeSimple(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int i;

	if(pVoxel == NULL) { return 0; };

	if(pVoxel->NodeCount > 0 || pVoxel->ElementCount > 0 || pVoxel->FreeSurfaceCount > 0) {
		if(pVoxel->FreeSurfaceCount > 0) {
			pVoxel->Type = ppohVIS_BASE_VoxelBoundary;
		} else {
			pVoxel->Type = ppohVIS_BASE_VoxelInner;
		};
	} else {
		pVoxel->Type = ppohVIS_BASE_VoxelOuter;
	};

	if(pVoxel->Children != NULL) {
		for(i=0; i<8; i++) {
			ppohVIS_BASE_SetVoxelTypeSimple(pVoxel->Children[i]);
		};
	};

	return 0;
};


/*============================================================================*/
/*
extern int
ppohVIS_BASE_SetVoxelsTypeSimple(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		if(ppohVIS_BASE_SetVoxelTypeSimple(&pIniVoxel->Voxels[i])) { goto error; };
	};
	};
	};

	return 0;

error:
	return -1;
};
*/

extern int
ppohVIS_BASE_SetVoxelsTypeSimple(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int NXYZ;
	int i;
	int iRc = 0;

	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:iRc)
	for(i=0; i<NXYZ; i++) {
		if(ppohVIS_BASE_SetVoxelTypeSimple(&pIniVoxel->Voxels[i])) {
			iRc++;
		};
	};
	if(iRc) {
		goto error;
	};

	return 0;

error:
	return -1;
};


/*******************************************************************************
 * Count voxels
 ******************************************************************************/
extern int
ppohVIS_BASE_GetVoxelCountAll(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int Count, i;

	Count = 0;

	if(pVoxel->Children == NULL) {
		Count = 1;
	} else {
		for(i=0; i<8; i++) {
			Count += ppohVIS_BASE_GetVoxelCountAll(pVoxel->Children[i]);
		};
	};

	return Count;
};


/*============================================================================*/
/*
extern int
ppohVIS_BASE_GetVoxelCountAllRoot(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NX, NXY;
	int iX, iY, iZ, i;

	Count = 0;
	NX    = pIniVoxel->NX;
	NXY   = pIniVoxel->NX * pIniVoxel->NY;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = iZ * NXY + iY * NX + iX;
		Count += ppohVIS_BASE_GetVoxelCountAll(&pIniVoxel->Voxels[i]);
	};
	};
	};

	return Count;
};
*/

extern int
ppohVIS_BASE_GetVoxelCountAllRoot(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NXYZ;
	int i;

	Count = 0;
	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:Count)
	for(i=0; i<NXYZ; i++) {
		Count += ppohVIS_BASE_GetVoxelCountAll(&pIniVoxel->Voxels[i]);
	};

	return Count;
};
