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
#include <omp.h>

#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelItem.h"
#include "ppohVIS_BASE_VoxelCount.h"


/*******************************************************************************
 * All voxels (Inner + Boundary + Outer)
 ******************************************************************************/
static int
GetVoxelCount(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int Count, i;

	Count = 0;

	if(pVoxel->Children == NULL) {
		Count = 1;
	} else {
		for(i=0; i<8; i++) {
			Count += GetVoxelCount(pVoxel->Children[i]);
		};
	};

	return Count;
};

/******************************************************************************/
/*
extern int
ppohVIS_BASE_GetVoxelCount(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NX, NXY;
	int iX, iY, iZ, i;

	Count = 0;
	NX = pIniVoxel->NX;
	NXY = pIniVoxel->NX * pIniVoxel->NY;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = iZ * NXY + iY * NX + iX;
		Count += GetVoxelCount(&pIniVoxel->Voxels[i]);
	};
	};
	};

	return Count;
};
*/

extern int
ppohVIS_BASE_GetVoxelCount(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NXYZ;
	int i;

	Count = 0;
	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:Count)
	for(i=0; i<NXYZ; i++) {
		Count += GetVoxelCount(&pIniVoxel->Voxels[i]);
	};

	return Count;
};


/*
extern int
ppohVIS_BASE_GetVoxelCount2(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int MyRank)
{
	int Count;
	int NX, NXY;
	int iX, iY, iZ, i;

	Count = 0;
	NX = pIniVoxel->NX;
	NXY = pIniVoxel->NX * pIniVoxel->NY;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = iZ * NXY + iY * NX + iX;
		if(pIniVoxel->Rank[i] == MyRank) {
			Count += GetVoxelCount(&pIniVoxel->Voxels[i]);
		};
	};
	};
	};

	return Count;
};
*/

extern int
ppohVIS_BASE_GetVoxelCount2(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int MyRank)
{
	int Count;
	int NXYZ;
	int i;

	Count = 0;
	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:Count)
	for(i=0; i<NXYZ; i++) {
		if(pIniVoxel->Rank[i] == MyRank) {
			Count += GetVoxelCount(&pIniVoxel->Voxels[i]);
		};
	};

	return Count;
};


/*******************************************************************************
 * Without outer voxels (Inner + Boundary)
 ******************************************************************************/
static int
GetVoxelCountWithoutOuter(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int Count, i;

	Count = 0;

	if(pVoxel->Children == NULL) {
		if(pVoxel->Type == ppohVIS_BASE_VoxelInner ||
		   pVoxel->Type == ppohVIS_BASE_VoxelBoundary) {
			Count = 1;
		};
	} else {
		for(i=0; i<8; i++) {
			Count += GetVoxelCountWithoutOuter(pVoxel->Children[i]);
		};
	};

	return Count;
};

/******************************************************************************/
/*
extern int
ppohVIS_BASE_GetVoxelCountWithoutOuter(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NX, NXY;
	int iX, iY, iZ, i;

	Count = 0;
	NX = pIniVoxel->NX;
	NXY = pIniVoxel->NX * pIniVoxel->NY;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = iZ * NXY + iY * NX + iX;
		Count += GetVoxelCountWithoutOuter(&pIniVoxel->Voxels[i]);
	};
	};
	};

	return Count;
};
*/

extern int
ppohVIS_BASE_GetVoxelCountWithoutOuter(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NXYZ;
	int i;

	Count = 0;
	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:Count)
	for(i=0; i<NXYZ; i++) {
		Count += GetVoxelCountWithoutOuter(&pIniVoxel->Voxels[i]);
	};

	return Count;
};


/*******************************************************************************
 * Inner only (Inner)
 ******************************************************************************/
static int
GetInnerVoxelCount(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int Count, i;

	Count = 0;

	if(pVoxel->Children == NULL) {
		if(pVoxel->Type == ppohVIS_BASE_VoxelInner) {
			Count = 1;
		};
	} else {
		for(i=0; i<8; i++) {
			Count += GetInnerVoxelCount(pVoxel->Children[i]);
		};
	};

	return Count;
};

/******************************************************************************/
/*
extern int
ppohVIS_BASE_GetInnerVoxelCount(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NX, NXY;
	int iX, iY, iZ, i;

	Count = 0;
	NX = pIniVoxel->NX;
	NXY = pIniVoxel->NX * pIniVoxel->NY;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = iZ * NXY + iY * NX + iX;
		Count += GetInnerVoxelCount(&pIniVoxel->Voxels[i]);
	};
	};
	};

	return Count;
};
*/

extern int
ppohVIS_BASE_GetInnerVoxelCount(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NXYZ;
	int i;

	Count = 0;
	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:Count)
	for(i=0; i<NXYZ; i++) {
		Count += GetInnerVoxelCount(&pIniVoxel->Voxels[i]);
	};

	return Count;
};


/*******************************************************************************
 * With branch (Inner + Boundary + Outer + Parent)
 ******************************************************************************/
static int
GetVoxelCountWithBranch(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int Count, i;

	Count = 1; /* this voxel */

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			Count += GetVoxelCountWithBranch(pVoxel->Children[i]);
		};
	};

	return Count;
};

/******************************************************************************/
/*
extern int
ppohVIS_BASE_GetVoxelCountWithBranch(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	Count = 0;
	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		Count += GetVoxelCountWithBranch(&pIniVoxel->Voxels[i]);
	};
	};
	};

	return Count;
};
*/

extern int
ppohVIS_BASE_GetVoxelCountWithBranch(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int Count;
	int NXYZ;
	int i;

	Count = 0;
	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:Count)
	for(i=0; i<NXYZ; i++) {
		Count += GetVoxelCountWithBranch(&pIniVoxel->Voxels[i]);
	};

	return Count;
};


/******************************************************************************/
static int
GetVoxelMaxLevel(
	struct ppohVIS_BASE_stVoxelItem *pVoxel, int *MaxLevel)
{
	int i;

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			GetVoxelMaxLevel(pVoxel->Children[i], MaxLevel);
		};
	} else {
		if(pVoxel->Level > (*MaxLevel)) {
			(*MaxLevel) = pVoxel->Level;
		};
	};

	return 0;
};

extern int
ppohVIS_BASE_GetVoxelMaxLevel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int MaxLevel;
	int NX, NY;
	int iX, iY, iZ, i;

	MaxLevel = 1;
	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;

	for(iZ=0; iZ<pIniVoxel->NZ; iZ++) {
	for(iY=0; iY<pIniVoxel->NY; iY++) {
	for(iX=0; iX<pIniVoxel->NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		GetVoxelMaxLevel(&pIniVoxel->Voxels[i], &MaxLevel);
	};
	};
	};

	return MaxLevel;
};


