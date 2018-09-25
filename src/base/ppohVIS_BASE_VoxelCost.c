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
#include <math.h>
#include <omp.h>

#include "ppohVIS_BASE_VoxelItem.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_VoxelCount.h"
#include "ppohVIS_BASE_VoxelCost.h"


static int
VoxelItemToArray(struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stVoxelItem **aIniVoxel, int *iCount)
{
	int i;

	if(pVoxel == NULL) { return 0; };

	aIniVoxel[*iCount] = pVoxel;
	(*iCount)++;

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			VoxelItemToArray(pVoxel->Children[i], aIniVoxel, iCount);
		};
	};

	return 0;
};

#if 0
static struct ppohVIS_BASE_stVoxelItem **
InitialVoxelToArray(struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int *iCount)
{
	struct ppohVIS_BASE_stVoxelItem **aIniVoxel;
	int iVoxelCount;
	int NX, NY, NZ;
	int iX, iY, iZ, i;

//	iVoxelCount = ppohVIS_BASE_GetVoxelCountWithBranch();
	iVoxelCount = ppohVIS_BASE_GetVoxelCountWithBranch(pIniVoxel);
	*iCount = 0;

	aIniVoxel = (struct ppohVIS_BASE_stVoxelItem **)malloc(sizeof(struct ppohVIS_BASE_stVoxelItem *)*iVoxelCount);

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(VoxelItemToArray(&pIniVoxel->Voxels[i], aIniVoxel, iCount)) {
				goto error;
			};
		};
	};
	};
	};

//	aIniVoxel = (struct ppohVIS_BASE_stVoxelItem **)realloc(aIniVoxel, *iCount);

	return aIniVoxel;

error:
	return NULL;
};
#endif

#if 0
static int
SetVoxelCostsByNode2(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	struct ppohVIS_BASE_stVoxelItem **aIniVoxel;
	struct ppohVIS_BASE_stVoxelItem *pVoxel;
	int iCount;
	int ip;
	int NDOF;
	double ValSquare, ValTmp, Val1, Val2, DVal;
	int Node1, Node2;
	int i, j, k;

	aIniVoxel = InitialVoxelToArray(pIniVoxel, &iCount);

	NDOF = pResult->FreedomCount;

//#pragma omp parallel for private(ValSquare,pVoxel,ip,i,j,k,Node1,Node2,Val1,Val2,DVal,ValTmp) schedule(static, 1)
#pragma omp parallel for private(ValSquare,pVoxel,ip,i,j,k,Node1,Node2,Val1,Val2,DVal,ValTmp)
	for(ip=0; ip<iCount; ip++) {
//fprintf(stderr, "%d/%d %d\n", omp_get_thread_num(), omp_get_num_threads(), ip);
		ValSquare = 0.0;
		pVoxel = aIniVoxel[ip];
		for(i=0; i<pVoxel->NodeCount-1; i++) {
			Node1 = pVoxel->NodeID[i];

			for(j=i+1; j<pVoxel->NodeCount; j++) {
				Node2 = pVoxel->NodeID[j];

				for(k=0; k<NDOF; k++) {
					Val1 = pResult->Value[NDOF*(Node1-1)+k];
					Val2 = pResult->Value[NDOF*(Node2-1)+k];
					DVal = Val1 - Val2;
					ValTmp = DVal * DVal;

					if(ValTmp > ValSquare) {
						ValSquare = ValTmp;
					};
				};
			};
		};
		pVoxel->Cost = ValSquare;
	};

	free(aIniVoxel);

	return 0;
};
#endif

/*******************************************************************************
 * Set cost of voxel by node
 ******************************************************************************/
static int
SetVoxelCostByNode(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NDOF;
	double ValSquare, ValSquareTmp, Val1, Val2, DVal;
	int Node1, Node2;
	int i, j, k;

	if(pVoxel == NULL) { return 0; };

	NDOF = pResult->FreedomCount;

	ValSquare = 0.0;
	for(i=0; i<pVoxel->NodeCount-1; i++) {
		Node1 = pVoxel->NodeID[i];

		for(j=i+1; j<pVoxel->NodeCount; j++) {
			Node2 = pVoxel->NodeID[j];

			ValSquareTmp = 0.0;
			for(k=0; k<NDOF; k++) {
				Val1 = pResult->Value[NDOF*(Node1-1)+k];
				Val2 = pResult->Value[NDOF*(Node2-1)+k];
				DVal = Val1 - Val2;
				ValSquareTmp = ValSquareTmp + DVal * DVal;
			};
			if(ValSquareTmp > ValSquare) {
				ValSquare = ValSquareTmp;
			};
		};
	};
	pVoxel->Cost = ValSquare;

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			if(SetVoxelCostByNode(pVoxel->Children[i], pResult)) {
				goto error;
			};
		};
	};

	return 0;

error:
	return -1;
};

/******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelCostByNode(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	return SetVoxelCostByNode(pVoxel, pResult);
};

/******************************************************************************/
static int
SetVoxelCostsByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
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
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(SetVoxelCostByNode(&pIniVoxel->Voxels[i], pResult)) {
				goto error;
			};
		};
	};
	};
	};

	return 0;

error:
	return -1;
};

#if 0
static int
SetVoxelCostsByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NXYZ;
	int i;
	int iRc = 0;

	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:iRc)
	for(i=0; i<NXYZ; i++) {
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(SetVoxelCostByNode(&pIniVoxel->Voxels[i], pResult)) {
				iRc++;
//				break;
			};
		};
	};
	if(iRc) {
		goto error;
	};

	return 0;

error:
	return -1;
};
#endif
/******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelCostsByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	return SetVoxelCostsByNode(pIniVoxel, pResult);
//	return SetVoxelCostsByNode2(pIniVoxel, pResult);
};


/*******************************************************************************
 * Set cost of Voxel by element
 ******************************************************************************/
static int
SetVoxelCostByElement(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NDOF;
	double ValSquare, ValSquareTmp, Val1, Val2, DVal;
	int Elem1, Elem2;
	int i, j, k;

	if(pVoxel == NULL) { return 0; };

	NDOF = pResult->FreedomCount;

	ValSquare = 0.0;
	for(i=0; i<pVoxel->ElementCount-1; i++) {
		Elem1 = pVoxel->ElementID[i];

		for(j=i+1; j<pVoxel->ElementCount; j++) {
			Elem2 = pVoxel->ElementID[j];

			ValSquareTmp = 0.0;
			for(k=0; k<NDOF; k++) {
				Val1 = pResult->Value[NDOF*(Elem1-1)+k];
				Val2 = pResult->Value[NDOF*(Elem2-1)+k];
				DVal = Val1 - Val2;
				ValSquareTmp = ValSquareTmp + DVal * DVal;
			};
			if(ValSquareTmp > ValSquare) {
				ValSquare = ValSquareTmp;
			};
		};
	};
	pVoxel->Cost = ValSquare;

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			if(SetVoxelCostByElement(
				pVoxel->Children[i], pResult)) {
				goto error;
			};
		};
	};

	return 0;

error:
	return -1;
};

/******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelCostByElement(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	return SetVoxelCostByElement(pVoxel, pResult);
};

/******************************************************************************/
static int
SetVoxelCostsByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
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
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(SetVoxelCostByElement(
				&pIniVoxel->Voxels[i], pResult)) {
				goto error;
			};
		};
	};
	};
	};

	return 0;

error:
	return -1;
};

#if 0
static int
SetVoxelCostsByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NXYZ;
	int i;
	int iRc = 0;

	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:iRc)
	for(i=0; i<NXYZ; i++) {
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(SetVoxelCostByElement(
				&pIniVoxel->Voxels[i], pResult)) {
				iRc++;
//				break;
			};
		};
	};
	if(iRc) {
		goto error;
	};

	return 0;

error:
	return -1;
};
#endif

/******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelCostsByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	return SetVoxelCostsByElement(pIniVoxel, pResult);
};


/*******************************************************************************
 * Calculate cost of voxel
 ******************************************************************************/
static struct ppohVIS_BASE_stVoxelItem *
SearchCostliestVoxelInner(
	struct ppohVIS_BASE_stVoxelItem *pVoxel, int MaxLevel,
	struct ppohVIS_BASE_stVoxelItem *pSearched)
{
	int i;

	if(pVoxel == NULL) { return pSearched; };
	if(pVoxel->Level >= MaxLevel) { return pSearched; };

	if(pVoxel->Children == NULL) {
		if(pSearched == NULL) {
			pSearched = pVoxel;
		} else {
			if(pVoxel->Cost > pSearched->Cost) {
				pSearched = pVoxel;
			};
		};
	} else {
		for(i=0; i<8; i++) {
			pSearched = SearchCostliestVoxelInner(
			              pVoxel->Children[i], MaxLevel, pSearched);
		};
	};

	return pSearched;
};

/******************************************************************************/
/*
static struct ppohVIS_BASE_stVoxelItem *
SearchCostliestVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int MaxLevel)
{
	int NX, NY, NZ;
	int iX, iY, iZ, i;
	struct ppohVIS_BASE_stVoxelItem *pSearched;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;
	pSearched = NULL;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			pSearched = SearchCostliestVoxelInner(
			            &pIniVoxel->Voxels[i], MaxLevel, pSearched);
		};
	};
	};
	};

	return pSearched;
};
*/

static struct ppohVIS_BASE_stVoxelItem *
SearchCostliestVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int MaxLevel)
{
	int NXYZ;
	int i;
	struct ppohVIS_BASE_stVoxelItem *pSearched;

	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;
	pSearched = NULL;
//	pSearchedTmp = NULL;

//#pragma omp parallel for private(i,pSearchedTmp)
	for(i=0; i<NXYZ; i++) {
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			pSearched = SearchCostliestVoxelInner(
			            &pIniVoxel->Voxels[i], MaxLevel, pSearched);
		};
	};

//#pragma omp critical
//	{
//		if(pSearched == NULL) {
//			pSearched = pSearchedTmp;
//		};
//		if(pSearchedTmp->Cost > pSearched->Cost) {
//			pSearched = pSearchedTmp;
//		};
//	};

	return pSearched;
};

/******************************************************************************/
extern struct ppohVIS_BASE_stVoxelItem *
ppohVIS_BASE_SearchCostliestVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int MaxLevel)
{
	return SearchCostliestVoxel(pIniVoxel, MaxLevel);
};

