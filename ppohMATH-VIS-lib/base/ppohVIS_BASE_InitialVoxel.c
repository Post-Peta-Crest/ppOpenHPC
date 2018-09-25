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
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_VoxelItem.h"
#include "ppohVIS_BASE_InitialVoxel.h"

#define MINIMUM_VOXEL_COUNT 4
//#define EXPAND_FACTOR (0.5)
#define EXPAND_FACTOR (0.0)
#define INITIAL_FACTOR (100.0)


/*******************************************************************************
 * Allocate
 ******************************************************************************/
extern struct ppohVIS_BASE_stInitialVoxel *
ppohVIS_BASE_AllocateInitialVoxel(void)
{
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stInitialVoxel);
	pIniVoxel = (struct ppohVIS_BASE_stInitialVoxel *)malloc(BufSize);
	if(pIniVoxel == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "initial voxel");
		return NULL;
	};

	pIniVoxel->OX = 0.0;
	pIniVoxel->OY = 0.0;
	pIniVoxel->OZ = 0.0;
	pIniVoxel->DX = 0.0;
	pIniVoxel->DY = 0.0;
	pIniVoxel->DZ = 0.0;
	pIniVoxel->NX = 0;
	pIniVoxel->NY = 0;
	pIniVoxel->NZ = 0;
	pIniVoxel->Voxels = NULL;

	return pIniVoxel;
};

/******************************************************************************/
extern void
ppohVIS_BASE_FreeInitialVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	if(pIniVoxel == NULL) { return; }

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	if(pIniVoxel->Voxels) {
		for(iZ=0; iZ<NZ; iZ++) {
		for(iY=0; iY<NY; iY++) {
		for(iX=0; iX<NX; iX++) {
			i = NX * NY * iZ + NX * iY + iX;
			//ppohVIS_BASE_FreeVoxelItem(&pIniVoxel->Voxels[i]);
			ppohVIS_BASE_FreeVoxelItems(&pIniVoxel->Voxels[i]);
		};
		};
		};
		free(pIniVoxel->Voxels);
		pIniVoxel->Voxels = NULL;
	};

	free(pIniVoxel);
	pIniVoxel = NULL;
};


/*******************************************************************************
 * Initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_InitVoxelItems(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	double OX, OY, OZ, DX, DY, DZ;
	int NX, NY, NZ, NXY, NXYZ;
	int i, iXY, iX, iY, iZ;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;
	DX = pIniVoxel->DX;
	DY = pIniVoxel->DY;
	DZ = pIniVoxel->DZ;
	OX = pIniVoxel->OX;
	OY = pIniVoxel->OY;
	OZ = pIniVoxel->OZ;

	pIniVoxel->Rank = (int *)calloc(NX*NY*NZ, sizeof(int));
	if(pIniVoxel->Rank == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "initial voxels");
		return NULL;
	};

//	NXY = NX * NY;
//	NXYZ = NX * NY * NZ;

//#pragma omp parallel for private(iXY,iX,iY,iZ)
//	for(i=0; i<NXYZ; i++) {
//		iXY = i % NXY;
//		iZ  = (i - iXY) / NXY;
//		iX  = iXY % NX;
//		iY  = (iXY - iX) / NX;
	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;

		pIniVoxel->Voxels[i].Level = 1;
		pIniVoxel->Voxels[i].IndexX = iX;
		pIniVoxel->Voxels[i].IndexY = iY;
		pIniVoxel->Voxels[i].IndexZ = iZ;

		pIniVoxel->Voxels[i].Type = ppohVIS_BASE_VoxelUnknown;

		pIniVoxel->Voxels[i].OX = OX + DX * (double)(iX);
		pIniVoxel->Voxels[i].OY = OY + DY * (double)(iY);
		pIniVoxel->Voxels[i].OZ = OZ + DZ * (double)(iZ);
		pIniVoxel->Voxels[i].DX = DX;
		pIniVoxel->Voxels[i].DY = DY;
		pIniVoxel->Voxels[i].DZ = DZ;

		pIniVoxel->Voxels[i].NodeCount = 0;
		pIniVoxel->Voxels[i].NodeID = NULL;
		pIniVoxel->Voxels[i].ElementCount = 0;
		pIniVoxel->Voxels[i].ElementID = NULL;
		pIniVoxel->Voxels[i].FreeSurfaceCount = 0;
		pIniVoxel->Voxels[i].FreeSurfaceID = NULL;

		pIniVoxel->Voxels[i].Value = NULL;
		pIniVoxel->Voxels[i].Cost = 0.0;

		pIniVoxel->Voxels[i].Parent = NULL;
		pIniVoxel->Voxels[i].Children = NULL;
	};
	};
	};

	return 0;
};


/*******************************************************************************
 * Copy
 ******************************************************************************/
extern struct ppohVIS_BASE_stInitialVoxel *
ppohVIS_BASE_CopyInitialVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxelSrc)
{
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxelDst = NULL;
	int NX, NY, NZ;
	int iRc, i;
	size_t BufSize;

	/* allocate */
	pIniVoxelDst = ppohVIS_BASE_AllocateInitialVoxel();
	if(pIniVoxelDst == NULL) {
		goto Error;
	};

	/* set region */
	pIniVoxelDst->OX = pIniVoxelSrc->OX;
	pIniVoxelDst->OY = pIniVoxelSrc->OY;
	pIniVoxelDst->OZ = pIniVoxelSrc->OZ;
	pIniVoxelDst->DX = pIniVoxelSrc->DX;
	pIniVoxelDst->DY = pIniVoxelSrc->DY;
	pIniVoxelDst->DZ = pIniVoxelSrc->DZ;
	pIniVoxelDst->NX = pIniVoxelSrc->NX;
	pIniVoxelDst->NY = pIniVoxelSrc->NY;
	pIniVoxelDst->NZ = pIniVoxelSrc->NZ;

	NX = pIniVoxelDst->NX;
	NY = pIniVoxelDst->NY;
	NZ = pIniVoxelDst->NZ;

	pIniVoxelDst->Rank = (int *)calloc(NX*NY*NZ, sizeof(int));
	if(pIniVoxelDst->Rank == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "initial voxels");
		return NULL;
	};
	if(pIniVoxelSrc->Rank) {
		for(i=0; i<NX*NY*NZ; i++) {
			pIniVoxelDst->Rank[i] = pIniVoxelSrc->Rank[i];
		};
	};

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem)*NX*NY*NZ;
	pIniVoxelDst->Voxels = (struct ppohVIS_BASE_stVoxelItem *)malloc(BufSize);
	if(pIniVoxelDst->Voxels == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "initial voxels");
		return NULL;
	};

	iRc = ppohVIS_BASE_InitVoxelItems(pIniVoxelDst);
	if(iRc != 0) goto Error;

	return pIniVoxelDst;

Error:
	if(pIniVoxelDst != NULL) {
		ppohVIS_BASE_FreeInitialVoxel(pIniVoxelDst);
		pIniVoxelDst = NULL;
	};
	return NULL;
};


/*******************************************************************************
 * Print voxel information
 ******************************************************************************/
static void
PrintVoxel(FILE *fp, struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int i;

	fprintf(fp, "Level:%3d, Type:%3d\n", pVoxel->Level, pVoxel->Type);
	fprintf(fp, "   IX:%15d, IY:%15d, IZ:%15d\n",
		pVoxel->IndexX, pVoxel->IndexY, pVoxel->IndexZ);
	fprintf(fp, "   OX:%15.7e, OY:%15.7e, OZ:%15.7e\n", pVoxel->OX, pVoxel->OY, pVoxel->OZ);
	fprintf(fp, "   DX:%15.7e, DY:%15.7e, DZ:%15.7e\n", pVoxel->DX, pVoxel->DY, pVoxel->DZ);
/*
	fprintf(fp, "   Node:%10d\n", pVoxel->NodeCount);
	for(i=0; i<pVoxel->NodeCount; i++) {
		fprintf(fp, " %d", pVoxel->NodeID[i]);
	};
	fprintf(fp, "\n");

	fprintf(fp, "   Element:%10d\n", pVoxel->ElementCount);
	for(i=0; i<pVoxel->ElementCount; i++) {
		fprintf(fp, " %d", pVoxel->ElementID[i]);
	};
	fprintf(fp, "\n");

	fprintf(fp, "   FreeSurface:%10d\n", pVoxel->FreeSurfaceCount);
	for(i=0; i<pVoxel->FreeSurfaceCount; i++) {
		fprintf(fp, " %d", pVoxel->FreeSurfaceID[i]);
	};
	fprintf(fp, "\n");
*/
	fprintf(fp, "   Node:%10d, Element:%10d, Surface:%10d\n",
		pVoxel->NodeCount, pVoxel->ElementCount, pVoxel->FreeSurfaceCount);
/*
	fprintf(fp, "   Children:");
	if(pVoxel->Children == NULL) {
		fprintf(fp, "NULL\n");
	} else {
		fprintf(fp, "\n");
		for(i=0; i<8; i++) {
			PrintVoxel(fp, pVoxel->Children[i]);
		};
	};
*/
};


/*============================================================================*/
extern void
ppohVIS_BASE_PrintInitialVoxel(
	FILE *fp, struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	int iX, iY, iZ, i;
	int NX, NY, NZ;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	fprintf(fp, "Origin:%15.7e%15.7e%15.7e\n",
		pIniVoxel->OX, pIniVoxel->OY, pIniVoxel->OZ);
	fprintf(fp, "Delta: %15.7e%15.7e%15.7e\n",
		pIniVoxel->DX, pIniVoxel->DY, pIniVoxel->DZ);
	fprintf(fp, "Count: %10d%10d%10d\n",
		pIniVoxel->NX, pIniVoxel->NY, pIniVoxel->NZ);

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		fprintf(fp, "%5d%5d%5d\n", iX, iY, iZ);

		PrintVoxel(fp, &pIniVoxel->Voxels[i]);
	};
	};
	};
};


/*============================================================================*/
extern int
ppohVIS_BASE_PutInitialVoxel(
	char *FileName, struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	int Append)
{
	FILE *fp = NULL;
	int iMyRank;
	char cFileName[PPOHVIS_BASE_FILE_NAME_LEN];

	iMyRank = ppohVIS_BASE_GetCommRank();
	ppohVIS_BASE_GetDistFileName(FileName, iMyRank,
				cFileName, PPOHVIS_BASE_FILE_NAME_LEN);

	/* Open file */
	if(Append == 0) {
		if((fp = fopen(cFileName, "w")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "initial voxel");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "initial voxel");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintInitialVoxel(fp, pIniVoxel);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "initial voxel");
		goto error;
	};

	return 0;

error:
	return -1;
};

