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
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelUtilSetValue.h"

#define EPS (1.0E-8)


/*******************************************************************************
 * Set value of voxel by node
 ******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelValueByNode(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	double X0, Y0, Z0, GX, GY, GZ, D, DSum, Val;
	int NDOF, Node, i, k;

	if(pVoxel == NULL) { return 0; };

	NDOF = pResult->FreedomCount;
	if(pVoxel->Value != NULL) {
		free(pVoxel->Value);
		pVoxel->Value = NULL;
	};
	pVoxel->Value = (double *)malloc(sizeof(double)*NDOF);
	for(k=0; k<NDOF; k++) {
		pVoxel->Value[k] = 0.0;
	};
	pVoxel->CoefD = 0.0;

	if(pVoxel->NodeCount <= 0 && pVoxel->Level > 1) {
		for(k=0; k<NDOF; k++) {
			pVoxel->Value[k] = pVoxel->Parent->Value[k];
		};

	} else if(pVoxel->NodeCount > 0) {
		DSum = 0.0;
		GX = pVoxel->OX + pVoxel->DX * 0.5;
		GY = pVoxel->OY + pVoxel->DY * 0.5;
		GZ = pVoxel->OZ + pVoxel->DZ * 0.5;

		for(i=0; i<pVoxel->NodeCount; i++) {
			Node = pVoxel->NodeID[i];

			X0 = pMesh->Node->Coords[3*Node-3];
			Y0 = pMesh->Node->Coords[3*Node-2];
			Z0 = pMesh->Node->Coords[3*Node-1];

			D = sqrt((GX-X0)*(GX-X0) + (GY-Y0)*(GY-Y0) + (GZ-Z0)*(GZ-Z0));
			if(D < EPS) {
				for(k=0; k<NDOF; k++) {
					Val = pResult->Value[NDOF*(Node-1)+k];
					pVoxel->Value[k] = Val;
				};
				DSum = 1.0;
				break;
			} else {
				DSum += 1.0 / D;
				for(k=0; k<NDOF; k++) {
					Val = pResult->Value[NDOF*(Node-1)+k];
					pVoxel->Value[k] += Val * 1.0 / D;
				};
			};
		};
		pVoxel->CoefD = DSum;
		for(k=0; k<NDOF; k++) {
			pVoxel->Value[k] = pVoxel->Value[k] / DSum;
		};
	};

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			if(ppohVIS_BASE_SetVoxelValueByNode(
				pVoxel->Children[i], pMesh, pResult)) {
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
ppohVIS_BASE_SetVoxelValuesByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NX, NY, NZ;
	int iX, iY, iZ, i, k;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		if(pIniVoxel->Voxels[i].Value != NULL) {
			free(pIniVoxel->Voxels[i].Value);
			pIniVoxel->Voxels[i].Value = NULL;
		}
		pIniVoxel->Voxels[i].Value = (double *)malloc(sizeof(double)*pResult->FreedomCount);
		for(k=0; k<pResult->FreedomCount; k++) {
			pIniVoxel->Voxels[i].Value[k] = 0.0;
		};
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(ppohVIS_BASE_SetVoxelValueByNode(
				&pIniVoxel->Voxels[i], pMesh, pResult)) {
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
extern int
ppohVIS_BASE_SetVoxelValuesByNode(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NXYZ;
	int i;
	int iRc = 0;

	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:iRc)
	for(i=0; i<NXYZ; i++) {
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(ppohVIS_BASE_SetVoxelValueByNode(
				&pIniVoxel->Voxels[i], pMesh, pResult)) {
				iRc++;
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

/*******************************************************************************
 * Set value of voxel by element
 ******************************************************************************/
extern int
ppohVIS_BASE_SetVoxelValueByElement(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	double X0, Y0, Z0, GX, GY, GZ, D, DSum, Val;
	int NDOF, Elem, i, k;

	if(pVoxel == NULL) { return 0; };

	NDOF = pResult->FreedomCount;
	if(pVoxel->Value != NULL) {
		free(pVoxel->Value);
		pVoxel->Value = NULL;
	};
	pVoxel->Value = (double *)malloc(sizeof(double)*NDOF);
	for(k=0; k<NDOF; k++) {
		pVoxel->Value[k] = 0.0;
	};
	pVoxel->CoefD = 0.0;

	if(pVoxel->ElementCount <= 0 && pVoxel->Level > 1) {
		for(k=0; k<NDOF; k++) {
			pVoxel->Value[k] = pVoxel->Parent->Value[k];
		};

	} else if(pVoxel->ElementCount > 0) {
		DSum = 0.0;
		GX = pVoxel->OX + pVoxel->DX * 0.5;
		GY = pVoxel->OY + pVoxel->DY * 0.5;
		GZ = pVoxel->OZ + pVoxel->DZ * 0.5;

		for(i=0; i<pVoxel->ElementCount; i++) {
			Elem = pVoxel->ElementID[i];

			X0 = pMesh->Element->Gravity[3*Elem-3];
			Y0 = pMesh->Element->Gravity[3*Elem-2];
			Z0 = pMesh->Element->Gravity[3*Elem-1];

			D = sqrt((GX-X0)*(GX-X0) + (GY-Y0)*(GY-Y0) + (GZ-Z0)*(GZ-Z0));
			if(D < EPS) {
				for(k=0; k<NDOF; k++) {
					Val = pResult->Value[NDOF*(Elem-1)+k];
					pVoxel->Value[k] = Val;
				};
				DSum = 1.0;
				break;
			} else {
				DSum += 1.0 / D;
				for(k=0; k<NDOF; k++) {
					Val = pResult->Value[NDOF*(Elem-1)+k];
					pVoxel->Value[k] += Val * 1.0 / D;
				};
			};
		};
		pVoxel->CoefD = DSum;
		for(k=0; k<NDOF; k++) {
			pVoxel->Value[k] = pVoxel->Value[k] / DSum;
		};
	};

	if(pVoxel->Children) {
		for(i=0; i<8; i++) {
			if(ppohVIS_BASE_SetVoxelValueByElement(
				pVoxel->Children[i], pMesh, pResult)) {
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
ppohVIS_BASE_SetVoxelValuesByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NX, NY, NZ;
	int iX, iY, iZ, i, k;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		if(pIniVoxel->Voxels[i].Value != NULL) {
			free(pIniVoxel->Voxels[i].Value);
			pIniVoxel->Voxels[i].Value = NULL;
		}
		pIniVoxel->Voxels[i].Value = (double *)malloc(sizeof(double)*pResult->FreedomCount);
		for(k=0; k<pResult->FreedomCount; k++) {
			pIniVoxel->Voxels[i].Value[k] = 0.0;
		};

		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(ppohVIS_BASE_SetVoxelValueByElement(
				&pIniVoxel->Voxels[i], pMesh, pResult)) {
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
extern int
ppohVIS_BASE_SetVoxelValuesByElement(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NXYZ;
	int i;
	int iRc = 0;

	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;

#pragma omp parallel for reduction(+:iRc)
	for(i=0; i<NXYZ; i++) {
		if(pIniVoxel->Voxels[i].Type != ppohVIS_BASE_VoxelOuter) {
			if(ppohVIS_BASE_SetVoxelValueByElement(
				&pIniVoxel->Voxels[i], pMesh, pResult)) {
				iRc++;
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
