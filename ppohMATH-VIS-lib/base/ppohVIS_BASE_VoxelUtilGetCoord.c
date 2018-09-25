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
/*******************************************************************************
 * Create list of center coordinate of voxel
 ******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <omp.h>

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_VoxelItem.h"
#include "ppohVIS_BASE_VoxelCount.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelUtilGetCoord.h"


/*******************************************************************************
 * All voxels (inner + boundary + outer)
 ******************************************************************************/
static int
GetVoxelCoordList(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMeshNode *pNode, int NumNode)
{
	int Node, i;

	if(pVoxel->Children == NULL) {
		NumNode++;

		Node = NumNode;
		pNode->ID[Node-1] = Node;
		pNode->Coords[3*Node-3] = pVoxel->OX + pVoxel->DX * 0.5;
		pNode->Coords[3*Node-2] = pVoxel->OY + pVoxel->DY * 0.5;
		pNode->Coords[3*Node-1] = pVoxel->OZ + pVoxel->DZ * 0.5;
	} else {
		for(i=0; i<8; i++) {
			NumNode = GetVoxelCoordList(
				pVoxel->Children[i], pNode, NumNode);
		};
	};

	return NumNode;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stMeshNode *
ppohVIS_BASE_GetVoxelCoordList(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	struct ppohVIS_BASE_stMeshNode *pNode = NULL;
	size_t BufSize;
	int NumNode;
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	NumNode = ppohVIS_BASE_GetVoxelCount(pIniVoxel);

	BufSize = sizeof(struct ppohVIS_BASE_stMeshNode);
	pNode = (struct ppohVIS_BASE_stMeshNode *)malloc(BufSize);
	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node info.");
		goto error;
	};
	pNode->Count = NumNode;
	pNode->ID = (int *)calloc(pNode->Count, sizeof(int));
	if(pNode->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto error;
	};
	pNode->Coords = (double *)malloc(sizeof(double)*pNode->Count*3);
	if(pNode->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "coordinate");
		goto error;
	};

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	NumNode = 0;
	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		NumNode = GetVoxelCoordList(&pIniVoxel->Voxels[i], pNode, NumNode);
	};
	};
	};

	if(NumNode != pNode->Count) {
		fprintf(stderr, "Assert: number of nodes\n");
	};

	return pNode;

error:
	return NULL;
};


/*******************************************************************************
 * Inner voxel only (inner)
 ******************************************************************************/
static int
GetInnerVoxelCoordList(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMeshNode *pNode, int NumNode)
{
	int Node, i;

	if(pVoxel->Children == NULL) {
		if(pVoxel->Type == ppohVIS_BASE_VoxelInner) {
			NumNode++;

			Node = NumNode;
			pNode->ID[Node-1] = Node;
			pNode->Coords[3*Node-3] = pVoxel->OX + pVoxel->DX * 0.5;
			pNode->Coords[3*Node-2] = pVoxel->OY + pVoxel->DY * 0.5;
			pNode->Coords[3*Node-1] = pVoxel->OZ + pVoxel->DZ * 0.5;
		};
	} else {
		for(i=0; i<8; i++) {
			NumNode = GetInnerVoxelCoordList(pVoxel->Children[i], pNode, NumNode);
		};
	};

	return NumNode;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stMeshNode *
ppohVIS_BASE_GetInnerVoxelCoordList(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel)
{
	struct ppohVIS_BASE_stMeshNode *pNode = NULL;
	size_t BufSize;
	int NumNode;
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	NumNode = ppohVIS_BASE_GetInnerVoxelCount(pIniVoxel);
	BufSize = sizeof(struct ppohVIS_BASE_stMeshNode);
	pNode = (struct ppohVIS_BASE_stMeshNode *)malloc(BufSize);
	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node info.");
		goto error;
	};
	pNode->Count = NumNode;
	pNode->ID = (int *)calloc(pNode->Count, sizeof(int));
	if(pNode->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto error;
	};
	pNode->Coords = (double *)malloc(sizeof(double)*pNode->Count*3);
	if(pNode->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "coordinate");
		goto error;
	};

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	NumNode = 0;
	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		NumNode = GetInnerVoxelCoordList(&pIniVoxel->Voxels[i], pNode, NumNode);
	};
	};
	};

	if(NumNode != pNode->Count) {
		fprintf(stderr, "Assert: number of nodes\n");
	};

	return pNode;

error:
	return NULL;
};


/*******************************************************************************
 * Inner voxel only (inner) & from ppohVIS_BASE_stVoxelValue
 ******************************************************************************/
extern int
ppohVIS_BASE_GetInnerVoxelCoordListFromVoxelValue(
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue,
	struct ppohVIS_BASE_stMeshNode **pVoxelNode,
	struct ppohVIS_BASE_stResult **pVoxelResult)
{
	struct ppohVIS_BASE_stMeshNode *pNode = NULL;
	struct ppohVIS_BASE_stResult *pResult = NULL;
	int nDOF;
	int i, j, iRc;

	pNode = ppohVIS_BASE_AllocateMeshNode();
	if(pNode == NULL) {
		goto error;
	};
	iRc = ppohVIS_BASE_InitMeshNode(pNode, pVoxelValue->VoxelCount);
	if(iRc != 0) {
		goto error;
	};
	for(i=0; i<pVoxelValue->VoxelCount; i++) {
		pNode->ID[i]         = i+1;
		pNode->Rank[i]       = 0;
		pNode->Coords[3*i]   = pVoxelValue->Values[i]->OX + pVoxelValue->Values[i]->DX * 0.5;
		pNode->Coords[3*i+1] = pVoxelValue->Values[i]->OY + pVoxelValue->Values[i]->DY * 0.5;
		pNode->Coords[3*i+2] = pVoxelValue->Values[i]->OZ + pVoxelValue->Values[i]->DZ * 0.5;
	};

	pResult = ppohVIS_BASE_AllocateResult();
	if(pResult == NULL) {
		goto error;
	};
	iRc = ppohVIS_BASE_InitResult(
		pResult, pVoxelValue->VoxelCount,
		ppohVIS_BASE_ResultElement, pVoxelValue->ValueCount);
	if(iRc != 0) {
		goto error;
	};
	strcpy(pResult->Label, pVoxelValue->ValueLabel);
	nDOF = pVoxelValue->ValueCount;
	for(i=0; i<pVoxelValue->VoxelCount; i++) {
		for(j=0; j<nDOF; j++) {
			pResult->Value[nDOF*i+j] = pVoxelValue->Values[i]->Value[j];
		};
	};

	*pVoxelNode = pNode;
	*pVoxelResult = pResult;

	return 0;

error:
	return -1;
};
