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
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_UCD.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelCount.h"
#include "ppohVIS_BASE_VoxelValue.h"


/*******************************************************************************
 * Allocate member variables
 ******************************************************************************/
extern struct ppohVIS_BASE_stVoxelValueItem *
ppohVIS_BASE_AllocVoxelValueItem(void)
{
	struct ppohVIS_BASE_stVoxelValueItem *p = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelValueItem);
	p = (struct ppohVIS_BASE_stVoxelValueItem *)malloc(BufSize);
	if(p == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "voxel value info");
		return NULL;
	};

	p->OX = 0.0;
	p->OY = 0.0;
	p->OZ = 0.0;
	p->DX = 0.0;
	p->DY = 0.0;
	p->DZ = 0.0;
	p->Level = 0;
	p->IndexI = 0;
	p->IndexJ = 0;
	p->IndexK = 0;
	p->Value = NULL;
	p->Cost = 0.0;

	return p;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_AllocVoxelValue(void)
{
	struct ppohVIS_BASE_stVoxelValue *p = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelValue);
	p = (struct ppohVIS_BASE_stVoxelValue *)malloc(BufSize);
	if(p == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "voxel value info");
		return NULL;
	};

	p->VoxelCount = 0;
	p->ValueCount = 0;
	memset(p->ValueLabel, '\0', PPOHVIS_BASE_LABEL_LEN);
	p->Values = NULL;

	return p;
};


/*******************************************************************************
 * Free member variables
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeVoxelValueItem(struct ppohVIS_BASE_stVoxelValueItem *p)
{
	if(p == NULL) { return; };

	if(p->Value) {
		free(p->Value);
		p->Value = NULL;
	};

	free(p);
	p = NULL;

	return;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeVoxelValue(struct ppohVIS_BASE_stVoxelValue *p)
{
	int i;

	if(p == NULL) { return; };

	if(p->Values) {
		for(i=0; i<p->VoxelCount; i++) {
			ppohVIS_BASE_FreeVoxelValueItem(p->Values[i]);
		};

		free(p->Values);
		p->Values = NULL;
	};

	free(p);
	p = NULL;

	return;
};


/*******************************************************************************
 * Initialize member variables
 ******************************************************************************/
extern struct ppohVIS_BASE_stVoxelValueItem *
ppohVIS_BASE_InitVoxelValueItem(int ValueCount)
{
	struct ppohVIS_BASE_stVoxelValueItem *p = NULL;

	p = ppohVIS_BASE_AllocVoxelValueItem();
	if(p == NULL) { return NULL; };

	p->Value = (double *)malloc(sizeof(double)*ValueCount);
	if(p->Value == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "voxel value info");
		return NULL;
	};

	return p;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_InitVoxelValue(int VoxelCount, int ValueCount, char *ValueLabel)
{
	struct ppohVIS_BASE_stVoxelValue *p = NULL;
	int iLen, i;
	size_t BufSize;

	p = ppohVIS_BASE_AllocVoxelValue();
	if(p == NULL) { return NULL; };

	/* number of voxels */
	p->VoxelCount = VoxelCount;

	/* number of values */
	p->ValueCount = ValueCount;

	/* label of value */
	memset(p->ValueLabel, '\0', PPOHVIS_BASE_LABEL_LEN);
	iLen = strlen(ValueLabel);
	if(iLen >= PPOHVIS_BASE_LABEL_LEN) {
		iLen = PPOHVIS_BASE_LABEL_LEN - 1;
	};
	strncpy(p->ValueLabel, ValueLabel, iLen);

	/* value of each voxel */
	BufSize = sizeof(struct ppohVIS_BASE_stVoxelValueItem *) * VoxelCount;
	p->Values = (struct ppohVIS_BASE_stVoxelValueItem **)malloc(BufSize);
	if(p->Values == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "voxel value info");
		ppohVIS_BASE_FreeVoxelValue(p);
		return NULL;
	};

	for(i=0; i<VoxelCount; i++) {
		p->Values[i] = ppohVIS_BASE_InitVoxelValueItem(ValueCount);
		if(p->Values[i] == NULL) {
			ppohVIS_BASE_FreeVoxelValue(p);
			return NULL;
		};
	};

	return p;
};


/*******************************************************************************
 * Create voxel value information from initial voxel information
 ******************************************************************************/
static int
VoxelItemToVoxelValue(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	int ValueCount,
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue, int *iCount, int *iLevel,
	int IndexI, int IndexJ, int IndexK)
{
	struct ppohVIS_BASE_stVoxelValueItem *p = NULL;
	int i, j, iX, iY, iZ;

	if(pVoxel->Children == NULL) {
		p = pVoxelValue->Values[(*iCount)];
		p->OX = pVoxel->OX;
		p->OY = pVoxel->OY;
		p->OZ = pVoxel->OZ;
		p->DX = pVoxel->DX;
		p->DY = pVoxel->DY;
		p->DZ = pVoxel->DZ;
		p->Level = (*iLevel);
		p->IndexI = IndexI;
		p->IndexJ = IndexJ;
		p->IndexK = IndexK;
		p->Cost = pVoxel->Cost;
		p->Value = (double *)malloc(sizeof(double)*ValueCount);
		for(j=0; j<ValueCount; j++) {
			p->Value[j] = pVoxel->Value[j];
		};
		(*iCount)++;
	} else {
		(*iLevel)++;
		for(iZ=0; iZ<2; iZ++) {
		for(iY=0; iY<2; iY++) {
		for(iX=0; iX<2; iX++) {
			i = 4 * iZ + 2 * iY + iX;
			VoxelItemToVoxelValue(
				pVoxel->Children[i], ValueCount, pVoxelValue, iCount,
				iLevel, 2*IndexI+iX, 2*IndexJ+iY, 2*IndexK+iZ);
		};
		};
		};
		(*iLevel)--;
	};

	return 0;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_InitialVoxelToVoxelValue(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult)
{
	int NX, NY, NZ;
	int iX, iY, iZ, i, iCount, iLevel;
	int VoxelCount, ValueCount;
	char *ValueLabel;
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue = NULL;
	int MyRank;

	MyRank = ppohVIS_BASE_GetCommRank();

	/*======================================================================
	 * Check arguments
	 *====================================================================*/
	if(pIniVoxel == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "initial voxel information");
		return NULL;
	};

	if(pResult == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "result information");
		return NULL;
	};

	/*======================================================================
	 * 
	 *====================================================================*/
//	VoxelCount = ppohVIS_BASE_GetVoxelCount(pIniVoxel);
	VoxelCount = ppohVIS_BASE_GetVoxelCount2(pIniVoxel, MyRank);
	ValueCount = pResult->FreedomCount;
	ValueLabel = pResult->Label;

	pVoxelValue = ppohVIS_BASE_InitVoxelValue(VoxelCount, ValueCount, ValueLabel);
	if(pVoxelValue == NULL) {
		return NULL;
	};

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;
	iCount = 0;
	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = iZ * NX * NY + iY * NX + iX;
		iLevel = 1;
		if(pIniVoxel->Rank[i] == MyRank) {
			VoxelItemToVoxelValue(
				&pIniVoxel->Voxels[i], ValueCount, pVoxelValue, &iCount,
				&iLevel, iX, iY, iZ);
		};
	};
	};
	};

	return pVoxelValue;
};


/*******************************************************************************
 * Convert voxel value information to UCD
 ******************************************************************************/
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertVoxelValue2UCD(
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue)
{
	struct ppohVIS_BASE_stVoxelValueItem *p;
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	double OX, OY, OZ, DX, DY, DZ;
	int iNode, iElem, i, j, k;

	/* allocate */
	pUCDData = ppohVIS_BASE_AllocateUCD();
	if(pUCDData == NULL) {
		goto error;
	};
	pUCDData->Node = ppohVIS_BASE_AllocateUCDNode(pVoxelValue->VoxelCount*8);
	if(pUCDData->Node == NULL) {
		goto error;
	};
	pUCDData->Element = ppohVIS_BASE_AllocateUCDElement(pVoxelValue->VoxelCount);
	if(pUCDData->Element == NULL) {
		goto error;
	};
	pUCDData->NodeValue = ppohVIS_BASE_AllocateUCDValue(pVoxelValue->VoxelCount*8, 0, 0);
	if(pUCDData->NodeValue == NULL) {
		goto error;
	};
	pUCDData->ElementValue = ppohVIS_BASE_AllocateUCDValue(pVoxelValue->VoxelCount, pVoxelValue->ValueCount, 1);
	if(pUCDData->ElementValue == NULL) {
		goto error;
	};

	pUCDData->ElementValue->DOF[0] = pVoxelValue->ValueCount;
	strncpy(pUCDData->ElementValue->Label[0], pVoxelValue->ValueLabel, PPOHVIS_BASE_LABEL_LEN);

	for(i=0; i<pVoxelValue->VoxelCount; i++) {
		p = pVoxelValue->Values[i];

		OX = p->OX;
		OY = p->OY;
		OZ = p->OZ;
		DX = p->DX;
		DY = p->DY;
		DZ = p->DZ;

		iNode = 8 * i;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX;
		pUCDData->Node->Coords[3*iNode+1] = OY;
		pUCDData->Node->Coords[3*iNode+2] = OZ;

		iNode = 8 * i + 1;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX + DX;
		pUCDData->Node->Coords[3*iNode+1] = OY;
		pUCDData->Node->Coords[3*iNode+2] = OZ;

		iNode = 8 * i + 2;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX + DX;
		pUCDData->Node->Coords[3*iNode+1] = OY + DY;
		pUCDData->Node->Coords[3*iNode+2] = OZ;

		iNode = 8 * i + 3;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX;
		pUCDData->Node->Coords[3*iNode+1] = OY + DY;
		pUCDData->Node->Coords[3*iNode+2] = OZ;

		iNode = 8 * i + 4;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX;
		pUCDData->Node->Coords[3*iNode+1] = OY;
		pUCDData->Node->Coords[3*iNode+2] = OZ + DZ;

		iNode = 8 * i + 5;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX + DX;
		pUCDData->Node->Coords[3*iNode+1] = OY;
		pUCDData->Node->Coords[3*iNode+2] = OZ + DZ;

		iNode = 8 * i + 6;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX + DX;
		pUCDData->Node->Coords[3*iNode+1] = OY + DY;
		pUCDData->Node->Coords[3*iNode+2] = OZ + DZ;

		iNode = 8 * i + 7;
		pUCDData->Node->ID[iNode] = iNode + 1;
		pUCDData->Node->Coords[3*iNode  ] = OX;
		pUCDData->Node->Coords[3*iNode+1] = OY + DY;
		pUCDData->Node->Coords[3*iNode+2] = OZ + DZ;


		iElem = i;

		pUCDData->Element->ID[iElem] = iElem + 1;
		pUCDData->Element->MaterialID[iElem] = 1;
		pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Hexa8;
		pUCDData->Element->NodeIndex[iElem+1] = 8 * (iElem + 1);
		pUCDData->Element->Node[8*iElem  ] = 8 * iElem + 1;
		pUCDData->Element->Node[8*iElem+1] = 8 * iElem + 2;
		pUCDData->Element->Node[8*iElem+2] = 8 * iElem + 3;
		pUCDData->Element->Node[8*iElem+3] = 8 * iElem + 4;
		pUCDData->Element->Node[8*iElem+4] = 8 * iElem + 5;
		pUCDData->Element->Node[8*iElem+5] = 8 * iElem + 6;
		pUCDData->Element->Node[8*iElem+6] = 8 * iElem + 7;
		pUCDData->Element->Node[8*iElem+7] = 8 * iElem + 8;

		pUCDData->ElementValue->ID[iElem] = iElem + 1;
		for(j=0; j<pVoxelValue->ValueCount; j++) {
			k = pVoxelValue->ValueCount * iElem + j;
			pUCDData->ElementValue->Value[k] = p->Value[j];
		};
	};

	return pUCDData;

error:
	return NULL;
};


/*******************************************************************************
 * Convert voxel value information to UCD
 ******************************************************************************/
static int
CheckExistNode(int iNodeCount, int *NodeMask, int iX, int iY, int iZ)
{
	int i;

	for(i=0; i<iNodeCount; i++) {
		if(NodeMask[3*i] == iX) {
		if(NodeMask[3*i+1] == iY) {
		if(NodeMask[3*i+2] == iZ) {
			return i;
		};
		};
		};
	};

	return -1;
};


/******************************************************************************/
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertVoxelValue2UCD2(
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue)
{
	struct ppohVIS_BASE_stVoxelValueItem *p;
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	int *NodeMask = NULL;
	double OX, OY, OZ, DX, DY, DZ;
	int iElem, i, j, k;
	int iNodeCount, IndexI, IndexJ, IndexK, ii;
	int MaxLevel, iOffset;

	/* allocate */
	pUCDData = ppohVIS_BASE_AllocateUCD();
	if(pUCDData == NULL) {
		goto error;
	};
	pUCDData->Node = ppohVIS_BASE_AllocateUCDNode(pVoxelValue->VoxelCount*8);
	if(pUCDData->Node == NULL) {
		goto error;
	};
	pUCDData->Element = ppohVIS_BASE_AllocateUCDElement(pVoxelValue->VoxelCount);
	if(pUCDData->Element == NULL) {
		goto error;
	};
	pUCDData->NodeValue = ppohVIS_BASE_AllocateUCDValue(pVoxelValue->VoxelCount*8, 0, 0);
	if(pUCDData->NodeValue == NULL) {
		goto error;
	};
	pUCDData->ElementValue = ppohVIS_BASE_AllocateUCDValue(pVoxelValue->VoxelCount, pVoxelValue->ValueCount, 1);
	if(pUCDData->ElementValue == NULL) {
		goto error;
	};
	if(pUCDData == NULL) {
		goto error;
	};

	pUCDData->ElementValue->DOF[0] = pVoxelValue->ValueCount;
	strncpy(pUCDData->ElementValue->Label[0], pVoxelValue->ValueLabel, PPOHVIS_BASE_LABEL_LEN);

	MaxLevel = 1;
	for(i=0; i<pVoxelValue->VoxelCount; i++) {
		if(pVoxelValue->Values[i]->Level > MaxLevel) MaxLevel = pVoxelValue->Values[i]->Level;
	};

	NodeMask = (int *)calloc(sizeof(int), 24*pVoxelValue->VoxelCount);
	if(NodeMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node mask");
		goto error;
	}

	iNodeCount = 0;
	for(i=0; i<pVoxelValue->VoxelCount; i++) {
		p = pVoxelValue->Values[i];

		OX = p->OX;
		OY = p->OY;
		OZ = p->OZ;
		DX = p->DX;
		DY = p->DY;
		DZ = p->DZ;

		iOffset = pow(2, MaxLevel-p->Level);
		IndexI = p->IndexI * iOffset;
		IndexJ = p->IndexJ * iOffset;
		IndexK = p->IndexK * iOffset;
		iElem = i;

		/* Node-1 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI, IndexJ, IndexK);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ;

			NodeMask[3*iNodeCount  ] = IndexI;
			NodeMask[3*iNodeCount+1] = IndexJ;
			NodeMask[3*iNodeCount+2] = IndexK;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem] = ii + 1;
		};

		/* Node-2 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI+iOffset, IndexJ, IndexK);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+1] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX + DX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ;

			NodeMask[3*iNodeCount  ] = IndexI+iOffset;
			NodeMask[3*iNodeCount+1] = IndexJ;
			NodeMask[3*iNodeCount+2] = IndexK;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+1] = ii + 1;
		};

		/* Node-3 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI+iOffset, IndexJ+iOffset, IndexK);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+2] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX + DX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY + DY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ;

			NodeMask[3*iNodeCount  ] = IndexI+iOffset;
			NodeMask[3*iNodeCount+1] = IndexJ+iOffset;
			NodeMask[3*iNodeCount+2] = IndexK;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+2] = ii + 1;
		};

		/* Node-4 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI, IndexJ+iOffset, IndexK);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+3] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY + DY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ;

			NodeMask[3*iNodeCount  ] = IndexI;
			NodeMask[3*iNodeCount+1] = IndexJ+iOffset;
			NodeMask[3*iNodeCount+2] = IndexK;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+3] = ii + 1;
		};

		/* Node-5 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI, IndexJ, IndexK+iOffset);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+4] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ + DZ;

			NodeMask[3*iNodeCount  ] = IndexI;
			NodeMask[3*iNodeCount+1] = IndexJ;
			NodeMask[3*iNodeCount+2] = IndexK+iOffset;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+4] = ii + 1;
		};

		/* Node-6 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI+iOffset, IndexJ, IndexK+iOffset);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+5] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX + DX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ + DZ;

			NodeMask[3*iNodeCount  ] = IndexI+iOffset;
			NodeMask[3*iNodeCount+1] = IndexJ;
			NodeMask[3*iNodeCount+2] = IndexK+iOffset;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+5] = ii + 1;
		};

		/* Node-7 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI+iOffset, IndexJ+iOffset, IndexK+iOffset);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+6] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX + DX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY + DY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ + DZ;

			NodeMask[3*iNodeCount  ] = IndexI+iOffset;
			NodeMask[3*iNodeCount+1] = IndexJ+iOffset;
			NodeMask[3*iNodeCount+2] = IndexK+iOffset;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+6] = ii + 1;
		};

		/* Node-8 */
		ii = CheckExistNode(iNodeCount, NodeMask, IndexI, IndexJ+iOffset, IndexK+iOffset);
		if(ii < 0) {
			pUCDData->Element->Node[8*iElem+7] = iNodeCount + 1;

			pUCDData->Node->ID[iNodeCount] = iNodeCount + 1;
			pUCDData->Node->Coords[3*iNodeCount  ] = OX;
			pUCDData->Node->Coords[3*iNodeCount+1] = OY + DY;
			pUCDData->Node->Coords[3*iNodeCount+2] = OZ + DZ;

			NodeMask[3*iNodeCount  ] = IndexI;
			NodeMask[3*iNodeCount+1] = IndexJ+iOffset;
			NodeMask[3*iNodeCount+2] = IndexK+iOffset;

			iNodeCount++;
		} else {
			pUCDData->Element->Node[8*iElem+7] = ii + 1;
		};

		pUCDData->Element->ID[iElem] = iElem + 1;
		pUCDData->Element->MaterialID[iElem] = 1;
		pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Hexa8;
		pUCDData->Element->NodeIndex[iElem+1] = 8 * (iElem + 1);

		pUCDData->ElementValue->ID[iElem] = iElem + 1;
		for(j=0; j<pVoxelValue->ValueCount; j++) {
			k = pVoxelValue->ValueCount * iElem + j;
			pUCDData->ElementValue->Value[k] = p->Value[j];
		};
	};

	pUCDData->Node->Count = iNodeCount;
	pUCDData->NodeValue->ItemCount = iNodeCount;

	free(NodeMask);

	return pUCDData;

error:
	free(NodeMask);

	return NULL;
};


/*******************************************************************************
 * Print voxel value information
 ******************************************************************************/
extern int
ppohVIS_BASE_PrintVoxelValue(
	FILE *fp, struct ppohVIS_BASE_stVoxelValue *pVoxelValue)
{
	struct ppohVIS_BASE_stVoxelValueItem *p;
	int i, j;

	fprintf(fp, "%d\n", pVoxelValue->VoxelCount);
	fprintf(fp, "%d\n", pVoxelValue->ValueCount);
	fprintf(fp, "%s\n", pVoxelValue->ValueLabel);

	for(i=0; i<pVoxelValue->VoxelCount; i++) {
		p = pVoxelValue->Values[i];

		fprintf(fp, "%d", i+1);
		fprintf(fp, ",%15.7e,%15.7e,%15.7e", p->OX, p->OY, p->OZ);
		fprintf(fp, ",%15.7e,%15.7e,%15.7e", p->DX, p->DY, p->DZ);
		fprintf(fp, ",%15.7e", p->Cost);

		for(j=0; j<pVoxelValue->ValueCount; j++) {
			fprintf(fp, ",%15.7e", p->Value[j]);
		};

		fprintf(fp, "\n");
	};

	return 0;
};


/*============================================================================*/
extern int
ppohVIS_BASE_PutVoxelValue(
	char *FileName, struct ppohVIS_BASE_stVoxelValue *pVoxelValue)
{
	FILE *fp = NULL;
	int iMyRank;
	char cFileName[PPOHVIS_BASE_FILE_NAME_LEN];

	iMyRank = ppohVIS_BASE_GetCommRank();
	ppohVIS_BASE_GetDistFileName(FileName, iMyRank,
	                              cFileName, PPOHVIS_BASE_FILE_NAME_LEN);

	/* Open file */
	if((fp = fopen(cFileName, "w")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError,
			"%s [%s]", strerror(errno), "voxel value");
		goto error;
	};

	/* Print to a file */
	ppohVIS_BASE_PrintVoxelValue(fp, pVoxelValue);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "voxel value");
		goto error;
	};

	return 0;

error:
	return -1;
};


