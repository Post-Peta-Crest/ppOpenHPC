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
#include <errno.h>

#include "ppohVIS_BASE_mpi.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_VoxelValue.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_ParallelVoxel.h"


/*******************************************************************************
 * Set maximum cost and location
 ******************************************************************************/
extern int
ppohVIS_BASE_GetMaxCostRank(
	struct ppohVIS_BASE_stVoxelItem *pRefineVoxel,
	ppohVIS_BASE_Comm comm)
{
	struct {
		double dVal;
		int iRank;
	} oSend[1], oRecv[1];
	int iMyRank;

#ifndef SERIAL
	MPI_Comm_rank(comm, &iMyRank);

	if(pRefineVoxel) {
		oSend[0].dVal = pRefineVoxel->Cost;
	} else {
		oSend[0].dVal = -1.0;
	};
	oSend[0].iRank = iMyRank;

	MPI_Allreduce(oSend, oRecv, 1, MPI_DOUBLE_INT, MPI_MAXLOC, comm);

#else
	iMyRank = 0;

	if(pRefineVoxel) {
		oRecv[0].dVal = pRefineVoxel->Cost;
	} else {
		oRecv[0].dVal = -1.0;
	};
	oRecv[0].iRank = iMyRank;
#endif

	if(oRecv[0].dVal < 0.0) {
		return -1;
	} else {
		return oRecv[0].iRank;
	};
};


/*******************************************************************************
 * Gather voxel count
 ******************************************************************************/
extern int
ppohVIS_BASE_ReduceVoxelCount(
	int iVoxelCountLocal,
	ppohVIS_BASE_Comm comm,
	int *iVoxelCountGlobal)
{
	int iSend, iRecv;
	int iRc;

#ifndef SERIAL
	iSend = iVoxelCountLocal;
	iRc = MPI_Allreduce(&iSend, &iRecv, 1, MPI_INT, MPI_SUM, comm);
	if(iRc != 0) return 0;
	*iVoxelCountGlobal = iRecv;
#else
	*iVoxelCountGlobal = iVoxelCountLocal;
#endif

	return *iVoxelCountGlobal;
};


extern int
ppohVIS_BASE_ReduceVoxelValueInt(
	struct ppohVIS_BASE_stVoxelValue *pValueLocal,
	ppohVIS_BASE_Comm comm,
	struct ppohVIS_BASE_stVoxelValue *pValueGlobal)
{
#ifndef SERIAL
	int iMyRank, iProcCount;
	int *iRecvCount, *iRecvIndex = NULL;
	double *iSend = NULL, *iRecv = NULL;
	int iItemCount, iSendCount, iBufCount;
	int iRc, i;

	iMyRank = ppohVIS_BASE_GetCommRank();
	iProcCount = ppohVIS_BASE_GetCommSize();
	iItemCount = pValueLocal->ValueCount + 4;

	/* index of receive count */
	iSendCount = pValueLocal->VoxelCount * iItemCount;

	if(ppohVIS_BASE_GetCommRank() == 0) {
		iRecvCount = (int *)calloc(iProcCount, sizeof(int));
		iRecvIndex = (int *)calloc(iProcCount+1, sizeof(int));
	};

	iRc = MPI_Gather(&iSendCount, 1, MPI_INT,
	                 iRecvCount, 1, MPI_INT, 0, comm);
	if(iRc != MPI_SUCCESS) return -1;

	if(iMyRank == 0) {
		iRecvIndex[0] = 0;
		for(i=0; i<iProcCount; i++) {
			iRecvIndex[i+1] = iRecvIndex[i] + iRecvCount[i];
		};
	};

	/* value */
	iSendCount = pValueLocal->VoxelCount * iItemCount;
	iSend = (double *)malloc(sizeof(double) * iSendCount);
		if(iSend == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "send buffer");
		return -1;
	};
	for(i=0; i<pValueLocal->VoxelCount; i++) {
		iSend[i*iItemCount  ] = pValueLocal->Values[i]->Level;
		iSend[i*iItemCount+1] = pValueLocal->Values[i]->IndexI;
		iSend[i*iItemCount+2] = pValueLocal->Values[i]->IndexJ;
		iSend[i*iItemCount+3] = pValueLocal->Values[i]->IndexK;
	};

	if(ppohVIS_BASE_GetCommRank() == 0) {
		iBufCount = pValueGlobal->VoxelCount * iItemCount;
		iRecv = (double *)malloc(sizeof(double) * iBufCount);
		if(iRecv == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve buffer");
			return -1;
		};
	};

	iRc = MPI_Gatherv(iSend, iSendCount, MPI_DOUBLE,
	                  iRecv, iRecvCount, iRecvIndex, MPI_DOUBLE, 0, comm);
	if(iRc != MPI_SUCCESS) return -1;

	if(ppohVIS_BASE_GetCommRank() == 0) {
		for(i=0; i<pValueGlobal->VoxelCount; i++) {
			pValueGlobal->Values[i]->Level  = iRecv[i*iItemCount  ];
			pValueGlobal->Values[i]->IndexI = iRecv[i*iItemCount+1];
			pValueGlobal->Values[i]->IndexJ = iRecv[i*iItemCount+2];
			pValueGlobal->Values[i]->IndexK = iRecv[i*iItemCount+3];
		};
	};
#endif
	return 0;
};


extern int
ppohVIS_BASE_ReduceVoxelValueDouble(
	struct ppohVIS_BASE_stVoxelValue *pValueLocal,
	ppohVIS_BASE_Comm comm,
	struct ppohVIS_BASE_stVoxelValue *pValueGlobal)
{
#ifndef SERIAL
	int iMyRank, iProcCount;
	int *iRecvCount, *iRecvIndex = NULL;
	double *dSend = NULL, *dRecv = NULL;
	int iItemCount, iSendCount, iBufCount;
	int iRc, i, j;

	iMyRank = ppohVIS_BASE_GetCommRank();
	iProcCount = ppohVIS_BASE_GetCommSize();
	iItemCount = pValueLocal->ValueCount + 7;

	/* index of receive count */
	iSendCount = pValueLocal->VoxelCount * iItemCount;

	if(ppohVIS_BASE_GetCommRank() == 0) {
		iRecvCount = (int *)calloc(iProcCount, sizeof(int));
		iRecvIndex = (int *)calloc(iProcCount+1, sizeof(int));
	};

	iRc = MPI_Gather(&iSendCount, 1, MPI_INT,
	                 iRecvCount, 1, MPI_INT, 0, comm);
	if(iRc != MPI_SUCCESS) return -1;

	if(iMyRank == 0) {
		iRecvIndex[0] = 0;
		for(i=0; i<iProcCount; i++) {
			iRecvIndex[i+1] = iRecvIndex[i] + iRecvCount[i];
		};
	};

	/* value */
	iSendCount = pValueLocal->VoxelCount * iItemCount;
	dSend = (double *)malloc(sizeof(double) * iSendCount);
	if(dSend == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "send buffer");
		return -1;
	};
	for(i=0; i<pValueLocal->VoxelCount; i++) {
		dSend[i*iItemCount  ] = pValueLocal->Values[i]->OX;
		dSend[i*iItemCount+1] = pValueLocal->Values[i]->OY;
		dSend[i*iItemCount+2] = pValueLocal->Values[i]->OZ;
		dSend[i*iItemCount+3] = pValueLocal->Values[i]->DX;
		dSend[i*iItemCount+4] = pValueLocal->Values[i]->DY;
		dSend[i*iItemCount+5] = pValueLocal->Values[i]->DZ;
		dSend[i*iItemCount+6] = pValueLocal->Values[i]->Cost;
		for(j=0; j<pValueLocal->ValueCount; j++) {
			dSend[i*iItemCount+7+j] = pValueLocal->Values[i]->Value[j];
		};
	};

	if(ppohVIS_BASE_GetCommRank() == 0) {
		iBufCount = pValueGlobal->VoxelCount * iItemCount;
		dRecv = (double *)malloc(sizeof(double) * iBufCount);
		if(dRecv == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve buffer");
			return -1;
		};
	};

	iRc = MPI_Gatherv(dSend, iSendCount, MPI_DOUBLE,
	                  dRecv, iRecvCount, iRecvIndex, MPI_DOUBLE, 0, comm);
	if(iRc != MPI_SUCCESS) return -1;

	if(ppohVIS_BASE_GetCommRank() == 0) {
		for(i=0; i<pValueGlobal->VoxelCount; i++) {
			pValueGlobal->Values[i]->OX   = dRecv[i*iItemCount  ];
			pValueGlobal->Values[i]->OY   = dRecv[i*iItemCount+1];
			pValueGlobal->Values[i]->OZ   = dRecv[i*iItemCount+2];
			pValueGlobal->Values[i]->DX   = dRecv[i*iItemCount+3];
			pValueGlobal->Values[i]->DY   = dRecv[i*iItemCount+4];
			pValueGlobal->Values[i]->DZ   = dRecv[i*iItemCount+5];
			pValueGlobal->Values[i]->Cost = dRecv[i*iItemCount+6];
			for(j=0; j<pValueGlobal->ValueCount; j++) {
				pValueGlobal->Values[i]->Value[j] = dRecv[i*iItemCount+7+j];
			};
		};
	};
#endif
	return 0;
};


static int
SearchValue(
	struct ppohVIS_BASE_stVoxelValue *pValueNew,
	struct ppohVIS_BASE_stVoxelValueItem *pValueItem)
{
	int iFlag, i;

	iFlag = -1;
	for(i=0; i<pValueNew->VoxelCount; i++) {
		if((pValueItem->Level == pValueNew->Values[i]->Level) &&
			(pValueItem->IndexI == pValueNew->Values[i]->IndexI) &&
			(pValueItem->IndexJ == pValueNew->Values[i]->IndexJ) &&
			(pValueItem->IndexK == pValueNew->Values[i]->IndexK)) {
			iFlag = i;
			break;
		};
	};

	return iFlag;
};


static struct ppohVIS_BASE_stVoxelValue *
RemakeVoxelValue(
	struct ppohVIS_BASE_stVoxelValue *pValueGlobal)
{
	struct ppohVIS_BASE_stVoxelValue *pValueGlobalNew = NULL;
	int i, j, k;

	pValueGlobalNew = ppohVIS_BASE_InitVoxelValue(
				pValueGlobal->VoxelCount,
                                pValueGlobal->ValueCount,
                                pValueGlobal->ValueLabel);
	k = 0;
	for(i=0; i<pValueGlobal->VoxelCount; i++) {
		j = SearchValue(pValueGlobalNew, pValueGlobal->Values[i]);
		if(j < 0) {
			pValueGlobalNew->Values[k] = pValueGlobal->Values[i];
			k++;
			pValueGlobalNew->VoxelCount = k;
		} else {
			ppohVIS_BASE_FreeVoxelValueItem(pValueGlobal->Values[i]);
		};
	};

	return pValueGlobalNew;
};


extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_GatherVoxelValue(
	struct ppohVIS_BASE_stVoxelValue *pValueLocal,
	ppohVIS_BASE_Comm comm)
{
	struct ppohVIS_BASE_stVoxelValue *pValueGlobal = NULL;
	struct ppohVIS_BASE_stVoxelValue *pValueGlobalNew = NULL;
	int iVoxelCountLocal, iVoxelCountGlobal;


	iVoxelCountLocal = pValueLocal->VoxelCount;
	ppohVIS_BASE_ReduceVoxelCount(iVoxelCountLocal, comm, &iVoxelCountGlobal);

	if(ppohVIS_BASE_GetCommRank() == 0) {
		pValueGlobal = ppohVIS_BASE_InitVoxelValue(iVoxelCountGlobal,
		                                pValueLocal->ValueCount,
		                                pValueLocal->ValueLabel);
	};

	ppohVIS_BASE_ReduceVoxelValueInt(pValueLocal, comm, pValueGlobal);
	ppohVIS_BASE_ReduceVoxelValueDouble(pValueLocal, comm, pValueGlobal);

	if(ppohVIS_BASE_GetCommRank() == 0) {
		pValueGlobalNew = RemakeVoxelValue(pValueGlobal);

		free(pValueGlobal->Values);
		free(pValueGlobal);
	};

	return pValueGlobalNew;
};

#if 0
static int
CreateChildVoxels(
	struct ppohVIS_BASE_stVoxelItem *pVoxel)
{
	int i, iX, iY, iZ;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stVoxelItem *) * 8;
	pVoxel->Children = (struct ppohVIS_BASE_stVoxelItem **)malloc(BufSize);
	if(pVoxel->Children == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "child voxel");
		goto Error;
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
		if(pVoxel->Children == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "child voxel");
			goto Error;
		};

		pVoxel->Children[i]->Level = pVoxel->Level + 1;
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

Error:
	return -1;
};
#endif
