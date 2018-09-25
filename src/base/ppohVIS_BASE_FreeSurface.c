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

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_FreeSurface.h"


/*******************************************************************************
 * Create free surface information
 ******************************************************************************/
extern struct ppohVIS_BASE_stGeometryItem *
ppohVIS_BASE_GetFreeSurface(
	struct ppohVIS_BASE_stGeometryItem *pTsuf,
	struct ppohVIS_BASE_stGeometryItem *pQsuf)
{
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf = NULL;
	int Count, CountT, CountQ;
	int iTria, iQuad;

	pFreeSurf = ppohVIS_BASE_AllocateGeometryItem();
	if(pFreeSurf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "free surface");
		goto error;
	};

	CountT = 0;
	CountQ = 0;
#pragma omp parallel for reduction(+:CountT)
	for(iTria=0; iTria<pTsuf->Count; iTria++) {
		if(pTsuf->Refered[iTria] == 1) {
			CountT += 1;
		};
	};
#pragma omp parallel for reduction(+:CountQ)
	for(iQuad=0; iQuad<pQsuf->Count; iQuad++) {
		if(pQsuf->Refered[iQuad] == 1) {
			CountQ += 2;
		};
	};
	Count = CountT + CountQ;

	pFreeSurf->Count = Count;
	pFreeSurf->Type = ppohVIS_BASE_Tria3;
	pFreeSurf->ID = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(pFreeSurf->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface ID");
		goto error;
	};
	pFreeSurf->Refered = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(pFreeSurf->Refered == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface reference count");
		goto error;
	};
	pFreeSurf->Node = (int *)calloc(pFreeSurf->Count*3, sizeof(int));
	if(pFreeSurf->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface connectivity");
		goto error;
	};
	pFreeSurf->Element = (int *)calloc(pFreeSurf->Count*2, sizeof(int));
	if(pFreeSurf->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface attached element");
		goto error;
	};

	Count = 0;
	for(iTria=0; iTria<pTsuf->Count; iTria++) {
		if(pTsuf->Refered[iTria] == 1) {
			pFreeSurf->ID[Count]          = Count + 1;
			pFreeSurf->Refered[Count]     = 1;
			pFreeSurf->Node[3*Count]      = pTsuf->Node[3*iTria];
			pFreeSurf->Node[3*Count+1]    = pTsuf->Node[3*iTria+1];
			pFreeSurf->Node[3*Count+2]    = pTsuf->Node[3*iTria+2];
			pFreeSurf->Element[2*Count]   = pTsuf->Element[2*iTria];
			pFreeSurf->Element[2*Count+1] = pTsuf->Element[2*iTria+1];
			Count += 1;
		};
	};
	for(iQuad=0; iQuad<pQsuf->Count; iQuad++) {
		if(pQsuf->Refered[iQuad] == 1) {
			pFreeSurf->ID[Count]          = Count + 1;
			pFreeSurf->Refered[Count]     = 1;
			pFreeSurf->Node[3*Count]      = pQsuf->Node[4*iQuad];
			pFreeSurf->Node[3*Count+1]    = pQsuf->Node[4*iQuad+1];
			pFreeSurf->Node[3*Count+2]    = pQsuf->Node[4*iQuad+2];
			pFreeSurf->Element[2*Count]   = pQsuf->Element[2*iQuad];
			pFreeSurf->Element[2*Count+1] = pQsuf->Element[2*iQuad+1];
			Count += 1;

			pFreeSurf->ID[Count]          = Count + 1;
			pFreeSurf->Refered[Count]     = 1;
			pFreeSurf->Node[3*Count]      = pQsuf->Node[4*iQuad+2];
			pFreeSurf->Node[3*Count+1]    = pQsuf->Node[4*iQuad+3];
			pFreeSurf->Node[3*Count+2]    = pQsuf->Node[4*iQuad];
			pFreeSurf->Element[2*Count]   = pQsuf->Element[2*iQuad];
			pFreeSurf->Element[2*Count+1] = pQsuf->Element[2*iQuad+1];
			Count += 1;
		};
	};

	return pFreeSurf;

error:
	if(pFreeSurf) {
		ppohVIS_BASE_FreeGeometryItem(pFreeSurf);
		pFreeSurf = NULL;
	};
	return NULL;
};


extern struct ppohVIS_BASE_stGeometryItem *
ppohVIS_BASE_GetDistFreeSurface(
	struct ppohVIS_BASE_stGeometryItem *pOrg,
	struct ppohVIS_BASE_stMeshNode *pNode)
{
	struct ppohVIS_BASE_stGeometryItem *pNew = NULL;
	int iNode1, iNode2, iNode3;
	int iRank1, iRank2, iRank3;
	int iCount, iSurf;
	int iMyRank;

	iMyRank = ppohVIS_BASE_GetCommRank();

	iCount = 0;
#pragma omp parallel for private(iSurf,iNode1,iNode2,iNode3,iRank1,iRank2,iRank3) reduction(+:iCount)
	for(iSurf=0; iSurf<pOrg->Count; iSurf++) {
		iNode1 = pOrg->Node[3*iSurf];
		iNode2 = pOrg->Node[3*iSurf+1];
		iNode3 = pOrg->Node[3*iSurf+2];
		iRank1 = pNode->Rank[iNode1-1];
		iRank2 = pNode->Rank[iNode2-1];
		iRank3 = pNode->Rank[iNode3-1];

		if((iRank1 == iMyRank) || (iRank2 == iMyRank) || (iRank3 == iMyRank)) {
			if((iRank1 >= iMyRank) && (iRank2 >= iMyRank) && (iRank3 >= iMyRank)) {
				iCount++;
			};
		};
	};

	pNew = ppohVIS_BASE_AllocateGeometryItem();
	if(pNew == NULL) goto error;

	pNew->Count = iCount;
	pNew->Type = ppohVIS_BASE_Tria3;
	pNew->ID = (int *)calloc(pNew->Count, sizeof(int));
	if(pNew->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface ID");
		goto error;
	};
	pNew->Refered = (int *)calloc(pNew->Count, sizeof(int));
	if(pNew->Refered == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface reference count");
		goto error;
	};
	pNew->Node = (int *)calloc(pNew->Count*3, sizeof(int));
	if(pNew->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface connectivity");
		goto error;
	};
	pNew->Element = (int *)calloc(pNew->Count*2, sizeof(int));
	if(pNew->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "free surface attached element");
		goto error;
	};

	iCount = 0;
	for(iSurf=0; iSurf<pOrg->Count; iSurf++) {
		iNode1 = pOrg->Node[3*iSurf];
		iNode2 = pOrg->Node[3*iSurf+1];
		iNode3 = pOrg->Node[3*iSurf+2];
		iRank1 = pNode->Rank[iNode1-1];
		iRank2 = pNode->Rank[iNode2-1];
		iRank3 = pNode->Rank[iNode3-1];

		if((iRank1 == iMyRank) || (iRank2 == iMyRank) || (iRank3 == iMyRank)) {
			if((iRank1 >= iMyRank) && (iRank2 >= iMyRank) && (iRank3 >= iMyRank)) {
				pNew->ID[iCount]          = pOrg->ID[iSurf];
				pNew->Refered[iCount]     = pOrg->Refered[iSurf];
				pNew->Node[3*iCount]      = pOrg->Node[3*iSurf];
				pNew->Node[3*iCount+1]    = pOrg->Node[3*iSurf+1];
				pNew->Node[3*iCount+2]    = pOrg->Node[3*iSurf+2];
				pNew->Element[2*iCount]   = pOrg->Node[2*iSurf];
				pNew->Element[2*iCount+1] = pOrg->Node[2*iSurf+1];
				iCount++;
			};
		};
	};

	return pNew;

error:
	return NULL;
};


#if 0
static struct ppohVIS_BASE_stMeshNode *
MaskNode(
	struct ppohVIS_BASE_stMeshNode *pNode,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	struct ppohVIS_BASE_stMeshNode *pNodeNew = NULL;
	int *NodeMask = NULL;
	int iNode1, iNode2, iNode3;
	int iProc1, iProc2, iProc3;
	int iNode, iSurf;
	int iMyRank;
	int iCount;

	iMyRank = ppohVIS_BASE_GetCommRank();

	NodeMask = (int *)calloc(pNode->Count, sizeof(int));
	if(NodeMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "node mask");
		goto Error;
	};

#pragma omp parallel for private(iSurf,iNode1,iNode2,iNode3,iProc1,iProc2,iProc3,iNode)
	for(iSurf=0; iSurf<pFreeSurf->Count; iSurf++) {
		iNode1 = pFreeSurf->Node[3*iSurf];
		iNode2 = pFreeSurf->Node[3*iSurf+1];
		iNode3 = pFreeSurf->Node[3*iSurf+2];
		iProc1 = pNode->Rank[iNode1-1];
		iProc2 = pNode->Rank[iNode2-1];
		iProc3 = pNode->Rank[iNode3-1];

		if(iProc1 == iMyRank) {
			iNode = pNode->ID[iNode1-1];
			NodeMask[iNode-1] = 1;
		};
		if(iProc2 == iMyRank) {
			iNode = pNode->ID[iNode2-1];
			NodeMask[iNode-1] = 1;
		};
		if(iProc3 == iMyRank) {
			iNode = pNode->ID[iNode3-1];
			NodeMask[iNode-1] = 1;
		};
	};

	iCount = 0;
#pragma omp parallel for private(iNode) reduction(+:iCount)
	for(iNode=0; iNode<pNode->Count; iNode++) {
		if(NodeMask[iNode] > 0) {
			iCount += 1;
		};
	};

	pNodeNew = ppohVIS_BASE_AllocateMeshNode();
	if(pNodeNew == NULL) {
		goto Error;
	};
	if(ppohVIS_BASE_InitMeshNode(pNodeNew, iCount)) {
		goto Error;
	};

	for(iNode=0; iNode<pNode->Count; iNode++) {
		if(NodeMask[iNode] > 0) {
			pNodeNew->ID[iCount]         = pNode->ID[iNode];
			pNodeNew->Rank[iCount]       = pNode->Rank[iNode];
			pNodeNew->Coords[3*iCount]   = pNode->Coords[3*iNode];
			pNodeNew->Coords[3*iCount+1] = pNode->Coords[3*iNode+1];
			pNodeNew->Coords[3*iCount+2] = pNode->Coords[3*iNode+2];
			iCount += 1;
		};
	};

	if(NodeMask) {
		free(NodeMask);
	};

	return pNodeNew;

Error:
	if(NodeMask) {
		free(NodeMask);
	};
	return NULL;
};


static struct ppohVIS_BASE_stMeshNode *
GatherNode(
	struct ppohVIS_BASE_stMeshNode *pNode)
{
	struct ppohVIS_BASE_stMeshNode *pNodeNew = NULL;
	int *iSendBuf = NULL, *iRecvBuf = NULL;
	double *dSendBuf = NULL, *dRecvBuf = NULL;
	int iSendCount, iRecvTotal;
	int *iRecvCount = NULL, *iRecvCountI = NULL, *iRecvCountD = NULL;
	int *iRecvIndex = NULL, *iRecvIndexI = NULL, *iRecvIndexD = NULL;
	int iProcCount, iMyRank;
	ppohVIS_BASE_Comm comm;
	int iRc, i, iNode;

	/* parallel information */
	iProcCount = ppohVIS_BASE_GetCommSize();
	comm = ppohVIS_BASE_GetCommunicator();
	iMyRank = ppohVIS_BASE_GetCommRank();

	/****************************************/
	/* gather number of nodes to root proc. */
	/****************************************/
	/* send buf. */
	iSendCount = pNode->Count;

	/* recieve buf. */
	if(iMyRank == 0) {
		iRecvCount = (int *)calloc(iProcCount, sizeof(int));
		if(iRecvCount == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve buffer");
			goto Error;
		};
	};

	/* MPI_Gather */
	iRc = MPI_Gather(&iSendCount, 1, MPI_INT, iRecvCount, 1, MPI_INT, 0, comm);
	if(iRc != MPI_SUCCESS) goto Error;

	/* recieve info. */
	if(iMyRank == 0) {
		iRecvIndex = (int *)calloc(iProcCount+1, sizeof(int));
		if(iRecvIndex == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve index");
			goto Error;
		};

		iRecvIndex[0] = 0;
		for(i=0; i<iProcCount; i++) {
			iRecvIndex[i+1] = iRecvIndex[i] + iRecvCount[i];
		};
	};

	/**************************************/
	/* gather integer info. to root proc. */
	/**************************************/
	/* send buf. */
	iSendCount = pNode->Count * 2;
	iSendBuf = (int *)malloc(iSendCount * sizeof(int));
	if(iSendBuf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "send buf.");
		goto Error;
	};
	for(iNode=0; iNode<pNode->Count; iNode++) {
		iSendBuf[2*iNode]   = pNode->ID[iNode];
		iSendBuf[2*iNode+1] = pNode->Rank[iNode];
	};

	/* recieve buf. */
	if(iMyRank == 0) {
		iRecvIndexI = (int *)calloc(iProcCount+1, sizeof(int));
		if(iRecvIndexI == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve index");
			goto Error;
		};

		iRecvCountI = (int *)calloc(iProcCount, sizeof(int));
		if(iRecvIndexI == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve count");
			goto Error;
		};

		iRecvIndexI[0] = 0;
		for(i=0; i<iProcCount; i++) {
			iRecvCountI[i] = iRecvCount[i] * 2;
			iRecvIndexI[i+1] = iRecvIndex[i+1] * 2;
		};

		iRecvTotal = iRecvIndexI[iProcCount];
		iRecvBuf = (int *)malloc(iRecvTotal * sizeof(int));
		if(iRecvBuf == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recv buf.");
			goto Error;
		};
	};

	/* MPI_Gatherv */
	iRc = MPI_Gatherv(
			iSendBuf, iSendCount, MPI_INT,
			iRecvBuf, iRecvCountI, iRecvIndexI, MPI_INT, 0, comm);
	if(iRc != MPI_SUCCESS) goto Error;

	/* save buf. */
	if(iMyRank == 0) {
		for(iNode=0; iNode<iRecvIndex[iProcCount]; iNode++) {
			pNodeNew->ID[iNode] = iRecvBuf[2*iNode];
			pNodeNew->Rank[iNode] = iRecvBuf[2*iNode+1];
		};
	};

	/************************************/
	/* gather double info to root proc. */
	/************************************/
	/* send buf. */
	iSendCount = pNode->Count * 3;
	dSendBuf = (double *)malloc(iSendCount * sizeof(double));
	if(dSendBuf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "send buf.");
		goto Error;
	};
	for(iNode=0; iNode<pNode->Count; iNode++) {
		dSendBuf[3*iNode]   = pNode->Coords[3*iNode];
		dSendBuf[3*iNode+1] = pNode->Coords[3*iNode+1];
		dSendBuf[3*iNode+2] = pNode->Coords[3*iNode+2];
	};

	/* recieve buf. */
	if(iMyRank == 0) {
		iRecvIndexD = (int *)calloc(iProcCount+1, sizeof(int));
		if(iRecvIndexD == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve index");
			goto Error;
		};

		iRecvCountD = (int *)calloc(iProcCount, sizeof(int));
		if(iRecvCountD == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve count");
			goto Error;
		};

		iRecvIndexD[0] = 0;
		for(i=0; i<iProcCount; i++) {
			iRecvCountD[i] = iRecvCount[i] * 3;
			iRecvIndexD[i+1] = iRecvIndex[i+1] * 3;
		};

		iRecvTotal = iRecvIndexD[iProcCount];
		dRecvBuf = (double *)malloc(iRecvTotal * sizeof(double));
		if(dRecvBuf == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recv buf.");
			goto Error;
		};
	};

	/* MPI_Gatherv */
	iRc = MPI_Gatherv(
			dSendBuf, iSendCount, MPI_DOUBLE,
			dRecvBuf, iRecvCountD, iRecvIndexD, MPI_DOUBLE, 0, comm);
	if(iRc != MPI_SUCCESS) goto Error;

	/* save buf. */
	if(iMyRank == 0) {
		for(iNode=0; iNode<iRecvIndex[iProcCount]; iNode++) {
			pNodeNew->ID[iNode] = iRecvBuf[2*iNode];
			pNodeNew->Rank[iNode] = iRecvBuf[2*iNode+1];
			pNodeNew->Coords[3*iNode]   = dRecvBuf[3*iNode];
			pNodeNew->Coords[3*iNode+1] = dRecvBuf[3*iNode+1];
			pNodeNew->Coords[3*iNode+2] = dRecvBuf[3*iNode+2];
		};
	};

	if(iRecvCount) {
		free(iRecvCount);
	};
	if(iSendBuf) { free(iSendBuf); };
	if(iRecvBuf) { free(iRecvBuf); };

	return pNodeNew;

Error:
	return NULL;
};

extern struct ppohVIS_BASE_stMesh *
ppohVIS_BASE_GatherFreeSurface(
	struct ppohVIS_BASE_stMeshNode *pNodeNew,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	struct ppohVIS_BASE_stMesh *pMesh = NULL;
	int iSendCount, *iRecvCount = NULL, *iRecvIndex = NULL;
	int iProcCount;
	int iMyRank;
	ppohVIS_BASE_Comm comm;
	int iRc, i;

	iProcCount = ppohVIS_BASE_GetCommSize();
	comm = ppohVIS_BASE_GetCommunicator();
	iMyRank = ppohVIS_BASE_GetCommRank();

	iSendCount = pFreeSurf->Count;

	if(iMyRank == 0) {
		iRecvCount = (int *)calloc(iProcCount, sizeof(int));
		if(iRecvCount == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "recieve buffer");
			goto Error;
		};
	};

	iRc = MPI_Gather(&iSendCount, 1, MPI_INT, iRecvCount, 1, MPI_INT, 0, comm);
	if(iRc != MPI_SUCCESS) goto Error;

	if(iMyRank == 0) {
		iRecvIndex = (int *)calloc(iProcCount+1, sizeof(int));
		iRecvIndex[0] = 0;
		for(i=0; i<iProcCount; i++) {
			iRecvIndex[i+1] = iRecvIndex[i] + iRecvCount[i];
		};
	};

	return pMesh;

Error:
	return NULL;
};
#endif


/*******************************************************************************
 * Set node list on free surface   ##### currently not used #####
 ******************************************************************************/
#if 0
static int *
MaskNodeOnFreeSurface(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	int *NodeMask = NULL;
	int Node1, Node2, Node3;
	int iTsuf;

	NodeMask = (int *)calloc(pMesh->Node->Count, sizeof(int));
	if(NodeMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "node mask");
		goto error;
	};

	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		Node1 = pFreeSurf->Node[3*iTsuf];
		Node2 = pFreeSurf->Node[3*iTsuf+1];
		Node3 = pFreeSurf->Node[3*iTsuf+2];

		NodeMask[Node1-1] = 1;
		NodeMask[Node2-1] = 1;
		NodeMask[Node3-1] = 1;
	};

	return NodeMask;

error:
	if(NodeMask) {
		free(NodeMask);
		NodeMask = NULL;
	};
	return NULL;
};
#endif


/*******************************************************************************
 * Print free surface information
 ******************************************************************************/
/*=================*
 * Print to stream *
 *=================*/
extern void
ppohVIS_BASE_PrintFreeSurface(
	FILE *fp, struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	int iTsuf;

	fprintf(fp, "%10d%3d\n", pFreeSurf->Count, pFreeSurf->Type);
	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		fprintf(fp, "%10d%10d%10d%10d%10d%10d%10d\n",
				pFreeSurf->ID[iTsuf],
				pFreeSurf->Refered[iTsuf],
				pFreeSurf->Node[3*iTsuf],
				pFreeSurf->Node[3*iTsuf+1],
				pFreeSurf->Node[3*iTsuf+2],
				pFreeSurf->Element[2*iTsuf],
				pFreeSurf->Element[2*iTsuf+1]);
	};

	return;
};


/*=================*
 * Print to a file *
 *=================*/
extern int
ppohVIS_BASE_PutFreeSurface(
	char *FileName, struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
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
				"%s [%s]",
				strerror(errno), "free surface info.");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]",
				strerror(errno), "free surface info.");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintFreeSurface(fp, pFreeSurf);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "free surface info.");
		goto error;
	};

	return 0;

error:
	return -1;
};


/*******************************************************************************
 * Print mesh of free surface to a file
 ******************************************************************************/
extern int
ppohVIS_BASE_PutFreeSurfaceMesh(
	char *FileName, struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	int Node, Count, iTsuf, iNode, iNodeS, iNodeE;
	int *Old2New = NULL;
	int *New2Old = NULL;
	FILE *fp;
	int iMyRank;
	char cFileName[PPOHVIS_BASE_FILE_NAME_LEN];

	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh info");
		goto error;
	};
	if(pMesh->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh node info");
		goto error;
	};
	if(pMesh->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh element info");
		goto error;
	};
	if(pFreeSurf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "free surface info");
		goto error;
	};

	iMyRank = ppohVIS_BASE_GetCommRank();
	ppohVIS_BASE_GetDistFileName(FileName, iMyRank,
	                              cFileName, PPOHVIS_BASE_FILE_NAME_LEN);

	if((fp = fopen(cFileName, "w")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError,
			"%s [%s]", strerror(errno), "surface patch file");
		goto error;
	};

	Old2New = (int *)calloc(pMesh->Node->Count, sizeof(int));
	if(Old2New == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask array for node");
		goto error;
	};
	New2Old = (int *)calloc(pMesh->Node->Count, sizeof(int));
	if(New2Old == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask array for node");
		goto error;
	};

	Count = 0;
	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		for(iNode=0; iNode<3; iNode++) {
			Node = pFreeSurf->Node[3*iTsuf+iNode];
			if(Old2New[Node-1] == 0) {
				Count++;
				Old2New[Node-1]  = Count;
				New2Old[Count-1] = Node;
			};
		};
	};

	fprintf(fp, "%10d\n", Count);

	for(iNode=0; iNode<Count; iNode++) {
		Node = New2Old[iNode];
		fprintf(fp, "%10d%15.7e%15.7e%15.7e\n",
			iNode+1,
			pMesh->Node->Coords[3*(Node-1)],
			pMesh->Node->Coords[3*(Node-1)+1],
			pMesh->Node->Coords[3*(Node-1)+2]);
	};

	fprintf(fp, "%10d\n", pFreeSurf->Count);

	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		fprintf(fp, "%10d%5d", iTsuf+1, 231);
		iNodeS = 3*iTsuf;
		iNodeE = 3*(iTsuf+1);
		for(iNode=iNodeS; iNode<iNodeE; iNode++) {
			Node = pFreeSurf->Node[iNode];
			fprintf(fp, "%10d", Old2New[Node-1]);
		};
		fprintf(fp, "\n");
	};


	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "surface patch file");
		goto error;
	};

	return 0;

error:
	return -1;
};



/*******************************************************************************
 *
 ******************************************************************************/
extern int
ppohVIS_BASE_CreateFreeSurfaceMesh(
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult,
	struct ppohVIS_BASE_stMesh *pMeshNew,
	struct ppohVIS_BASE_stResult *pResultNew)
{
	int *Old2New, *New2Old;
	int iElemNodeCount = 3;   /* triangular surface */
	double GX, GY, GZ, DN;
	int Elem, Node, Count, BufSize, BufCount;
	int iElem, iNode, iDOF, i, j;
	enum ppohVIS_BASE_eTopology iType;
	int iRc;

	/*----------------------------------------------------------------------
	 * Check data
	 *--------------------------------------------------------------------*/
	if(pFreeSurf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "Geometry data of free surfaces");
		goto error;
	};
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound, 
			"%s", "Mesh data");
		goto error;
	};
	if(pResult == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "Result data");
		goto error;
	};

	/*----------------------------------------------------------------------
	 * Node ID conversion table
	 *--------------------------------------------------------------------*/
	BufCount = pMesh->Node->Count;
	BufSize = sizeof(int);
	Old2New = (int *)calloc(BufCount, BufSize);
	if(Old2New == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask array for node");
		goto error;
	};
	New2Old = (int *)calloc(BufCount, BufSize);
	if(New2Old == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask array for node");
		goto error;
	};

	Count = 0;
	for(iNode=0; iNode<pFreeSurf->Count*iElemNodeCount; iNode++) {
		Node = pFreeSurf->Node[iNode];
		if(Old2New[Node-1] == 0) {
			Count++;
			Old2New[Node-1]  = Count;
			New2Old[Count-1] = Node;
		};
	};

	/*----------------------------------------------------------------------
	 * Node info.
	 *--------------------------------------------------------------------*/
	BufSize = sizeof(struct ppohVIS_BASE_stMeshNode);
	pMeshNew->Node = (struct ppohVIS_BASE_stMeshNode *)malloc(BufSize);
       	if(pMeshNew->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mesh node info");
		goto error;
	};

	pMeshNew->Node->Count = Count;
	BufCount = pMeshNew->Node->Count;
	BufSize = sizeof(int);
	pMeshNew->Node->ID = (int *)calloc(BufCount, BufSize);
	if(pMeshNew->Node->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto error;
	};
	BufSize = sizeof(double)*pMeshNew->Node->Count*3;
	pMeshNew->Node->Coords = (double *)malloc(BufSize);
	if(pMeshNew->Node->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "nodal coordinate");
		goto error;
	};

#pragma omp parallel for private(Node)
	for(iNode=0; iNode<pMeshNew->Node->Count; iNode++) {
		Node = New2Old[iNode];
		pMeshNew->Node->ID[iNode]         = iNode+1;
		pMeshNew->Node->Coords[3*iNode]   = pMesh->Node->Coords[3*(Node-1)];
		pMeshNew->Node->Coords[3*iNode+1] = pMesh->Node->Coords[3*(Node-1)+1];
		pMeshNew->Node->Coords[3*iNode+2] = pMesh->Node->Coords[3*(Node-1)+2];
	};

	/*----------------------------------------------------------------------
	 * Element info.
	 *--------------------------------------------------------------------*/
	pMeshNew->Element = ppohVIS_BASE_AllocateMeshElement();
	if(pMeshNew->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element info");
		goto error;
	};

	iRc = ppohVIS_BASE_InitMeshElement(pMeshNew->Element, pFreeSurf->Count);
	if(iRc != 0) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element info");
		goto error;
	};

	BufCount = pFreeSurf->Count * iElemNodeCount;
	BufSize = sizeof(int);
	pMeshNew->Element->Node = (int *)calloc(BufCount, BufSize);
	if(pMeshNew->Element->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "connectivity");
		goto error;
        };

	pMeshNew->Element->Count = pFreeSurf->Count;
	memcpy(pMeshNew->Element->ID, pFreeSurf->ID, pFreeSurf->Count*sizeof(int));
	for(iElem=0; iElem<pFreeSurf->Count; iElem++) {
		pMeshNew->Element->Type[iElem] = iType;
		pMeshNew->Element->NodeIndex[iElem+1] = iElemNodeCount*(iElem+1);

		GX=0.0;
		GY=0.0;
		GZ=0.0;
		DN=0.0;
		for(iNode=iElemNodeCount*iElem; iNode<iElemNodeCount*(iElem+1); iNode++) {
			Node = pFreeSurf->Node[iNode];
			pMeshNew->Element->Node[iNode] = Old2New[Node-1];

			GX += pMesh->Node->Coords[3*(Node-1)];
			GY += pMesh->Node->Coords[3*(Node-1)+1];
			GZ += pMesh->Node->Coords[3*(Node-1)+2];
			DN += 1.0;
		};
		pMesh->Element->Gravity[3*iElem]   = GX / DN;
		pMesh->Element->Gravity[3*iElem+1] = GY / DN;
		pMesh->Element->Gravity[3*iElem+2] = GZ / DN;
	};

	/*----------------------------------------------------------------------
	 * Result info.
	 *--------------------------------------------------------------------*/
	if(pResult->EntityType == ppohVIS_BASE_ResultNode) {
		iRc = ppohVIS_BASE_InitResult(
			pResultNew, pMeshNew->Node->Count,
			ppohVIS_BASE_ResultNode, pResult->FreedomCount);
		strncpy(pResultNew->Label, pResult->Label, PPOHVIS_BASE_LABEL_LEN);

		for(iDOF=0; iDOF<pResultNew->FreedomCount; iDOF++) {
#pragma omp parallel for private(Node,i,j)
			for(iNode=0; iNode<pResultNew->ItemCount; iNode++) {
				Node = New2Old[iNode];
				i = iNode * pResultNew->FreedomCount + iDOF;
				j = (Node-1) * pResult->FreedomCount + iDOF;
				pResultNew->Value[i] = pResult->Value[j];
			};
		};

	} else {
		iRc = ppohVIS_BASE_InitResult(
			pResultNew, pMeshNew->Element->Count,
			ppohVIS_BASE_ResultElement, pResult->FreedomCount);
		strncpy(pResultNew->Label, pResult->Label, PPOHVIS_BASE_LABEL_LEN);

		for(iDOF=0; iDOF<pResultNew->FreedomCount; iDOF++) {
#pragma omp parallel for private(Node,i,j)
			for(iElem=0; iElem<pResultNew->ItemCount; iElem++) {
				Elem = pFreeSurf->Element[2*iElem];
				i = iElem * pResultNew->FreedomCount + iDOF;
				j = (Elem-1) * pResult->FreedomCount + iDOF;
				pResultNew->Value[i] = pResult->Value[j];
			};
		};
	};

	return 0;

error:
	return -1;
};
