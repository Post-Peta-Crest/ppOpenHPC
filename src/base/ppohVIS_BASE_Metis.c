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
#include "metis.h"

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_InnerComm.h"


/******************************************************************************/
static int
METIS_Kway(int NodeCount, int ProcCount, int *AdjIndex, int *AdjItem, int *NodeRank, int *EdgeCut)
{
	idx_t nvtxs;
	idx_t ncon = 1;
	idx_t *xadj;
	idx_t *adjncy;
	idx_t *vwgt = NULL;
	idx_t *vsize = NULL;
	idx_t *adjwgt = NULL;
	idx_t nparts;
	real_t *tpwgts = NULL;
	real_t *ubvec = NULL;
	idx_t objval;
	idx_t *part;
	int options[METIS_NOPTIONS];
	int iRc;

	options[METIS_OPTION_OBJTYPE] = METIS_OBJTYPE_CUT;
	options[METIS_OPTION_CTYPE] = METIS_CTYPE_RM;
	options[METIS_OPTION_IPTYPE] = METIS_IPTYPE_GROW;
	options[METIS_OPTION_RTYPE] = METIS_RTYPE_FM;
	//options[METIS_OPTION_NCUTS] = 1;
	//options[METIS_OPTION_NITER] = 10;
	//options[METIS_OPTION_UFACTOR] = 1;
	//options[METIS_OPTION_MINCONN] = 0;
	//options[METIS_OPTION_CONTIG] = 0;
	//options[METIS_OPTION_SEED] = 0;
	options[METIS_OPTION_NUMBERING] = 0;
	//options[METIS_OPTION_DBGLVL] = 0;

	nvtxs = (idx_t)NodeCount;
	nparts = (idx_t)ProcCount;
	xadj = (idx_t *)AdjIndex;
	adjncy = (idx_t *)AdjItem;
	part = (idx_t *)NodeRank;

	iRc = METIS_PartGraphKway(
		&nvtxs, &ncon, xadj, adjncy,
		vwgt, vsize, adjwgt, &nparts, tpwgts, ubvec,
		options, &objval, part);

	(*EdgeCut) = (int)objval;

	return 0;
};


/******************************************************************************/
static int
MakeAdjacency(
	struct ppohVIS_BASE_stMeshNode *pNode, 
	struct ppohVIS_BASE_stGeometryItem *pEdge,
	int *AdjSum, int **AdjCount, int **AdjIndex, int **AdjItem)
{
	int Index1, Index2, Node1, Node2;
	int i;

	/*
 	 * count adjacency nodes of each node
 	 */
	(*AdjCount) = (int *)calloc(pNode->Count, sizeof(int));
	if((*AdjCount) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "# of adjacency node");
		goto error;
	};

	for(i=0; i<pEdge->Count; i++) {
		Node1 = pEdge->Node[2*i];
		Node2 = pEdge->Node[2*i+1];
		(*AdjCount)[Node1-1] += 1;
		(*AdjCount)[Node2-1] += 1;
	};

	/*
	 * make index
	 */
	(*AdjIndex) = (int *)calloc(pNode->Count+1, sizeof(int));
	if((*AdjIndex) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", sterror(errno), "index of adjacency node count");
		goto error;
	};

	(*AdjIndex)[0] = 0;
	for(i=0; i<pNode->Count; i++) {
		(*AdjIndex)[i+1] = (*AdjIndex)[i] + (*AdjCount)[i];
		(*AdjCount)[i] = 0;
	};

	/*
	 * sum of adjacency nodes
	 */
	(*AdjSum) = (*AdjIndex)[pNode->Count];

	/*
	 * make adjacency node list
	 */
	(*AdjItem) = (int *)calloc((*AdjSum), sizeof(int));
	if((*AdjItem) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "adjacency item");
		goto error;
	};

	for(i=0; i<pEdge->Count; i++) {
		Node1 = pEdge->Node[2*i];
		Node2 = pEdge->Node[2*i+1];
		Index1 = (*AdjIndex)[Node1-1] + (*AdjCount)[Node1-1];
		Index2 = (*AdjIndex)[Node2-1] + (*AdjCount)[Node2-1];

		(*AdjItem)[Index1] = Node2-1;
		(*AdjItem)[Index2] = Node1-1;

		(*AdjCount)[Node1-1] += 1;
		(*AdjCount)[Node2-1] += 1;
	};

	return 0;

error:
	return -1;
};


/******************************************************************************/
static int
CountNodes(
	struct ppohVIS_BASE_stMeshNode *pNode, int ProcCount,
	struct ppohVIS_BASE_stInnerComm *pComm)
{
	int iProc, iIndex;
	int i;

	for(i=0; i<pNode->Count; i++) {
		pComm->NodeCount[ProcCount] += 1;
	};

	pComm->NodeIndex[0] = 0;
	for(i=0; i<ProcCount; i++) {
		pComm->NodeIndex[i+1] = pComm->NodeIndex[i] + pComm->NodeCount[i];
		pComm->NodeCount[i] = 0;
	};

	for(i=0; i<pNode->Count; i++) {
		iProc = pComm->NodeRank[i];
		iIndex = pComm->NodeIndex[iProc] + pComm->NodeCount[iProc];
		pComm->NodeItem[iIndex] = i+1;
		pComm->NodeCount[iProc] += 1;
	};

	return 0;
};


/******************************************************************************/
static int
ppohVIS_BASE_GraphPartition(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pEdge,
	int ProcCount)
{
	struct ppohVIS_BASE_stInnerComm *pInnerComm = NULL;
	int AdjSum;
	int *AdjCount = NULL;
	int *AdjIndex = NULL;
	int *AdjItem = NULL;
	int *parts = NULL;
	int EdgeCut;
	int iRc;

	iRc = MakeAdjacency(
		pMesh->Node, pEdge, &AdjSum, &AdjCount, &AdjIndex, &AdjItem);
	if(iRc != 0) {
		goto error;
	};

	pInnerComm = ppohVIS_BASE_AllocateInnerComm(
			ProcCount, pMesh->Node->Count);

	iRc = METIS_Kway(
		pMesh->Node->Count, ProcCount,
		AdjIndex, AdjItem, pInnerComm->NodeRank, &EdgeCut);
	if(iRc != 0) {
		goto error;
	};

	iRc = CountNodes(pMesh->Node, ProcCount, pInnerComm);
	if(iRc != 0) {
		goto error;
	};

	return 0;

error:
	return -1;
};

