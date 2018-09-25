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
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Mesh.h"


/*******************************************************************************
 * Allocate
 ******************************************************************************/
extern struct ppohVIS_BASE_stMeshNode *
ppohVIS_BASE_AllocateMeshNode(void)
{
	struct ppohVIS_BASE_stMeshNode *pNode = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stMeshNode);
	pNode = (struct ppohVIS_BASE_stMeshNode *)malloc(BufSize);
	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node info");
		return NULL;
	};

	pNode->Count = 0;
	pNode->IntCount = 0;
	pNode->ID = NULL;
	pNode->Rank = NULL;
	pNode->Coords = NULL;

	return pNode;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stMeshElement *
ppohVIS_BASE_AllocateMeshElement(void)
{
	struct ppohVIS_BASE_stMeshElement *pElem = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stMeshElement);
	pElem = (struct ppohVIS_BASE_stMeshElement *)malloc(BufSize);
	if(pElem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element info");
		return NULL;
	};

	pElem->Count = 0;
	pElem->IntCount = 0;
	pElem->ID = NULL;
	pElem->Rank = NULL;
	pElem->Type = NULL;
	pElem->NodeIndex = NULL;
	pElem->Node = NULL;
	pElem->Gravity = NULL;

	return pElem;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stMeshComm *
ppohVIS_BASE_AllocateMeshComm(void)
{
	struct ppohVIS_BASE_stMeshComm *pComm = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stMeshComm);
	pComm = (struct ppohVIS_BASE_stMeshComm *)malloc(BufSize);
	if(pComm == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "comm. info");
		return NULL;
	};

	pComm->NeighborCount = 0;
	pComm->Neighbor = NULL;
	pComm->ImportIndex = NULL;
	pComm->ImportItem = NULL;
	pComm->ExportIndex = NULL;
	pComm->ExportItem = NULL;
	pComm->SharedIndex = NULL;
	pComm->SharedItem = NULL;

	return pComm;
};


/*============================================================================*/
extern struct ppohVIS_BASE_stMesh *
ppohVIS_BASE_AllocateMesh(void)
{
	struct ppohVIS_BASE_stMesh *pMesh = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stMesh);
	pMesh = (struct ppohVIS_BASE_stMesh *)malloc(BufSize);
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mesh info");
		goto Error;
	};

	pMesh->Node = ppohVIS_BASE_AllocateMeshNode();
	if(pMesh->Node == NULL) {
		goto Error;
	};

	pMesh->Element = ppohVIS_BASE_AllocateMeshElement();
	if(pMesh->Element == NULL) {
		goto Error;
	};

	pMesh->Comm = ppohVIS_BASE_AllocateMeshComm();
	if(pMesh->Comm == NULL) {
		goto Error;
	};

	return pMesh;

Error:
	return NULL;
};


/*******************************************************************************
 * Initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_InitMeshNode(struct ppohVIS_BASE_stMeshNode *pNode, int nNode)
{
	size_t BufSize;
	int i;

	/* allocation check */
	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info");
		goto Error;
	} else {
		if(pNode->ID != NULL) {
			free(pNode->ID);
			pNode->ID = NULL;
		};
		if(pNode->Rank != NULL) {
			free(pNode->Rank);
			pNode->Rank = NULL;
		};
		if(pNode->Coords != NULL) {
			free(pNode->Coords);
			pNode->Coords = NULL;
		};
	};

	/* number of nodes */
	pNode->Count = nNode;
	if(pNode->Count <= 0) { return 0; };

	/* node ID */
	BufSize = sizeof(int) * pNode->Count;
	pNode->ID = (int *)malloc(BufSize);
	if(pNode->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto Error;
	};

	/* node rank */
	BufSize = sizeof(int) * pNode->Count;
	pNode->Rank = (int *)malloc(BufSize);
	if(pNode->Rank == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node rank");
		goto Error;
	};

	/* coordinates */
	BufSize = sizeof(double) * pNode->Count * 3;
	pNode->Coords = (double *)malloc(BufSize);
	if(pNode->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "coordinates");
		goto Error;
	};

#pragma omp parallel for
	for(i=0; i<pNode->Count; i++) {
		pNode->ID[i]         = 0;
		pNode->Rank[i]       = 0;
		pNode->Coords[3*i  ] = 0.0;
		pNode->Coords[3*i+1] = 0.0;
		pNode->Coords[3*i+2] = 0.0;
	};

	return 0;

Error:
	return -1;
};

/*============================================================================*/
extern int
ppohVIS_BASE_InitMeshElement(
	struct ppohVIS_BASE_stMeshElement *pElem, int nElement)
{
	size_t BufSize;
	int i;

	/* allocation check */
	if(pElem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info");
		goto Error;
	} else {
		if(pElem->ID != NULL) {
			free(pElem->ID);
			pElem->ID = NULL;
		};
		if(pElem->Rank != NULL) {
			free(pElem->Rank);
			pElem->Rank = NULL;
		};
		if(pElem->Type != NULL) {
			free(pElem->Type);
			pElem->Type = NULL;
		};
		if(pElem->NodeIndex != NULL) {
			free(pElem->NodeIndex);
			pElem->NodeIndex = NULL;
		};
		if(pElem->Node != NULL) {
			free(pElem->Node);
			pElem->Node = NULL;
		};
		if(pElem->Gravity != NULL) {
			free(pElem->Gravity);
			pElem->Gravity = NULL;
		};
	};

	/* number of elements */
	pElem->Count = nElement;
	if(pElem->Count <= 0) { return 0; };

	/* element ID */
	BufSize = sizeof(int) * pElem->Count;
	pElem->ID = (int *)malloc(BufSize);
	if(pElem->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element ID");
		goto Error;
	};

	/* element rank */
	BufSize = sizeof(int) * pElem->Count;
	pElem->Rank = (int *)malloc(BufSize);
	if(pElem->Rank == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element rank");
		goto Error;
	};

	/* element type */
	BufSize = sizeof(enum ppohVIS_BASE_eTopology) * pElem->Count;
	pElem->Type = (enum ppohVIS_BASE_eTopology *)malloc(BufSize);
	if(pElem->Type == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element type");
		goto Error;
	};

	/* index of connectivity */
	BufSize = sizeof(int) * (pElem->Count + 1);
	pElem->NodeIndex = (int *)malloc(BufSize);
	if(pElem->NodeIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of connectivity");
		goto Error;
	};

	/* connectivity */
	/* cannot determine buffer size */
	pElem->Node = NULL;

	/* gravity */
	BufSize = sizeof(double) * pElem->Count * 3;
	pElem->Gravity = (double *)malloc(BufSize);
	if(pElem->Gravity == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "gravity");
		goto Error;
	};

#pragma omp parallel for
	for(i=0; i<pElem->Count; i++) {
		pElem->ID[i]          = 0;
		pElem->Rank[i]        = 0;
		pElem->Type[i]        = ppohVIS_BASE_Unknown;
		pElem->NodeIndex[i]   = 0;
		pElem->Gravity[3*i]   = 0.0;
		pElem->Gravity[3*i+1] = 0.0;
		pElem->Gravity[3*i+2] = 0.0;
	};
	pElem->NodeIndex[pElem->Count] = 0;

	return 0;

Error:
	return -1;
};

/*============================================================================*/
extern int
ppohVIS_BASE_InitMeshComm(
	struct ppohVIS_BASE_stMeshComm *pComm, int nNeighbor)
{
	size_t BufSize;
	int BufCount;

	/* allocate check */
	if(pComm == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "neighbor info");
		goto Error;
	} else {
		if(pComm->Neighbor != NULL) {
			free(pComm->Neighbor);
			pComm->Neighbor = NULL;
		};
		if(pComm->ExportIndex != NULL) {
			free(pComm->ExportIndex);
			pComm->ExportIndex = NULL;
		};
		if(pComm->ExportItem != NULL) {
			free(pComm->ExportItem);
			pComm->ExportItem = NULL;
		};
		if(pComm->ImportIndex != NULL) {
			free(pComm->ImportIndex);
			pComm->ImportIndex = NULL;
		};
		if(pComm->ImportItem != NULL) {
			free(pComm->ImportItem);
			pComm->ImportItem = NULL;
		};
		if(pComm->SharedIndex != NULL) {
			free(pComm->SharedIndex);
			pComm->SharedIndex = NULL;
		};
		if(pComm->SharedItem != NULL) {
			free(pComm->SharedItem);
			pComm->SharedItem = NULL;
		};
	};

	/* number of neighboring regions */
	pComm->NeighborCount = nNeighbor;

	/* neighboring region */
	if(pComm->NeighborCount > 0) {
		BufSize = sizeof(int);
		BufCount = pComm->NeighborCount;
		pComm->Neighbor = (int *)calloc(BufCount, BufSize);
		if(pComm->Neighbor == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "neighbor pe");
			goto Error;
		};
	};

	/* export index */
	BufSize = sizeof(int);
	BufCount = pComm->NeighborCount + 1;
	pComm->ExportIndex = (int *)calloc(BufCount, BufSize);
	if(pComm->ExportIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of export item");
		goto Error;
	};

	/* export item */
	/* cannot determine buffer size */

	/* import index */
	BufSize = sizeof(int);
	BufCount = pComm->NeighborCount + 1;
	pComm->ImportIndex = (int *)calloc(BufCount, BufSize);
	if(pComm->ImportIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of import item");
		goto Error;
	};

	/* import item */
	/* cannot determine buffer size */

	/* shared index */
	BufSize = sizeof(int);
	BufCount = pComm->NeighborCount + 1;
	pComm->SharedIndex = (int *)calloc(BufCount, BufSize);
	if(pComm->SharedIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of shared item");
		goto Error;
	};

	/* shared item */
	/* cannot determine buffer size */

	return 0;

Error:
	return -1;
};

/*============================================================================*/
extern int
ppohVIS_BASE_InitMesh(
	struct ppohVIS_BASE_stMesh *pMesh, int nNode, int nElement, int nNeighbor)
{
	/* allocation check */
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh info");
		goto Error;
	};

	/* node info */
	if(ppohVIS_BASE_InitMeshNode(pMesh->Node, nNode)) {
		goto Error;
	};

	/* element info */
	if(ppohVIS_BASE_InitMeshElement(pMesh->Element, nElement)) {
		goto Error;
	};

	/* comm. info */
	if(ppohVIS_BASE_InitMeshComm(pMesh->Comm, nNeighbor)) {
		goto Error;
	};

	return 0;

Error:
	return -1;
};


/*******************************************************************************
 * Free
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeMeshNode(
	struct ppohVIS_BASE_stMeshNode *pNode)
{
	if(pNode == NULL) return;

	if(pNode->ID != NULL) {
		free(pNode->ID);
		pNode->ID = NULL;
	};
	if(pNode->Rank != NULL) {
		free(pNode->Rank);
		pNode->Rank = NULL;
	};
	if(pNode->Coords != NULL) {
		free(pNode->Coords);
		pNode->Coords = NULL;
	};

	pNode->Count = 0;

	free(pNode);
	pNode = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeMeshElement(
	struct ppohVIS_BASE_stMeshElement *pElem)
{
	if(pElem == NULL) return;

	if(pElem->ID != NULL) {
		free(pElem->ID);
		pElem->ID = NULL;
	};
	if(pElem->Rank != NULL) {
		free(pElem->Rank);
		pElem->Rank = NULL;
	};
	if(pElem->Type != NULL) {
		free(pElem->Type);
		pElem->Type = NULL;
	};
	if(pElem->NodeIndex != NULL) {
		free(pElem->NodeIndex);
		pElem->NodeIndex = NULL;
	};
	if(pElem->Node != NULL) {
		free(pElem->Node);
		pElem->Node = NULL;
	};
	if(pElem->Gravity != NULL) {
		free(pElem->Gravity);
		pElem->Gravity = NULL;
	};

	pElem->Count = 0;

	free(pElem);
	pElem = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeMeshComm(
	struct ppohVIS_BASE_stMeshComm *pComm)
{
	if(pComm == NULL) return;

	if(pComm->Neighbor != NULL) {
		free(pComm->Neighbor);
		pComm->Neighbor = NULL;
	};
	if(pComm->ExportIndex != NULL) {
		free(pComm->ExportIndex);
		pComm->ExportIndex = NULL;
	};
	if(pComm->ExportItem != NULL) {
		free(pComm->ExportItem);
		pComm->ExportItem = NULL;
	};
	if(pComm->ImportIndex != NULL) {
		free(pComm->ImportIndex);
		pComm->ImportIndex = NULL;
	};
	if(pComm->ImportItem != NULL) {
		free(pComm->ImportItem);
		pComm->ImportItem = NULL;
	};
	if(pComm->SharedIndex != NULL) {
		free(pComm->SharedIndex);
		pComm->SharedIndex = NULL;
	};
	if(pComm->SharedItem != NULL) {
		free(pComm->SharedItem);
		pComm->SharedItem = NULL;
	};

	pComm->NeighborCount = 0;

	free(pComm);
	pComm = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeMesh(
	struct ppohVIS_BASE_stMesh *pMesh)
{
	if(pMesh == NULL) return;

	if(pMesh->Node != NULL) {
		ppohVIS_BASE_FreeMeshNode(pMesh->Node);
	};
	if(pMesh->Element != NULL) {
		ppohVIS_BASE_FreeMeshElement(pMesh->Element);
	};
	if(pMesh->Comm != NULL) {
		ppohVIS_BASE_FreeMeshComm(pMesh->Comm);
	};

	free(pMesh);
	pMesh = NULL;
};


/*******************************************************************************
 * Calculate gravity of all elements
 ******************************************************************************/
extern int
ppohVIS_BASE_CalculateGravity(
	struct ppohVIS_BASE_stMesh *pMesh)
{
	struct ppohVIS_BASE_stMeshNode *pNode;
	struct ppohVIS_BASE_stMeshElement *pElem;
	double GX, GY, GZ;
	int Node;
	int iNode, iElem, iNodeS, iNodeE;

	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh info");
		goto error;
	};
	if(pMesh->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info");
		goto error;
	};
	if(pMesh->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info");
		goto error;
	};
	pNode = pMesh->Node;
	pElem = pMesh->Element;

	if(pElem->Gravity != NULL) {
		free(pElem->Gravity);
	};
	pElem->Gravity = (double *)malloc(sizeof(double)*pElem->Count*3);
	if(pElem->Gravity == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "gravity of element");
		goto error;
	};

#pragma omp parallel for private (iNodeS,iNodeE,GX,GY,GZ,iNode,Node)
	for(iElem=0; iElem<pElem->Count; iElem++) {
		iNodeS = pElem->NodeIndex[iElem];
		iNodeE = pElem->NodeIndex[iElem+1];
		GX = 0.0;
		GY = 0.0;
		GZ = 0.0;
		for(iNode=iNodeS; iNode<iNodeE; iNode++) {
			Node = pElem->Node[iNode];
			GX += pNode->Coords[3*Node-3];
			GY += pNode->Coords[3*Node-2];
			GZ += pNode->Coords[3*Node-1];
		};
		pElem->Gravity[3*iElem  ] = GX / (double)(iNodeE - iNodeS);
		pElem->Gravity[3*iElem+1] = GY / (double)(iNodeE - iNodeS);
		pElem->Gravity[3*iElem+2] = GZ / (double)(iNodeE - iNodeS);
	};

	return 0;

error:
	return -1;
};


/*******************************************************************************
 * Print mesh information
 ******************************************************************************/
extern void
ppohVIS_BASE_PrintMeshNodeTetGen(
	FILE *fp, struct ppohVIS_BASE_stMeshNode *pNode)
{
	int iNode;

	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info");
		return;
	};

	fprintf(fp, "%10d%10d%10d%10d\n", pNode->Count, 3, 0, 0);
	for(iNode=0; iNode<pNode->Count; iNode++) {
		fprintf(fp, "%10d%15.7e%15.7e%15.7e\n",
			pNode->ID[iNode],
			pNode->Coords[3*iNode],
			pNode->Coords[3*iNode+1],
			pNode->Coords[3*iNode+2]);
	};

	return;
};


/*============================================================================*/
extern void
ppohVIS_BASE_PrintMeshNode(
	FILE *fp, struct ppohVIS_BASE_stMeshNode *pNode)
{
	int iNode;

	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info");
		return;
	};

	fprintf(fp, "%10d\n", pNode->Count);
	for(iNode=0; iNode<pNode->Count; iNode++) {
		fprintf(fp, "%10d%10d%15.7e%15.7e%15.7e\n",
			pNode->ID[iNode],
			pNode->Rank[iNode],
			pNode->Coords[3*iNode],
			pNode->Coords[3*iNode+1],
			pNode->Coords[3*iNode+2]);
	};

	return;
};


/*============================================================================*/
extern void
ppohVIS_BASE_PrintMeshElement(
	FILE *fp, struct ppohVIS_BASE_stMeshElement *pElem)
{
	int iElem, iNode;

	if(pElem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info");
		return;
	};

	fprintf(fp, "%10d\n", pElem->Count);
	for(iElem=0; iElem<pElem->Count; iElem++) {
		fprintf(fp, "%10d%10d%3d%15.7e%15.7e%15.7e",
			pElem->ID[iElem],
			pElem->Rank[iElem],
			pElem->Type[iElem],
			pElem->Gravity[3*iElem],
			pElem->Gravity[3*iElem+1],
			pElem->Gravity[3*iElem+2]);
		for(iNode=pElem->NodeIndex[iElem]; iNode<pElem->NodeIndex[iElem+1]; iNode++) {
			fprintf(fp, "%10d", pElem->Node[iNode]);
		};
		fprintf(fp, "\n");
	};
};


/*============================================================================*/
extern void
ppohVIS_BASE_PrintMesh(
	FILE *fp, struct ppohVIS_BASE_stMesh *pMesh)
{
	struct ppohVIS_BASE_stMeshNode *pNode;
	struct ppohVIS_BASE_stMeshElement *pElem;
	struct ppohVIS_BASE_stMeshComm *pComm;
	int iNode, iElem;

	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "mesh info");
		return;
	};

	/* node info. */
	if(pMesh->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info");
		return;
	};
	pNode = pMesh->Node;

	fprintf(fp, "%10d\n", pNode->Count);
	for(iNode=0; iNode<pNode->Count; iNode++) {
		fprintf(fp, "%10d%10d%15.7e%15.7e%15.7e\n",
			pNode->ID[iNode],
			pNode->Rank[iNode],
			pNode->Coords[3*iNode],
			pNode->Coords[3*iNode+1],
			pNode->Coords[3*iNode+2]);
	};

	/* element info. */
	if(pMesh->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info");
		return;
	};
	pElem = pMesh->Element;

	fprintf(fp, "%10d\n", pElem->Count);
	for(iElem=0; iElem<pElem->Count; iElem++) {
		fprintf(fp, "%10d%10d%3d%15.7e%15.7e%15.7e",
			pElem->ID[iElem],
			pElem->Rank[iElem],
			pElem->Type[iElem],
			pElem->Gravity[3*iElem],
			pElem->Gravity[3*iElem+1],
			pElem->Gravity[3*iElem+2]);
		for(iNode=pElem->NodeIndex[iElem]; iNode<pElem->NodeIndex[iElem+1]; iNode++) {
			fprintf(fp, "%10d", pElem->Node[iNode]);
		};
		fprintf(fp, "\n");
	};

	/* comm. info. */
	if(pMesh->Comm == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "comm. info");
		return;
	};
	pComm = pMesh->Comm;

	fprintf(fp, "%10d\n", pComm->NeighborCount);

	return;
};


/*******************************************************************************
 * Print mesh information to a file
 ******************************************************************************/
extern int
ppohVIS_BASE_PutMeshNodeTetGen(
	char *FileName, struct ppohVIS_BASE_stMeshNode *pNode, int Append)
{
	FILE *fp = NULL;

	/* Open file */
	if(Append == 0) {
		if((fp = fopen(FileName, "w")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh node file");
			goto error;
		};
	} else {
		if((fp = fopen(FileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh node file");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintMeshNodeTetGen(fp, pNode);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "mesh node file");
		goto error;
	};

	return 0;

error:
	return -1;
};


/*============================================================================*/
extern int
ppohVIS_BASE_PutMeshNode(
	char *FileName, struct ppohVIS_BASE_stMeshNode *pNode, int Append)
{
	FILE *fp = NULL;

	/* Open file */
	if(Append == 0) {
		if((fp = fopen(FileName, "w")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh node file");
			goto error;
		};
	} else {
		if((fp = fopen(FileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh node file");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintMeshNode(fp, pNode);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "mesh node file");
		goto error;
	};

	return 0;

error:
	return -1;
};


/*============================================================================*/
extern int
ppohVIS_BASE_PutMeshElement(
	char *FileName, struct ppohVIS_BASE_stMeshElement *pElem, int Append)
{
	FILE *fp = NULL;

	/* Open file */
	if(Append == 0) {
		if((fp = fopen(FileName, "w")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh element file");
			goto error;
		};
	} else {
		if((fp = fopen(FileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh element file");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintMeshElement(fp, pElem);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "mesh element file");
		goto error;
	};

	return 0;

error:
	return -1;
};


/*============================================================================*/
extern int
ppohVIS_BASE_PutMesh(
	char *FileName, struct ppohVIS_BASE_stMesh *pMesh, int Append)
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
				"%s [%s]", strerror(errno), "mesh file");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "mesh file");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintMesh(fp, pMesh);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "mesh file");
		goto error;
	};

	return 0;

error:
	return -1;
};

