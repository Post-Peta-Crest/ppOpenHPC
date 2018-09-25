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
#include <float.h>

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_Edge.h"

struct stHash {
	int Key;
	struct stHash *Next;
};

static unsigned long int Threshold = 10000000;
static float ExpandFactor = 1.2;

static unsigned long int HashSize;
static unsigned long int BuffSize;
static struct stHash **HashTable = NULL;
static int *Nodes = NULL;
static int *Refered = NULL;
static int EdgeCount;


/*******************************************************************************
 * Create edge information
 ******************************************************************************/
/*================*
 * Initialization *
 *================*/
static int
Init(int Capacity)
{
	int i;

	if(Capacity < Threshold) {
		HashSize = Capacity;
		BuffSize = Capacity * 10;
	} else {
		HashSize = Capacity;
		BuffSize = Capacity;
	};

	HashTable = (struct stHash **)malloc(sizeof(struct stHash *)*HashSize);
	if(HashTable == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "edge hash table");
		return -1;
	};
	for(i=0; i<HashSize; i++) {
		HashTable[i] = NULL;
	};

	Nodes = (int *)calloc(BuffSize*2, sizeof(int));
	if(Nodes == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "connectivitiy");
		return -1;
	};

	Refered = (int *)calloc(BuffSize, sizeof(int));
	if(Refered == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "reference count");
		return -1;
	};

	EdgeCount = 0;

	return 0;
}


/*==============*
 * Finalization *
 *==============*/
static void
Finalize()
{
	struct stHash *p, *q;
	int i;

	if(HashTable && HashSize > 0) {
		for(i=0; i<HashSize; i++) {
			if(HashTable[i]) {
				for(q=HashTable[i], p=HashTable[i]; p; p=q) {
					q = q->Next;
					free(p);
				};
				HashTable[i] = NULL;
			};
		};
		free(HashTable);
	};

	if(Nodes) {
		free(Nodes);
	};

	if(Refered) {
		free(Refered);
	};

	HashTable = NULL;
	Nodes = NULL;
	Refered = NULL;
	EdgeCount = 0;
}


/*================================*
 * Expand array of connectivities *
 *================================*/
static int
Expand(void)
{
	unsigned long int NewSize;
	int i;

	NewSize = (unsigned long int)(BuffSize * ExpandFactor);

	Nodes = (int *)realloc(Nodes, sizeof(int)*NewSize*2);
	if(Nodes == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "connectivitiy");
		return -1;
	};

	Refered = (int *)realloc(Refered, sizeof(int)*NewSize);
	if(Refered == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "reference count");
		return -1;
	};
	for(i=BuffSize; i<NewSize; i++) {
		Refered[i] = 0;
	};

	BuffSize = NewSize;

	return 0;
}


/*=====================================*
 * Arrange node IDs in ascending order *
 *=====================================*/
static void
Reorder(int Node1, int Node2, int *N1, int *N2)
{
	if(Node1 < Node2) {
		*N1 = Node1;
		*N2 = Node2;
	} else {
		*N1 = Node2;
		*N2 = Node1;
	};
}


/*=============*
 * Search edge *
 *=============*/
static int
Search(int Node1, int Node2)
{
	int N1, N2, EdgeID;
	int Index;
	unsigned long int Dot;
	struct stHash *p;

	Reorder(Node1, Node2, &N1, &N2);

	Dot = ((unsigned long int)N1 % HashSize) *
	      ((unsigned long int)N2 % HashSize);
	Index = Dot % HashSize;

	for(p=HashTable[Index]; p; p=p->Next) {
		EdgeID = p->Key;
		if(N1 == Nodes[2*EdgeID-2] && N2 == Nodes[2*EdgeID-1]) {
			Refered[EdgeID-1]++;
			return EdgeID;
		};
	};

	p = (struct stHash *)malloc(sizeof(struct stHash));
	if(p == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "hash link");
		return -1;
	};
	p->Next = HashTable[Index];
	HashTable[Index] = p;

	if(EdgeCount >= BuffSize) {
		if(Expand()) {
			return -1;
		};
	};

	EdgeCount++;
	EdgeID = EdgeCount;
	p->Key = EdgeID;
	Nodes[2*EdgeID-2] = N1;
	Nodes[2*EdgeID-1] = N2;
	Refered[EdgeID-1] = 1;

	return EdgeID;
}


/*==========================================*
 * Create quadrilateral surface information *
 *==========================================*/
extern struct ppohVIS_BASE_stGeometryItem *
ppohVIS_BASE_SearchEdge(
	struct ppohVIS_BASE_stMeshElement *pElem)
{
	struct ppohVIS_BASE_stGeometryItem *pEdge;
	int iElem, iEdge, iType, iNodeS;
	size_t BufSize;

	if(pElem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info");
		return NULL;
	};

	Init(pElem->Count);

	for(iElem=0; iElem<pElem->Count; iElem++) {
		iType = pElem->Type[iElem];
		iNodeS = pElem->NodeIndex[iElem];

		switch (iType) {
			case ppohVIS_BASE_Tetra4:
				Search(pElem->Node[iNodeS+0],
				       pElem->Node[iNodeS+1]);
				Search(pElem->Node[iNodeS+1],
				       pElem->Node[iNodeS+2]);
				Search(pElem->Node[iNodeS+2],
				       pElem->Node[iNodeS+0]);
				Search(pElem->Node[iNodeS+0],
				       pElem->Node[iNodeS+3]);
				Search(pElem->Node[iNodeS+1],
				       pElem->Node[iNodeS+3]);
				Search(pElem->Node[iNodeS+2],
				       pElem->Node[iNodeS+3]);
				break;

			case ppohVIS_BASE_Penta6:
				Search(pElem->Node[iNodeS+0],
				       pElem->Node[iNodeS+1]);
				Search(pElem->Node[iNodeS+1],
				       pElem->Node[iNodeS+2]);
				Search(pElem->Node[iNodeS+2],
				       pElem->Node[iNodeS+0]);
				Search(pElem->Node[iNodeS+3],
				       pElem->Node[iNodeS+4]);
				Search(pElem->Node[iNodeS+4],
				       pElem->Node[iNodeS+5]);
				Search(pElem->Node[iNodeS+5],
				       pElem->Node[iNodeS+3]);
				Search(pElem->Node[iNodeS+0],
				       pElem->Node[iNodeS+3]);
				Search(pElem->Node[iNodeS+1],
				       pElem->Node[iNodeS+4]);
				Search(pElem->Node[iNodeS+2],
				       pElem->Node[iNodeS+5]);
				break;

			case ppohVIS_BASE_Hexa8:
				Search(pElem->Node[iNodeS+0],
				       pElem->Node[iNodeS+1]);
				Search(pElem->Node[iNodeS+1],
				       pElem->Node[iNodeS+2]);
				Search(pElem->Node[iNodeS+2],
				       pElem->Node[iNodeS+3]);
				Search(pElem->Node[iNodeS+3],
				       pElem->Node[iNodeS+0]);
				Search(pElem->Node[iNodeS+4],
				       pElem->Node[iNodeS+5]);
				Search(pElem->Node[iNodeS+5],
				       pElem->Node[iNodeS+6]);
				Search(pElem->Node[iNodeS+6],
				       pElem->Node[iNodeS+7]);
				Search(pElem->Node[iNodeS+7],
				       pElem->Node[iNodeS+4]);
				Search(pElem->Node[iNodeS+0],
				       pElem->Node[iNodeS+4]);
				Search(pElem->Node[iNodeS+1],
				       pElem->Node[iNodeS+5]);
				Search(pElem->Node[iNodeS+2],
				       pElem->Node[iNodeS+6]);
				Search(pElem->Node[iNodeS+3],
				       pElem->Node[iNodeS+7]);
				break;

			default:
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_InvalidElemType,
					"%d", iType);
				break;
		};
	};

	BufSize = sizeof(struct ppohVIS_BASE_stGeometryItem);
	pEdge = (struct ppohVIS_BASE_stGeometryItem *)malloc(BufSize);
	if(pEdge == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "ppohVIS_BASE_stGeometryItem");
		return NULL;
	};
	pEdge->ID = (int *)calloc(EdgeCount, sizeof(int *));
	if(pEdge->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "edge ID");
		return NULL;
	};
	pEdge->Refered = (int *)calloc(EdgeCount, sizeof(int));
	if(pEdge->Refered == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "reference count");
		return NULL;
	};
	pEdge->Node = (int *)calloc(EdgeCount*2, sizeof(int));
	if(pEdge->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "edge connectivity");
		return NULL;
	};

	pEdge->Count = EdgeCount;
	pEdge->Type = ppohVIS_BASE_Line2;
	memcpy(pEdge->Node, Nodes, sizeof(int)*EdgeCount*2);
	memcpy(pEdge->Refered, Refered, sizeof(int)*EdgeCount);
	for(iEdge=0; iEdge<EdgeCount; iEdge++) {
		pEdge->ID[iEdge] = iEdge + 1;
	};

	Finalize();

	return pEdge;
};


/*******************************************************************************
 * Search maximum and minimum length of edge
 ******************************************************************************/
extern int
ppohVIS_BASE_GetEdgeLengthMinMax(
	struct ppohVIS_BASE_stMeshNode *pNode,
	struct ppohVIS_BASE_stGeometryItem *pEdge,
	double *MinLength, double *MaxLength)
{
	double X1, X2, Y1, Y2, Z1, Z2;
	double Len, Len2;
	int Node1, Node2, iEdge;

	if(pNode == NULL || pEdge == NULL) {
		*MinLength = 0.0;
		*MaxLength = 0.0;
		return 0;
	};

	*MinLength = DBL_MAX;
	*MaxLength = -DBL_MAX;

	for(iEdge=0; iEdge<pEdge->Count; iEdge++) {
		Node1 = pEdge->Node[2*iEdge];
		Node2 = pEdge->Node[2*iEdge+1];

		X1 = pNode->Coords[3*Node1-3];
		Y1 = pNode->Coords[3*Node1-2];
		Z1 = pNode->Coords[3*Node1-1];
		X2 = pNode->Coords[3*Node2-3];
		Y2 = pNode->Coords[3*Node2-2];
		Z2 = pNode->Coords[3*Node2-1];

		Len2 = (X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2) + (Z1-Z2)*(Z1-Z2);
		Len = sqrt(Len2);

		if(Len < *MinLength) *MinLength = Len;
		if(Len > *MaxLength) *MaxLength = Len;
	};

	return 0;
};


/*******************************************************************************
 * Print edge information
 ******************************************************************************/
extern void
ppohVIS_BASE_PrintEdge(
	FILE *fp, struct ppohVIS_BASE_stGeometryItem *pEdge)
{
	int iEdge;

	fprintf(fp, "%10d%3d\n", pEdge->Count, pEdge->Type);
	for(iEdge=0; iEdge<pEdge->Count; iEdge++) {
		fprintf(fp, "%10d%10d%10d%10d\n",
				pEdge->ID[iEdge],
				pEdge->Refered[iEdge],
				pEdge->Node[2*iEdge],
				pEdge->Node[2*iEdge+1]);
	};

	return;
};


/*******************************************************************************
 * Print edge information to a file
 ******************************************************************************/
extern int
ppohVIS_BASE_PutEdge(
	char *FileName, struct ppohVIS_BASE_stGeometryItem *pEdge, int Append)
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
				"%s [%s]", strerror(errno), "edge info.");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]", strerror(errno), "edge info.");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintEdge(fp, pEdge);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "edge info.");
		goto error;
	};

	return 0;

error:
	return -1;
};

