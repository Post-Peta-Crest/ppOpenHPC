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

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_TriangularSurface.h"

struct stHash {
	int Key;
	struct stHash *Next;
};

static unsigned long int Threshold = 10000000;
static float ExpandFactor = 1.1;

static unsigned long int HashSize;
static unsigned long int BuffSize;
static struct stHash **HashTable = NULL;
static int *Nodes = NULL;
static int *Elements = NULL;
static int *Refered = NULL;
static int TsufCount;


/*******************************************************************************
 * Create triangular surface information
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
		BuffSize = Capacity * 4;
	} else {
		HashSize = Capacity;
		BuffSize = Capacity;
	};

	HashTable = (struct stHash **)malloc(sizeof(struct stMesh *)*HashSize);
	if(HashTable == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "triangular surface table");
		return -1;
	};
	for(i=0; i<HashSize; i++) {
		HashTable[i] = NULL;
	};

	Nodes = (int *)calloc(BuffSize*3, sizeof(int));
	if(Nodes == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "connectivity");
		return -1;
	};

	Elements = (int *)calloc(BuffSize*2, sizeof(int));
	if(Elements == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "attached elements");
		return -1;
	};

	Refered = (int *)calloc(BuffSize, sizeof(int));
	if(Refered == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "reference count");
		return -1;
	};

	TsufCount = 0;

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

	if(Elements) {
		free(Elements);
	};

	if(Refered) {
		free(Refered);
	};

	HashTable = NULL;
	Nodes = NULL;
	Elements = NULL;
	Refered = NULL;
	TsufCount = 0;
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

	Nodes = (int *)realloc(Nodes, sizeof(int)*NewSize*3);
	if(Nodes == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "connectivity");
		return -1;
	};

	Elements = (int *)realloc(Elements, sizeof(int)*NewSize*2);
	if(Elements == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "attached elements");
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
		Elements[2*i] = 0;
		Elements[2*i+1] = 0;
	};

	BuffSize = NewSize;

	return 0;
}


/*=====================================*
 * Arrange node IDs in ascending order *
 *=====================================*/
static void
Reorder(int Node1, int Node2, int Node3, int *N1, int *N2, int *N3)
{
	int M1, M2, M3;

	if(Node1 < Node2) {
		M1 = Node1;
		M2 = Node2;
	} else {
		M1 = Node2;
		M2 = Node1;
	};

	if(Node3 < M1) {
		*N1 = Node3;
		M3 = M1;
	} else {
		*N1 = M1;
		M3 = Node3;
	};

	if(M2 < M3) {
		*N2 = M2;
		*N3 = M3;
	} else {
		*N2 = M3;
		*N3 = M2;
	};
}


/*===========================*
 * Search triangluar surface *
 *===========================*/
static int
Search(int Node1, int Node2, int Node3)
{
	int N1, N2, N3, M1, M2, M3, TsufID;
	int Index;
	unsigned long int Dot1, Dot2;
	struct stHash *p;

	Reorder(Node1, Node2, Node3, &N1, &N2, &N3);

	Dot1 = ((unsigned long int)N1 % HashSize) *
	       ((unsigned long int)N2 % HashSize);
	Dot2 = ((unsigned long int)N3 % HashSize) * (Dot1 % HashSize);
	Index = Dot2 % HashSize;

	for(p=HashTable[Index]; p; p=p->Next) {
		TsufID = p->Key;

		Reorder(Nodes[3*TsufID-3], Nodes[3*TsufID-2],
		        Nodes[3*TsufID-1], &M1, &M2, &M3);

		if(N1 == M1 && N2 == M2 && N3 == M3) {
			Refered[TsufID-1]++;
			return TsufID;
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

	if(TsufCount >= BuffSize) {
		if(Expand()) {
			return -1;
		};
	};

	TsufCount++;
	TsufID = TsufCount;
	p->Key = TsufID;
	Nodes[3*TsufID-3] = Node1;
	Nodes[3*TsufID-2] = Node2;
	Nodes[3*TsufID-1] = Node3;
	Refered[TsufID-1] = 1;

	return TsufID;
}


/*=======================================*
 * Create triangular surface information *
 *=======================================*/
extern struct ppohVIS_BASE_stGeometryItem *
ppohVIS_BASE_SearchTriangularSurface(
	struct ppohVIS_BASE_stMeshElement *pElem)
{
	struct ppohVIS_BASE_stGeometryItem *pTsuf;
	int iElem, iTsuf, iType, iNodeS, TsufID;
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
				TsufID = Search(pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+0]);
				Elements[2*TsufID-3+Refered[TsufID-1]] = iElem+1;
				TsufID = Search(pElem->Node[iNodeS+0],
						pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+3]);
				Elements[2*TsufID-3+Refered[TsufID-1]] = iElem+1;
				TsufID = Search(pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+3]);
				Elements[2*TsufID-3+Refered[TsufID-1]] = iElem+1;
				TsufID = Search(pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+0],
						pElem->Node[iNodeS+3]);
				Elements[2*TsufID-3+Refered[TsufID-1]] = iElem+1;
				break;

			case ppohVIS_BASE_Penta6:
				TsufID = Search(pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+0]);
				Elements[2*TsufID-3+Refered[TsufID-1]] = iElem+1;
				TsufID = Search(pElem->Node[iNodeS+3],
						pElem->Node[iNodeS+4],
						pElem->Node[iNodeS+5]);
				Elements[2*TsufID-3+Refered[TsufID-1]] = iElem+1;
				break;

			case ppohVIS_BASE_Hexa8:
				break;

			default:
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_InvalidElemType,
					"%d", iType);
				break;
		};
	};

	BufSize = sizeof(struct ppohVIS_BASE_stGeometryItem);
	pTsuf = (struct ppohVIS_BASE_stGeometryItem *)malloc(BufSize);
	if(pTsuf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "ppohVIS_BASE_stGeometryItem");
		return NULL;
	};

	pTsuf->Count = TsufCount;
	pTsuf->Type = ppohVIS_BASE_Tria3;
	if(pTsuf->Count > 0) {
		pTsuf->ID = (int *)calloc(TsufCount, sizeof(int *));
		if(pTsuf->ID == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "triangular surface ID");
			return NULL;
		};
		pTsuf->Refered = (int *)calloc(TsufCount, sizeof(int));
		if(pTsuf->Refered == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "reference count");
			return NULL;
		};
		pTsuf->Node = (int *)calloc(TsufCount*3, sizeof(int));
		if(pTsuf->Node == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "triangle connectivity");
			return NULL;
		};
		pTsuf->Element = (int *)calloc(TsufCount*2, sizeof(int));
		if(pTsuf->Element == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "attached elements");
			return NULL;
		};

		memcpy(pTsuf->Node, Nodes, sizeof(int)*TsufCount*3);
		memcpy(pTsuf->Refered, Refered, sizeof(int)*TsufCount);
		memcpy(pTsuf->Element, Elements, sizeof(int)*TsufCount*2);
		for(iTsuf=0; iTsuf<TsufCount; iTsuf++) {
			pTsuf->ID[iTsuf] = iTsuf + 1;
		};
	} else {
		pTsuf->ID = NULL;
		pTsuf->Refered = NULL;
		pTsuf->Node = NULL;
		pTsuf->Element = NULL;
	};

	Finalize();

	return pTsuf;
};


/*******************************************************************************
 * Print triangluar surface information
 ******************************************************************************/
extern void
ppohVIS_BASE_PrintTriangularSurface(
	FILE *fp, struct ppohVIS_BASE_stGeometryItem *pTsuf)
{
	int iTsuf;

	fprintf(fp, "%10d%3d\n", pTsuf->Count, pTsuf->Type);
	for(iTsuf=0; iTsuf<pTsuf->Count; iTsuf++) {
		fprintf(fp, "%10d%10d%10d%10d%10d%10d%10d\n",
				pTsuf->ID[iTsuf],
				pTsuf->Refered[iTsuf],
				pTsuf->Node[3*iTsuf],
				pTsuf->Node[3*iTsuf+1],
				pTsuf->Node[3*iTsuf+2],
				pTsuf->Element[2*iTsuf],
				pTsuf->Element[2*iTsuf+1]);
	};

	return;
};


/*******************************************************************************
 * Print triangluar surface information to a file
 ******************************************************************************/
extern int
ppohVIS_BASE_PutTriangularSurface(
	char *FileName, struct ppohVIS_BASE_stGeometryItem *pTsuf, int Append)
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
				strerror(errno), "triangular surface info.");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]",
				strerror(errno), "triangular surface info.");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintTriangularSurface(fp, pTsuf);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError, "%s [%s]",
			strerror(errno), "triangular surface info.");
		goto error;
	};

	return 0;

error:
	return -1;
};
