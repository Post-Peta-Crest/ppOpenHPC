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
#include "ppohVIS_BASE_QuadrilateralSurface.h"

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
static int QsufCount;


/*******************************************************************************
 * Create quadrilateral surface information
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

	Nodes = (int *)calloc(BuffSize*4, sizeof(int));
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

	QsufCount = 0;

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
	Refered = NULL;
	QsufCount = 0;
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

	Nodes = (int *)realloc(Nodes, (size_t)(sizeof(int)*NewSize*4));
	if(Nodes == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "connectivity");
		return -1;
	};

	Elements = (int *)realloc(Elements, (size_t)(sizeof(int)*NewSize*2));
	if(Elements == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "attached elements");
		return -1;
	};

	Refered = (int *)realloc(Refered, (size_t)(sizeof(int)*NewSize));
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
Reorder(int Node1, int Node2, int Node3, int Node4,
        int *N1, int *N2, int *N3, int *N4)
{
	int L1, L2, L3, L4, L5, L6;

	if(Node1 < Node2) {
		L1 = Node1;
		L2 = Node2;
	} else {
		L1 = Node2;
		L2 = Node1;
	};

	if(Node3 < Node4) {
		L3 = Node3;
		L4 = Node4;
	} else {
		L3 = Node4;
		L4 = Node3;
	};

	if(L1 < L3) {
		*N1 = L1;
		L5 = L3;
	} else {
		*N1 = L3;
		L5 = L1;
	};

	if(L2 > L4) {
		*N4 = L2;
		L6 = L4;
	} else {
		*N4 = L4;
		L6 = L2;
	};

	if(L5 < L6) {
		*N2 = L5;
		*N3 = L6;
	} else {
		*N2 = L6;
		*N3 = L5;
	};
}

/*==============================*
 * Search quadrilateral surface *
 *==============================*/
static int
Search(int Node1, int Node2, int Node3, int Node4)
{
	int N1, N2, N3, N4, M1, M2, M3, M4, QsufID;
	int Index;
	unsigned long int Dot1, Dot2, Dot3;
	struct stHash *p;

	Reorder(Node1, Node2, Node3, Node4, &N1, &N2, &N3, &N4);

	Dot1 = ((unsigned long int)N1 % HashSize) *
	       ((unsigned long int)N2 % HashSize);
	Dot2 = ((unsigned long int)N3 % HashSize) *
	       ((unsigned long int)N4 % HashSize);
	Dot3 = (Dot1 % HashSize) * (Dot2 % HashSize);
	Index = Dot3 % HashSize;

	for(p=HashTable[Index]; p; p=p->Next) {
		QsufID = p->Key;

		Reorder(Nodes[4*QsufID-4], Nodes[4*QsufID-3],
		        Nodes[4*QsufID-2], Nodes[4*QsufID-1],
		        &M1, &M2, &M3, &M4);

		if(N1 == M1 && N2 == M2 && N3 == M3 && N4 == M4) {
			Refered[QsufID-1]++;
			return QsufID;
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

	if(QsufCount >= BuffSize) {
		if(Expand()) {
			return -1;
		};
	};

	QsufCount++;
	QsufID = QsufCount;
	p->Key = QsufID;
	Nodes[4*QsufID-4] = Node1;
	Nodes[4*QsufID-3] = Node2;
	Nodes[4*QsufID-2] = Node3;
	Nodes[4*QsufID-1] = Node4;
	Refered[QsufID-1] = 1;

	return QsufID;
}

/*==========================================*
 * Create quadrilateral surface information *
 *==========================================*/
extern struct ppohVIS_BASE_stGeometryItem *
ppohVIS_BASE_SearchQuadrilateralSurface(
	struct ppohVIS_BASE_stMeshElement *pElem)
{
	struct ppohVIS_BASE_stGeometryItem *pQsuf;
	int iElem, iQsuf, iType, iNodeS, QsufID;
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
				break;

			case ppohVIS_BASE_Penta6:
				QsufID = Search(pElem->Node[iNodeS+0],
						pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+4],
						pElem->Node[iNodeS+3]);
				Elements[2*QsufID-3+Refered[QsufID-1]] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+5],
						pElem->Node[iNodeS+4]);
				Elements[2*QsufID-3+Refered[QsufID-1]] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+0],
						pElem->Node[iNodeS+3],
						pElem->Node[iNodeS+5]);
				Elements[2*QsufID-3+Refered[QsufID-1]] = iElem+1;
				break;

			case ppohVIS_BASE_Hexa8:
				QsufID = Search(pElem->Node[iNodeS+3],
						pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+0]);
				Elements[2*(QsufID-1)+Refered[QsufID-1]-1] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+4],
						pElem->Node[iNodeS+5],
						pElem->Node[iNodeS+6],
						pElem->Node[iNodeS+7]);
				Elements[2*(QsufID-1)+Refered[QsufID-1]-1] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+0],
						pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+5],
						pElem->Node[iNodeS+4]);
				Elements[2*(QsufID-1)+Refered[QsufID-1]-1] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+1],
						pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+6],
						pElem->Node[iNodeS+5]);
				Elements[2*(QsufID-1)+Refered[QsufID-1]-1] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+2],
						pElem->Node[iNodeS+3],
						pElem->Node[iNodeS+7],
						pElem->Node[iNodeS+6]);
				Elements[2*(QsufID-1)+Refered[QsufID-1]-1] = iElem+1;
				QsufID = Search(pElem->Node[iNodeS+3],
						pElem->Node[iNodeS+0],
						pElem->Node[iNodeS+4],
						pElem->Node[iNodeS+7]);
				Elements[2*(QsufID-1)+Refered[QsufID-1]-1] = iElem+1;
				break;

			default:
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_InvalidElemType,
					"%d", iType);
				break;
		};
	};

	BufSize = sizeof(struct ppohVIS_BASE_stGeometryItem);
	pQsuf = (struct ppohVIS_BASE_stGeometryItem *)malloc(BufSize);
	if(pQsuf == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "ppohVIS_BASE_stGeometryItem");
		return NULL;
	};

	pQsuf->Count = QsufCount;
	pQsuf->Type = ppohVIS_BASE_Quad4;
	if(pQsuf->Count > 0) {
		pQsuf->ID = (int *)calloc(QsufCount, sizeof(int *));
		if(pQsuf->ID == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "quadrilateral surface ID");
			return NULL;
		};
		pQsuf->Refered = (int *)calloc(QsufCount, sizeof(int));
		if(pQsuf->Refered == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "reference count");
			return NULL;
		};
		pQsuf->Node = (int *)calloc(QsufCount*4, sizeof(int));
		if(pQsuf->Node == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "quadrilateral connectivity");
			return NULL;
		};
		pQsuf->Element = (int *)calloc(QsufCount*2, sizeof(int));
		if(pQsuf->Element == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s", "attached element");
			return NULL;
		};

		memcpy(pQsuf->Node, Nodes, sizeof(int)*QsufCount*4);
		memcpy(pQsuf->Refered, Refered, sizeof(int)*QsufCount);
		memcpy(pQsuf->Element, Elements, sizeof(int)*QsufCount*2);
		for(iQsuf=0; iQsuf<QsufCount; iQsuf++) {
			pQsuf->ID[iQsuf] = iQsuf + 1;
		};
	} else {
		pQsuf->ID = NULL;
		pQsuf->Refered = NULL;
		pQsuf->Node = NULL;
		pQsuf->Element = NULL;
	};

	Finalize();

	return pQsuf;
};


/*******************************************************************************
 * Print quadrilateral surface information
 ******************************************************************************/
extern void
ppohVIS_BASE_PrintQuadrilateralSurface(
	FILE *fp, struct ppohVIS_BASE_stGeometryItem *pQsuf)
{
	int iQsuf;

	fprintf(fp, "%10d%3d\n", pQsuf->Count, pQsuf->Type);
	for(iQsuf=0; iQsuf<pQsuf->Count; iQsuf++) {
		fprintf(fp, "%10d%10d%10d%10d%10d%10d%10d%10d\n",
				pQsuf->ID[iQsuf],
				pQsuf->Refered[iQsuf],
				pQsuf->Node[4*iQsuf],
				pQsuf->Node[4*iQsuf+1],
				pQsuf->Node[4*iQsuf+2],
				pQsuf->Node[4*iQsuf+3],
				pQsuf->Element[2*iQsuf],
				pQsuf->Element[2*iQsuf+1]);
	};

	return;
};


/*******************************************************************************
 * Print quadrilateral surface information
 ******************************************************************************/
extern int
ppohVIS_BASE_PutQuadrilateralSurface(
	char *FileName, struct ppohVIS_BASE_stGeometryItem *pQsuf, int Append)
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
				strerror(errno), "quadrilateral surface info.");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]",
				strerror(errno), "quadrilateral surface info.");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintQuadrilateralSurface(fp, pQsuf);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]",
			strerror(errno), "quadrilateral surface info.");
		goto error;
	};

	return 0;

error:
	return -1;
};

