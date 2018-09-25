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
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <omp.h>

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Geometry.h"


/*******************************************************************************
 * Allocate
 ******************************************************************************/
extern struct ppohVIS_BASE_stGeometryItem *
ppohVIS_BASE_AllocateGeometryItem(void)
{
	struct ppohVIS_BASE_stGeometryItem *pGeom = NULL;
	size_t BufSize;

	/* Initialize */
	BufSize = sizeof(struct ppohVIS_BASE_stGeometryItem);
	pGeom = (struct ppohVIS_BASE_stGeometryItem *)malloc(BufSize);
	if(pGeom == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "geometry");
		goto Error;
	};

	pGeom->Count = 0;
	pGeom->Type = ppohVIS_BASE_Unknown;
	pGeom->ID = NULL;
	pGeom->Refered = NULL;
	pGeom->Node = NULL;
	pGeom->Element = NULL;

	return pGeom;

Error:
	ppohVIS_BASE_FreeGeometryItem(pGeom);
	return NULL;
};


/*******************************************************************************
 * Free
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeGeometryItem(
	struct ppohVIS_BASE_stGeometryItem *pGeom)
{
	if(pGeom == NULL) { return; };

	free(pGeom->ID);
	free(pGeom->Refered);
	free(pGeom->Node);
	free(pGeom->Element);
	free(pGeom);
	pGeom = NULL;
};


/*******************************************************************************
 * Convert geometry data to mesh
 ******************************************************************************/
extern struct ppohVIS_BASE_stMesh *
ppohVIS_BASE_ConvertGeometry2Mesh(
	struct ppohVIS_BASE_stMeshNode *pNode,
	struct ppohVIS_BASE_stGeometryItem *pGeom)
{
	struct ppohVIS_BASE_stMesh *pMesh = NULL;
	int *Old2New = NULL;
	int *New2Old = NULL;
	enum ppohVIS_BASE_eTopology iType;
	int Node, Count, iElem, iNode, iCount;
	int BufCount;
	double GX, GY, GZ, DN;
	size_t BufSize;

	/*----------------------------------------------------------------------
	 * Check Data
	 *--------------------------------------------------------------------*/
	if(pGeom == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "UCD data");
		goto error;
	};
	if(pGeom->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info in UCD data");
		goto error;
	};
	if(pGeom->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info in UCD data");
		goto error;
	};

	/*----------------------------------------------------------------------
	 * Initialization
	 *--------------------------------------------------------------------*/
/*
	BufSize = sizeof(struct ppohVIS_BASE_stMesh);
	pMesh = (struct ppohVIS_BASE_stMesh *)malloc(BufSize);
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mesh info");
		goto error;
	};
*/
	pMesh = ppohVIS_BASE_AllocateMesh();
	if(pMesh == NULL) {
		goto error;
	};

	switch (pGeom->Type) {
		case ppohVIS_BASE_Line2:
			iType = ppohVIS_BASE_Line2;
			iCount = 2;
			break;

		case ppohVIS_BASE_Tria3:
			iType = ppohVIS_BASE_Tria3;
			iCount = 3;
			break;

		case ppohVIS_BASE_Quad4:
			iType = ppohVIS_BASE_Quad4;
			iCount = 4;
			break;

		case ppohVIS_BASE_Tetra4:
			iType = ppohVIS_BASE_Tetra4;
			iCount = 4;
			break;

		case ppohVIS_BASE_Penta6:
			iType = ppohVIS_BASE_Penta6;
			iCount = 6;
			break;

		case ppohVIS_BASE_Hexa8:
			iType = ppohVIS_BASE_Hexa8;
			iCount = 8;
			break;

		default:
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_InvalidElemType,
				"%d", pGeom->Type);
			goto error;
	};

	/*----------------------------------------------------------------------
	 * Node info.
	 *--------------------------------------------------------------------*/
/*
	BufSize = sizeof(struct ppohVIS_BASE_stMeshNode);
	pMesh->Node = (struct ppohVIS_BASE_stMeshNode *)malloc(BufSize);
	if(pMesh->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mesh node info");
		goto error;
	};
*/

	BufCount = pNode->Count;
	BufSize = sizeof(int);
	Old2New = (int *)calloc(BufCount, BufSize);
	if(Old2New == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask array for node");
		goto error;
	};

	BufCount = pNode->Count;
	BufSize = sizeof(int);
	New2Old = (int *)calloc(BufCount, BufSize);
	if(New2Old == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mask array for node");
		goto error;
	};

	Count = 0;
	for(iNode=0; iNode<pGeom->Count*iCount; iNode++) {
		Node = pGeom->Node[iNode];
		if(Old2New[Node-1] == 0) {
			Count++;
			Old2New[Node-1]  = Count;
			New2Old[Count-1] = Node;
		};
	};

	pMesh->Node->Count = Count;
	BufCount = pMesh->Node->Count;
	BufSize = sizeof(int);
	pMesh->Node->ID = (int *)calloc(BufCount, BufSize);
	if(pMesh->Node->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto error;
	};

	BufSize = sizeof(double)*pMesh->Node->Count*3;
	pMesh->Node->Coords = (double *)malloc(BufSize);
	if(pMesh->Node->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "nodal coordinate");
		goto error;
	};

#pragma omp parallel for private(Node)
	for(iNode=0; iNode<pMesh->Node->Count; iNode++) {
		Node = New2Old[iNode];
		pMesh->Node->ID[iNode]         = iNode+1;
		pMesh->Node->Coords[3*iNode]   = pNode->Coords[3*(Node-1)];
		pMesh->Node->Coords[3*iNode+1] = pNode->Coords[3*(Node-1)+1];
		pMesh->Node->Coords[3*iNode+2] = pNode->Coords[3*(Node-1)+2];
	};

	/*----------------------------------------------------------------------
	 * Element info.
	 *--------------------------------------------------------------------*/
/*
	BufSize = sizeof(struct ppohVIS_BASE_stMeshElement);
	pMesh->Element = (struct ppohVIS_BASE_stMeshElement *)malloc(BufSize);
	if(pMesh->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mesh element info");
		goto error;
	};
*/

	BufCount = pGeom->Count;
	BufSize = sizeof(int);
	pMesh->Element->ID = (int *)calloc(BufCount, BufSize);
	if(pMesh->Element->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element ID");
		goto error;
	};

	BufCount = pGeom->Count;
	BufSize = sizeof(enum ppohVIS_BASE_eTopology);
	pMesh->Element->Type = (enum ppohVIS_BASE_eTopology *)calloc(BufCount, BufSize);
	if(pMesh->Element->Type == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element type");
		goto error;
	};

	BufCount = pGeom->Count * 3;
	BufSize = sizeof(double) * BufCount;
	pMesh->Element->Gravity = (double *)malloc(BufSize);
	if(pMesh->Element->Gravity == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "gravity");
		goto error;
	};

	BufCount = pGeom->Count + 1;
	BufSize = sizeof(int);
	pMesh->Element->NodeIndex = (int *)calloc(BufCount+1, BufSize);
	if(pMesh->Element->NodeIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of connectivity");
		goto error;
	};

	BufCount = pGeom->Count * iCount;
	BufSize = sizeof(int);
	pMesh->Element->Node = (int *)calloc(BufCount, BufSize);
	if(pMesh->Element->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "connectivity");
		goto error;
	};

	pMesh->Element->Count = pGeom->Count;
	memcpy(pMesh->Element->ID, pGeom->ID, pGeom->Count*sizeof(int));
#pragma omp parallel for private(GX,GY,GZ,DN,Node)
	for(iElem=0; iElem<pGeom->Count; iElem++) {
		pMesh->Element->Type[iElem] = iType;
		pMesh->Element->NodeIndex[iElem+1] = iCount*(iElem+1);

		GX=0.0;
		GY=0.0;
		GZ=0.0;
		DN=0.0;
		for(iNode=iCount*iElem; iNode<iCount*(iElem+1); iNode++) {
			Node = pGeom->Node[iNode];
			pMesh->Element->Node[iNode] = Old2New[Node-1];

			GX += pNode->Coords[3*(Node-1)];
			GY += pNode->Coords[3*(Node-1)+1];
			GZ += pNode->Coords[3*(Node-1)+2];
			DN += 1.0;
		};
		pMesh->Element->Gravity[3*iElem]   = GX / DN;
		pMesh->Element->Gravity[3*iElem+1] = GY / DN;
		pMesh->Element->Gravity[3*iElem+2] = GZ / DN;
	};

	return pMesh;

error:
	return NULL;
};


