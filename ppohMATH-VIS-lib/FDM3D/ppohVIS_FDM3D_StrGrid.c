/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_FDM3D                                     *
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

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_FDM3D_StrGrid.h"


/*******************************************************************************
 * Allocate
 ******************************************************************************/
extern struct ppohVIS_FDM3D_stStrGrid *
ppohVIS_FDM3D_AllocateStrGrid(void)
{
	struct ppohVIS_FDM3D_stStrGrid *pGrid = NULL;
	size_t BufSize;

	/* allocate */
	BufSize = sizeof(struct ppohVIS_FDM3D_stStrGrid);
	pGrid = (struct ppohVIS_FDM3D_stStrGrid *)malloc(BufSize);
	if(pGrid == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "structured grid");
		goto Error;
	};

	/* initialize */
	ppohVIS_FDM3D_InitStrGrid(pGrid);

	return pGrid;

Error:
	ppohVIS_FDM3D_FreeStrGrid(pGrid);
	return NULL;
};


/*******************************************************************************
 * Initialize
 ******************************************************************************/
extern int
ppohVIS_FDM3D_InitStrGrid(struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	if(pGrid == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "structure grid info");
		return -1;
	};

	pGrid->OriginX = 0.0;
	pGrid->OriginY = 0.0;
	pGrid->OriginZ = 0.0;

	pGrid->DeltaX = 0.0;
	pGrid->DeltaY = 0.0;
	pGrid->DeltaZ = 0.0;

	pGrid->NumX = 0;
	pGrid->NumY = 0;
	pGrid->NumZ = 0;

	return 0;
};


/*******************************************************************************
 * Free
 ******************************************************************************/
extern void
ppohVIS_FDM3D_FreeStrGrid(struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	if(pGrid == NULL) { return; };

	free(pGrid);
	pGrid = NULL;
};


/*******************************************************************************
 * Print
 ******************************************************************************/
extern int
ppohVIS_FDM3D_PrintStrGrid(FILE *fp, struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	if(pGrid == NULL) {
		fprintf(fp, "### not allocated\n");
		return -1;
	};


	fprintf(fp, "### Origin: %15.7le,%15.7le,%15.7le\n",
	            pGrid->OriginX, pGrid->OriginY, pGrid->OriginZ);
	fprintf(fp, "### Delta : %15.7le,%15.7le,%15.7le\n",
	            pGrid->DeltaX, pGrid->DeltaY, pGrid->DeltaZ);
	fprintf(fp, "### Cells : %10d,%10d,%10d\n",
	            pGrid->NumX, pGrid->NumY, pGrid->NumZ);

	return 0;
};


/*============================================================================*/
extern int
ppohVIS_FDM3D_PutStrGrid(char *cFileName, struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	FILE *fp = NULL;

	if((fp = fopen(cFileName, "w")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError, "%s [%s]",
			strerror(errno), "structure grid info.");
		goto error;
	};

	if(ppohVIS_FDM3D_PrintStrGrid(fp, pGrid)) {
		goto error;
	};

	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError, "%s [%s]",
			strerror(errno), "structure grid info.");
		goto error;
	};

	return 0;

error:
	if(fp) {
		fclose(fp);
	};
	return -1;
};


/*******************************************************************************
 * StrGrid to StrMesh
 ******************************************************************************/
extern struct ppohVIS_BASE_stMesh *
ppohVIS_FDM3D_ConvertStrGridToMesh(struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	struct ppohVIS_BASE_stMesh *pMesh = NULL;
	struct ppohVIS_BASE_stMeshNode *pNode = NULL;
	struct ppohVIS_BASE_stMeshElement *pElem = NULL;
	int nNode, nElem;
	int nNX, nNY, nNZ, nEX, nEY, nEZ;
	double DX, DY, DZ, OX, OY, OZ;
	int iBase, iX, iY, iZ;

	if(pGrid == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "structure grid info");
		goto Error;
	};

	/* initialization */
	nNX = pGrid->NumX + 1;
	nNY = pGrid->NumY + 1;
	nNZ = pGrid->NumZ + 1;
	nEX = pGrid->NumX;
	nEY = pGrid->NumY;
	nEZ = pGrid->NumZ;
	DX = pGrid->DeltaX;
	DY = pGrid->DeltaY;
	DZ = pGrid->DeltaZ;
	OX = pGrid->OriginX;
	OY = pGrid->OriginY;
	OZ = pGrid->OriginZ;

	pMesh = ppohVIS_BASE_AllocateMesh();
	if(pMesh == NULL) {
		goto Error;
	};

	nNode = nNX * nNY * nNZ;
	nElem = nEX * nEY * nEZ;

	if(ppohVIS_BASE_InitMesh(pMesh, nNode, nElem, 0)) {
		goto Error;
	};

	/* coordinates */
	pNode = pMesh->Node;
	nNode = 0;
	for(iZ=0; iZ<nNZ; iZ++) {
	for(iY=0; iY<nNY; iY++) {
	for(iX=0; iX<nNX; iX++) {
		pNode->ID[nNode]         = nNode+1;
		pNode->Coords[3*nNode  ] = OX + DX * (double)(iX);
		pNode->Coords[3*nNode+1] = OY + DY * (double)(iY);
		pNode->Coords[3*nNode+2] = OZ + DZ * (double)(iZ);
		nNode++;
	};
	};
	};

	/* connectivity */
	pElem = pMesh->Element;
	pElem->Node = (int *)malloc(sizeof(int)*nElem*8);
	if(pElem->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "connectivity");
		goto Error;
	};

	nElem = 0;
	for(iZ=0; iZ<nEZ; iZ++) {
	for(iY=0; iY<nEY; iY++) {
	for(iX=0; iX<nEX; iX++) {
		pElem->ID  [nElem] = nElem+1;
		pElem->Type[nElem] = ppohVIS_BASE_Hexa8;
		pElem->NodeIndex[nElem] = nElem * 8;

		iBase = nNX * nNY * iZ + nNX * iY + iX + 1;

		pElem->Node[nElem*8  ] = iBase;
		pElem->Node[nElem*8+1] = iBase                 + 1;
		pElem->Node[nElem*8+2] = iBase           + nNX + 1;
		pElem->Node[nElem*8+3] = iBase           + nNX;
		pElem->Node[nElem*8+4] = iBase + nNX*nNY;
		pElem->Node[nElem*8+5] = iBase + nNX*nNY       + 1;
		pElem->Node[nElem*8+6] = iBase + nNX*nNY + nNX + 1;
		pElem->Node[nElem*8+7] = iBase + nNX*nNY + nNX;

		nElem++;
	};
	};
	};
	pElem->NodeIndex[nElem] = nElem * 8;

	if(ppohVIS_BASE_CalculateGravity(pMesh)) {
		goto Error;
	};

	return pMesh;

Error:
	return NULL;
};

