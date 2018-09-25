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
#include "ppohVIS_BASE_BoundingBox.h"


/*******************************************************************************
 * Allocate bounding box information
 ******************************************************************************/
extern struct ppohVIS_BASE_stBoundingBox *
ppohVIS_BASE_AllocateBoundingBox(void)
{
	struct ppohVIS_BASE_stBoundingBox *pBBox = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stBoundingBox);
	pBBox = (struct ppohVIS_BASE_stBoundingBox *)malloc(BufSize);
	if(pBBox == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "ppohVIS_BASE_stBoundingBox");
		return NULL;
	};

	pBBox->XMin = 0.0;
	pBBox->YMin = 0.0;
	pBBox->ZMin = 0.0;

	pBBox->XMax = 0.0;
	pBBox->YMax = 0.0;
	pBBox->ZMax = 0.0;

	return pBBox;
};


/*******************************************************************************
 * Free bounding box information
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeBoundingBox(
	struct ppohVIS_BASE_stBoundingBox *pBBox)
{
	if(pBBox) {
		free(pBBox);
		pBBox = NULL;
	};
};


/*******************************************************************************
 * Create bounding box information
 ******************************************************************************/
extern struct ppohVIS_BASE_stBoundingBox *
ppohVIS_BASE_GetBoundingBox(
	struct ppohVIS_BASE_stMesh *pMesh)
{
	struct ppohVIS_BASE_stBoundingBox *pBBox;
	double X, Y, Z;
	double XMin, YMin, ZMin;
	double XMax, YMax, ZMax;
	int iNode;
	size_t BufSize;

	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "pMesh");
		return NULL;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stBoundingBox);
	pBBox = (struct ppohVIS_BASE_stBoundingBox *)malloc(BufSize);
	if(pBBox == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s", "ppohVIS_BASE_stBoundingBox");
		return NULL;
	};

	XMin = pMesh->Node->Coords[0];
	XMax = pMesh->Node->Coords[0];
	YMin = pMesh->Node->Coords[1];
	YMax = pMesh->Node->Coords[1];
	ZMin = pMesh->Node->Coords[2];
	ZMax = pMesh->Node->Coords[2];
//#pragma omp parallel for reduction(min:XMin,YMin,ZMin) reduction(max:XMax,YMax,ZMax) private(iNode,X,Y,Z)
	for(iNode=1; iNode<pMesh->Node->Count; iNode++) {
		X = pMesh->Node->Coords[3*iNode  ];
		Y = pMesh->Node->Coords[3*iNode+1];
		Z = pMesh->Node->Coords[3*iNode+2];

		if(X < XMin) { XMin = X; }
		if(X > XMax) { XMax = X; }
		if(Y < YMin) { YMin = Y; }
		if(Y > YMax) { YMax = Y; }
		if(Z < ZMin) { ZMin = Z; }
		if(Z > ZMax) { ZMax = Z; }
	};

	pBBox->XMin = XMin;
	pBBox->XMax = XMax;
	pBBox->YMin = YMin;
	pBBox->YMax = YMax;
	pBBox->ZMin = ZMin;
	pBBox->ZMax = ZMax;

	return pBBox;
}


/*******************************************************************************
 * Print bounding box information
 ******************************************************************************/
extern void
ppohVIS_BASE_PrintBoundingBox(
	FILE *fp, struct ppohVIS_BASE_stBoundingBox *pBBox)
{
	fprintf(fp, "Min: %14.7e %14.7e %14.7e\n",
			pBBox->XMin, pBBox->YMin, pBBox->ZMin);
	fprintf(fp, "Max: %14.7e %14.7e %14.7e\n",
			pBBox->XMax, pBBox->YMax, pBBox->ZMax);

	return;
}


/*******************************************************************************
 * Put bounding box information
 ******************************************************************************/
extern int
ppohVIS_BASE_PutBoundingBox(
	char *FileName, struct ppohVIS_BASE_stBoundingBox *pBBox, int Append)
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
				strerror(errno), "bounding box info.");
			goto error;
		};
	} else {
		if((fp = fopen(cFileName, "a")) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileOpenError,
				"%s [%s]",
				strerror(errno), "bounding box info.");
			goto error;
		};
	};

	/* Print to a file */
	ppohVIS_BASE_PrintBoundingBox(fp, pBBox);

	/* Close file */
	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "bounding box info.");
		goto error;
	};

	return 0;

error:
	return -1;
};
