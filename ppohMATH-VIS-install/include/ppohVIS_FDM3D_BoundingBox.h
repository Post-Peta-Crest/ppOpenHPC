/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_FDM3D                                     *
 *         Version : 0.1                                               *
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
 *   Copyright (c) 2012 <Kengo Nakajima, The University of Tokyo       *
 *                       nakajima(at)cc.u-tokyo.ac.jp           >      *
 *=====================================================================*/
#ifndef __H_PPOHVIS_FDM3D_BOUNDING_BOX
#define __H_PPOHVIS_FDM3D_BOUNDING_BOX

#include "ppohVIS_FDM3D_Mesh.h"


/*
 * definition
 */
struct ppohVIS_FDM3D_stBoundingBox {
	double XMin;
	double YMin;
	double ZMin;
	double XMax;
	double YMax;
	double ZMax;
};


/*
 * allocate
 */
extern struct ppohVIS_FDM3D_stBoundingBox *
ppohVIS_FDM3D_AllocateBoundingBox(void);

/*
 * free
 */
extern void
ppohVIS_FDM3D_FreeBoundingBox(
	struct ppohVIS_FDM3D_stBoundingBox *pBBox);


/*
 * create
 */
extern struct ppohVIS_FDM3D_stBoundingBox *
ppohVIS_FDM3D_GetBoundingBox(
	struct ppohVIS_FDM3D_stMesh *pMesh);


/*
 * print
 */
extern void
ppohVIS_FDM3D_PrintBoundingBox(
	FILE *fp, struct ppohVIS_FDM3D_stBoundingBox *pBBox);

extern int
ppohVIS_FDM3D_PutBoundingBox(
	char *FileName, struct ppohVIS_FDM3D_stBoundingBox *pBBox, int Append);


#endif /* __H_PPOHVIS_FDM3D_BOUNDING_BOX */

