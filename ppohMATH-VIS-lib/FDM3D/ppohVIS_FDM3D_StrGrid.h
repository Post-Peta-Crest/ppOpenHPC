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
#ifndef __H_PPOHVIS_FDM3D_STRGRID
#define __H_PPOHVIS_FDM3D_STRGRID

#include "ppohVIS_BASE_Mesh.h"


/*
 * definition
 */
struct ppohVIS_FDM3D_stStrGrid {
	int NumX;
	int NumY;
	int NumZ;
	double DeltaX;
	double DeltaY;
	double DeltaZ;
	double OriginX;
	double OriginY;
	double OriginZ;
};


/*============================================================================*/
/*
 * allocate
 */
extern struct ppohVIS_FDM3D_stStrGrid *
ppohVIS_FDM3D_AllocateStrGrid(void);

/*
 * initialize
 */
extern int
ppohVIS_FDM3D_InitStrGrid(
	struct ppohVIS_FDM3D_stStrGrid *pGrid);

/*
 * free
 */
extern void
ppohVIS_FDM3D_FreeStrGrid(
	struct ppohVIS_FDM3D_stStrGrid *pGrid);


/*
 * I/O
 */
extern int
ppohVIS_FDM3D_PrintStrGrid(
	FILE *fp, struct ppohVIS_FDM3D_stStrGrid *pGrid);

extern int
ppohVIS_FDM3D_PutStrGrid(
	char *cFileName, struct ppohVIS_FDM3D_stStrGrid *pGrid);


/*
 * utilities
 */
extern struct ppohVIS_BASE_stMesh *
ppohVIS_FDM3D_ConvertStrGridToMesh(
	struct ppohVIS_FDM3D_stStrGrid *pGrid);

#endif /* __H_PPOHVIS_FDM3D_STRGRID */