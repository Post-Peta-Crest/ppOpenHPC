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
#ifndef __H_PPOHVIS_BASE_INITIAL_VOXEL
#define __H_PPOHVIS_BASE_INITIAL_VOXEL

#include "ppohVIS_BASE_VoxelItem.h"


struct ppohVIS_BASE_stInitialVoxel {
	double OX;
	double OY;
	double OZ;
	double DX;
	double DY;
	double DZ;
	int NX;
	int NY;
	int NZ;
	int *Rank;
	struct ppohVIS_BASE_stVoxelItem *Voxels;
};


/* allocate */
extern struct ppohVIS_BASE_stInitialVoxel *
ppohVIS_BASE_AllocateInitialVoxel(void);

/* free */
extern void
ppohVIS_BASE_FreeInitialVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

/* initialize */
extern int
ppohVIS_BASE_InitVoxelItems(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

/* copy */
extern struct ppohVIS_BASE_stInitialVoxel *
ppohVIS_BASE_CopyInitialVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxelSrc);

/* output */
extern void
ppohVIS_BASE_PrintInitialVoxel(
	FILE *fp,
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

extern int
ppohVIS_BASE_PutInitialVoxel(
	char *FileName,
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	int Append);

#endif /* __H_PPOHVIS_BASE_INITIAL_VOXEL */
