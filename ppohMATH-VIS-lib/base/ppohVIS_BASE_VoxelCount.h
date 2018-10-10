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
#ifndef __H_PPOHVIS_BASE_VOXEL_COUNT
#define __H_PPOHVIS_BASE_VOXEL_COUNT

#include "ppohVIS_BASE_InitialVoxel.h"


/*=====================*
 * function interfaces *
 *=====================*/
extern int
ppohVIS_BASE_GetVoxelCount(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

extern int
ppohVIS_BASE_GetVoxelCount2(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel, int MyRank);

extern int
ppohVIS_BASE_GetVoxelCountWithoutOuter(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

extern int
ppohVIS_BASE_GetInnerVoxelCount(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

extern int
ppohVIS_BASE_GetVoxelCountWithBranch(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

extern int
ppohVIS_BASE_GetVoxelMaxLevel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel);

#endif /* __H_PPOHVIS_BASE_VOXEL_COUNT */