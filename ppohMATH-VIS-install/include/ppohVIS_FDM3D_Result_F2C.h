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
#ifndef __H_PPOHVIS_FDM3D_RESULT_F2C
#define __H_PPOHVIS_FDM3D_RESULT_F2C

#include "ppohVIS_FDM3D_Result.h"

extern int
ppohVIS_FDM3D_ResultF2CInit(
	enum ppohVIS_FDM3D_eResultEntityType EntityType,
	struct ppohVIS_FDM3D_stResultCollection *pResult);

extern int
ppohVIS_FDM3D_ResultF2CSetListCount(
	enum ppohVIS_FDM3D_eResultEntityType EntityType,
	int *ListCount);

extern int
ppohVIS_FDM3D_ResultF2CSetResult(
	enum ppohVIS_FDM3D_eResultEntityType EntityType, int iIndex,
	int *ItemCount, int *FreedomCount, char *Label, double *Value);

extern int
ppohVIS_FDM3D_ResultF2CFinalize(
	enum ppohVIS_FDM3D_eResultEntityType EntityType);

#endif /* __H_PPOHVIS_FDM3D_RESULT_F2C */
