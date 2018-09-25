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
#ifndef __H_PPOHVIS_BASE_RESULT_F2C
#define __H_PPOHVIS_BASE_RESULT_F2C

#include "ppohVIS_BASE_Result.h"

extern int
ppohVIS_BASE_ResultF2CInit(
	enum ppohVIS_BASE_eResultEntityType EntityType,
	struct ppohVIS_BASE_stResultCollection *pResult);

extern int
ppohVIS_BASE_ResultF2CSetListCount(
	enum ppohVIS_BASE_eResultEntityType EntityType,
	int *ListCount);

extern int
ppohVIS_BASE_ResultF2CSetResult(
	enum ppohVIS_BASE_eResultEntityType EntityType, int iIndex,
	int *ItemCount, int *FreedomCount, char *Label, double *Value);

extern int
ppohVIS_BASE_ResultF2CFinalize(
	enum ppohVIS_BASE_eResultEntityType EntityType);

#endif /* __H_PPOHVIS_BASE_RESULT_F2C */
