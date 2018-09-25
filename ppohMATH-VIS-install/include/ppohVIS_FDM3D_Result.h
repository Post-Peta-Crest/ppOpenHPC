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
#ifndef __H_PPOHVIS_FDM3D_RESULT
#define __H_PPOHVIS_FDM3D_RESULT


#include "ppohVIS_FDM3D_Config.h"

/*============================================================================*/
/*
 * definition
 */
enum ppohVIS_FDM3D_eResultEntityType {
	ppohVIS_FDM3D_ResultNode,
	ppohVIS_FDM3D_ResultElement,
};

struct ppohVIS_FDM3D_stResult {
	int ItemCount;
	enum ppohVIS_FDM3D_eResultEntityType EntityType;
	int FreedomCount;
	char Label[PPOHVIS_FDM3D_LABEL_LEN];
	double *Value;
};

struct ppohVIS_FDM3D_stResultCollection {
	int ListCount;
	struct ppohVIS_FDM3D_stResult **Results;
};


/*============================================================================*/
/*
 * allocate
 */
extern struct ppohVIS_FDM3D_stResult *
ppohVIS_FDM3D_AllocateResult(void);

extern struct ppohVIS_FDM3D_stResultCollection *
ppohVIS_FDM3D_AllocateResultCollection(void);

/*
 * initialize
 */
extern int
ppohVIS_FDM3D_InitResult(
	struct ppohVIS_FDM3D_stResult *pResult,
	int ItemCount,
	enum ppohVIS_FDM3D_eResultEntityType Type,
	int FreedomCount);

extern int
ppohVIS_FDM3D_InitResultCollection(
	struct ppohVIS_FDM3D_stResultCollection *pResults, int Count);

/*
 * Free
 */
extern void
ppohVIS_FDM3D_FreeResult(
	struct ppohVIS_FDM3D_stResult *pResult);

extern void
ppohVIS_FDM3D_FreeResultCollection(
	struct ppohVIS_FDM3D_stResultCollection *pResults);


/*
 * I/O
 */
extern void
ppohVIS_FDM3D_PrintResult(
	FILE *fp, struct ppohVIS_FDM3D_stResult *pResult);

extern int
ppohVIS_FDM3D_PutResult(
	char *FileName, struct ppohVIS_FDM3D_stResult *pResult, int Append);


#endif /* __H_PPOHVIS_FDM3D_RESULT */
