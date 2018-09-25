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
#ifndef __H_PPOHVIS_BASE_RESULT
#define __H_PPOHVIS_BASE_RESULT


#include "ppohVIS_BASE_Config.h"

/*============================================================================*/
/*
 * definition
 */
enum ppohVIS_BASE_eResultEntityType {
	ppohVIS_BASE_ResultNode,
	ppohVIS_BASE_ResultElement,
};

struct ppohVIS_BASE_stResult {
	int ItemCount;
	enum ppohVIS_BASE_eResultEntityType EntityType;
	int FreedomCount;
	char Label[PPOHVIS_BASE_LABEL_LEN];
	double *Value;
};

struct ppohVIS_BASE_stResultCollection {
	int ListCount;
	struct ppohVIS_BASE_stResult **Results;
};


/*============================================================================*/
/*
 * allocate
 */
extern struct ppohVIS_BASE_stResult *
ppohVIS_BASE_AllocateResult(void);

extern struct ppohVIS_BASE_stResultCollection *
ppohVIS_BASE_AllocateResultCollection(void);

/*
 * initialize
 */
extern int
ppohVIS_BASE_InitResult(
	struct ppohVIS_BASE_stResult *pResult,
	int ItemCount,
	enum ppohVIS_BASE_eResultEntityType Type,
	int FreedomCount);

extern int
ppohVIS_BASE_InitResultCollection(
	struct ppohVIS_BASE_stResultCollection *pResults, int Count);

/*
 * Free
 */
extern void
ppohVIS_BASE_FreeResult(
	struct ppohVIS_BASE_stResult *pResult);

extern void
ppohVIS_BASE_FreeResultCollection(
	struct ppohVIS_BASE_stResultCollection *pResults);


/*
 * I/O
 */
extern void
ppohVIS_BASE_PrintResult(
	FILE *fp, struct ppohVIS_BASE_stResult *pResult);

extern int
ppohVIS_BASE_PutResult(
	char *FileName, struct ppohVIS_BASE_stResult *pResult, int Append);


#endif /* __H_PPOHVIS_BASE_RESULT */
