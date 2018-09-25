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
#ifndef __H_PPOHVIS_BASE_VOXEL_VALUE
#define __H_PPOHVIS_BASE_VOXEL_VALUE

#include "ppohVIS_BASE_UCD.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_Result.h"


/*
 * definition
 */
struct ppohVIS_BASE_stVoxelValueItem {
	double OX;
	double OY;
	double OZ;
	double DX;
	double DY;
	double DZ;
	int Level;
	int IndexI;
	int IndexJ;
	int IndexK;
	double *Value;
	double Cost;
};

struct ppohVIS_BASE_stVoxelValue {
	int VoxelCount;
	int ValueCount;
	char ValueLabel[PPOHVIS_BASE_LABEL_LEN];
	struct ppohVIS_BASE_stVoxelValueItem **Values;
};


/******************************************************************************/
/*
 * allocate
 */
extern struct ppohVIS_BASE_stVoxelValueItem *
ppohVIS_BASE_AllocVoxelValueItem(void);

extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_AllocVoxelValue(void);

/*
 * free
 */
extern void
ppohVIS_BASE_FreeVoxelValueItem(
	struct ppohVIS_BASE_stVoxelValueItem *p);

extern void
ppohVIS_BASE_FreeVoxelValue(
	struct ppohVIS_BASE_stVoxelValue *p);

/*
 * initialize
 */
extern struct ppohVIS_BASE_stVoxelValueItem *
ppohVIS_BASE_InitVoxelValueItem(
	int ValueCount);

extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_InitVoxelValue(
	int VoxelCount, int ValueCount, char *ValueLabel);

extern struct ppohVIS_BASE_stVoxelValue *
ppohVIS_BASE_InitialVoxelToVoxelValue(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stResult *pResult);

/*
 * write
 */
extern int
ppohVIS_BASE_PrintVoxelValue(
	FILE *fp, struct ppohVIS_BASE_stVoxelValue *pVoxelValue);

extern int
ppohVIS_BASE_PutVoxelValue(
	char *FileName, struct ppohVIS_BASE_stVoxelValue *pVoxelValue);

/*
 * convert
 */
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertVoxelValue2UCD2(
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue);

extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertVoxelValue2UCD(
	struct ppohVIS_BASE_stVoxelValue *pVoxelValue);

#endif /* __H_PPOHVIS_BASE_VOXEL_VALUE */

