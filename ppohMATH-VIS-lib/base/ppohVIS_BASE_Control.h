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
#ifndef __H_PPOHVIS_BASE_CONTROL
#define __H_PPOHVIS_BASE_CONTROL

#include "ppohVIS_BASE_Config.h"

/*
 * definition
 */
enum ppohVIS_BASE_eAnalysisType {
	ppohVIS_BASE_AnalysisFEM,
	ppohVIS_BASE_AnalysisFDM,
	ppohVIS_BASE_AnalysisUnknown,
};

struct ppohVIS_BASE_stControlDebug {
	int Print;
	char FileName[PPOHVIS_BASE_FILE_NAME_LEN];
};

struct ppohVIS_BASE_stControlPrint {
	struct ppohVIS_BASE_stControlDebug Log;
	struct ppohVIS_BASE_stControlDebug Mesh;
	struct ppohVIS_BASE_stControlDebug TriaSurf;
	struct ppohVIS_BASE_stControlDebug QuadSurf;
	struct ppohVIS_BASE_stControlDebug FreeSurf;
	struct ppohVIS_BASE_stControlDebug InitialVoxel;
};

struct ppohVIS_BASE_stRefineControl {
	double AvailableMemory;
	int MaxRefineLevel;
	int MaxVoxelCount;
};

struct ppohVIS_BASE_stSimpleControl {
	double ReductionRate;
};

struct ppohVIS_BASE_stControl {
	struct ppohVIS_BASE_stRefineControl Refine;
	struct ppohVIS_BASE_stSimpleControl Simple;
};


/*
 * allocate
 */
extern struct ppohVIS_BASE_stControl *
ppohVIS_BASE_AllocateControl(void);

/*
 * initialize
 */
extern int
ppohVIS_BASE_InitRefineControl(
	struct ppohVIS_BASE_stRefineControl *pRefineCtrl);

extern int
ppohVIS_BASE_InitControl(
	struct ppohVIS_BASE_stControl *pControl);

/*
 * free
 */
extern void
ppohVIS_BASE_FreeControl(
	struct ppohVIS_BASE_stControl *pControl);


/*
 * I/O
 */
extern int
ppohVIS_BASE_PrintControl(
	FILE *fp, struct ppohVIS_BASE_stControl *pControl);

extern int
ppohVIS_BASE_PutControl(
	char *cFileName, struct ppohVIS_BASE_stControl *pControl);

#endif /* __H_PPOHVIS_BASE_CONTROL */
