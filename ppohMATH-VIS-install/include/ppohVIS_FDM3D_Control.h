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
#ifndef __H_PPOHVIS_FDM3D_CONTROL
#define __H_PPOHVIS_FDM3D_CONTROL

#include "ppohVIS_FDM3D_Config.h"

/*
 * definition
 */
enum ppohVIS_FDM3D_eAnalysisType {
	ppohVIS_FDM3D_AnalysisFEM,
	ppohVIS_FDM3D_AnalysisFDM,
	ppohVIS_FDM3D_AnalysisUnknown,
};

struct ppohVIS_FDM3D_stControlDebug {
	int Print;
	char FileName[PPOHVIS_FDM3D_FILE_NAME_LEN];
};

struct ppohVIS_FDM3D_stControlPrint {
	struct ppohVIS_FDM3D_stControlDebug Log;
	struct ppohVIS_FDM3D_stControlDebug Mesh;
	struct ppohVIS_FDM3D_stControlDebug TriaSurf;
	struct ppohVIS_FDM3D_stControlDebug QuadSurf;
	struct ppohVIS_FDM3D_stControlDebug FreeSurf;
	struct ppohVIS_FDM3D_stControlDebug InitialVoxel;
};

struct ppohVIS_FDM3D_stRefineControl {
	double AvailableMemory;
	int MaxRefineLevel;
	int MaxVoxelCount;
};

struct ppohVIS_FDM3D_stControl {
	struct ppohVIS_FDM3D_stRefineControl Refine;
};



/*
 * allocate
 */
extern struct ppohVIS_FDM3D_stControl *
ppohVIS_FDM3D_AllocateControl(void);

/*
 * initialize
 */
extern int
ppohVIS_FDM3D_InitRefineControl(
	struct ppohVIS_FDM3D_stRefineControl *pRefineCtrl);

extern int
ppohVIS_FDM3D_InitControl(
	struct ppohVIS_FDM3D_stControl *pControl);

/*
 * free
 */
extern void
ppohVIS_FDM3D_FreeControl(
	struct ppohVIS_FDM3D_stControl *pControl);


/*
 * I/O
 */
extern int
ppohVIS_FDM3D_PrintControl(
	FILE *fp, struct ppohVIS_FDM3D_stControl *pControl);

extern int
ppohVIS_FDM3D_PutControl(
	char *cFileName, struct ppohVIS_FDM3D_stControl *pControl);

#endif /* __H_PPOHVIS_FDM3D_CONTROL */
