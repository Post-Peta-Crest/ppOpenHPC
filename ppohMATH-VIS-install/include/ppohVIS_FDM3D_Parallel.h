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
#ifndef __H_PPOHVIS_FDM3D_PARALLEL
#define __H_PPOHVIS_FDM3D_PARALLEL

#include "ppohVIS_FDM3D_mpi.h"
#include "ppohVIS_FDM3D_StrGrid.h"
#include "ppohVIS_FDM3D_BoundingBox.h"
#include "ppohVIS_FDM3D_Voxel.h"
#include "ppohVIS_FDM3D_VoxelValue.h"


extern void
ppohVIS_FDM3D_InitParallel(ppohVIS_FDM3D_Comm comm);

extern void
ppohVIS_FDM3D_SetCommSize(int psize);

extern void
ppohVIS_FDM3D_SetCommRank(int rank);

extern void
ppohVIS_FDM3D_SetCommunicator(ppohVIS_FDM3D_Comm comm);

extern int
ppohVIS_FDM3D_GetCommSize(void);

extern int
ppohVIS_FDM3D_GetCommRank(void);

extern ppohVIS_FDM3D_Comm
ppohVIS_FDM3D_GetCommunicator(void);

extern int
ppohVIS_FDM3D_Comm_size(ppohVIS_FDM3D_Comm comm, int *iProcCount);

extern int
ppohVIS_FDM3D_Comm_rank(ppohVIS_FDM3D_Comm comm, int *iMyRank);

extern void
ppohVIS_FDM3D_Barrier(ppohVIS_FDM3D_Comm comm);

extern ppohVIS_FDM3D_Comm
ppohVIS_FDM3D_Comm_f2c(int fComm);



extern struct ppohVIS_FDM3D_stBoundingBox *
ppohVIS_FDM3D_GetGlobalBoundingBox(
	struct ppohVIS_FDM3D_stBoundingBox *pBBoxLocal,
	ppohVIS_FDM3D_Comm comm);

extern struct ppohVIS_FDM3D_stStrGrid *
ppohVIS_FDM3D_GetGlobalStrGrid(
	struct ppohVIS_FDM3D_stStrGrid *pStrGrid,
	ppohVIS_FDM3D_Comm comm);

extern int
ppohVIS_FDM3D_GetMaxCostRank(
	struct ppohVIS_FDM3D_stVoxelItem *pRefineVoxel,
	ppohVIS_FDM3D_Comm comm);

extern int
ppohVIS_FDM3D_GatherVoxelCount(
	int iVoxelCountLocal,
	ppohVIS_FDM3D_Comm comm,
	int *iVoxelCountGlobal);


extern int
ppohVIS_FDM3D_ReduceVoxelCount(
	int iVoxelCountLocal,
	ppohVIS_FDM3D_Comm comm,
	int *iVoxelCountGlobal);

extern int
ppohVIS_FDM3D_ReduceVoxelValue(
	struct ppohVIS_FDM3D_stVoxelValue *pValueLocal,
	ppohVIS_FDM3D_Comm comm,
	struct ppohVIS_FDM3D_stVoxelValue *pValueGlobal);

extern struct ppohVIS_FDM3D_stVoxelValue *
ppohVIS_FDM3D_GatherVoxelValue(
	struct ppohVIS_FDM3D_stVoxelValue *pValueLocal,
	ppohVIS_FDM3D_Comm comm);

#endif /* __H_PPOHVIS_FDM3D_PARALLEL */

