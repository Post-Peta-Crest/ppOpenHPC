/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_FDM3D                                     *
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
#ifndef __H_PPOHVIS_FDM3D_VOXELREFINE
#define __H_PPOHVIS_FDM3D_VOXELREFINE

#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_Voxel.h"


extern int
ppohVIS_FDM3D_RefineVoxel(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	struct ppohVIS_BASE_stResult *pResult,
	struct ppohVIS_BASE_stControl *pCtrl,
	ppohVIS_BASE_Comm comm);

#endif /* __H_PPOHVIS_FDM3D_VOXELREFINE */
