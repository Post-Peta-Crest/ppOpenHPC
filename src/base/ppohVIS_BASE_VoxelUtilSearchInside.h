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
#ifndef __H_PPOHVIS_BASE_VOXELSEARCHINSIDE
#define __H_PPOHVIS_BASE_VOXELSEARCHINSIDE

#include "ppohVIS_BASE_VoxelItem.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_UCD.h"


/*============================*
 * definitions and structures *
 *============================*/
/*
// if no problems, remove them
enum ppohVIS_BASE_eVoxelType {
	ppohVIS_BASE_VoxelInner,
	ppohVIS_BASE_VoxelBoundary,
	ppohVIS_BASE_VoxelOuter,
	ppohVIS_BASE_VoxelUnknown,
};

struct ppohVIS_BASE_stVoxelItem {
	int Level;
	enum ppohVIS_BASE_eVoxelType Type;
	double OX;
	double OY;
	double OZ;
	double DX;
	double DY;
	double DZ;
	int NodeCount;
	int *NodeID;
	int ElementCount;
	int *ElementID;
	int FreeSurfaceCount;
	int *FreeSurfaceID;
	double *Value;
	double Cost;
	struct ppohVIS_BASE_stVoxelItem *Parent;
	struct ppohVIS_BASE_stVoxelItem **Children;
};
*/

/*
struct ppohVIS_BASE_stInitialVoxel {
	double OX;
	double OY;
	double OZ;
	double DX;
	double DY;
	double DZ;
	int NX;
	int NY;
	int NZ;
	int *Rank;
	struct ppohVIS_BASE_stVoxelItem *Voxels;
};
*/

extern int
ppohVIS_BASE_SetVoxelType(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf);

extern int
ppohVIS_BASE_SearchFreeSurfaceInsideByParent(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	int *TsufMask);

extern int
ppohVIS_BASE_SearchFreeSurfaceInside(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf,
	int *TsufMask);

extern int
ppohVIS_BASE_CheckVoxelType(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf);

extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertVoxel2UCD(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	const int OmitOuter,
	const int OmitBoundary);

#endif /* __H_PPOHVIS_BASE_VOXELSEARCHINSIDE */
