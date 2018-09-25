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
#ifndef __H_PPOHVIS_FDM3D_VOXEL
#define __H_PPOHVIS_FDM3D_VOXEL

#include "ppohVIS_FDM3D_Mesh.h"
#include "ppohVIS_FDM3D_Geometry.h"
#include "ppohVIS_FDM3D_UCD.h"


/*============================*
 * definitions and structures *
 *============================*/
enum ppohVIS_FDM3D_eVoxelType {
	ppohVIS_FDM3D_VoxelInner,
	ppohVIS_FDM3D_VoxelBoundary,
	ppohVIS_FDM3D_VoxelOuter,
	ppohVIS_FDM3D_VoxelUnknown,
};

struct ppohVIS_FDM3D_stVoxelItem {
	int Level;
	enum ppohVIS_FDM3D_eVoxelType Type;
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
	struct ppohVIS_FDM3D_stVoxelItem *Parent;
	struct ppohVIS_FDM3D_stVoxelItem **Children;
};

struct ppohVIS_FDM3D_stInitialVoxel {
	double OX;
	double OY;
	double OZ;
	double DX;
	double DY;
	double DZ;
	int NX;
	int NY;
	int NZ;
	struct ppohVIS_FDM3D_stVoxelItem *Voxels;
};


extern int
ppohVIS_FDM3D_SetVoxelType(
	struct ppohVIS_FDM3D_stInitialVoxel *pIniVoxel,
	struct ppohVIS_FDM3D_stMesh *pMesh,
	struct ppohVIS_FDM3D_stGeometryItem *pFreeSurf);

extern int
ppohVIS_FDM3D_SearchFreeSurfaceInsideByParent(
	struct ppohVIS_FDM3D_stVoxelItem *pVoxel,
	struct ppohVIS_FDM3D_stMesh *pMesh,
	struct ppohVIS_FDM3D_stGeometryItem *pFreeSurf,
	int *TsufMask);

extern int
ppohVIS_FDM3D_SearchFreeSurfaceInside(
	struct ppohVIS_FDM3D_stVoxelItem *pVoxel,
	struct ppohVIS_FDM3D_stMesh *pMesh,
	struct ppohVIS_FDM3D_stGeometryItem *pFreeSurf,
	int *TsufMask);

extern int
ppohVIS_FDM3D_CheckVoxelType(
	struct ppohVIS_FDM3D_stInitialVoxel *pIniVoxel,
	struct ppohVIS_FDM3D_stMesh *pMesh,
	struct ppohVIS_FDM3D_stGeometryItem *pFreeSurf);

extern struct ppohVIS_FDM3D_stUCDData *
ppohVIS_FDM3D_ConvertVoxel2UCD(
	struct ppohVIS_FDM3D_stInitialVoxel *pIniVoxel,
	const int OmitOuter,
	const int OmitBoundary);

#endif /* __H_PPOHVIS_FDM3D_VOXEL */
