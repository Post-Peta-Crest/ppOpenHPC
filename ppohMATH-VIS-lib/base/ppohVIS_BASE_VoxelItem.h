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
#ifndef __H_PPOHVIS_BASE_VOXEL_ITEM
#define __H_PPOHVIS_BASE_VOXEL_ITEM

enum ppohVIS_BASE_eVoxelType {
	ppohVIS_BASE_VoxelInner,
	ppohVIS_BASE_VoxelBoundary,
	ppohVIS_BASE_VoxelOuter,
	ppohVIS_BASE_VoxelUnknown,
};

struct ppohVIS_BASE_stVoxelItem {
	int Level;
	int IndexX;
	int IndexY;
	int IndexZ;
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
	double CoefN;
	double CoefD;
	struct ppohVIS_BASE_stVoxelItem *Parent;
	struct ppohVIS_BASE_stVoxelItem **Children;
};


extern struct ppohVIS_BASE_stVoxelItem *
ppohVIS_BASE_AllocateVoxelItem(void);

extern void
ppohVIS_BASE_FreeVoxelItem(
	struct ppohVIS_BASE_stVoxelItem *pVoxel);

extern void
ppohVIS_BASE_FreeVoxelItems(
	struct ppohVIS_BASE_stVoxelItem *pVoxel);

extern int
ppohVIS_BASE_InitVoxelItem(
	struct ppohVIS_BASE_stVoxelItem *pVoxel);


extern int
ppohVIS_BASE_GetLocalIndex(int IndexGlobal);

extern int
ppohVIS_BASE_GetParentIndex(int IndexCurrent);

extern int
ppohVIS_BASE_GetRootIndex(int SrcLevel, int DstLevel, int IndexCurrent);


#endif /* __H_PPOHVIS_BASE_VOXEL_ITEM */
