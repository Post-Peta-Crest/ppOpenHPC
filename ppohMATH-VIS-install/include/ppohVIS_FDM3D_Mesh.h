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
#ifndef __H_PPOHVIS_FDM3D_MESH
#define __H_PPOHVIS_FDM3D_MESH

/*
 * definition
 */
struct ppohVIS_FDM3D_stMeshNode {
	int Count;
	int *ID;
	double *Coords;
};

enum ppohVIS_FDM3D_eTopology {
	ppohVIS_FDM3D_Line2,
	ppohVIS_FDM3D_Line3,
	ppohVIS_FDM3D_Tria3,
	ppohVIS_FDM3D_Tria6,
	ppohVIS_FDM3D_Quad4,
	ppohVIS_FDM3D_Quad8,
	ppohVIS_FDM3D_Tetra4,
	ppohVIS_FDM3D_Tetra10,
	ppohVIS_FDM3D_Penta6,
	ppohVIS_FDM3D_Penta15,
	ppohVIS_FDM3D_Hexa8,
	ppohVIS_FDM3D_Hexa20,
	ppohVIS_FDM3D_Unknown,
};

struct ppohVIS_FDM3D_stMeshElement {
	int Count;
	int *ID;
	enum ppohVIS_FDM3D_eTopology *Type;
	int *NodeIndex;
	int *Node;
	double *Gravity;
};

struct ppohVIS_FDM3D_stMesh {
	struct ppohVIS_FDM3D_stMeshNode *Node;
	struct ppohVIS_FDM3D_stMeshElement *Element;
};



/*
 * allocate
 */
extern struct ppohVIS_FDM3D_stMeshNode *
ppohVIS_FDM3D_AllocateMeshNode(void);

extern struct ppohVIS_FDM3D_stMeshElement *
ppohVIS_FDM3D_AllocateMeshElement(void);

extern struct ppohVIS_FDM3D_stMesh *
ppohVIS_FDM3D_AllocateMesh(void);

/*
 * initialize
 */
extern int
ppohVIS_FDM3D_InitMeshNode(
	struct ppohVIS_FDM3D_stMeshNode *pNode, int nNode);

extern int
ppohVIS_FDM3D_InitMeshElement(
	struct ppohVIS_FDM3D_stMeshElement *pElement, int nElement);

extern int
ppohVIS_FDM3D_InitMesh(
	struct ppohVIS_FDM3D_stMesh *pMesh, int nNode, int nElement);

/* 
 * free
 */
extern void
ppohVIS_FDM3D_FreeMeshNode(
	struct ppohVIS_FDM3D_stMeshNode *pNode);

extern void
ppohVIS_FDM3D_FreeMeshElement(
	struct ppohVIS_FDM3D_stMeshElement *pElement);

extern void
ppohVIS_FDM3D_FreeMesh(
	struct ppohVIS_FDM3D_stMesh *pMesh);


/*
 * utilities
 */
extern int
ppohVIS_FDM3D_CalculateGravity(
	struct ppohVIS_FDM3D_stMesh *pMesh);


/*
 * I/O
 */
extern void
ppohVIS_FDM3D_PrintMeshNode(
	FILE *fp, struct ppohVIS_FDM3D_stMeshNode *pNode);

extern void
ppohVIS_FDM3D_PrintMesh(
	FILE *fp, struct ppohVIS_FDM3D_stMesh *pMesh);

extern int
ppohVIS_FDM3D_PutMeshNode(
	char *FileName, struct ppohVIS_FDM3D_stMeshNode *pNode, int Append);

extern int
ppohVIS_FDM3D_PutMesh(
	char *FileName, struct ppohVIS_FDM3D_stMesh *pMesh, int Append);

#endif /* __H_PPOHVIS_FDM3D_MESH */

