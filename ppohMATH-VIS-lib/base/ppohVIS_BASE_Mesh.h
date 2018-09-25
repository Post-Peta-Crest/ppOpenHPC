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
#ifndef __H_PPOHVIS_BASE_MESH
#define __H_PPOHVIS_BASE_MESH

/*
 * definition
 */
struct ppohVIS_BASE_stMeshNode {
	int Count;
	int IntCount;
	int *ID;
	int *Rank;
	double *Coords;
};

enum ppohVIS_BASE_eTopology {
	ppohVIS_BASE_Line2,
	ppohVIS_BASE_Line3,
	ppohVIS_BASE_Tria3,
	ppohVIS_BASE_Tria6,
	ppohVIS_BASE_Quad4,
	ppohVIS_BASE_Quad8,
	ppohVIS_BASE_Tetra4,
	ppohVIS_BASE_Tetra10,
	ppohVIS_BASE_Penta6,
	ppohVIS_BASE_Penta15,
	ppohVIS_BASE_Hexa8,
	ppohVIS_BASE_Hexa20,
	ppohVIS_BASE_Unknown,
};

struct ppohVIS_BASE_stMeshElement {
	int Count;
	int IntCount;
	int *ID;
	int *Rank;
	enum ppohVIS_BASE_eTopology *Type;
	int *NodeIndex;
	int *Node;
	double *Gravity;
};

struct ppohVIS_BASE_stMeshComm {
	int NeighborCount;
	int *Neighbor;
	int *ImportIndex;
	int *ImportItem;
	int *ExportIndex;
	int *ExportItem;
	int *SharedIndex;
	int *SharedItem;
};

struct ppohVIS_BASE_stMesh {
	struct ppohVIS_BASE_stMeshNode *Node;
	struct ppohVIS_BASE_stMeshElement *Element;
	struct ppohVIS_BASE_stMeshComm *Comm;
};


/*
 * allocate
 */
extern struct ppohVIS_BASE_stMeshNode *
ppohVIS_BASE_AllocateMeshNode(void);

extern struct ppohVIS_BASE_stMeshElement *
ppohVIS_BASE_AllocateMeshElement(void);

extern struct ppohVIS_BASE_stMeshComm *
ppohVIS_BASE_AllocateMeshComm(void);

extern struct ppohVIS_BASE_stMesh *
ppohVIS_BASE_AllocateMesh(void);

/*
 * initialize
 */
extern int
ppohVIS_BASE_InitMeshNode(
	struct ppohVIS_BASE_stMeshNode *pNode, int nNode);

extern int
ppohVIS_BASE_InitMeshElement(
	struct ppohVIS_BASE_stMeshElement *pElement, int nElement);

extern int
ppohVIS_BASE_InitMeshComm(
	struct ppohVIS_BASE_stMeshComm *pComm, int nNeighbor);

extern int
ppohVIS_BASE_InitMesh(
	struct ppohVIS_BASE_stMesh *pMesh,
	int nNode, int nElement, int nNeighbor);

/* 
 * free
 */
extern void
ppohVIS_BASE_FreeMeshNode(
	struct ppohVIS_BASE_stMeshNode *pNode);

extern void
ppohVIS_BASE_FreeMeshElement(
	struct ppohVIS_BASE_stMeshElement *pElement);

extern void
ppohVIS_BASE_FreeMeshComm(
	struct ppohVIS_BASE_stMeshComm *pComm);

extern void
ppohVIS_BASE_FreeMesh(
	struct ppohVIS_BASE_stMesh *pMesh);


/*
 * utilities
 */
extern int
ppohVIS_BASE_CalculateGravity(
	struct ppohVIS_BASE_stMesh *pMesh);


/*
 * I/O
 */
extern void
ppohVIS_BASE_PrintMeshNode(
	FILE *fp, struct ppohVIS_BASE_stMeshNode *pNode);

extern void
ppohVIS_BASE_PrintMeshElement(
	FILE *fp, struct ppohVIS_BASE_stMeshElement *pElem);

extern void
ppohVIS_BASE_PrintMesh(
	FILE *fp, struct ppohVIS_BASE_stMesh *pMesh);

extern int
ppohVIS_BASE_PutMeshNode(
	char *FileName, struct ppohVIS_BASE_stMeshNode *pNode, int Append);

extern int
ppohVIS_BASE_PutMeshElement(
	char *FileName, struct ppohVIS_BASE_stMeshElement *pElem, int Append);

extern int
ppohVIS_BASE_PutMesh(
	char *FileName, struct ppohVIS_BASE_stMesh *pMesh, int Append);

#endif /* __H_PPOHVIS_BASE_MESH */

