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
#ifndef __H_PPOHVIS_FDM3D_UCD
#define __H_PPOHVIS_FDM3D_UCD

#include "ppohVIS_FDM3D_Mesh.h"
#include "ppohVIS_FDM3D_Result.h"


#define PPOHVIS_FDM3D_UCD_LABEL_LEN (16)
#define PPOHVIS_FDM3D_UCD_DATALABEL_LEN (256)

static int ppohVIS_FDM3D_UCDTopologyCount = 12;

enum ppohVIS_FDM3D_eUCDTopology {
	ppohVIS_FDM3D_UCDTopology_Line2 = 0,
	ppohVIS_FDM3D_UCDTopology_Line3,
	ppohVIS_FDM3D_UCDTopology_Tria3,
	ppohVIS_FDM3D_UCDTopology_Tria6,
	ppohVIS_FDM3D_UCDTopology_Quad4,
	ppohVIS_FDM3D_UCDTopology_Quad8,
	ppohVIS_FDM3D_UCDTopology_Tetra4,
	ppohVIS_FDM3D_UCDTopology_Tetra10,
	ppohVIS_FDM3D_UCDTopology_Penta6,
	ppohVIS_FDM3D_UCDTopology_Penta15,
	ppohVIS_FDM3D_UCDTopology_Hexa8,
	ppohVIS_FDM3D_UCDTopology_Hexa20,
	ppohVIS_FDM3D_UCDTopology_Invalid,
};

static char ppohVIS_FDM3D_UCDLabel[][PPOHVIS_FDM3D_UCD_LABEL_LEN] = {
	"line",
	"line2",
	"tri",
	"tri2",
	"quad",
	"quad2",
	"tet",
	"tet2",
	"prism",
	"prism2",
	"hex",
	"hex2",
	"",
};

static int ppohVIS_FDM3D_UCDVertexCount[] = {
	2,
	3,
	3,
	6,
	4,
	8,
	4,
	10,
	6,
	15,
	8,
	20,
	-1,
};

enum ppohVIS_FDM3D_eUCDCycleType {
	ppohVIS_FDM3D_UCDCycle_Data,
	ppohVIS_FDM3D_UCDCycle_Geom,
	ppohVIS_FDM3D_UCDCycle_Both,
};

static char ppohVIS_FDM3D_UCDCycleLabel[][PPOHVIS_FDM3D_UCD_LABEL_LEN] = {
	"data",
	"geom",
	"data_geom",
};


struct ppohVIS_FDM3D_stUCDNode {
	int Count;
	int *ID;
	double *Coords;
};

struct ppohVIS_FDM3D_stUCDElement {
	int Count;
	int *ID;
	int *MaterialID;
	enum ppohVIS_FDM3D_eUCDTopology *Type;
	int *NodeIndex;
	int *Node;
};

struct ppohVIS_FDM3D_stUCDValue {
	int ItemCount;
	int Count;
	int Sum;
	int *DOF;
	char **Label;
	char **Unit;
	int *ID;
	double *Value;
};

struct ppohVIS_FDM3D_stUCDData {
	int Step;
	enum ppohVIS_FDM3D_eUCDCycleType CycleType;
	char Label[PPOHVIS_FDM3D_UCD_DATALABEL_LEN];
	struct ppohVIS_FDM3D_stUCDNode *Node;
	struct ppohVIS_FDM3D_stUCDElement *Element;
	struct ppohVIS_FDM3D_stUCDValue *NodeValue;
	struct ppohVIS_FDM3D_stUCDValue *ElementValue;
};


/******************************************************************************/
/*
 * allocate
 */
extern struct ppohVIS_FDM3D_stUCDNode *
ppohVIS_FDM3D_AllocateUCDNode(int NodeCount);

extern struct ppohVIS_FDM3D_stUCDElement *
ppohVIS_FDM3D_AllocateUCDElement(int ElemCount);

extern struct ppohVIS_FDM3D_stUCDValue *
ppohVIS_FDM3D_AllocateUCDValue(int ItemCount, int ValueSum, int ValueCount);

extern struct ppohVIS_FDM3D_stUCDData *
ppohVIS_FDM3D_AllocateUCD(void);


/*
 * free
 */
extern void
ppohVIS_FDM3D_FreeUCDNode(
	struct ppohVIS_FDM3D_stUCDNode *pUCDNode);

extern void
ppohVIS_FDM3D_FreeUCDElement(
	struct ppohVIS_FDM3D_stUCDElement *pUCDElement);

extern void
ppohVIS_FDM3D_FreeUCDValue(
	struct ppohVIS_FDM3D_stUCDValue *pUCDValue);

extern void
ppohVIS_FDM3D_FreeUCD(
	struct ppohVIS_FDM3D_stUCDData *pUCD);


/*
 * read
 */
extern struct ppohVIS_FDM3D_stUCDData *
ppohVIS_FDM3D_GetUCD(char *FileName);

extern struct ppohVIS_FDM3D_stUCDData *
ppohVIS_FDM3D_GetStepUCD(char *FileName);


/*
 * write
 */
extern void
ppohVIS_FDM3D_PrintUCDNode(
	FILE *fp, struct ppohVIS_FDM3D_stUCDNode *pUCDNode);

extern void
ppohVIS_FDM3D_PrintUCDElement(
	FILE *fp, struct ppohVIS_FDM3D_stUCDElement *pUCDElement);

extern void
ppohVIS_FDM3D_PrintUCDValue(
	FILE *fp, struct ppohVIS_FDM3D_stUCDValue *pUCDValue);

extern void
ppohVIS_FDM3D_PrintUCD(
	FILE *fp, struct ppohVIS_FDM3D_stUCDData *pUCDData);

extern int
ppohVIS_FDM3D_PutUCD(
	char *FileName, struct ppohVIS_FDM3D_stUCDData *pUCDData);

extern int
ppohVIS_FDM3D_PutUCD2(
	char *FileName, struct ppohVIS_FDM3D_stUCDData *pUCDData);


/*
 * convert
 */
extern struct ppohVIS_FDM3D_stMesh *
ppohVIS_FDM3D_ConvertUCD2Mesh(
	struct ppohVIS_FDM3D_stUCDData *pUCDData);

extern struct ppohVIS_FDM3D_stResult *
ppohVIS_FDM3D_ConvertUCD2Result(
	struct ppohVIS_FDM3D_stUCDData *pUCDData,
	enum ppohVIS_FDM3D_eResultEntityType Type, int ValueID);

extern struct ppohVIS_FDM3D_stUCDData *
ppohVIS_FDM3D_ConvertMesh2UCD(
	struct ppohVIS_FDM3D_stMesh *pMesh);


/*
 * utility
 */
extern int
ppohVIS_FDM3D_AddUCDValueFromResult(
	struct ppohVIS_FDM3D_stUCDData *pUCDData,
	struct ppohVIS_FDM3D_stResult *pResult);

#endif /* __H_PPOHVIS_FDM3D_UCD */

