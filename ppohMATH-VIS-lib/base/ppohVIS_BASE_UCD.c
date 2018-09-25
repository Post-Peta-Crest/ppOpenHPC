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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <omp.h>

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Lib.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_VoxelValue.h"
#include "ppohVIS_BASE_UCD.h"

#define BUFFER_LEN (1024)


/*******************************************************************************
 * Allocate
 ******************************************************************************/
/* Node */
extern struct ppohVIS_BASE_stUCDNode *
ppohVIS_BASE_AllocateUCDNode(const int NodeCount)
{
	struct ppohVIS_BASE_stUCDNode *pUCDNode = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stUCDNode);
	pUCDNode = (struct ppohVIS_BASE_stUCDNode *)malloc(BufSize);
	if(pUCDNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node data");
		goto error;
	} else {
		pUCDNode->Count = 0;
		pUCDNode->ID = NULL;
		pUCDNode->Coords = NULL;
	};

	pUCDNode->Count = NodeCount;

	pUCDNode->ID = (int *)calloc(pUCDNode->Count, sizeof(int));
	if(pUCDNode->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto error;
	};

	pUCDNode->Coords = (double *)malloc(sizeof(double)*pUCDNode->Count*3);
	if(pUCDNode->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "coordinates");
		goto error;
	};

	return pUCDNode;

error:
	ppohVIS_BASE_FreeUCDNode(pUCDNode);
	return NULL;
};


/* Element */
extern struct ppohVIS_BASE_stUCDElement *
ppohVIS_BASE_AllocateUCDElement(const int ElemCount)
{
	struct ppohVIS_BASE_stUCDElement *pUCDElem = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stUCDElement);
	pUCDElem = (struct ppohVIS_BASE_stUCDElement *)malloc(BufSize);
	if(pUCDElem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element data");
		goto error;
	} else {
		pUCDElem->Count = 0;
		pUCDElem->ID = NULL;
		pUCDElem->MaterialID = NULL;
		pUCDElem->Type = NULL;
		pUCDElem->NodeIndex = NULL;
		pUCDElem->Node = NULL;
	};

	pUCDElem->Count = ElemCount;

	pUCDElem->ID = (int *)calloc(pUCDElem->Count, sizeof(int));
	if(pUCDElem->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element ID");
		goto error;
	};

	pUCDElem->MaterialID = (int *)calloc(pUCDElem->Count, sizeof(int));
	if(pUCDElem->MaterialID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "material ID");
		goto error;
	};

	pUCDElem->Type = (enum ppohVIS_BASE_eUCDTopology *)calloc(pUCDElem->Count, sizeof(int));
	if(pUCDElem->Type == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element type");
		goto error;
	};

	pUCDElem->NodeIndex = (int *)calloc(pUCDElem->Count+1, sizeof(int));
	if(pUCDElem->NodeIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of connectivity");
		goto error;
	};

	pUCDElem->Node = (int *)calloc(pUCDElem->Count*8, sizeof(int));
	if(pUCDElem->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "connectivity");
		goto error;
	};

	return pUCDElem;

error:
	ppohVIS_BASE_FreeUCDElement(pUCDElem);
	return NULL;
};


/* Value */
extern struct ppohVIS_BASE_stUCDValue *
ppohVIS_BASE_AllocateUCDValue(
	const int ItemCount, const int ValueSum, const int ValueCount)
{
	struct ppohVIS_BASE_stUCDValue *pUCDValue = NULL;
	size_t BufSize;
	int BufCount, i;

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node value");
		goto error;
	} else {
		pUCDValue->ItemCount = 0;
		pUCDValue->Count = 0;
		pUCDValue->Sum = 0;
		pUCDValue->DOF = NULL;
		pUCDValue->Label = NULL;
		pUCDValue->Unit = NULL;
		pUCDValue->ID = NULL;
		pUCDValue->Value = NULL;
	};

	pUCDValue->ItemCount = ItemCount;
	pUCDValue->Sum = ValueSum;
	pUCDValue->Count = ValueCount;

	if(pUCDValue->Count > 0) {
		pUCDValue->DOF = (int *)calloc(pUCDValue->Count, sizeof(int));
		if(pUCDValue->DOF == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "DOF");
			goto error;
		};

		BufSize = sizeof(char *) * pUCDValue->Count;
		pUCDValue->Label = (char **)malloc(BufSize);
		if(pUCDValue->Label == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "Label");
			goto error;
		} else {
			for(i=0; i<pUCDValue->Count; i++) {
				pUCDValue->Label[i] = NULL;
			};
		};
		for(i=0; i<pUCDValue->Count; i++) {
			BufCount = PPOHVIS_BASE_LABEL_LEN;
			BufSize = sizeof(char);
			pUCDValue->Label[i] = (char *)calloc(BufCount, BufSize);
			if(pUCDValue->Label[i] == NULL) {
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_AllocationError,
					"%s [%s]", strerror(errno), "Label");
				goto error;
			};
		};

		BufSize = sizeof(char *) * pUCDValue->Count;
		pUCDValue->Unit = (char **)malloc(BufSize);
		if(pUCDValue->Unit == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "Unit");
			goto error;
		} else {
			for(i=0; i<pUCDValue->Count; i++) {
				pUCDValue->Unit[i] = NULL;
			};
		};
		for(i=0; i<pUCDValue->Count; i++) {
			BufCount = PPOHVIS_BASE_LABEL_LEN;
			BufSize = sizeof(char);
			pUCDValue->Unit[i] = (char *)calloc(BufCount, BufSize);
			if(pUCDValue->Unit[i] == NULL) {
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_AllocationError,
					"%s [%s]", strerror(errno), "Unit");
				goto error;
			};
		};
	};

	if(pUCDValue->ItemCount > 0 && pUCDValue->Sum > 0) {
		BufCount = pUCDValue->ItemCount;
		BufSize = sizeof(int);
		pUCDValue->ID = (int *)calloc(BufCount, BufSize);
		if(pUCDValue->ID == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "ID");
			goto error;
		};

		BufSize = pUCDValue->Sum*pUCDValue->ItemCount*sizeof(double);
		pUCDValue->Value = (double *)malloc(BufSize);
		if(pUCDValue->Value == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "value");
			goto error;
		};
	};

	return pUCDValue;

error:
	ppohVIS_BASE_FreeUCDValue(pUCDValue);
	return NULL;
};


extern struct ppohVIS_BASE_stUCDValue *
ppohVIS_BASE_AllocateUCDNodeValue(
	struct ppohVIS_BASE_stMeshNode *pNode)
{
	struct ppohVIS_BASE_stUCDValue *pUCDValue = NULL;
	size_t BufSize;
	int iNode;

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node value");
		goto error;
	} else {
		pUCDValue->ItemCount = 0;
		pUCDValue->Count = 0;
		pUCDValue->Sum = 0;
		pUCDValue->DOF = NULL;
		pUCDValue->Label = NULL;
		pUCDValue->Unit = NULL;
		pUCDValue->ID = NULL;
		pUCDValue->Value = NULL;
	};

	pUCDValue->ItemCount = pNode->Count;
	pUCDValue->Sum = 0;
	pUCDValue->Count = 0;

	pUCDValue->ID = (int *)calloc(pUCDValue->ItemCount, sizeof(int));
	if(pUCDValue->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "ID");
		goto error;
	} else {
		for(iNode=0; iNode<pNode->Count; iNode++) {
			pUCDValue->ID[iNode] = pNode->ID[iNode];
		};
	};

	return pUCDValue;

error:
	ppohVIS_BASE_FreeUCDValue(pUCDValue);
	return NULL;
};


extern struct ppohVIS_BASE_stUCDValue *
ppohVIS_BASE_AllocateUCDElementValue(
	struct ppohVIS_BASE_stMeshElement *pElem)
{
	struct ppohVIS_BASE_stUCDValue *pUCDValue = NULL;
	size_t BufSize;
	int iElem;

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node value");
		goto error;
	} else {
		pUCDValue->ItemCount = 0;
		pUCDValue->Count = 0;
		pUCDValue->Sum = 0;
		pUCDValue->DOF = NULL;
		pUCDValue->Label = NULL;
		pUCDValue->Unit = NULL;
		pUCDValue->ID = NULL;
		pUCDValue->Value = NULL;
	};

	pUCDValue->ItemCount = pElem->Count;
	pUCDValue->Sum = 0;
	pUCDValue->Count = 0;

	pUCDValue->ID = (int *)calloc(pUCDValue->ItemCount, sizeof(int));
	if(pUCDValue->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "ID");
		goto error;
	} else {
		for(iElem=0; iElem<pElem->Count; iElem++) {
			pUCDValue->ID[iElem] = pElem->ID[iElem];
		};
	};

	return pUCDValue;

error:
	ppohVIS_BASE_FreeUCDValue(pUCDValue);
	return NULL;
};


extern int
ppohVIS_BASE_ReallocateUCDValue(
	struct ppohVIS_BASE_stUCDValue *pUCDValue,
	const int ValueSum, const int ValueCount)
{
	double *ValueTmp;
	int ValueSumBak, ValueCountBak;
	size_t BufSize;
	int BufCount, iPos, jPos, i, j;

	ValueSumBak = pUCDValue->Sum;
	ValueCountBak = pUCDValue->Count;

	pUCDValue->Sum = ValueSum;
	pUCDValue->Count = ValueCount;

	if(pUCDValue->Count > 0) {
		BufSize = sizeof(int) * pUCDValue->Count;
		pUCDValue->DOF = (int *)realloc(pUCDValue->DOF, BufSize);
		if(pUCDValue->DOF == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "DOF");
			goto error;
		};

		BufSize = sizeof(char *) * pUCDValue->Count;
		pUCDValue->Label = (char **)realloc(pUCDValue->Label, BufSize);
		if(pUCDValue->Label == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "Label");
			goto error;
		} else {
			for(i=ValueCountBak; i<pUCDValue->Count; i++) {
				pUCDValue->Label[i] = NULL;
			};
		};
		for(i=ValueCountBak; i<pUCDValue->Count; i++) {
			BufCount = PPOHVIS_BASE_LABEL_LEN;
			BufSize = sizeof(char);
			pUCDValue->Label[i] = (char *)calloc(BufCount, BufSize);
			if(pUCDValue->Label[i] == NULL) {
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_AllocationError,
					"%s [%s]", strerror(errno), "Label");
				goto error;
			};
		};

		BufSize = sizeof(char *)*pUCDValue->Count;
		pUCDValue->Unit = (char **)realloc(pUCDValue->Unit, BufSize);
		if(pUCDValue->Unit == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "Unit");
			goto error;
		} else {
			for(i=ValueCountBak; i<pUCDValue->Count; i++) {
				pUCDValue->Unit[i] = NULL;
			};
		};
		for(i=ValueCountBak; i<pUCDValue->Count; i++) {
			BufCount = PPOHVIS_BASE_LABEL_LEN;
			BufSize = sizeof(char);
			pUCDValue->Unit[i] = (char *)calloc(BufCount, BufSize);
			if(pUCDValue->Unit[i] == NULL) {
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_AllocationError,
					"%s [%s]", strerror(errno), "Unit");
				goto error;
			};
		};
	};

	if(pUCDValue->ItemCount > 0 && pUCDValue->Sum > 0) {
		BufSize = ValueSumBak*pUCDValue->ItemCount*sizeof(double);
		ValueTmp = (double *)malloc(BufSize);
		if(ValueTmp == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "value tmp");
			goto error;
		};

		BufSize = ValueSumBak*pUCDValue->ItemCount*sizeof(double);
		memcpy(ValueTmp, pUCDValue->Value, BufSize);

		BufSize = pUCDValue->Sum*pUCDValue->ItemCount*sizeof(double);
		pUCDValue->Value = (double *)realloc(pUCDValue->Value, BufSize);
		if(pUCDValue->Value == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "value");
			goto error;
		};

		for(i=0; i<pUCDValue->ItemCount; i++) {
			for(j=0; j<ValueSumBak; j++) {
				iPos = pUCDValue->Sum*i+j;
				jPos = ValueSumBak*i+j;
				pUCDValue->Value[iPos] = ValueTmp[jPos];
			};
			for(j=ValueSumBak; j<pUCDValue->Sum; j++) {
				iPos = pUCDValue->Sum*i+j;
				pUCDValue->Value[iPos] = 0.0;
			};
		};
	};

	return 0;

error:
	ppohVIS_BASE_FreeUCDValue(pUCDValue);
	return -1;
};


/* Base */
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_AllocateUCD(void)
{
	struct ppohVIS_BASE_stUCDData *pUCD = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stUCDData);
	pUCD = (struct ppohVIS_BASE_stUCDData *)malloc(BufSize);
	if(pUCD == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD data");
		goto error;
	} else {
		pUCD->Node = NULL;
		pUCD->Element = NULL;
		pUCD->NodeValue = NULL;
		pUCD->ElementValue = NULL;
	};

	return pUCD;

error:
	ppohVIS_BASE_FreeUCD(pUCD);
	return NULL;
};


/*******************************************************************************
 * Free
 ******************************************************************************/
extern void
ppohVIS_BASE_FreeUCDNode(
	struct ppohVIS_BASE_stUCDNode *pUCDNode)
{
	if(pUCDNode != NULL) {
		free(pUCDNode->ID);
		free(pUCDNode->Coords);
		free(pUCDNode);
	};
	pUCDNode = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeUCDElement(
	struct ppohVIS_BASE_stUCDElement *pUCDElem)
{
	if(pUCDElem != NULL) {
		free(pUCDElem->ID);
		free(pUCDElem->MaterialID);
		free(pUCDElem->Type);
		free(pUCDElem->NodeIndex);
		free(pUCDElem->Node);
		free(pUCDElem);
	};
	pUCDElem = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeUCDValue(
	struct ppohVIS_BASE_stUCDValue *pUCDValue)
{
	int i;

	if(pUCDValue != NULL) {
		if(pUCDValue->DOF != NULL) {
			free(pUCDValue->DOF);
		};
		if(pUCDValue->Label != NULL) {
			for(i=0; i<pUCDValue->Count; i++) {
				if(pUCDValue->Label[i] != NULL) {
					free(pUCDValue->Label[i]);
				};
			};
			free(pUCDValue->Label);
		};
		if(pUCDValue->Unit != NULL) {
			for(i=0; i<pUCDValue->Count; i++) {
				if(pUCDValue->Unit[i] != NULL) {
					free(pUCDValue->Unit[i]);
				};
			};
			free(pUCDValue->Unit);
		};
		if(pUCDValue->ID != NULL) {
			free(pUCDValue->ID);
		};
		if(pUCDValue->Value != NULL) {
			free(pUCDValue->Value);
		};
		free(pUCDValue);
	};
	pUCDValue = NULL;
};


/*============================================================================*/
extern void
ppohVIS_BASE_FreeUCD(struct ppohVIS_BASE_stUCDData *pUCD)
{
	if(pUCD != NULL) {
		ppohVIS_BASE_FreeUCDNode(pUCD->Node);
		ppohVIS_BASE_FreeUCDElement(pUCD->Element);
		ppohVIS_BASE_FreeUCDValue(pUCD->NodeValue);
		ppohVIS_BASE_FreeUCDValue(pUCD->ElementValue);
		free(pUCD);
	};
	pUCD = NULL;
};


/*******************************************************************************
 *
 ******************************************************************************/
static char *
GetUCDLabel(enum ppohVIS_BASE_eUCDTopology Code)
{
	if(0 <= Code && Code < ppohVIS_BASE_UCDTopology_Invalid) {
		return ppohVIS_BASE_UCDLabel[Code];
	} else {
		return ppohVIS_BASE_UCDLabel[ppohVIS_BASE_UCDTopology_Invalid];
	};
};


/*============================================================================*/
static int
GetUCDVertex(enum ppohVIS_BASE_eUCDTopology Code)
{
	if(0 <= Code && Code < ppohVIS_BASE_UCDTopology_Invalid) {
		return ppohVIS_BASE_UCDVertexCount[Code];
	} else {
		return ppohVIS_BASE_UCDVertexCount[ppohVIS_BASE_UCDTopology_Invalid];
	};
};


/*============================================================================*/
static enum ppohVIS_BASE_eUCDTopology
GetUCDType(char *ElemType)
{
	char *s;
	int i;

	for(i=0; ; i++) {
		s = ppohVIS_BASE_UCDLabel[i];
		if(s == NULL) break;

		if(strcmp(s, ElemType) == 0) {
			return (enum ppohVIS_BASE_eUCDTopology)(i);
		};
	};

	return ppohVIS_BASE_UCDTopology_Invalid;
};


/*******************************************************************************
 * Read UCD File
 ******************************************************************************/
/*==================*
 * node information *
 *==================*/
static int
GetUCDNode(FILE *fp, struct ppohVIS_BASE_stUCDNode *pNode)
{
	int iNode;
	char Line[BUFFER_LEN];

	pNode->ID = (int *)calloc(pNode->Count, sizeof(int));
	if(pNode->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		return -1;
	};
	pNode->Coords = (double *)malloc(sizeof(double)*pNode->Count*3);
	if(pNode->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "coordinates");
		return -1;
	};

	for(iNode=0; iNode<pNode->Count; iNode++) {
		if(fgets(Line, BUFFER_LEN, fp) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileReadError,
				"%s [%s]", strerror(errno), "node info");
			return -1;
		};

		sscanf(Line, "%d%le%le%le",
			&pNode->ID[iNode],
			&pNode->Coords[3*iNode],
			&pNode->Coords[3*iNode+1],
			&pNode->Coords[3*iNode+2]);
	};

	return 0;
};

/*=====================*
 * element information *
 *=====================*/
static int
GetUCDElement(FILE *fp, struct ppohVIS_BASE_stUCDElement *pElem)
{
	int iElem, iNode, iNodeS, ID, MaterialID, DummyI, VertexCount;
	int Nodes[20];
	enum ppohVIS_BASE_eUCDTopology Type;
	char Line[BUFFER_LEN+1], ElemType[PPOHVIS_BASE_UCD_LABEL_LEN];
	fpos_t fPos;
	size_t BufSize;
	int BufCount;

	pElem->ID = (int *)calloc(pElem->Count, sizeof(int));
	if(pElem->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element ID");
		return -1;
	};

	pElem->MaterialID = (int *)calloc(pElem->Count, sizeof(int));
	if(pElem->MaterialID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "material ID");
		return -1;
	};

	BufSize = sizeof(enum ppohVIS_BASE_eUCDTopology);
	BufCount = pElem->Count;
	pElem->Type = (enum ppohVIS_BASE_eUCDTopology *)calloc(BufCount, BufSize);
	if(pElem->Type == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element type");
		return -1;
	};

	pElem->NodeIndex = (int *)calloc(pElem->Count+1, sizeof(int));
	if(pElem->NodeIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of connectivity");
		return -1;
	};

	if(fgetpos(fp, &fPos)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "element info");
		return -1;
	};


	for(iElem=0; iElem<pElem->Count; iElem++) {
		memset(ElemType, '\0', PPOHVIS_BASE_UCD_LABEL_LEN);

		if(fgets(Line, BUFFER_LEN, fp) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileReadError,
				"%s [%s]", strerror(errno), "element info");
			return -1;
		};
		sscanf(Line, "%d%d%s", &ID, &MaterialID, ElemType);

		pElem->ID[iElem] = ID;
		pElem->MaterialID[iElem] = MaterialID;

		Type = GetUCDType(ElemType);
		if(Type == ppohVIS_BASE_UCDTopology_Invalid) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_InvalidElemType,
				"%d:%s:%d", iElem, ElemType, Type);
			return -1;
		};

		pElem->Type[iElem] = Type;

		VertexCount = GetUCDVertex(Type);
		if(VertexCount < 0) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_InvalidElemType,
				"%d", Type);
			return -1;
		}

		pElem->NodeIndex[iElem+1] = pElem->NodeIndex[iElem] + VertexCount;
	};

	BufSize = sizeof(int);
	BufCount = pElem->NodeIndex[pElem->Count];
	pElem->Node = (int *)calloc(BufCount, BufSize);
	if(pElem->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "connectivity");
		return -1;
	};

	if(fsetpos(fp, &fPos)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "element info");
		return -1;
	};

	for(iElem=0; iElem<pElem->Count; iElem++) {
		if(fgets(Line, BUFFER_LEN, fp) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileReadError,
				"%s [%s]", strerror(errno), "element info");
			return -1;
		};

		switch (pElem->Type[iElem])
		{
			case ppohVIS_BASE_UCDTopology_Tetra4:
				sscanf(Line, "%d%d%s%d%d%d%d",
					&DummyI, &DummyI, ElemType,
					&Nodes[0], &Nodes[1],
					&Nodes[2], &Nodes[3]);
				break;

			case ppohVIS_BASE_UCDTopology_Penta6:
				sscanf(Line, "%d%d%s%d%d%d%d%d%d",
					&DummyI, &DummyI, ElemType,
					&Nodes[0], &Nodes[1], &Nodes[2],
					&Nodes[3], &Nodes[4], &Nodes[5]);
				break;

			case ppohVIS_BASE_UCDTopology_Hexa8:
				sscanf(Line, "%d%d%s%d%d%d%d%d%d%d%d",
					&DummyI, &DummyI, ElemType,
					&Nodes[0], &Nodes[1],
					&Nodes[2], &Nodes[3],
					&Nodes[4], &Nodes[5],
					&Nodes[6], &Nodes[7]);
				break;

			default:
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_InvalidElemType,
					"%d", pElem->Type[iElem]);
				return -1;

		};

		iNodeS = pElem->NodeIndex[iElem];
		VertexCount = GetUCDVertex(Type);
		for(iNode=0; iNode<VertexCount; iNode++) {
			pElem->Node[iNodeS+iNode] = Nodes[iNode];
		};
	};

	return 0;
};

/*========*
 * values *
 *========*/
static int
GetUCDValue(FILE *fp, struct ppohVIS_BASE_stUCDValue *pValue)
{
	int Count;
	int i, j;
	char Line[BUFFER_LEN], *Token;
	int BufCount;
	size_t BufSize;

	pValue->ID = (int *)calloc(pValue->ItemCount, sizeof(int));
	if(pValue->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "ID");
		goto error;
	};

	BufSize = sizeof(double)*pValue->Sum*pValue->ItemCount;
	pValue->Value = (double *)malloc(BufSize);
	if(pValue->Value == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "value");
		goto error;
	};

	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD value");
		goto error;
	};

	sscanf(Line, "%d", &Count);
	if(Count <= 0) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s", "# of UCD value items is zero");
		goto error;
	};
	pValue->Count = Count;

	pValue->DOF = (int *)calloc(pValue->Count, sizeof(int));
	if(pValue->DOF == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD value DOF");
		goto error;
	};
	pValue->Label = (char **)malloc(sizeof(char *)*pValue->Count);
	if(pValue->Label == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD value label");
		goto error;
	};
	pValue->Unit = (char **)malloc(sizeof(char *)*pValue->Count);
	if(pValue->Unit == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD value unit");
		goto error;
	};
	for(i=0; i<pValue->Count; i++) {
		BufSize = sizeof(char);
		BufCount = PPOHVIS_BASE_UCD_DATALABEL_LEN;
		pValue->Label[i] = (char *)calloc(BufCount, BufSize);
		if(pValue->Label[i] == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "UCD value label");
			goto error;
		};

		BufSize = sizeof(char);
		BufCount = PPOHVIS_BASE_UCD_DATALABEL_LEN;
		pValue->Unit[i] = (char *)calloc(BufCount, BufSize);
		if(pValue->Unit[i] == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "UCD value unit");
			goto error;
		};
	};

	ppohVIS_BASE_StrSSV2CSV(Line);

	Token = strtok(Line, ",");
	for(i=0; i<pValue->Count; i++) {
		Token = strtok(NULL, ",");
		if(Token == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileReadError,
				"%s", "UCD value DOF is too few");
			goto error;
		}

		sscanf(Token, "%d", &pValue->DOF[i]);
	};

	for(i=0; i<pValue->Count; i++) {
		if(fgets(Line, BUFFER_LEN, fp) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileReadError,
				"%s [%s]", strerror(errno), "UCD label & unit");
			goto error;
		};

		Token = strtok(Line, ",");
		sscanf(Token, "%s", pValue->Label[i]);

		Token = strtok(NULL, ",");
		sscanf(Token, "%s", pValue->Unit[i]);
	};

	for(i=0; i<pValue->ItemCount; i++) {
		if(fgets(Line, BUFFER_LEN, fp) == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_FileReadError,
				"%s [%s]", strerror(errno), "UCD value");
			goto error;
		};

		ppohVIS_BASE_StrSSV2CSV(Line);

		Token = strtok(Line, ",");
		sscanf(Token, "%d", &pValue->ID[i]);

		for(j=0; j<pValue->Sum; j++) {
			Token = strtok(NULL, ",");
			sscanf(Token, "%le", &pValue->Value[i*pValue->Sum+j]);
		};
	};

	return 0;

error:
	return -1;
};


/*======================*
 * Single step UCD File *
 *======================*/
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_GetUCD(char *FileName)
{
	int ValueI[5];
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	char Line[BUFFER_LEN];
	FILE *fp = NULL;
	size_t BufSize;

	fp = fopen(FileName, "r");
	if(fp == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDData);
	pUCDData = (struct ppohVIS_BASE_stUCDData *)malloc(BufSize);
	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD data");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDNode);
	pUCDData->Node = (struct ppohVIS_BASE_stUCDNode *)malloc(BufSize);
	if(pUCDData->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD node info");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDElement);
	pUCDData->Element = (struct ppohVIS_BASE_stUCDElement *)malloc(BufSize);
	if(pUCDData->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD element info");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDData->NodeValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDData->NodeValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD node value info");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDData->ElementValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDData->ElementValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD element value info");
		goto error;
	};

	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};
	sscanf(Line, "%d%d%d%d%d", &ValueI[0], &ValueI[1], &ValueI[2],
	                           &ValueI[3], &ValueI[4]);
	pUCDData->Node->Count = ValueI[0];
	pUCDData->Element->Count = ValueI[1];
	pUCDData->NodeValue->Sum = ValueI[2];
	pUCDData->NodeValue->ItemCount = pUCDData->Node->Count;
	pUCDData->ElementValue->Sum = ValueI[3];
	pUCDData->ElementValue->ItemCount = pUCDData->Element->Count;

	if(GetUCDNode(fp, pUCDData->Node)) goto error;
	if(GetUCDElement(fp, pUCDData->Element)) goto error;

	if(pUCDData->NodeValue->Sum > 0) {
		if(GetUCDValue(fp, pUCDData->NodeValue)) goto error;
	} else {
		pUCDData->NodeValue->Count = 0;
		pUCDData->NodeValue->Label = NULL;
		pUCDData->NodeValue->Unit = NULL;
		pUCDData->NodeValue->ID = NULL;
		pUCDData->NodeValue->Value = NULL;
	};
	if(pUCDData->ElementValue->Sum > 0) {
		if(GetUCDValue(fp, pUCDData->ElementValue)) goto error;
	} else {
		pUCDData->ElementValue->Count = 0;
		pUCDData->ElementValue->Label = NULL;
		pUCDData->ElementValue->Unit = NULL;
		pUCDData->ElementValue->ID = NULL;
		pUCDData->ElementValue->Value = NULL;
	};

	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	return pUCDData;

error:
	return NULL;
};


/*=====================*
 * Multi-step UCD file *
 *=====================*/
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_GetStepUCD(char *FileName)
{
	int ValueI[2];
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	char Line[BUFFER_LEN];
	FILE *fp = NULL;
	size_t BufSize;

	fp = fopen(FileName, "r");
	if(fp == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDData);
	pUCDData = (struct ppohVIS_BASE_stUCDData *)malloc(BufSize);
	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD data");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDNode);
	pUCDData->Node = (struct ppohVIS_BASE_stUCDNode *)malloc(BufSize);
	if(pUCDData->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD node info");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDElement);
	pUCDData->Element = (struct ppohVIS_BASE_stUCDElement *)malloc(BufSize);
	if(pUCDData->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD element info");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDData->NodeValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDData->NodeValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD node value info");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stUCDValue);
	pUCDData->ElementValue = (struct ppohVIS_BASE_stUCDValue *)malloc(BufSize);
	if(pUCDData->ElementValue == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "UCD element value info");
		goto error;
	};

	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};
	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};
	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};
	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};
	sscanf(Line, "%d%d", &ValueI[0], &ValueI[1]);
	pUCDData->Node->Count = ValueI[0];
	pUCDData->Element->Count = ValueI[1];

	/* node information */
	if(GetUCDNode(fp, pUCDData->Node)) goto error;

	/* element information */
	if(GetUCDElement(fp, pUCDData->Element)) goto error;

	/* value information */
	if(fgets(Line, BUFFER_LEN, fp) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileReadError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};
	sscanf(Line, "%d%d", &ValueI[0], &ValueI[1]);
	pUCDData->NodeValue->Sum = ValueI[0];
	pUCDData->NodeValue->ItemCount = pUCDData->Node->Count;
	pUCDData->ElementValue->Sum = ValueI[1];
	pUCDData->ElementValue->ItemCount = pUCDData->Element->Count;

	/* node value information */
	if(pUCDData->NodeValue->Sum > 0) {
		if(GetUCDValue(fp, pUCDData->NodeValue)) goto error;
	} else {
		pUCDData->NodeValue->Count = 0;
		pUCDData->NodeValue->Label = NULL;
		pUCDData->NodeValue->Unit = NULL;
		pUCDData->NodeValue->ID = NULL;
		pUCDData->NodeValue->Value = NULL;
	};

	/* element value information */
	if(pUCDData->ElementValue->Sum > 0) {
		if(GetUCDValue(fp, pUCDData->ElementValue)) goto error;
	} else {
		pUCDData->ElementValue->Count = 0;
		pUCDData->ElementValue->Label = NULL;
		pUCDData->ElementValue->Unit = NULL;
		pUCDData->ElementValue->ID = NULL;
		pUCDData->ElementValue->Value = NULL;
	};

	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	return pUCDData;

error:
	return NULL;
};



/*******************************************************************************
 * Create mesh information from UCD data
 ******************************************************************************/
/*==================*
 * Node information *
 *==================*/
static struct ppohVIS_BASE_stMeshNode *
ConvertUCD2MeshNode(struct ppohVIS_BASE_stUCDNode *pUCDNode)
{
	struct ppohVIS_BASE_stMeshNode *pNode = NULL;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stMeshNode);
	pNode = (struct ppohVIS_BASE_stMeshNode *)malloc(BufSize);
	if(pNode == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node info");
		goto error;
	};

	pNode->Count = pUCDNode->Count;
	pNode->ID = (int *)calloc(pNode->Count, sizeof(int));
	if(pNode->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node ID");
		goto error;
	};
	pNode->Coords = (double *)malloc(sizeof(double)*pNode->Count*3);
	if(pNode->Coords == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "coordinate");
		goto error;
	};

	memcpy(pNode->ID, pUCDNode->ID, sizeof(int)*pNode->Count);
	memcpy(pNode->Coords, pUCDNode->Coords, sizeof(double)*pNode->Count*3);

	return pNode;

error:
	return NULL;
};


/*=====================*
 * Element information *
 *=====================*/
static struct ppohVIS_BASE_stMeshElement *
ConvertUCD2MeshElement(struct ppohVIS_BASE_stUCDElement *pUCDElem)
{
	struct ppohVIS_BASE_stMeshElement *pElem = NULL;
	int Count, iElem, BufCount;
	int iRc = 0;
	size_t BufSize;

	BufSize = sizeof(struct ppohVIS_BASE_stMeshElement);
	pElem = (struct ppohVIS_BASE_stMeshElement *)malloc(BufSize);
	if(pElem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element info");
		goto error;
	};

	pElem->Count = pUCDElem->Count;
	pElem->ID = (int *)calloc(pElem->Count, sizeof(int));
	if(pElem->ID == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element ID");
		goto error;
	};

	BufSize = sizeof(enum ppohVIS_BASE_eTopology);
	BufCount = pElem->Count;
	pElem->Type = (enum ppohVIS_BASE_eTopology *)calloc(BufCount, BufSize);
	if(pElem->Type == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "element type");
		goto error;
	};
	pElem->NodeIndex = (int *)calloc(pElem->Count+1, sizeof(int));
	if(pElem->NodeIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of connectivity");
		goto error;
	};
	Count = pUCDElem->NodeIndex[pUCDElem->Count];
	pElem->Node = (int *)calloc(Count, sizeof(int));
	if(pElem->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "connectivity");
		goto error;
	};
	pElem->Gravity = NULL;

	memcpy(pElem->ID, pUCDElem->ID, sizeof(int)*pElem->Count);
	memcpy(pElem->NodeIndex, pUCDElem->NodeIndex, sizeof(int)*(pElem->Count+1));
	memcpy(pElem->Node, pUCDElem->Node, sizeof(int)*Count);

#pragma omp parallel for reduction(+:iRc)
	for(iElem=0; iElem<pElem->Count; iElem++) {
		switch(pUCDElem->Type[iElem])
		{
			case ppohVIS_BASE_UCDTopology_Line2:
				pElem->Type[iElem] = ppohVIS_BASE_Line2;
				break;
			case ppohVIS_BASE_UCDTopology_Line3:
				pElem->Type[iElem] = ppohVIS_BASE_Line3;
				break;
			case ppohVIS_BASE_UCDTopology_Tria3:
				pElem->Type[iElem] = ppohVIS_BASE_Tria3;
				break;
			case ppohVIS_BASE_UCDTopology_Tria6:
				pElem->Type[iElem] = ppohVIS_BASE_Tria6;
				break;
			case ppohVIS_BASE_UCDTopology_Quad4:
				pElem->Type[iElem] = ppohVIS_BASE_Quad4;
				break;
			case ppohVIS_BASE_UCDTopology_Quad8:
				pElem->Type[iElem] = ppohVIS_BASE_Quad8;
				break;
			case ppohVIS_BASE_UCDTopology_Tetra4:
				pElem->Type[iElem] = ppohVIS_BASE_Tetra4;
				break;
			case ppohVIS_BASE_UCDTopology_Tetra10:
				pElem->Type[iElem] = ppohVIS_BASE_Tetra10;
				break;
			case ppohVIS_BASE_UCDTopology_Penta6:
				pElem->Type[iElem] = ppohVIS_BASE_Penta6;
				break;
			case ppohVIS_BASE_UCDTopology_Penta15:
				pElem->Type[iElem] = ppohVIS_BASE_Penta15;
				break;
			case ppohVIS_BASE_UCDTopology_Hexa8:
				pElem->Type[iElem] = ppohVIS_BASE_Hexa8;
				break;
			case ppohVIS_BASE_UCDTopology_Hexa20:
				pElem->Type[iElem] = ppohVIS_BASE_Hexa20;
				break;
			default:
				iRc++;
				break;
		};
	};
	if(iRc) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_InvalidElemType,
			"%d", pUCDElem->Type[iElem]);
		goto error;
	};

	return pElem;

error:
	return NULL;
};


/*=================*
 * All information *
 *=================*/
extern struct ppohVIS_BASE_stMesh *
ppohVIS_BASE_ConvertUCD2Mesh(struct ppohVIS_BASE_stUCDData *pUCDData)
{
	struct ppohVIS_BASE_stMesh *pMesh = NULL;
	size_t BufSize;


	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "UCD data");
		goto error;
	};
	if(pUCDData->Node == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "node info in UCD data");
		goto error;
	};
	if(pUCDData->Element == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "element info in UCD data");
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stMesh);
	pMesh = (struct ppohVIS_BASE_stMesh *)malloc(BufSize);
	if(pMesh == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "mesh info");
		goto error;
	};

	pMesh->Node = ConvertUCD2MeshNode(pUCDData->Node);
	if(pMesh->Node == NULL) {
		goto error;
	};

	pMesh->Element = ConvertUCD2MeshElement(pUCDData->Element);
	if(pMesh->Element == NULL) {
		goto error;
	};

	return pMesh;

error:
	return NULL;
};


/*******************************************************************************
 * Create result information from UCD data
 ******************************************************************************/
static struct ppohVIS_BASE_stResult *
ConvertUCD2Result(
	struct ppohVIS_BASE_stUCDValue *pUCDValue,
	enum ppohVIS_BASE_eResultEntityType Type, int ValueID)
{
	struct ppohVIS_BASE_stResult *pResult = NULL;
	int DOFSum, DOFCount, DOFBase;
	size_t BufSize;
	int iPos, jPos, i, j;

	if(ValueID <= 0 || pUCDValue->Count < ValueID) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_InvalidData,
			"%s [%d]", "value ID", ValueID);
		goto error;
	};

	BufSize = sizeof(struct ppohVIS_BASE_stResult);
	pResult = (struct ppohVIS_BASE_stResult *)malloc(BufSize);
	if(pResult == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "result");
		goto error;
	};

	DOFSum = pUCDValue->Sum;
	DOFCount = pUCDValue->DOF[ValueID-1];
	DOFBase = 0;
	for(i=0; i<ValueID-1; i++) {
		DOFBase += pUCDValue->DOF[i];
	};

	BufSize = sizeof(double)*pUCDValue->ItemCount*DOFCount*3;
	pResult->Value = (double *)malloc(BufSize);
	if(pResult->Value == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "result value");
		goto error;
	};

	pResult->ItemCount = pUCDValue->ItemCount;
	pResult->EntityType = Type;
	pResult->FreedomCount = DOFCount;

	BufSize = PPOHVIS_BASE_LABEL_LEN;
	strncpy(pResult->Label, pUCDValue->Label[ValueID-1], BufSize);

	for(i=0; i<pUCDValue->ItemCount; i++) {
		for(j=0; j<DOFCount; j++) {
			iPos = DOFCount*i+j;
			jPos = DOFSum*i+DOFBase+j;
			pResult->Value[iPos] = pUCDValue->Value[jPos];
		};
	};

	return pResult;

error:
	return NULL;
};


extern struct ppohVIS_BASE_stResult *
ppohVIS_BASE_ConvertUCD2Result(
	struct ppohVIS_BASE_stUCDData *pUCDData,
	enum ppohVIS_BASE_eResultEntityType Type, int ValueID)
{
	struct ppohVIS_BASE_stResult *pResult;

	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "UCD data");
		goto error;
	};

	if(Type == ppohVIS_BASE_ResultNode) {
		if(pUCDData->Node == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_NullPointerFound,
				"%s", "node info in UCD data");
			goto error;
		};
		pResult = ConvertUCD2Result(pUCDData->NodeValue, Type, ValueID);
		if(pResult == NULL) {
			goto error;
		};

	} else if(Type == ppohVIS_BASE_ResultElement) {
		if(pUCDData->Element == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_NullPointerFound,
				"%s", "element info in UCD data");
			goto error;
		};
		pResult = ConvertUCD2Result(pUCDData->ElementValue, Type, ValueID);
		if(pResult == NULL) {
			goto error;
		};

	} else {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_UnknownEntityType, "%d", Type);
		goto error;
	};


	return pResult;

error:
	return NULL;
};


/*******************************************************************************
 * Create UCD format file
 ******************************************************************************/
/*==================*
 * Node information *
 *==================*/
static int
PutUCDNode(FILE *fp, struct ppohVIS_BASE_stUCDNode *pNode)
{
	int iNode;

	for(iNode=0; iNode<pNode->Count; iNode++) {
		fprintf(fp, "%d %14.7e %14.7e %14.7e\n",
			iNode+1,
			pNode->Coords[3*iNode+0],
			pNode->Coords[3*iNode+1],
			pNode->Coords[3*iNode+2]);
	};

	return 0;
};

/*=====================*
 * Element information *
 *=====================*/
static int
PutUCDElement(FILE *fp, struct ppohVIS_BASE_stUCDElement *pElem)
{
	char *Label;
	enum ppohVIS_BASE_eUCDTopology ElemType;
	int NodeCount, iElem, iNode, iNodeS, iNodeE;

	for(iElem=0; iElem<pElem->Count; iElem++) {
		fprintf(fp, "%d %d",
			iElem+1,
			pElem->MaterialID[iElem]);

		ElemType = pElem->Type[iElem];
		NodeCount = GetUCDVertex(ElemType);
		Label = GetUCDLabel(ElemType);

		if(NodeCount < 0 || Label == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_InvalidElemType,
				"%s", "UCD data");
			return -1;
		};

		fprintf(fp, " %s", Label);

		iNodeS = pElem->NodeIndex[iElem];
		iNodeE = pElem->NodeIndex[iElem+1];
		for(iNode=iNodeS; iNode<iNodeE; iNode++) {
			fprintf(fp, " %d", pElem->Node[iNode]);
		};
		fprintf(fp, "\n");
	};

	return 0;
};


/*===================*
 * Value information *
 *===================*/
static int
PutUCDValue(FILE *fp, struct ppohVIS_BASE_stUCDValue *pUCDValue)
{
	int i, j;

	if(pUCDValue == NULL) {
		return 0;
	};
	if(pUCDValue->Sum == 0) {
		return 0;
	};

	/* DOF */
	fprintf(fp, "%d", pUCDValue->Count);
	for(i=0; i<pUCDValue->Count; i++) {
		fprintf(fp, " %d", pUCDValue->DOF[i]);
	};
	fprintf(fp, "\n");

	/* Label and unit */
	for(i=0; i<pUCDValue->Count; i++) {
		fprintf(fp, "%s,%s,\n",
			pUCDValue->Label[i], pUCDValue->Unit[i]);
	};

	/* Value */
	for(i=0; i<pUCDValue->ItemCount; i++) {
		fprintf(fp, "%10d", pUCDValue->ID[i]);
		for(j=0; j<pUCDValue->Sum; j++) {
			fprintf(fp, "%15.7e",
				pUCDValue->Value[pUCDValue->Sum*i+j]);
		};
		fprintf(fp, "\n");
	};

	return 0;
};


/*=================*
 * All information *
 *=================*/
extern int
ppohVIS_BASE_PutUCD(
	char *cFileName, struct ppohVIS_BASE_stUCDData *pUCDData)
{
	FILE *fp = NULL;

	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "UCD data");
		goto error;
	};

	if((fp = fopen(cFileName, "w")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	/* number of nodes, elements and data */
	fprintf(fp, "%d %d %d %d %d\n",
			pUCDData->Node->Count, pUCDData->Element->Count,
			pUCDData->NodeValue->Sum, pUCDData->ElementValue->Sum, 0);

	/* node information */
	if(PutUCDNode(fp, pUCDData->Node)) goto error;

	/* element information */
	if(PutUCDElement(fp, pUCDData->Element)) goto error;

	/* value information */
	if(PutUCDValue(fp, pUCDData->NodeValue)) goto error;
	if(PutUCDValue(fp, pUCDData->ElementValue)) goto error;


	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	return 0;

error:
	return -1;
};


extern int
ppohVIS_BASE_PutUCD2(
	char *cFileName, struct ppohVIS_BASE_stUCDData *pUCDData)
{
	FILE *fp = NULL;

	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "UCD data");
		goto error;
	};

	if((fp = fopen(cFileName, "w")) == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileOpenError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	/* header part */
/*	fprintf(fp, "# unstructured formatted UCD data file\n"); */
	fprintf(fp, "1\n");
	fprintf(fp, "%s\n", ppohVIS_BASE_UCDCycleLabel[ppohVIS_BASE_UCDCycle_Data]);
	fprintf(fp, "step1\n");

	/* number of nodes and elements */
	fprintf(fp, "%d %d\n",
			pUCDData->Node->Count, pUCDData->Element->Count);

	/* node information */
	if(PutUCDNode(fp, pUCDData->Node)) goto error;

	/* element information */
	if(PutUCDElement(fp, pUCDData->Element)) goto error;

	/* value information */
	fprintf(fp, "%d %d\n",
			pUCDData->NodeValue->Sum,
			pUCDData->ElementValue->Sum);
	if(PutUCDValue(fp, pUCDData->NodeValue)) goto error;
	if(PutUCDValue(fp, pUCDData->ElementValue)) goto error;


	if(fclose(fp)) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_FileCloseError,
			"%s [%s]", strerror(errno), "UCD data file");
		goto error;
	};

	return 0;

error:
	return -1;
};


/*******************************************************************************
 * Print UCD data
 ******************************************************************************/
/*==================*
 * Node information *
 *==================*/
extern void
ppohVIS_BASE_PrintUCDNode(
	FILE *fp, struct ppohVIS_BASE_stUCDNode *pUCDNode)
{
	int iNode;

	if(pUCDNode == NULL) {
		return;
	};

	fprintf(fp, "Node=%10d\n", pUCDNode->Count);

	for(iNode=0; iNode<pUCDNode->Count; iNode++) {
		fprintf(fp, "%10d%15.7e%15.7e%15.7e\n",
			pUCDNode->ID[iNode],
			pUCDNode->Coords[3*iNode],
			pUCDNode->Coords[3*iNode+1],
			pUCDNode->Coords[3*iNode+2]);
	};

	return;
};


/*=====================*
 * Element information *
 *=====================*/
extern void
ppohVIS_BASE_PrintUCDElement(
	FILE *fp, struct ppohVIS_BASE_stUCDElement *pUCDElem)
{
	int iElem, iNode, iS, iE;

	if(pUCDElem == NULL) {
		return;
	};

	fprintf(fp, "Element=%10d\n", pUCDElem->Count);
	for(iElem=0; iElem<pUCDElem->Count; iElem++) {
		fprintf(fp, "%10d%10d%8s",
			pUCDElem->ID[iElem],
			pUCDElem->MaterialID[iElem],
			GetUCDLabel(pUCDElem->Type[iElem]));

		iS = pUCDElem->NodeIndex[iElem];
		iE = pUCDElem->NodeIndex[iElem+1];
		for(iNode=iS; iNode<iE; iNode++) {
			fprintf(fp, "%10d", pUCDElem->Node[iNode]);
		};
		fprintf(fp, "\n");
	};

	return;
};


/*===================*
 * Value information *
 *===================*/
extern void
ppohVIS_BASE_PrintUCDValue(
	FILE *fp, struct ppohVIS_BASE_stUCDValue *pUCDValue)
{
	int i, j;

	if(pUCDValue == NULL) {
		return;
	};

	fprintf(fp, "Value: Sum=%10d, Count=%10d\n",
		pUCDValue->Sum, pUCDValue->Count);
	if(pUCDValue->Sum <= 0) {
		return;
	};

	for(i=0; i<pUCDValue->Count; i++) {
		fprintf(fp, "%10d %s,%s\n",
			pUCDValue->DOF[i],
			pUCDValue->Label[i],
			pUCDValue->Unit[i]);
	};

	for(i=0; i<pUCDValue->ItemCount; i++) {
		fprintf(fp, "%10d", pUCDValue->ID[i]);
		for(j=0; j<pUCDValue->Sum; j++) {
			fprintf(fp, "%15.7e",
				pUCDValue->Value[pUCDValue->Sum*i+j]);
		};
		fprintf(fp, "\n");
	};

	return;
};


/*================*
 * All informaion *
 *================*/
extern void
ppohVIS_BASE_PrintUCD(
	FILE *fp, struct ppohVIS_BASE_stUCDData *pUCDData)
{
	if(pUCDData == NULL) {
		return;
	};

	ppohVIS_BASE_PrintUCDNode(fp, pUCDData->Node);
	ppohVIS_BASE_PrintUCDElement(fp, pUCDData->Element);
	ppohVIS_BASE_PrintUCDValue(fp, pUCDData->NodeValue);
	ppohVIS_BASE_PrintUCDValue(fp, pUCDData->ElementValue);

	return;
};

/*******************************************************************************
 * Set UCD data
 ******************************************************************************/
extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertMesh2UCD(
	struct ppohVIS_BASE_stMesh *pMesh)
{
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	int iNode, iElem, k;

	/* allocate */
	pUCDData = ppohVIS_BASE_AllocateUCD();
	if(pUCDData == NULL) {
		goto error;
	};
	pUCDData->Node = ppohVIS_BASE_AllocateUCDNode(pMesh->Node->Count);
	if(pUCDData->Node == NULL) {
		goto error;
	};
	pUCDData->Element = ppohVIS_BASE_AllocateUCDElement(pMesh->Element->Count);
	if(pUCDData->Element == NULL) {
		goto error;
	};
	pUCDData->NodeValue = ppohVIS_BASE_AllocateUCDNodeValue(pMesh->Node);
	if(pUCDData->NodeValue == NULL) {
		goto error;
	};
	pUCDData->ElementValue = ppohVIS_BASE_AllocateUCDElementValue(pMesh->Element);
	if(pUCDData->ElementValue == NULL) {
		goto error;
	};


	/* node information */
	for(iNode=0; iNode<pMesh->Node->Count; iNode++) {
		pUCDData->Node->ID[iNode] = pMesh->Node->ID[iNode];
		pUCDData->Node->Coords[3*iNode] = pMesh->Node->Coords[3*iNode];
		pUCDData->Node->Coords[3*iNode+1] = pMesh->Node->Coords[3*iNode+1];
		pUCDData->Node->Coords[3*iNode+2] = pMesh->Node->Coords[3*iNode+2];
	};

	/* element information */
	for(iElem=0; iElem<pMesh->Element->Count; iElem++) {
		pUCDData->Element->ID[iElem] = pMesh->Element->ID[iElem];
		pUCDData->Element->MaterialID[iElem] = 1;

		switch (pMesh->Element->Type[iElem]) {
		case (ppohVIS_BASE_Line2):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Line2;
			break;
		case (ppohVIS_BASE_Line3):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Line3;
			break;
		case (ppohVIS_BASE_Tria3):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tria3;
			break;
		case (ppohVIS_BASE_Tria6):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tria6;
			break;
		case (ppohVIS_BASE_Quad4):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Quad4;
			break;
		case (ppohVIS_BASE_Quad8):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Quad8;
			break;
		case (ppohVIS_BASE_Tetra4):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tetra4;
			break;
		case (ppohVIS_BASE_Tetra10):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tetra10;
			break;
		case (ppohVIS_BASE_Penta6):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Penta6;
			break;
		case (ppohVIS_BASE_Penta15):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Penta15;
			break;
		case (ppohVIS_BASE_Hexa8):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Hexa8;
			break;
		case (ppohVIS_BASE_Hexa20):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Hexa20;
			break;
		default:
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Invalid;
			break;
		};

		pUCDData->Element->NodeIndex[iElem+1] = pMesh->Element->NodeIndex[iElem+1];

		for(k=pMesh->Element->NodeIndex[iElem]; k<pMesh->Element->NodeIndex[iElem+1]; k++) {
			pUCDData->Element->Node[k] = pMesh->Element->Node[k];
		};
	};

	return pUCDData;

error:
	return NULL;
};


extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertMeshResult2UCD(
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stResult *pResult)
{
	struct ppohVIS_BASE_stUCDData *pUCDData = NULL;
	int iNode, iElem, i, j, k, nDOF;

	/* allocate */
	pUCDData = ppohVIS_BASE_AllocateUCD();
	if(pUCDData == NULL) {
		goto error;
	};
	pUCDData->Node = ppohVIS_BASE_AllocateUCDNode(pMesh->Node->Count);
	if(pUCDData->Node == NULL) {
		goto error;
	};
	pUCDData->Element = ppohVIS_BASE_AllocateUCDElement(pMesh->Element->Count);
	if(pUCDData->Element == NULL) {
		goto error;
	};
	pUCDData->NodeValue = ppohVIS_BASE_AllocateUCDValue(
		pResult->ItemCount, pResult->FreedomCount, 1);
	if(pUCDData->NodeValue == NULL) {
		goto error;
	};
	pUCDData->ElementValue = ppohVIS_BASE_AllocateUCDElementValue(pMesh->Element);
	if(pUCDData->ElementValue == NULL) {
		goto error;
	};

	nDOF = pResult->FreedomCount;

	/* node information */
	for(iNode=0; iNode<pMesh->Node->Count; iNode++) {
		pUCDData->Node->ID[iNode] = pMesh->Node->ID[iNode];
		pUCDData->Node->Coords[3*iNode] = pMesh->Node->Coords[3*iNode];
		pUCDData->Node->Coords[3*iNode+1] = pMesh->Node->Coords[3*iNode+1];
		pUCDData->Node->Coords[3*iNode+2] = pMesh->Node->Coords[3*iNode+2];
	};

	/* element information */
	for(iElem=0; iElem<pMesh->Element->Count; iElem++) {
		pUCDData->Element->ID[iElem] = pMesh->Element->ID[iElem];
		pUCDData->Element->MaterialID[iElem] = 1;

		switch (pMesh->Element->Type[iElem]) {
		case (ppohVIS_BASE_Line2):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Line2;
			break;
		case (ppohVIS_BASE_Line3):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Line3;
			break;
		case (ppohVIS_BASE_Tria3):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tria3;
			break;
		case (ppohVIS_BASE_Tria6):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tria6;
			break;
		case (ppohVIS_BASE_Quad4):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Quad4;
			break;
		case (ppohVIS_BASE_Quad8):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Quad8;
			break;
		case (ppohVIS_BASE_Tetra4):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tetra4;
			break;
		case (ppohVIS_BASE_Tetra10):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Tetra10;
			break;
		case (ppohVIS_BASE_Penta6):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Penta6;
			break;
		case (ppohVIS_BASE_Penta15):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Penta15;
			break;
		case (ppohVIS_BASE_Hexa8):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Hexa8;
			break;
		case (ppohVIS_BASE_Hexa20):
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Hexa20;
			break;
		default:
			pUCDData->Element->Type[iElem] = ppohVIS_BASE_UCDTopology_Invalid;
			break;
		};

		pUCDData->Element->NodeIndex[iElem+1] = pMesh->Element->NodeIndex[iElem+1];

		for(k=pMesh->Element->NodeIndex[iElem]; k<pMesh->Element->NodeIndex[iElem+1]; k++) {
			pUCDData->Element->Node[k] = pMesh->Element->Node[k];
		};
	};

	strcpy(pUCDData->NodeValue->Label[0], pResult->Label);
	strcpy(pUCDData->NodeValue->Unit[0], pResult->Label);
	pUCDData->NodeValue->DOF[0] = nDOF;
	for(i=0; i<pResult->ItemCount; i++) {
		pUCDData->NodeValue->ID[i] = i+1;
		for(j=0; j<nDOF; j++) {
			pUCDData->NodeValue->Value[nDOF*i+j] = pResult->Value[nDOF*i+j];
		};
	};

	return pUCDData;

error:
	return NULL;
};


extern int
ppohVIS_BASE_AddUCDValueFromResult(
	struct ppohVIS_BASE_stUCDData *pUCDData,
	struct ppohVIS_BASE_stResult *pResult)
{
	int iValueSumBak, iValueCountBak, iValueSumNew, iValueCountNew, iItemCount;
	struct ppohVIS_BASE_stUCDValue *pUCDValue;
	enum ppohVIS_BASE_eResultEntityType iType;
	int iPos, jPos, iRc, i, j;


	if(pUCDData == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "UCD data");
		goto error;
	};
	if(pResult == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_NullPointerFound,
			"%s", "result data");
		goto error;
	};

	iItemCount = pResult->ItemCount;
	iType = pResult->EntityType;
	if(iType == ppohVIS_BASE_ResultNode) {
		if(pUCDData->NodeValue == NULL) {
			iValueSumBak = 0;
			iValueCountBak = 0;
			iValueSumNew = pResult->FreedomCount;
			iValueCountNew = 1;
			pUCDData->NodeValue =
				ppohVIS_BASE_AllocateUCDValue(
					iItemCount, iValueSumNew,
					iValueCountNew);
			if(pUCDData->NodeValue == NULL) goto error;
		} else {
			iValueSumBak = pUCDData->NodeValue->Sum;
			iValueCountBak = pUCDData->NodeValue->Count;
			iValueSumNew = iValueSumBak + pResult->FreedomCount;
			iValueCountNew = iValueCountBak + 1;
			iRc = ppohVIS_BASE_ReallocateUCDValue(
				pUCDData->NodeValue,
				iValueSumNew, iValueCountNew);
			if(iRc != 0) goto error;
		};
		pUCDValue = pUCDData->NodeValue;

	} else if(iType == ppohVIS_BASE_ResultElement) {
		if(pUCDData->ElementValue == NULL) {
			iValueSumBak = 0;
			iValueCountBak = 0;
			iValueSumNew = pResult->FreedomCount;
			iValueCountNew = 1;
			pUCDData->ElementValue =
				ppohVIS_BASE_AllocateUCDValue(
					iItemCount, iValueSumNew,
					iValueCountNew);
			if(pUCDData->ElementValue == NULL) goto error;
		} else {
			iValueSumBak = pUCDData->ElementValue->Sum;
			iValueCountBak = pUCDData->ElementValue->Count;
			iValueSumNew = iValueSumBak + pResult->FreedomCount;
			iValueCountNew = iValueCountBak + 1;
			iRc = ppohVIS_BASE_ReallocateUCDValue(
				pUCDData->ElementValue,
				iValueSumNew, iValueCountNew);
			if(iRc != 0) goto error;
		};
		pUCDValue = pUCDData->ElementValue;

	} else {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_InvalidData,
			"%s [%d]", "entity type", iType);
	};

	pUCDValue->DOF[iValueCountNew-1] = pResult->FreedomCount;
	strncpy(pUCDValue->Label[iValueCountNew-1], pResult->Label,
		PPOHVIS_BASE_UCD_DATALABEL_LEN);

	for(i=0; i<iItemCount; i++) {
		for(j=iValueSumBak; j<iValueSumNew; j++) {
			iPos = iValueSumNew*i+j;
			jPos = pResult->FreedomCount*i+j-iValueSumBak;
			pUCDValue->Value[iPos] = pResult->Value[jPos];
		};
	};

	return 0;

error:
	return -1;
};

