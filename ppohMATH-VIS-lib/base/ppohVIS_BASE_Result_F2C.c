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

#include "ppohVIS_BASE_Result.h"
#include "ppohVIS_BASE_Result_F2C.h"

static struct ppohVIS_BASE_stResultCollection *ptrResultNode;
static struct ppohVIS_BASE_stResultCollection *ptrResultElem;


/*******************************************************************************
 * initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_ResultF2CInit(
	enum ppohVIS_BASE_eResultEntityType EntityType,
	struct ppohVIS_BASE_stResultCollection *pResult)
{
	if(EntityType == ppohVIS_BASE_ResultElement) {
		ptrResultElem = pResult;
	} else {   /* ppohVIS_BASE_ResultNode */
		ptrResultNode = pResult;
	};

	return 0;
};


/*******************************************************************************
 * main procedure
 ******************************************************************************/
extern int
ppohVIS_BASE_ResultF2CSetListCount(
	enum ppohVIS_BASE_eResultEntityType EntityType,
	int *ListCount)
{
	struct ppohVIS_BASE_stResultCollection *pResult;

	if(EntityType == ppohVIS_BASE_ResultElement) {
		pResult = ptrResultElem;
	} else {   /* ppohVIS_BASE_ResultNode */
		pResult = ptrResultNode;
	};

	if(ppohVIS_BASE_InitResultCollection(pResult, *ListCount)) {
		return 1;
	};

	return 0;
};


/*============================================================================*/
extern int
ppohVIS_BASE_ResultF2CSetResult(
	enum ppohVIS_BASE_eResultEntityType EntityType, int iIndex,
	int *ItemCount, int *FreedomCount, char *Label, double *Value)
{
	struct ppohVIS_BASE_stResultCollection *pResult;
	size_t BufSize;

	if(EntityType == ppohVIS_BASE_ResultElement) {
		pResult = ptrResultElem;
	} else {   /* ppohVIS_BASE_ResultNode */
		pResult = ptrResultNode;
	};

	pResult->Results[iIndex] = ppohVIS_BASE_AllocateResult();
	if(pResult->Results[iIndex] == NULL) {
		return 1;
	};

	if(ppohVIS_BASE_InitResult(pResult->Results[iIndex], *ItemCount,
	                            EntityType, *FreedomCount)) {
		return 1;
	};

	strncpy(pResult->Results[iIndex]->Label, Label, PPOHVIS_BASE_LABEL_LEN);

	BufSize = sizeof(double) * (*ItemCount) * (*FreedomCount);
	memcpy(pResult->Results[iIndex]->Value, Value, BufSize);

	return 0;
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern int
ppohVIS_BASE_ResultF2CFinalize(
	enum ppohVIS_BASE_eResultEntityType EntityType)
{
	if(EntityType == ppohVIS_BASE_ResultElement) {
		ptrResultElem = NULL;
	} else {   /* ppohVIS_BASE_ResultNode */
		ptrResultNode = NULL;
	};

	return 0;
};
