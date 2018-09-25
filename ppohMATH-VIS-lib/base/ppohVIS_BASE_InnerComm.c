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

#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_InnerComm.h"


extern struct ppohVIS_BASE_stInnerComm *
ppohVIS_BASE_AllocateInnerComm(int nProc, int nItem)
{
	struct ppohVIS_BASE_stInnerComm *pComm = NULL;

	pComm = (struct ppohVIS_BASE_stInnerComm *)malloc(sizeof(struct ppohVIS_BASE_stInnerComm));
	pComm->ProcCount = nProc;
	pComm->ItemCount = nItem;

	pComm->NodeRank = (int *)calloc(nItem, sizeof(int));
	if(pComm->NodeRank == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "node rank");
		goto error;
	};

	pComm->NodeCount = (int *)calloc(nProc, sizeof(int));
	if(pComm->NodeCount == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "nodes on procs");
		goto error;
	};

	pComm->NodeIndex = (int *)calloc(nProc+1, sizeof(int));
	if(pComm->NodeIndex == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "index of inner-comm");
		goto error;
	};
	
	pComm->NodeItem = (int *)calloc(nItem, sizeof(int));
	if(pComm->NodeItem == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "inner-comm item");
		goto error;
	};

	return pComm;

error:
	return NULL;
};


extern void
ppohVIS_BASE_FreeInnerComm(struct ppohVIS_BASE_stInnerComm *pComm)
{
	if(pComm == NULL) {
		return;
	};

	free(pComm->NodeRank);
	free(pComm->NodeCount);
	free(pComm->NodeIndex);
	free(pComm->NodeItem);

	return;
};
