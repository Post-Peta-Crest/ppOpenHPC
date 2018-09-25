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

#include "ppohVIS_BASE_Control.h"
#include "ppohVIS_BASE_Control_C2F.h"

static struct ppohVIS_BASE_stControl *ptrControl;


/*******************************************************************************
 * initialize
 ******************************************************************************/
extern int
ppohVIS_BASE_ControlC2FInit(
	struct ppohVIS_BASE_stControl *pControl)
{
	ptrControl = pControl;

	return 0;
};


/*******************************************************************************
 * get data
 ******************************************************************************/
extern int
ppohVIS_BASE_ControlC2FGetRefineControl(
	double *AvailableMemory, int *MaxRefineLevel, int *MaxVoxelCount)
{
	size_t BufSize;

	BufSize = sizeof(double);
	memcpy(AvailableMemory, &ptrControl->Refine.AvailableMemory, BufSize);

	BufSize = sizeof(int);
	memcpy(MaxRefineLevel, &ptrControl->Refine.MaxRefineLevel, BufSize);
	memcpy(MaxVoxelCount, &ptrControl->Refine.MaxVoxelCount, BufSize);

	return 0;
};


extern int
ppohVIS_BASE_ControlC2FGetSimpleControl(
	double *ReductionRate)
{
	size_t BufSize;

	BufSize = sizeof(double);
	memcpy(ReductionRate, &ptrControl->Simple.ReductionRate, BufSize);

	return 0;
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern int
ppohVIS_BASE_ControlC2FFinalize(void)
{
	ptrControl = NULL;

	return 0;
};
