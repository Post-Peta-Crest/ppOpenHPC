/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_FDM3D                                     *
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

#include "ppohVIS_FDM3D_StrGrid.h"
#include "ppohVIS_FDM3D_StrGrid_F2C.h"

static struct ppohVIS_FDM3D_stStrGrid *ptrGrid;


/*******************************************************************************
 * initialize
 ******************************************************************************/
extern int
ppohVIS_FDM3D_StrGridF2CInit(
	struct ppohVIS_FDM3D_stStrGrid *pGrid)
{
	ptrGrid = pGrid;

	return 0;
};


/*******************************************************************************
 * main procedure
 ******************************************************************************/
extern int
ppohVIS_FDM3D_StrGridF2CSetStrGrid(
	int *NumX, int *NumY, int *NumZ,
	double *DeltaX, double *DeltaY, double *DeltaZ,
	double *OriginX, double *OriginY, double *OriginZ)
{
	ptrGrid->NumX = *NumX;
	ptrGrid->NumY = *NumY;
	ptrGrid->NumZ = *NumZ;

	ptrGrid->DeltaX = *DeltaX;
	ptrGrid->DeltaY = *DeltaY;
	ptrGrid->DeltaZ = *DeltaZ;

	ptrGrid->OriginX = *OriginX;
	ptrGrid->OriginY = *OriginY;
	ptrGrid->OriginZ = *OriginZ;

	return 0;
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern int
ppohVIS_FDM3D_StrGridF2CFinalize(void)
{
	ptrGrid = NULL;

	return 0;
};
