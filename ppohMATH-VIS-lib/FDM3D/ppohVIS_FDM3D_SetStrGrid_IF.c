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

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_F2CUtil.h"
#include "ppohVIS_FDM3D_StrGrid.h"
#include "ppohVIS_FDM3D_StrGrid_F2C.h"
#include "ppohVIS_FDM3D_Extern.h"


static struct ppohVIS_FDM3D_stStrGrid *ptrGrid;

/*******************************************************************************
 * initialize
 ******************************************************************************/
extern void
ppohvis_fdm3d_set_strgrid_init_if(int *iErr)
{
	size_t BufSize;
	char *cErrMsg;

	BufSize = sizeof(struct ppohVIS_FDM3D_stStrGrid);
	ptrGrid = (struct ppohVIS_FDM3D_stStrGrid *)malloc(BufSize);
	if(ptrGrid == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError, "%s [%s]",
			strerror(errno), "stracture grid info");
		goto Error;
	};

	if(ppohVIS_FDM3D_StrGridF2CInit(ptrGrid)) {
		goto Error;
	};

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_fdm3d_set_strgrid_init_if_(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_init_if(iErr);
};

extern void
ppohvis_fdm3d_set_strgrid_init_if__(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_init_if(iErr);
};

extern void
PARAVIS_FDM3D_SET_STRGRID_INIT_IF(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_init_if(iErr);
};


/*******************************************************************************
 * main procedure
 ******************************************************************************/
extern void
ppohvis_fdm3d_set_strgrid_set_strgrid_if(
                   int *NumX, int *NumY, int *NumZ,
                   double *DeltaX, double *DeltaY, double *DeltaZ,
                   double *OriginX, double *OriginY, double *OriginZ, int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_FDM3D_StrGridF2CSetStrGrid(NumX, NumY, NumZ,
	                                      DeltaX, DeltaY, DeltaZ,
	                                      OriginX, OriginY, OriginZ)) {
		goto Error;
	};

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_fdm3d_set_strgrid_set_strgrid_if_(
                   int *NumX, int *NumY, int *NumZ,
                   double *DeltaX, double *DeltaY, double *DeltaZ,
                   double *OriginX, double *OriginY, double *OriginZ, int *iErr)
{
	ppohvis_fdm3d_set_strgrid_set_strgrid_if(NumX, NumY, NumZ,
	                                   DeltaX, DeltaY, DeltaZ,
	                                   OriginX, OriginY, OriginZ, iErr);
};

extern void
ppohvis_fdm3d_set_strgrid_set_strgrid_if__(
                   int *NumX, int *NumY, int *NumZ,
                   double *DeltaX, double *DeltaY, double *DeltaZ,
                   double *OriginX, double *OriginY, double *OriginZ, int *iErr)
{
	ppohvis_fdm3d_set_strgrid_set_strgrid_if(NumX, NumY, NumZ,
	                                   DeltaX, DeltaY, DeltaZ,
	                                   OriginX, OriginY, OriginZ, iErr);
};

extern void
PARAVIS_FDM3D_SET_STRGRID_SET_STRGRID_IF(
                   int *NumX, int *NumY, int *NumZ,
                   double *DeltaX, double *DeltaY, double *DeltaZ,
                   double *OriginX, double *OriginY, double *OriginZ, int *iErr)
{
	ppohvis_fdm3d_set_strgrid_set_strgrid_if(NumX, NumY, NumZ,
	                                   DeltaX, DeltaY, DeltaZ,
	                                   OriginX, OriginY, OriginZ, iErr);
};


/*============================================================================*/
extern void
ppohvis_fdm3d_set_strgrid_if(int *iErr)
{
	char *cErrMsg;
	int iRc;

	/* c function */
	iRc = ppohVIS_FDM3D_SetStrGrid(ptrGrid);
	if(iRc) {
		goto Error;
	};

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_fdm3d_set_strgrid_if_(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_if(iErr);
};

extern void
ppohvis_fdm3d_set_strgrid_if__(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_if(iErr);
};

extern void
PARAVIS_FDM3D_SET_STRGRID_IF(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_if(iErr);
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern void
ppohvis_fdm3d_set_strgrid_finalize_if(int *iErr)
{
	char *cErrMsg;

	if(ppohVIS_FDM3D_StrGridF2CFinalize()) {
		goto Error;
	};

	free(ptrGrid);
	ptrGrid = NULL;

	*iErr = 0;
	return;

Error:
	ppohVIS_BASE_GetError(&cErrMsg);
	ppohVIS_BASE_DebugPrint(cErrMsg);
	*iErr = 1;
	return;
};

extern void
ppohvis_fdm3d_set_strgrid_finalize_if_(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_finalize_if(iErr);
};

extern void
ppohvis_fdm3d_set_strgrid_finalize_if__(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_finalize_if(iErr);
};

extern void
PARAVIS_FDM3D_SET_STRGRID_FINALIZE_IF(int *iErr)
{
	ppohvis_fdm3d_set_strgrid_finalize_if(iErr);
};
