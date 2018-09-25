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
#include "ppohVIS_BASE_mpi.h"
#include "ppohVIS_BASE_Parallel.h"
#include "ppohVIS_BASE_F2CUtil.h"
#include "ppohVIS_FDM3D_Extern.h"


/*******************************************************************************
 * initialize
 ******************************************************************************/
extern void
ppohvis_fdm3d_init_init_if(int *iErr)
{
	*iErr = 0;
	return;
};

extern void
ppohvis_fdm3d_init_init_if_(int *iErr)
{
	ppohvis_fdm3d_init_init_if(iErr);
};

extern void
ppohvis_fdm3d_init_init_if__(int *iErr)
{
	ppohvis_fdm3d_init_init_if(iErr);
};

extern void
PPOHVIS_FDM3D_INIT_INIT_IF(int *iErr)
{
	ppohvis_fdm3d_init_init_if(iErr);
};


/*******************************************************************************
 * main procedure
 ******************************************************************************/


/*============================================================================*/
extern void
ppohvis_fdm3d_init_if(int *fComm, int *iErr)
{
	ppohVIS_BASE_Comm cComm;
	char *cErrMsg;

	cComm = ppohVIS_BASE_Comm_f2c(*fComm);

	if(ppohVIS_FDM3D_Init(cComm)) {
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
ppohvis_fdm3d_init_if_(int *fComm, int *iErr)
{
	ppohvis_fdm3d_init_if(fComm, iErr);
};

extern void
ppohvis_fdm3d_init_if__(int *fComm, int *iErr)
{
	ppohvis_fdm3d_init_if(fComm, iErr);
};

extern void
PPOHVIS_FDM3D_INIT_IF(int *fComm, int *iErr)
{
	ppohvis_fdm3d_init_if(fComm, iErr);
};


/*******************************************************************************
 * finalize
 ******************************************************************************/
extern void
ppohvis_fdm3d_init_finalize_if(int *iErr)
{
	*iErr = 0;
	return;
};

extern void
ppohvis_fdm3d_init_finalize_if_(int *iErr)
{
	ppohvis_fdm3d_init_finalize_if(iErr);
};

extern void
ppohvis_fdm3d_init_finalize_if__(int *iErr)
{
	ppohvis_fdm3d_init_finalize_if(iErr);
};

extern void
PPOHVIS_FDM3D_INIT_FINALIZE_IF(int *iErr)
{
	ppohvis_fdm3d_init_finalize_if(iErr);
};
