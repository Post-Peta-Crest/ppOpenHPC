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
#include <math.h>
#include <errno.h>

#include "ppohVIS_BASE_mpi.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_BoundingBox.h"
#include "ppohVIS_BASE_ParallelBoundingBox.h"


/*******************************************************************************
 * Bounding box for global region
 ******************************************************************************/
extern struct ppohVIS_BASE_stBoundingBox *
ppohVIS_BASE_GetGlobalBoundingBox(
	struct ppohVIS_BASE_stBoundingBox *pBBoxLocal,
	ppohVIS_BASE_Comm comm)
{
	struct ppohVIS_BASE_stBoundingBox *pBBoxGlobal = NULL;
	int iRc;

	pBBoxGlobal = ppohVIS_BASE_AllocateBoundingBox();
	if(pBBoxGlobal == NULL) {
		return NULL;
	};

#ifndef SERIAL
	iRc = MPI_Allreduce(&pBBoxLocal->XMin, &pBBoxGlobal->XMin, 1,
	                    MPI_DOUBLE, MPI_MIN, comm);
	if(iRc != MPI_SUCCESS) return NULL;

	iRc = MPI_Allreduce(&pBBoxLocal->YMin, &pBBoxGlobal->YMin, 1,
	                    MPI_DOUBLE, MPI_MIN, comm);
	if(iRc != MPI_SUCCESS) return NULL;

	iRc = MPI_Allreduce(&pBBoxLocal->ZMin, &pBBoxGlobal->ZMin, 1,
	                    MPI_DOUBLE, MPI_MIN, comm);
	if(iRc != MPI_SUCCESS) return NULL;

	iRc = MPI_Allreduce(&pBBoxLocal->XMax, &pBBoxGlobal->XMax, 1,
	                    MPI_DOUBLE, MPI_MAX, comm);
	if(iRc != MPI_SUCCESS) return NULL;

	iRc = MPI_Allreduce(&pBBoxLocal->YMax, &pBBoxGlobal->YMax, 1,
	                    MPI_DOUBLE, MPI_MAX, comm);
	if(iRc != MPI_SUCCESS) return NULL;

	iRc = MPI_Allreduce(&pBBoxLocal->ZMax, &pBBoxGlobal->ZMax, 1,
	                    MPI_DOUBLE, MPI_MAX, comm);
	if(iRc != MPI_SUCCESS) return NULL;
#else
	pBBoxGlobal->XMin = pBBoxLocal->XMin;
	pBBoxGlobal->YMin = pBBoxLocal->YMin;
	pBBoxGlobal->ZMin = pBBoxLocal->ZMin;
	pBBoxGlobal->XMax = pBBoxLocal->XMax;
	pBBoxGlobal->YMax = pBBoxLocal->YMax;
	pBBoxGlobal->ZMax = pBBoxLocal->ZMax;
#endif

	return pBBoxGlobal;
};

