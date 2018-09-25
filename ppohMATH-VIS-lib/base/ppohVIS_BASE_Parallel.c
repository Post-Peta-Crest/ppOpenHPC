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
#include <omp.h>

#include "ppohVIS_BASE_mpi.h"
#include "ppohVIS_BASE_Parallel.h"


static int iProcCount;
static int iMyRank;
static ppohVIS_BASE_Comm iComm;
static int iThreadCount;


/*******************************************************************************
 * Initialize parallel information
 ******************************************************************************/
extern void
ppohVIS_BASE_InitParallel(ppohVIS_BASE_Comm comm)
{
	ppohVIS_BASE_Comm_size(comm, &iProcCount);
	ppohVIS_BASE_Comm_rank(comm, &iMyRank);
	iComm = comm;

#pragma omp parallel
	{
	  /*
		iThreadCount = omp_get_num_threads();
	  */
	};
};


/*******************************************************************************
 * Set number of processes
 ******************************************************************************/
extern void
ppohVIS_BASE_SetCommSize(int psize)
{
	iProcCount = psize;
};


/*******************************************************************************
 * Set process rank
 ******************************************************************************/
extern void
ppohVIS_BASE_SetCommRank(int rank)
{
	iMyRank = rank;
};


/*******************************************************************************
 * Set MPI communicator
 ******************************************************************************/
extern void
ppohVIS_BASE_SetCommunicator(ppohVIS_BASE_Comm comm)
{
	iComm = comm;
};


/*******************************************************************************
 * Set number of threads
 ******************************************************************************/
extern void
ppohVIS_BASE_SetThreadSize(int isize)
{
	iThreadCount = isize;
};


/*******************************************************************************
 * Get number of processes
 ******************************************************************************/
extern int
ppohVIS_BASE_GetCommSize(void)
{
	return iProcCount;
};


/*******************************************************************************
 * Get process rank
 ******************************************************************************/
extern int
ppohVIS_BASE_GetCommRank(void)
{
	return iMyRank;
};


/*******************************************************************************
 * Get MPI communicator
 ******************************************************************************/
extern ppohVIS_BASE_Comm
ppohVIS_BASE_GetCommunicator(void)
{
	return iComm;
};


/*******************************************************************************
 * Get number of threads
 ******************************************************************************/
extern int
ppohVIS_BASE_GetThreadSize(void)
{
	return iThreadCount;
};


/*******************************************************************************
 * Wrapper function of 'MPI_Comm_rank'
 ******************************************************************************/
extern int
ppohVIS_BASE_Comm_size(ppohVIS_BASE_Comm comm, int *psize)
{
	int iRc;

#ifndef SERIAL
	iRc = MPI_Comm_size(comm, psize);
#else
	*psize = 1;
	iRc = 0;
#endif

	return iRc;
};


/*******************************************************************************
 * Wrapper function of 'MPI_Comm_rank'
 ******************************************************************************/
extern int
ppohVIS_BASE_Comm_rank(ppohVIS_BASE_Comm comm, int *rank)
{
	int iRc;

#ifndef SERIAL
	iRc = MPI_Comm_rank(comm, rank);
#else
	*rank = 0;
	iRc = 0;
#endif

	return iRc;
};


/*******************************************************************************
 * Wrapper function of 'MPI_Barrier'
 ******************************************************************************/
extern void
ppohVIS_BASE_Barrier(ppohVIS_BASE_Comm comm)
{
#ifndef SERIAL
	MPI_Barrier(comm);
#endif
};


/*******************************************************************************
 * Wrapper function of 'MPI_Comm_f2c'
 ******************************************************************************/
extern ppohVIS_BASE_Comm
ppohVIS_BASE_Comm_f2c(int fComm)
{
	ppohVIS_BASE_Comm cComm;

#ifdef MPI_NOT_HAS_COMM_TRANSLATOR
	cComm = (ppohVIS_BASE_Comm)fComm;
#else
	cComm = MPI_Comm_f2c((MPI_Fint)fComm);
#endif

	return cComm;
};

