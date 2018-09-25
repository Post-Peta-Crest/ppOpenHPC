/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohFEM                                           *
 *         Version : 1.0                                               *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppohFEM.                                   *
 *     ppohFEM is a free software, you can use it under the terms      *
 *     of The MIT License (MIT). See LICENSE file and User's guide     *
 *     for more details.                                               *
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
 *       - Interfaculty Initiative in Information Studies              *
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
 *   Copyright (c) 2015 The University of Tokyo                        *
 *                       - Graduate School of Frontier Science         *
 *                                                                     *
 *=====================================================================*/



#include "hecmw_time.h"


double HECMW_Wtime(void)
{
#ifndef HECMW_SERIAL
	double t;
	t = MPI_Wtime();
	return t;
#else
	struct timeb t;
	double sec;
	ftime(&t);
	sec = t.time + (double)t.millitm * 1e-3;
	return sec;
#endif
}

double HECMW_Wtick(void)
{
#ifndef HECMW_SERIAL
	return MPI_Wtick();
#else
	return 1e-3;
#endif
}

/* interface for fortran */


double hecmw_wtime_fi(void) { return HECMW_Wtime(); }
double hecmw_wtime_fi_(void) { return HECMW_Wtime(); }
double hecmw_wtime_fi__(void) { return HECMW_Wtime(); }
double HECMW_WTIME_FI(void) { return HECMW_Wtime(); }
double HECMW_WTIME_FI_(void) { return HECMW_Wtime(); }
double HECMW_WTIME_FI__(void) { return HECMW_Wtime(); }


double hecmw_wtick_fi(void) { return HECMW_Wtick(); }
double hecmw_wtick_fi_(void) { return HECMW_Wtick(); }
double hecmw_wtick_fi__(void) { return HECMW_Wtick(); }
double HECMW_WTICK_FI(void) { return HECMW_Wtick(); }
double HECMW_WTICK_FI_(void) { return HECMW_Wtick(); }
double HECMW_WTICK_FI__(void) { return HECMW_Wtick(); }




