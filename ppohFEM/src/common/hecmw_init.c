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




#include <stdio.h>
#include "hecmw_util.h"
#include "hecmw_init.h"
/* #include "hecmw_couple_info.h"  2007/12/27 S.Ito   */


int
HECMW_init_ex(int *argc, char ***argv, const char *ctrlfile)
{
	if(HECMW_comm_init(argc, argv)) return -1;
	HECMW_log(HECMW_LOG_DEBUG, "Initilalizing...");
	if(ctrlfile == NULL) ctrlfile = HECMW_CTRL_FILE;
	if(HECMW_ctrl_init_ex(ctrlfile)) return -1;
/*     if(HECMW_couple_comm_init() != HECMW_SUCCESS) return -1;  2007/12/27 S.Ito */
	return 0;
}



int
HECMW_init(int *argc, char ***argv)
{
	return HECMW_init_ex(argc, argv, HECMW_CTRL_FILE);
}

