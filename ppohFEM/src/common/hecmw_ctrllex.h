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


#ifndef HECMW_CTRLLEX_INCLUDED
#define HECMW_CTRLLEX_INCLUDED

#include <stdio.h>

enum {
	HECMW_CTRLLEX_NL = 1000,
	HECMW_CTRLLEX_INT,
	HECMW_CTRLLEX_DOUBLE,
	HECMW_CTRLLEX_NAME,
	HECMW_CTRLLEX_FILENAME,

	HECMW_CTRLLEX_H_CONTROL = 2000,
	HECMW_CTRLLEX_H_MESH,
	HECMW_CTRLLEX_H_MESH_GROUP,
	HECMW_CTRLLEX_H_RESULT,
	HECMW_CTRLLEX_H_RESTART,
	HECMW_CTRLLEX_H_SUBDIR,

	HECMW_CTRLLEX_K_ABAQUS = 3000,
	HECMW_CTRLLEX_K_DIR,
	HECMW_CTRLLEX_K_FEMAP,
	HECMW_CTRLLEX_K_GEOFEM,
	HECMW_CTRLLEX_K_HECMW_DIST,
	HECMW_CTRLLEX_K_HECMW_ENTIRE,
	HECMW_CTRLLEX_K_IN,
	HECMW_CTRLLEX_K_INOUT,
	HECMW_CTRLLEX_K_IO,
	HECMW_CTRLLEX_K_LIMIT,
	HECMW_CTRLLEX_K_NAME,
	HECMW_CTRLLEX_K_NASTRAN,
	HECMW_CTRLLEX_K_ON,
	HECMW_CTRLLEX_K_OFF,
	HECMW_CTRLLEX_K_OUT,
	HECMW_CTRLLEX_K_REFINE,
	HECMW_CTRLLEX_K_TYPE,
};

extern double HECMW_ctrllex_get_number(void);

extern char *HECMW_ctrllex_get_text(void);

extern int HECMW_ctrllex_get_lineno(void);

extern int HECMW_ctrllex_next_token(void);

extern int HECMW_ctrllex_next_token_skip(int skip_token);

extern int HECMW_ctrllex_set_input(FILE *fp);

extern int HECMW_ctrllex_skip_line(void);

extern int HECMW_ctrllex_unput_token(void);

#endif
