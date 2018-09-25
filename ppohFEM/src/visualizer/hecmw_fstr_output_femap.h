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

#ifndef HECMW_FSTR_OUTPUT_FEMAP_H_INCLUDED
#define HECMW_FSTR_OUTPUT_FEMAP_H_INCLUDED

#include "hecmw_struct.h"
#include "hecmw_result.h"
#include "hecmw_util.h"

void
HECMW_fstr_output_femap (struct hecmwST_local_mesh *mesh,
		struct hecmwST_result_data *data, char *outfile,
		HECMW_Comm VIS_COMM);
void
HECMW_avs_output (struct hecmwST_local_mesh *mesh,
		struct hecmwST_result_data *data, char *outfile,
		HECMW_Comm VIS_COMM);
void
HECMW_reorder_avs_output (struct hecmwST_local_mesh *mesh,
		struct hecmwST_result_data *data, char *outfile,
		HECMW_Comm VIS_COMM);
void
HECMW_microavs_output (struct hecmwST_local_mesh *mesh,
		struct hecmwST_result_data *data, char *outfile,
		HECMW_Comm VIS_COMM);
void
HECMW_bin_avs_output (struct hecmwST_local_mesh *mesh,
		struct hecmwST_result_data *data, char *outfile,
		HECMW_Comm VIS_COMM);

void
HECMW_separate_avs_output (struct hecmwST_local_mesh *mesh,
		struct hecmwST_result_data *data, char *outfile);

#endif /* HECMW_FSTR_OUTPUT_FEMAP_H_INCLUDED */
