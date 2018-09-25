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




#ifndef HECMW_DIST_PRINT_INCLUDED
#define HECMW_DIST_PRINT_INCLUDED

#include <stdio.h>
#include "hecmw_struct.h"


extern void HECMW_dist_print_flags(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_header(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_gridfile(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_files(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_zero_temp(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_node(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_elem(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_pe(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_adapt(const struct hecmwST_local_mesh *mesh, FILE *fp);


extern void HECMW_dist_print_section(const struct hecmwST_section *sect, FILE *fp);


extern void HECMW_dist_print_material(const struct hecmwST_material *material, FILE *fp);


extern void HECMW_dist_print_mpc(const struct hecmwST_mpc *mpc, FILE *fp);


extern void HECMW_dist_print_amp(const struct hecmwST_amplitude *amp, FILE *fp);


extern void HECMW_dist_print_ngrp(const struct hecmwST_node_grp *ngrp, FILE *fp);


extern void HECMW_dist_print_egrp(const struct hecmwST_elem_grp *egrp, FILE *fp);


extern void HECMW_dist_print_sgrp(const struct hecmwST_surf_grp *sgrp, FILE *fp);


extern void HECMW_dist_print(const struct hecmwST_local_mesh *mesh, FILE *fp);

#endif

