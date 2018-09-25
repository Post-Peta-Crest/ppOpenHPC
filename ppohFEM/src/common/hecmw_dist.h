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




#ifndef HECMW_DIST_INCLUDED
#define HECMW_DIST_INCLUDED

#include "hecmw_struct.h"


extern int HECMW_dist_get_mat_id(const struct hecmwST_material *mat, const char *name);


extern int HECMW_dist_get_ngrp_id(const struct hecmwST_node_grp *ngrp, const char *name);


extern int HECMW_dist_get_egrp_id(const struct hecmwST_elem_grp *egrp, const char *name);


extern int HECMW_dist_get_sgrp_id(const struct hecmwST_surf_grp *sgrp, const char *name);


extern int HECMW_dist_gid2lid_node(const struct hecmwST_local_mesh *mesh, int gid);


extern int HECMW_dist_gid2lid_elem(const struct hecmwST_local_mesh *mesh, int gid);

#endif
