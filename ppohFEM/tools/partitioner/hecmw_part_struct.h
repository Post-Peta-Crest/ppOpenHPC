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


#ifndef INC_HECMW_PART_STRUCT
#define INC_HECMW_PART_STRUCT

#include "hecmw_part_define.h"


struct hecmw_part_edge_data {

    long long int n_edge;

    int *edge_node_item;
};


struct hecmw_part_node_data {

    int *node_elem_index;

    int *node_elem_item;
};


struct hecmw_part_cont_data {

    int n_domain;

    int depth;

    int type;

    int method;

    int n_rcb_div;

    int *rcb_axis;

    int is_print_ucd;

    char ucd_file_name[HECMW_FILENAME_LEN+1]; /* ucd file name */

    int n_my_domain;

    int *my_domain;

    int contact;
};

#endif  /* INC_HECMW_PART_STRUCT */
