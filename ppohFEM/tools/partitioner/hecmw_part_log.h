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



#ifndef INC_HECMW_PART_LOG
#define INC_HECMW_PART_LOG

extern int
HECMW_part_init_log( int _n_domain );

extern int
HECMW_part_set_log_part_type( int _part_type );
extern int
HECMW_part_set_log_part_method( int _part_method );
extern int
HECMW_part_set_log_part_depth( int _depth );
extern int
HECMW_part_set_log_n_edgecut( long long int _n_edge, int _n_edgecut );
extern int
HECMW_part_set_log_n_node_g( int _n_node_g );
extern int
HECMW_part_set_log_n_elem_g( int _n_elem_g );
extern int
HECMW_part_set_log_n_node( int domain, int _n_node );
extern int
HECMW_part_set_log_n_elem( int domain, int _n_elem );
extern int
HECMW_part_set_log_nn_internal( int domain, int _nn_internal );
extern int
HECMW_part_set_log_ne_internal( int domain, int _ne_internal );

extern int
HECMW_part_print_log( void );

extern void
HECMW_part_finalize_log( void );

#endif  /* INC_HECMW_PART_LOG */
