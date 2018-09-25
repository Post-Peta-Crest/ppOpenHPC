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



#ifndef INC_HECMW_MESH_HASH_SORT
#define INC_HECMW_MESH_HASH_SORT

/* edge */
extern int
HECMW_mesh_hsort_edge_init( int n_node, int n_elem );
extern int
HECMW_mesh_hsort_edge_realloc( void );
extern void
HECMW_mesh_hsort_edge_final( void );
extern long long int
HECMW_mesh_hsort_edge_get_n( void );
extern int
*HECMW_mesh_hsort_edge_get_v( void );
extern long long int
HECMW_mesh_hsort_edge( int node1, int node2 );

/* triangular surface */
extern int
HECMW_mesh_hsort_tsuf_init( int n_node, int n_elem );
extern int
HECMW_mesh_hsort_tsuf_realloc( void );
extern void
HECMW_mesh_hsort_tsuf_final( void );
extern int
HECMW_mesh_hsort_tsuf_get_n( void );
extern int
*HECMW_mesh_hsort_tsuf_get_v( void );
extern int
HECMW_mesh_hsort_tsuf( int node1, int node2, int node3 );

/* quadrilateral surface */
extern int
HECMW_mesh_hsort_qsuf_init( int n_node, int n_elem );
extern int
HECMW_mesh_hsort_qsuf_realloc( void );
extern void
HECMW_mesh_hsort_qsuf_final( void );
extern int
HECMW_mesh_hsort_qsuf_get_n( void );
extern int
*HECMW_mesh_hsort_qsuf_get_v( void );
extern int
HECMW_mesh_hsort_qsuf( int node1, int node2, int node3, int node4 );

#endif  /* INC_HECMW_MESH_HASH_SORT */
