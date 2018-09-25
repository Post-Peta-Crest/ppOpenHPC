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



#ifndef HECMW_PARTLEX_INCLUDED
#define HECMW_PARTLEX_INCLUDED

#include <stdio.h>


#define HECMW_PARTLEX_NL              1001

#define HECMW_PARTLEX_INT             1002

#define HECMW_PARTLEX_DOUBLE          1003

#define HECMW_PARTLEX_NAME            1004

#define HECMW_PARTLEX_FILENAME        1005


#define HECMW_PARTLEX_H_PARTITION     2001


#define HECMW_PARTLEX_K_TYPE          3001

#define HECMW_PARTLEX_K_METHOD        3011

#define HECMW_PARTLEX_K_DOMAIN        3021

#define HECMW_PARTLEX_K_DEPTH         3031

#define HECMW_PARTLEX_K_UCD           3041

#define HECMW_PARTLEX_K_CONTACT       3051


#define HECMW_PARTLEX_V_NODE_BASED    3002

#define HECMW_PARTLEX_V_ELEMENT_BASED 3003

#define HECMW_PARTLEX_V_RCB           3012

#define HECMW_PARTLEX_V_KMETIS        3013

#define HECMW_PARTLEX_V_PMETIS        3014

#define HECMW_PARTLEX_V_DEFAULT       3052

#define HECMW_PARTLEX_V_AGGREGATE     3053


extern double HECMW_partlex_get_number( void );


extern char *HECMW_partlex_get_text( void );


extern int HECMW_partlex_get_lineno( void );


extern int HECMW_partlex_next_token( void );


extern int HECMW_partlex_next_token_skip( int skip_token );


extern int HECMW_partlex_set_input( FILE *fp );


extern int HECMW_partlex_skip_line( void );


extern int HECMW_partlex_unput_token( void );

#endif
