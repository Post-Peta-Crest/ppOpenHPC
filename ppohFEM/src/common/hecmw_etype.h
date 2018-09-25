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




#ifndef INC_HECMW_ETYPE
#define INC_HECMW_ETYPE

#include "hecmw_util.h"

extern int
HECMW_get_etype_UTIL2HECMW( int etype );

extern int
HECMW_get_etype_HECMW2UTIL( int etype );

extern int
HECMW_get_etype_GeoFEM2HECMW( int etype );

extern int
HECMW_get_max_node( int etype );

extern int
HECMW_get_max_edge( int etype );

extern int
HECMW_get_max_surf( int etype );

extern int
HECMW_get_max_tsuf( int etype );

extern int
HECMW_get_max_qsuf( int etype );

extern char
*HECMW_get_ucd_label( int etype );


extern int
HECMW_is_etype_rod(int etype);


extern int
HECMW_is_etype_surface(int etype);


extern int
HECMW_is_etype_solid(int etype);


extern int
HECMW_is_etype_interface(int etype);


extern int
HECMW_is_etype_beam(int etype);


extern int
HECMW_is_etype_shell(int etype);


extern int
HECMW_is_etype_link(int etype);


extern int
HECMW_is_etype_33struct(int etype);


extern int
HECMW_is_etype_truss(int etype);

#endif

