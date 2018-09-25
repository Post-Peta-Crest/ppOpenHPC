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
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include "hecmw_util.h"
#include "hecmw_io.h"
#include "hecmw_init_for_partition.h"
#include "hecmw_partition.h"


int
main( int argc, char **argv )
{
  struct hecmwST_local_mesh *global_mesh = NULL;
  struct hecmwST_local_mesh *local_mesh = NULL;
  int rtc;

  rtc = HECMW_init( &argc, &argv );
  if( rtc != 0 )  goto error;

  rtc = HECMW_init_for_partition( argc, argv );
  if( rtc != 0 )  goto error;

  HECMW_log( HECMW_LOG_INFO, "Reading mesh file..." );
  global_mesh = HECMW_get_mesh( "part_in" );
  if( global_mesh == NULL )  goto error;

  local_mesh = HECMW_partition( global_mesh );
  if( local_mesh == NULL )  goto error;

  HECMW_dist_free( global_mesh );

  HECMW_finalize( );

  return 0;

error:
  HECMW_dist_free( global_mesh );
  HECMW_finalize( );
  HECMW_abort( HECMW_comm_get_comm( ) );

  return -1;
}
