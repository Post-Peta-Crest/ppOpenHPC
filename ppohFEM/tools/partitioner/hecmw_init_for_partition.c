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

#include "hecmw_msgno.h"
#include "hecmw_malloc.h"
#include "hecmw_error.h"

#include "hecmw_part_define.h"
#include "hecmw_part_get_control.h"
#include "hecmw_init_for_partition.h"

#define DEFAULT_CONTROL_FILE_NAME "hecmw_part_ctrl.dat"


static void
print_usage( void )
{
    fprintf( stderr, "Usage: hecmw_part [-f file]\n" );
    fprintf( stderr, "\n" );
    fprintf( stderr, "  -f  specify control file name\n" );
    fprintf( stderr, "  -v  print verbose messages\n" );
    fprintf( stderr, "  -h  print usage\n" );
}


extern int
HECMW_init_for_partition( int argc, char **argv )
{
    int counter;
    char control_file_name[HECMW_FILENAME_LEN+1];


    strcpy( control_file_name, DEFAULT_CONTROL_FILE_NAME );

    if( argc > 1 ) {
        counter=1;
        while( counter < argc ) {

            if( !strcmp( argv[counter], "-f" ) ) {
                counter++;
                if( counter >= argc ) {
                    print_usage( );
                    goto error;
                }
                if( strlen( argv[counter] ) > HECMW_FILENAME_LEN ) {
                    HECMW_set_error( HECMW_PART_E_TOO_LONG_FNAME,
                                     "%s", "control file for partitioner" );
                    goto error;
                }
                strcpy( control_file_name, argv[counter] );
                counter++;

            } else if( !strcmp( argv[counter], "-v" ) ) {
                counter++;
                HECMW_setloglv( HECMW_LOG_DEBUG );

            } else {
                print_usage( );
                goto error;
            }
        }
    }

    if( HECMW_part_set_ctrl_file_name( control_file_name ) )  goto error;

    return 0;

error:
    return -1;
}
