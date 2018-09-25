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


#ifndef hecmw_bin_ioH
#define hecmw_bin_ioH

#include <stdio.h>

/*---------------------------------------------------------------------------*/
/* CAUTION) hecmw_set_endian_info must be executed before calling following functions. */

void hecmw_set_endian_info(void);

/*---------------------------------------------------------------------------*/

int hecmw_write_bin_value( unsigned char* x, int size, FILE* fp );
int hecmw_write_bin_int( int x, FILE* fp );
int hecmw_write_bin_int_arr( int* x, int n, FILE* fp );
int hecmw_write_bin_double( double x, FILE* fp );
int hecmw_write_bin_double_arr( double* x, int n, FILE* fp );

/*---------------------------------------------------------------------------*/

int hecmw_read_bin_value( unsigned char* x, int size, FILE* fp );
int hecmw_read_bin_int( int* x, FILE* fp );
int hecmw_read_bin_int_arr( int* x, int n, FILE* fp );
int hecmw_read_bin_double( double* x, FILE* fp );
int hecmw_read_bin_double_arr( double* x, int n, FILE* fp );

/*---------------------------------------------------------------------------*/

/*
 * fmt : type and array size of arguments
 *       Each cherecter in fmt specifies type of argument and
 *       number after the cheracter means array size.
 * meening of character in fmt
 * 'I' : int
 * 'F' : double
 * 'S' : string (char*)
 * exp)
 *     int n, i[10]; char s[20] = "123";
 *     hecmw_write_bin(fp, "II10S", n, i, s);
 */

int hecmw_write_bin( FILE* fp, const char* fmt, ... );
int hecmw_read_bin( FILE* fp, const char* fmt, ... );


#endif
