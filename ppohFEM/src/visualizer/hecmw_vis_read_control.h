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

#ifndef HECMW_VIS_READ_CONTROL_H_INCLUDED
#define HECMW_VIS_READ_CONTROL_H_INCLUDED

#include <stdio.h>
#include "hecmw_vis_SF_geom.h"
#include "hecmw_vis_ray_trace.h"

#define MAX_LINE_LEN  256

int is_blank_line(char *buf);
int is_comment_line(char *buf);
void get_string_item(char *para, char *buf, int *start_location, char para2[128]);
int get_int_item(char *para, char *buf, int *start_location);
double get_double_item(char *para, char *buf, int *start_location);
int get_keyword_item(char *buf, char *para);
void HECMW_vis_read_control(FILE *fp, int pesize, int mynode, PSF_link *psf, PVR_link *pvr);

#endif /* HECMW_VIS_READ_CONTROL_H_INCLUDED */


