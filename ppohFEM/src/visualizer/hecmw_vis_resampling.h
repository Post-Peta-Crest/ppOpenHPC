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

#ifndef HECMW_VIS_RESAMPLING_H_INCLUDED
#define HECMW_VIS_RESAMPLING_H_INCLUDED
/*
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include    <memory.h>
#include "hecmw_struct.h"
#include "hecmw_util.h"
#include "hecmw_io.h"
*/
#define MASTER_PE 	0

#define PI  3.1415926
#define HEX_N_NODE	8
#define HEX_N_FACE	6
#define HEX_NODE_INDEX	255	/* 2^8 */
#define HEX_FACE_INDEX	63	/* 2^6 */

#define	MAX_LINE_LEN	256

typedef struct surfacep_info {
	double vertex[9];
	double color[3];
	struct surfacep_info *next_patch;
} Surfacep_info;

typedef struct head_surface_info {
	int num_of_patch;
	Surfacep_info *next_patch;
} Head_surfacep_info;

typedef struct in_surface {
	int    n_vertex;
	int    n_patch;
	double *vertex;
	int    *patch;
	double *color;
} In_surface;




typedef struct voxel_info {
	double	dx;
	double	dy;
	double	dz;
	double	orig_x;
	double	orig_y;
	double	orig_z;
	int		level[3];
	int		n_neighbor_pe;
	int		*neighbor_pe;
} Voxel_info;

typedef struct voxel_data {
	int		n_voxel;
	Voxel_info	*info;
} Voxel_data;

typedef struct extent_data {
	double	x_min;
	double	x_max;
	double	y_min;
	double	y_max;
	double	z_min;
	double	z_max;
} Extent_data;



typedef struct cube_pointer_struct {
	int		code[3];
	double	field;
	double	grad[3];
	struct cube_pointer_struct *next_point;

} Cube_point;

typedef struct cube_head_struct {
	int point_num;
	struct cube_pointer_struct *cube_link;
} Cube_head;
typedef struct pvr_data {
	int		myID;
	int		parentID;
	int		level;
	int 		flag;
	double	field;
	double	grad[3];
} PVR_data;

typedef struct cont_data {
	int		n_var;
	char		**name;
} Cont_data;
#endif /* HECMW_VIS_RESAMPLING_H_INCLUDED */
