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

#ifndef HECMW_VIS_GENERATE_HISTOGRAM_VR_H_INCLUDED
#define HECMW_VIS_GENERATE_HISTOGRAM_VR_H_INCLUDED

#include "hecmw_util.h"

void find_color_minmax_vr(double *var, int *empty_flag, int nx, int ny, int nz, double *mincolor, double *maxcolor);
void generate_histogram_graph_vr(double tmincolor, double tmaxcolor, double *var, int *empty_flag, int nx, int ny, int nz,int mynode,
		int pesize, HECMW_Comm VIS_COMM, int color_system_type);
void generate_interval_point_vr(double tmincolor, double tmaxcolor, double *var, int *empty_flag,
		int nx, int ny, int nz,int mynode, int pesize, HECMW_Comm VIS_COMM, double *interval_point);
void output_histogram_vr(double tmincolor, double tmaxcolor, double *var, int *empty_flag, int nx, int ny, int nz,int mynode, int pesize, HECMW_Comm VIS_COMM);
void find_minmax_vr(double *voxel_dxyz, double *voxel_orig_xyz, int mynode, double range[6]);
void find_dis_minmax(double view_point_d[3], double vertex[24], double dis_minmax[2]);
void find_feap_minmax(int num_of_features, double *fea_point, double mincolor, double maxcolor, double feap_minmax[2]);
void find_feai_minmax(int num_of_features, double *fea_point, double mincolor, double maxcolor, double feai_minmax[2]);


#endif /* HECMW_VIS_GENERATE_HISTOGRAM_VR_H_INCLUDED */


