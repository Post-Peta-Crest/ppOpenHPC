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

#ifndef HECMW_VIS_COLOR_COMPOSITE_SF_H_INCLUDED
#define HECMW_VIS_COLOR_COMPOSITE_SF_H_INCLUDED

int find_surface_point(double fp[3][3], double point_o[3], double view_point_d[3],double point[3], double n_f[4],
		double v_f[9], double c_value[3], double *value, double normal[9], int normal_flag, int smooth_shading);
int find_point_depth(double fp[3][3], double point_o[3], double view_point_d[3], double n_f[4], double point[3],
		int normal_flag);
int find2_point_depth(double fp[3][3], double point_o[3], double view_point_d[3], double point[3]);
void compute_color_sf(double p[3], double value, double n[3], int color_mapping_style, double *interval_point,
		double view_point_d[3],  int interval_mapping_num, int color_system_type, int num_of_lights,
		double *light_point, double k_ads[3], double accum_rgba[4],
		double mincolor, double maxcolor, int display_method);

#endif /* HECMW_VIS_COLOR_COMPOSITE_SF_H_INCLUDED */






