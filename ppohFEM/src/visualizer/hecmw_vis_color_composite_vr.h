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

#ifndef HECMW_VIS_COLOR_COMPOSITE_VR_H_INCLUDED
#define HECMW_VIS_COLOR_COMPOSITE_VR_H_INCLUDED

extern void compute_color_vr(int current_ijk[3], int color_mapping_style, double *interval_point,int transfer_function_style,
		double opa_value, int num_of_features, double *fea_point,
		double view_point_d[3],  int interval_mapping_num, int color_system_type, int num_of_lights,
		double *light_point, double k_ads[3], int r_level[3], double orig_xyz[3],
		double r_dxyz[3], double *var, double *grad_var, double accum_rgba[4],
		double mincolor, double maxcolor, double grad_minmax[2], double feap_minmax[2],
		double feai_minmax[2], double dis_minmax[2], double *opa_table,
		double in_point[3], double out_point[3], double tav_length,
		int time_step, int print_flag);


#endif /* HECMW_VIS_COLOR_COMPOSITE_VR_H_INCLUDED */





