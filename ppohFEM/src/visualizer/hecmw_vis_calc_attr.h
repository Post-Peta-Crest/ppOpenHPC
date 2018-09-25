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

#ifndef HECMW_VIS_CALC_ATTR_H_INCLUDED
#define HECMW_VIS_CALC_ATTR_H_INCLUDED

#include "hecmw_vis_SF_geom.h"

/*----------------------------------------------------------------------
#     Subroutines in this file on isosurface generation by Marching Cubes is based
	  on the revision of Dr. Yuriko Takeshima's codes when she was working part time in RIST
#---------------------------------------------------------------------- */



int	get_point_geom(int point_index, Cell *cell, double fvalue,
		Fgeom *point_geom, double *cdata, int disamb_flag);
void	get_gridpoint(int voxel_index, Cell *cell, Fgeom *vert_geom,
		double *cdata);
void	get_edgepoint(int edge_index, Cell *cell, double fvalue,
		Fgeom *vert_geom, double *cdata);
void	get_insidepoint(int inside_index, Cell *cell, double fvalue,
		Fgeom *vert_geom, double *cdata, int disamb_flag);
double	linear_interpolate(double left, double right, double fvalue);
double	calc_cross_field(double f00, double f10, double f11, double f01);
double	facial_average(double f00, double f10, double f11, double f01);

#endif /* HECMW_VIS_CALC_ATTR_H_INCLUDED */
