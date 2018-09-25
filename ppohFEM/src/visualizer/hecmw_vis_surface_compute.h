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

#ifndef HECMW_VIS_SURFACE_COMPUTE_H_INCLUDED
#define HECMW_VIS_SURFACE_COMPUTE_H_INCLUDED

#include "hecmw_struct.h"
#include "hecmw_result.h"
#include "hecmw_util.h"
#include "hecmw_vis_SF_geom.h"

/*----------------------------------------------------------------------
#     Subroutines in this file on isosurface generation for hexahedra by Marching Cubes is based
	  on the revision of Dr. Yuriko Takeshima's codes when she was working part time in RIST
#---------------------------------------------------------------------- */


int HECMW_vis_surface_compute(Surface *sff, struct hecmwST_local_mesh *mesh, struct hecmwST_result_data *data,
		int *bdflag, int *sum_v, int *sum_t,int *tvertex, int *tpatch,
		double *minc, double *maxc, Result *result, int sf_i, int mynode,  HECMW_Comm VIS_COMM );
int HECMW_vis_chk_bounds(struct hecmwST_local_mesh *mesh, int *bdflag);
void find_isoline(int isomum, int sum_polys, double mincolor, double maxcolor, double *vcoord,
		int *plist, double *vcolor, Isohead *isohead);
int find_line_segment(double f[3][3], double c[3], double isocolor, double iso_p[6]);
void line_find(double isocolor, double c[3], Fgeom g[3], int k, Isohead *isohead);
void isoline_free(int isonum,Isohead *isohead);

#endif /* HECMW_VIS_SURFACE_COMPUTE_H_INCLUDED */
