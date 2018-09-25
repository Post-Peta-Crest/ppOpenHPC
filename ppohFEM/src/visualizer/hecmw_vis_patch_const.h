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

#ifndef HECMW_VIS_PATCH_CONST_H_INCLUDED
#define HECMW_VIS_PATCH_CONST_H_INCLUDED

#include "hecmw_vis_SF_geom.h"

/*----------------------------------------------------------------------
#     Subroutines in this file on isosurface generation by Marching Cubes is based
	  on the revision of Dr. Yuriko Takeshima's codes when she was working part time in RIST
#---------------------------------------------------------------------- */

int	merge_vol_iso(int, Cell *, double, Cube_polygons *,
		double, Cube_polygons *, int, int *,
		Point **, Point **, Point **, Polygon **);
int get_edge_index(int bound_index, int vert_index);
int get_vert_ident(int pindex, Cell *cell, double fvalue, int *sum_verts,
		Point **CS_verts_tail, Point **CS_verts_refer,
		Point **CS_verts_head, int bdflag);
int search_verts_table(Fgeom *pgeom, Point **CS_verts_refer,
		int bdflag, int table_no);
int add_verts_table(Point **CS_verts_tail, int table_no, int pident,
		double pfield, double cdata, Fgeom *pgeom,
		int bdflag);
int check_vertex(Cell *cell, double fvalue, Cube_polygons *cube_polys);

#endif /* HECMW_VIS_PATCH_CONST_H_INCLUDED */
