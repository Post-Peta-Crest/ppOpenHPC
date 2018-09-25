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


#include "hecmw_vis_mem_util.h"

#include <stdio.h>
#include <stdlib.h>
#include "hecmw_util.h"


void HECMW_vis_memory_exit(char *var)
{
	fprintf(stderr, "#### HEC-MW-VIS-E0001:There is no enough memory allocated for variable %s\n", var);
	HECMW_finalize();
	exit(0);
}

void HECMW_vis_print_exit(char *var)
{
	fprintf(stderr, "%s\n", var);
	HECMW_finalize();
	exit(0);
}


void mfree(void *pointer)
{
	HECMW_free(pointer);
	pointer = NULL;
}

Point *alloc_verts(int num)
{
	int i;
	Point *verts;

	if ((verts = (Point *) HECMW_calloc(num,sizeof(Point))) == NULL) {
		fprintf(stderr,"There is not enough memory, alloc_verts\n");
		return NULL;
	}

	for (i = 0; i < (num - 1); i++) {
		(verts + i)->nextpoint = (verts + i + 1);
		(verts + i)->ident = 0;
	}
	(verts + num - 1)->ident = 0;
	(verts + num - 1)->nextpoint = NULL;


	return verts;
}


Polygon *alloc_polygons(int num)
{
	int	i;
	Polygon *polygons;

	if ((polygons = (Polygon *) HECMW_calloc(num,sizeof(Polygon))) == NULL) {
		fprintf(stderr,"There is not enough memory, alloc_polygons\n");
		return NULL;
	}

	for (i = 0; i < (num - 1); i++) {
		(polygons + i)->nextpolygon = (polygons + i + 1);
		(polygons + i)->plist = NULL;
	}
	(polygons + num - 1)->nextpolygon = NULL;
	(polygons + num - 1)->plist = NULL;


	return polygons;
}
