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




#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "hecmw_geometric.h"


#define HECMW_PI 3.1415926535897932

/* Degree to Radian Transformation */
double
HECMW_degree_to_radian(double deg)
{
	return deg * HECMW_PI / 180;
}


/* Radian to Degree Transformation */
double
HECMW_radian_to_degree(double rad)
{
	return rad * 180 / HECMW_PI;
}


int
HECMW_cylindrical_to_cartesian(
		const struct hecmw_coord *coord, struct hecmw_coord *result)
{
	double r;
	double rtheta;
	double z;

	if(result == NULL) return -1;

	r      = coord->x;
	rtheta = coord->y;	/* radian */
	z      = coord->z;

	result->x = r * cos(rtheta);
	result->y = r * sin(rtheta);
	result->z = z;

	return 0;
}

