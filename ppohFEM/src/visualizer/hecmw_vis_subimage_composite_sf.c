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


#include "hecmw_vis_subimage_composite_sf.h"

void composite_subimage_sf(int pesize, int pixn, double *n_subimage, double *n_subopa,
		double *subimage, double *subopa)
{
	int i, j;

	for(j=0;j<pixn;j++) {
		subimage[j*3+0]=n_subimage[j*3+0];
		subimage[j*3+1]=n_subimage[j*3+1];
		subimage[j*3+2]=n_subimage[j*3+2];
		subopa[j]=n_subopa[j];
	}
	for(i=1;i<pesize;i++) {
		for(j=0;j<pixn;j++) {
			if(n_subopa[i*pixn+j]<subopa[j]) {
				subopa[j]=n_subopa[i*pixn+j];
				subimage[j*3]=n_subimage[i*pixn*3+j*3+0];
				subimage[j*3+1]=n_subimage[i*pixn*3+j*3+1];
				subimage[j*3+2]=n_subimage[i*pixn*3+j*3+2];
			}
		}
	}
	return;
}
