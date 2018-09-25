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


#include "hecmw_vis_subimage_composite_vr.h"

void composite_subimage_vr(int pesize, int *pe_id, int pixn, double *n_subimage, double *n_subopa,
		double *subimage)
{
	int i, j, pe_no;
	double acc_opa, r, g, b;

	for(j=0;j<pixn;j++) {
		pe_no=pe_id[0];
		subimage[j*3+0]=n_subimage[pe_no*pixn*3+j*3+0];
		subimage[j*3+1]=n_subimage[pe_no*pixn*3+j*3+1];
		subimage[j*3+2]=n_subimage[pe_no*pixn*3+j*3+2];
		acc_opa=n_subopa[pe_no*pixn+j];
		i=0;
		while((acc_opa<0.99) && (i<pesize-1)) {
			i++;
			pe_no=pe_id[i];
			r=n_subimage[pe_no*pixn*3+j*3];
			g=n_subimage[pe_no*pixn*3+j*3+1];
			b=n_subimage[pe_no*pixn*3+j*3+2];
			subimage[j*3]+=r*(1.0-acc_opa);
			subimage[j*3+1]+=g*(1.0-acc_opa);
			subimage[j*3+2]+=b*(1.0-acc_opa);
			acc_opa+=n_subopa[pe_no*pixn+j]*(1.0-acc_opa);
		}
	}
	return;
}



