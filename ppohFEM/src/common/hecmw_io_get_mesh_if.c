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
#include "hecmw_struct.h"
#include "hecmw_util.h"
#include "hecmw_dist_free.h"
#include "hecmw_io_get_mesh.h"
#include "hecmw_dist_copy_c2f.h"

static struct hecmwST_local_mesh *mesh;



void
hecmw_get_mesh_init_if(char *name_ID, int *err, int len)
{
	char cname[HECMW_FILENAME_LEN+1];

	if(HECMW_strcpy_f2c_r(name_ID, len, cname, sizeof(cname)) == NULL) {
		*err = 1;
		return;
	}

	mesh = HECMW_get_mesh(cname);
	if(mesh == NULL) {
		*err = 1;
		return;
	}

	if(HECMW_dist_copy_c2f_init(mesh)) {
		*err = 1;
		return;
	}

	*err = 0;
}



void
hecmw_get_mesh_init_if_(char *name_ID, int *err, int len)
{
	hecmw_get_mesh_init_if(name_ID, err, len);
}



void
hecmw_get_mesh_init_if__(char *name_ID, int *err, int len)
{
	hecmw_get_mesh_init_if(name_ID, err, len);
}


void
HECMW_GET_MESH_INIT_IF(char *name_ID, int *err, int len)
{
	hecmw_get_mesh_init_if(name_ID, err, len);
}


/*----------------------------------------------------------------------------*/


void
hecmw_get_mesh_finalize_if(int *ierr)
{
	if(HECMW_dist_copy_c2f_finalize()) {
		*ierr = 1;
		return;
	}
	HECMW_dist_free(mesh);
	mesh = NULL;
	*ierr = 0;
}



void
hecmw_get_mesh_finalize_if_(int *ierr)
{
	hecmw_get_mesh_finalize_if(ierr);
}



void
hecmw_get_mesh_finalize_if__(int *ierr)
{
	hecmw_get_mesh_finalize_if(ierr);
}



void
HECMW_GET_MESH_FINALIZE_IF(int *ierr)
{
	hecmw_get_mesh_finalize_if(ierr);
}
