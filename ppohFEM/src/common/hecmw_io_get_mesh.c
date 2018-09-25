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
#include "hecmw_io_get_mesh.h"
#include "hecmw_io_mesh.h"
#include "hecmw_io_hec.h"
#include "hecmw_io_geofem.h"
#include "hecmw_io_abaqus.h"
#include "hecmw_io_dist.h"
#include "hecmw_dist_refine.h"


static struct hecmwST_local_mesh *
get_entire_mesh(struct hecmw_ctrl_meshfiles *files)
{
	int i;
	struct hecmwST_local_mesh *mesh;

	if(HECMW_io_init()) return NULL;
	HECMW_log(HECMW_LOG_DEBUG, "io_init done");

	if(HECMW_io_pre_process()) return NULL;
	HECMW_log(HECMW_LOG_DEBUG, "io_pre_process done");

	for(i=0; i < files->n_mesh; i++) {
		struct hecmw_ctrl_meshfile *file = &files->meshfiles[i];

		switch(file->type) {
			case HECMW_CTRL_FTYPE_HECMW_ENTIRE:
				if(HECMW_read_entire_mesh(file->filename)) return NULL;
				break;
			case HECMW_CTRL_FTYPE_GEOFEM:
				if(HECMW_read_geofem_mesh(file->filename)) return NULL;
				break;
			case HECMW_CTRL_FTYPE_ABAQUS:
				if(HECMW_read_abaqus_mesh(file->filename)) return NULL;
				break;
			default:
				HECMW_assert(0);
		}
	}
	HECMW_log(HECMW_LOG_DEBUG, "reading mesh done\n");

	if(HECMW_io_post_process()) return NULL;
	HECMW_log(HECMW_LOG_DEBUG, "post_process done\n");

	mesh = HECMW_io_make_local_mesh();
	if(mesh == NULL) return NULL;
	HECMW_log(HECMW_LOG_DEBUG, "converting mesh done\n");

	if(HECMW_io_finalize()) return NULL;
	HECMW_log(HECMW_LOG_DEBUG, "io_finalize done\n");

	return mesh;
}


struct hecmwST_local_mesh *
HECMW_get_mesh(char *name_ID)
{
	struct hecmw_ctrl_meshfiles *files;
	struct hecmwST_local_mesh *mesh;
	char filename[HECMW_FILENAME_LEN+1];
	char *cad_filename;
	FILE* fp;

	files = HECMW_ctrl_get_meshfiles(name_ID);
	if(files == NULL) return NULL;

	if(files->n_mesh == 1 && files->meshfiles[0].type == HECMW_CTRL_FTYPE_HECMW_DIST) {
		strcpy(filename, files->meshfiles[0].filename);
		mesh = HECMW_get_dist_mesh(filename);
	} else {
		mesh = get_entire_mesh(files);
	}

	strcpy(filename, files->meshfiles[0].filename);
	strtok(filename, ".");
	strcat(filename, ".rnf");
	if((fp = fopen(filename, "r")) == NULL) {
		cad_filename = NULL;
	} else {
		fclose(fp);
		cad_filename = filename;
	}

	if(HECMW_dist_refine(&mesh, files->meshfiles[0].refine, cad_filename, NULL) != HECMW_SUCCESS) {
		HECMW_dist_free(mesh);
		return NULL;
	}

	HECMW_ctrl_free_meshfiles(files);

	return mesh;
}
