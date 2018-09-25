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


#ifndef HECMW_RESULT_INCLUDED
#define HECMW_RESULT_INCLUDED

#include "hecmw_struct.h"

struct hecmwST_result_data {
	int nn_component;
	int ne_component;
	int *nn_dof;
	int *ne_dof;
	char **node_label;
	char **elem_label;
	double *node_val_item;
	double *elem_val_item;
};


extern int HECMW_result_init(struct hecmwST_local_mesh *hecMESH, int n_step, int i_step, char *header);
extern int HECMW_result_init_body(int n_node, int n_elem, int *nodeID, int *elemID, int n_step, int i_step, char *header);
extern int HECMW_result_finalize(void);
extern int HECMW_result_add(int node_or_elem, int n_dof, char *label, double *ptr);
extern int HECMW_result_write_by_name(char *name_ID);
extern int HECMW_result_write_by_addfname(char *name_ID, char *addfname);
extern int HECMW_result_write_bin_by_fname(char *filename);
extern int HECMW_result_write_txt_by_fname(char *filename);

extern int HECMW_result_write_ST_by_name(char *name_ID, struct hecmwST_result_data *result,
										 int n_node, int n_elem, char *header);
extern int HECMW_result_write_bin_ST_by_fname(char *filename, struct hecmwST_result_data *result,
											  int n_node, int n_elem, char *header);
extern int HECMW_result_write_txt_ST_by_fname(char *filename, struct hecmwST_result_data *result,
											  int n_node, int n_elem, char *header);

extern struct hecmwST_result_data *HECMW_result_read_by_name(char *name_ID, int n_step, int i_step);
extern struct hecmwST_result_data *HECMW_result_read_by_fname(char *filename);
extern void HECMW_result_free(struct hecmwST_result_data *result);

extern int HECMW_result_get_nnode(void);
extern int HECMW_result_get_nelem(void);
extern char* HECMW_result_get_header(char* buff);
extern int* HECMW_result_get_nodeID(int* buff);
extern int* HECMW_result_get_elemID(int* buff);
extern void HECMW_result_free_nodeID(void);
extern void HECMW_result_free_elemID(void);

#endif
