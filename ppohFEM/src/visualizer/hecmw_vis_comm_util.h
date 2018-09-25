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

#ifndef HECMW_VIS_COMM_UTIL_H_INCLUDED
#define HECMW_VIS_COMM_UTIL_H_INCLUDED

#include "hecmw_util.h"

#define HECMW_STATUS_SIZE 4

extern int HECMW_ANY_TAG;


extern void
whole_copy_array(int *recv_num, int *global_recv_num, int mynode, int pesize, HECMW_Comm repart_comm);

extern int
stack_part_send_recv(int neibpetot, int *neibpe, int *stack_import,  int *stack_export,
		HECMW_Comm repart_comm, int my_rank);

extern int
stack_whole_send_recv(int pesize, int *stack_export,int *stack_import, HECMW_Comm repart_comm, int my_rank);

extern int
int_part_send_recv(int n, int neibpetot, int *neibpe,int *stack_import, int *nod_import,int *stack_export, int *nod_export,
		int *x,  HECMW_Comm repart_comm, int my_rank);

extern int
double_part_send_recv(int n, int neibpetot, int *neibpe, int *stack_import, int *nod_import,
		int *stack_export, int *nod_export, double *x, HECMW_Comm repart_comm, int my_rank);

extern void
int_whole_send_recv(int n1, int n2, int pesize,  int *stack_import, int *nod_import,
		int *stack_export, int *nod_export, int *x, int *y,
		HECMW_Comm repart_comm, int my_rank);
extern void
int2_whole_send_recv(int n1, int n2, int pesize,  int *stack_import,
		int *stack_export, int *x, int *y,
		HECMW_Comm repart_comm, int my_rank);

extern void
double2_whole_send_recv(int n1, int n2, int pesize,  int *stack_import,
		int *stack_export, double *x, double *y,
		HECMW_Comm repart_comm, int my_rank);

extern void
int3_whole_send_recv(int n1, int n2, int pesize,  int *stack_import,
		int *stack_export, int *x, int *y,
		HECMW_Comm repart_comm, int my_rank);

extern void
double_whole_send_recv(int n1, int n2, int pesize,  int *stack_import, int *nod_import,
		int *stack_export, int *nod_export,
		double *x, double *y,
		HECMW_Comm repart_comm, int my_rank);

#endif /* HECMW_VIS_COMM_UTIL_H_INCLUDED */
