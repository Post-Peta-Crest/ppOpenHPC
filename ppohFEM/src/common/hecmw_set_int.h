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



#ifndef HECMW_SET_INT_INCLUDED
#define HECMW_SET_INT_INCLUDED

struct hecmw_varray_int;

struct hecmw_set_int {
  struct hecmw_varray_int *vals;

  int checked;
  int sorted;

  int in_iter;
  size_t iter;
};


extern int HECMW_set_int_init(struct hecmw_set_int *set);

extern void HECMW_set_int_finalize(struct hecmw_set_int *set);


extern size_t HECMW_set_int_nval(struct hecmw_set_int *set);

extern int HECMW_set_int_is_empty(const struct hecmw_set_int *set);

extern int HECMW_set_int_add(struct hecmw_set_int *set, int value);

extern size_t HECMW_set_int_check_dup(struct hecmw_set_int *set);

extern int HECMW_set_int_del(struct hecmw_set_int *set, int value);


extern void HECMW_set_int_iter_init(struct hecmw_set_int *set);

extern int HECMW_set_int_iter_next(struct hecmw_set_int *set, int *value);

#endif /* HECMW_SET_INT_INCLUDED */
