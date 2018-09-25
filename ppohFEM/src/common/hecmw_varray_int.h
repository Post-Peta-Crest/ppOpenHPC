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



#ifndef HECMW_VARRAY_INT_INCLUDED
#define HECMW_VARRAY_INT_INCLUDED

struct hecmw_varray_int {
  size_t n_val;
  size_t max_val;

  int *vals;
};


extern int HECMW_varray_int_init(struct hecmw_varray_int *varray);

extern void HECMW_varray_int_finalize(struct hecmw_varray_int *varray);


extern size_t HECMW_varray_int_nval(const struct hecmw_varray_int *varray);

extern int HECMW_varray_int_append(struct hecmw_varray_int *varray, int value);

extern int HECMW_varray_int_get(const struct hecmw_varray_int *varray, size_t index);

extern int HECMW_varray_int_cat(struct hecmw_varray_int *varray,
                                const struct hecmw_varray_int *varray2);

extern void HECMW_varray_int_sort(struct hecmw_varray_int *varray);

extern int HECMW_varray_int_search(struct hecmw_varray_int *varray, int value, size_t *index);

extern size_t HECMW_varray_int_uniq(struct hecmw_varray_int *varray);


extern int HECMW_varray_int_resize(struct hecmw_varray_int *varray, size_t len);

extern int *HECMW_varray_int_get_v(struct hecmw_varray_int *varray);

extern const int *HECMW_varray_int_get_cv(const struct hecmw_varray_int *varray);


extern int HECMW_varray_int_copy(const struct hecmw_varray_int *varray,
                                 struct hecmw_varray_int *varray2);

extern int HECMW_varray_int_rmdup(struct hecmw_varray_int *varray);


extern int HECMW_varray_int_assign(struct hecmw_varray_int *varray,
                                   size_t begin, size_t end, int val);

extern int HECMW_varray_int_insert(struct hecmw_varray_int *varray,
                                   size_t index, int val);

extern int HECMW_varray_int_delete(struct hecmw_varray_int *varray,
                                   size_t index);

#endif /* HECMW_VARRAY_INT_INCLUDED */
