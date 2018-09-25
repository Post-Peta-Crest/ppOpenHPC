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
#include "hecmw_config.h"
#include "hecmw_varray_int.h"

int main()
{
  struct hecmw_varray_int va, vb;
  int i;
  int ndup;
  int *vb_vals;

  if (HECMW_varray_int_init(&va) != HECMW_SUCCESS) {
    perror("init va");
    return HECMW_EXIT_ERROR;
  }

  for (i = 0; i < 1000; i += 7) {
    if (HECMW_varray_int_append(&va, i%13) != HECMW_SUCCESS) {
      perror("append va");
      return HECMW_EXIT_ERROR;
    }
  }

  printf("Initial va:");
  for (i = 0; i < HECMW_varray_int_nval(&va); i++) {
    printf(" %d", HECMW_varray_int_get(&va, i));
  }
  printf("\n");

  if (HECMW_varray_int_sort(&va) != HECMW_SUCCESS) {
    perror("sort va");
    return HECMW_EXIT_ERROR;
  }

  printf("After sorting va:");
  for (i = 0; i < HECMW_varray_int_nval(&va); i++) {
    printf(" %d", HECMW_varray_int_get(&va, i));
  }
  printf("\n");

  ndup = HECMW_varray_int_uniq(&va);
  printf("%d elemeent(s) removed\n", ndup);

  printf("After uniq va:");
  for (i = 0; i < HECMW_varray_int_nval(&va); i++) {
    printf(" %d", HECMW_varray_int_get(&va, i));
  }
  printf("\n");

  if (HECMW_varray_int_init(&vb) != HECMW_SUCCESS) {
    perror("init vb");
    return HECMW_EXIT_ERROR;
  }

  if (HECMW_varray_int_resize(&vb, 100) != HECMW_SUCCESS) {
    perror("resize vb");
    return HECMW_EXIT_ERROR;
  }

  vb_vals = HECMW_varray_int_get_v(&vb);

  if (vb_vals == NULL) {
    perror("get_v vb");
    return HECMW_EXIT_ERROR;
  }

  for (i = 0; i < 100; i++) {
    vb_vals[i] = i * 23 % 31;
  }

  printf("Initial vb:");
  for (i = 0; i < HECMW_varray_int_nval(&vb); i++) {
    printf(" %d", HECMW_varray_int_get(&vb, i));
  }
  printf("\n");

  if (HECMW_varray_int_cat(&va, &vb) != HECMW_SUCCESS) {
    perror("cat va vb");
    return HECMW_EXIT_ERROR;
  }

  printf("After cat va&vb:");
  for (i = 0; i < HECMW_varray_int_nval(&va); i++) {
    printf(" %d", HECMW_varray_int_get(&va, i));
  }
  printf("\n");

  if (HECMW_varray_int_rmdup(&va) != HECMW_SUCCESS) {
    perror("rmdup va");
    return HECMW_EXIT_ERROR;
  }

  printf("After rmdup va:");
  for (i = 0; i < HECMW_varray_int_nval(&va); i++) {
    printf(" %d", HECMW_varray_int_get(&va, i));
  }
  printf("\n");

  HECMW_varray_int_finalize(&vb);
  HECMW_varray_int_finalize(&va);

  return HECMW_EXIT_SUCCESS;
}
