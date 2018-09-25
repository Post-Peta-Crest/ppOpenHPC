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
#include <string.h>
#include "hecmw_util.h"
#include "hecmw_malloc.h"
#include "hecmw_config.h"
#include "hecmw_bit_array.h"


static const size_t nbit_ulong = 8 * sizeof(unsigned long);


int HECMW_bit_array_init(struct hecmw_bit_array *ba, size_t len)
{
  size_t size;

  HECMW_assert(ba);
  HECMW_assert(len >= 0);

  size = (len / nbit_ulong + 1) * sizeof(unsigned long);

  ba->vals = (unsigned long *) HECMW_malloc(size);
  if (ba->vals == NULL) {
    return HECMW_ERROR;
  }
  memset(ba->vals, 0, size);

  ba->len = len;

  return HECMW_SUCCESS;
}

void HECMW_bit_array_finalize(struct hecmw_bit_array *ba)
{
  HECMW_assert(ba);

  HECMW_free(ba->vals);
  ba->len = 0;
}


size_t HECMW_bit_array_len(struct hecmw_bit_array *ba)
{
  HECMW_assert(ba);

  return ba->len;
}

void HECMW_bit_array_set(struct hecmw_bit_array *ba, size_t index)
{
  HECMW_assert(ba);
  HECMW_assert(0 <= index && index < ba->len);

  ba->vals[index / nbit_ulong] |= 1UL << (index % nbit_ulong);
}

int HECMW_bit_array_get(struct hecmw_bit_array *ba, size_t index)
{
  HECMW_assert(ba);
  HECMW_assert(0 <= index && index < ba->len);

  if (ba->vals[index / nbit_ulong] & (1UL << (index % nbit_ulong)))
    return 1;
  else
    return 0;
}

void HECMW_bit_array_set_all(struct hecmw_bit_array *ba)
{
  unsigned long ptn = 0;
  size_t i, nval;

  HECMW_assert(ba);

  for (i = 0; i < nbit_ulong; i++)
    ptn |= 1UL << i;

  nval = ba->len / nbit_ulong + 1;

  for (i = 0; i < nval; i++)
    ba->vals[i] = ptn;
}

void HECMW_bit_array_unset(struct hecmw_bit_array *ba, size_t index)
{
  HECMW_assert(ba);
  HECMW_assert(0 <= index && index < ba->len);

  ba->vals[index / nbit_ulong] &= ~(1UL << (index % nbit_ulong));
}
