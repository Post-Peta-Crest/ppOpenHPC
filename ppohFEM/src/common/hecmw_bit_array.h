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



#ifndef HECMW_BIT_ARRAY_INCLUDED
#define HECMW_BIT_ARRAY_INCLUDED

struct hecmw_bit_array {
  size_t len;
  unsigned long *vals;
};


extern int HECMW_bit_array_init(struct hecmw_bit_array *ba, size_t len);

extern void HECMW_bit_array_finalize(struct hecmw_bit_array *ba);


extern size_t HECMW_bit_array_len(struct hecmw_bit_array *ba);

extern void HECMW_bit_array_set(struct hecmw_bit_array *ba, size_t index);

extern int HECMW_bit_array_get(struct hecmw_bit_array *ba, size_t index);

extern void HECMW_bit_array_set_all(struct hecmw_bit_array *ba);

extern void HECMW_bit_array_unset(struct hecmw_bit_array *ba, size_t index);

#endif /* HECMW_BIT_ARRAY_INCLUDED */
