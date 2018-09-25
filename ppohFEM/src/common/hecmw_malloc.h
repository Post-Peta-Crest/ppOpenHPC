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




#ifndef HECMW_MALLOC_INCLUDED
#define HECMW_MALLOC_INCLUDED

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef HECMW_MALLOC
#define HECMW_malloc(size) HECMW_malloc_(size, __FILE__, __LINE__)
#define HECMW_calloc(nmemb, size) HECMW_calloc_(nmemb, size, __FILE__, __LINE__)
#define HECMW_realloc(ptr, size) HECMW_realloc_(ptr, size, __FILE__, __LINE__)
#define HECMW_strdup(s) HECMW_strdup_(s, __FILE__, __LINE__)
#define HECMW_free(ptr) HECMW_free_(ptr, __FILE__, __LINE__)
#else
#define HECMW_malloc(size) malloc(size)
#define HECMW_calloc(nmemb, size) calloc(nmemb, size)
#define HECMW_realloc(ptr, size) realloc(ptr, size)
#define HECMW_strdup(s) strdup(s)
#define HECMW_free(ptr) free(ptr)
#endif



extern void *HECMW_malloc_(size_t size, char *file, int line);

extern void *HECMW_calloc_(size_t nmemb, size_t size, char *file, int line);


extern void *HECMW_realloc_(void *ptr, size_t size, char *file, int line);


extern char *HECMW_strdup_(const char *s, char *file, int line);


extern void HECMW_free_(void *ptr, char *file, int line);


extern int HECMW_check_memleak(void);


extern long HECMW_get_memsize(void);


extern void HECMW_set_autocheck_memleak(int flag);


extern int HECMW_list_meminfo(FILE *fp);

#endif
