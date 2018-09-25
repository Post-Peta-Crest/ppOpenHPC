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




#ifndef HECMW_LOG_INCLUDED
#define HECMW_LOG_INCLUDED

#include <stdarg.h>


#define HECMW_LOG_MAX 10

/* ログレベル定義 */

#define HECMW_LOG_NONE	0

#define HECMW_LOG_ERROR	1

#define HECMW_LOG_WARN	2

#define HECMW_LOG_INFO	4

#define HECMW_LOG_DEBUG	8

#define HECMW_LOG_ALL	(HECMW_LOG_ERROR|HECMW_LOG_WARN|HECMW_LOG_INFO|HECMW_LOG_DEBUG)

/* オプション */

#define HECMW_LOG_PERROR	1

#define HECMW_LOG_OPTALL	(HECMW_LOG_PERROR)


extern int HECMW_openlog(const char *logfile, int loglv, int options);


extern int HECMW_closelog(int id);


extern int HECMW_vlog(int loglv, const char *fmt, va_list ap);


extern int HECMW_log(int loglv, const char *fmt , ...);


extern void HECMW_setloglv(int loglv);


extern void HECMW_log_set_enable(int from, int to, int true_or_false);

#endif
