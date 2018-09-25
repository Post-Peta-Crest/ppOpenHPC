/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_FDM3D                                     *
 *         Version : 0.1                                               *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppohVIS_FDM3D                              *
 *     ppohVIS_FDM3D is a free software, you can use it under the      *
 *     termas of The MIT License (MIT). See LICENSE file and User's    *
 *     guide for more details.                                         *
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
 *       - Graduate School of Interdisciplinary Information Studies    *
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
 *   Copyright (c) 2012 <Kengo Nakajima, The University of Tokyo       *
 *                       nakajima(at)cc.u-tokyo.ac.jp           >      *
 *=====================================================================*/
#ifndef __H_ppohVIS_FDM3D_ERROR
#define __H_ppohVIS_FDM3D_ERROR

#include <stdarg.h>

#define PPOHVIS_FDM3D_ERRMSG_LEN (256)

#define ppohVIS_FDM3D_DebugPrint(s) fprintf(stderr, "Error: %s l.%d: %s\n", __FILE__, __LINE__, s);

enum ppohVIS_FDM3D_eErrorCode {
	ppohVIS_FDM3D_ErrorCode_NormalEnd = 10000,
	ppohVIS_FDM3D_ErrorCode_AllocationError,
	ppohVIS_FDM3D_ErrorCode_FileOpenError,
	ppohVIS_FDM3D_ErrorCode_FileReadError,
	ppohVIS_FDM3D_ErrorCode_FileWriteError,
	ppohVIS_FDM3D_ErrorCode_FileCloseError,
	ppohVIS_FDM3D_ErrorCode_NullPointerFound,
	ppohVIS_FDM3D_ErrorCode_InvalidElemType,
	ppohVIS_FDM3D_ErrorCode_UnknownEntityType,
	ppohVIS_FDM3D_ErrorCode_InvalidData,
	ppohVIS_FDM3D_ErrorCode_Error,
};

static char ppohVIS_FDM3D_ErrorMsg[][PPOHVIS_FDM3D_ERRMSG_LEN] = {
	"normal end",
	"memory allocation error",
	"file open error",
	"file read error",
	"file write error",
	"file close error",
	"null pointer found",
	"invalid element type found",
	"unknown entity type found",
	"invalid data found",
	"error occured",
};


extern int
ppohVIS_FDM3D_SetErrorv(int error_code, const char *fmt, va_list ap);

extern int
ppohVIS_FDM3D_SetError(int error_code, const char *fmt, ...);

extern int
ppohVIS_FDM3D_GetError(char **error_msg);


#endif /* __H_ppohVIS_FDM3D_ERROR */
