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


#ifndef HECMW_CONTROL_INCLUDED
#define HECMW_CONTROL_INCLUDED

#define HECMW_CTRL_FILE "hecmw_ctrl.dat"

struct hecmw_ctrl_meshfile {

	int type;

#define HECMW_CTRL_FTYPE_HECMW_DIST 1

#define HECMW_CTRL_FTYPE_HECMW_ENTIRE 2

#define HECMW_CTRL_FTYPE_GEOFEM 3

#define HECMW_CTRL_FTYPE_ABAQUS 4

#define HECMW_CTRL_FTYPE_NASTRAN 5

#define HECMW_CTRL_FTYPE_FEMAP 6

	int io;

#define HECMW_CTRL_FILE_IO_IN 1

#define HECMW_CTRL_FILE_IO_OUT 2

	int refine;

	char *filename;

};

#define HECMW_CTRL_FILE_IO_INOUT 4

struct hecmw_ctrl_meshfiles {

	int n_mesh;

	struct hecmw_ctrl_meshfile *meshfiles;
};

extern int HECMW_ctrl_init(void);
extern int HECMW_ctrl_init_ex(const char *ctrlfile);
extern int HECMW_ctrl_finalize(void);
extern struct hecmw_ctrl_meshfiles *HECMW_ctrl_get_meshfiles(char *name_ID);
extern struct hecmw_ctrl_meshfiles *HECMW_ctrl_get_meshfiles_header(char *name_ID);
extern struct hecmw_ctrl_meshfiles *HECMW_ctrl_get_meshfiles_sub(char *name_ID, int n_rank, int i_rank);
extern struct hecmw_ctrl_meshfiles *HECMW_ctrl_get_meshfiles_header_sub(char *name_ID, int n_rank, int i_rank);
extern void HECMW_ctrl_free_meshfiles(struct hecmw_ctrl_meshfiles *meshfiles);
extern char *HECMW_ctrl_get_result_file(char *name_ID, int nstep, int istep, int* fg_text);
extern char *HECMW_ctrl_get_result_fileheader(char *name_ID, int nstep, int istep, int* fg_text);
extern char *HECMW_ctrl_get_result_file_sub(char *name_ID, int nstep, int istep, int n_rank, int i_rank, int* fg_text);
extern char *HECMW_ctrl_get_result_fileheader_sub(char *name_ID, int nstep, int istep, int n_rank, int i_rank, int* fg_text);
extern char *HECMW_ctrl_get_restart_file(char *name_ID);
extern char *HECMW_ctrl_get_restart_file_by_io(int io);
extern char *HECMW_ctrl_get_control_file(char *name_ID);
extern int HECMW_ctrl_is_exists_control(char *name_ID);
extern int HECMW_ctrl_make_subdir(char *filename);
extern int HECMW_ctrl_is_subdir(void);

#endif
