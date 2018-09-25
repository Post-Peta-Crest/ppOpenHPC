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

#ifndef HECMW_H_INCLUDED
#define HECMW_H_INCLUDED
#include "hecmw_ablex.h"
#include "hecmw_bit_array.h"
#include "hecmw_comm.h"
#include "hecmw_common.h"
#include "hecmw_common_define.h"
#include "hecmw_config.h"
#include "hecmw_conn_conv.h"
#include "hecmw_control.h"
#include "hecmw_ctrllex.h"
#include "hecmw_debug_write_dist.h"
#include "hecmw_dist.h"
#include "hecmw_dist_alloc.h"
#include "hecmw_dist_copy_c2f.h"
#include "hecmw_dist_copy_f2c.h"
#include "hecmw_dist_free.h"
#include "hecmw_dist_print.h"
#include "hecmw_dist_refine.h"
#include "hecmw_error.h"
#include "hecmw_etype.h"
#include "hecmw_finalize.h"
#include "hecmw_geometric.h"
#include "hecmw_gflex.h"
#include "hecmw_heclex.h"
#include "hecmw_init.h"
#include "hecmw_io.h"
#include "hecmw_io_abaqus.h"
#include "hecmw_io_dist.h"
#include "hecmw_io_geofem.h"
#include "hecmw_io_get_mesh.h"
#include "hecmw_io_hec.h"
#include "hecmw_io_mesh.h"
#include "hecmw_io_nastran.h"
#include "hecmw_io_put_mesh.h"
#include "hecmw_io_struct.h"
#include "hecmw_lib_fc.h"
#include "hecmw_log.h"
#include "hecmw_malloc.h"
#include "hecmw_map_int.h"
#include "hecmw_msg.h"
#include "hecmw_msgno.h"
#include "hecmw_path.h"
#include "hecmw_reorder.h"
#include "hecmw_restart.h"
#include "hecmw_result.h"
#include "hecmw_result_copy_c2f.h"
#include "hecmw_result_copy_f2c.h"
#include "hecmw_set_int.h"
#include "hecmw_struct.h"
#include "hecmw_system.h"
#include "hecmw_time.h"
#include "hecmw_ucd_print.h"
#include "hecmw_util.h"
#include "hecmw_varray_int.h"
#include "hecmw_vis_SF_geom.h"
#include "hecmw_vis_comm_util.h"
#include "hecmw_vis_mem_util.h"
#include "hecmw_vis_psf_rendering.h"
#include "hecmw_vis_ray_trace.h"
#include "hecmw_vis_read_control.h"
#include "hecmw_visualizer.h"
#endif
