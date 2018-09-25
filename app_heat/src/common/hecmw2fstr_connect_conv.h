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





/*======================================================================*/
/*                                                                      */
/* HECMW to FSTR Mesh Data Converter                                    */
/* Convering Conectivity of Element Type 232, 342 and 352               */
/*                                                                      */
/*======================================================================*/

void c_hecmw2fstr_connect_conv( int n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] );

/*======================================================================*/
/*                                                                      */
/* FSTR to HECMW Mesh Data Converter                                    */
/* Convering Conectivity of Element Type 232, 342 and 352               */
/*                                                                      */
/*======================================================================*/

void c_fstr2hecmw_connect_conv( int n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] );
void c_fstr2hecmw_elem_conv( int elem_type, int node[] );
