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




#ifndef HECMW_COMM_INCLUDED
#define HECMW_COMM_INCLUDED

#include "hecmw_config.h"

extern int
HECMW_Comm_rank( HECMW_Comm comm, int *rank );
extern int
HECMW_Comm_size( HECMW_Comm comm, int *size );
extern int
HECMW_Comm_dup( HECMW_Comm comm, HECMW_Comm *new_comm );


extern int
HECMW_Barrier( HECMW_Comm comm );
extern int
HECMW_Wait( HECMW_Request *request, HECMW_Status *statuse );
extern int
HECMW_Waitall( int count, HECMW_Request *array_of_requests, HECMW_Status *array_of_statuses );
extern int
HECMW_Bcast( void *buffer, int count, HECMW_Datatype datatype, int root, HECMW_Comm comm );
extern int
HECMW_Send( void *buffer, int count, HECMW_Datatype datatype, int dest,
            int tag, HECMW_Comm comm );
extern int
HECMW_Recv( void *buffer, int count, HECMW_Datatype datatype, int source,
            int tag, HECMW_Comm comm, HECMW_Status *status );
extern int
HECMW_Isend( void *buffer, int count, HECMW_Datatype datatype, int dest,
             int tag, HECMW_Comm comm, HECMW_Request *request );
extern int
HECMW_Irecv( void *buffer, int count, HECMW_Datatype datatype, int source,
             int tag, HECMW_Comm comm, HECMW_Request *request );
extern int
HECMW_Allreduce( void *sendbuf, void *recvbuf, int count,
                 HECMW_Datatype datatype, HECMW_Op op, HECMW_Comm comm );
extern int
HECMW_Allgather( void *sendbuf, int sendcount, HECMW_Datatype sendtype,
                 void *recvbuf, int recvcount, HECMW_Datatype recvtype, HECMW_Comm comm );
extern int
HECMW_Group_incl( HECMW_Group group, int n, int *ranks, HECMW_Group *newgroup );
extern int
HECMW_Group_excl( HECMW_Group group, int n, int *ranks, HECMW_Group *newgroup );
extern int
HECMW_Comm_create( HECMW_Comm comm, HECMW_Group group, HECMW_Comm *comm_out );
extern int
HECMW_Group_rank( HECMW_Group group, int *rank );
extern int
HECMW_Group_size( HECMW_Group group, int *size );
extern int
HECMW_Comm_group( HECMW_Comm comm, HECMW_Group *group );


extern int HECMW_comm_init(int *argc, char ***argv);
extern int HECMW_comm_is_initialized(void);
extern HECMW_Comm HECMW_comm_get_comm(void);
extern int HECMW_comm_get_size(void);
extern int HECMW_comm_get_rank(void);
extern HECMW_Group HECMW_comm_get_group(void);
extern HECMW_Fint HECMW_Comm_c2f(HECMW_Comm comm);
extern HECMW_Comm HECMW_Comm_f2c(HECMW_Fint comm);
extern int HECMW_comm_is_initialized(void);


extern void
hecmw_comm_init_if(HECMW_Fint *comm, int *size, int *rank, HECMW_Fint *group);
#endif
