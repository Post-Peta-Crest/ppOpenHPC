!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/


module hecmw_allocate
contains

   subroutine hecmw_allocate_matrix(hecMAT, mat, nBlock)
      use hecmw_util

      implicit none

      type (hecmwST_matrix):: hecMAT, mat
      integer:: nBlock
      integer:: ierr

      allocate( mat%AL( nBlock*hecMAT%NPL ), STAT=ierr )
      if ( ierr /= 0 ) then
         write(0,*) 'Allocation error: Not enough memory for matrix!'
         stop
      end if
      allocate( mat%AU( nBlock*hecMAT%NPU ), STAT=ierr )
      if ( ierr /= 0 ) then
         write(0,*) 'Allocation error: Not enough memory for matrix!'
         stop
      end if
      allocate( mat%D ( nBlock*hecMAT%NP  ), STAT=ierr )
      if ( ierr /= 0 ) then
         write(0,*) 'Allocation error: Not enough memory for matrix!'
         stop
      end if

      MAT%AL = 0.0d0
      MAT%AU = 0.0d0
      MAT%D  = 0.0d0

   end subroutine hecmw_allocate_matrix

   subroutine hecmw_allocate_vector_I(vector, size)
      use hecmw_util
      implicit none

      integer(kind=kint), pointer:: vector(:)
      integer(kind=kint):: size
      integer(kind=kint):: ierr

      allocate( vector( size ), STAT=ierr )
      if ( ierr /= 0 ) then
         write(0,*) 'Allocation error: Not enough memory for integer array!'
         stop
      end if

      vector = 0
   end subroutine hecmw_allocate_vector_I

   subroutine hecmw_allocate_vector_R(vector, size)
      use hecmw_util
      implicit none

      real(kind=kreal), pointer:: vector(:)
      integer(kind=kint):: size
      integer(kind=kint):: ierr

      allocate( vector( size ), STAT=ierr )
      if ( ierr /= 0 ) then
         write(0,*) 'Allocation error: Not enough memory for real array!'
         stop
      end if

      vector = 0.0d0
   end subroutine hecmw_allocate_vector_R

end module hecmw_allocate
