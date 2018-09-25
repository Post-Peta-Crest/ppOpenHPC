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

module mULoad

use hecmw

implicit none

   !> Structure for user defines load. User may need to fill in it
   !> according to specified loads
   type tULoad
      integer, pointer :: nodeID(:)=>null()    !< nodes' ID
      integer, pointer :: dof(:)=>null()       !< dof to be loaded
	  ! == add futher defintiions here ==
   end type

   type(tULoad), pointer, save :: uloads(:)=>null()

contains

!> This suborutine read in variables needs to define user-defined external loads
   integer function ureadload( fname )
	 character(len=*), intent(in)    :: fname   !< input file name
     ureadload = 0
   end function

!> This subroutine take consider of user-defined external loading
   subroutine uloading( cstep, factor, exForce )
     integer, INTENT(IN)             :: cstep      !< current step number
     REAL(KIND=kreal), INTENT(IN)    :: factor     !< loading factor of current step
     REAL(KIND=kreal), INTENT(INOUT) :: exForce(:) !< external force

   end subroutine

!> This subroutine take consider of user-defined external loading
   subroutine uResidual( cstep, factor, residual )
     integer, INTENT(IN)             :: cstep        !< current step number
     REAL(KIND=kreal), INTENT(IN)    :: factor       !< loading factor of current step
     REAL(KIND=kreal), INTENT(INOUT) :: residual(:)  !< residual

   end subroutine

end module

