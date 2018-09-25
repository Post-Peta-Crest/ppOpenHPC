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

module fstr_ctrl_modifier

use fstr_setup_util
use hecmw_util
implicit none

contains

  !> Append new equation condition at end of existing mpc conditions
  subroutine fstr_append_mpc( np, nodes, dofs, values, mpcs )
    integer, intent(in)                :: np           !< number of equation items
    integer, intent(in)                :: nodes(np)    !< number of nodes mpc related
    integer, intent(in)                :: dofs(np)     !< dofs of mpc related
    real(kind=kreal), intent(in)       :: values(np+1) !< coefficient of the equation
    type( hecmwST_mpc ), intent(inout) :: mpcs         !< to who mpc be appended

    integer :: i, n_mpc, old_size, new_size
    n_mpc = mpcs%n_mpc
    new_size= n_mpc+1
    mpcs%n_mpc = new_size
    call fstr_expand_index_array( mpcs%mpc_index, n_mpc+1, new_size+1 )
    call fstr_expand_real_array( mpcs%mpc_const, n_mpc, n_mpc+1 )
    old_size = mpcs%mpc_index( n_mpc )
    new_size = old_size+np
    call fstr_expand_integer_array( mpcs%mpc_item, old_size, new_size )
    call fstr_expand_integer_array( mpcs%mpc_dof, old_size, new_size )
    call fstr_expand_real_array( mpcs%mpc_val, old_size, new_size )

    mpcs%mpc_index(mpcs%n_mpc) = mpcs%mpc_index(mpcs%n_mpc-1)+np
    mpcs%mpc_const(mpcs%n_mpc) = values(np+1)
    do i=1,np
      mpcs%mpc_item(old_size+i) = nodes(i)
      mpcs%mpc_dof(old_size+i) = dofs(i)
      mpcs%mpc_val(old_size+i) = values(i)
    enddo
  end subroutine

  !> Delete last n equation conditions from current mpc condition
  subroutine fstr_delete_mpc( np, mpcs )
    integer, intent(in)                :: np     !< number of equations to be deleted
    type( hecmwST_mpc ), intent(inout) :: mpcs   !< from who mpcs to be deleted

    integer :: n_mpc, old_size, nitem
    n_mpc = mpcs%n_mpc
    old_size = mpcs%mpc_index( n_mpc )
    nitem = old_size- mpcs%mpc_index( n_mpc-np )
    call fstr_delete_real_array( mpcs%mpc_val, old_size, nitem )
    call fstr_delete_integer_array( mpcs%mpc_dof, old_size, nitem )
    call fstr_delete_integer_array( mpcs%mpc_item, old_size, nitem )

    call fstr_delete_real_array( mpcs%mpc_const, n_mpc, np )
    call fstr_delete_index_array( mpcs%mpc_index, n_mpc, np )
    mpcs%n_mpc = n_mpc-np
  end subroutine

end module

