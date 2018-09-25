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

module m_fstr_AddBC

   implicit none

   contains

!>  Add Essential Boundary Conditions
!------------------------------------------------------------------------------------------*
      subroutine fstr_AddBC(cstep,substep,idx_mesh,idx_mat,fstrSOLID,fstrPARAM,fstrMAT,iter,conMAT)
!------------------------------------------------------------------------------------------*
      use m_fstr
      use fstr_matrix_con_contact
      use m_addContactStiffness
      use mContact
      use m_static_LIB_1d
      integer, intent(in)                  :: cstep     !< current step
      integer, intent(in)                  :: substep   !< current substep
!      type (hecmwST_local_mesh)             :: hecMESH   !< hecmw mesh
      integer, intent(in)                   :: idx_mesh  !< hecmw mesh index
!      type (hecmwST_matrix)                 :: hecMAT    !< hecmw matrix
      integer, intent(in)                   :: idx_mat   !< hecmw matrix index
      type (fstr_solid       )              :: fstrSOLID !< fstr_solid
      type (fstr_param       )              :: fstrPARAM !< analysis control parameters
      type (fstrST_matrix_contact_lagrange) :: fstrMAT   !< type fstrST_matrix_contact_lagrange
      integer(kind=kint)                    :: iter      !< NR iterations
      type (hecmwST_matrix),optional        :: conMAT    !< hecmw matrix for contact only

      integer(kind=kint) :: ig0, ig, ityp, idofS, idofE, idof, iS0, iE0, ik, in
      real(kind=kreal) :: RHS,factor
      integer(kind=kint) :: idof1, idof2, ndof, i, grpid

!
      factor = fstrSOLID%FACTOR(2)-fstrSOLID%FACTOR(1)

      if( cstep<=fstrSOLID%nstep_tot .and. fstrSOLID%step_ctrl(cstep)%solution==stepVisco ) then
         factor = 0.d0
         if( substep==1 ) factor=1.d0
      endif
      if( iter>1 ) factor=0.d0
!   ----- Prescibed displacement Boundary Conditions
      do ig0 = 1, fstrSOLID%BOUNDARY_ngrp_tot
        grpid = fstrSOLID%BOUNDARY_ngrp_GRPID(ig0)
        if( .not. fstr_isBoundaryActive( fstrSOLID, grpid, cstep ) ) cycle
        ig   = fstrSOLID%BOUNDARY_ngrp_ID(ig0)
        RHS  = fstrSOLID%BOUNDARY_ngrp_val(ig0)
!
        RHS= RHS*factor
!
        ityp = fstrSOLID%BOUNDARY_ngrp_type(ig0)
        idofS = ityp/10
        idofE = ityp - idofS*10

        do ik = 1, ppohFEM_get_num_nodes_in_node_group(idx_mesh, ig)
          in = ppohFEM_get_node_item_in_node_group(idx_mesh, ig, ik)

          do idof = idofS, idofE
            call ppohFEM_mat_ass_bc(idx_mat, in, idof, RHS)
! currently contact is not supported
!            if(present(conMAT)) then
!              call hecmw_mat_ass_bc(hecMAT, in, idof, RHS, conMAT)
!            else
!              call hecmw_mat_ass_bc(hecMAT, in, idof, RHS)
!            endif
!            if( fstr_is_contact_active() .and. fstrPARAM%solution_type == kstNLSTATIC   &
!                                         .and. fstrPARAM%contact_algo == kcaSLagrange ) then
!              if(present(conMAT)) then
!                call fstr_mat_ass_bc_contact(conMAT,fstrMAT,in,idof,RHS)
!              else
!                call fstr_mat_ass_bc_contact(hecMAT,fstrMAT,in,idof,RHS)
!              endif
!            endif
          enddo
        enddo
      enddo

!
!   ------ Truss element Diagonal Modification
! DEBUG it should be renew
!      call truss_diag_modify(hecMAT,hecMESH)

!
!   ------ Equation boundary conditions
      do ig0=1,fstrSOLID%n_fix_mpc
          if( fstrSOLID%mpc_const(ig0) == 0.d0 ) cycle
      ! we need to confirm if it is active in curr step here
          RHS = fstrSOLID%mpc_const(ig0)*factor
!          hecMESH%mpc%mpc_const(ig0) = RHS ! DEBUG currently MPC is not supported in ppohFEM
        write(*,*) 'equation is not supported in ppohFEM'
        call ppohFEM_abort
      enddo

      end subroutine fstr_AddBC

end module m_fstr_AddBC
