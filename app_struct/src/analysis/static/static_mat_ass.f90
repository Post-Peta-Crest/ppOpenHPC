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

module m_static_mat_ass
   contains

   SUBROUTINE fstr_mat_ass (idx_mesh, idx_mat, myEIG, fstrSOLID)

      USE m_fstr
      USE lczparm
      use m_static_lib
      use m_static_mat_ass_main
      use m_fstr_ass_load
      use m_fstr_AddBC
      use fstr_matrix_con_contact

      use ppohFEM

      implicit none
      integer(kind=kint) :: IFLAG, numnp, ndof, i
      real(kind=kreal) :: bsize
      data IFLAG/0/

      integer, intent(IN) :: idx_mesh ! index of mesh 
      integer, intent(IN) :: idx_mat  ! index of stiffness matrix
      type (fstr_solid)         :: fstrSOLID
      type (fstr_param       )  :: fstrPARAM
      type (fstrST_matrix_contact_lagrange)  :: fstrMAT
!* LCZ
      type(lczparam) :: myEIG

      INTEGER(kind=kint) :: n1, n2


      iexit = 0
      IF(myrank == 0) THEN
        WRITE(IMSG,*)
        WRITE(IMSG,*) ' ****   STAGE Stiffness Matrix assembly  **'
      ENDIF

!      hecMAT%NDOF = hecMESH%n_dof ! ndof is set by ppohFEM_mat_con so this code is not required in this stage

      if (IFLAG==0) then
!        CALL memget(masbr,n2*hecMAT%NPL,8)
!        CALL memget(masbr,n2*hecMAT%NPU,8)
!        CALL memget(masbr,(n1*2+n2)*hecMAT%NP,8)
!        CALL memget(masbr,n2*hecMAT%N,8)

        IFLAG= 1
      endif

      fstrSOLID%factor(1)=0.d0; fstrSOLID%factor(2)=1.d0
      call fstr_mat_ass_main (idx_mesh, idx_mat, fstrSOLID)
      call fstr_AddSPRING(1, 1, idx_mesh, idx_mat, fstrSOLID, fstrPARAM)
      write(IDBG,*) 'fstr_mat_ass_main: OK'

      IF(myEIG%eqset==0) THEN
        call fstr_ass_load (1, idx_mesh, idx_mat, fstrSOLID, fstrPARAM)
        write(IDBG,*) 'fstr_mat_ass_load: OK'

      ELSE IF(myEIG%eqset==1) THEN
        IF(myrank == 0) THEN
          WRITE(IMSG,*) '*-------------------------------------------*'
          WRITE(IMSG,*) 'NOTE: Loads ignored for eigenvalue analysis.'
          WRITE(IMSG,*) '*-------------------------------------------*'
        ENDIF

      ENDIF

      call fstr_AddBC(1, 1, idx_mesh,idx_mat,fstrSOLID,fstrPARAM,fstrMAT,1)
      write(IDBG,*) 'fstr_mat_ass_bc: OK'
!C
!C  RHS LOAD VECTOR CHECK
!C
      IF(myEIG%eqset==0) THEN
!        numnp=hecMAT%NP
!        ndof =hecMESH%n_dof
!        bsize=0.0
!        do i=1,numnp*ndof
!          bsize=bsize+hecMAT%B(i)**2
!        enddo
!!C
!!C Gather RHS vector
!!C
!        call hecmw_allREDUCE_R1( hecMESH,bsize,hecmw_sum )
bsize = ppohFEM_get_rhs_norm(1) ! 1 is index of matrix
        if( ppohFEM_comm_get_rank() == 0 ) then
          write(IMSG,*) 'Total RHS size=',bsize
!          write(IMSG,*) 'Total number of equations=',hecMESH%mpc%n_mpc
          write(IMSG,*) 'Total number of equations=', 0 ! currently mpc is not supported
        endif

!        if( bsize < 1.0e-31 .and. hecMESH%mpc%n_mpc==0 ) then
        if( bsize < 1.0e-31 ) then
          iexit = 1
          WRITE(IMSG,*) '###Load Vector Error!'
          stop
        endif

      ENDIF

      return
      end subroutine FSTR_MAT_ASS
end module m_static_mat_ass
