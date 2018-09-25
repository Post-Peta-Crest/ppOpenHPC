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

module m_fstr_StiffMatrix
   implicit none

   contains

!---------------------------------------------------------------------*
!> \brief 接線剛性マトリックスを作成するサブルーチン
subroutine fstr_StiffMatrix( hecMESH, hecMAT, fstrSOLID, tincr)
!---------------------------------------------------------------------*
  use m_fstr
  use m_static_LIB
  use mMechGauss

  type (hecmwST_local_mesh)  :: hecMESH      !< mesh information
  type (hecmwST_matrix)      :: hecMAT       !< linear equation, its right side modified here
  type (fstr_solid)          :: fstrSOLID    !< we need boundary conditions of curr step
  real(kind=kreal),intent(in) :: tincr       !< time increment

  type( tMaterial ), pointer :: material     !< material information

  real(kind=kreal)   :: stiffness(20*6, 20*6)
  integer(kind=kint) :: ierror, nodLOCAL(20)
  real(kind=kreal)   :: tt(20), ecoord(3,20)
  real(kind=kreal)   :: thick, val, pa1
  integer(kind=kint) :: ndof, itype, iS, iE, ic_type, nn, icel, iiS, i, j
  real(kind=kreal)   :: u(3,20), du(3,20), coords(3,3)
  integer            :: ig0, grpid, ig, iS0, iE0,ik, in, isect, ihead, cdsys_ID

  integer(kind=kint), parameter :: idx_mesh = 1

! ----- initialize
  call hecmw_mat_clear( hecMAT )

  ndof = hecMAT%NDOF
  do itype= 1, hecMESH%n_elem_type
    iS= hecMESH%elem_type_index(itype-1) + 1
    iE= hecMESH%elem_type_index(itype  )
    ic_type= hecMESH%elem_type_item(itype)
! ----- Ignore link elements
    if (hecmw_is_etype_link(ic_type)) cycle
! ----- Set number of nodes
    nn = hecmw_get_max_node(ic_type)

! ----- element loop
!$omp parallel default(none), &
!$omp&  private(icel,iiS,j,nodLOCAL,i,ecoord,du,u,tt,cdsys_ID,coords, &
!$omp&          material,thick,stiffness,isect,ihead), &
!$omp&  shared(iS,iE,hecMESH,nn,ndof,fstrSOLID,ic_type,hecMAT,tincr)
!$omp do
    do icel= iS, iE

! ----- nodal coordinate & displacement
      iiS= hecMESH%elem_node_index(icel-1)
      do j=1,nn
        nodLOCAL(j)= hecMESH%elem_node_item (iiS+j)
        do i=1, 3
          ecoord(i,j) = hecMESH%node(3*nodLOCAL(j)+i-3)
        enddo
        do i=1,ndof
          du(i,j) = fstrSOLID%dunode(ndof*nodLOCAL(j)+i-ndof)
          u(i,j)  = fstrSOLID%unode(ndof*nodLOCAL(j)+i-ndof) + du(i,j)
        enddo
        if( fstrSOLID%TEMP_ngrp_tot > 0 .or. fstrSOLID%TEMP_irres >0 )  &
           tt(j)=fstrSOLID%temperature( nodLOCAL(j) )
      enddo

      isect = hecMESH%section_ID(icel)
      cdsys_ID = hecMESH%section%sect_orien_ID(isect)
      if( cdsys_ID > 0 ) call get_coordsys(cdsys_ID, idx_mesh, fstrSOLID, coords)

      material => fstrSOLID%elements(icel)%gausses(1)%pMaterial
      thick = material%variables(M_THICK)
      if(  getSpaceDimension( ic_type )==2 ) thick =1.d0
      if ( ic_type==241 .or. ic_type==242 .or. ic_type==231 .or. ic_type==232 .or. ic_type==2322) then
        call STF_C2( ic_type,nn,ecoord(1:2,1:nn),fstrSOLID%elements(icel)%gausses(:),thick,  &
                     stiffness(1:nn*ndof,1:nn*ndof), fstrSOLID%elements(icel)%iset,          &
                     u(1:2,1:nn) )

      else if ( ic_type==301 ) then
        isect= hecMESH%section_ID(icel)
        ihead = hecMESH%section%sect_R_index(isect-1)
        thick = hecMESH%section%sect_R_item(ihead+1)
        call STF_C1( ic_type,nn,ecoord(:,1:nn),thick,fstrSOLID%elements(icel)%gausses(:),   &
            stiffness(1:nn*ndof,1:nn*ndof), u(1:3,1:nn) )

      else if ( ic_type==361 ) then
        if( fstrSOLID%TEMP_ngrp_tot > 0 .or. fstrSOLID%TEMP_irres >0 ) then
          call STF_C3D8Bbar                                                                        &
               ( ic_type, nn, ecoord(:, 1:nn), fstrSOLID%elements(icel)%gausses(:),                &
                 stiffness(1:nn*ndof,1:nn*ndof), cdsys_ID, coords, tincr, u(1:3, 1:nn), tt(1:nn) )
        else
          call STF_C3D8Bbar                                                               &
               ( ic_type, nn, ecoord(:, 1:nn),fstrSOLID%elements(icel)%gausses(:),        &
                 stiffness(1:nn*ndof, 1:nn*ndof), cdsys_ID, coords, tincr, u(1:3, 1:nn) )
        endif

      else if (ic_type==341 .or. ic_type==351 .or.                     &
               ic_type==342 .or. ic_type==352 .or. ic_type==362 ) then
        if( fstrSOLID%TEMP_ngrp_tot > 0 .or. fstrSOLID%TEMP_irres >0 ) then
          call STF_C3                                                                              &
               ( ic_type, nn, ecoord(:, 1:nn), fstrSOLID%elements(icel)%gausses(:),                &
                 stiffness(1:nn*ndof, 1:nn*ndof), cdsys_ID, coords, tincr, u(1:3,1:nn), tt(1:nn) )
        else
          call STF_C3                                                                     &
               ( ic_type,nn,ecoord(:, 1:nn),fstrSOLID%elements(icel)%gausses(:),          &
                 stiffness(1:nn*ndof, 1:nn*ndof), cdsys_ID, coords, tincr, u(1:3, 1:nn) )
        endif


!
!      else if ( ic_type==731) then
!        call STF_S3(xx,yy,zz,ee,pp,thick,local_stf)
!        call fstr_local_stf_restore_temp(local_stf, nn*ndof, stiffness)
!      else if ( ic_type==741) then
!        call STF_S4(xx,yy,zz,ee,pp,thick,local_stf)
!        call fstr_local_stf_restore_temp(local_stf, nn*ndof, stiffness)
      else
        write(*,*) '###ERROR### : Element type not supported for nonlinear static analysis'
        write(*,*) ' ic_type = ', ic_type
        call hecmw_abort(hecmw_comm_get_comm())
      endif
!
! ----- CONSTRUCT the GLOBAL MATRIX STARTED
      call hecmw_mat_ass_elem(hecMAT, nn, nodLOCAL, stiffness)

    enddo      ! icel
!$omp end do
!$omp end parallel
  enddo        ! itype

end subroutine fstr_StiffMatrix

end module m_fstr_StiffMatrix
