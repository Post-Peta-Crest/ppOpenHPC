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

module m_fstr_NodalStress
  implicit none
  private :: NodalStress_INV3, NodalStress_INV2, inverse_func
contains

!> Calculate NODAL STRESS of solid elements
!----------------------------------------------------------------------*
  subroutine fstr_NodalStress3D( idx_mesh, fstrSOLID, tnstrain, testrain )
!----------------------------------------------------------------------*
    use m_fstr
    use m_static_lib
    integer, intent(IN) :: idx_mesh
    type (fstr_solid)         :: fstrSOLID
    real(kind=kreal), pointer :: tnstrain(:), testrain(:)
!C** local variables
    integer(kind=kint) :: itype, icel, ic, iS, iE, jS, i, j, k, m, ic_type, nn, ni, ID_area, truss
    real(kind=kreal)   :: estrain(6), estress(6), tstrain(6), naturalCoord(3)
    real(kind=kreal)   :: edstrain(20,6), edstress(20,6), tdstrain(20,6)
    real(kind=kreal)   :: s11, s22, s33, s12, s23, s13, ps, smises
    real(kind=kreal), allocatable :: func(:,:), inv_func(:,:)
    real(kind=kreal), allocatable :: ndstrain(:), ndstress(:)
    real(kind=kreal), allocatable :: trstrain(:,:), trstress(:,:)
    integer(kind=kint), allocatable :: nnumber(:), tnumber(:), snumber(:)
    real(kind = kreal)   :: ecoord(3, 20), edisp(60), tt(20), t0(20)
    integer(kind = kint) :: nodlocal(20), ntemp
!C** Shell33 variables
    integer(kind=kint) :: isect, ihead, ntot_lyr, nlyr, flag33, cid
    real(kind=kreal)   :: thick, thick_lyr, dtot_lyr
    real(kind=kreal)   :: strain(9,6), stress(9,6), estrain_minus(6), estress_minus(6)
    real(kind=kreal), allocatable :: ndstrain_plus(:,:), ndstrain_minus(:,:)
    real(kind=kreal), allocatable :: ndstress_plus(:,:), ndstress_minus(:,:)


    fstrSOLID%STRAIN = 0.0d0
    fstrSOLID%STRESS = 0.0d0
    allocate( nnumber(ppohFEM_get_n_node(idx_mesh)) )
    nnumber = 0
    if( associated(tnstrain) ) tnstrain = 0.0d0

!C** truss and beam setting
    truss = 0
    do itype = 1, ppohFEM_get_n_elem_type(idx_mesh)
      ic_type = ppohFEM_get_elem_type_item(idx_mesh, itype)
      if( ic_type == 301 ) truss = 1
      IF( ic_type == 641 ) truss = 2
    enddo
    if( truss == 1 .or. truss == 2 ) then
      allocate( trstrain(ppohFEM_get_n_node(idx_mesh),6), trstress(ppohFEM_get_n_node(idx_mesh),6) )
      allocate( tnumber(ppohFEM_get_n_node(idx_mesh)) )
      trstrain = 0.0d0
      trstress = 0.0d0
      tnumber = 0
    endif

!C** shell33 setting
    ntot_lyr = 1
    flag33 = 0

    do ic=1, ppohFEM_get_n_sect(idx_mesh)
      cid = ppohFEM_get_sect_mat_ID_item(idx_mesh, ic)
      nlyr=  int(fstrSOLID%materials(cid)%variables(M_TOTAL_LAYER))
      if (nlyr > ntot_lyr)then
        ntot_lyr = nlyr
      endif
    enddo
    do ic=1,ppohFEM_get_n_elem_type(idx_mesh)
      ic_type = ppohFEM_get_elem_type_item(idx_mesh, ic)
      if (ic_type == 761 .or. ic_type == 781)then
        flag33 = 1
      endif
    enddo

    if( flag33 == 1 )then
      allocate ( ndstrain_plus(ppohFEM_get_n_node(idx_mesh),6*ntot_lyr) )
      allocate ( ndstrain_minus(ppohFEM_get_n_node(idx_mesh),6*ntot_lyr) )
      allocate ( ndstress_plus(ppohFEM_get_n_node(idx_mesh),6*ntot_lyr) )
      allocate ( ndstress_minus(ppohFEM_get_n_node(idx_mesh),6*ntot_lyr) )
      allocate ( snumber(ppohFEM_get_n_node(idx_mesh)) )
      allocate ( ndstrain(6*ppohFEM_get_n_node(idx_mesh)) )
      allocate ( ndstress(7*ppohFEM_get_n_node(idx_mesh)) )
      ndstrain_plus  = 0.0d0
      ndstrain_minus = 0.0d0
      ndstress_plus  = 0.0d0
      ndstress_minus = 0.0d0
      estrain_minus  = 0.0d0
      estress_minus  = 0.0d0
      ndstrain = 0.0d0
      ndstress = 0.0d0
      thick_lyr = 0.0d0
      snumber   = 0
    else
      allocate ( ndstrain(6*ppohFEM_get_n_node(idx_mesh)) )
      allocate ( ndstress(7*ppohFEM_get_n_node(idx_mesh)) )
      ndstrain = 0.0d0
      ndstress = 0.0d0
    endif

!C +-------------------------------+
!C | according to ELEMENT TYPE     |
!C +-------------------------------+
    do itype = 1, ppohFEM_get_n_elem_type(idx_mesh)
       iS= ppohFEM_get_elem_type_index(idx_mesh, itype-1) + 1
       iE= ppohFEM_get_elem_type_index(idx_mesh, itype  )
       ic_type= ppohFEM_get_elem_type_item(idx_mesh, itype)
      if( ic_type == fe_tet10nc ) ic_type = fe_tet10n
      if( .not. (ppohFEM_is_etype_solid(ic_type) .or. ic_type == 781 &
      & .or. ic_type == 761 .or. ic_type == fe_beam341 ) ) cycle
!C** set number of nodes and shape function
      nn = ppohFEM_get_max_node_of_elem_type(ic_type)
      ni = ppohFEM_get_num_quadpoints_of_element( ic_type )
      allocate( func(ni,nn), inv_func(nn,ni) )
      if( ic_type == fe_tet10n ) then
        ic = ppohFEM_get_max_node_of_elem_type( fe_tet4n )
        do i = 1, ni
          call ppohFEM_get_quadpoints_of_element( ic_type, i, naturalCoord )
          call ppohFEM_get_shape_func( fe_tet4n, naturalCoord, func(i,1:ic) )
        enddo
        call inverse_func( ic, func, inv_func )
      else if( ic_type == fe_hex8n ) then
        do i = 1, ni
          call ppohFEM_get_quadpoints_of_element( ic_type, i, naturalCoord )
          call ppohFEM_get_shape_func( ic_type, naturalCoord, func(i,1:nn) )
        enddo
        call inverse_func( ni, func, inv_func )
      else if( ic_type == fe_prism15n ) then
        ic = 0
        do i = 1, ni
          if( i==1 .or. i==2 .or. i==3 .or. i==7 .or. i==8 .or. i==9 ) then
            ic = ic + 1
            call ppohFEM_get_quadpoints_of_element( ic_type, i, naturalCoord )
            call ppohFEM_get_shape_func( fe_prism6n, naturalCoord, func(ic,1:6) )
          endif
        enddo
        call inverse_func( ic, func, inv_func )
        ni = ic
      else if( ic_type == fe_hex20n ) then
        ic = 0
        do i = 1, ni
          if( i==1 .or. i==3 .or. i==7 .or. i==9 .or. &
              i==19 .or. i==21 .or. i==25 .or. i==27 ) then
            ic = ic + 1
            call ppohFEM_get_quadpoints_of_element( ic_type, i, naturalCoord )
            call ppohFEM_get_shape_func( fe_hex8n, naturalCoord, func(ic,1:8) )
           endif
        enddo
        call inverse_func( ic, func, inv_func )
        ni = ic
      endif
!C** element loop
      do icel = iS, iE
!        jS = hecMESH%elem_node_index(icel-1) ! DEBUG for shell element. remove later.
        ID_area = ppohFEM_get_rank_assigned_to_element(idx_mesh, icel)
        isect = ppohFEM_get_section_ID(idx_mesh, icel)
        ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
        thick = ppohFEM_get_sect_R_item(idx_mesh, ihead+1)
        !initialize
        estrain  = 0.0d0
        estrain_minus  = 0.0d0
        estress  = 0.0d0
        estress_minus  = 0.0d0
!--- calculate nodal stress and strain
        if( ic_type == 641 ) then !<3*3 beam section
write(*,*)'beam output is currently not supported'
call ppohFEM_abort

!          do j = 1, 4
!            nodLOCAL(j) = hecMESH%elem_node_item(jS+j)
!            ecoord(1:3,j)    = hecMESH%node(3*nodLOCAL(j)-2:3*nodLOCAL(j))
!            edisp(3*j-2:3*j) = fstrSOLID%unode(3*nodLOCAL(j)-2:3*nodLOCAL(j))
!          end do
!          ntemp = 0
!          if( associated( fstrSOLID%temperature ) ) then
!            ntemp = 1
!            do j = 1, 4
!              nodLOCAL(j) = hecMESH%elem_node_item(jS+j)
!              t0(j) = fstrSOLID%last_temp( nodLOCAL(j) )
!              tt(j) = fstrSOLID%temperature( nodLOCAL(j) )
!            end do
!          end if
!          call NodalStress_Beam_641( ic_type, nn, ecoord, fstrSOLID%elements(icel)%gausses, &
!          &     hecMESH%section%sect_R_item(ihead+1:), edisp,                               &
!          &     edstrain(1:nn,1:6), edstress(1:nn,1:6), tt(1:nn), t0(1:nn), ntemp )
!          estrain(1:6) = fstrSOLID%elements(icel)%gausses(1)%strain(1:6)
!          estress(1:6) = fstrSOLID%elements(icel)%gausses(1)%stress(1:6)

        elseif( ic_type == 781) then !<3*3 shell section
write(*,*)'3*3 shell output is currently not supported'
call ppohFEM_abort
!          do j = 1, 4
!            nodLOCAL(j  ) = hecMESH%elem_node_item(jS+j  )
!            nodLOCAL(j+4) = hecMESH%elem_node_item(jS+j+4)
!            ecoord(1:3,j  ) = hecMESH%node(3*nodLOCAL(j  )-2:3*nodLOCAL(j  ))
!            ecoord(1:3,j+4) = hecMESH%node(3*nodLOCAL(j+4)-2:3*nodLOCAL(j+4))
!            edisp(6*j-5:6*j-3) = fstrSOLID%unode(3*nodLOCAL(j  )-2:3*nodLOCAL(j  ))
!            edisp(6*j-2:6*j  ) = fstrSOLID%unode(3*nodLOCAL(j+4)-2:3*nodLOCAL(j+4))
!          enddo
!          dtot_lyr = fstrSOLID%elements(icel)%gausses(1)%pMaterial%variables(M_TOTAL_LAYER)
!          ntot_lyr = int(dtot_lyr)
!          DO nlyr=1,ntot_lyr
!            call ElementStress_Shell_MITC( 741, 4, 6, ecoord, fstrSOLID%elements(icel)%gausses, edisp, &
!               & strain, stress, thick, 1.0d0, nlyr, ntot_lyr)
!            do j = 1, 4
!              i = nodLOCAL(j)
!              m = nodLOCAL(j+4)
!              do k = 1, 6
!                ndstrain_plus(i,6*(nlyr-1)+k) = ndstrain_plus(i,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_plus(i,6*(nlyr-1)+k) = ndstress_plus(i,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!                ndstrain_plus(m,6*(nlyr-1)+k) = ndstrain_plus(m,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_plus(m,6*(nlyr-1)+k) = ndstress_plus(m,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!              enddo
!            enddo
!            do j = 1, 4
!              do k = 1, 6
!                estrain(k) = estrain(k) + strain(j,k)/(4*dtot_lyr)
!                estress(k) = estress(k) + stress(j,k)/(4*dtot_lyr)
!              enddo
!            enddo
!            !minus section
!            call ElementStress_Shell_MITC( 741, 4, 6, ecoord, fstrSOLID%elements(icel)%gausses, edisp, &
!               & strain, stress, thick, -1.0d0, nlyr, ntot_lyr)
!            do j = 1, 4
!              i = nodLOCAL(j)
!              m = nodLOCAL(j+4)
!              do k = 1, 6
!                ndstrain_minus(i,6*(nlyr-1)+k) = ndstrain_minus(i,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_minus(i,6*(nlyr-1)+k) = ndstress_minus(i,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!                ndstrain_minus(m,6*(nlyr-1)+k) = ndstrain_minus(m,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_minus(m,6*(nlyr-1)+k) = ndstress_minus(m,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!              enddo
!            enddo
!            do j = 1, 4
!              do k = 1, 6
!                estrain_minus(k) = estrain_minus(k) + strain(j,k)/(4*dtot_lyr)
!                estress_minus(k) = estress_minus(k) + stress(j,k)/(4*dtot_lyr)
!              enddo
!            enddo
!          enddo

        elseif( ic_type == 761) then !<3*3 shell section
write(*,*)'3*3 shell output is currently not supported'
call ppohFEM_abort
!          do j = 1, 3
!            nodLOCAL(j  ) = hecMESH%elem_node_item(jS+j  )
!            nodLOCAL(j+3) = hecMESH%elem_node_item(jS+j+3)
!            ecoord(1:3,j  ) = hecMESH%node(3*nodLOCAL(j  )-2:3*nodLOCAL(j  ))
!            ecoord(1:3,j+3) = hecMESH%node(3*nodLOCAL(j+3)-2:3*nodLOCAL(j+3))
!            edisp(6*j-5:6*j-3) = fstrSOLID%unode(3*nodLOCAL(j  )-2:3*nodLOCAL(j  ))
!            edisp(6*j-2:6*j  ) = fstrSOLID%unode(3*nodLOCAL(j+3)-2:3*nodLOCAL(j+3))
!          enddo
!          dtot_lyr = fstrSOLID%elements(icel)%gausses(1)%pMaterial%variables(M_TOTAL_LAYER)
!          ntot_lyr = int(dtot_lyr)
!          DO nlyr=1,ntot_lyr
!            call ElementStress_Shell_MITC( 731, 3, 6, ecoord, fstrSOLID%elements(icel)%gausses, edisp, &
!               & strain, stress, thick, 1.0d0, nlyr, ntot_lyr)
!            do j = 1, 3
!              i = nodLOCAL(j)
!              m = nodLOCAL(j+3)
!              do k = 1, 6
!                ndstrain_plus(i,6*(nlyr-1)+k) = ndstrain_plus(i,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_plus(i,6*(nlyr-1)+k) = ndstress_plus(i,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!                ndstrain_plus(m,6*(nlyr-1)+k) = ndstrain_plus(m,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_plus(m,6*(nlyr-1)+k) = ndstress_plus(m,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!              enddo
!            enddo
!            do j = 1, 3
!              do k = 1, 6
!                estrain(k) = estrain(k) + strain(j,k)/(3*dtot_lyr)
!                estress(k) = estress(k) + stress(j,k)/(3*dtot_lyr)
!              enddo
!            enddo
!            !minus section
!            call ElementStress_Shell_MITC( 731, 3, 6, ecoord, fstrSOLID%elements(icel)%gausses, edisp, &
!               & strain, stress, thick, -1.0d0, nlyr, ntot_lyr)
!            do j = 1, 3
!              i = nodLOCAL(j)
!              m = nodLOCAL(j+3)
!              do k = 1, 6
!                ndstrain_minus(i,6*(nlyr-1)+k) = ndstrain_minus(i,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_minus(i,6*(nlyr-1)+k) = ndstress_minus(i,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!                ndstrain_minus(m,6*(nlyr-1)+k) = ndstrain_minus(m,6*(nlyr-1)+k) + strain(j,k)/dtot_lyr
!                ndstress_minus(m,6*(nlyr-1)+k) = ndstress_minus(m,6*(nlyr-1)+k) + stress(j,k)/dtot_lyr
!              enddo
!            enddo
!            do j = 1, 3
!              do k = 1, 6
!                estrain_minus(k) = estrain_minus(k) + strain(j,k)/(3*dtot_lyr)
!                estress_minus(k) = estress_minus(k) + stress(j,k)/(3*dtot_lyr)
!              enddo
!            enddo
!          enddo

        else if( ic_type == 301 ) then
          call NodalStress_C1( ic_type, nn, fstrSOLID%elements(icel)%gausses, &
                               edstrain(1:nn,1:6), edstress(1:nn,1:6) )

        else if( ic_type == fe_tet10n .or. ic_type == fe_hex8n .or. &
                 ic_type == fe_prism15n .or. ic_type == fe_hex20n ) then
          call NodalStress_INV3( ic_type, ni, fstrSOLID%elements(icel)%gausses, &
                                 inv_func, edstrain(1:nn,1:6), edstress(1:nn,1:6), &
                                 tdstrain(1:nn,1:6) )

        else
          call NodalStress_C3( ic_type, nn, fstrSOLID%elements(icel)%gausses, &
                               edstrain(1:nn,1:6), edstress(1:nn,1:6) )
!          call NodalStress_C3( ic_type, nn, fstrSOLID%elements(icel)%gausses, &
!                               edstrain(1:nn,1:6), edstress(1:nn,1:6), tdstrain(1:nn,1:6) )
        endif
        do j = 1, nn
          ic = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
          if( ic_type == 301 .or. ic_type == 641 ) then
            trstrain(ic,1:6) = trstrain(ic,1:6) + edstrain(j,1:6)
            trstress(ic,1:6) = trstress(ic,1:6) + edstress(j,1:6)
            !if( associated(tnstrain) ) tnstrain(6*ic-5:6*ic) = tnstrain(6*ic-5:6*ic) + tdstrain(j,1:6)
            nnumber(ic) = nnumber(ic) + 1
          elseif( ic_type == 761 .or. ic_type == 781 ) then
            !if( associated(tnstrain) ) tnstrain(6*ic-5:6*ic) = tnstrain(6*ic-5:6*ic) + tdstrain(j,1:6)
            nnumber(ic) = nnumber(ic) + 1
          else
            ndstrain(6*ic-5:6*ic)   = ndstrain(6*ic-5:6*ic)   + edstrain(j,1:6)
            ndstress(7*ic-6:7*ic-1) = ndstress(7*ic-6:7*ic-1) + edstress(j,1:6)
            if( associated(tnstrain) ) tnstrain(6*ic-5:6*ic) = tnstrain(6*ic-5:6*ic) + tdstrain(j,1:6)
            nnumber(ic) = nnumber(ic) + 1
          endif
        enddo

!--- calculate elemental stress and strain
        if( ID_area == ppohFEM_comm_get_rank() ) then
          if( ic_type == 301 ) then
            call ElementStress_C1( ic_type, fstrSOLID%elements(icel)%gausses, estrain, estress )
          elseif( .not. (ic_type == 641 .or. ic_type == 781 .or. ic_type == 761 )) then
            call ElementStress_C3( ic_type, fstrSOLID%elements(icel)%gausses, estrain, estress )
!            call ElementStress_C3( ic_type, fstrSOLID%elements(icel)%gausses, estrain, estress, tstrain )
          endif
          if( flag33 == 1 ) then
            if( ic_type == 761 .or. ic_type == 781)then
              fstrSOLID%ESTRAIN(12*icel-11:12*icel-6) = estrain
              fstrSOLID%ESTRAIN(12*icel-5 :12*icel  ) = estrain_minus
              fstrSOLID%ESTRESS(14*icel-13:14*icel-8) = estress
              fstrSOLID%ESTRESS(14*icel-7 :14*icel-2) = estress_minus
            else
              fstrSOLID%ESTRAIN(12*icel-11:12*icel-6) = estrain
              fstrSOLID%ESTRAIN(12*icel-5 :12*icel  ) = estrain
              fstrSOLID%ESTRESS(14*icel-13:14*icel-8) = estress
              fstrSOLID%ESTRESS(14*icel-7 :14*icel-2) = estress
            endif
            fstrSOLID%ESTRESS(14*icel-1) = get_mises(fstrSOLID%ESTRESS(14*icel-13:14*icel-8))
            fstrSOLID%ESTRESS(14*icel  ) = get_mises(fstrSOLID%ESTRESS(14*icel-7 :14*icel-2))
            !if( associated(testrain) ) testrain(6*icel-5:6*icel) = tstrain
          else
            fstrSOLID%ESTRAIN(6*icel-5:6*icel)   = estrain
            fstrSOLID%ESTRESS(7*icel-6:7*icel-1) = estress
            fstrSOLID%ESTRESS(7*icel) = get_mises(fstrSOLID%ESTRESS(7*icel-6:7*icel-1))
            !if( associated(testrain) ) testrain(6*icel-5:6*icel) = tstrain
          endif
        endif
      enddo
      deallocate( func, inv_func )
    enddo

    if( flag33 == 1 ) then
!C** average over nodes (normal + shell33)
      do i = 1, ppohFEM_get_nn_internal(idx_mesh)
        if( nnumber(i) == 0 ) cycle
        fstrSOLID%STRAIN(12*i-11:12*i-6) = ndstrain(6*i-5:6*i)   / nnumber(i)
        fstrSOLID%STRAIN(12*i-5 :12*i  ) = ndstrain(6*i-5:6*i)   / nnumber(i) !< copy
        fstrSOLID%STRESS(14*i-13:14*i-8) = ndstress(7*i-6:7*i-1) / nnumber(i)
        fstrSOLID%STRESS(14*i-7 :14*i-2) = ndstress(7*i-6:7*i-1) / nnumber(i) !< copy
        if( associated(tnstrain) ) tnstrain(6*i-5:6*i) = tnstrain(6*i-5:6*i) / nnumber(i)
      enddo
      !C** average over nodes (truss + shell33)
      if( truss == 1 ) then
        do i = 1, ppohFEM_get_nn_internal(idx_mesh)
          if( nnumber(i) == 0 ) cycle
          fstrSOLID%STRAIN(12*i-11:12*i-6) = fstrSOLID%STRAIN(12*i-11:12*i-6) + trstrain(i,1:6) / nnumber(i)
          fstrSOLID%STRAIN(12*i-5 :12*i  ) = fstrSOLID%STRAIN(12*i-5 :12*i  ) + trstrain(i,1:6) / nnumber(i) !< copy
          fstrSOLID%STRESS(14*i-13:14*i-8) = fstrSOLID%STRESS(14*i-13:14*i-8) + trstress(i,1:6) / nnumber(i)
          fstrSOLID%STRESS(14*i-7 :14*i-2) = fstrSOLID%STRESS(14*i-7 :14*i-2) + trstress(i,1:6) / nnumber(i) !< copy
        enddo
      endif
      !C** average over nodes (shell33)
      do i = 1, ppohFEM_get_nn_internal(idx_mesh)
        if( nnumber(i) == 0 ) cycle
        fstrSOLID%STRAIN(12*i-11:12*i-6) = fstrSOLID%STRAIN(12*i-11:12*i-6) + ndstrain_plus(i,1:6)  / nnumber(i)
        fstrSOLID%STRAIN(12*i-5 :12*i  ) = fstrSOLID%STRAIN(12*i-5 :12*i  ) + ndstrain_minus(i,1:6) / nnumber(i)
        fstrSOLID%STRESS(14*i-13:14*i-8) = fstrSOLID%STRESS(14*i-13:14*i-8) + ndstress_plus(i,1:6)  / nnumber(i)
        fstrSOLID%STRESS(14*i-7 :14*i-2) = fstrSOLID%STRESS(14*i-7 :14*i-2) + ndstress_minus(i,1:6) / nnumber(i)
      enddo

    else
!C** average over nodes (normal)
      do i = 1, ppohFEM_get_nn_internal(idx_mesh)
        if( nnumber(i) == 0 ) cycle
        fstrSOLID%STRAIN(6*i-5:6*i)   = ndstrain(6*i-5:6*i)   / nnumber(i)
        fstrSOLID%STRESS(7*i-6:7*i-1) = ndstress(7*i-6:7*i-1) / nnumber(i)
        if( associated(tnstrain) ) tnstrain(6*i-5:6*i) = tnstrain(6*i-5:6*i) / nnumber(i)
      enddo
      !C** average over nodes (truss)
      if( truss == 1 ) then
        do i = 1, ppohFEM_get_nn_internal(idx_mesh)
          if( nnumber(i) == 0 ) cycle
          fstrSOLID%STRAIN(6*i-5:6*i)   = fstrSOLID%STRAIN(6*i-5:6*i)   + trstrain(i,1:6) / nnumber(i)
          fstrSOLID%STRESS(7*i-6:7*i-1) = fstrSOLID%STRESS(7*i-6:7*i-1) + trstress(i,1:6) / nnumber(i)
        enddo
      endif
    endif

!C** calculate von MISES stress
    if( flag33 == 1 ) then
      do i = 1, ppohFEM_get_n_node(idx_mesh)
        fstrSOLID%STRESS(14*i-1) = get_mises(fstrSOLID%STRESS(14*i-13:14*i-8))
        fstrSOLID%STRESS(14*i  ) = get_mises(fstrSOLID%STRESS(14*i-7 :14*i-2))
      enddo
    else
      do i = 1, ppohFEM_get_n_node(idx_mesh)
        fstrSOLID%STRESS(7*i) = get_mises(fstrSOLID%STRESS(7*i-6:7*i-1))
      enddo
    endif

    deallocate( nnumber )
    if( truss == 1 ) then
      deallocate( trstrain, trstress )
      deallocate( tnumber )
    endif
    if( flag33 == 1 ) then
      deallocate( ndstrain_plus, ndstrain_minus )
      deallocate( ndstress_plus, ndstress_minus )
      deallocate( snumber )
    endif

  end subroutine fstr_NodalStress3D
  !----------------------------------------------------------------------*
  subroutine NodalStress_INV3( etype, ni, gausses, func, edstrain, edstress, tdstrain )
    !----------------------------------------------------------------------*
    use m_fstr
    use mMechGauss
    integer(kind=kint) :: etype, ni
    type(tGaussStatus) :: gausses(:)
    real(kind=kreal)   :: func(:,:), edstrain(:,:), edstress(:,:), tdstrain(:,:)
    integer :: i, j, k, ic

    edstrain = 0.0d0
    edstress = 0.0d0
    tdstrain = 0.0d0

    if( etype == fe_hex8n ) then
      do i = 1, ni
        do j = 1, ni
          do k = 1, 6
            edstrain(i,k) = edstrain(i,k) + func(i,j) * gausses(j)%strain(k)
            edstress(i,k) = edstress(i,k) + func(i,j) * gausses(j)%stress(k)
            !            tdstrain(i,k) = tdstrain(i,k) + func(i,j) * gausses(j)%tstrain(k)
          enddo
        enddo
      enddo
    else if( etype == fe_tet10n ) then
      do i = 1, ni
        do j = 1, ni
          do k = 1, 6
            edstrain(i,k) = edstrain(i,k) + func(i,j) * gausses(j)%strain(k)
            edstress(i,k) = edstress(i,k) + func(i,j) * gausses(j)%stress(k)
            !            tdstrain(i,k) = tdstrain(i,k) + func(i,j) * gausses(j)%tstrain(k)
          enddo
        enddo
      enddo
      edstrain(5,1:6) = ( edstrain(1,1:6) + edstrain(2,1:6) ) / 2.0
      edstress(5,1:6) = ( edstress(1,1:6) + edstress(2,1:6) ) / 2.0
      tdstrain(5,1:6) = ( tdstrain(1,1:6) + tdstrain(2,1:6) ) / 2.0
      edstrain(6,1:6) = ( edstrain(2,1:6) + edstrain(3,1:6) ) / 2.0
      edstress(6,1:6) = ( edstress(2,1:6) + edstress(3,1:6) ) / 2.0
      tdstrain(6,1:6) = ( tdstrain(2,1:6) + tdstrain(3,1:6) ) / 2.0
      edstrain(7,1:6) = ( edstrain(3,1:6) + edstrain(1,1:6) ) / 2.0
      edstress(7,1:6) = ( edstress(3,1:6) + edstress(1,1:6) ) / 2.0
      tdstrain(7,1:6) = ( tdstrain(3,1:6) + tdstrain(1,1:6) ) / 2.0
      edstrain(8,1:6) = ( edstrain(1,1:6) + edstrain(4,1:6) ) / 2.0
      edstress(8,1:6) = ( edstress(1,1:6) + edstress(4,1:6) ) / 2.0
      tdstrain(8,1:6) = ( tdstrain(1,1:6) + tdstrain(4,1:6) ) / 2.0
      edstrain(9,1:6) = ( edstrain(2,1:6) + edstrain(4,1:6) ) / 2.0
      edstress(9,1:6) = ( edstress(2,1:6) + edstress(4,1:6) ) / 2.0
      tdstrain(9,1:6) = ( tdstrain(2,1:6) + tdstrain(4,1:6) ) / 2.0
      edstrain(10,1:6) = ( edstrain(3,1:6) + edstrain(4,1:6) ) / 2.0
      edstress(10,1:6) = ( edstress(3,1:6) + edstress(4,1:6) ) / 2.0
      tdstrain(10,1:6) = ( tdstrain(3,1:6) + tdstrain(4,1:6) ) / 2.0
    else if( etype == fe_prism15n ) then
      do i = 1, ni
        ic = 0
        do j = 1, NumOfQuadPoints(etype)
          if( j==1 .or. j==2 .or. j==3 .or. j==7 .or. j==8 .or. j==9 ) then
            ic = ic + 1
            do k = 1, 6
              edstrain(i,k) = edstrain(i,k) + func(i,ic) * gausses(j)%strain(k)
              edstress(i,k) = edstress(i,k) + func(i,ic) * gausses(j)%stress(k)
              !              tdstrain(i,k) = tdstrain(i,k) + func(i,ic) * gausses(j)%tstrain(k)
            enddo
          endif
        enddo
      enddo
      edstrain(7,1:6) = ( edstrain(1,1:6) + edstrain(2,1:6) ) / 2.0
      edstress(7,1:6) = ( edstress(1,1:6) + edstress(2,1:6) ) / 2.0
      tdstrain(7,1:6) = ( tdstrain(1,1:6) + tdstrain(2,1:6) ) / 2.0
      edstrain(8,1:6) = ( edstrain(2,1:6) + edstrain(3,1:6) ) / 2.0
      edstress(8,1:6) = ( edstress(2,1:6) + edstress(3,1:6) ) / 2.0
      tdstrain(8,1:6) = ( tdstrain(2,1:6) + tdstrain(3,1:6) ) / 2.0
      edstrain(9,1:6) = ( edstrain(3,1:6) + edstrain(1,1:6) ) / 2.0
      edstress(9,1:6) = ( edstress(3,1:6) + edstress(1,1:6) ) / 2.0
      tdstrain(9,1:6) = ( tdstrain(3,1:6) + tdstrain(1,1:6) ) / 2.0
      edstrain(10,1:6) = ( edstrain(4,1:6) + edstrain(5,1:6) ) / 2.0
      edstress(10,1:6) = ( edstress(4,1:6) + edstress(5,1:6) ) / 2.0
      tdstrain(10,1:6) = ( tdstrain(4,1:6) + tdstrain(5,1:6) ) / 2.0
      edstrain(11,1:6) = ( edstrain(5,1:6) + edstrain(6,1:6) ) / 2.0
      edstress(11,1:6) = ( edstress(5,1:6) + edstress(6,1:6) ) / 2.0
      tdstrain(11,1:6) = ( tdstrain(5,1:6) + tdstrain(6,1:6) ) / 2.0
      edstrain(12,1:6) = ( edstrain(6,1:6) + edstrain(4,1:6) ) / 2.0
      edstress(12,1:6) = ( edstress(6,1:6) + edstress(4,1:6) ) / 2.0
      tdstrain(12,1:6) = ( tdstrain(6,1:6) + tdstrain(4,1:6) ) / 2.0
      edstrain(13,1:6) = ( edstrain(1,1:6) + edstrain(4,1:6) ) / 2.0
      edstress(13,1:6) = ( edstress(1,1:6) + edstress(4,1:6) ) / 2.0
      tdstrain(13,1:6) = ( tdstrain(1,1:6) + tdstrain(4,1:6) ) / 2.0
      edstrain(14,1:6) = ( edstrain(2,1:6) + edstrain(5,1:6) ) / 2.0
      edstress(14,1:6) = ( edstress(2,1:6) + edstress(5,1:6) ) / 2.0
      tdstrain(14,1:6) = ( tdstrain(2,1:6) + tdstrain(5,1:6) ) / 2.0
      edstrain(15,1:6) = ( edstrain(3,1:6) + edstrain(6,1:6) ) / 2.0
      edstress(15,1:6) = ( edstress(3,1:6) + edstress(6,1:6) ) / 2.0
      tdstrain(15,1:6) = ( tdstrain(3,1:6) + tdstrain(6,1:6) ) / 2.0
    else if( etype == fe_hex20n ) then
      do i = 1, ni
        ic = 0
        do j = 1, NumOfQuadPoints(etype)
          if( j==1 .or. j==3 .or. j==7 .or. j==9 .or. &
               j==19 .or. j==21 .or. j==25 .or. j==27 ) then
            ic = ic + 1
            do k = 1, 6
              edstrain(i,k) = edstrain(i,k) + func(i,ic) * gausses(j)%strain(k)
              edstress(i,k) = edstress(i,k) + func(i,ic) * gausses(j)%stress(k)
              !              tdstrain(i,k) = tdstrain(i,k) + func(i,ic) * gausses(j)%tstrain(k)
            enddo
          endif
        enddo
      enddo
      edstrain(9,1:6) = ( edstrain(1,1:6) + edstrain(2,1:6) ) / 2.0
      edstress(9,1:6) = ( edstress(1,1:6) + edstress(2,1:6) ) / 2.0
      tdstrain(9,1:6) = ( tdstrain(1,1:6) + tdstrain(2,1:6) ) / 2.0
      edstrain(10,1:6) = ( edstrain(2,1:6) + edstrain(3,1:6) ) / 2.0
      edstress(10,1:6) = ( edstress(2,1:6) + edstress(3,1:6) ) / 2.0
      tdstrain(10,1:6) = ( tdstrain(2,1:6) + tdstrain(3,1:6) ) / 2.0
      edstrain(11,1:6) = ( edstrain(3,1:6) + edstrain(4,1:6) ) / 2.0
      edstress(11,1:6) = ( edstress(3,1:6) + edstress(4,1:6) ) / 2.0
      tdstrain(11,1:6) = ( tdstrain(3,1:6) + tdstrain(4,1:6) ) / 2.0
      edstrain(12,1:6) = ( edstrain(4,1:6) + edstrain(1,1:6) ) / 2.0
      edstress(12,1:6) = ( edstress(4,1:6) + edstress(1,1:6) ) / 2.0
      tdstrain(12,1:6) = ( tdstrain(4,1:6) + tdstrain(1,1:6) ) / 2.0
      edstrain(13,1:6) = ( edstrain(5,1:6) + edstrain(6,1:6) ) / 2.0
      edstress(13,1:6) = ( edstress(5,1:6) + edstress(6,1:6) ) / 2.0
      tdstrain(13,1:6) = ( tdstrain(5,1:6) + tdstrain(6,1:6) ) / 2.0
      edstrain(14,1:6) = ( edstrain(6,1:6) + edstrain(7,1:6) ) / 2.0
      edstress(14,1:6) = ( edstress(6,1:6) + edstress(7,1:6) ) / 2.0
      tdstrain(14,1:6) = ( tdstrain(6,1:6) + tdstrain(7,1:6) ) / 2.0
      edstrain(15,1:6) = ( edstrain(7,1:6) + edstrain(8,1:6) ) / 2.0
      edstress(15,1:6) = ( edstress(7,1:6) + edstress(8,1:6) ) / 2.0
      tdstrain(15,1:6) = ( tdstrain(7,1:6) + tdstrain(8,1:6) ) / 2.0
      edstrain(16,1:6) = ( edstrain(8,1:6) + edstrain(5,1:6) ) / 2.0
      edstress(16,1:6) = ( edstress(8,1:6) + edstress(5,1:6) ) / 2.0
      tdstrain(16,1:6) = ( tdstrain(8,1:6) + tdstrain(5,1:6) ) / 2.0
      edstrain(17,1:6) = ( edstrain(1,1:6) + edstrain(5,1:6) ) / 2.0
      edstress(17,1:6) = ( edstress(1,1:6) + edstress(5,1:6) ) / 2.0
      tdstrain(17,1:6) = ( tdstrain(1,1:6) + tdstrain(5,1:6) ) / 2.0
      edstrain(18,1:6) = ( edstrain(2,1:6) + edstrain(6,1:6) ) / 2.0
      edstress(18,1:6) = ( edstress(2,1:6) + edstress(6,1:6) ) / 2.0
      tdstrain(18,1:6) = ( tdstrain(2,1:6) + tdstrain(6,1:6) ) / 2.0
      edstrain(19,1:6) = ( edstrain(3,1:6) + edstrain(7,1:6) ) / 2.0
      edstress(19,1:6) = ( edstress(3,1:6) + edstress(7,1:6) ) / 2.0
      tdstrain(19,1:6) = ( tdstrain(3,1:6) + tdstrain(7,1:6) ) / 2.0
      edstrain(20,1:6) = ( edstrain(4,1:6) + edstrain(8,1:6) ) / 2.0
      edstress(20,1:6) = ( edstress(4,1:6) + edstress(8,1:6) ) / 2.0
      tdstrain(20,1:6) = ( tdstrain(4,1:6) + tdstrain(8,1:6) ) / 2.0
    endif
  end subroutine NodalStress_INV3

  function get_mises(s)
    use m_fstr
    implicit none
    real(kind=kreal) :: get_mises, s(1:6)
    real(kind=kreal) :: s11, s22, s33, s12, s23, s13, ps, smises

    s11 = s(1)
    s22 = s(2)
    s33 = s(3)
    s12 = s(4)
    s23 = s(5)
    s13 = s(6)
    ps = ( s11 + s22 + s33 ) / 3.0d0
    smises = 0.5d0 * ( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2 + s13**2
    get_mises = dsqrt( 3.0d0 * smises )

  end function get_mises

  !> Calculate NODAL STRESS of plane elements
  !----------------------------------------------------------------------*
  subroutine fstr_NodalStress2D( hecMESH, fstrSOLID, tnstrain, testrain )
    !----------------------------------------------------------------------*
    use m_fstr
    use m_static_lib
    type (hecmwST_local_mesh) :: hecMESH
    type (fstr_solid)         :: fstrSOLID
    real(kind=kreal), pointer :: tnstrain(:), testrain(:)
    !C** local variables
    integer(kind=kint) :: itype, icel, ic, iS, iE, jS, i, j, ic_type, nn, ni, ID_area
    real(kind=kreal)   :: estrain(4), estress(4), tstrain(4), naturalCoord(4)
    real(kind=kreal)   :: edstrain(8,4), edstress(8,4), tdstrain(8,4)
    real(kind=kreal)   :: s11, s22, s33, s12, s23, s13, ps, smises
    real(kind=kreal), allocatable :: func(:,:), inv_func(:,:)
    integer(kind=kint), allocatable :: nnumber(:)

    fstrSOLID%STRAIN = 0.0d0
    fstrSOLID%STRESS = 0.0d0
    allocate( nnumber(hecMESH%n_node) )
    nnumber = 0

    !C +-------------------------------+
    !C | according to ELEMENT TYPE     |
    !C +-------------------------------+
    do itype = 1, hecMESH%n_elem_type
      iS = hecMESH%elem_type_index(itype-1) + 1
      iE = hecMESH%elem_type_index(itype  )
      ic_type = hecMESH%elem_type_item(itype)
      if( .not. hecmw_is_etype_surface(ic_type) ) cycle
      !C** set number of nodes and shape function
      nn = hecmw_get_max_node( ic_type )
      ni = NumOfQuadPoints( ic_type )
      allocate( func(ni,nn), inv_func(nn,ni) )
      if( ic_type == fe_tri6n ) then
        ic = hecmw_get_max_node( fe_tri3n )
        do i = 1, ni
          call getQuadPoint( ic_type, i, naturalCoord )
          call getShapeFunc( fe_tri3n, naturalCoord, func(i,1:ic) )
        enddo
        call inverse_func( ic, func, inv_func )
      else if( ic_type == fe_quad4n ) then
        do i = 1, ni
          call getQuadPoint( ic_type, i, naturalCoord )
          call getShapeFunc( ic_type, naturalCoord, func(i,1:nn) )
        enddo
        call inverse_func( ni, func, inv_func )
      else if( ic_type == fe_quad8n ) then
        ic = 0
        do i = 1, ni
          if( i==1 .or. i==3 .or. i==7 .or. i==9 ) then
            ic = ic + 1
            call getQuadPoint( ic_type, i, naturalCoord )
            call getShapeFunc( fe_quad4n, naturalCoord, func(ic,1:4) )
          endif
        enddo
        call inverse_func( ic, func, inv_func )
        ni = ic
      endif
      !C** element loop
      do icel = iS, iE
        jS = hecMESH%elem_node_index(icel-1)
        ID_area = hecMESH%elem_ID(icel*2)
        !--- calculate nodal stress and strain
        if( ic_type == fe_tri6n .or. ic_type == fe_quad4n .or. ic_type == fe_quad8n ) then
          call NodalStress_INV2( ic_type, ni, fstrSOLID%elements(icel)%gausses, &
               inv_func, edstrain(1:nn,1:4), edstress(1:nn,1:4), &
               tdstrain(1:nn,1:4) )
        else
          call NodalStress_C2( ic_type, nn, fstrSOLID%elements(icel)%gausses, &
               edstrain(1:nn,1:4), edstress(1:nn,1:4) )
          !          call NodalStress_C2( ic_type, nn, fstrSOLID%elements(icel)%gausses, &
          !                               edstrain(1:nn,1:4), edstress(1:nn,1:4), tdstrain(1:nn,1:4) )
        endif
        do j = 1, nn
          ic = hecMESH%elem_node_item(jS+j)
          fstrSOLID%STRAIN(6*ic-5:6*ic-2) = fstrSOLID%STRAIN(6*ic-5:6*ic-2) + edstrain(j,1:4)
          fstrSOLID%STRESS(7*ic-6:7*ic-3) = fstrSOLID%STRESS(7*ic-6:7*ic-3) + edstress(j,1:4)
          if( associated(tnstrain) ) then
            tnstrain(3*ic-2) = tnstrain(3*ic-2) + tdstrain(j,1)
            tnstrain(3*ic-1) = tnstrain(3*ic-1) + tdstrain(j,2)
            tnstrain(3*ic  ) = tnstrain(3*ic  ) + tdstrain(j,4)
          endif
          nnumber(ic) = nnumber(ic) + 1
        enddo
        !--- calculate elemental stress and strain
        if( ID_area == hecMESH%my_rank ) then
          call ElementStress_C2( ic_type, fstrSOLID%elements(icel)%gausses, estrain, estress )
          !          call ElementStress_C2( ic_type, fstrSOLID%elements(icel)%gausses, estrain, estress, tstrain )
          fstrSOLID%ESTRAIN(6*icel-5:6*icel-2) = estrain
          fstrSOLID%ESTRESS(7*icel-6:7*icel-3) = estress
          if( associated(testrain) ) then
            testrain(3*icel-2) = tstrain(1)
            testrain(3*icel-1) = tstrain(2)
            testrain(3*icel  ) = tstrain(4)
          endif
          s11 = fstrSOLID%ESTRESS(7*icel-6)
          s22 = fstrSOLID%ESTRESS(7*icel-5)
          s33 = fstrSOLID%ESTRESS(7*icel-4)
          s12 = fstrSOLID%ESTRESS(7*icel-3)
          s23 = 0.0d0
          s13 = 0.0d0
          ps = ( s11 + s22 + s33 ) / 3.0
          smises = 0.5d0 * ( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2 + s13**2
          fstrSOLID%ESTRESS(7*icel) = sqrt( 3.0d0 * smises )
        endif
      enddo
      deallocate( func, inv_func )
    enddo

    !C** average over nodes
    do i = 1, hecMESH%n_node
      if( nnumber(i) == 0 ) cycle
      fstrSOLID%STRAIN(6*i-5:6*i-2) = fstrSOLID%STRAIN(6*i-5:6*i-2) / nnumber(i)
      fstrSOLID%STRESS(7*i-6:7*i-3) = fstrSOLID%STRESS(7*i-6:7*i-3) / nnumber(i)
      if( associated(tnstrain) ) tnstrain(3*i-2:3*i) = tnstrain(3*i-2:3*i) / nnumber(i)
    enddo
    !C** calculate von MISES stress
    do i = 1, hecMESH%n_node
      s11 = fstrSOLID%STRESS(7*i-6)
      s22 = fstrSOLID%STRESS(7*i-5)
      s33 = fstrSOLID%STRESS(7*i-4)
      s12 = fstrSOLID%STRESS(7*i-3)
      s23 = 0.0d0
      s13 = 0.0d0
      ps = ( s11 + s22 + s33 ) / 3.0
      smises = 0.5d0 *( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2+ s13**2
      fstrSOLID%STRESS(7*i) = sqrt( 3.0d0 * smises )
    enddo
    !C** set array
    do i = 1, hecMESH%n_node
      fstrSOLID%STRAIN(6*(i-1)+3) = fstrSOLID%STRAIN(6*(i-1)+4)
      fstrSOLID%STRAIN(6*(i-1)+4) = 0.0d0
      fstrSOLID%STRESS(7*(i-1)+3) = fstrSOLID%STRESS(7*(i-1)+4)
      fstrSOLID%STRESS(7*(i-1)+4) = fstrSOLID%STRESS(7*i)
      fstrSOLID%STRESS(7*i) = 0.0d0
    enddo
    do i = 1, hecMESH%n_elem
      fstrSOLID%ESTRAIN(6*(i-1)+3) = fstrSOLID%ESTRAIN(6*(i-1)+4)
      fstrSOLID%ESTRAIN(6*(i-1)+4) = 0.0d0
      fstrSOLID%ESTRESS(7*(i-1)+3) = fstrSOLID%ESTRESS(7*(i-1)+4)
      fstrSOLID%ESTRESS(7*(i-1)+4) = fstrSOLID%ESTRESS(7*i)
      fstrSOLID%ESTRESS(7*i) = 0.0d0
    enddo

    deallocate( nnumber )
  end subroutine fstr_NodalStress2D

  !----------------------------------------------------------------------*
  subroutine NodalStress_INV2( etype, ni, gausses, func, edstrain, edstress, tdstrain )
    !----------------------------------------------------------------------*
    use m_fstr
    use mMechGauss
    integer(kind=kint) :: etype, ni
    type(tGaussStatus) :: gausses(:)
    real(kind=kreal)   :: func(:,:), edstrain(:,:), edstress(:,:), tdstrain(:,:)
    integer :: i, j, k, ic

    edstrain = 0.0d0
    edstress = 0.0d0
    tdstrain = 0.0d0

    if( etype == fe_quad4n ) then
      do i = 1, ni
        do j = 1, ni
          do k = 1, 4
            edstrain(i,k) = edstrain(i,k) + func(i,j) * gausses(j)%strain(k)
            edstress(i,k) = edstress(i,k) + func(i,j) * gausses(j)%stress(k)
            !            tdstrain(i,k) = tdstrain(i,k) + func(i,j) * gausses(j)%tstrain(k)
          enddo
        enddo
      enddo
    else if( etype == fe_tri6n ) then
      do i = 1, ni
        do j = 1, ni
          do k = 1, 4
            edstrain(i,k) = edstrain(i,k) + func(i,j) * gausses(j)%strain(k)
            edstress(i,k) = edstress(i,k) + func(i,j) * gausses(j)%stress(k)
            !            tdstrain(i,k) = tdstrain(i,k) + func(i,j) * gausses(j)%tstrain(k)
          enddo
        enddo
      enddo
      edstrain(4,1:4) = ( edstrain(1,1:4) + edstrain(2,1:4) ) / 2.0
      edstress(4,1:4) = ( edstress(1,1:4) + edstress(2,1:4) ) / 2.0
      tdstrain(4,1:4) = ( tdstrain(1,1:4) + tdstrain(2,1:4) ) / 2.0
      edstrain(5,1:4) = ( edstrain(2,1:4) + edstrain(3,1:4) ) / 2.0
      edstress(5,1:4) = ( edstress(2,1:4) + edstress(3,1:4) ) / 2.0
      tdstrain(5,1:4) = ( tdstrain(2,1:4) + tdstrain(3,1:4) ) / 2.0
      edstrain(6,1:4) = ( edstrain(3,1:4) + edstrain(1,1:4) ) / 2.0
      edstress(6,1:4) = ( edstress(3,1:4) + edstress(1,1:4) ) / 2.0
      tdstrain(6,1:4) = ( tdstrain(3,1:4) + tdstrain(1,1:4) ) / 2.0
    else if( etype == fe_quad8n ) then
      do i = 1, ni
        ic = 0
        do j = 1, NumOfQuadPoints(etype)
          if( j==1 .or. j==3 .or. j==7 .or. j==9 ) then
            ic = ic + 1
            do k = 1, 4
              edstrain(i,k) = edstrain(i,k) + func(i,ic) * gausses(j)%strain(k)
              edstress(i,k) = edstress(i,k) + func(i,ic) * gausses(j)%stress(k)
              !              tdstrain(i,k) = tdstrain(i,k) + func(i,ic) * gausses(j)%tstrain(k)
            enddo
          endif
        enddo
      enddo
      edstrain(5,1:4) = ( edstrain(1,1:4) + edstrain(2,1:4) ) / 2.0
      edstress(5,1:4) = ( edstress(1,1:4) + edstress(2,1:4) ) / 2.0
      tdstrain(5,1:4) = ( tdstrain(1,1:4) + tdstrain(2,1:4) ) / 2.0
      edstrain(6,1:4) = ( edstrain(2,1:4) + edstrain(3,1:4) ) / 2.0
      edstress(6,1:4) = ( edstress(2,1:4) + edstress(3,1:4) ) / 2.0
      tdstrain(6,1:4) = ( tdstrain(2,1:4) + tdstrain(3,1:4) ) / 2.0
      edstrain(7,1:4) = ( edstrain(3,1:4) + edstrain(4,1:4) ) / 2.0
      edstress(7,1:4) = ( edstress(3,1:4) + edstress(4,1:4) ) / 2.0
      tdstrain(7,1:4) = ( tdstrain(3,1:4) + tdstrain(4,1:4) ) / 2.0
      edstrain(8,1:4) = ( edstrain(4,1:4) + edstrain(1,1:4) ) / 2.0
      edstress(8,1:4) = ( edstress(4,1:4) + edstress(1,1:4) ) / 2.0
      tdstrain(8,1:4) = ( tdstrain(4,1:4) + tdstrain(1,1:4) ) / 2.0
    endif
  end subroutine NodalStress_INV2

  !----------------------------------------------------------------------*
  subroutine inverse_func( n, a, inv_a )
    !----------------------------------------------------------------------*
    use m_fstr
    integer(kind=kint) :: n
    real(kind=kreal)   :: a(:,:), inv_a(:,:)
    integer(kind=kint) :: i, j, k
    real(kind=kreal)   :: buf

    do i = 1, n
      do j = 1, n
        if( i == j ) then
          inv_a(i,j) = 1.0
        else
          inv_a(i,j) = 0.0
        endif
      enddo
    enddo

    do i = 1, n
      buf = 1.0 / a(i,i)
      do j = 1, n
        a(i,j) = a(i,j) * buf
        inv_a(i,j) = inv_a(i,j) *buf
      enddo
      do j = 1, n
        if( i /= j ) then
          buf = a(j,i)
          do k = 1, n
            a(j,k) = a(j,k) - a(i,k) * buf
            inv_a(j,k) = inv_a(j,k) - inv_a(i,k) * buf
          enddo
        endif
      enddo
    enddo
  end subroutine inverse_func

  !> Calculate NODAL STRESS of shell elements
  !----------------------------------------------------------------------*
  subroutine fstr_NodalStress6D( hecMESH, fstrSOLID )
    !----------------------------------------------------------------------*
    use m_fstr
    use m_static_lib
    type (hecmwST_local_mesh) :: hecMESH
    type (fstr_solid)         :: fstrSOLID
    !C** local variables
    integer(kind=kint) :: itype, icel, iS, iE, jS, i, j, k, it, ic_type, nn, isect, ihead, ID_area
    integer(kind=kint) :: nodLOCAL(9), n_layer, n_total_layer, com_total_layer, shellmatl
    real(kind=kreal)   :: ecoord(3,9), edisp(6,9), strain(9,6), stress(9,6)
    real(kind=kreal)   :: thick, thick_layer
    real(kind=kreal)   :: s11, s22, s33, s12, s23, s13, t11, t22, t33, t12, t23, t13, ps, smises, tmises
    real(kind=kreal), allocatable :: ndstrain_plus(:,:), ndstrain_minus(:,:)
    real(kind=kreal), allocatable :: ndstress_plus(:,:), ndstress_minus(:,:)
    integer(kind=kint), allocatable :: nnumber(:)

    fstrSOLID%ESTRAIN = 0.0d0
    fstrSOLID%ESTRESS = 0.0d0
    n_total_layer = 1

    do it = 1, hecMESH%material%n_mat
      com_total_layer =  int(fstrSOLID%materials(it)%variables(M_TOTAL_LAYER))
      if (com_total_layer >= n_total_layer)then
        n_total_layer = com_total_layer
      endif
    enddo

    allocate ( ndstrain_plus(hecMESH%n_node,6*n_total_layer) )
    allocate ( ndstrain_minus(hecMESH%n_node,6*n_total_layer) )
    allocate ( ndstress_plus(hecMESH%n_node,6*n_total_layer) )
    allocate ( ndstress_minus(hecMESH%n_node,6*n_total_layer) )
    allocate ( nnumber(hecMESH%n_node) )
    ndstrain_plus = 0.0d0
    ndstrain_minus = 0.0d0
    ndstress_plus = 0.0d0
    ndstress_minus = 0.0d0
    nnumber = 0
    thick_layer = 0.0d0

    !C +-------------------------------+
    !C | according to ELEMENT TYPE     |
    !C +-------------------------------+
    do itype = 1, hecMESH%n_elem_type
      iS = hecMESH%elem_type_index(itype-1) + 1
      iE = hecMESH%elem_type_index(itype  )
      ic_type = hecMESH%elem_type_item(itype)
      if( .not. hecmw_is_etype_shell(ic_type) ) cycle
      nn = hecmw_get_max_node( ic_type )
      !C** element loop
      do icel = iS, iE
        jS = hecMESH%elem_node_index(icel-1)
        ID_area = hecMESH%elem_ID(icel*2)
        do j = 1, nn
          nodLOCAL(j) = hecMESH%elem_node_item(jS+j)
          ecoord(1,j) = hecMESH%node(3*nodLOCAL(j)-2)
          ecoord(2,j) = hecMESH%node(3*nodLOCAL(j)-1)
          ecoord(3,j) = hecMESH%node(3*nodLOCAL(j)  )
          edisp(1,j) = fstrSOLID%unode(6*nodLOCAL(j)-5)
          edisp(2,j) = fstrSOLID%unode(6*nodLOCAL(j)-4)
          edisp(3,j) = fstrSOLID%unode(6*nodLOCAL(j)-3)
          edisp(4,j) = fstrSOLID%unode(6*nodLOCAL(j)-2)
          edisp(5,j) = fstrSOLID%unode(6*nodLOCAL(j)-1)
          edisp(6,j) = fstrSOLID%unode(6*nodLOCAL(j)  )
        enddo
        isect = hecMESH%section_ID(icel)
        ihead = hecMESH%section%sect_R_index(isect-1)
        thick = hecMESH%section%sect_R_item(ihead+1)
        !--- calculate elemental stress and strain
        if( ic_type == 731 .or. ic_type == 741 .or. ic_type == 743 ) then
          n_total_layer =  int(fstrSOLID%elements(icel)%gausses(1)%pMaterial%variables(M_TOTAL_LAYER))
          DO n_layer=1,n_total_layer
            call ElementStress_Shell_MITC( ic_type, nn, 6, ecoord(1:3,1:nn), fstrSOLID%elements(icel)%gausses, &
                 edisp(1:6,1:nn), strain, stress, thick, 1.0d0, n_layer, n_total_layer)
            do j = 1, nn
              i = nodLOCAL(j)
              do k = 1, 6
                ndstrain_plus(i,6*(n_layer-1)+k) = ndstrain_plus(i,6*(n_layer-1)+k) + strain(j,k)
                ndstress_plus(i,6*(n_layer-1)+k) = ndstress_plus(i,6*(n_layer-1)+k) + stress(j,k)
              enddo
            enddo
            if( ID_area == hecMESH%my_rank ) then
              do j = 1, nn
                do k = 1, 6
                  fstrSOLID%ESTRAIN(12*n_total_layer*(icel-1)+12*(n_layer-1)+k) = &
                       fstrSOLID%ESTRAIN(12*n_total_layer*(icel-1)+12*(n_layer-1)+k) + strain(j,k)/nn
                  fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+k) = &
                       fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+k) + stress(j,k)/nn
                enddo
              enddo
              s11 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+1)
              s22 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+2)
              s33 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+3)
              s12 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+4)
              s23 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+5)
              s13 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+6)
              ps = ( s11 + s22 + s33 ) / 3.0d0
              smises = 0.5d0 *( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2+ s13**2
              fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+13) = sqrt( 3.0d0 * smises )
              t11 = s11
              t22 = s22
              t33 = s33
              t12 = s12
              t23 = s23
              t13 = s13
              tmises = smises
            endif

            call ElementStress_Shell_MITC( ic_type, nn, 6, ecoord(1:3,1:nn), fstrSOLID%elements(icel)%gausses, &
                 edisp(1:6,1:nn), strain, stress, thick, -1.0d0, n_layer, n_total_layer)
            do j = 1, nn
              i = nodLOCAL(j)
              do k = 1, 6
                ndstrain_minus(i,6*(n_layer-1)+k) = ndstrain_minus(i,6*(n_layer-1)+k) + strain(j,k)
                ndstress_minus(i,6*(n_layer-1)+k) = ndstress_minus(i,6*(n_layer-1)+k) + stress(j,k)
              enddo
              nnumber(i) = nnumber(i) + 1
            enddo
            if( ID_area == hecMESH%my_rank ) then
              do j = 1, nn
                do k = 1, 6
                  fstrSOLID%ESTRAIN(12*n_total_layer*(icel-1)+12*(n_layer-1)+k+6) = &
                       fstrSOLID%ESTRAIN(12*n_total_layer*(icel-1)+12*(n_layer-1)+k+6) + strain(j,k)/nn
                  fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+k+6) = &
                       fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+k+6) + stress(j,k)/nn
                enddo
              enddo
              s11 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+7)
              s22 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+8)
              s33 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+9)
              s12 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+10)
              s23 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+11)
              s13 = fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+12)
              ps = ( s11 + s22 + s33 ) / 3.0
              smises = 0.5d0 *( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2+ s13**2
              fstrSOLID%ESTRESS(14*n_total_layer*(icel-1)+14*(n_layer-1)+14) = sqrt( 3.0d0 * smises )
            endif
            shellmatl = int(fstrSOLID%elements(icel)%gausses(1)%pMaterial%variables(M_SHELL_MATLTYPE))
            if (shellmatl == 0)then
              thick_layer = fstrSOLID%elements(icel)%gausses(1)%pMaterial%variables(100+3*n_layer)
            elseif (shellmatl == 1)then
              thick_layer = fstrSOLID%elements(icel)%gausses(1)%pMaterial%variables(100+8*n_layer-5)
            else
              write(*,*)"ERROR : shellmatl isnot correct"; stop
            endif
            !  ********** input of Sectional Stress **********
            !fstrSOLID%ESTRESS(14*n_total_layer*icel-5) = &
            !     0.5d0*(sqrt( 3.0d0 * smises )+sqrt( 3.0d0 * tmises ))*thick_layer / thick
            !fstrSOLID%ESTRESS(14*n_total_layer*icel-4) = (t11+s11)*thick_layer*0.5d0
            !fstrSOLID%ESTRESS(14*n_total_layer*icel-3) = (t22+s22)*thick_layer*0.5d0
            !fstrSOLID%ESTRESS(14*n_total_layer*icel-2) = (t12+s12)*thick_layer*0.5d0
            !fstrSOLID%ESTRESS(14*n_total_layer*icel-1) = (t23+s23)*thick_layer*0.5d0
            !fstrSOLID%ESTRESS(14*n_total_layer*icel  ) = (t13+s13)*thick_layer*0.5d0
          ENDDO     !DO n_layer=1,n_total_layer
        endif
      enddo
    enddo

    !C** average over nodes
    do i = 1, hecMESH%n_node
      do n_layer=1,n_total_layer
        do j = 1, 6
          ndstrain_plus(i,6*(n_layer-1)+j) = ndstrain_plus(i,6*(n_layer-1)+j) / nnumber(i)
          ndstress_plus(i,6*(n_layer-1)+j) = ndstress_plus(i,6*(n_layer-1)+j) / nnumber(i)
          ndstrain_minus(i,6*(n_layer-1)+j) = ndstrain_minus(i,6*(n_layer-1)+j) / nnumber(i)
          ndstress_minus(i,6*(n_layer-1)+j) = ndstress_minus(i,6*(n_layer-1)+j) / nnumber(i)
          fstrSOLID%STRAIN(12*n_total_layer*(i-1)+12*(n_layer-1)+j) = ndstrain_plus(i,6*(n_layer-1)+j)
          fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+j) = ndstress_plus(i,6*(n_layer-1)+j)
          fstrSOLID%STRAIN(12*n_total_layer*(i-1)+12*(n_layer-1)+j+6) = ndstrain_minus(i,6*(n_layer-1)+j)
          fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+j+6) = ndstress_minus(i,6*(n_layer-1)+j)
        enddo
        s11 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+1)
        s22 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+2)
        s33 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+3)
        s12 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+4)
        s23 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+5)
        s13 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+6)
        ps = ( s11 + s22 + s33 ) / 3.0
        tmises = 0.5d0 *( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2+ s13**2
        fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+13) = sqrt( 3.0d0 * tmises )
        s11 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+7)
        s22 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+8)
        s33 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+9)
        s12 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+10)
        s23 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+11)
        s13 = fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+12)
        ps = ( s11 + s22 + s33 ) / 3.0
        smises = 0.5d0 *( (s11-ps)**2 + (s22-ps)**2 + (s33-ps)**2 ) + s12**2 + s23**2+ s13**2
        fstrSOLID%STRESS(14*n_total_layer*(i-1)+14*(n_layer-1)+14) = sqrt( 3.0d0 * smises )
      enddo
    enddo

    deallocate( ndstrain_plus, ndstrain_minus )
    deallocate( ndstress_plus, ndstress_minus )
    deallocate( nnumber )

  end subroutine fstr_NodalStress6D

end module m_fstr_NodalStress
