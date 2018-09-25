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

module m_fstr_ass_load
    implicit none
    contains
!
!======================================================================!
!> This subroutine assmble following external force into fstrSOLID%GL and hecMAT%B afterwards
!>  -#  concentrated nodal force
!>  -#  surface pressure
!>  -#  volume force
!>  -#  thermal force

    subroutine fstr_ass_load(cstep, idx_mesh, idx_mat, fstrSOLID, fstrPARAM)
!======================================================================!
      use m_fstr
      use m_static_lib
      use m_fstr_precheck
      use mMechGauss
      use mReadTemp
      use mULoad
!#ifdef PARA_CONTACT
      use m_fstr_para_contact
!#endif

      use ppohFEM

      integer, intent(in)                  :: cstep       !< current step
      integer(kind=kint), intent(in)       :: idx_mesh    !< index of hecmw mesh
      integer(kind=kint), intent(in)       :: idx_mat     !< index of hecmw matrix
      type (fstr_solid),intent(inout)      :: fstrSOLID   !< fstr_solid
      type (fstr_param),intent(inout)      :: fstrPARAM   !< analysis control parameters

      real(kind=kreal) :: xx(20), yy(20), zz(20)
      real(kind=kreal) :: params(0:6)
      real(kind=kreal) :: vect(60)
      integer(kind=kint) :: iwk(60)
      integer(kind=kint) :: nodLocal(20)
      real(kind=kreal) :: tt(20), tt0(20), coords(3,3), factor
      integer(kind=kint) :: nnode, ndof, ig0, ig, ityp, ltype, iS0, iE0, ik, in, i, j
      integer(kind=kint) :: icel, ic_type, nn, iS, isect, id, iset, nsize
      integer(kind=kint) :: itype, iE, ierror, grpid, cdsys_ID
      real(kind=kreal) :: fval, rho, thick, pa1
      logical :: fg_surf
      integer(kind=kint) :: tstep
      type( tMaterial ), pointer :: material     !< material information
      integer(kind=kint) :: ihead
      real(kind=kreal) :: a
      real(kind=kreal) :: XYZ(3)
      real(kind=kreal) :: val



      ndof = ppohFEM_get_mat_ndof(idx_mat)
      nnode = ppohFEM_get_n_node(idx_mesh)

! -------------------------------------------------------------------
!  CLOAD
! -------------------------------------------------------------------
      fstrSOLID%GL(:) = 0.0d0
      do ig0 = 1, fstrSOLID%CLOAD_ngrp_tot
        grpid = fstrSOLID%CLOAD_ngrp_GRPID(ig0)
        if( .not. fstr_isLoadActive( fstrSOLID, grpid, cstep ) ) cycle
        factor = fstrSOLID%factor(2)
        if( cstep > 1 ) then
          if( fstr_isLoadActive( fstrSOLID, grpid, cstep-1 ) ) factor = 1.0d0
        endif
        ig = fstrSOLID%CLOAD_ngrp_ID(ig0)
        ityp = fstrSOLID%CLOAD_ngrp_DOF(ig0)
        fval = fstrSOLID%CLOAD_ngrp_val(ig0)

        
        do ik = 1, ppohFEM_get_num_nodes_in_node_group(idx_mesh, ig)
          in = ppohFEM_get_node_item_in_node_group(idx_mesh, ig, ik)
          fstrSOLID%GL(ndof*(in-1)+ityp) = fstrSOLID%GL(ndof*(in-1)+ityp)+factor*fval
        end do

      enddo
!
!
! -------------------------------------------------------------------
!  DLOAD
! -------------------------------------------------------------------
      do ig0 = 1, fstrSOLID%DLOAD_ngrp_tot
        grpid = fstrSOLID%DLOAD_ngrp_GRPID(ig0)
        if( .not. fstr_isLoadActive( fstrSOLID, grpid, cstep ) ) cycle
        factor = fstrSOLID%factor(2)
        if( cstep > 1 ) then
          if( fstr_isLoadActive( fstrSOLID, grpid, cstep-1 ) ) factor = 1.0d0
        endif
        ig = fstrSOLID%DLOAD_ngrp_ID(ig0)
        ltype = fstrSOLID%DLOAD_ngrp_LID(ig0)
        do i = 0, 6
          params(i)= fstrSOLID%DLOAD_ngrp_params(i,ig0)
        enddo

! ----- START & END
        fg_surf = (ltype == 100)


! fg_surf == .true. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if( fg_surf ) then                  ! surface group
        do ik = 1, ppohFEM_get_num_items_in_surf_group(idx_mesh, ig)
            ltype = ppohFEM_get_item_ltype_in_surf_group(idx_mesh, ig, ik)*10
            icel    = ppohFEM_get_item_element_in_surf_group(idx_mesh, ig, ik)
            ic_type= ppohFEM_get_elem_type_item(idx_mesh, itype)
          if( hecmw_is_etype_link(ic_type) ) cycle
         ! if( ic_type==3422 ) ic_type=342
          nn = hecmw_get_max_node(ic_type)
! ----- node ID
          if( fstrSOLID%DLOAD_follow == 0 ) then
            do j = 1, nn
              nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
! ----- nodal coordinate
              call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
              xx(j)=XYZ(1)
              yy(j)=XYZ(2)
              zz(j)=XYZ(3)
! ----- create iwk array ***
              do i = 1, ndof
                iwk( ndof*(j-1)+i ) = ndof*( nodLOCAL(j)-1 )+i
              enddo
            enddo
          else
            do j = 1, nn
              nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
! ----- nodal coordinate
              call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
              if (ndof==2) then
                xx(j) = XYZ(1)+fstrSOLID%unode( 2*nodLOCAL(j)-1 )+fstrSOLID%dunode( 2*nodLOCAL(j)-1 )
                yy(j) = XYZ(2)+fstrSOLID%unode( 2*nodLOCAL(j)   )+fstrSOLID%dunode( 2*nodLOCAL(j)   )
              else if (ndof==3) then
                xx(j) = XYZ(1)+fstrSOLID%unode( 3*nodLOCAL(j)-2 )+fstrSOLID%dunode( 3*nodLOCAL(j)-2 )
                yy(j) = XYZ(2)+fstrSOLID%unode( 3*nodLOCAL(j)-1 )+fstrSOLID%dunode( 3*nodLOCAL(j)-1 )
                zz(j) = XYZ(3)+fstrSOLID%unode( 3*nodLOCAL(j)   )+fstrSOLID%dunode( 3*nodLOCAL(j)   )
              else if (ndof==6) then
                xx(j) = XYZ(1)+fstrSOLID%unode( 6*nodLOCAL(j)-5 )+fstrSOLID%dunode( 6*nodLOCAL(j)-5 )
                yy(j) = XYZ(2)+fstrSOLID%unode( 6*nodLOCAL(j)-4 )+fstrSOLID%dunode( 6*nodLOCAL(j)-4 )
                zz(j) = XYZ(3)+fstrSOLID%unode( 6*nodLOCAL(j)-3 )+fstrSOLID%dunode( 6*nodLOCAL(j)-3 )
              endif
! ----- create iwk array ***
              do i = 1, ndof
                iwk( ndof*(j-1)+i ) = ndof*( nodLOCAL(j)-1 )+i
              enddo
            enddo
          end if
! ----- section  ID
          isect = ppohFEM_get_section_ID(idx_mesh, icel)
! ----- Get Properties
          material => fstrSOLID%elements(icel)%gausses(1)%pMaterial
          rho = material%variables(M_DENSITY)
          call fstr_get_thickness(idx_mesh,isect,thick)
! ----- Section Data
          if( ndof == 2 ) then
            id=ppohFEM_get_sect_opt(idx_mesh, isect)
            if( id == 0 ) then
              iset = 1
            else if( id == 1 ) then
              iset = 0
            else if( id == 2) then
              iset = 2
            endif
            pa1=1.d0
          endif
! ----- Create local stiffness
          if (ic_type==301)then
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            call DL_C1(ic_type,nn,xx(1:nn),yy(1:nn),zz(1:nn),rho,thick,ltype,params,vect(1:nn*ndof),nsize)

          elseif( ic_type == 241 .or. ic_type == 242 .or. ic_type == 231 .or. ic_type == 232 .or. ic_type == 2322 ) then
            call DL_C2(ic_type,nn,xx(1:nn),yy(1:nn),rho,pa1,ltype,params,vect(1:nn*ndof),nsize,iset)

          else if ( ic_type == 341 .or. ic_type == 351 .or. ic_type == 361 .or.   &
                    ic_type == 342 .or. ic_type == 352 .or. ic_type == 362 ) then
            call DL_C3(ic_type,nn,xx(1:nn),yy(1:nn),zz(1:nn),rho,ltype,params,vect(1:nn*ndof),nsize)

          else if ( ic_type == 641 ) then
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            call DL_Beam_641(ic_type, nn, xx(1:nn), yy(1:nn), zz(1:nn), rho, ltype, params, &
                            ppohFEM_get_sect_R_item(idx_mesh, ihead+4), vect(1:nn*ndof), nsize)

          else if( ( ic_type == 741 ) .or. ( ic_type == 743 ) .or. ( ic_type == 731 ) ) then
            call DL_Shell(ic_type, nn, ndof, xx, yy, zz, rho, thick, ltype, params, vect, nsize, fstrSOLID%elements(icel)%gausses)

          else if( ( ic_type==761 ) .or. ( ic_type==781 ) ) then
            call DL_Shell_33(ic_type, nn, ndof, xx, yy, zz, rho, thick, ltype, params, vect, nsize, &
                 fstrSOLID%elements(icel)%gausses)

          else
            nsize = 0
            write(*,*)"### WARNING: DLOAD",ic_type

          endif
! ----- Add vector
          do j=1,nsize
              fstrSOLID%GL( iwk(j) )=fstrSOLID%GL( iwk(j) )+factor*vect(j)
          enddo
        enddo
! end block fg_surf == .true. 

else ! fg_surf == .false. use element group  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! ----- START & END
        do ik = 1, ppohFEM_get_num_elements_in_element_group(idx_mesh, ig)
            icel    = ppohFEM_get_element_item_in_element_group(idx_mesh, ig, ik)
            ic_type = ppohFEM_get_elem_type(idx_mesh, icel)
          if( ppohFEM_is_etype_link(ic_type) ) cycle
         ! if( ic_type==3422 ) ic_type=342
          nn = ppohFEM_get_max_node_of_elem_type(ic_type)
! ----- node ID
          if( fstrSOLID%DLOAD_follow == 0 ) then
            do j = 1, nn
              nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
! ----- nodal coordinate
              call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
              xx(j)=XYZ(1)
              yy(j)=XYZ(2)
              zz(j)=XYZ(3)
! ----- create iwk array ***
              do i = 1, ndof
                iwk( ndof*(j-1)+i ) = ndof*( nodLOCAL(j)-1 )+i
              enddo
            enddo
          else
            do j = 1, nn
              nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
! ----- nodal coordinate
              call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
              if (ndof==2) then
                xx(j) = XYZ(1)+fstrSOLID%unode( 2*nodLOCAL(j)-1 )+fstrSOLID%dunode( 2*nodLOCAL(j)-1 )
                yy(j) = XYZ(2)+fstrSOLID%unode( 2*nodLOCAL(j)   )+fstrSOLID%dunode( 2*nodLOCAL(j)   )
              else if (ndof==3) then
                xx(j) = XYZ(1)+fstrSOLID%unode( 3*nodLOCAL(j)-2 )+fstrSOLID%dunode( 3*nodLOCAL(j)-2 )
                yy(j) = XYZ(2)+fstrSOLID%unode( 3*nodLOCAL(j)-1 )+fstrSOLID%dunode( 3*nodLOCAL(j)-1 )
                zz(j) = XYZ(3)+fstrSOLID%unode( 3*nodLOCAL(j)   )+fstrSOLID%dunode( 3*nodLOCAL(j)   )
              else if (ndof==6) then
                xx(j) = XYZ(1)+fstrSOLID%unode( 6*nodLOCAL(j)-5 )+fstrSOLID%dunode( 6*nodLOCAL(j)-5 )
                yy(j) = XYZ(2)+fstrSOLID%unode( 6*nodLOCAL(j)-4 )+fstrSOLID%dunode( 6*nodLOCAL(j)-4 )
                zz(j) = XYZ(3)+fstrSOLID%unode( 6*nodLOCAL(j)-3 )+fstrSOLID%dunode( 6*nodLOCAL(j)-3 )
              endif
! ----- create iwk array ***
              do i = 1, ndof
                iwk( ndof*(j-1)+i ) = ndof*( nodLOCAL(j)-1 )+i
              enddo
            enddo
          end if
! ----- section  ID
          isect = ppohFEM_get_section_ID(idx_mesh, icel)
! ----- Get Properties
          material => fstrSOLID%elements(icel)%gausses(1)%pMaterial
          rho = material%variables(M_DENSITY)
          call fstr_get_thickness(idx_mesh,isect,thick)
! ----- Section Data
          if( ndof == 2 ) then
            id=ppohFEM_get_sect_opt(idx_mesh, isect)
            if( id == 0 ) then
              iset = 1
            else if( id == 1 ) then
              iset = 0
            else if( id == 2) then
              iset = 2
            endif
            pa1=1.d0
          endif
! ----- Create local stiffness
          if (ic_type==301)then
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            call DL_C1(ic_type,nn,xx(1:nn),yy(1:nn),zz(1:nn),rho,thick,ltype,params,vect(1:nn*ndof),nsize)

          elseif( ic_type == 241 .or. ic_type == 242 .or. ic_type == 231 .or. ic_type == 232 .or. ic_type == 2322 ) then
            call DL_C2(ic_type,nn,xx(1:nn),yy(1:nn),rho,pa1,ltype,params,vect(1:nn*ndof),nsize,iset)

          else if ( ic_type == 341 .or. ic_type == 351 .or. ic_type == 361 .or.   &
                    ic_type == 342 .or. ic_type == 352 .or. ic_type == 362 ) then
            call DL_C3(ic_type,nn,xx(1:nn),yy(1:nn),zz(1:nn),rho,ltype,params,vect(1:nn*ndof),nsize)

          else if ( ic_type == 641 ) then
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            call DL_Beam_641(ic_type, nn, xx(1:nn), yy(1:nn), zz(1:nn), rho, ltype, params, &
                             ppohFEM_get_sect_R_item(idx_mesh, ihead+4), vect(1:nn*ndof), nsize)

          else if( ( ic_type == 741 ) .or. ( ic_type == 743 ) .or. ( ic_type == 731 ) ) then
            call DL_Shell(ic_type, nn, ndof, xx, yy, zz, rho, thick, ltype, params, vect, nsize, fstrSOLID%elements(icel)%gausses)

          else if( ( ic_type==761 ) .or. ( ic_type==781 ) ) then
            call DL_Shell_33(ic_type, nn, ndof, xx, yy, zz, rho, thick, ltype, params, vect, nsize, &
                 fstrSOLID%elements(icel)%gausses)

          else
            nsize = 0
            write(*,*)"### WARNING: DLOAD",ic_type

          endif
! ----- Add vector
          do j=1,nsize
              fstrSOLID%GL( iwk(j) )=fstrSOLID%GL( iwk(j) )+factor*vect(j)
          enddo
        enddo
end if ! end block fg_surf !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      enddo

! -----Uload
      call uloading( cstep, factor, fstrSOLID%GL )

!C
!C Update for fstrSOLID%GL
!C
      if( ppohFEM_get_mat_ndof(idx_mat) == 3 ) then
!        if(paraContactFlag) then
!          call paraContact_update_3_R (hecMESH,fstrSOLID%GL) ! contact is disabled
!        else
         call ppohFEM_update_3_R (idx_mesh,fstrSOLID%GL,ppohFEM_get_n_node(idx_mesh))
!        endif
      else if( ppohFEM_get_mat_ndof(idx_mat) == 2 ) then
        call ppohFEM_update_2_R (idx_mesh,fstrSOLID%GL,ppohFEM_get_n_node(idx_mesh))
      endif

      call ppohFEM_mat_clear_b( idx_mat )
      do i=1, nnode
        do j=1, ndof
         val = fstrSOLID%GL(ndof*(i-1)+j)-fstrSOLID%QFORCE(ndof*(i-1)+j)
         call ppohFEM_set_stiffMAT_B(idx_mat, i, j, val)
        end do
      enddo

!
!
! -------------------------------------------------------------------
!  TLOAD : THERMAL LOAD USING TEMPERATURE
! -------------------------------------------------------------------
!C
!C Set Temperature
!C
      if( fstrSOLID%TEMP_ngrp_tot > 0 .or. fstrSOLID%TEMP_irres > 0 ) then
        do ig0 = 1, fstrSOLID%TEMP_ngrp_tot
          grpid = fstrSOLID%TEMP_ngrp_GRPID(ig0)
          if( .not. fstr_isLoadActive( fstrSOLID, grpid, cstep ) ) cycle
          factor = fstrSOLID%factor(2)
          if( cstep > 1 ) then
            if( fstr_isLoadActive( fstrSOLID, grpid, cstep-1 ) ) factor = 1.d0
          endif
          ig = fstrSOLID%TEMP_ngrp_ID(ig0)
          fval =fstrSOLID%TEMP_ngrp_val(ig0)
          do ik = 1, ppohFEM_get_num_nodes_in_node_group(idx_mesh, ig)
            in = ppohFEM_get_node_item_in_node_group(idx_mesh, ig, ik)
            pa1 = fstrSOLID%temp_bak( in )
            fstrSOLID%temperature( in ) = pa1+(fval-pa1)*factor
          enddo
        enddo

        if( fstrSOLID%TEMP_irres > 0 ) then
          call read_temperature_result(idx_mesh, fstrSOLID%TEMP_irres, fstrSOLID%TEMP_tstep, fstrSOLID%temperature)
        endif

! ----- element TYPE loop.
        do itype = 1, ppohFEM_get_n_elem_type(idx_mesh)

            iS= ppohFEM_get_elem_type_index(idx_mesh, itype-1) + 1
            iE= ppohFEM_get_elem_type_index(idx_mesh, itype  )
            ic_type= ppohFEM_get_elem_type_item(idx_mesh, itype)
          if( ppohFEM_is_etype_link(ic_type) ) cycle
! ----- Set number of nodes
            nn = ppohFEM_get_max_node_of_elem_type(ic_type)

! ----- element loop
          do icel = iS, iE

! ----- node ID
            do j=1,nn
              nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
! ----- nodal coordinate
              call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
              if (ndof==2) then
                xx(j)=XYZ(1)+fstrSOLID%unode(ndof*nodLOCAL(j)-1)
                yy(j)=XYZ(2)+fstrSOLID%unode(ndof*nodLOCAL(j)  )
              else if (ndof==3) then
                xx(j)=XYZ(1)+fstrSOLID%unode(ndof*nodLOCAL(j)-2)
                yy(j)=XYZ(2)+fstrSOLID%unode(ndof*nodLOCAL(j)-1)
                zz(j)=XYZ(3)+fstrSOLID%unode(ndof*nodLOCAL(j))
              endif
              tt0(j)=fstrSOLID%last_temp( nodLOCAL(j) )
              tt(j) = fstrSOLID%temperature( nodLOCAL(j) )
! ----- create iwk array ***
              do i=1,ndof
                iwk(ndof*(j-1)+i)=ndof*(nodLOCAL(j)-1)+i
              enddo
            enddo

! ----- section  Data
            isect= ppohFEM_get_section_ID(idx_mesh, icel)
            cdsys_ID = ppohFEM_get_sect_orien_ID(idx_mesh, isect)
            call get_coordsys(cdsys_ID, idx_mesh, fstrSOLID, coords)

            if( ndof == 2 ) then
              id=ppohFEM_get_sect_opt(idx_mesh, isect)
              if( id==0 ) then
                iset = 1
              else if( id == 1 ) then
                iset = 0
              else if( id == 2 ) then
                iset = 2
              endif
              pa1 = 1.0d0
            endif

            IF( ic_type == 641 ) THEN

WRITE(*,*) ' Thermal loading option for 641 elements', &
                               'not yet available.'
  call ppohFEM_abort
!             isect= ppohFEM_get_section_ID(idx_mesh, icel)
!             ihead= ppohFEM_get_sect_R_index(isect-1)
!
!             CALL TLOAD_Beam_641( ic_type, nn, ndof, xx(1:nn), yy(1:nn), zz(1:nn), tt(1:nn), tt0(1:nn),    &
!                                  fstrSOLID%elements(icel)%gausses, hecMESH%section%sect_R_item(ihead+1:), &
!                                  vect(1:nn*ndof) )
!
!             DO j = 1, ndof*nn
!              hecMAT%B( iwk(j) ) = hecMAT%B( iwk(j) )+vect(j)
!             END DO
!             CYCLE
            END IF

! ----- Create local stiffness
            if(ic_type == 241 .or. ic_type == 242 .or. ic_type == 231 .or. ic_type == 232 ) then
              call TLOAD_C2( ic_type, nn, xx(1:nn), yy(1:nn), tt(1:nn), tt0(1:nn),      &
                             fstrSOLID%elements(icel)%gausses,pa1, iset, vect(1:nn*2) )

            else if( ic_type == 361 ) then
              call TLOAD_C3D8Bbar                                                          &
                   ( ic_type, nn, xx(1:nn), yy(1:nn), zz(1:nn), tt(1:nn), tt0(1:nn),       &
                     fstrSOLID%elements(icel)%gausses, vect(1:nn*ndof), cdsys_ID, coords )

            else if( ic_type == 341 .or. ic_type == 351 .or.                       &
                     ic_type == 342 .or. ic_type == 352 .or. ic_type == 362 ) then
              call TLOAD_C3                                                                &
                   ( ic_type, nn, xx(1:nn), yy(1:nn), zz(1:nn), tt(1:nn), tt0(1:nn),       &
                     fstrSOLID%elements(icel)%gausses, vect(1:nn*ndof), cdsys_ID, coords )

            else if( ic_type == 741 .or. ic_type == 743 .or. ic_type == 731 ) then
              if( myrank == 0 ) then
                WRITE(IMSG,*) '*------------------------', &
                               '-------------------*'
                WRITE(IMSG,*) ' Thermal loading option for shell elements', &
                               'not yet available.'
                WRITE(IMSG,*) '*------------------------', &
                               '-------------------*'
                call hecmw_abort( hecmw_comm_get_comm())
              endif

            endif

! ----- Add vector
!            do j = 1, ndof*nn
!               ! fstrSOLID%GL( iwk(j) )=fstrSOLID%GL( iwk(j) )+vect(j)
!               hecMAT%B( iwk(j) ) = hecMAT%B( iwk(j) )+vect(j)
!            enddo

        do j=1, ndof*nn
         val = ppohFEM_get_stiffMAT_B_direct_index(idx_mat, iwk(j))
         val = val + vect(j)
         call ppohFEM_set_stiffMAT_B_direct_index(idx_mat, iwk(j), val)
        end do


          enddo
        enddo
      endif

      if( associated( fstrSOLID%contacts ) .and. fstrPARAM%contact_algo == kcaALagrange ) then
        write(*,*) 'contact is not supported'
        call ppohFEM_abort
!        do i = 1, size(fstrSOLID%contacts)
!          call ass_contact_force( fstrSOLID%contacts(i), hecMESH%node, fstrSOLID%unode, hecMAT%B )
!        enddo
      endif

    end subroutine fstr_ass_load

    subroutine fstr_AddSPRING(cstep, sub_step, idx_mesh, idx_mat, fstrSOLID, fstrPARAM)
      use m_fstr
      use m_static_lib

      use ppohFEM

      integer, intent(in)                  :: cstep       !< current step
      integer, intent(in)                  :: sub_step    !< current sub_step
      integer, intent(in)                  :: idx_mat     !< index of stiffness matrix (hecMAT)
      integer, intent(in)                  :: idx_mesh    !< index of mesh (hecMESH)
      type (fstr_solid),intent(inout)      :: fstrSOLID   !< fstr_solid
      type (fstr_param),intent(inout)      :: fstrPARAM   !< analysis control parameters

      integer(kind=kint) :: grpid, ndof, ig0, ig, ityp, iS0, iE0, ik, in, idx, num
      real(kind=kreal) :: fval, fact
      real(kind=kreal) :: tmpval



      ndof = ppohFEM_get_mat_ndof(idx_mat)

      do ig0= 1, fstrSOLID%SPRING_ngrp_tot
        grpid = fstrSOLID%SPRING_ngrp_GRPID(ig0)
        if( .not. fstr_isLoadActive( fstrSOLID, grpid, cstep ) ) cycle
        ig= fstrSOLID%SPRING_ngrp_ID(ig0)
        ityp= fstrSOLID%SPRING_ngrp_DOF(ig0)
        fval= fstrSOLID%SPRING_ngrp_val(ig0)

        if( fval < 0.d0 ) then
          num = fstrSOLID%step_ctrl(cstep)%num_substep
          fact = DFLOAT( num - sub_step ) / DFLOAT( num )
          fval = - fval * fact
        endif

        do ik= 1, ppohFEM_get_num_nodes_in_node_group(idx_mesh, ig)
          in = ppohFEM_get_node_item_in_node_group(idx_mesh, ig, ik)
          tmpval = ppohFEM_get_hecMAT_matrix_element_value(idx_mat, in, in, ityp, ityp) + fval
          call ppohFEM_set_hecMAT_matrix_element_value(idx_mat, in, in, ityp, ityp, tmpval)
        enddo
      enddo

   end subroutine fstr_AddSPRING

end module m_fstr_ass_load
