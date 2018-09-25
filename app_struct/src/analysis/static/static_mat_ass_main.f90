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

MODULE m_static_mat_ass_main

    IMPLICIT NONE

    CONTAINS


    SUBROUTINE FSTR_MAT_ASS_MAIN (idx_mesh, idx_mat, fstrSOLID)

        USE m_fstr
        USE ppohFEM

        integer, intent(in) :: idx_mesh ! index of mesh (hecMESH)
        integer, intent(in) :: idx_mat  ! index of stiffness matrix (hecMAT)
        TYPE( fstr_solid )         :: fstrSOLID

!** Local variables
        REAL(kind=kreal) :: xx(20), yy(20), zz(20), stiffness(20*6, 20*6)
        INTEGER(kind=kint) :: nodLOCAL(20)
        INTEGER(kind=kint) :: ndof, itype, iS, iE, ic_type, nn, icel, j
        INTEGER(kind=kint) :: flag_shell, flag_solid, mixflag
        INTEGER(kind=kint) :: isect, cdsys_ID
        REAL(kind=kreal) :: coords(3,3), XYZ(3)

!C
!C +-------+
!C | INIT. |
!C +-------+
!C====
        CALL ppohFEM_mat_clear(idx_mat)
!C
!C +-------------------------------+
!C | ELEMENT-by-ELEMENT ASSEMBLING |
!C | according to ELEMENT TYPE     |
!C +-------------------------------+
        ndof = ppohFEM_get_mat_ndof(idx_mat)

        do itype = 1, ppohFEM_get_n_elem_type(idx_mesh)
            iS= ppohFEM_get_elem_type_index(idx_mesh, itype-1) + 1
            iE= ppohFEM_get_elem_type_index(idx_mesh, itype  )
            ic_type= ppohFEM_get_elem_type_item(idx_mesh, itype)
!C** Ignore link elements
            if (ppohFEM_is_etype_link(ic_type)) cycle
!C** Set number of nodes
            nn = ppohFEM_get_max_node_of_elem_type(ic_type)
!C element loop
!$omp parallel default(none),private(icel,j,nodLOCAL,XYZ,xx,yy,zz,stiffness,cdsys_ID,coords,isect), &
!$omp&         shared(iS,iE,nn,ndof,ic_type,fstrSOLID,idx_mat,idx_mesh)
!$omp do
            do icel= iS, iE
!C** node ID
                do j=1,nn
                    nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, icel, j)
!C** nodal coordinate
                    call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
                    xx(j)=XYZ(1)
                    yy(j)=XYZ(2)
                    zz(j)=XYZ(3)
                enddo
!C** Create local stiffness
                isect = ppohFEM_get_section_ID(idx_mesh, icel)
                cdsys_ID = ppohFEM_get_sect_orien_ID(idx_mesh, isect)
                if( cdsys_ID > 0 ) call get_coordsys( cdsys_ID, idx_mesh, fstrSOLID, coords )
                call fstr_local_stf_create                                                                                       &
                     (ndof, ic_type, icel, xx, yy, zz, fstrSOLID%elements(icel)%gausses,                                &
                      fstrSOLID%elements(icel)%iset, stiffness, cdsys_ID, coords, fstrSOLID%TEMP_ngrp_tot, fstrSOLID%TEMP_irres)
!== CONSTRUCT the GLOBAL MATRIX STARTED
                call ppohFEM_mat_ass_elem(idx_mat, nn, nodLOCAL, stiffness)
            enddo
!$omp end do
!$omp end parallel
        enddo

!* for EQUATION
!        CALL hecmw_mat_ass_equation ( hecMESH, hecMAT )

    END SUBROUTINE FSTR_MAT_ASS_MAIN


!> Calculate stiff matrix of current element
    SUBROUTINE FSTR_LOCAL_STF_CREATE                                          &
               (ndof, ic_type, icel, xx, yy, zz, gausses,            &
                iset, stiffness, cdsys_ID, coords, TEMP_ngrp_tot, TEMP_irres)

        USE m_fstr
        USE m_static_lib
        USE mMechGauss

        use ppohFEM

!        TYPE( hecmwST_local_mesh ) :: hecMESH
        INTEGER(kind=kint) :: ndof, ic_type, icel, iset
        REAL(kind=kreal) :: xx(:), yy(:), zz(:)
        TYPE( tGaussStatus ), INTENT(IN) :: gausses(:)
        REAL(kind=kreal) :: stiffness(:, :)
        INTEGER(kind=kint) :: cdsys_ID
        REAL(kind=kreal) :: coords(3, 3)
        INTEGER(kind=kint) :: TEMP_ngrp_tot, TEMP_irres

        !** Local variables
        REAL(kind=kreal) :: ee, pp, thick, ecoord(3, 20)
        TYPE( tMaterial ), POINTER :: material
        REAL(kind=kreal) :: local_stf(1830)
        INTEGER(kind=kint) :: nn, isect, ihead, mixflag

        integer(kind=kint), parameter :: idx_mesh = 1

        nn = ppohFEM_get_max_node_of_elem_type(ic_type)
        ecoord(1,1:nn) = xx(1:nn)
        ecoord(2,1:nn) = yy(1:nn)
        ecoord(3,1:nn) = zz(1:nn)
        material => gausses(1)%pMaterial
        ee = material%variables(M_YOUNGS)
        pp = material%variables(M_POISSON)

        if ( ic_type == 241 .or. ic_type == 242 .or.                        &
             ic_type == 231 .or. ic_type == 232 .or. ic_type == 2322 ) then
            thick =1.d0
            CALL STF_C2( ic_type,nn,ecoord(1:2,1:nn),gausses(:),thick,stiffness(1:nn*ndof,1:nn*ndof),iset)

        else if( ic_type == 301 ) then
            isect= ppohFEM_get_section_ID(idx_mesh, icel)
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            thick = ppohFEM_get_sect_R_item(idx_mesh, ihead+1)
            CALL STF_C1( ic_type,nn,ecoord(:,1:nn),thick,gausses(:),stiffness(1:nn*ndof,1:nn*ndof) )

        else if( ic_type == 781 ) then   !for shell-solid mixed analysis
            isect= ppohFEM_get_section_ID(idx_mesh, icel)
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            thick = ppohFEM_get_sect_R_item(idx_mesh, ihead+1)
            !nn = 4; ndof = 6; ic_type = 741
            mixflag = 1
            CALL STF_Shell_MITC(741, 4, 6, ecoord(1:3, 1:4), gausses(:), stiffness(1:nn*ndof, 1:nn*ndof), thick, mixflag)
            !ic_type = 781; nn = 8; ndof = 3
            return

        else if( ic_type == 761 ) then   !for shell-solid mixed analysis
            isect= ppohFEM_get_section_ID(idx_mesh, icel)
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            thick = ppohFEM_get_sect_R_item(idx_mesh, ihead+1)
            !nn = 3; ndof = 6; ic_type = 731;
            mixflag = 2
            CALL STF_Shell_MITC(731, 3, 6, ecoord(1:3, 1:3), gausses(:), stiffness(1:nn*ndof, 1:nn*ndof), thick, mixflag)
            !ic_type = 761; nn = 6; ndof = 3
            return

        else if( ic_type == 361 ) then
            if( TEMP_ngrp_tot > 0 .or. TEMP_irres > 0 ) then
             CALL STF_C3D8Bbar( ic_type, nn, ecoord(:, 1:nn), gausses(:), stiffness(1:nn*ndof, 1:nn*ndof), cdsys_ID, coords, 1.0D0 )
            else
             CALL STF_C3D8IC( ic_type, nn, ecoord(:, 1:nn), gausses(:), stiffness(1:nn*ndof, 1:nn*ndof), cdsys_ID, coords )
            endif

        else if( ic_type == 341 .or. ic_type == 351 .or.                       &
                 ic_type == 342 .or. ic_type == 352 .or. ic_type == 362 ) then
            CALL STF_C3( ic_type,nn,ecoord(:,1:nn),gausses(:),stiffness(1:nn*ndof,1:nn*ndof), cdsys_ID, coords, 1.0D0)

        else if( ( ic_type == 741 ) .or. ( ic_type == 743 ) .or. ( ic_type == 731 ) ) then
            isect= ppohFEM_get_section_ID(idx_mesh, icel)
            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
            thick = ppohFEM_get_sect_R_item(idx_mesh, ihead+1)
            mixflag = 0
            CALL STF_Shell_MITC(ic_type, nn, ndof, ecoord(1:3, 1:nn), gausses(:), stiffness(1:nn*ndof, 1:nn*ndof), thick, mixflag)

!        else if( ic_type == 611) then
!            isect= ppohFEM_get_section_ID(idx_mesh, icel)
!            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
!            CALL STF_Beam(ic_type,nn,ecoord,hecMESH%section%sect_R_item(ihead+1:),ee, pp,stiffness(1:nn*ndof,1:nn*ndof))
!
!        else if( ic_type == 641 ) then
!            isect= ppohFEM_get_section_ID(idx_mesh, icel)
!            ihead = ppohFEM_get_sect_R_index(idx_mesh, isect-1)
!            CALL STF_Beam_641(ic_type, nn, ecoord,gausses(:), hecMESH%section%sect_R_item(ihead+1:), stiffness(1:nn*ndof,1:nn*ndof))
!
        else
            write(*,*) '###ERROR### : Element type not supported for linear static analysis'
            write(*,*) ' ic_type = ', ic_type
            CALL hecmw_abort(hecmw_comm_get_comm())

        endif

    END SUBROUTINE FSTR_LOCAL_STF_CREATE


END MODULE m_static_mat_ass_main
