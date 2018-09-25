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


!> This module provides function to check input data of IFSTR solver
module m_fstr_precheck
   contains

   subroutine fstr_get_thickness(idx_mesh,mid,thick)
      use hecmw
      use m_fstr
      use ppohFEM
      IMPLICIT REAL(kind=kreal) (A-H,O-Z)

      integer, intent(IN)  :: idx_mesh
      integer, intent(IN)  :: mid
      real(kind=kreal), intent(OUT) :: thick

      ihead = ppohFEM_get_sect_R_index(idx_mesh, mid-1)
      thick = ppohFEM_get_sect_R_item(idx_mesh, ihead+1)
    !  if(thick.LE.0.0) then
    !      write(*,*) "Zero thickness <= 0 is illegal"
    !      call hecmw_abort( hecmw_comm_get_comm())
    !  endif
   end subroutine fstr_get_thickness
!C
!C***
!C*** Pre Check for FSTR solver
!C***
!C
   subroutine fstr_precheck ( idx_mesh )

      use m_fstr

      IMPLICIT DOUBLE PRECISION(a-h,o-z)

      integer, intent(IN) :: idx_mesh

      IF(myrank .EQ. 0) THEN
        WRITE(IMSG,*)
        WRITE(IMSG,*) ' ****   STAGE PreCheck  **'
      ENDIF

      call fstr_precheck_elem ( idx_mesh )
      write(IDBG,*) 'fstr_precheck_elem: OK'

   end subroutine fstr_precheck
!C
!C
   subroutine fstr_precheck_elem ( idx_mesh )

      use m_fstr
      use m_precheck_LIB_2d
      use m_precheck_LIB_3d
      use m_precheck_LIB_shell

      implicit REAL(kind=kreal) (A-H,O-Z)

      type (hecmwST_matrix)     :: hecMAT
      type (fstr_solid)         :: fstrSOLID
      integer, intent(IN) :: idx_mesh

!** Local variables
      real(kind=kreal) xx(20),yy(20),zz(20)
      integer(kind=kint) nodLOCAL(20),NTOTsum(1)
      real(kind=kreal) TOTsum(1),TOTmax(3),TOTmin(2)

      real(kind=kreal)   :: XYZ(3)
      integer, parameter :: idx_mat  = 1 ! DEBUG
!C
!C INIT
!C
      nelem  = 0
      tvol   = 0.0
      tvmax  = 0.0
      tvmin  = 1.0e+20
      tlmax  = 0.0
      tlmin  = 1.0e+20
      aspmax = 0.0
!C
!C 3D
!C
      if( ppohFEM_get_mat_ndof(idx_mat) .eq. 3 ) then
        do ie = 1, ppohFEM_get_n_elem(idx_mesh)
!          ia = hecMESH%elem_ID(ie*2)
!          if( ia.ne.hecMESH%my_rank ) cycle
!          je = hecMESH%elem_ID(ie*2-1)

          jelem = ppohFEM_get_global_elem_ID(idx_mesh, ie)

!C
          ic_type = ppohFEM_get_elem_type(idx_mesh, ie)

!C
          if (.not. (ppohFEM_is_etype_rod(ic_type) .or. ppohFEM_is_etype_solid(ic_type))) then
            write(ILOG,*) jelem, ' This Element cannot be checked. Type=',ic_type
            cycle
          endif
          nn = ppohFEM_get_max_node_of_elem_type(ic_type)
!C
          do j = 1, nn
            nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, ie, j)
            call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
            xx(j) = XYZ(1)
            yy(j) = XYZ(2)
            zz(j) = XYZ(3)
          enddo
!C
          if    ( ic_type.eq.111 ) then
            isect = ppohFEM_get_section_ID(idx_mesh, ie)
            mid = ppohFEM_get_sect_mat_ID_item(idx_mesh, isect)
            CALL fstr_get_thickness( idx_mesh,mid,AA )
            al = SQRT( (xx(2)-xx(1))**2+(yy(2)-yy(1))**2+(zz(2)-zz(1))**2 )
            nline = 1
            tline = al
            vol = AA*al
            almax = al
            almin = al
          elseif( ic_type.eq.341 ) then
            call PRE_341 ( xx,yy,zz,vol,almax,almin )
          elseif( ic_type.eq.351 ) then
            call PRE_351 ( xx,yy,zz,vol,almax,almin )
          elseif( ic_type.eq.361 ) then
            call PRE_361 ( xx,yy,zz,vol,almax,almin )
          elseif( ic_type.eq.342 ) then
            call PRE_342 ( xx,yy,zz,vol,almax,almin )
          elseif( ic_type.eq.352 ) then
            call PRE_352 ( xx,yy,zz,vol,almax,almin )
          elseif( ic_type.eq.362 ) then
            call PRE_362 ( xx,yy,zz,vol,almax,almin )
          endif
!C
          if( vol.le.0.0 ) then
            write(ILOG,*) '  %%%  ERROR %%%  Volume of Element no.=',jelem,' is zero or negative.'
          endif
          nelem = nelem + 1
          tvol = tvol + vol
          if( vol.gt.tvmax ) tvmax = vol
          if( vol.lt.tvmin ) tvmin = vol
          if( almax.gt.tlmax ) tlmax = almax
          if( almin.lt.tlmin ) tlmin = almin
          asp = almax/almin
          if( asp.gt.aspmax ) aspmax = asp
          if( asp.gt.50 ) then
            write(ILOG,*) '  %%%  WARNIG %%% Aspect ratio of Element no.=',jelem,' exceeds 50.'
            write(ILOG,*) '      Maximum length =',almax
            write(ILOG,*) '      Minimum length =',almin
          endif
        enddo
!C
!C 2D
!C
      elseif( ppohFEM_get_mat_ndof(idx_mat) .eq. 2 ) then
        do ie = 1, ppohFEM_get_n_elem(idx_mesh)
!          ia = hecMESH%elem_ID(ie*2)
!          if( ia.ne.hecMESH%my_rank ) cycle
!          je = hecMESH%elem_ID(ie*2-1)
          jelem = ppohFEM_get_global_elem_ID(idx_mesh, ie)
!C
          ic_type = ppohFEM_get_elem_type(idx_mesh, ie)
!C
          if (.not. (ppohFEM_is_etype_rod(ic_type) .or. ppohFEM_is_etype_surface(ic_type))) then
            write(ILOG,*) jelem, ' This Element cannot be checked. Type=',ic_type
            cycle
          endif
          nn = ppohFEM_get_max_node_of_elem_type(ic_type)
!C
          do j = 1, nn
            nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, ie, j)
            call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
            xx(j) = XYZ(1)
            yy(j) = XYZ(2)
          enddo
!C
          isect = ppohFEM_get_section_ID(idx_mesh, ie)
          mid = ppohFEM_get_sect_mat_ID_item(idx_mesh, isect)
          CALL fstr_get_thickness( idx_mesh,mid,AA )
!C
          if    ( ic_type.eq.111 ) then
            al = SQRT( (xx(2)-xx(1))**2+(yy(2)-yy(1))**2 )
            vol = AA*al
            if( al.gt.tlmax ) tlmax = al
            if( al.lt.tlmin ) tlmin = al
            aspmax = 1.0
          elseif( ic_type.eq.231 ) then
            call PRE_231 ( xx,yy,AA,vol,almax,almin )
          elseif( ic_type.eq.241 ) then
            call PRE_241 ( xx,yy,AA,vol,almax,almin )
          elseif( ic_type.eq.232 ) then
            call PRE_232 ( xx,yy,AA,vol,almax,almin )
          elseif( ic_type.eq.242 ) then
            call PRE_242 ( xx,yy,AA,vol,almax,almin )
          else
            vol = 0.0
          endif
!C
          if( vol.le.0.0 ) then
            write(ILOG,*) '  %%%  ERROR %%%  Volume of Element no.=',jelem,' is zero or negative.'
          endif
          nelem = nelem + 1
          tvol = tvol + vol
          if( vol.gt.tvmax ) tvmax = vol
          if( vol.lt.tvmin ) tvmin = vol
          if( almax.gt.tlmax ) tlmax = almax
          if( almin.lt.tlmin ) tlmin = almin
          asp = almax/almin
          if( asp.gt.aspmax ) aspmax = asp
          if( asp.gt.50 ) then
            write(ILOG,*) '  %%%  WARNIG %%% Aspect ratio of Element no.=',jelem,' exceeds 50.'
            write(ILOG,*) '      Maximum length =',almax
            write(ILOG,*) '      Minimum length =',almin
          endif
        enddo
!C
!C SHELL
!C
      elseif( ppohFEM_get_mat_ndof(idx_mat) .eq. 6 ) then
        do ie = 1, ppohFEM_get_n_elem(idx_mesh)
!          ia = hecMESH%elem_ID(ie*2)
!          if( ia.ne.hecMESH%my_rank ) cycle
!          je = hecMESH%elem_ID(ie*2-1)
          jelem = ppohFEM_get_global_elem_ID(idx_mesh, ie)
!C
          ic_type = ppohFEM_get_elem_type(idx_mesh, ie)
!C
          if (.not. (ppohFEM_is_etype_beam(ic_type) .or. ppohFEM_is_etype_shell(ic_type))) then
            write(ILOG,*) jelem, ' This Element cannot be checked. Type=',ic_type
            cycle
          endif
          nn = ppohFEM_get_max_node_of_elem_type(ic_type)
!C
          do j = 1, nn
            nodLOCAL(j) = ppohFEM_get_node_item_of_element(idx_mesh, ie, j)
            call ppohFEM_get_node_coord(idx_mesh, nodLOCAL(j), XYZ)
            xx(j) = XYZ(1)
            yy(j) = XYZ(2)
            zz(j) = XYZ(3)
          enddo
!C
          isect = ppohFEM_get_section_ID(idx_mesh, ie)
          mid = ppohFEM_get_sect_mat_ID_item(idx_mesh, isect)
          CALL fstr_get_thickness( idx_mesh,mid,AA )
!C
          if    ( ic_type.eq.111 ) then
            al = SQRT( (xx(2)-xx(1))**2+(yy(2)-yy(1))**2+(zz(2)-zz(1))**2 )
            nline = nline + 1
            tline = tline + al
            vol = AA*al
            if( al.gt.tlmax ) tlmax = al
            if( al.lt.tlmin ) tlmin = al
            aspmax = 1.0
          elseif( ic_type.eq.731 ) then
            call PRE_731 ( xx,yy,zz,AA,vol,almax,almin )
          elseif( ic_type.eq.741 ) then
            call PRE_741 ( xx,yy,zz,AA,vol,almax,almin )
          endif
!C
          if( vol.le.0.0 ) then
            write(ILOG,*) '  %%%  ERROR %%%  Volume of Element no.=',jelem,' is zero or negative.'
          endif
          nelem = nelem + 1
          tvol = tvol + vol
          if( vol.gt.tvmax ) tvmax = vol
          if( vol.lt.tvmin ) tvmin = vol
          if( almax.gt.tlmax ) tlmax = almax
          if( almin.lt.tlmin ) tlmin = almin
          asp = almax/almin
          if( asp.gt.aspmax ) aspmax = asp
          if( asp.gt.50 ) then
            write(ILOG,*) '  %%%  WARNIG %%% Aspect ratio of Element no.=',jelem,' exceeds 50.'
            write(ILOG,*) '      Maximum length =',almax
            write(ILOG,*) '      Minimum length =',almin
          endif
        enddo
      endif
!C
      avvol = tvol / nelem
      write(ILOG,*) '###  Sub Summary  ###'
      write(ILOG,*) ' Total Volumes in this region        = ',tvol
      write(ILOG,*) ' Average Volume of elements          = ',avvol
      write(ILOG,*) ' Maximum Volume of elements          = ',tvmax
      write(ILOG,*) ' Minimum Volume of elements          = ',tvmin
      write(ILOG,*) ' Maximum length of element edges     = ',tlmax
      write(ILOG,*) ' Minimum length of element edges     = ',tlmin

      write(ILOG,*) ' Maximum aspect ratio in this region = ',aspmax
      TOTsum(1) = tvol
      call ppohFEM_allREDUCE_R(idx_mesh,TOTsum,1,ppohFEM_sum)
      NTOTsum(1) = nelem
      call ppohFEM_allREDUCE_I(idx_mesh,NTOTsum,1,ppohFEM_sum)
      TOTmax(1) = tvmax
      TOTmax(2) = tlmax
      TOTmax(3) = aspmax
      call ppohFEM_allREDUCE_R(idx_mesh,TOTmax,3,ppohFEM_max)
      TOTmin(1) = tvmin
      TOTmin(2) = tlmin
      call ppohFEM_allREDUCE_R(idx_mesh,TOTmin,2,ppohFEM_min)
      if( ppohFEM_comm_get_rank() .eq. 0 ) then
        avvol = TOTsum(1) / NTOTsum(1)
        write(ILOG,*) '###  Global Summary  ###'
        write(ILOG,*) ' TOTAL VOLUME = ',TOTsum(1)
        write(*,*)    ' TOTAL VOLUME = ',TOTsum(1)
        write(ILOG,*) ' AVERAGE VOLUME OF ELEMENTS = ',avvol
        write(*,*)    ' AVERAGE VOLUME OF ELEMENTS = ',avvol
        write(ILOG,*) ' MAXIMUM VOLUME OF ELEMENTS = ',TOTmax(1)
        write(*,*)    ' MAXIMUM VOLUME OF ELEMENTS = ',TOTmax(1)
        write(ILOG,*) ' MINIMUM VOLUME OF ELEMENTS = ',TOTmin(1)
        write(*,*)    ' MINIMUM VOLUME OF ELEMENTS = ',TOTmin(1)
        write(ILOG,*) ' MAXIMUM LENGTH OF ELEMENT EDGES = ',TOTmax(2)
        write(*,*)    ' MAXIMUM LENGTH OF ELEMENT EDGES = ',TOTmax(2)
        write(ILOG,*) ' MINIMUM LENGTH OF ELEMENT EDGES = ',TOTmin(2)
        write(*,*)    ' MINIMUM LENGTH OF ELEMENT EDGES = ',TOTmin(2)
        write(ILOG,*) ' MAXIMUM ASPECT RATIO  = ',TOTmax(3)
        write(*,*)    ' MAXIMUM ASPECT RATIO  = ',TOTmax(3)
      endif
!C
   end subroutine fstr_precheck_elem
end module m_fstr_precheck
