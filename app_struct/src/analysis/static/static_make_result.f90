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


!> This module provide a function to prepare output of static analysis
module m_static_make_result
  private

  public:: fstr_write_static_result
  public:: fstr_make_static_result

  contains

!C***
!>  OUTPUT result file for static analysis
!C***
  subroutine fstr_write_static_result( idx_mesh, fstrSOLID, ndof, maxstep, istep, tnstrain, testrain, flag )
    use m_fstr
    use m_out
    use m_static_lib
    use mMaterial
    implicit none

    integer, intent(IN)       :: idx_mesh
    type (fstr_solid)         :: fstrSOLID
    integer, intent(IN)       :: ndof
    integer(kind=kint)        :: maxstep, istep, flag
    real(kind=kreal), pointer :: tnstrain(:), testrain(:)

    character(len=PPOHFEM_HEADER_LEN) :: header
    character(len=PPOHFEM_NAME_LEN)   :: s, label, nameID, addfname
    integer(kind=kint) :: i, j, k, mdof, id, nitem, nn, mm, ngauss, it
    real(kind=kreal), allocatable   :: work(:), unode781(:),rnode781(:)

    integer(kind=kint) :: n_lyr, ntot_lyr, bb, aa, mixedflag, cid
    character(len=4) :: char_n_lyr

    mm = ppohFEM_get_n_node(idx_mesh)
    if( ppohFEM_get_n_elem(idx_mesh) > ppohFEM_get_n_node(idx_mesh) ) mm = ppohFEM_get_n_elem(idx_mesh)
    if( ndof==2 ) mdof = 3
    if( ndof==3 ) mdof = 6
    if( ndof==6 ) mdof = 24

    bb = 0
    ntot_lyr = 1
    mixedflag = 0
    aa = 0

    do it=1, ppohFEM_get_n_sect(idx_mesh)
      cid = ppohFEM_get_sect_mat_ID_item(idx_mesh, it)
      aa =  int(fstrSOLID%materials(cid)%variables(M_TOTAL_LAYER))
      if (aa > ntot_lyr)then
        ntot_lyr = aa
      endif
    enddo
    do it=1, ppohFEM_get_n_elem_type(idx_mesh)
      aa =  ppohFEM_get_elem_type_item(idx_mesh, it)
      if (aa == 641)then
        if (mixedflag == 0) then
          mixedflag = 2
        endif
      elseif (aa == 781 .or. aa == 761)then
        mixedflag = 1
      endif
      if (mixedflag == 1)then
        mdof = 24
      endif
    enddo
    nn = mm * mdof
    allocate( work(nn) )

    ! --- INITIALIZE
    header = '*fstrresult'
    call ppohFEM_result_init( idx_mesh, maxstep, istep, header )
! --- DISPLACEMENT

    if( fstrSOLID%output_ctrl(3)%outinfo%on(1)) then
      id = 1
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(1), ndof )
      if (mixedflag == 1) then
write(*,*) 'currently shell-solid mixed element is not supported' ! DEBUG
call ppohFEM_abort !DEBUG
! currently node781 is not supported
!        allocate( unode781(3*mm) )
!        allocate( rnode781(3*mm) )
!        unode781 = 0.0d0
!        rnode781 = 0.0d0
!        label = 'DISPLACEMENT'
!        call reorder_node781(fstrSOLID, hecMESH, unode781, rnode781, mixedflag)
!        call ppohFEM_result_add( id, nitem, label, unode781 )
!
!        if (fstrSOLID%output_ctrl(3)%outinfo%on(18)) then
!          label = 'ROTATION'
!          call ppohFEM_result_add( id, nitem, label, rnode781 )
!        endif
!
!        deallocate( unode781 )
!        deallocate( rnode781 )
!      elseif(  mixedflag == 2) then
!        allocate( unode781(1) )
!        allocate( rnode781(1) )
!        unode781 = 0.0d0
!        rnode781 = 0.0d0
!        label = 'DISPLACEMENT'
!        call reorder_node781(fstrSOLID, hecMESH, unode781,rnode781, mixedflag)
!        call ppohFEM_result_add( id, 3, label, fstrSOLID%unode )
!        deallocate( unode781 )
!        deallocate( rnode781 )
      else
        label = 'DISPLACEMENT'
        call ppohFEM_result_add( id, nitem, label, fstrSOLID%unode )
      endif
    endif
! --- REACTION FORCE
    if( fstrSOLID%output_ctrl(3)%outinfo%on(2) ) then
      id = 1
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(2), ndof )
      label = 'REACTION_FORCE'
      call ppohFEM_result_add( id, nitem, label, fstrSOLID%QFORCE )
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! --- STRAIN @node

    DO n_lyr=1,ntot_lyr
      write(char_n_lyr, '(i0)') n_lyr

      if (fstrSOLID%output_ctrl(3)%outinfo%on(3)) then
        id = 1
        if( (ndof==6 .or. mixedflag==1 ) ) then
          nitem = 6
          label = 'NodalSTRAIn_lyr'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              work(6*(i-1)+j) = fstrSOLID%STRAIN(12*ntot_lyr*(i-1)+12*(n_lyr-1)+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
          label = 'NodalSTRAIn_lyr'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              work(6*(i-1)+j) = fstrSOLID%STRAIN(12*ntot_lyr*(i-1)+12*(n_lyr-1)+6+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        else
          nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(3), ndof )
          label = 'NodalSTRAIN'
          call ppohFEM_result_add( id, nitem, label, fstrSOLID%STRAIN )
        endif
      endif
! --- STRESS @node
      if( fstrSOLID%output_ctrl(3)%outinfo%on(4) ) then
        id = 1
        if( (ndof==6 .or. mixedflag==1 ) ) then
          nitem = 6
          label = 'NodalSTRESS_Layer'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              work(6*(i-1)+j) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
          label = 'NodalSTRESS_Layer'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              work(6*(i-1)+j) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+6+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        else
          nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(4), ndof )
          label = 'NodalSTRESS'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, nitem
              work(nitem*(i-1)+j) = fstrSOLID%STRESS((nitem+1)*(i-1)+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        endif
      endif
  ! --- MISES @node
      if( fstrSOLID%output_ctrl(3)%outinfo%on(5) ) then
        id = 1
        if( (ndof==6 .or. mixedflag==1 )  ) then
          nitem = 1
          label = 'NodalMISES_Layer'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            work(i) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+13)
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
          label = 'NodalMISES_Layer'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            work(i) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+14)
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        else if( fstrSOLID%output_ctrl(3)%outinfo%on(5) ) then
          nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(5), ndof )
          label = 'NodalMISES'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            work(i) = fstrSOLID%STRESS((mdof+1)*i)
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        endif
      endif
  ! --- STRAIN @element
      if( fstrSOLID%output_ctrl(3)%outinfo%on(6) ) then
        id = 2
        if(  (ndof==6 .or. mixedflag==1 ) ) then
          nitem = 6
          label = 'ElementalSTRAIn_lyr'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            do j = 1, 6
              work(6*(i-1)+j) = fstrSOLID%ESTRAIN(12*ntot_lyr*(i-1)+12*(n_lyr-1)+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )

          label = 'ElementalSTRAIn_lyr'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            do j = 1, 6
              work(6*(i-1)+j) = fstrSOLID%ESTRAIN(12*ntot_lyr*(i-1)+12*(n_lyr-1)+6+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        else
          nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(6), ndof )
          label = 'ElementalSTRAIN'
          call ppohFEM_result_add( id, nitem, label, fstrSOLID%ESTRAIN )
        endif
      endif
  ! --- STRESS @element
      if( fstrSOLID%output_ctrl(3)%outinfo%on(7) ) then
        id = 2
        if((ndof==6 .or. mixedflag==1 )) then
          nitem = 6
          label = 'ElementalSTRESS_Layer'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            do j = 1, 6
            work(6*(i-1)+j) = fstrSOLID%ESTRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
          label = 'ElementalSTRESS_Layer'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            do j = 1, 6
            work(6*(i-1)+j) = fstrSOLID%ESTRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+6+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        else
          nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(7), ndof )
          label = 'ElementalSTRESS'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            do j = 1, nitem
            work(nitem*(i-1)+j) = fstrSOLID%ESTRESS((nitem+1)*(i-1)+j)
            enddo
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        endif
      endif
  ! --- MISES @element
      if( fstrSOLID%output_ctrl(3)%outinfo%on(8)) then

        id = 2

        if( (ndof==6 .or. mixedflag==1 ) ) then
          nitem = 1
          label = 'ElementalMISES_Layer'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            work(i) = fstrSOLID%ESTRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+13)
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
          label = 'ElementalMISES_Layer'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            work(i) = fstrSOLID%ESTRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+14)
          enddo
          call ppohFEM_result_add( id, nitem, label, work )

          !nitem = 1
          !label = 'ElementalTotalMises'
          !do i = 1, ppohFEM_get_n_elem(idx_mesh)
          !  work(i) = fstrSOLID%ESTRESS(14*ntot_lyr*i-5)
          !enddo
          !call ppohFEM_result_add( id, nitem, label, work )

          !nitem = 5
          !label = 'ElementalSectionalForce' 
          !do i = 1, ppohFEM_get_n_elem(idx_mesh)
          !  do j = 1, 5
          !    work(5*(i-1)+j) = fstrSOLID%ESTRESS(14*ntot_lyr*i+j-5)
          !  enddo
          !enddo
          !call ppohFEM_result_add( id, nitem, label, work )

        else if( fstrSOLID%output_ctrl(3)%outinfo%on(8) ) then
          nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(8), ndof )
          label = 'ElementalMISES'
          do i = 1, ppohFEM_get_n_elem(idx_mesh)
            work(i) = fstrSOLID%ESTRESS((mdof+1)*i)
          enddo
          call ppohFEM_result_add( id, nitem, label, work )
        endif

      endif
    enddo

! --- STRAIN @gauss
    if( fstrSOLID%output_ctrl(3)%outinfo%on(9) .and. ndof/=6 ) then
      id = 2
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(9), ndof )
      ngauss = ppohFEM_get_num_quadpoints_of_element( ppohFEM_get_elem_type_item(idx_mesh, 1) )
      do k = 1, ngauss
        write(s,*) k
        write(label,'(a,a)') 'GaussSTRAIN',trim(adjustl(s))
        label = adjustl(label)
        do i = 1, ppohFEM_get_n_elem(idx_mesh)
         if( size(fstrSOLID%elements(i)%gausses) .lt. ngauss ) then
           do j = 1, nitem
            work(nitem*(i-1)+j) = 0.0D0
           enddo
         else
           do j = 1, nitem
            work(nitem*(i-1)+j) = fstrSOLID%elements(i)%gausses(k)%strain(j)
           enddo
         endif
        enddo
        call ppohFEM_result_add( id, nitem, label, work )
      enddo
    endif
! --- STRESS @gauss
    if( fstrSOLID%output_ctrl(3)%outinfo%on(10) .and. ndof/=6 ) then
      id = 2
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(10), ndof )
      ngauss = ppohFEM_get_num_quadpoints_of_element( ppohFEM_get_elem_type_item(idx_mesh, 1) )
      do k = 1, ngauss
        write(s,*) k
        write(label,'(a,a)') 'GaussSTRESS',trim(adjustl(s))
        label = adjustl(label)
        do i = 1, ppohFEM_get_n_elem(idx_mesh)
         if( size(fstrSOLID%elements(i)%gausses) .lt. ngauss ) then
           do j = 1, nitem
            work(nitem*(i-1)+j) = 0.0D0
           enddo
         else
           do j = 1, nitem
            work(nitem*(i-1)+j) = fstrSOLID%elements(i)%gausses(k)%stress(j)
           enddo
         endif
        enddo
        call ppohFEM_result_add( id, nitem, label, work )
      enddo
    endif
! --- PLASTIC STRAIN @gauss
    if( fstrSOLID%output_ctrl(3)%outinfo%on(11) .and. fstrSOLID%StaticType/=3 ) then
      id = 2
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(11), ndof )
      ngauss = ppohFEM_get_num_quadpoints_of_element( ppohFEM_get_elem_type_item(idx_mesh, 1) )
      do k = 1, ngauss
        write(s,*) k
        write(label,'(a,a)') 'PLASTIC_GaussSTRAIN',trim(adjustl(s))
        label = adjustl(label)
        do i = 1, ppohFEM_get_n_elem(idx_mesh)
          work(i) = fstrSOLID%elements(i)%gausses(k)%plstrain
        enddo
        call ppohFEM_result_add( id, nitem, label, work )
      enddo
    endif
! --- THERMAL STRAIN @node
    if( fstrSOLID%output_ctrl(3)%outinfo%on(12) .and. associated(tnstrain) ) then
      id = 1
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(12), ndof )
      label = 'THERMAL_NodalSTRAIN'
      call ppohFEM_result_add( id, nitem, label, tnstrain )
    endif
! --- THERMAL STRAIN @element
    if( fstrSOLID%output_ctrl(3)%outinfo%on(13) .and. associated(testrain) ) then
      id = 2
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(13), ndof )
      label = 'THERMAL_ElementalSTRAIN'
      call ppohFEM_result_add( id, nitem, label, testrain )
    endif
! --- THERMAL STRAIN @gauss
    if( fstrSOLID%output_ctrl(3)%outinfo%on(14) .and. associated(testrain) ) then
      id = 2
      nitem = n_comp_valtype( fstrSOLID%output_ctrl(3)%outinfo%vtype(14), ndof )
      ngauss = ppohFEM_get_num_quadpoints_of_element( ppohFEM_get_elem_type_item(idx_mesh, 1) )
      do k = 1, ngauss
        write(s,*) k
        write(label,'(a,a)') 'THERMAL_GaussSTRAIN',trim(adjustl(s))
        label = adjustl(label)
        do i = 1, ppohFEM_get_n_elem(idx_mesh)
          do j = 1, nitem
!                work(nitem*(i-1)+j) = fstrSOLID%elements(i)%gausses(k)%tstrain(j)
          enddo
        enddo
        call ppohFEM_result_add( id, nitem, label, work )
      enddo
    endif
! --- WRITE
    nameID = 'fstrRES'
    if( flag==0 ) then
      call ppohFEM_result_write_by_name( nameID )
    else
      addfname = '_dif'
      call ppohFEM_result_write_by_addfname( nameID, addfname )
    endif
! --- FINALIZE
    call ppohFEM_result_finalize

    deallocate( work )
  end subroutine fstr_write_static_result

!C***
!>  MAKE RESULT for static analysis (WITHOUT ELEMENTAL RESULTS) --------------------------------------------------------------
!C***
  subroutine fstr_make_static_result( idx_mesh, fstrSOLID, ndof, fstrRESULT, tnstrain, testrain )
    use m_fstr
    implicit none
    integer, intent(IN)       :: idx_mesh
    type (fstr_solid)         :: fstrSOLID
    integer, intent(IN)       :: ndof
    type (hecmwST_result_data):: fstrRESULT
    real(kind=kreal), pointer :: tnstrain(:), testrain(:)
    character(len=4) :: char_n_lyr
    real(kind=kreal), allocatable   ::unode781(:),rnode781(:)

    integer(kind=kint) :: n_lyr, ntot_lyr, mixedflag, aa, com_total_layer, it, shell33_coeff
    integer(kind=kint) :: i, j, mdof, ncomp, nitem, iitem, ecomp, eitem, jitem, nn, mm

    com_total_layer = 0
    ntot_lyr = 1
    mixedflag = 0
    aa = 0
    shell33_coeff=1;

    mm = ppohFEM_get_n_node(idx_mesh)
    if( ppohFEM_get_n_elem(idx_mesh) > ppohFEM_get_n_node(idx_mesh) ) mm = ppohFEM_get_n_elem(idx_mesh)

    if( ndof==2 ) mdof = 3
    if( ndof==3 ) mdof = 6
    if( ndof==6 ) mdof = 12
    if( ndof==6 ) shell33_coeff = 2

    do it = 1, ppohFEM_get_n_material(idx_mesh)
      com_total_layer =  int(fstrSOLID%materials(it)%variables(M_TOTAL_LAYER))
      if (com_total_layer >= ntot_lyr)then
        ntot_lyr = com_total_layer
      endif
    enddo

    do it=1, ppohFEM_get_n_elem_type(idx_mesh)
      aa =  ppohFEM_get_elem_type_item(idx_mesh, it)
      if (aa == 641)then
        if (mixedflag == 0) then
          mixedflag = 2
        endif
      elseif (aa == 781 .or. aa == 761)then
        mixedflag = 1
        shell33_coeff=2
      endif
      if (mixedflag == 1)then
        mdof = 12
      endif
    enddo
    n_lyr = 1

    call ppohFEM_nullify_result_data( fstrRESULT )
    ncomp = 0
    nitem = 0
    ecomp = 0
    eitem = 0

! --- DISPLACEMENT
        if( fstrSOLID%output_ctrl(4)%outinfo%on(1) ) then
          ncomp = ncomp + 1
          nitem = nitem + n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(1), ndof )
        endif
! --- ROTATION (Only for 781 shell)
        if( fstrSOLID%output_ctrl(4)%outinfo%on(18) .and. mixedflag==1 ) then
          ncomp = ncomp + 1
          nitem = nitem + n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(18), ndof )
        endif
! --- REACTION FORCE
        if( fstrSOLID%output_ctrl(4)%outinfo%on(2) ) then
          ncomp = ncomp + 1
          nitem = nitem + n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(2), ndof )
        endif
! --- STRAIN @node
! --- shell has + and -, shell33_coeff=2
    DO n_lyr = 1,ntot_lyr
    if( fstrSOLID%output_ctrl(4)%outinfo%on(3) ) then
      if( ndof==6 ) then
        ncomp = ncomp + 2
        nitem = nitem + mdof
      else
        ncomp = ncomp + shell33_coeff*1
        nitem = nitem + shell33_coeff*n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(3), ndof )
      endif
    endif
! --- STRESS @node
    if( fstrSOLID%output_ctrl(4)%outinfo%on(4) ) then
      if( ndof==6 ) then
        ncomp = ncomp + 2
        nitem = nitem + mdof
      else
        ncomp = ncomp + shell33_coeff*1
        nitem = nitem + shell33_coeff*n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(4), ndof )
      endif
    endif

! --- MISES @node
    if( fstrSOLID%output_ctrl(4)%outinfo%on(5) ) then
      if( ndof==6 ) then
        ncomp = ncomp + 2
        nitem = nitem + mdof
      else
        ncomp = ncomp + shell33_coeff*1
        nitem = nitem + shell33_coeff*n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(5), ndof )
      endif
    endif

    ENDDO
! --- THERMAL STRAIN @node
        if( fstrSOLID%output_ctrl(4)%outinfo%on(12) .and. associated(tnstrain) ) then
          ncomp = ncomp + 1
          nitem = nitem + n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(12), ndof )
        endif

! shell-solid mixed mesh is not supported !DEBUG
!      if (mixedflag == 1)then
!        ndof = 3
!      endif

      fstrRESULT%nn_component = ncomp
      fstrRESULT%ne_component = ecomp
      allocate( fstrRESULT%nn_dof(ncomp) )
      allocate( fstrRESULT%node_label(ncomp) )
      allocate( fstrRESULT%node_val_item(shell33_coeff*nitem*ppohFEM_get_n_node(idx_mesh)) )
      allocate( fstrRESULT%ne_dof(ecomp) )
      allocate( fstrRESULT%elem_label(ecomp) )
      allocate( fstrRESULT%elem_val_item(eitem*ppohFEM_get_n_elem(idx_mesh)) )
      ncomp = 0
      iitem = 0
      ecomp = 0
      jitem = 0

! --- DISPLACEMENT
    if (fstrSOLID%output_ctrl(4)%outinfo%on(1) ) then
      if (mixedflag == 1) then
write(*,*) 'shell-solid mixed mesh is not supported' !DEBUG
call ppohFEM_abort !DEBUG
!        allocate( unode781(3*mm) )
!        allocate( rnode781(3*mm) )
!        unode781 = 0.0d0
!        rnode781 = 0.0d0
!
!        call reorder_node781(fstrSOLID, hecMESH, unode781, rnode781, mixedflag)
!        ncomp = ncomp + 1
!        fstrRESULT%nn_dof(ncomp) = 3
!        fstrRESULT%node_label(ncomp) = 'DISPLACEMENT'
!
!        do i = 1, hecMESH%n_node
!          do j = 1, 3
!          fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = unode781(3*(i-1)+j)
!          enddo
!        enddo
!        iitem = iitem + 3
!
!        if (fstrSOLID%output_ctrl(4)%outinfo%on(18)) then
!          ncomp = ncomp + 1
!          fstrRESULT%nn_dof(ncomp) = 3
!          fstrRESULT%node_label(ncomp) = 'ROTATION'
!          do i = 1, hecMESH%n_node
!            do j = 1, 3
!            fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = rnode781(3*(i-1)+j)
!            enddo
!          enddo
!          iitem = iitem + 3
!        endif
!
!        deallocate( unode781 )
!        deallocate( rnode781 )
!
!      elseif(mixedflag == 2) then
!
!        allocate( unode781(1) )
!        allocate( rnode781(1) )
!        unode781 = 0.0d0
!        rnode781 = 0.0d0
!        call reorder_node781(fstrSOLID, hecMESH, unode781, rnode781, mixedflag)
!        ncomp = ncomp + 1
!        fstrRESULT%nn_dof(ncomp) = 3
!        fstrRESULT%node_label(ncomp) = 'DISPLACEMENT'
!        do i = 1, hecMESH%n_node
!              do j = 1, 3
!                fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%unode(3*(i-1)+j)
!              enddo
!        enddo
!        iitem = iitem + 3
!        deallocate( unode781 )
!        deallocate( rnode781 )
!
     else

          ncomp = ncomp + 1
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(1), ndof )
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'DISPLACEMENT'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, nn
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%unode(nn*(i-1)+j)
            enddo
          enddo
          iitem = iitem + nn
      endif
    endif
! --- REACTION FORCE
        if( fstrSOLID%output_ctrl(4)%outinfo%on(2) ) then
          ncomp = ncomp + 1
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(2), ndof )
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'REACTION_FORCE'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, nn
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%QFORCE(nn*(i-1)+j)
            enddo
          enddo
          iitem = iitem + nn
        endif
! --- STRAIN @node

    DO n_lyr = 1,ntot_lyr
      write(char_n_lyr, '(i0)') n_lyr
      if( fstrSOLID%output_ctrl(4)%outinfo%on(3)) then
        if( mixedflag==1  ) then
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(3), ndof )

          ncomp = ncomp + 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalSTRAIn_lyr'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%STRAIN(12*ntot_lyr*(i-1)+12*(n_lyr-1)+j)
            enddo
          enddo
          iitem = iitem + nn

          ncomp = ncomp + 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalSTRAIn_lyr'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%STRAIN(12*ntot_lyr*(i-1)+12*(n_lyr-1)+j+6)
            enddo
          enddo
          iitem = iitem + nn

        else
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(3), ndof )
          ncomp = ncomp + 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalSTRAIN'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, nn
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%STRAIN(nn*(i-1)+j)
            enddo
          enddo
          iitem = iitem + nn
        endif
      endif


! --- STRESS @node
      if(fstrSOLID%output_ctrl(4)%outinfo%on(4)) then

        if(ndof==6 .or. mixedflag==1 ) then
          nn = 6
          ncomp = ncomp + 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalSTRESS_Layer'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+j)
            enddo
          enddo
          iitem = iitem + nn

          ncomp = ncomp + 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalSTRESS_Layer'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, 6
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+j+6)
            enddo
          enddo
          iitem = iitem + nn

        else
          ncomp = ncomp + 1
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(4), ndof )
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalSTRESS'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, nn
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = fstrSOLID%STRESS((nn+1)*(i-1)+j)
            enddo
          enddo
          iitem = iitem + nn
        endif
      endif
! --- MISES @node
      if(fstrSOLID%output_ctrl(4)%outinfo%on(5)) then

        if( (ndof==6 .or. mixedflag==1 ) ) then
          ncomp = ncomp + 1
          nn = 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalMISES_Layer'//trim(char_n_lyr)//'+'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            fstrRESULT%node_val_item(nitem*(i-1)+1+iitem) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+13)
          enddo
          iitem = iitem + nn

          ncomp = ncomp + 1
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalMISES_Layer'//trim(char_n_lyr)//'-'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            fstrRESULT%node_val_item(nitem*(i-1)+1+iitem) = fstrSOLID%STRESS(14*ntot_lyr*(i-1)+14*(n_lyr-1)+14)
          enddo
          iitem = iitem + nn

        else
          ncomp = ncomp + 1
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(5), ndof )
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'NodalMISES'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            fstrRESULT%node_val_item(nitem*(i-1)+1+iitem) = fstrSOLID%STRESS((mdof+1)*i)
          enddo
          iitem = iitem + nn
        endif

      endif
    ENDDO
! --- THERMAL STRAIN @node
        if( fstrSOLID%output_ctrl(4)%outinfo%on(12) .and. associated(tnstrain) ) then
          ncomp = ncomp + 1
          nn = n_comp_valtype( fstrSOLID%output_ctrl(4)%outinfo%vtype(12), ndof )
          fstrRESULT%nn_dof(ncomp) = nn
          fstrRESULT%node_label(ncomp) = 'THERMAL_NodalSTRAIN'
          do i = 1, ppohFEM_get_n_node(idx_mesh)
            do j = 1, nn
              fstrRESULT%node_val_item(nitem*(i-1)+j+iitem) = tnstrain(nn*(i-1)+j)
            enddo
          enddo
          iitem = iitem + nn
        endif

      end subroutine fstr_make_static_result


! --- Reorder unode for mixed solid-shell analysis	----------
!  subroutine reorder_node781(fstrSOLID, hecMESH, unode781,rnode781, mixedflag)
!    use m_fstr
!    use m_out
!    use m_static_lib
!    implicit none
!    type (fstr_solid)         :: fstrSOLID
!    type (hecmwST_local_mesh) :: hecMESH
!    integer(kind=kint) :: i, j, k, itype, iS, iE, ic_type, jS, icel, nodLOCAL(9), mixedflag
!    integer(kind=kint) :: mm, n1, n2
!    real(kind=kreal), allocatable   :: unode781(:)
!    real(kind=kreal), allocatable   :: rnode781(:)
!
!    integer, parameter :: idx_mesh = 1
!
!  if(mixedflag == 1)then
!
!    do itype = 1, ppohFEM_get_n_elem_type(idx_mesh)
!       iS= ppohFEM_get_elem_type_index(idx_mesh, itype-1) + 1
!       iE= ppohFEM_get_elem_type_index(idx_mesh, itype  )
!       ic_type= ppohFEM_get_elem_type_item(idx_mesh, itype)
!      if(ic_type == 781)then
!        do icel = iS, iE
!          jS = hecMESH%elem_node_index(icel-1)
!          do j = 1, 4
!            n1 = hecMESH%elem_node_item(jS+j)
!            n2 = hecMESH%elem_node_item(jS+j+4)
!
!            unode781(3*n1-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n1-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n1  ) = fstrSOLID%unode(3*n1  )
!            unode781(3*n2-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n2-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n2  ) = fstrSOLID%unode(3*n1  )
!
!            rnode781(3*n1-2) = fstrSOLID%unode(3*n2-2)
!            rnode781(3*n1-1) = fstrSOLID%unode(3*n2-1)
!            rnode781(3*n1  ) = fstrSOLID%unode(3*n2  )
!            rnode781(3*n2-2) = fstrSOLID%unode(3*n2-2)
!            rnode781(3*n2-1) = fstrSOLID%unode(3*n2-1)
!            rnode781(3*n2  ) = fstrSOLID%unode(3*n2  )
!          enddo
!        enddo
!      elseif(ic_type == 761)then
!        do icel = iS, iE
!          jS = hecMESH%elem_node_index(icel-1)
!          do j = 1, 3
!            n1 = hecMESH%elem_node_item(jS+j)
!            n2 = hecMESH%elem_node_item(jS+j+3)
!
!            unode781(3*n1-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n1-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n1  ) = fstrSOLID%unode(3*n1  )
!            unode781(3*n2-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n2-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n2  ) = fstrSOLID%unode(3*n1  )
!
!            rnode781(3*n1-2) = fstrSOLID%unode(3*n2-2)
!            rnode781(3*n1-1) = fstrSOLID%unode(3*n2-1)
!            rnode781(3*n1  ) = fstrSOLID%unode(3*n2  )
!            rnode781(3*n2-2) = fstrSOLID%unode(3*n2-2)
!            rnode781(3*n2-1) = fstrSOLID%unode(3*n2-1)
!            rnode781(3*n2  ) = fstrSOLID%unode(3*n2  )
!          enddo
!        enddo
!      elseif(ic_type == 641)then
!        do icel = iS, iE
!          jS = hecMESH%elem_node_index(icel-1)
!          do j = 1, 2
!            n1 = hecMESH%elem_node_item(jS+j)
!            n2 = hecMESH%elem_node_item(jS+j+2)
!
!            unode781(3*n1-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n1-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n1  ) = fstrSOLID%unode(3*n1  )
!            unode781(3*n2-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n2-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n2  ) = fstrSOLID%unode(3*n1  )
!
!            rnode781(3*n1-2) = fstrSOLID%unode(3*n2-2)
!            rnode781(3*n1-1) = fstrSOLID%unode(3*n2-1)
!            rnode781(3*n1  ) = fstrSOLID%unode(3*n2  )
!            rnode781(3*n2-2) = fstrSOLID%unode(3*n2-2)
!            rnode781(3*n2-1) = fstrSOLID%unode(3*n2-1)
!            rnode781(3*n2  ) = fstrSOLID%unode(3*n2  )
!          enddo
!        enddo
!      else
!        do icel = iS, iE
!          jS = hecMESH%elem_node_index(icel-1)
!          mm = ppohFEM_get_max_node_of_elem_type(ic_type)
!          do j = 1, mm
!            n1 = hecMESH%elem_node_item(jS+j)
!            unode781(3*n1-2) = fstrSOLID%unode(3*n1-2)
!            unode781(3*n1-1) = fstrSOLID%unode(3*n1-1)
!            unode781(3*n1  ) = fstrSOLID%unode(3*n1  )
!          enddo
!        enddo
!      endif
!    enddo
!  elseif(mixedflag == 2)then
!    do itype = 1, hecMESH%n_elem_type
!      iS = hecMESH%elem_type_index(itype-1) + 1
!      iE = hecMESH%elem_type_index(itype  )
!      ic_type = hecMESH%elem_type_item(itype)
!      if(ic_type == 641)then
!        do icel = iS, iE
!          jS = hecMESH%elem_node_index(icel-1)
!          do j = 1, 2
!            nodLOCAL(j) = hecMESH%elem_node_item(jS+j)
!            nodLOCAL(j+2) = hecMESH%elem_node_item(jS+j+2)
!            fstrSOLID%unode(3*nodLOCAL(j+2)-2) = fstrSOLID%unode(3*nodLOCAL(j)-2)
!            fstrSOLID%unode(3*nodLOCAL(j+2)-1) = fstrSOLID%unode(3*nodLOCAL(j)-1)
!            fstrSOLID%unode(3*nodLOCAL(j+2)  ) = fstrSOLID%unode(3*nodLOCAL(j)  )
!            fstrSOLID%STRESS(7*nodLOCAL(j  )) = 0.0
!            fstrSOLID%STRESS(7*nodLOCAL(j+2)) = 0.0
!          enddo
!        enddo
!      endif
!    enddo
!  endif
!
!  end subroutine reorder_node781

end module m_static_make_result
