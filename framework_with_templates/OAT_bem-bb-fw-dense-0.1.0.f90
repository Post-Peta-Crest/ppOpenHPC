!=====================================================================*
!                                                                     *
!   Software Name : ppohBEM                                           *
!         Version : 0.1                                               *
!                                                                     *
!   License                                                           *
!     This file is part of ppohBEM.                                   *
!     ppohBEM is a free software, you can use it under the terms   *
!     of The MIT License (MIT). See LICENSE file and User's guide     *
!     for more details.                                               *
!                                                                     *
!   ppOpen-HPC project:                                               *
!     Open Source Infrastructure for Development and Execution of     *
!     Large-Scale Scientific Applications on Post-Peta-Scale          *
!     Supercomputers with Automatic Tuning (AT).                      *
!                                                                     *
!   Organizations:                                                    *
!     The University of Tokyo                                         *
!       - Information Technology Center                               *
!       - Atmosphere and Ocean Research Institute (AORI)              *
!       - Interfaculty Initiative in Information Studies              *
!         /Earthquake Research Institute (ERI)                        *
!       - Graduate School of Frontier Science                         *
!     Kyoto University                                                *
!       - Academic Center for Computing and Media Studies             *
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
!                                                                     *
!   Sponsorship:                                                      *
!     Japan Science and Technology Agency (JST), Basic Research       *
!     Programs: CREST, Development of System Software Technologies    *
!     for post-Peta Scale High Performance Computing.                 *
!                                                                     *
!   Copyright (c) 2012 <Takeshi Iwashita, Takeshi Mifune, Yuki Noseda,*
!                    Yasuhito Takahashi, Masatoshi Kawai, Akihiro Ida>*
!                                                                     *
!=====================================================================*

program bem_bb_dense_mpi
  implicit none
  include 'mpif.h'

  integer   irank, nrank
  integer   comm, ierr
  integer   iunit

  real*8  st1, st2, st3, st4, time1, time2, time3, time4

  integer   max_steps
  integer   number_element_dof
  character linear_solver*16
  real*8    tor

  character filename*256

  integer   nond, nofc, nond_on_face
  integer   nint_para_fc, ndble_para_fc

  type :: coordinate
     real(8) :: x ,y ,z
  end type coordinate
  type (coordinate), dimension(:), allocatable :: np

  integer, dimension(:,:), allocatable :: face2node
  integer, dimension(:,:), allocatable :: int_para_fc
  real*8,  dimension(:,:), allocatable :: dble_para_fc

  integer  ndim, steps
  integer  pbicgstab,ppohBEM_pbicgstab_dense
  real*8, dimension(:,:), allocatable :: a
  real*8, dimension(:),   allocatable :: rhs
  real*8, dimension(:),   allocatable :: sol

  integer  ext_ndim
  integer  lhp, ltp, proc_dim
  integer  act_nrank

  integer  i
  integer, dimension(:), allocatable :: act_nranks
  integer  MPI_GROUP_WORLD, act_grp
  integer  act_comm

!      integer iusw1_ppohBEMresidual_direct
!      integer iusw1_ppohBEMmatvec_direct



  comm = MPI_COMM_WORLD; ierr = 0

  call Initialization_MPI( comm, nrank, irank, ierr )

  if( ierr .ne. 0 ) then
    print*, 'Error: MPI_Init failed'
    goto 1000
  endif


     if (irank == 0) then
        open(13, status = 'replace', &
     &     file = 'OAT_bem-bb-fw-dense_timings.dat', &
     &     action = 'write', pad= 'yes')
      endif




  call Read_bem_bb_config( number_element_dof, linear_solver, tor, max_steps, irank, comm, ierr )

  if( ierr .ne. 0 ) then
    print*, 'Error: in reading bem_bb_config.txt'
    goto 1000
  endif

  call MPI_Barrier( comm, ierr )
  if( ierr .ne. 0 ) then
    print*, 'Error: MPI_Barrier #1'
    goto 1000
  endif

  st1 = MPI_Wtime()


   filename="sample.input.txt"
!  filename="BEM-Inputs/input.600.txt" 
!  filename="BEM-Inputs/input.2400.txt" 
!  filename="BEM-Inputs/input.21600.txt" 

  if( irank .eq. 0 ) then
    print*, 'Sample model data file is used.'
  endif


  iunit = 10

  call Read_model_data( filename, number_element_dof, &
                        nond, nofc, nond_on_face, nint_para_fc, ndble_para_fc, &
                        np, face2node, int_para_fc, dble_para_fc, iunit, irank, comm, ierr )


  call MPI_Barrier( comm, ierr )
  if(ierr.ne.0) then
    print*, 'Error: MPI_Barrier #2'
    goto 1000
  endif

  st2 = MPI_Wtime()


  call Make_equation_data( number_element_dof, &
                           nond, nofc, nond_on_face, nint_para_fc, ndble_para_fc, &
                           np, face2node, int_para_fc, dble_para_fc, &
                           ndim, a, rhs, sol, &
                           ext_ndim, lhp, ltp, proc_dim, irank, nrank, act_nrank, ierr )


  call MPI_Barrier( comm, ierr )
  if(ierr.ne.0) then
    print*, 'Error: MPI_Barrier #3'
    goto 1000
  endif

  st3 = MPI_Wtime()


  allocate( act_nranks(act_nrank), stat=ierr )
  if( ierr .ne. 0 ) then
    print*, 'Memory allocation #01 failed'
    goto 1000
  endif

  do i = 1, act_nrank
    act_nranks(i) = i - 1
  enddo

  call MPI_Comm_group( MPI_COMM_WORLD, MPI_GROUP_WORLD, ierr )
  call MPI_Group_incl( MPI_GROUP_WORLD, act_nrank, act_nranks, act_grp, ierr )
  call MPI_Comm_create( MPI_COMM_WORLD, act_grp, act_comm, ierr )




  if( irank .lt. act_nrank ) then
    if( linear_solver == "BICGSTAB" ) then
      steps = ppohBEM_pbicgstab_dense( irank, nrank, lhp, ltp, &
		& proc_dim, ndim, ext_ndim, a, rhs, sol, tor, &
		& max_steps, act_comm, ierr )
    endif
  endif

  call MPI_Comm_free( act_comm, ierr )
  call MPI_Group_free( MPI_GROUP_WORLD, ierr )
  call MPI_Group_free( act_grp, ierr )


  call MPI_Barrier( comm, ierr )
  if(ierr.ne.0) then
    print*, 'Error: MPI_Barrier #4'
    goto 1000
  endif

  st4 = MPI_Wtime()


  filename="sample.output.vtk"
  iunit = 11
  call Print_result( filename, nond, nofc, nond_on_face, np, face2node, &
                     ndim, sol, irank, iunit, ierr )


  time1 = st2-st1
  time2 = st3-st2
  time3 = st4-st3
  time4 = st4-st1

  if ( irank .eq. 0 ) then
    print*,'before user_func time = ',sngl(time1),' [sec]'
    print*,'       user_func time = ',sngl(time2),' [sec]'
    print*,' '
    print*,'        bicgstab time = ',sngl(time3),' [sec]'
    if( steps .ne. 0 ) then
      print*,'  bicgstab time/steps = ',sngl(time3/dble(steps)),' [sec/times]'
    endif
    print*,' '
    print*,'_____________all time = ',sngl(time4),' [sec]'
    print*,' '
    print*,' '
    print*, sngl(time2)
    print*, sngl(time3/dble(steps))
    print*, sngl(time4)

    write(13,'(A,F9.4,A)') '        bicgstab time = ',sngl(time3),' [sec]'
    write(13,'(A,F9.4,A)') '  bicgstab time/steps = ',sngl(time3/dble(steps)),' [sec/times]'
    write(13,'(A,F9.4,A)') '_____________all time = ',sngl(time4),' [sec]'
    write(13,'(F9.4)') sngl(time3)
    write(13,'(F9.4)') sngl(time3/dble(steps))
    write(13,'(F9.4)') sngl(time4)
    close(13, status = 'keep')
  endif



1000  continue

  if(allocated(np))            deallocate(np)
  if(allocated(face2node))     deallocate(face2node)
  if(allocated(int_para_fc))   deallocate(int_para_fc)
  if(allocated(dble_para_fc))  deallocate(dble_para_fc)

  if(allocated(a))             deallocate(a)
  if(allocated(rhs))           deallocate(rhs)
  if(allocated(sol))           deallocate(sol)

  if(allocated(act_nranks))    deallocate(act_nranks)

  call MPI_Finalize (ierr)

  stop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    subroutines    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!    Initialization_MPI    !!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Initialization_MPI( comm, nrank, irank, ierr )
    integer, intent(out) :: irank
    integer, intent(out) :: nrank
    integer, intent(in)  :: comm
    integer, intent(out) :: ierr

    call MPI_Init ( ierr )
    if( ierr .ne. 0 ) then
      print*, 'Error: MPI_Init failed !!!'
    endif

    call MPI_Comm_size ( comm, nrank, ierr )
    if( ierr .ne. 0 ) then
      print*, 'Error: MPI_Comm_size failed !!!'
    endif

    call MPI_Comm_rank ( comm, irank, ierr )
    if( ierr .ne. 0 ) then
      print*, 'Error: MPI_Comm_rank failed !!!'
    endif

    if (irank.eq.0) then
      print*, 'Number of processes ', nrank
    endif

    call MPI_Barrier( comm, ierr )
    if( ierr .ne. 0 ) then
      print*, 'Error: MPI_Barrier #1 !!!'
    endif

  end subroutine   !!!! subroutine Initialize_MPI


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!    Read_bem_bb_config    !!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Read_bem_bb_config( number_element_dof, linear_solver, tor, max_steps, irank, comm, ierr )

    integer,   intent(out) :: number_element_dof
    character, intent(out) :: linear_solver*16
    real*8,    intent(out) :: tor
    integer,   intent(out) :: max_steps
    integer,   intent(in)  :: irank
    integer,   intent(in)  :: comm
    integer,   intent(out) :: ierr

    if( irank .eq. 0 ) then
      open( 11, file="bem-bb-config.txt", action='read', iostat=ierr )
      read( 11, * ) number_element_dof
      read( 11, * ) linear_solver
      read( 11, * ) tor
      read( 11, * ) max_steps
      close( 11 )

      write(*,*) "Number of unknowns set on each face element = ", number_element_dof
      write(*,*) "Selected linear solver = ", linear_solver
      write(*,*) "Convergence criterion = ", tor
      write(*,*) "Upper limit of iteration counts = ", max_steps

    endif

    call MPI_Bcast ( number_element_dof, 1, MPI_INTEGER, 0, comm, ierr )
    call MPI_Bcast ( linear_solver, 16, MPI_CHARACTER, 0, comm, ierr )
    call MPI_Bcast ( tor, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr )
    call MPI_Bcast ( max_steps, 1, MPI_INTEGER, 0, comm, ierr )

  end subroutine   !!!! subroutine Read_bem_bb_config


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!    Read_model_data    !!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Read_model_data( filename, number_element_dof, &
                              nond, nofc, nond_on_face, nint_para_fc, ndble_para_fc, &
                              np, face2node, int_para_fc, dble_para_fc, &
                              iunit, irank, comm, ierr )

    character, intent(in) :: filename*256
    integer,   intent(in)  :: number_element_dof

    integer, intent(out) :: nond
    integer, intent(out) :: nofc
    integer, intent(out) :: nond_on_face
    integer, intent(out) :: nint_para_fc
    integer, intent(out) :: ndble_para_fc

    type (coordinate), dimension(:),   allocatable, intent(out) :: np
    integer,           dimension(:,:), allocatable, intent(out) :: face2node
    integer,           dimension(:,:), allocatable, intent(out) :: int_para_fc
    real*8,            dimension(:,:), allocatable, intent(out) :: dble_para_fc

    integer, intent(in)  :: iunit
    integer, intent(in)  :: irank
    integer, intent(in)  :: comm
    integer, intent(out) :: ierr

    integer  i, j
    integer  dims(5)



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Case: irank is equal to 0 (Master processes) !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( irank .eq. 0 ) then
      open( iunit, file=filename, action='read', pad='yes', iostat=ierr )
      if( ierr .ne. 0 ) then
        print*, 'input.txt does not exists'
      endif

    !!!!  Read number of nodes from input data file : nond  !!!!
      read( iunit, * ) nond

    !!!!  Allocation for the array for the coordinates of the nodes : np  !!!!
      allocate( np(nond), stat=ierr )
      if( ierr .ne. 0 ) then
        print*, 'Memory allocation #1 failed'
      endif

    !!!!  Read the coordinates of the nodes from input data file : np  !!!!
      do i = 1, nond
        read( iunit, * ) np(i)%x, np(i)%y, np(i)%z
      enddo

    !!!!  Read number of faces from input data file : nofc  !!!!
      read( iunit, * ) nofc

    !!!!  Read number of nodes on each face from input data file : nond_on_face  !!!!
      read( iunit, * ) nond_on_face

    !!!!  Read number of integer parameters set on each face from input data file : nint_para_fc  !!!!
      read( iunit, * ) nint_para_fc

    !!!!  Read number of double precision parameters set on each face from input data file : ndble_para_fc  !!!!
      read( iunit, * ) ndble_para_fc

      dims(1) = nond
      dims(2) = nofc
      dims(3) = nond_on_face
      dims(4) = nint_para_fc
      dims(5) = ndble_para_fc

      call MPI_Bcast( dims(1), 5, MPI_INTEGER, 0, comm, ierr )
      if( ierr .ne. 0 ) then
        print*, 'Error: MPI_Bcast#11'
      endif

      call MPI_Bcast( np(1)%x, nond*3, MPI_DOUBLE_PRECISION, 0, comm, ierr )
      if( ierr .ne. 0 ) then
        print*, 'Error: MPI_Bcast#12'
      endif

      allocate( face2node(nond_on_face,nofc), stat=ierr )
      if( ierr .ne. 0 ) then
        print*, 'Memory allocation #11 failed'
      endif
      do i = 1, nofc
        read( iunit, * ) (face2node(j,i), j=1,nond_on_face)
      enddo
      call MPI_Bcast ( face2node(1,1), nofc*nond_on_face, MPI_INTEGER, 0, comm, ierr )
      if( ierr .ne. 0 ) then
        print*, 'Error: MPI_Bcast#13'
      endif

      if( nint_para_fc .gt. 0 ) then
        allocate( int_para_fc(nint_para_fc,nofc), stat=ierr )
        if( ierr .ne. 0 ) then
          print*, 'Memory allocation #12 failed'
        endif
        do i = 1, nofc
          read( iunit, * ) (int_para_fc(j,i), j=1,nint_para_fc)
        enddo
        call MPI_Bcast ( int_para_fc(1,1), nofc*nint_para_fc, MPI_INTEGER, 0, comm, ierr )
        if( ierr .ne. 0 ) then
          print*, 'Error: MPI_Bcast#14 !!!'
        endif
      endif

      if( ndble_para_fc .gt. 0 ) then
        allocate( dble_para_fc(ndble_para_fc,nofc), stat=ierr )
        if( ierr .ne. 0 ) then
          print*, 'Memory allocation #13 failed'
        endif
        do i = 1, nofc
          read( iunit, * ) (dble_para_fc(j,i), j=1,ndble_para_fc)
        enddo
        call MPI_Bcast ( dble_para_fc(1,1), nofc*ndble_para_fc, MPI_DOUBLE_PRECISION, 0, comm, ierr )
        if( ierr .ne. 0 ) then
          print*, 'Error: MPI_Bcast#15 !!!'
        endif
      endif

      close ( iunit )

    else
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Case: irank is not equal to 0 (Worker processes) !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call MPI_Bcast( dims(1), 5, MPI_INTEGER, 0, comm, ierr )
      if( ierr .ne. 0 ) then
        print*, 'Error: MPI_Bcast#21 !!!'
      endif

      nond = dims(1)
      nofc = dims(2)
      nond_on_face  = dims(3)
      nint_para_fc  = dims(4)
      ndble_para_fc = dims(5)

      allocate( np(nond), stat=ierr )
      if( ierr .ne. 0 ) then
        print*, 'Memory allocation #21 failed !!!'
      endif
      call MPI_Bcast( np(1)%x, nond*3, MPI_DOUBLE_PRECISION, 0, comm, ierr )
      if( ierr .ne. 0 ) then
        print*, 'Error: MPI_Bcast#22'
      endif

      allocate( face2node(nond_on_face,nofc), stat=ierr )
      if( ierr .ne. 0 ) then
        print*, 'Memory allocation #22 failed !!!'
      endif
      call MPI_Bcast ( face2node(1,1), nofc*nond_on_face, MPI_INTEGER, 0, comm, ierr )
      if( ierr .ne. 0 ) then
        print*, 'Error: MPI_Bcast#23'
      endif

      if ( nint_para_fc .gt. 0 ) then
        allocate( int_para_fc(nint_para_fc,nofc), stat=ierr )
        if( ierr .ne. 0 ) then
          print*, 'Memory allocation #23 failed !!!'
        endif
        call MPI_Bcast ( int_para_fc(1,1), nofc*nint_para_fc, MPI_INTEGER, 0, comm, ierr )
        if( ierr .ne. 0 ) then
          print*, 'Error: MPI_Bcast#24 !!!'
        endif
      endif
      if ( ndble_para_fc .gt. 0 ) then
        allocate( dble_para_fc(ndble_para_fc,nofc), stat=ierr )
        if( ierr .ne. 0 ) then
          print*, 'Memory allocation #24 failed !!!'
        endif
        call MPI_Bcast ( dble_para_fc(1,1), nofc*ndble_para_fc, MPI_DOUBLE_PRECISION, 0, comm, ierr )
        if( ierr .ne. 0 ) then
          print*, 'Error: MPI_Bcast#25 !!!'
        endif
      endif
    endif
  end subroutine   !!!! subroutine Read_model_data



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!    Make_equation_data    !!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Make_equation_data( number_element_dof, &
                                 nond, nofc, nond_on_face, nint_para_fc, ndble_para_fc, &
                                 np, face2node, int_para_fc, dble_para_fc, &
                                 ndim, a, rhs, sol, &
                                 ext_ndim, lhp, ltp, proc_dim, irank, nrank, act_nrank, ierr )

    integer, intent(in)  :: number_element_dof

    integer, intent(in)  :: nond
    integer, intent(in)  :: nofc
    integer, intent(in)  :: nond_on_face
    integer, intent(in)  :: nint_para_fc
    integer, intent(in)  :: ndble_para_fc

    type (coordinate), dimension(:),  allocatable, intent(in) :: np
    integer, dimension(:,:), allocatable, intent(in) :: face2node
    integer, dimension(:,:), allocatable, intent(in) :: int_para_fc
    real*8, dimension(:,:), allocatable, intent(in) :: dble_para_fc

    integer, intent(out) :: ndim
    real*8, dimension(:,:), allocatable, intent(out) :: a
    real*8, dimension(:),   allocatable, intent(out) :: rhs
    real*8, dimension(:),   allocatable, intent(out) :: sol

    integer, intent(out) :: ext_ndim
    integer, intent(out) :: lhp, ltp
    integer, intent(out) :: proc_dim
    integer, intent(in)  :: irank
    integer, intent(in)  :: nrank
    integer, intent(out) :: act_nrank
    integer, intent(out) :: ierr

    integer  i, j

    integer  thr_dim, thr_dim1
    integer  ith, nth, mth
    integer  j_st, j_en

    integer  omp_get_thread_num, omp_get_num_threads

    real*8   element_ij
    real*8   right_hand_side_vector_element_i



    ndim = nofc * number_element_dof

    proc_dim = ndim / nrank

    if( ndim .eq. proc_dim * nrank ) then
      lhp = irank * proc_dim
      act_nrank = nrank
    else
      proc_dim = proc_dim + 1
      lhp = irank * proc_dim
      act_nrank = (ndim / proc_dim) + 1
    endif

    ext_ndim = act_nrank * proc_dim

    ltp = lhp + proc_dim
    lhp = lhp + 1

    allocate( a(ext_ndim,lhp:ltp), stat = ierr )
    allocate( rhs(ext_ndim), stat = ierr )
    allocate( sol(ext_ndim), stat = ierr )


!!!!!!!!!!!! Thread parallelization starts !!!!!!!!!!!!
!$omp parallel private(thr_dim, thr_dim1, ith, nth, mth, j_st, j_en, i, j)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ith = omp_get_thread_num()
    nth = omp_get_num_threads()

    if ( ith.eq.0 .and. irank.eq.0 ) then
      print*, 'Number of total threads, Thread number ', nth, ith
      print*, 'ext_ndim = ', ext_ndim, ', ndim = ', ndim
    endif

    thr_dim = ndim / nth
    mth  = ndim - thr_dim * nth
    thr_dim1 = thr_dim + 1
    j_st = 0
    if( ith .lt. mth ) then
      do i = 1, ith
        j_st = j_st + thr_dim1
      enddo
      j_en = j_st + thr_dim1
    else
      do i = 1, mth
        j_st = j_st + thr_dim1
      enddo
      do i = mth+1, ith
        j_st = j_st + thr_dim
      enddo
      j_en = j_st + thr_dim
    endif

  !$omp do private(j) schedule(static)
    do i = lhp, ltp
      do j = 1, ext_ndim
        a(j,i) = 0.0d0
      enddo
    enddo
  !$omp end do
    if( ltp .gt. ndim ) then
      ltp = ndim
    endif

    j_st = j_st + 1


!!!!!!!!!!!!!!! Caution: the function element_ij should be given by users. !!!!!!!!!!!!!!!
    do i=lhp, ltp
      do j=j_st, j_en
        a(j,i) = element_ij( i, j, nond, nofc, nond_on_face, np, int_para_fc, nint_para_fc, &
                             dble_para_fc, ndble_para_fc, face2node )
      enddo
    enddo

    do j=j_st, j_en
      rhs(j) = right_hand_side_vector_element_i( j, nond, nofc, nond_on_face, np, int_para_fc, nint_para_fc, &
                                                 dble_para_fc, ndble_para_fc, face2node )
      sol(j) = 0.0d0
    enddo

!!!!!!!!!!!!!!!!!!
!$omp end parallel
!!!!!!!!!!!!!!!!!!

  end subroutine   !!!! subroutine Make_equation_data


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!    Print_result    !!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Print_result( filename, nond, nofc, nond_on_face, np, face2node, &
                           ndim, sol, irank, iunit, ierr )

    character, intent(in) :: filename*256
    integer,   intent(in)  :: nond
    integer,   intent(in)  :: nofc
    integer,   intent(in)  :: nond_on_face

    type (coordinate), dimension(:),   allocatable, intent(in) :: np
    integer,           dimension(:,:), allocatable, intent(in) :: face2node

    integer,   intent(in) :: ndim
    real*8, dimension(:),   allocatable, intent(in) :: sol

    integer,  intent(in)  :: irank
    integer,  intent(in)  :: iunit
    integer,  intent(out) :: ierr

    integer  i, j

    ierr=0

    if( irank .ne. 0 ) then
      goto 5000
    endif

    open( iunit, file=filename, status='replace')
    write( iunit, '(a)' ) "# vtk DataFile Version 2.0"
    write( iunit, '(a)' ) "header : this data is results of BEM-BB"
    write( iunit, '(a)' ) "ASCII"
    write( iunit, '(a)' ) "DATASET UNSTRUCTURED_GRID"
    write( iunit, '(a,i0,a)' ) "POINTS ", nond, " float"
    do i = 1, nond
      write( iunit, '(f16.13, 1x, f16.13, 1x, f16.13)' ) np(i)%x, np(i)%y, np(i)%z
    enddo
    write( iunit, '(a,i0,1x,i0)' ) "CELLS ", nofc, (nond_on_face+1)*nofc
    do i = 1, nofc
      write( iunit, '(i0,1x)', advance='no' ) nond_on_face
      do j = 1, nond_on_face
        write( iunit, '(i0,1x)', advance='no' ) face2node(j,i)
      enddo
      write( iunit, * ) " "
    enddo
    write( iunit, '(a,i0)' ) "CELL_TYPES ", nofc
    do i = 1, nofc
      write( iunit, * ) "5"
    enddo
    write( iunit, * ) "CELL_DATA ", nofc
    write( iunit, * ) "SCALARS solve float 1"
    write( iunit, * ) "LOOKUP_TABLE default"
    do i = 1, nofc
      write( iunit, '(f32.28)' ) sol(i)
    enddo

    close( iunit )

5000 continue
    return
  end subroutine   !!!! subroutine Print_result

end program !!!!!!!!!!!!! program bem_bb_dense_mpi !!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!    pbicgstab    !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function ppohBEM_pbicgstab_dense( irank, nrank, lhp, ltp, &
  & proc_dim, ndim, ext_ndim, a, rhs, sol, tor, max_steps, act_comm, ierr )
      use ppohAT_ControlRoutines
      use ppohAT_InstallRoutines
      use ppohAT_StaticRoutines
      use ppohAT_DynamicRoutines

  implicit none
  integer omp_get_thread_num, omp_get_num_threads

  include 'mpif.h'

  integer, intent(in) :: irank, nrank
  integer, intent(in) :: lhp, ltp
  integer, intent(in) :: proc_dim, ndim, ext_ndim

  real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
  real*8, dimension( ext_ndim ),          intent(in) :: rhs
  real*8, dimension( ext_ndim ),          intent(inout) :: sol

  real*8,  intent(in) :: tor
  integer, intent(in) :: max_steps

  integer, intent(in) :: act_comm
  integer, intent(out) :: ierr

  real*8, dimension(:), allocatable :: r, shdw, p, t, kp, akp, kt, akt
  real*8, dimension(:), allocatable :: asum
  real*8   asum1, asum2

  integer  step, nsteps, i, j
  real*8   alpha, beta, zeta, nom, den, nomold, rnorm, bnorm

  integer  thr_dim, thr_dim1
  integer  ith, nth, inth, mth
  integer  j_st, j_en, i_st, i_en

  integer  iunit
  include 'OAT.h'
!      integer iusw1_ppohBEMresidual_direct
!      integer iusw1_ppohBEMmatvec_direct



! ==== for ATExec
  real*8, dimension( ext_ndim ) :: q
  real*8, dimension( ext_ndim ) :: x


!!!! Allocation of arrays !!!!
  allocate( r(ext_ndim), shdw(ext_ndim), p(ext_ndim), t(ext_ndim), kp(ext_ndim), stat=ierr )
  if( ierr .ne. 0 ) then
    print*, 'Memory allocation #101 failed !!!'
    goto 1000
  endif
  allocate( akp(ext_ndim), kt(ext_ndim), akt(ext_ndim), stat=ierr)
  if( ierr .ne. 0 ) then
    print*, 'Memory allocation #102 failed !!!'
    goto 1000
  endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$omp parallel &
!$omp private(step,i,j,alpha,beta,zeta,nom,den,nomold,rnorm,bnorm,thr_dim,thr_dim1,ith,nth,inth,mth,j_st,j_en,i_st,i_en)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ith = omp_get_thread_num()
  nth = omp_get_num_threads()

  thr_dim  = ndim / nth
  mth = ndim - thr_dim * nth
  thr_dim1 = thr_dim + 1
  j_st = 0
  if( ith .lt. mth ) then
    do i = 1, ith
      j_st = j_st + thr_dim1
    enddo
    j_en = j_st + thr_dim1
  else
    do i = 1, mth
      j_st = j_st + thr_dim1
    enddo
    do i = mth+1, ith
      j_st = j_st + thr_dim
    enddo
    j_en = j_st + thr_dim
  endif

  thr_dim = proc_dim / nth
  mth  = proc_dim - thr_dim * nth
  thr_dim1 = thr_dim + 1
  i_st = lhp - 1
  if( ith .lt. mth ) then
    do i = 1, ith
      i_st = i_st + thr_dim1
    enddo
    i_en = i_st + thr_dim1
  else
    do i = 1, mth
      i_st = i_st + thr_dim1
    enddo
    do i = mth+1, ith
      i_st = i_st + thr_dim
    enddo
    i_en = i_st + thr_dim
  endif

  j_st = j_st + 1
  i_st = i_st + 1

  ith  = ith + 1
  inth = ith + nth


!!!! Initialization !!!!
  do i = i_st, i_en
    r(i) = 0.0d0;   shdw(i) = 0.0d0
    t(i) = 0.0d0;   akp(i) = 0.0d0
    kt(i) = 0.0d0;  akt(i) = 0.0d0
  enddo
  do j = j_st, j_en
    r(j) = 0.0d0;   shdw(j) = 0.0d0
    p(j) = 0.0d0;   t(j) = 0.0d0
    kp(j) = 0.0d0;  akp(j) = 0.0d0
    kt(j) = 0.0d0;  akt(j) = 0.0d0
  enddo
  if( j_en==ndim ) then
    do j = ndim, ext_ndim
      r(j) = 0.0d0;   shdw(j) = 0.0d0
      p(j) = 0.0d0;   t(j) = 0.0d0
      kp(j) = 0.0d0;  akp(j) = 0.0d0
      kt(j) = 0.0d0;  akt(j) = 0.0d0
    enddo
  endif
  alpha = 0.0d0;  beta = 0.0d0;  zeta = 0.0d0



!$omp master
  allocate( asum(nth+nth), stat=ierr )
  asum(1) = 0.0d0
!$omp end master
  if( ierr .ne. 0 ) then
    print*, 'Memory allocation #103 failed !!!'
    goto 990
  endif
!$omp barrier


      oat_myid = irank
      oat_mythread_num = ith - 1

      call OAT_ATset(OAT_ALL, OAT_AllRoutines)
      call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
      call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
      call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)

!!OAT$ call OAT_BPset("ndim")
      OAT_NUMPROCS = 1
      OAT_STARTTUNESIZE = ndim
      OAT_ENDTUNESIZE = ndim
      OAT_SAMPDIST = 100
      OAT_DEBUG = 1
      OAT_MAXSAMPITER = 100
      call OAT_ATexec(OAT_INSTALL,OAT_InstallRoutines,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
!      print *, oat_myid, oat_mythread_num, "OAT_ATEXEC_FLAG=",OAT_ATEXEC_FLAG
      
      if (OAT_ATEXEC_FLAG .eq. 1) then 
        if (oat_mythread_num .eq. 0) then            
          call MPI_Finalize (ierr)       
        endif   
        stop
      endif 


  call residual_direct( i_st, i_en, ndim, ext_ndim, lhp, ltp, a, sol, rhs, r )


!$omp master
  call MPI_Allgather ( MPI_IN_PLACE, proc_dim, MPI_DOUBLE_PRECISION, &
                       r, proc_dim, MPI_DOUBLE_PRECISION, act_comm, ierr )
!$omp end master
!$omp barrier


!!!! Set shadow vector !!!!
  do j = j_st, j_en
    shdw(j) = r(j)
  enddo


!$omp barrier
  call dot_product( i_st, i_en, ext_ndim, rhs, rhs, asum(ith) )
  call dot_product( i_st, i_en, ext_ndim, r, r, asum(inth) )
  asum1 = 0.0
  asum2 = 0.0
!$omp barrier
!$omp do reduction(+:asum1, asum2) schedule(static,1)
  do i = 1, nth
    asum1 = asum1 + asum(i)
    asum2 = asum2 + asum(i+nth)
  enddo
!$omp barrier
!$omp master
  asum(1) = asum1
  asum(2) = asum2
  call MPI_Allreduce( MPI_IN_PLACE, asum, 2, MPI_DOUBLE_PRECISION, MPI_SUM, act_comm, ierr )
!$omp end master
!$omp barrier
  bnorm = sqrt( asum(1) )
  rnorm = sqrt( asum(2) )
  asum1 = 0.0
  asum2 = 0.0
!$omp barrier


  if( irank .eq. 0 ) then
!$omp master
    write(*,1010) 'Original relative residual norm =', rnorm/bnorm
!$omp end master
  endif

  if ( rnorm .lt. tor*bnorm ) then
    nsteps = 0; step = 0;  goto 900
  endif

!!!! BiCGSTAB iteration !!!!
  do step=1, max_steps

    do j = j_st, j_en
      p(j) = r(j) + beta * ( p(j) - zeta * akp(j) )
    enddo
  !!!! No preconditioning !!!!
    do j = j_st, j_en
      kp(j) = p(j)
    enddo
  !$omp barrier
    call matvec_direct( i_st, i_en, ndim, ext_ndim, lhp, ltp, a, kp, akp )


  !$omp barrier
  !$omp master
    call MPI_Allgather( MPI_IN_PLACE, proc_dim, MPI_DOUBLE_PRECISION, &
                        akp, proc_dim, MPI_DOUBLE_PRECISION, act_comm, ierr )
  !$omp end master
  !$omp barrier
    call dot_product( i_st, i_en, ext_ndim, shdw, r, asum(ith) )
    call dot_product( i_st, i_en, ext_ndim, shdw, akp, asum(inth) )
  !$omp barrier
  !$omp do reduction(+:asum1, asum2) schedule(static,1)
    do i = 1, nth
      asum1 = asum1 + asum(i)
      asum2 = asum2 + asum(i+nth)
    enddo
  !$omp barrier
  !$omp master
    asum(1) = asum1
    asum(2) = asum2
    call MPI_Allreduce( MPI_IN_PLACE, asum, 2, MPI_DOUBLE_PRECISION, MPI_SUM, act_comm, ierr )
  !$omp end master
  !$omp barrier
    nom = asum(1)
    den = asum(2)
    asum1 = 0.0
    asum2 = 0.0
  !$omp barrier
    alpha  = nom / den
    nomold = nom

    do j = j_st, j_en
      t(j) = r(j) - alpha * akp(j)
    enddo

    !!!! No preconditioning !!!!
    do j = j_st, j_en
      kt(j) = t(j)
    enddo

  !$omp barrier
    call matvec_direct( i_st, i_en, ndim, ext_ndim, lhp, ltp, a, kt, akt )
  !$omp barrier
  !$omp master
    call MPI_Allgather( MPI_IN_PLACE, proc_dim, MPI_DOUBLE_PRECISION, &
                        akt, proc_dim, MPI_DOUBLE_PRECISION, act_comm, ierr )
  !$omp end master
  !$omp barrier
    call dot_product( i_st, i_en, ext_ndim, akt, t, asum(ith) )
    call dot_product( i_st, i_en, ext_ndim, akt, akt, asum(inth) )
  !$omp barrier
  !$omp do reduction(+:asum1, asum2) schedule(static,1)
    do i = 1, nth
      asum1 = asum1 + asum(i)
      asum2 = asum2 + asum(i+nth)
    enddo
  !$omp barrier
  !$omp master
    asum(1) = asum1
    asum(2) = asum2
    call MPI_Allreduce( MPI_IN_PLACE, asum, 2, MPI_DOUBLE_PRECISION, MPI_SUM, act_comm, ierr )
  !$omp end master
  !$omp barrier
    nom = asum(1)
    den = asum(2)
    asum1 = 0.0
    asum2 = 0.0
  !$omp barrier
    zeta  = nom / den

    do j = j_st, j_en
      sol(j) = sol(j) + alpha * kp(j) + zeta * kt(j)
    enddo
    do j = j_st, j_en
      r(j) = t(j) - zeta * akt(j)
    enddo

  !$omp barrier
    call dot_product( i_st, i_en, ext_ndim, shdw, r, asum(ith) )
    call dot_product( i_st, i_en, ext_ndim, r, r, asum(inth) )
  !$omp barrier
  !$omp do reduction(+:asum1, asum2) schedule(static,1)
    do i = 1, nth
      asum1 = asum1 + asum(i)
      asum2 = asum2 + asum(i+nth)
    enddo
  !$omp barrier
  !$omp master
    asum(1) = asum1
    asum(2) = asum2
    call MPI_Allreduce( MPI_IN_PLACE, asum, 2, MPI_DOUBLE_PRECISION, MPI_SUM, act_comm, ierr )
  !$omp end master
  !$omp barrier
    beta = alpha * asum(1) / ( zeta * nomold )
    rnorm = sqrt( asum(2) )
    asum1 = 0.0
    asum2 = 0.0
  !$omp barrier
    if( irank .eq. 0 ) then
  !$omp master
      write(*,1011) 'Step', step, ' relative residual =', rnorm/bnorm
  !$omp end master
    endif
    if( rnorm .lt. tor*bnorm ) then
      exit   !!! end iteration
    endif
  enddo  !!!! do step=1, max_steps !!!!

900  continue

  call residual_direct( i_st, i_en, ndim, ext_ndim, lhp, ltp, a, sol, rhs, r )

!$omp master
  call MPI_Allgather ( MPI_IN_PLACE, proc_dim, MPI_DOUBLE_PRECISION, &
                       r, proc_dim, MPI_DOUBLE_PRECISION, act_comm, ierr )
!$omp end master
!$omp barrier

  call dot_product( i_st, i_en, ext_ndim, r, r, asum(ith) )

!$omp barrier
!$omp do reduction(+:asum1) schedule(static,1)
  do i = 1, nth
    asum1 = asum1 + asum(i)
  enddo
!$omp barrier
!$omp master
  asum(1) = asum1
  call MPI_Allreduce( MPI_IN_PLACE, asum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, act_comm, ierr )
!$omp end master
!$omp barrier
  rnorm = sqrt( asum(1) )
!$omp barrier

    if( irank .eq. 0 ) then
!$omp master
      write(*,*) ' '
      write(*,1010) 'Relative residual norm = ', rnorm/bnorm
      write(*,*) ' '
      write(*,*) ' '
!$omp end master
    endif
    nsteps = step

990  continue

!$omp end parallel

1000 continue
  if(allocated(r))    deallocate(r)
  if(allocated(shdw)) deallocate(shdw)
  if(allocated(p))    deallocate(p)
  if(allocated(t))    deallocate(t)
  if(allocated(kp))   deallocate(kp)
  if(allocated(akp))  deallocate(akp)
  if(allocated(kt))   deallocate(kt)
  if(allocated(akt))  deallocate(akt)
  if(allocated(asum)) deallocate(asum)

  ppohBEM_pbicgstab_dense = nsteps
  return

!1010 FORMAT(a, 1ES)
!1011 FORMAT(a, 1I5, a, 1ES)

1010 FORMAT(a, 1pe15.7)
1011 FORMAT(a, 1I5, a, 1pe15.7)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    subroutines    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!    residual_direct    !!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine residual_direct( i_st, i_en, ndim, ext_ndim, lhp, ltp, a, x, rhs, r )
      use ppohAT_ControlRoutines
      use ppohAT_InstallRoutines
      use ppohAT_StaticRoutines
      use ppohAT_DynamicRoutines
    integer, intent(in) :: i_st, i_en, ndim
    integer, intent(in) :: ext_ndim, lhp, ltp

    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: x, rhs
    real*8, dimension( ext_ndim ),          intent(out) :: r

    integer  i, j

    include "OAT.h"
!      integer iusw1_ppohBEMresidual_direct
!      integer iusw1_ppohBEMmatvec_direct
   
    character*100 ctmp

    do i = i_st, i_en
      r(i) = rhs(i)
    enddo

!      print *, "iusw1_ppohBEMresidual_direct_flag=",iusw1_ppohBEMresidual_direct_flag 

      ctmp = "ppohBEMresidual_direct" 
      call OAT_SetParm(1,ctmp,ndim,iusw1_ppohBEMresidual_direct,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
      call OAT_InstallppohBEMresidual_direct(ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,iusw1_ppohBEMresidual_direct)
!!OAT$ install unroll(i,j) region start
!!OAT$ name ppohBEMresidual_direct
!!OAT$ varied (i,j) from 1 to 2
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine: ppohBEMresidual_direct=',iusw1_ppohBEMresidual_direct
        endif
!    do i = i_st, i_en
!      do j = 1, ndim
!        r(i) = r(i) - a(j,i) * x(j)
!      enddo
!    enddo
!!OAT$ install unroll(i,j) region end


  end subroutine   !!!! subroutine residual_direct

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!    matvec_direct    !!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine matvec_direct( i_st, i_en, ndim, ext_ndim, lhp, ltp, a, p, q )
      use ppohAT_ControlRoutines
      use ppohAT_InstallRoutines
      use ppohAT_StaticRoutines
      use ppohAT_DynamicRoutines
    integer, intent(in) :: i_st, i_en, ndim
    integer, intent(in) :: ext_ndim, lhp, ltp

    real*8, dimension( ext_ndim, lhp:ltp ), intent(in) :: a
    real*8, dimension( ext_ndim ),          intent(in) :: p
    real*8, dimension( ext_ndim ),          intent(out) :: q

    integer  i, j

    include "OAT.h"
      integer iusw1_ppohBEMresidual_direct
      integer iusw1_ppohBEMmatvec_direct

    character*100 ctmp 

    do i = i_st, i_en
      q(i) = 0.0d0
    enddo

!      print *, "iusw1_ppohBEMmatvec_direct_flag=",iusw1_ppohBEMmatvec_direct_flag

      ctmp = "ppohBEMmatvec_direct"
      call OAT_SetParm(1,ctmp,ndim,iusw1_ppohBEMmatvec_direct,ext_ndim,lhp,ltp,i_st,i_en,ndim,r,a,x,q,p)
      call OAT_InstallppohBEMmatvec_direct(ext_ndim,lhp,ltp,i_st,i_en,ndim,a,q,p,iusw1_ppohBEMmatvec_direct)
!!OAT$ install unroll(i,j) region start
!!OAT$ name ppohBEMmatvec_direct
!!OAT$ varied (i,j) from 1 to 2
!!OAT$ debug (pp)
        if (OAT_DEBUG .ge. 2)then
          print *, 'oat_myid: ',oat_myid
          print *, 'Install Routine: ppohBEMmatvec_direct=',iusw1_ppohBEMmatvec_direct
        endif
!    do i = i_st, i_en
!      do j = 1, ndim
!        q(i) = q(i) + a(j,i) * p(j)
!      enddo
!    enddo
!!OAT$ install unroll(i,j) region end


  end subroutine   !!!! subroutine residual_direct

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!    dot_product    !!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine dot_product( i_st, i_en, ext_ndim, p, q, asum )

    integer, intent(in) :: i_st, i_en, ext_ndim

    real*8, dimension( ext_ndim ), intent(in) :: p, q
    real*8, intent(out) :: asum

    integer  i

    asum = 0.0d0
    do i = i_st, i_en
      asum = asum + p(i) * q(i)
    enddo

  end subroutine   !!!! subroutine dot_product

end function ppohBEM_pbicgstab_dense


