!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohDEM                                          !!
!!         Version : 0.2.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohDEM.                                  !!
!!     ppohDEM is a free software, you can use it under the terms     !!
!!     of The MIT License (MIT). See LICENSE file and User's guide    !!
!!     for more details.                                              !!
!!                                                                    !!
!!   ppOpen-HPC project:                                              !!
!!     Open Source Infrastructure for Development and Execution of    !!
!!     Large-Scale Scientific Applications on Post-Peta-Scale         !!
!!     Supercomputers with Automatic Tuning (AT).                     !!
!!                                                                    !!
!!   Organizations:                                                   !!
!!     The University of Tokyo                                        !!
!!       - Information Technology Center                              !!
!!       - Atmosphere and Ocean Research Institute (AORI)             !!
!!       - Interfaculty Initiative in Information Studies/            !!
!!         Earthquake Research Institute (ERI)                        !!
!!       - Graduate School of Frontier Science                        !!
!!     Kyoto University                                               !!
!!       - Academic Center for Computing and Media Studies            !!
!!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC) !!
!!                                                                    !!
!!   Sponsorship:                                                     !!
!!     Japan Science and Technology Agency (JST), Basic Research      !!
!!     Programs: CREST, Development of System Software Technologies   !!
!!     for post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Miki Yamamoto Matsuo, JAMSTEC                !!
!!                       mikiy(at)jamstec.go.jp                       !!
!!                                                                    !!
!!====================================================================!!

program sample_main01

!    include 'mpif.h'

    use  ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none
    



    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel


!----------------------------------------------------------------
!    Error code
    integer(kind=kint) :: err

    integer(kind=kint) :: i, j, k
!    integer(kind=kint) :: nx, ny, nz


    type(ppohDEM_gridco) :: gridco

!    real :: clock_start, clock_finish, timings_total
!    real, DIMENSION(:,:), ALLOCATABLE :: timings_real

    DOUBLE PRECISION :: clock_start, clock_finish, timings_total
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: timings_real

    character(len=ppohDEM_name_length) :: t_char, ex_char, filename

    integer(kind=kint) :: c_len







!------------------------------------------------------------------------------
!     start MPI 
!------------------------------------------------------------------------------
      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)


      print *, 'NPROCS = ', nprocs, 'myrank = ', myrank








    i = 0
    j = 0
    k = 0
    err = 0







    allocate(timings_real(0:9, 1:20), stat=err)




!----------------------------------------------------------------
!    call cpu_time(clock_start)
!    call cpu_time(timings_real(0,1)) 
    clock_start = MPI_Wtime()
    timings_real(0,1) = MPI_Wtime()

    write (*,*) '*** 1 load file ', myrank
    call ppohDEM_loadfileinfo(file_info,1)

!    call cpu_time(timings_real(1,1)) 
    timings_real(1,1) = MPI_Wtime()


    write (*,*) '*** 2 variables init. ', myrank
!    call ppohDEM_v_pre(file_info,meshXYZ,voxel)
    call ppohDEM_v_pre(file_info,meshXYZ,voxel,gridco)

!    call cpu_time(timings_real(2,1)) 
    timings_real(2,1) = MPI_Wtime()


    write (*,*) '*** 3 voxelizing ', myrank
!    call ppohDEM_v_makevoxel(file_info,meshXYZ,voxel)
    call ppohDEM_v_makevoxel(file_info,meshXYZ,voxel,gridco)

!    call cpu_time(timings_real(3,1)) 
    timings_real(3,1) = MPI_Wtime()


!----------------------------------------------------------------
!  distancecalculate() 
!----------------------------------------------------------------
    write (*,*) '*** 4 distancecalculate ', myrank
!    call ppohDEM_v_distancecalculate(voxel)
    call ppohDEM_v_distancecalculate_MPI1(voxel)

!    call cpu_time(timings_real(4,1)) 
    timings_real(4,1) = MPI_Wtime()


    write (*,*) '*** 5 distanceprecise', myrank
!    call ppohDEM_v_distancevoxel(file_info,voxel,meshXYZ,gridco)
    call ppohDEM_v_distanceprecise(voxel,meshXYZ,gridco)

!    call cpu_time(timings_real(5,1)) 
    timings_real(5,1) = MPI_Wtime()


    write (*,*) '*** 6 distancecalculate 2', myrank
!    call ppohDEM_v_distancecalculate(voxel)
    call ppohDEM_v_distancecalculate_MPI2(voxel)

!    call cpu_time(timings_real(6,1)) 
    timings_real(6,1) = MPI_Wtime()


!----------------------------------------------------------------
    write (*,*) '*** 7 output_data ', myrank  
    call output_data(file_info,voxel)

!    call cpu_time(timings_real(7,1)) 
    timings_real(7,1) = MPI_Wtime()


!----------------------------------------------------------------
    write (*,*) '*** 8 ppohDEM_v_exit ', myrank
!    call ppohDEM_v_exit(file_info,meshXYZ,voxel)
    call ppohDEM_v_exit(meshXYZ,voxel,gridco)

!    call cpu_time(timings_real(8,1)) 
    timings_real(8,1) = MPI_Wtime()


    write (*,*) '*** 9 completed ', myrank

!    call cpu_time(timings_real(9,1)) 
    timings_real(9,1) = MPI_Wtime()


!    call cpu_time(clock_finish)
    clock_finish = MPI_Wtime()

    timings_total = clock_finish - clock_start
    write (*,"(1A,F10.6,1A)") 'Total ', timings_total, ' s '








   if(myrank .eq. 0) then 

!----------------------------------------------------------------
!      write log-timings
!----------------------------------------------------------------

    WRITE(t_char, *) myrank  
    ex_char = "000" // trim(ADJUSTL(t_char))
    c_len = LEN_TRIM(ex_char)
    t_char = ex_char(c_len - 3:c_len)

    filename = trim("log-timings_" // trim(t_char) // ".log" )

    open (1001, file = trim(filename), status='replace')

    do i=1, 9
       write(1001,"(1I3,F16.6)") i, timings_real(i,1) - timings_real(i-1,1)
    end do

!    write (1001,"(1A)") ' '
    write (1001,"(1A,F24.6)") 'Total ', timings_total

    close(1001)

    endif !(myrank .eq. 0) then 








    deallocate(timings_real, stat=err)








    call MPI_FINALIZE(ierr)
!------------------------------------------------------------------------------
!    end MPI 
!------------------------------------------------------------------------------
    stop
    
end program sample_main01



