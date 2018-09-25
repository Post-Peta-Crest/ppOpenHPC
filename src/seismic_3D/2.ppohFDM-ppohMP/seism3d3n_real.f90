!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                               !
!                                                                     !
!   License                                                           !
!     This file is part of ppOpen-APPL/FDM.                           !
!     ppOpen-APPL/FDM is a free software, you can use it under the    !
!     terms of The MIT License (MIT). See LICENSE file and User's     !
!     guide for more details.                                         !
!                                                                     !
!   ppOpen-HPC project:                                               !
!     Open Source Infrastructure for Development and Execution of     !
!     Large-Scale Scientific Applications on Post-Peta-Scale          !
!     Supercomputers with Automatic Tuning (AT).                      !
!                                                                     !
!   Organizations:                                                    !
!     The University of Tokyo                                         !
!       - Information Technology Center                               !
!       - Atmosphere and Ocean Research Institute (AORI)              !
!       - Interfaculty Initiative in Information Studies              !
!         /Earthquake Research Institute (ERI)                        !
!       - Graduate School of Frontier Science                         !
!     Kyoto University                                                !
!       - Academic Center for Computing and Media Studies             !
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  !
!                                                                     !
!   Sponsorship:                                                      !
!     Japan Science and Technology Agency (JST), Basic Research       !
!     Programs: CREST, Development of System Software Technologies    !
!     for post-Peta Scale High Performance Computing.                 !
!                                                                     !
!                 Copyright (c) 2015 T.Furumura                       !
!                                                                     !
!=====================================================================!
program seism3d3n
!
! 3D FDM Code for MPI
!
  use ppohFDM_stdio
  use ppohFDM_param
  use ppohFDM_io
  use ppohFDM_pssub
  use ppohFDM_pfd3d
  use ppohFDM_boundary
  use ppohFDM_stress
  use ppohFDM_velocity
  use ppohFDM_source
  use ppohFDM_sponge_absorber
  use mpi
  use ppohFDM_set_condition
  use ppohFDM_coupling_prm
  
  !math/mp-s
  use m_ppohMATHMP_FDM
  !math/mp-e

  implicit none


  !!--------------------------------------------------------------------------!!
  !!                             MPI ENVIRONMENT                              !!
  !! Sets up MPI environment and an index array to examine the absolute x,y,z !!
  !! position (IA, JA, KA) in the full 3D simulation model from the rank      !!
  !!number (myid) of each of the subregions                                   !!
  !!--------------------------------------------------------------------------!!
  call ppohMATHMP_FDM_init("coupling.nmlst")
  call set_mpi_environment( myid, itbl, idx, idy, idz )
  
  ! Absolute Coordinate
  do I=NXP0, NXP1
     IA( I ) = NXP*idx + I
  end do

  do J=NYP0, NYP1
     JA( J ) = NYP*idy + J
  end do

  do K=NZP0, NZP1
     KA( K ) = NZP*idz + K
  end do
   
  if( myid == 0 ) then
     write(STDERR,'(A)') "PROGRAM SEISM3DZ"
     write(STDERR,*)
     write(STDERR,'(A,I5,A,I5,A,I5)') "MODEL  SIZE: ", NXP, "x", NYP, "x", NZP
  end if

  !! Initialize elapsed time counter
  if( myid == NP/2 ) then
     write(STDERR,*)
     call system_clock( timcount, crate )
     timcount0 = timcount
     timprev = timcount
  end if

  !!--------------------------------------------------------------------------!!
  !!                               ABOSORBER                                  !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_set_sponge_absorber()

  !!--------------------------------------------------------------------------!!
  !!                              SET SOURCE POINT                            !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_set_source()

  !!--------------------------------------------------------------------------!!
  !!                              Moment Tensor                               !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_sld2moment( STRIKE, DIP, RAKE, 1.0, RMXX, RMYY, RMZZ, RMXY, RMYZ, RMXZ )

  !!--------------------------------------------------------------------------!!
  !!                               FREE SURFACE                               !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_read_geom_data()

  !! 1. Free surface boundary on the absolute grid 
  do J=0, NY+1
     do I=0, NX+1
        KFSZA(I,J) = KFS
     end do
  end do

  print *,'ppohFDM_read_geom_data is completed'
  
  !! 2. Trimming kfsza, detection of horizontal boundary
  call ppohFDM_set_free_surface( KFSZA, KFSZ, NIFS, NJFS, &
                                IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ )
  print *,'ppohFDM_set_free_surface is completed'

  !!--------------------------------------------------------------------------!!
  !!                         SET MEDIUM PARAMETERS                            !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_set_medium( DEN, RIG, LAM )
  print *,'ppohFDM_set_medium is completed'
  !!--------------------------------------------------------------------------!!
  !!                            STABLE CONDITION                              !!
  !!--------------------------------------------------------------------------!!
  if( myid == nproc-1 ) then
     write(STDERR,'(A,F10.5)') "STABLE CONDITION (SHOULD BE SMALLER THAN 1)", &
          DT/( 0.45*min(DX,DY,DZ)/maxval(sqrt((LAM+2*RIG)/DEN)))
     if( DT > 0.45*min(DX,DY,DZ)/maxval(sqrt((LAM+2*RIG)/DEN)) ) then
        write(STDERR,*) "Enlarge Spatial Grid and/or Shorten Time Grid!"
     end if
     write(STDERR,*) 
  end if

  !!--------------------------------------------------------------------------!!
  !!                           PARAMETER FILE OUTPUT                          !!
  !!--------------------------------------------------------------------------!!
  if( myid == 0 )  call ppohFDM_output_prm
   
  !!--------------------------------------------------------------------------!!
  !!                          Rank FILE OUTPUT                                !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_output_rank

  !!--------------------------------------------------------------------------!!
  !!                            MOMENT FUNCTION                               !!
  !!--------------------------------------------------------------------------!!
  !! Use ikupper for Body Force Source, kupper for Stress Drop Source
  do IT = 1, NTMAX
     T = (IT-1)*DT
     STIME (IT) =  kupper (AT, T, T0)    ! For Stress Drop
  end do

  !!--------------------------------------------------------------------------!!
  !!                             ZERO-FILL ARRAYS                             !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_initialize_arrays()

  !!--------------------------------------------------------------------------!!
  !!                                 STATION                                  !!
  !!--------------------------------------------------------------------------!!
  call ppohFDM_set_station()
  call ppohFDM_station_func()

  !!--------------------------------------------------------------------------!!
  !!                            ppOpen- MATH/MP                               !!
  !!--------------------------------------------------------------------------!!
  !!math/mp-s
  ! to coupler
  call ppohMATHMP_FDM_def_grid(coord_x,coord_xx,coord_y,coord_yy,coord_z,coord_zz)
  ! set to changing value
  call ppohMATHMP_FDM_def_varp()
  ! set to mapping table
  call ppohMATHMP_FDM_set_mapping_tbl()
  ! set to initial time
  call ppohMATHMP_FDM_init_time()
  ! call integral for FEM model
  call ppohMATHMP_FDM_start_integration()
  !!math/mp-e

  !!--------------------------------------------------------------------------!!
  !!                               OUTPUT FILES                               !!
  !!--------------------------------------------------------------------------!!
  !!Please see io.f90
  call ppohFDM_io_open()

  !!--------------------------------------------------------------------------!!
  !!         set to calculation data for other method via coupler             !!
  !!--------------------------------------------------------------------------!!
  !! math/mp-s
  do k=1,NZP
    do j=1,NYP
       do i=1,NXP
           UVX(i,j,k)=VX(i,j,k)
           UVY(i,j,k)=VY(i,j,k)
           UVZ(i,j,k)=VZ(i,j,k)
        end do
     end do
  end do

  call ppohMATHMP_FDM_put_data("U",UVX)
  call ppohMATHMP_FDM_put_data("V",UVY)
  call ppohMATHMP_FDM_put_data("W",UVZ)
  !! math/mp-e
  !!--------------------------------------------------------------------------!!
  !!                           TIME STEP START                                !!
  !!--------------------------------------------------------------------------!!
  if( myid == NP/2 ) then
     write(STDERR,*)
     call system_clock( timcount, crate )
     timprev = timcount
  end if
  ttotal = 0.0_PN

  DXI = 1.0_PN / DX
  DYI = 1.0_PN / DY
  DZI = 1.0_PN / DZ

  xmax  = 0.0; ymax  = 0.0; zmax  = 0.0
  timestep: do IT=1, NTMAX
     T  =   DT * (IT-1)                                             
     if (myid==0) print *, 'IT', IT
     !!--- Time Measurement
     if( mod(IT, NWRITE) == 0 ) then
    write(*,*) 'FDM:seism3d3n.f90::258 before get_max' !DEBUG

        ! max value for debug output 
        call get_max( xmax, ymax, zmax )
    write(*,*) 'FDM:seism3d3n.f90::262 after get_max' !DEBUG
        
        if( myid == NP/2 ) then

           call system_clock( timcount, crate )
           tstep = real( timcount - timprev ) / real( crate )
           ttotal = ttotal + tstep
           etas   = (ntmax -it)/ real(it) * (timcount-timcount0)/real( crate )
           etah = int( etas/(   60*60) ); etas = etas - etah   *60*60
           etam = int( etas/(      60) ); etas = etas - etam      *60
           etasi = int(etas)
           timprev = timcount
           
           write(STDERR,'(A,I6,A,I6,A,I2.2,A,I2.2,A,I2.2,A,F9.4,A,3ES9.2,A)') &
                "IT=(", IT, "/",NTMAX,"), ETA=", &
                etah,":",etam,":",etasi, &
                ", Time/Step=", ttotal / IT, "[s], MAX = ( ", &
                xmax, ymax, zmax, " )"
        end if
     end if
     
     !!-----------------------------------------------------------------------!!
     !!                        Velocity  t=(n+1/2)*dt                         !!
     !!-----------------------------------------------------------------------!!
     if( is_fs .or. is_nearfs ) then
        call ppohFDM_bc_zero_stress( KFSZ,NIFS,NJFS,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ )
     end if
     
     call ppohFDM_pdiffx3_p4( SXX,DXSXX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_p4( SYY,DYSYY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffx3_m4( SXY,DXSXY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffx3_m4( SXZ,DXSXZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_m4( SXY,DYSXY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffy3_m4( SYZ,DYSYZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffz3_p4( SZZ,DZSZZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     call ppohFDM_pdiffz3_m4( SXZ,DZSXZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     call ppohFDM_pdiffz3_m4( SYZ,DZSYZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     
     !! Substitute to Reduced-order derivertives at around model boundary
     call ppohFDM_truncate_diff_stress(idx,idy,idz)

     if( is_fs .or. is_nearfs ) then
        call ppohFDM_bc_stress_deriv( KFSZ,NIFS,NJFS,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ )
     end if
         
     !! Velocity Update
     call ppohFDM_update_vel       ( 1, NXP, 1, NYP, 1, NZP )
     call ppohFDM_update_vel_sponge( 1, NXP, 1, NYP, 1, NZP )
  
     !!-----------------------------------------------------------------------!!
     !!                             BODY FORCE                                !!
     !!-----------------------------------------------------------------------!!
     !call ppohFDM_source_term_bodyforce     ! comment out if stress drop source
     
     !!-----------------------------------------------------------------------!!
     !!                            Message Passing                            !!
     !!-----------------------------------------------------------------------!!
     call ppohFDM_passing_velocity()

     !!-----------------------------------------------------------------------!!
     !!                send calculation data via coupler                      !!
     !!-----------------------------------------------------------------------!!
     !math/mp-s
     if (IT>=ppohMATHMP_FDM_get_start_step().and.IT<= ppohMATHMP_FDM_get_end_step())then

        call ppohMATHMP_FDM_set_time()

        do k=1,NZP
           do j=1,NYP
              do i=1,NXP
                 UVX(i,j,k)=VX(i,j,k)
                 UVY(i,j,k)=VY(i,j,k)
                 UVZ(i,j,k)=VZ(i,j,k)
              end do
           end do
        end do

        call ppohMATHMP_FDM_put_data("U",UVX)
        call ppohMATHMP_FDM_put_data("V",UVY)
        call ppohMATHMP_FDM_put_data("W",UVZ)
     end if
     !math/mp-e
     !!-----------------------------------------------------------------------!!
     !!                           Stress   t=(n+1)*dt                         !!
     !!-----------------------------------------------------------------------!!
     call ppohFDM_pdiffx3_m4( VX,DXVX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_m4( VY,DYVY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffx3_p4( VY,DXVY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffx3_p4( VZ,DXVZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DX )
     call ppohFDM_pdiffy3_p4( VX,DYVX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffy3_p4( VZ,DYVZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DY )
     call ppohFDM_pdiffz3_m4( VZ,DZVZ, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     call ppohFDM_pdiffz3_p4( VX,DZVX, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     call ppohFDM_pdiffz3_p4( VY,DZVY, NXP,NYP,NZP,NXP0,NXP1,NYP0,NYP1,NZP0,NZP1, DZ )
     
     !! Substitute to reduced order derivertives at around model boundary
     call ppohFDM_truncate_diff_vel(idx,idy,idz)

     
     if( is_fs .or. is_nearfs ) then
        call ppohFDM_bc_vel_deriv( KFSZ,NIFS,NJFS,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ )
     end if

     !!-- Update Stress Components
     call ppohFDM_update_stress        ( 1, NXP, 1, NYP, 1, NZP )
     call ppohFDM_update_stress_sponge ( 1, NXP, 1, NYP, 1, NZP )

     !!-----------------------------------------------------------------------!!
     !!                           STRESS DROP SOURCE                          !!
     !!-----------------------------------------------------------------------!!
     call ppohFDM_source_term_stressdrop() 

     !!-----------------------------------------------------------------------!!
     !!                            Message Passing                            !!
     !!-----------------------------------------------------------------------!!
     call ppohFDM_passing_stress()
    
     !!-----------------------------------------------------------------------!!
     !!                          SNAPSHOT DATA EXPORT                         !!
     !!-----------------------------------------------------------------------!!
     !!please see io.f90
     call ppohFDM_io_write()

     !!-----------------------------------------------------------------------!!
     !!                       VOLUME RENDERING DATA EXPORT                    !!
     !!-----------------------------------------------------------------------!!
     !!call ppohFDM_io_vol_psdiff( it, IOVOL )
     !!call ppohFDM_io_vol_dis( it, IOVOL )
  end do timestep

  if( is_fs ) then
     close( IOSPS )                                                       
     close( IOSNP )                                                       
     close( IOWAV )
  end if


  if( is_ioxy ) close( ioxy )
  if( is_ioyz ) close( ioyz )
  if( is_ioxz ) close( ioxz )

  if( myid == NP/2 ) then
     call system_clock( timcount, crate )
     ttotal = ttotal + tstep
     write(STDERR,'(A,I15,A)') &
          "Finished Computation. Total Time = ", int(ttotal), " sec."
  end if

  !math/mp-s
  call ppohMATHMP_FDM_finalize(.true.)
  !math/mp-e
  stop
contains

  subroutine get_max( xmax, ymax, zmax )
  !
  ! Returns maximum velocity amplitudes for x, y, z components on the surface
  !
    real(PN), intent(out) :: xmax, ymax, zmax

    integer :: i, j, kk
    real,    save :: xmax0, ymax0, zmax0
    logical, save :: is_firstcall = .true.

    ! initialize
    if( is_firstcall ) then
       xmax0 = 0.0_PN
       ymax0 = 0.0_PN
       zmax0 = 0.0_PN
       is_firstcall = .false.
    end if
    
    if( .not. is_fs ) then 
       xmax0 = -1.
       ymax0 = -1.
       zmax0 = -1.
    else
       do j=1, NYP, NYD
          do i=1, NXP, NXD
             kk = KFSZ(i,j)+1 ! top of the solid part
             xmax0 = max( xmax0, abs( VX( i,j,kk ) ) )
             ymax0 = max( ymax0, abs( VY( i,j,kk ) ) )
             zmax0 = max( zmax0, abs( VZ( i,j,kk ) ) )
          end do
       end do
    end if
    call mpi_reduce( xmax0, xmax, 1, MPI_REAL, MPI_MAX, NP/2, &
         my_comm, ierr )
    call mpi_reduce( ymax0, ymax, 1, MPI_REAL, MPI_MAX, NP/2, &
         my_comm, ierr )
    call mpi_reduce( zmax0, zmax, 1, MPI_REAL, MPI_MAX, NP/2, &
         my_comm, ierr )

  end subroutine get_max


  subroutine ppohFDM_truncate_diff_vel(idx,idy,idz)
  !
  ! Substitute the derivertives at around boundary to the recuced-order derivs.
  !
    integer, intent(in) :: idx, idy, idz

    integer :: i, j, k

    !! X dir
    if( idx == 0 ) then
       do K=1, NZP
          do J=1, NYP
             DXVX(1,J,K) = ( VX(1,J,K) - 0.0_PN    ) * DXI
             DXVX(2,J,K) = ( VX(2,J,K) - VX(1,J,K) ) * DXI
             DXVY(1,J,K) = ( VY(2,J,K) - VY(1,J,K) ) * DXI
             DXVZ(1,J,K) = ( VZ(2,J,K) - VZ(1,J,K) ) * DXI
          end do
       end do
    end if
    if( idx == IP-1 ) then
       do K=1, NZP
          do J=1, NYP
             DXVX(NXP  ,J,K) = ( VX(NXP,J,K) - VX(NXP-1,J,K) ) * DXI
             DXVY(NXP-1,J,K) = ( VY(NXP,J,K) - VY(NXP-1,J,K) ) * DXI
             DXVY(NXP  ,J,K) = ( 0.0_PN      - VY(NXP,  J,K) ) * DXI
             DXVZ(NXP-1,J,K) = ( VZ(NXP,J,K) - VZ(NXP-1,J,K) ) * DXI
             DXVZ(NXP  ,J,K) = ( 0.0_PN      - VZ(NXP,  J,K) ) * DXI
          end do
       end do
    end if

    if( idy == 0 ) then ! Shallowmost
       do K=1, NZP
          do I=1, NXP
             DYVX(I,1,K) = ( VX(I,2,K) - VX(I,1,K) ) * DYI
             DYVY(I,1,K) = ( VY(I,1,K) - 0.0_PN    ) * DYI
             DYVY(I,2,K) = ( VY(I,2,K) - VY(I,1,K) ) * DYI
             DYVZ(I,1,K) = ( VZ(I,2,K) - VZ(I,1,K) ) * DYI
          end do
       end do
    end if
    if( idy == JP-1 ) then
       do K=1, NZP
          do I=1, NXP
             DYVX(I,NYP-1,K) = ( VX(I,NYP,K) - VX(I,NYP-1,K) ) * DYI
             DYVX(I,NYP,  K) = ( 0.0_PN      - VX(I,NYP,  K) ) * DYI
             DYVY(I,NYP,  K) = ( VY(I,NYP,K) - VY(I,NYP-1,K) ) * DYI
             DYVZ(I,NYP-1,K) = ( VZ(I,NYP,K) - VZ(I,NYP-1,K) ) * DYI
             DYVZ(I,NYP,  K) = ( 0.0_PN      - VZ(I,NYP,  K) ) * DYI
          end do
       end do
    end if

    if( idz == 0 ) then ! Shallowmost
       do J=1, NYP
          do I=1, NXP
             DZVZ(I,J,1 ) = ( VZ(I,J,1) - 0.0_PN    ) * DZI
             DZVZ(I,J,2 ) = ( VZ(I,J,2) - VZ(I,J,1) ) * DZI
             DZVX(I,J,1 ) = ( VX(I,J,2) - VX(I,J,1) ) * DZI
             DZVY(I,J,1 ) = ( VY(I,J,2) - VY(I,J,1) ) * DZI
          end do
       end do
    end if
    if( idz == KP-1 ) then
       do J=1, NYP
          do I=1, NXP
             DZVZ(I,J,NZP  ) = ( VZ(I,J,NZP) - VZ(I,J,NZP-1) ) * DZI
             DZVY(I,J,NZP  ) = ( 0.0_PN      - VY(I,J,NZP  ) ) * DZI
             DZVY(I,J,NZP-1) = ( VY(I,J,NZP) - VY(I,J,NZP-1) ) * DZI
             DZVX(I,J,NZP  ) = ( 0.0_PN      - VX(I,J,NZP  ) ) * DZI
             DZVX(I,J,NZP-1) = ( VX(I,J,NZP) - VX(I,J,NZP-1) ) * DZI
          end do
       end do
    end if
    
  end subroutine ppohFDM_truncate_diff_vel

  subroutine set_mpi_environment( myid, itbl, idx, idy, idz )
  ! adapt mpi environment to mpi enviroment of ppOpen-MATH/MP
    integer, intent(out) :: myid
    integer, intent(out) :: itbl(-1:IP,-1:JP,-1:KP) 
    integer, intent(out) :: idx, idy, idz
    
!    call mpi_init( ierr )
!    call mpi_comm_size( mpi_comm_world, nproc, ierr )
!    call mpi_comm_rank( mpi_comm_world, myid , ierr )
    
    my_comm = ppohMATHMP_FDM_get_my_comm()
    myid    = ppohMATHMP_FDM_get_my_rank()
    nproc   = ppohMATHMP_FDM_get_my_size()

    if( nproc /= NP ) then
       call ppohMATHMP_FDM_abort()
       write(STDERR,'(A, I3, A, I3)') &
            '## NP Error ## Expected: ', NP, ' Prepared: ', nproc
       stop
       
    else if ( NZP < NL .or. NYP < NL .or. NZP < NL ) then
       call ppohMATHMP_FDM_abort()
       write(STDERR,'(A,I3,A,I3)') &
            '## NXP, NYP, NZP are too small: Must be larger than ', NL
       stop
    end if
    
    ! Communicate ID table
    itbl(-1:IP,-1:JP,-1:KP) =  MPI_PROC_NULL ! initialize
    do ii = 0, NP-1
       i = mod( ii, IP ) 
       j = mod( ii/IP, JP)
       k = ii/(IP*JP) 
       itbl(i,j,k) = ii
    end do

    ! location of this CPU
    idx = mod( MYID, IP ) 
    idy = mod( MYID/IP, JP)
    idz = MYID / (IP*JP) 

    ! MPI buffer area
    allocate( i1_sbuff(NYP*NZP*NL3), i2_sbuff(NYP*NZP*NL3) )
    allocate( i1_rbuff(NYP*NZP*NL3), i2_rbuff(NYP*NZP*NL3) )
    allocate( j1_sbuff(NXP*NZP*NL3), j2_sbuff(NXP*NZP*NL3) )
    allocate( j1_rbuff(NXP*NZP*NL3), j2_rbuff(NXP*NZP*NL3) )
    allocate( k1_sbuff(NXP*NYP*NL3), k2_sbuff(NXP*NYP*NL3) )
    allocate( k1_rbuff(NXP*NYP*NL3), k2_rbuff(NXP*NYP*NL3) )
    
   end subroutine set_mpi_environment
  !----------------------------------------------------------------------------!
   
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_set_free_surface( KFSZA, KFSZ, NIFS, NJFS, &
                               IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ )

    integer, intent(inout) :: KFSZA(-NL2-1:NX+NL2+1,-NL2-1:NY+NL2+1)
    integer, intent(out)   :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(out)   :: NIFS, NJFS
    integer, intent(out)   :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(out)   :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)

    integer :: i, j, k
    integer :: ii, jj, kk
    integer :: NIFSA, NJFSA
    integer :: dum(0:NX+1,0:NY+1)
    ! discontinuities in absolute coordinates
    integer :: IFSXA(NX1*NY1), IFSYA(NX1*NY1), IFSZA(NX1*NY1)
    integer :: JFSXA(NX1*NY1), JFSYA(NX1*NY1), JFSZA(NX1*NY1)

    !! 1 Gaussian filter for smoothing out too-small scale topographic change
    do J=0, NY+1
       do I=0, NX+1
          dum(i,j) = KFSZA(i,j)
       end do
    end do
    do J=1, NY
       do I=1, NX
          KFSZA(I,J) = 1*dum(i-1,j-1) + 2*dum(i-1,j  ) + 1*dum(i-1,j+1) &
                     + 2*dum(i  ,j-1) + 4*dum(i  ,j  ) + 2*dum(i  ,j+1) &
                     + 1*dum(i+1,j-1) + 2*dum(i+1,j  ) + 1*dum(i+1,j+1) 
          KFSZA(I,J) = KFSZA(I,J) / 16
       end do
    end do
    

    !! 2. ABSORBING REGION HAVE SAME STRUCTURE WITH LAYER BOUNDARY
    do J=1, NY1
       KFSZA(  -NL2+1:NPM   ,J) = KFSZA(NPM+1,J)
       KFSZA(NX-NPM+1:NX+NL2,J) = KFSZA(NX-NPM,J)
    end do
    do I=1, NX1
       KFSZA(I,  -NL2+1:NPM   ) = KFSZA(I,NPM+1)
       KFSZA(I,NY-NPM+1:NY+NL2) = KFSZA(I,NY-NPM)
    end do
    KFSZA(-NL2-1:NPM       ,-NL2+1:NPM        ) = KFSZA(NPM+1,NPM+1)
    KFSZA(NX-NPM+1:NX+NL2+1,-NL2+1:NPM        ) = KFSZA(NX-NPM,NPM+1)
    KFSZA(-NL2-1:NPM       ,NY-NPM+1:NY+NL2+1 ) = KFSZA(NPM+1 ,NY-NPM)
    KFSZA(NX-NPM+1:NX+NL2+1,NY-NPM+1:NY+NL2+1 ) = KFSZA(NX-NPM,NY-NPM)  
    
    !! 3. Horizontal Boundary Scan: X-dir
    NIFSA = 0
    do J=1, NY1
       do I=2, NX1
          if( KFSZA(I,J) > KFSZA(I-1,J) ) then
             do K=KFSZA(I-1,J), KFSZA(I,J)-1
                NIFSA = NIFSA+1
                IFSXA(NIFSA) = I-1 
                IFSYA(NIFSA) = J
                IFSZA(NIFSA) = K+1
             end do
          else if( KFSZA(I,J) < KFSZA(I-1,J) ) then
             do K=KFSZA(I,J), KFSZA(I-1,J)-1
                NIFSA = NIFSA+1
                IFSXA(NIFSA) = I-1
                IFSYA(NIFSA) = J
                IFSZA(NIFSA) = K+1
             end do
          end if
       end do
    end do
    
    !! 4. Horizontal Boundary Scan: Y-dir
    NJFSA = 0
    do I=1, NX1
       do J=2, NY1
          if( KFSZA(I,J) > KFSZA(I,J-1) ) then
             do K=KFSZA(I,J-1), KFSZA(I,J)-1
                NJFSA = NJFSA+1
                JFSXA(NJFSA) = I
                JFSYA(NJFSA) = J-1
                JFSZA(NJFSA) = K+1
             end do
          else if( KFSZA(I,J) < KFSZA(I,J-1) ) then
             do K=KFSZA(I,J), KFSZA(I,J-1)-1
                NJFSA = NJFSA+1
                JFSXA(NJFSA) = I
                JFSYA(NJFSA) = J-1
                JFSZA(NJFSA) = K+1
             end do
          end if
       end do
    end do
   
    
    !! 5.  Free surface boundary in MPI unit
    KFSZ(:,:) = NOSURF
    is_fs = .false. 
    do KK=1, NZP
       do J=NYP00, NYP10
          JJ = JA(J)
          do I=NXP00, NXP10
             II = IA(I)
             if( KA(KK) == KFSZA(II,JJ) ) then
                KFSZ(I,J) = KA(KK) - NZP*idz
                is_fs = .true. 
             end if
          end do
       end do
    end do
    
    ! 1-grid outside of the MPI unit: free surface condition must be consideard
    is_nearfs = .false. 
    do J=NYP0, NYP1
       JJ = JA(J)
       do I=NXP0, NXP1
          II = IA(I)
          if( KA(0    ) == KFSZA(II,JJ) ) then
             KFSZ(I,J) = KA(0    ) - NZP*idz
             is_nearfs = .true. 
          end if
          if( KA(NZP+1) == KFSZA(II,JJ) ) then
             KFSZ(I,J) = KA(NZP+1) - NZP*idz
             is_nearfs = .true.
          end if
       end do
    end do
    
    ! Horizontal step
    NIFS = 0
    NJFS = 0
    do I=1, NIFSA
       ii = IFSXA(I) - NXP * idx
       jj = IFSYA(I) - NYP * idy
       if( 0<= ii .and. ii <= NXP+1  ) then  ! including margin
          if( 1 <= jj .and. jj <= NYP ) then
             do KK=0, NZP+1
                if( KA(KK) == IFSZA(I) ) then
                   NIFS = NIFS+1
                   IFSX(NIFS) = ii
                   IFSY(NIFS) = jj
                   IFSZ(NIFS) = kk
                end if
             end do
          end if
       end if       
    end do
    do J=1, NJFSA
       ii = JFSXA(J) - NXP * idx
       jj = JFSYA(J) - NYP * idy
       if( 1<= ii .and. ii <= NXP  ) then  
          if( 0 <= jj .and. jj <= NYP+1 ) then ! including margin
             do KK=0, NZP+1
                if( KA(KK) == JFSZA(J) ) then
                   NJFS = NJFS+1
                   JFSX(NJFS) = ii
                   JFSY(NJFS) = jj
                   JFSZ(NJFS) = kk
                end if
             end do
          end if
       end if
    end do
    
  end subroutine ppohFDM_set_free_surface


  subroutine ppohFDM_initialize_arrays()

    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1,  VX,    0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1,  VY,    0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1,  VZ,    0.0_PN )

    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1, SXX,   0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1, SYY,   0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1, SZZ,   0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1, SXY,   0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1, SXZ,   0.0_PN )
    call ppohFDM_clear3d( NXP0, NXP1, NYP0, NYP1, NZP0, NZP1, SYZ,   0.0_PN )

  end subroutine ppohFDM_initialize_arrays

  subroutine velocity_output_binary()
    integer :: i, j, k,l
    integer :: coord_x, coord_y, coord_z
    character(len=80)::filename

    if (flag == 1) then
!       if ( myid==119 .or.myid==120 .or. myid==135 .or. myid==136) then
          write(filename, '("./output/Velocity-",i4.4,"-",i5.5,".dat")') myid,IT

          open (11,file=filename,form='unformatted',status='replace',access='direct',recl=12)
          l=1
          do k=1, NZP
             do j=1, NYP
                do i=1, NXP
                   coord_x = (i-1)+NXP*idx
                   coord_y = (j-1)+NYP*idy
                   coord_z = (k-1)+NZP*idz

                   if(coord_x>=SX0.and.coord_x<=EX1&
                        .and.coord_y>=SY0.and.coord_y<=EY1&
                        .and.coord_z>=SZ0.and.coord_z<=EZ1) then

                      write(11, rec=l) VX(i,j,k),VY(i,j,k),VZ(i,j,k)

                      l=l+1
                   end if
                end do
             end do
          end do
          close(11)
       !end if
    end if
  end subroutine velocity_output_binary

  subroutine velocity_output_ascii()
    integer :: i, j, k
    real(PN) :: coord_x, coord_y, coord_z
    character(len=80)::filename

    if (flag == 1) then
       write(filename, '("./output/Velocity-",i4.4,"-",i4.4,".dat")') myid+1,IT
       open(11, file=filename, status='replace')
       do k=1, NZP
          do j=1, NYP
             do i=1, NXP
                coord_x = ((i-1)+NXP*idx)*dx
                coord_y = ((j-1)+NYP*idy)*dy
                coord_z = ((k-1)+NZP*idz)*dz

                if ( coord_x>=SX0.and.coord_x<=EX1&
                     .and.coord_y>=SY0.and.coord_y<=EY1&
                     .and.coord_z>=SZ0.and.coord_z<=EZ1) then
                   write(11, '(ES12.5" "ES12.5" "ES12.5)') VX(i,j,k),VY(i,j,k),VZ(i,j,k)
                end if
             end do
          end do
       end do
       close(11)
    end if
  end subroutine velocity_output_ascii

  subroutine ppohFDM_flag()
    integer :: i, j, k
    integer :: coord_x, coord_y, coord_z

    flag = 0
    do k=1, NZP
       do j=1, NYP
          do i=1, NXP
             coord_x = (i-1)+NXP*idx
             coord_y = (j-1)+NYP*idy
             coord_z = (k-1)+NZP*idz

             if ( coord_x>=SX0.and.coord_x<=EX1&
                  .and.coord_y>=SY0.and.coord_y<=EY1&
                  .and.coord_z>=SZ0.and.coord_z<=EZ1) then
                flag = 1
             end if
          end do
       end do
    end do
!    if(flag==1) print *,'hoge',myid
  end subroutine ppohFDM_flag
end program seism3d3n

subroutine interpolate_data(recv_comp_name, send_comp_name, mapping_tag, &
                            sn1, sn2, send_data, rn1, rn2, recv_data, num_of_data, num_of_tag, exchange_tag)
  ! math/mp-s
  implicit none
  character(len=*), intent(IN) :: recv_comp_name, send_comp_name
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: sn1, sn2
  real(kind=8), intent(IN) :: send_data(sn1, sn2)
  integer, intent(IN) :: rn1, rn2
  real(kind=8), intent(INOUT) :: recv_data(rn1, rn2)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: num_of_tag
  integer, intent(IN) :: exchange_tag
  ! math/mp-e
end subroutine interpolate_data
