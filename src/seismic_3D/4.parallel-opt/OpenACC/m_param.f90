!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.2                                               !
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
!                 Copyright (c) 2013 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_param
!
! Declares all of the parameters used to specify the FDM simulation model
!
  use ppohFDM_stdio
  implicit none
  public

  !-<< Execute Title >>
  character(99), parameter :: TITLE="SEISM3D3"
  !-- << Model Size and Grid Width >>
  integer,  parameter :: NX      = 128
  integer,  parameter :: NY      = 128
  integer,  parameter :: NZ      = 128
  integer,  parameter :: KFS     = 25    ! Free Surface Grid
  integer,  parameter :: NX1     = NX+1
  integer,  parameter :: NY1     = NY+1
  integer,  parameter :: NZ1     = NZ+1
  integer,  parameter :: NTMAX   = 2000
  integer,  parameter :: NWRITE  = 10
  real(PN), parameter :: DX      = 0.5_PN
  real(PN), parameter :: DY      = 0.5_PN
  real(PN), parameter :: DZ      = 0.5_PN
  real(PN), parameter :: DT      = 0.025_PN 
  integer,  parameter :: NDUMP   = 5 ! Volume data dumping

  !-<< Parallel >> 
  integer, parameter :: IP     = 2        ! X-Division, >= 2
  integer, parameter :: JP     = 2        ! Y-Division, >= 2
  integer, parameter :: KP     = 2        ! Z-Division, >= 2
  integer, parameter :: NP     = IP*JP*KP ! Number of process
  integer, parameter :: NL     = 4      ! Order of the fd scheme
  integer, parameter :: NXP    = NX/IP    ! X-size per 1 process
  integer, parameter :: NYP    = NY/JP    ! X-size per 1 process
  integer, parameter :: NZP    = NZ/KP    ! X-size per 1 process

  integer, parameter :: NL2    = NL/2
  integer, parameter :: NL3    = NL2*3
  integer, parameter :: NXP00  = -NL2+1
  integer, parameter :: NXP10  = NXP+NL2
  integer, parameter :: NYP00  = -NL2+1
  integer, parameter :: NYP10  = NYP+NL2
  integer, parameter :: NXP0   = (-NL2+1)  -2 
  integer, parameter :: NXP1   = (NXP+NL2) +1
  integer, parameter :: NYP0   = (-NL2+1)  -2 
  integer, parameter :: NYP1   = (NYP+NL2) +1
  integer, parameter :: NZP0   = (-NL2+1)  -2 
  integer, parameter :: NZP1   = (NZP+NL2) +1
  integer, parameter :: NOSURF = NZP0+1

  !-- << Source >>
  real(PN), parameter :: UC      = 10.0_PN**(-15)          ! Unit correction
  real(PN), parameter :: RMO     = 1E15*UC                 ! (rho vs^2) D S


  !-- << Attenuation >>
  real(PN), parameter :: F0     = 1.0_PN                   ! Center Frequency

  !-- << Waveform Output >>
  integer,  parameter :: NSTMAX  = 100
  integer,  parameter :: NSKIP   = 1                       ! Waveform Decimation
  integer,  parameter :: NTMAX1  = NTMAX/NSKIP

  !-- << Snapshots >>
  integer,  parameter :: NXD     = 1                       ! Decimation factor
  integer,  parameter :: NYD     = 1                       ! Decimation factor
  integer,  parameter :: NZD     = 1                       ! Decimation factor
  integer,  parameter :: NXS     = NXP/NXD                 ! Snapshot Size
  integer,  parameter :: NYS     = NYP/NYD                 ! Snapshot Size
  integer,  parameter :: NZS     = NZP/NZD                 ! Snapshot Size

  !-- << ABSORBING BOUNDARY CONDITION >>
  integer, parameter  :: NPM     = 20                      ! size (15-20)
  
  !-- << I/O Numbers: BIG ENDIAN: Assumes 900-999 >>
  integer,  parameter :: IOSPS = IOBIG1
  integer,  parameter :: IOWAV = IOBIG2
  integer,  parameter :: IOSNP = IOBIG3
  integer,  parameter :: IOXY  = IOBIG4
  integer,  parameter :: IOYZ  = IOBIG5
  integer,  parameter :: IOXZ  = IOBIG6
  integer,  parameter :: IOVOL = 500

  !-- << Output Filename >>
  character(80), parameter :: ONAME  = trim(TITLE)//".SPS" ! P/S deconposition
  character(80), parameter :: WNAME  = trim(TITLE)//".WAV" ! Waveforms
  character(80), parameter :: SNAME  = trim(TITLE)//".SUR" ! Surf Deformation
  character(80), parameter :: XYNAME = trim(TITLE)//".XY"  ! XY-Snap
  character(80), parameter :: YZNAME = trim(TITLE)//".YZ"  ! YZ-Snap
  character(80), parameter :: XZNAME = trim(TITLE)//".XZ"  ! XZ-Snap

  !-- << Binary Size >>
  integer,  parameter :: IWORD   = 4                       ! Unit Record Length
  integer,  parameter :: IWCL_W  = 100*4/IWORD             ! Record Size

  !!-- << Medium Parameters >>
  real(PN) :: DEN(0:NXP1,0:NYP1,0:NZP1)                    ! Density 
  real(PN) :: RIG(0:NXP1,0:NYP1,0:NZP1)                    ! Rigidity
  real(PN) :: LAM(0:NXP1,0:NYP1,0:NZP1)                    ! Lame's coefficient
  real(PN) :: QP (0:NXP1,0:NYP1,0:NZP1)                    ! P intrinsic Q
  real(PN) :: QS (0:NXP1,0:NYP1,0:NZP1)                    ! S intrinsic Q
  real(PN) :: TU (0:NXP1,0:NYP1,0:NZP1)                    ! Relaxiation time

  !!-- << Independent Variables >>
  real(PN) :: SXX(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: SYY(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: SZZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: SXY(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: SYZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: SXZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: VX (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: VY (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: VZ (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)                           

  !!-- << Derivertives wrt Space >>
  real(PN) :: DXVX (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DXVY (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DXVZ (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DYVX (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DYVY (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DYVZ (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DZVX (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DZVY (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DZVZ (NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DXSXX(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DXSXY(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DXSXZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DYSYY(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DYSXY(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DYSYZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DZSZZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DZSXZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)
  real(PN) :: DZSYZ(NXP0:NXP1,NYP0:NYP1,NZP0:NZP1)

  !!-- << Source Time Function >>
  real(PN) :: STIME  (NTMAX)

  !!-- << Station >>
  integer  :: ISTXA(100), ISTYA(100), ISTZA(100)
  integer  :: ISTX(100), ISTY(100), ISTZ(100)
  

  !!-- << Free Surface >>
  integer :: KFSZ(NXP0:NXP1,NYP0:NYP1)
  integer, parameter :: NFSMAX = NXP1*NYP1
  integer :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX) ! X-dir
  integer :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX) ! Y-dir
  integer :: NIFS, NJFS                               ! # of discontinuities

  !!-- << Free Surface by absolute coordinate >>
  integer :: KFSZA(-NL2-1:NX+NL2+1,-NL2-1:NY+NL2+1)

  !!--<< SPONGE Boundary condition >>
  real(PN) :: gx(NXP), gy(NYP), gz(NZP)

  !<<Source >>  
  integer :: I0                         ! Source Loc x
  integer :: J0                         ! Source Loc y
  integer :: K0                         ! Source Loc z
  real(PN) :: STRIKE                    ! Double couple
  real(PN) :: DIP                       ! Double couple
  real(PN) :: RAKE                      ! Double couple
  real(PN) :: AT      
  real(PN) :: T0                        ! Onset Time

  !<< Counters >>
  integer  :: I, J, K, II, JJ, KK
  integer  :: IT

  !<< Physical Variables >>
  real(PN) :: T ! time 
  real(PN) :: RMXX, RMXY, RMXZ, RMYY, RMYZ, RMZZ   ! Moment Tensor

  !<< Lapse Time Measurement >>
  integer  :: timcount, timcount0, timprev, crate
  real(PN) :: tstep, ttotal
  real(PN) :: etas
  integer  :: etah, etam, etasi

  !<< MPI >>
  integer :: ierr
  integer :: nproc, myid
  integer :: idx, idy, idz
  logical :: is_fs, is_nearfs
  integer :: IA(NXP0:NXP1), JA(NYP0:NYP1), KA(0:NZP1)
  integer :: I1, J1, K1
  integer :: itbl(-1:IP,-1:JP,-1:KP) 
  logical :: is_src
  logical :: is_station
  logical :: is_ioxy, is_ioyz, is_ioxz

  !! ES2 with '-gmalloc' option
#ifdef _OPENACC
  real(PN), allocatable, pinned :: i1_sbuff(:), i2_sbuff(:) 
  real(PN), allocatable, pinned :: j1_sbuff(:), j2_sbuff(:) 
  real(PN), allocatable, pinned :: k1_sbuff(:), k2_sbuff(:) 
  real(PN), allocatable, pinned :: i1_rbuff(:), i2_rbuff(:) 
  real(PN), allocatable, pinned :: j1_rbuff(:), j2_rbuff(:) 
  real(PN), allocatable, pinned :: k1_rbuff(:), k2_rbuff(:) 
#else
  real(PN), allocatable :: i1_sbuff(:), i2_sbuff(:) 
  real(PN), allocatable :: j1_sbuff(:), j2_sbuff(:) 
  real(PN), allocatable :: k1_sbuff(:), k2_sbuff(:) 
  real(PN), allocatable :: i1_rbuff(:), i2_rbuff(:) 
  real(PN), allocatable :: j1_rbuff(:), j2_rbuff(:) 
  real(PN), allocatable :: k1_rbuff(:), k2_rbuff(:) 
#endif

  !! Working
  real(PN) :: DXI, DYI, DZI

  !! Max value check for debugging
  real(PN) :: xmax, ymax, zmax

  !! Medium value : import from .dat
  integer :: MST  = 100
  real(PN) :: ZDEP(100), RO1(100), VP1(100), VS1(100), QQ1(100)

  !! Station
  integer :: NST  = 100                       ! Stations

end module ppohFDM_param
