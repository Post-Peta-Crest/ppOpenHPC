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
module ppohFDM_set_condition
!
! This module sets up the medium parameters of subsurface structure,
! source, and station locations in the simulation model
!
  use ppohFDM_stdio
  use ppohFDM_param
  implicit none
contains


  subroutine ppohFDM_set_medium_param()
  !
  ! Sets up the test medium for the subsurface structure.
  !
    integer :: i
    character(len=80) filename
    character(len=80) tmp1

    filename="medium.dat"
    open(7,file=filename,status='old')
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) tmp1
    print *,'Number of medium layer'
    read(7,*) MST
    print *, MST
    print *,'Medium parameter [ZDEP RO VP VS]'
    do i=1, MST
       read(7, *) ZDEP(i), RO1(i), VP1(i), VS1(i)
       print *,ZDEP(i), RO1(i), VP1(i), VS1(i)
    end do
    close(7)
  end subroutine ppohFDM_set_medium_param


  subroutine ppohFDM_set_medium( DEN, RIG, LAM )
  !
  ! Sets up elastic parameters (density, rigidity, and lam.) in the subsurface structure.
  !
    real(PN), intent(out), dimension(0:NXP1,0:NYP1,0:NZP1) :: &
                                                    DEN, RIG, LAM

    real(PN) :: RO, VP, VS
    integer  :: I, J, K, kz, ii, jj, kk, M
    integer  :: kza(MST+1)

    !modify for set_medium parameter
    !RO1 ,RO2 and etc are defined in param.f90
    call ppohFDM_set_medium_param()
    kza(1)=KFS
    do J = 0, NYP1
       do I = 0, NXP1
          do K = 0, NZP1
             do M=1, MST
                ii = IA(i)
                jj = JA(j)
                kk = KA(k)
                kz = int (ZDEP(M)/Dz+0.5)+KFSZA(ii,jj)
                kza(M+1) = kz
                if(kk <= KFSZA(ii,jj)) then
                   !Air side that is fixed parameter.
                   RO = 0.001_PN
                   VP = 0.0_PN
                   VS = 0.0_PN
                   DEN  (I,J,K) = RO
                   RIG  (I,J,K) = RO*VS**2
                   LAM  (I,J,K) = RO*VP**2 - 2*RO*VS**2
                else if(kza(M) < kk .and. kk <= kza(M+1)) then
                   !M layers Ground side that is varied in accordance with medium.dat
                   RO = RO1(M)
                   VP = VP1(M)
                   VS = VS1(M)
                   DEN  (I,J,K) = RO
                   RIG  (I,J,K) = RO*VS**2
                   LAM  (I,J,K) = RO*VP**2 - 2*RO*VS**2
                else if(kza(MST+1) < kk) then
                   RO = RO1(MST)
                   VP = VP1(MST)
                   VS = VS1(MST)
                   DEN  (I,J,K) = RO
                   RIG  (I,J,K) = RO*VS**2
                   LAM  (I,J,K) = RO*VP**2 - 2*RO*VS**2
                end if
             end do
          end do
       end do
    end do

  end subroutine ppohFDM_set_medium


  subroutine ppohFDM_set_source()
  !
  ! Sets up the source location and mechanism.
  !
    real :: Xtmp, Ytmp, Ztmp
    character(len=80) filename
    character(len=80) tmp1
    character(len=80) tmp2
    
    filename="source.dat"
    open(7,file=filename,status='old')
    read(7,*) tmp1
    read(7,*) tmp2
    read(7,*) tmp1
    read(7,*) tmp2
    read(7, *) Xtmp, Ytmp, Ztmp
    I0 = int (Xtmp/Dx + 0.5)
    J0 = int (Ytmp/Dy + 0.5)
    K0 = int (Ztmp/Dz + 0.5)+KFS
    STRIKE = 0.0
    DIP    = 45.0
    RAKE   = 90.0
    AT = 2.0_PN/4
    T0 = AT * 2
    read(7, *) STRIKE, DIP, RAKE
    read(7, *) AT, T0
    print *,'SOURCE XYZ'
    print *,I0, J0, K0
    print *,'STRIKE, DIP, RAKE, AT, T0'
    print *,STRIKE, DIP, RAKE, AT, T0
    close(7)

    I1 = I0 - IA(1) +1
    J1 = J0 - JA(1) +1
    K1 = K0 - KA(1) +1
    !! Body-force source (0<=I<=NXP+1,0<=J<=NYP+1,0<=K<=NZP+1)
    if( K1 >= 0 .and. K1 <= NZP+1 .and. &
         I1 >= 0 .and. I1 <= NXP+1 .and. &
         J1 >= 0 .and. J1 <= NYP+1       ) then
       is_src = .true.
       !     write(STDERR,'(A,3I3,A)') "SOURCE IS LOCATED AT (", idx, idy, idz, ")"
    else
       is_src = .false.
    end if
    
    !! Snapshot Switches
    is_ioxy = .false.; is_ioyz = .false.; is_ioxz = .false.
    if( I1 >= 1 .and. I1 <= NXP ) is_ioyz = .true.
    if( J1 >= 1 .and. J1 <= NYP ) is_ioxz = .true.
    if( K1 >= 1 .and. K1 <= NZP ) is_ioxy = .true.
    
  end subroutine ppohFDM_set_source


  subroutine ppohFDM_set_station()
  !
  ! Sets up the number of stations and their locations.
  !
    real :: Xtmp, Ytmp, Ztmp
    integer :: i
    character(len=80) filename
    character(len=80) tmp1
    
    filename="station.dat"
    open(7,file=filename,status='old')
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) NST
    print *,'station points'
    do i=1,NST
       read(7, *) Xtmp, Ytmp, Ztmp
       ISTXA(i) = int (Xtmp/Dx + 0.5)
       ISTYA(i) = int (Ytmp/Dy + 0.5)
       ISTZA(i) = int (Ztmp/Dz + 0.5) + KFS +1
       print *, ISTXA(i), ISTYA(i), ISTZA(i)
    end do
    close(7)
  end subroutine ppohFDM_set_station


  subroutine ppohFDM_station_func()
    is_station = .false. 
    do I=1, NST
       if( IA(1)<= ISTXA(I) .and. ISTXA(I) <= IA(NXP) .and. &
            JA(1)<= ISTYA(I) .and. ISTYA(I) <= JA(NYP) .and. &
            KA(1)<= ISTZA(I) .and. ISTZA(I) <= KA(NZP)       ) then
          is_station = .true.
          ISTX(I) = ISTXA(I) - IA(1) + 1
          ISTY(I) = ISTYA(I) - JA(1) + 1
          ISTZ(I) = ISTZA(I) - KA(1) + 1
          !   write(STDERR,'(A,I3,A,3I3)') "ST",I,"is here:", idx,idy,idz
          !   write(STDERR,'(3I5)') istxa(i), istya(i), istza(i)
       else
          ISTXA(I) = 0
          ISTYA(I) = 0
          ISTZA(I) = 0
       end if
    end do
  end subroutine ppohFDM_station_func
end module ppohFDM_set_condition
