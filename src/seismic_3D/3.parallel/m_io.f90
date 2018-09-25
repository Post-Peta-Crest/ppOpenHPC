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
module ppohFDM_io
  use ppohFDM_stdio
  use ppohFDM_param
  use ppohFDM_pssub
  implicit none
  public

contains


  subroutine ppohFDM_io_write_wav( it, io )
  !
  ! Exports a waveform trace of velocity and displacements at stations at time step (it) to file number (io)
  !
    integer, intent(in) :: it
    integer, intent(in) :: io

    real(PN) :: VXALL(NSTMAX,NTMAX1), VYALL(NSTMAX,NTMAX1), VZALL(NSTMAX,NTMAX1) 
    real(PN) :: UXALL(NSTMAX,NTMAX1), UYALL(NSTMAX,NTMAX1), UZALL(NSTMAX,NTMAX1) 

    integer :: ns
    integer :: istax, istay, istaz
    real(PN), save :: ux(NSTMAX), uy(NSTMAX), uz(NSTMAX)
    real(PN), save :: vx0(NSTMAX), vy0(NSTMAX), vz0(NSTMAX)
    integer :: it1
    character(12) :: cproc

    
    if( .not. is_station ) return
    
    if( it == 1 ) then
       call ppohFDM_clear2d( 1, NST, 1, NTMAX1, VXALL, 0.0_PN )
       call ppohFDM_clear2d( 1, NST, 1, NTMAX1, VYALL, 0.0_PN )
       call ppohFDM_clear2d( 1, NST, 1, NTMAX1, VZALL, 0.0_PN )
       call ppohFDM_clear2d( 1, NST, 1, NTMAX1, UXALL, 0.0_PN )
       call ppohFDM_clear2d( 1, NST, 1, NTMAX1, UYALL, 0.0_PN )
       call ppohFDM_clear2d( 1, NST, 1, NTMAX1, UZALL, 0.0_PN )
       
       do ns = 1, NST
          istax  =  ISTX ( NS )                                           
          istay  =  ISTY ( NS ) 
          istaz  =  ISTZ ( NS ) 
          if( istax > 0 .and. istay > 0 .and. istaz > 0 ) then
             UXALL ( ns,1 ) = VX(istax,istay,istaz) * 0.5_PN * DT
             UYALL ( ns,1 ) = VY(istax,istay,istaz) * 0.5_PN * DT
             UZALL ( ns,1 ) = VZ(istax,istay,istaz) * 0.5_PN * DT 
             vx0(ns) = VX(istax,istay,istaz)
             vy0(ns) = VY(istax,istay,istaz)
             vz0(ns) = VZ(istax,istay,istaz)
          end if
          ux(ns) = 0.0_PN
          uy(ns) = 0.0_PN
          uz(ns) = 0.0_PN
       end do
       
    else
       
       do ns = 1, NST                                                 
          istax  =  ISTX ( NS )                                           
          istay  =  ISTY ( NS ) 
          istaz  =  ISTZ ( NS ) 
          if( istax > 0 .and. istay > 0 .and. istaz > 0 ) then
             ! Numerical integration
             UX(ns) = UX(ns) + (VX0(ns)+VX(istax,istay,istaz))*0.5_PN*DT
             UY(ns) = UY(ns) + (VY0(ns)+VY(istax,istay,istaz))*0.5_PN*DT
             UZ(ns) = UZ(ns) + (VZ0(ns)+VZ(istax,istay,istaz))*0.5_PN*DT
             ! store previous time-step value
             vx0(ns) = VX(istax,istay,istaz)
             vy0(ns) = VY(istax,istay,istaz)
             vz0(ns) = VZ(istax,istay,istaz)
          end if
       end do
    end if
    
    if( mod( it, NSKIP ) == 0 ) then
       it1 = it / nskip
       do ns=1, NST
          if( istxa(ns)>0 .and. istya(ns)>0 .and. istza(ns)>0 ) then
             istax  =  ISTX ( NS )                                           
             istay  =  ISTY ( NS ) 
             istaz  =  ISTZ ( NS ) 
             if( istax > 0 .and. istay > 0 .and. istaz > 0 ) then
                VXALL  ( ns,it1 ) = VX (istax,istay,istaz)
                VYALL  ( ns,it1 ) = VY (istax,istay,istaz)
                VZALL  ( ns,it1 ) = VZ (istax,istay,istaz) 
                UXALL  ( ns,it1 ) = UX (ns)
                UYALL  ( ns,it1 ) = UY (ns)
                UZALL  ( ns,it1 ) = UZ (ns)
             end if
          end if
       end do
       
       write(cproc,'(A,I3.3,A,I3.3,A,I3.3)') '.', idx, '.', idy, '.', idz
      
       ! overwrite waveform file
       open( io, file='./out/'//trim(WNAME)//cproc,  &
            form='unformatted', status='unknown', iostat=ierr )
       write( io ) NTMAX1, DT*NSKIP, NST
       write( io ) AT, T0
       write( io ) I0, J0, K0, ISTXA(1:NST), ISTYA(1:NST), ISTZA(1:NST)
       write( io ) VXALL(1:NST,:), VYALL(1:NST,:), VZALL(1:NST,:)
!       write( io ) UXALL, UYALL, UZALL
       close( io )
    end if
    
  end subroutine ppohFDM_io_write_wav


  subroutine ppohFDM_io_open_snp( io, fname, n1, n2, flag )
  !
  ! Creates a file (fname) for exporting snapshots of a seismic wavefield with a size of w*h grid points.
  !
    integer,      intent(in) :: io
    character(*), intent(in) :: fname
    integer,      intent(in) :: n1
    integer,      intent(in) :: n2
    logical,      intent(in) :: flag   ! open file if flag=.true.

    integer :: iwcl
    integer  :: ierr
    character(12) :: cproc


    if( .not. flag ) return

    write(cproc,'(A,I3.3,A,I3.3,A,I3.3)') '.', idx, '.', idy, '.', idz
    
    iwcl = n1*n2*4/iword ! binary record size

    open( io, file='./out/'//trim(fname)//cproc, access='direct', recl=iwcl, &
          iostat=ierr, action='write', status='replace' )

    if( ierr /= 0 ) then
       write(STDERR,'(A,I4,A)') 'FILE ', io, ' OPEN ERROR: '//trim(fname)
    end if
    
  end subroutine ppohFDM_io_open_snp


  subroutine ppohFDM_io_write_sur( it, io )
  !
  ! Exports snapshots of a seismic wavefield of a free surface at time step (it) to file number (io).
  !
    integer, intent(in) :: it
    integer, intent(in) :: io

    real(PN), save :: VXSNAP(NXS,NYS), VYSNAP(NXS,NYS), VZSNAP(NXS,NYS)
    real(PN), save :: VHSNAP(NXS,NYS)
    integer,  save :: isnap = 1
    integer        :: i, j, ii, jj, kk
    
    !! Write ?
    if( .not. is_fs ) return

    !! First call
    if( it == 1 ) then

       do J = 1, NYS
          do I = 1, NXS
             ii = I*NXD
             jj = J*NYD
             kk = KFSZ(ii,jj)+1
             if( KFSZ(ii,jj) == NOSURF ) then ! use dummy value
                VZSNAP(i,j) = -99999.0_PN
                VHSNAP(i,j) = -99999.0_PN
             else
                VZSNAP (i,j) = RIG (ii,jj,kk)
                VHSNAP (i,j) = DEN (ii,jj,kk)
             end if
             
          end do
       end do
       
       write ( io, rec=ISNAP ) VZSNAP
       isnap = 2
       write ( io, rec=ISNAP ) VHSNAP

       call ppohFDM_clear2d( 1, NXS, 1, NYS, VXSNAP, 0.0_PN )
       call ppohFDM_clear2d( 1, NXS, 1, NYS, VYSNAP, 0.0_PN )
       call ppohFDM_clear2d( 1, NXS, 1, NYS, VZSNAP, 0.0_PN )

    end if

    
    !! Preparation at every time step
    do j = 1, NYS
       do i = 1, NXS
          ii = i*NXD
          jj = j*NYD
          kk = KFSZ(ii,jj)+1
          VXSNAP(i,j) = VXSNAP(i,j) + VX(ii,jj,kk) * DT
          VYSNAP(i,j) = VYSNAP(i,j) + VY(ii,jj,kk) * DT
          VZSNAP(i,j) = VZSNAP(i,j) + VZ(ii,jj,kk) * DT
          VHSNAP(i,j) = sqrt( VXSNAP(i,j)**2 + VYSNAP(i,j)**2 )
       end do
    end do
    
    !! Export binary
    if( mod( it, NWRITE ) == 0 ) then
       isnap = isnap + 1
       write( io, rec = isnap ) real(abs(VZSNAP))
       isnap = isnap + 1
       write( io, rec = isnap ) real(VHSNAP)
    end if
    

  end subroutine ppohFDM_io_write_sur


  subroutine ppohFDM_io_write_sps( it, io )
    integer, intent(in) :: it
    integer, intent(in) :: io

    real(PN), save :: VPSNAP(NXS,NYS), VSSNAP(NXS,NYS)
    integer,  save :: isnap = 1
    integer        :: i, j, ii, jj, kk
    
    !! Write ?
    if( .not. is_fs ) return
    
    !! First call
    if( it == 1 ) then
       
       do J = 1, NYS
          do I = 1, NXS
             ii = I*NXD
             jj = J*NYD
             kk = KFSZ(ii,jj)+1
             if( KFSZ(ii,jj) == NOSURF ) then ! use dummy value
                VPSNAP(i,j) = -99999.0_PN
                VSSNAP(i,j) = -99999.0_PN
             else
                VPSNAP (i,j) = RIG (ii,jj,kk)
                VSSNAP (i,j) = DEN (ii,jj,kk)
             end if
             
          end do
       end do
       
       write ( io, rec=ISNAP ) VPSNAP
       isnap = 2
       write ( io, rec=ISNAP ) VSSNAP
       
    end if
    
    
    !! Export binary
    if( mod( it, NWRITE ) == 0 ) then
       
       !! Prepare P- and S- wave amplitude
       do j = 1, NYS
          do i = 1, NXS
             ii = i*NXD
             jj = j*NYD
             kk = KFSZ(ii,jj)+1
             
             ! divergence( v )
             VPSNAP (i,j) = abs( DXVX(ii,jj,kk) + DYVY(ii,jj,kk) &
                  + DZVZ(ii,jj,kk) )
             ! abs(rotx)+abs(roty)+abs(rotz)
             VSSNAP (i,j) = abs( DXVY(ii,jj,kk) - DYVX(ii,jj,kk) )&
                  + abs( DYVZ(ii,jj,kk) - DZVY(ii,jj,kk) )&
                  + abs( DZVX(ii,jj,kk) - DXVZ(ii,jj,kk) )
          end do
       end do
       
       isnap = isnap + 1
       write( io, rec = isnap ) real(VPSNAP)
       isnap = isnap + 1
       write( io, rec = isnap ) real(VSSNAP)
    end if
    
    
  end subroutine ppohFDM_io_write_sps


  subroutine ppohFDM_io_write_xy( it, io )
  !
  ! Exports snapshots of a seismic wavefield of a user-specified horizontal (x-y) plane at time step (it) to file number (io).
  !
    integer, intent(in) :: it
    integer, intent(in) :: io

    real(PN), save :: VPSNAP(NXS,NYS), VSSNAP(NXS,NYS)
    integer,  save :: isnap = 1
    integer        :: i, j, ii, jj, kk
    
    !! Write ?
    if( .not. is_ioxy ) return

    !! First call
    if( it == 1 ) then

       do j = 1, NYS
          do i = 1, NXS
             ii = i*NXD
             jj = j*NYD
             kk = K1
             VPSNAP (i,j) = RIG (ii,jj,kk)
             VSSNAP (i,j) = DEN (ii,jj,kk)
          end do
       end do
       
       write ( io, rec=ISNAP ) VPSNAP
       isnap = 2
       write ( io, rec=ISNAP ) VSSNAP
    end if

    
    !! Export binary
    if( mod( it, NWRITE ) == 0 ) then
       
       !! Prepare P- and S- wave amplitude
        do j = 1, NYS
           do i = 1, NXS
              ii = i*NXD
              jj = j*NYD
              kk = K1

              ! divergence( v )
              VPSNAP (i,j) = abs( DXVX(ii,jj,kk) + DYVY(ii,jj,kk) &
                                + DZVZ(ii,jj,kk) )
              ! abs(rotx)+abs(roty)+abs(rotz)
              VSSNAP (i,j) = abs( DXVY(ii,jj,kk) - DYVX(ii,jj,kk) )&
                           + abs( DYVZ(ii,jj,kk) - DZVY(ii,jj,kk) )&
                           + abs( DZVX(ii,jj,kk) - DXVZ(ii,jj,kk) )
           end do
        end do
        
        isnap = isnap + 1
        write( io, rec = isnap ) real(VPSNAP)
        isnap = isnap + 1
        write( io, rec = isnap ) real(VSSNAP)

    end if
    
  end subroutine ppohFDM_io_write_xy


  subroutine ppohFDM_io_write_xz( it, io )
  !
  ! Exports snapshots of a seismic wavefield of a user-specified vertical (x-z) plane at time step (it) to file number (io).
  !
    integer, intent(in) :: it
    integer, intent(in) :: io

    real(PN), save :: VPSNAP(NXS,NZS), VSSNAP(NXS,NZS)
    integer,  save :: isnap = 1
    integer        :: i, k, ii, jj, kk

    if( .not. is_ioxz ) return

    !! First call
    if( it == 1 ) then
       
       do k=1, NZS
          do i=1, NXS
             ii = i * NXD
             jj = j1
             kk = k * NZD
             VPSNAP(i,k) = RIG(ii,jj,kk)
             VSSNAP(i,k) = DEN(ii,jj,kk)
             
          end do
       end do
       
       write( io, rec=isnap ) VPSNAP
       isnap = isnap + 1
       write( io, rec=isnap ) VSSNAP
    end if
    
    
    !! Export binary
    if( mod( it, NWRITE ) == 0 ) then
       
       !! Prepare P- and S- wave amplitude
       do k = 1, NZS
          do i = 1, NXS
             ii = i*NXD
             jj = j1
             kk = k*NZD
             
             ! divergence( v )
             VPSNAP (i,k) = abs( DXVX(ii,jj,kk) + DYVY(ii,jj,kk) &
                  + DZVZ(ii,jj,kk) )
             ! abs(rotx)+abs(roty)+abs(rotz)
             VSSNAP (i,k) = abs( DXVY(ii,jj,kk) - DYVX(ii,jj,kk) )&
                  + abs( DYVZ(ii,jj,kk) - DZVY(ii,jj,kk) )&
                  + abs( DZVX(ii,jj,kk) - DXVZ(ii,jj,kk) )
          end do
       end do
       
       isnap = isnap + 1
       write( io, rec = isnap ) real(VPSNAP)
       isnap = isnap + 1
       write( io, rec = isnap ) real(VSSNAP)
       
    end if
    
  end subroutine ppohFDM_io_write_xz


  subroutine ppohFDM_io_write_yz( it, io )
  !
  ! Exports snapshots of a seismic wavefield of a user-specified vertical (y-z) plane at time step (it) to file number (io).
  !
    integer, intent(in) :: it
    integer, intent(in) :: io

    real(PN), save :: VPSNAP(NYS,NZS), VSSNAP(NYS,NZS)
    integer,  save :: isnap = 1
    integer        :: j, k, ii, jj, kk

    
    if( .not. is_ioyz ) return

    !! First call
    if( it == 1 ) then
       
       do k=1, NZS
          do j=1, NYS
             ii = i1
             jj = j * NYD
             kk = k * NZD
             VPSNAP(j,k) = RIG(ii,jj,kk)
             VSSNAP(j,k) = DEN(ii,jj,kk)
             
          end do
       end do
       
       write( io, rec=isnap ) VPSNAP
       isnap = isnap + 1
       write( io, rec=isnap ) VSSNAP
    end if
    
    
    !! Export binary
    if( mod( it, NWRITE ) == 0 ) then
       
       !! Prepare P- and S- wave amplitude
       do k = 1, NZS
          do j = 1, NYS
             ii = i1
             jj = j*NYD
             kk = k*NZD
             
             ! divergence( v )
             VPSNAP (j,k) = abs( DXVX(ii,jj,kk) + DYVY(ii,jj,kk) &
                  + DZVZ(ii,jj,kk) )
             ! abs(rotx)+abs(roty)+abs(rotz)
             VSSNAP (j,k) = abs( DXVY(ii,jj,kk) - DYVX(ii,jj,kk) )&
                  + abs( DYVZ(ii,jj,kk) - DZVY(ii,jj,kk) )&
                  + abs( DZVX(ii,jj,kk) - DXVZ(ii,jj,kk) )
          end do
       end do
       
       isnap = isnap + 1
       write( io, rec = isnap ) real(VPSNAP)
       isnap = isnap + 1
       write( io, rec = isnap ) real(VSSNAP)
       
    end if
    
  end subroutine ppohFDM_io_write_yz


  subroutine ppohFDM_output_prm
  !
  ! Outputs the parameter file used in the present simulation to the .prm file
  !
  !=Parameters
    integer, parameter :: io = 86

    open( io, file = './out/'//trim(title)//'.prm', action='write')
    write(io,*) '<TITLE>    ', trim(title)
    write(io,*) '<NX>       ', NX
    write(io,*) '<NY>       ', NY
    write(io,*) '<NZ>       ', NZ
    write(io,*) '<DX>       ', DX
    write(io,*) '<DY>       ', DY
    write(io,*) '<DZ>       ', DZ
    write(io,*) '<DT>       ', DT
    write(io,*) '<NTMAX>    ', NTMAX
    write(io,*) '<NWRITE>   ', NWRITE
    write(io,*) '<I0>       ', I0
    write(io,*) '<J0>       ', J0
    write(io,*) '<K0>       ', K0
    write(io,*) '<RMXX>     ', RMXX
    write(io,*) '<RMYY>     ', RMYY
    write(io,*) '<RMZZ>     ', RMZZ
    write(io,*) '<RMXY>     ', RMXY
    write(io,*) '<RMYZ>     ', RMYZ
    write(io,*) '<RMXZ>     ', RMXZ
    write(io,*) '<RMO>      ', RMO
    write(io,*) '<AT>       ', AT
    write(io,*) '<T0>       ', T0
    write(io,*) '<NST>      ', NST
    write(io,*) '<NXS>      ', NXS
    write(io,*) '<NYS>      ', NYS
    write(io,*) '<NZS>      ', NZS
    write(io,*) '<NXD>      ', NXD
    write(io,*) '<NYD>      ', NYD
    write(io,*) '<NZD>      ', NZD
    write(io,*) '<NPM>      ', NPM
    write(io,*) '<IWORD>    ', IWORD
    write(io,*) '<IWCL_W>   ', IWCL_W
    write(io,*) '<ONAME>    ', trim(ONAME)
    write(io,*) '<WNAME>    ', trim(WNAME)
    write(io,*) '<SNAME>    ', trim(SNAME)
    write(io,*) '<XYNAME>   ', trim(XYNAME)
    write(io,*) '<YZNAME>   ', trim(YZNAME)
    write(io,*) '<XZNAME>   ', trim(XZNAME)
    write(io,*) '<NP>       ', NP
    write(io,*) '<IP>       ', IP
    write(io,*) '<JP>       ', JP
    write(io,*) '<KP>       ', KP

    close( io )
    
  end subroutine ppohFDM_output_prm

  subroutine ppohFDM_io_open ()
      call ppohFDM_io_open_snp( IOSNP, SNAME,  NXS, NYS, is_fs   )
      call ppohFDM_io_open_snp( IOSPS, ONAME,  NXS, NYS, is_fs   )
      call ppohFDM_io_open_snp( IOXY,  XYNAME, NXS, NYS, is_ioxy )
      call ppohFDM_io_open_snp( IOYZ,  YZNAME, NYS, NZS, is_ioyz )
      call ppohFDM_io_open_snp( IOXZ,  XZNAME, NXS, NZS, is_ioxz )
  end subroutine ppohFDM_io_open

  subroutine ppohFDM_io_write ()
     call ppohFDM_io_write_sur( it, IOSNP )
     call ppohFDM_io_write_sps( it, IOSPS )
     call ppohFDM_io_write_xy ( it, IOXY  )
     call ppohFDM_io_write_xz ( it, IOXZ  )
     call ppohFDM_io_write_yz ( it, IOYZ  )
     call ppohFDM_io_write_wav( it, IOWAV )
  end subroutine ppohFDM_io_write

end module ppohFDM_io
