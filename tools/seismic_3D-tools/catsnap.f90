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
program catsnap
  use stdio
  implicit none

  integer, parameter :: IWORD = 4
  integer :: io_prm = 10
  integer :: io_bin = 900
  integer :: i,j,k
  character(99) :: fn_prm, fn_xy, fn_xz, fn_yz, title, fn_sur, fn_sps
  integer :: np
  integer :: NX, NZ, NY
  integer :: ierr
  integer :: isnap
  logical :: flag
  integer :: ip, jp, kp
  logical, allocatable :: is_file(:,:,:)

  
  !! Obtain grid size from parameter file
  if( iargc() /= 1 ) stop

  call getarg( 1, fn_prm )
  open( io_prm, file=fn_prm, action='read', status='old' )
  
  call readprm( io_prm, 'TITLE', title )
  call readprm( io_prm, 'IP', ip )
  call readprm( io_prm, 'JP', jp )
  call readprm( io_prm, 'KP', kp )
  call readprm( io_prm, 'NP', np )
  call readprm( io_prm, 'NXS',    NX  )
  call readprm( io_prm, 'NYS',    NY  )
  call readprm( io_prm, 'NZS',    NZ  )
  call readprm( io_prm, 'ONAME', fn_sps )
  call readprm( io_prm, 'XYNAME', fn_xy )
  call readprm( io_prm, 'XZNAME', fn_xz )
  call readprm( io_prm, 'YZNAME', fn_yz )
  call readprm( io_prm, 'SNAME', fn_sur )

  close( io_prm )
  
  allocate( is_file( IP, JP, KP ) )

  call cat_fs( fn_sur )
  call cat_fs( fn_sps )
  call cat_xy( fn_xy  )
  call cat_xz( fn_xz  )
  call cat_yz( fn_yz  )
  
  stop

contains

  subroutine cat_fs( fn_sur )
    character(*), intent(in) :: fn_sur
    real(PN) :: sur(2,NX,NY), sur1(2,NX*IP,NY*JP)
    logical :: is_fs(IP,JP,KP,NX,NY)
    integer :: iwcl
    character(99) :: fn_sur1
    character(11) :: cid
    integer :: nnp 
    integer :: ii, jj

    iwcl = NX*NY*4/IWORD
    !! Read & Set up logical table is_fs

    nnp = 0
    do k=1, kp
       do j=1, jp
          do i=1, ip
             nnp=nnp+1
             ! SUR filename
             write(cid,'(I3.3,A,I3.3,A,I3.3)') i-1, '.', j-1, '.', k-1
             fn_sur1 = trim(fn_sur) // '.' // cid
             
             ! Open the file if exists
             open( io_bin+nnp, file=fn_sur1, access='direct', &
                  recl=iwcl, status='old', iostat=ierr )
             
             ! Does file exist ?
             if( ierr /= 0 ) then
                is_file(i,j,k) = .false.
                cycle ! try next (i,j,k) pair
             end if
             is_file(i,j,k) = .true.

             write(STDERR,*) trim(fn_sur1), is_file(i,j,k)
             
             ! Read first two records
             read( io_bin+nnp, rec=1, iostat=ierr ) sur(1,:,:)
             read( io_bin+nnp, rec=1, iostat=ierr ) sur(2,:,:)
             
             ! Check if the grid is free surface ( or dummy )
             do jj=1, NY
                do ii=1, NX
                   if( sur(1,ii,jj) < -1 .and. sur(2,ii,jj) < -1 ) then ! dummy
                      is_fs(i,j,k,ii,jj) = .false.
                   else
                      is_fs(i,j,k,ii,jj) = .true.  ! ok, it is sufrace grid
                      sur1(:,(i-1)*NX+ii,(j-1)*NY+jj) = sur(:,ii,jj) 
                   end if
                end do
             end do
          end do
       end do
    end do
    
    !! Output file, export velocity structure
    open( io_bin, file=fn_sur, access='direct', recl=iwcl*IP*JP )
    write( io_bin, rec=1 ) sur1(1,:,:)
    write( io_bin, rec=2 ) sur1(2,:,:)
    isnap = 3
    flag = .true.
    
    !! Read & Concatnate time snapshots
    do
       write(STDERR,*) "snap = ", isnap/2
       call term_bkline(STDERR)
       if( .not. flag ) exit
       nnp = 0
       do k=1, kp
          do j=1, jp
             do i=1, ip
                nnp = nnp + 1
                if ( .not. is_file(i,j,k) ) cycle
                
                read( io_bin+nnp, rec=isnap,   iostat=ierr ) sur(1,:,:)
                read( io_bin+nnp, rec=isnap+1, iostat=ierr ) sur(2,:,:)
                
                if( ierr /= 0 ) then
                   flag = .false.
                   exit 
                end if
                
                do jj=1, NY
                   do ii=1, NX
                      if( is_fs(i,j,k, ii,jj ) ) then
                         sur1(:,(i-1)*NX+ii,(j-1)*NY+jj) = sur(:,ii,jj)
                      end if
                      
                   end do
                end do
             end do
          end do
       end do
       
       write( io_bin, rec=isnap  ) sur1(1,:,:)
       write( io_bin, rec=isnap+1) sur1(2,:,:)
       
       isnap = isnap + 2
    end do
    
    write(STDERR,*)
    !! Close all files
    do i=0, nnp
       close( io_bin + i ) 
    end do
    
  end subroutine cat_fs
  
  
  subroutine cat_xy( fn_xy )
    character(*), intent(in) :: fn_xy
    real(PN) :: sur(2,NX,NY), sur1(2,NX*IP,NY*JP)
    integer :: iwcl
    character(99) :: fn_xy1
    character(11) :: cid
    integer :: nnp 
    integer :: ii, jj

    iwcl = NX*NY*4/IWORD
    !! Read & Set up logical table is_fs

    nnp = 0
    do k=1, kp
       do j=1, jp
          do i=1, ip
             nnp=nnp+1
             ! SUR filename
             write(cid,'(I3.3,A,I3.3,A,I3.3)') i-1, '.', j-1, '.', k-1
             fn_xy1 = trim(fn_xy) // '.' // cid
             
             ! Open the file if exists
             open( io_bin+nnp, file=fn_xy1, access='direct', &
                  recl=iwcl, status='old', iostat=ierr )
             
             ! Does file exist ?
             if( ierr /= 0 ) then
                is_file(i,j,k) = .false.
                cycle ! try next (i,j,k) pair
             end if
             is_file(i,j,k) = .true.

             write(STDERR,*) trim(fn_xy1), is_file(i,j,k)
             
             ! Read first two records
             read( io_bin+nnp, rec=1, iostat=ierr ) sur(1,:,:)
             read( io_bin+nnp, rec=1, iostat=ierr ) sur(2,:,:)
             
             ! Check if the grid is free surface ( or dummy )
             do jj=1, NY
                do ii=1, NX
                   sur1(:,(i-1)*NX+ii,(j-1)*NY+jj) = sur(:,ii,jj) 
                end do
             end do
          end do
       end do
    end do
    
    
    !! Output file, export velocity structure
    open( io_bin, file=fn_xy, access='direct', recl=iwcl*IP*JP )
    write( io_bin, rec=1 ) sur1(1,:,:)
    write( io_bin, rec=2 ) sur1(2,:,:)
    isnap = 3
    flag = .true.
    
    !! Read & Concatnate time snapshots
    do
       write(STDERR,*) "snap = ", isnap/2
       call term_bkline(STDERR)
       if( .not. flag ) exit
       nnp = 0
       do k=1, kp
          do j=1, jp
             do i=1, ip
                nnp = nnp + 1
                if ( .not. is_file(i,j,k) ) cycle
                
                read( io_bin+nnp, rec=isnap,   iostat=ierr ) sur(1,:,:)
                read( io_bin+nnp, rec=isnap+1, iostat=ierr ) sur(2,:,:)
                
                if( ierr /= 0 ) then
                   flag = .false.
                   exit 
                end if
                
                do jj=1, NY
                   do ii=1, NX
                      sur1(:,(i-1)*NX+ii,(j-1)*NY+jj) = sur(:,ii,jj)
                   end do
                end do
             end do
          end do
       end do
       
       write( io_bin, rec=isnap  ) sur1(1,:,:)
       write( io_bin, rec=isnap+1) sur1(2,:,:)
       
       isnap = isnap + 2
    end do
    
    write(STDERR,*)
    !! Close all files
    do i=0, nnp
       close( io_bin + i ) 
    end do
    
  end subroutine cat_xy


  subroutine cat_xz( fn_xz )
    character(*), intent(in) :: fn_xz
    real(PN) :: xz(2,NX,NZ), xz1(2,NX*IP,NZ*KP)
    integer :: iwcl
    character(99) :: fn_xz1
    character(11) :: cid
    integer :: nnp 
    integer :: ii, kk
  
    iwcl = NX*NZ*4/IWORD
    !! Read & Set up logical table is_fs

    nnp = 0
    do k=1, kp
       do j=1, jp
          do i=1, ip
             nnp=nnp+1
             ! XZ filename
             write(cid,'(I3.3,A,I3.3,A,I3.3)') i-1, '.', j-1, '.', k-1
             fn_xz1 = trim(fn_xz) // '.' // cid
             
             ! Open the file if exists
             open( io_bin+nnp, file=fn_xz1, access='direct', &
                  recl=iwcl, status='old', iostat=ierr )
             
             ! Does file exist ?
             if( ierr /= 0 ) then
                is_file(i,j,k) = .false.
                cycle ! try next (i,j,k) pair
             end if
             is_file(i,j,k) = .true.

             write(STDERR,*) trim(fn_xz1), is_file(i,j,k)
             
             ! Read first two records
             read( io_bin+nnp, rec=1, iostat=ierr ) xz(1,:,:)
             read( io_bin+nnp, rec=1, iostat=ierr ) xz(2,:,:)
             
             ! Check if the grid is free surface ( or dummy )
             do kk=1, NZ
                do ii=1, NX
                   xz1(:,(i-1)*NX+ii,(k-1)*NZ+kk) = xz(:,ii,kk)
                end do
             end do
          end do
       end do
    end do
    
    
    !! Output file, export velocity structure
    open( io_bin, file=fn_xz, access='direct', recl=iwcl*IP*KP )
    write( io_bin, rec=1 ) xz1(1,:,:)
    write( io_bin, rec=2 ) xz1(2,:,:)
    isnap = 3
    flag = .true.
    
    !! Read & Concatnate time snapshots
    do
       write(STDERR,*) "snap = ", isnap/2
       call term_bkline(STDERR)
       if( .not. flag ) exit
       nnp = 0
       do k=1, kp
          do j=1, jp
             do i=1, ip
                nnp = nnp + 1
                if ( .not. is_file(i,j,k) ) cycle
                
                read( io_bin+nnp, rec=isnap,   iostat=ierr ) xz(1,:,:)
                read( io_bin+nnp, rec=isnap+1, iostat=ierr ) xz(2,:,:)
                
                if( ierr /= 0 ) then
                   flag = .false.
                   exit 
                end if
                
                do kk=1, NZ
                   do ii=1, NX
                      xz1(:,(i-1)*NX+ii,(k-1)*NZ+kk) = xz(:,ii,kk)
                   end do
                end do
             end do
          end do
       end do
       
       write( io_bin, rec=isnap  ) xz1(1,:,:)
       write( io_bin, rec=isnap+1) xz1(2,:,:)
       
       isnap = isnap + 2
    end do
    
    write(STDERR,*)
    !! Close all files
    do i=0, np
       close( io_bin + i ) 
    end do
    
  end subroutine cat_xz


  subroutine cat_yz( fn_yz )
    character(*), intent(in) :: fn_yz
    real(PN) :: yz(2,NY,NZ), yz1(2,NY*JP,NZ*KP)
    integer :: iwcl
    character(99) :: fn_yz1
    character(11) :: cid
    integer :: nnp 
    integer :: jj, kk
  
    iwcl = NY*NZ*4/IWORD
    !! Read & Set up logical table is_fs

    nnp = 0
    do k=1, kp
       do j=1, jp
          do i=1, ip
             nnp=nnp+1
             ! YZ filename
             write(cid,'(I3.3,A,I3.3,A,I3.3)') i-1, '.', j-1, '.', k-1
             fn_yz1 = trim(fn_yz) // '.' // cid
             
             ! Open the file if exists
             open( io_bin+nnp, file=fn_yz1, access='direct', &
                  recl=iwcl, status='old', iostat=ierr )
             
             ! Does file exist ?
             if( ierr /= 0 ) then
                is_file(i,j,k) = .false.
                cycle ! try next (i,j,k) pair
             end if
             is_file(i,j,k) = .true.

             write(STDERR,*) trim(fn_yz1), is_file(i,j,k)
             
             ! Read first two records
             read( io_bin+nnp, rec=1, iostat=ierr ) yz(1,:,:)
             read( io_bin+nnp, rec=1, iostat=ierr ) yz(2,:,:)
             
             ! Check if the grid is free surface ( or dummy )
             do kk=1, NZ
                do jj=1, NY
                   yz1(:,(j-1)*NY+jj,(k-1)*NZ+kk) = yz(:,jj,kk)
                end do
             end do
          end do
       end do
    end do
    
    
    !! Output file, export velocity structure
    open( io_bin, file=fn_yz, access='direct', recl=iwcl*JP*KP )
    write( io_bin, rec=1 ) yz1(1,:,:)
    write( io_bin, rec=2 ) yz1(2,:,:)
    isnap = 3
    flag = .true.
    
    !! Read & Concatnate time snapshots
    do
       write(STDERR,*) "snap = ", isnap/2
       call term_bkline(STDERR)
       if( .not. flag ) exit
       nnp = 0
       do k=1, kp
          do j=1, jp
             do i=1, ip
                nnp = nnp + 1
                if ( .not. is_file(i,j,k) ) cycle
                
                read( io_bin+nnp, rec=isnap,   iostat=ierr ) yz(1,:,:)
                read( io_bin+nnp, rec=isnap+1, iostat=ierr ) yz(2,:,:)
                
                if( ierr /= 0 ) then
                   flag = .false.
                   exit 
                end if
                
                do kk=1, NZ
                   do jj=1, NY
                      yz1(:,(j-1)*NY+jj,(k-1)*NZ+kk) = yz(:,jj,kk)
                   end do
                end do
             end do
          end do
       end do
       
       write( io_bin, rec=isnap  ) yz1(1,:,:)
       write( io_bin, rec=isnap+1) yz1(2,:,:)
       
       isnap = isnap + 2
    end do
    
    write(STDERR,*)
    !! Close all files
    do i=0, nnp
       close( io_bin + i ) 
    end do
    
  end subroutine cat_yz

end program catsnap
