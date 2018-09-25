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
module m_ppm
  implicit none
  private

  !=Public Procedure
  public :: ppm__write_col
  public :: ppm__read_col

contains
  
  subroutine ppm__write_col( fname, width, height, img )    
    character(*), intent(in) :: fname
    integer,      intent(in) :: width
    integer,      intent(in) :: height
    integer,      intent(in) :: img( 3, width, height) !RGB 0-255

    character :: aimg( 3, width, height)
    integer :: io_ppm = 100
    integer :: i,j ,k
    
    do i=1, height
       do j=1, width
          do k=1, 3
             aimg(k,j,i)  = transfer( max(min(img(k,j,i),255),0), 'a')
          end do
       end do
    end do
    

    open( io_ppm, file = fname  )
    write( io_ppm,'(A2,2I8,A4)') 'P6', width, height, ' 255'
    close( io_ppm )

    open( io_ppm, file = fname, access='stream', position='append' )
    write( io_ppm ) aimg
    close( io_ppm )
    
  end subroutine ppm__write_col


  subroutine ppm__read_col( fname,  width, height, image )
    implicit none
    
    character(*), intent(in)  :: fname
    integer,      intent(in)  :: width
    integer,      intent(in)  :: height
    integer,      intent(out) :: image(3,width,height)
 
    character, parameter  :: lf = char(10) ! 
    integer       :: io_ppm = 700
    character(80) :: buf
    character     :: aimage(3,width,height)
    integer       :: i, j, k


    open( io_ppm, file=fname, access='stream' )
    read(io_ppm) buf
    rewind(io_ppm)

    read(io_ppm) buf( 1:index(buf,lf) ) 
    read(io_ppm) aimage
    close(io_ppm)
    
    do k=1, height
       do j=1, width
          do i=1, 3
             image(i,j,k) = transfer( aimage(i,j,k), width)
          end do
       end do
    end do
    
  end subroutine ppm__read_col

end module m_ppm
