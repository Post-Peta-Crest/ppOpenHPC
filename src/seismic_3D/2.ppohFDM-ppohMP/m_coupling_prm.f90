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
module ppohFDM_coupling_prm
!=Declarations
  use ppohFDM_stdio
  use ppohFDM_param
  use mpi
  implicit none
  public
contains

!   subroutine ppohFDM_output_index
!   !
!   !=Description
!   ! Output index file
!   !
!   !=Parameters
!     integer, parameter :: io = 86
!   !+
!   !--
    
!     SX0=int(ERX0/DX+0.5)+1
!     EX1=int(ERX1/DX+0.5)+1
!     SY0=int(ERY0/DY+0.5)+1
!     EY1=int(ERY1/DY+0.5)+1
!     SZ0=KFS
!     EZ1=int(ERZ1/DZ+0.5)+KFS

!     if(myid==0) then
       
!        open( io, file = './out/'//trim(title)//'.index', action='write')
       
!        write(io,*) '<SX0>       ', SX0
!        write(io,*) '<EX1>       ', EX1
!        write(io,*) '<SY0>       ', SY0
!        write(io,*) '<EY1>       ', EY1
!        write(io,*) '<SZ0>       ', KFS
!        write(io,*) '<EZ1>       ', EZ1
       
!        close( io )

!     endif
!   end subroutine ppohFDM_output_index

  subroutine ppohFDM_output_rank
    coord_xx = (NXP*idx)+NXP
    coord_y  = 1+(NYP*idy)
    coord_yy = (NYP*idy)+NYP
    coord_z  = 1+(NZP*idz)
    coord_zz = (NZP*idz)+NZP
  end subroutine ppohFDM_output_rank

  subroutine ppohFDM_output_cod
    integer :: i, j, k
    integer :: coord_x, coord_y, coord_z
    integer :: coord_xx, coord_yy, coord_zz

    write(*,*) "coord_output"
    open(11,file="./out/SEISM3D3.cod",status='replace')
    
    do k= 0, KP-1
       do j= 0, JP-1
         do i=0, IP-1
             coord_x  = 1+(NXP*idx)
             coord_xx = (NXP*idx)+NXP
             coord_y  = 1+(NYP*idy)
             coord_yy = (NYP*idy)+NYP
             coord_z  = 1+(NZP*idz)
             coord_zz = (NZP*idz)+NZP
             write(11,'(I5" "I5" "I5)') coord_x,coord_y,coord_z
             write(11,'(I5" "I5" "I5)') coord_xx,coord_yy,coord_zz
          end do
       end do
    end do
    close(11)
  end subroutine ppohFDM_output_cod
end module ppohFDM_coupling_prm
