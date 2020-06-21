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


!subroutine ppohDEM_v_pre (file_info,meshXYZ,voxel)
subroutine ppohDEM_v_pre (file_info,meshXYZ,voxel,gridco)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel
!    integer(kind=kint) :: n_mesh  
    
    type(ppohDEM_gridco) :: gridco

    integer(kind=kint) :: nx
    





!      nx = 128
    nx = file_info%gridX

!------------------------------------------------------------------------------------------------------------------------
!     The computational range of the plane for the x direction
!-----------------------------------------------------------------------------------------------------------------------

      quotient = nx / nprocs
      remainder = MOD(nx, nprocs)


      if (myrank .lt. remainder) then 

        i_x_start = myrank * quotient + 1 + myrank
        i_x_end = i_x_start + quotient 

      else

        i_x_start = myrank * quotient + 1 + remainder
        i_x_end = i_x_start + quotient -1

      end if 


      print *, 'myrank = ', myrank, 'i_x_start = ', i_x_start, 'i_x_end = ', i_x_end 








!------------------------------------------------------------------------------------------------------------------------
!   end The computational range of the plane for the z direction
!-----------------------------------------------------------------------------------------------------------------------







    
    !Dat reader
    !call ppohDEM_v_datreader(file_info,meshXYZ,voxel)
    
    !STL reader
    if(file_info%func==1)then
!        call ppohDEM_v_stlreader(file_info,meshXYZ,voxel)
        call ppohDEM_v_stlreader(file_info,meshXYZ,voxel,gridco)
    elseif(file_info%func==3)then
!        call ppohDEM_v_patchreader(file_info,meshXYZ,voxel)
    endif
    
    close(11)
    
end subroutine




