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


subroutine output_data (file_info,voxel)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_voxel) :: voxel
    
!----------------------------------------------------------------
   
!    character(len=ppohDEM_name_length):: buf_x, buf_y, buf_z
!    character(len=ppohDEM_name_length):: buf_distance

    INTEGER :: i, j, k

    character(len=ppohDEM_name_length) :: t_char, ex_char, filename

    integer(kind=kint) :: c_len










!----------------------------------------------------------------

    WRITE(t_char, *) myrank  
    ex_char = "000" // trim(ADJUSTL(t_char))
    c_len = LEN_TRIM(ex_char)
    t_char = ex_char(c_len - 3:c_len)

    filename = trim(trim(file_info%output_filename) // "distance_" // trim(t_char) // ".dat" )

    open (11, file = trim(filename), status='replace')
    
    if(myrank .eq. 0) then  

    write(11,'(4i8)') voxel%gridX, voxel%gridY, voxel%gridZ, nprocs 
  
    endif

    
      do k = 1, voxel%gridZ
         do j = 1, voxel%gridY
            do i = i_x_start, i_x_end
               
             write(11,'(3I8, 1E20.8)') i, j, k, voxel%distance_3D(i,j,k)

          enddo
       enddo
    enddo

    close(11)

!----------------------------------------------------------------
















end subroutine





