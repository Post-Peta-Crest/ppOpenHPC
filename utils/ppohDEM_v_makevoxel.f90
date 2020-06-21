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


!subroutine ppohDEM_v_makevoxel (file_info,meshXYZ,voxel)
subroutine ppohDEM_v_makevoxel (file_info,meshXYZ,voxel,gridco)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel
    integer(kind=kint),dimension(:),allocatable :: facet_crosslist
    integer(kind=kint) n_fcl
    type(ppohDEM_gridco) :: gridco

    integer(kind=kint) :: loopX, loopY
    
!    integer(kind=kint) :: i,j,k
!    integer(kind=kint) :: loopX,loopY,loopZ

    
!----------------------------------------------------------------
!    allocate(gridco%x(voxel%gridX))
!    allocate(gridco%y(voxel%gridY))
!    allocate(gridco%z(voxel%gridZ))
!----------------------------------------------------------------
    
    call ppohDEM_v_inivar(file_info,meshXYZ,voxel,gridco)
    

    

    allocate(facet_crosslist(MAX_FACET_CROSSLIST))

    do loopY=1,voxel%gridY
        do loopX = i_x_start, i_x_end
            
            facet_crosslist(1:MAX_FACET_CROSSLIST) = 0
            
            call ppohDEM_v_makecrosslist_MPI(loopX,loopY,facet_crosslist,n_fcl,meshXYZ,gridco)
            call ppohDEM_v_makegridout_MPI(loopX,loopY,facet_crosslist,n_fcl,meshXYZ,voxel,gridco)

        enddo
    enddo








            deallocate(facet_crosslist)
            






    
!----------------------------------------------------------------
!    deallocate(gridco%x)
!    deallocate(gridco%y)
!    deallocate(gridco%z)
!----------------------------------------------------------------
    

end subroutine
