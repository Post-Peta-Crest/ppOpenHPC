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


subroutine ppohDEM_v_exit (meshXYZ,voxel,gridco)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_mesh) :: meshXYZ
    type(ppohDEM_voxel) :: voxel
    type(ppohDEM_gridco) :: gridco

    deallocate(meshXYZ%mesh)
!    deallocate(meshXYZ%distance_mesh)

!    deallocate(voxel%value)
    deallocate(voxel%value_3D)

!----------------------------------------------------------------
!  deallocate variables for distance_calculate() 
!----------------------------------------------------------------
!    write (*,*) 'deallocate voxel%distance'
!    deallocate(voxel%distance)
    deallocate(voxel%distance_3D)

!    deallocate(voxel%calculated)
    deallocate(voxel%calculated_3D)

!----------------------------------------------------------------
    deallocate(gridco%x)
    deallocate(gridco%y)
    deallocate(gridco%z)

!----------------------------------------------------------------

!----------------------------------------------------------------
    

end subroutine




