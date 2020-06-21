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

module ppohDEM_util

    implicit none
    public
    include 'mpif.h'
    include 'ppohDEM_precision.inc'
    
    
    integer(kind=kint), parameter :: MAX_MESH=220000
    integer(kind=kint), parameter :: MAX_FACET_CROSSLIST=200
      
    integer(kind=kint), parameter :: MAX_TEMPLATE=256


!----------------------------------------------------------------
!  parameter for distancecalculate() 
!----------------------------------------------------------------
    integer(kind=kint), parameter :: HPARAMS_ITERATE = 200

!----------------------------------------------------------------



!----------------------------------------------------------------
!    Variables for MPI
!----------------------------------------------------------------

    integer(kind=kint) :: nprocs, myrank, ierr

    integer(kind=kint) :: quotient, remainder
    integer(kind=kint) :: i_x_start, i_x_end

    integer(kind=kint) :: loop_x_start, loop_x_end






    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! fundamental struct
    
    type ppohDEM_vec3
        real(kind=kreal) :: x,y,z
    end type
    
    type ppohDEM_rvec3
        real(kind=kreal) :: x,y,z
    end type
    
    type ppohDEM_minmax
        real(kind=kreal) :: min
        real(kind=kreal) :: max
    end type
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! file information
    
    type ppohDEM_fileinfo
        integer(kind=kint) :: func
        character(len=ppohDEM_name_length):: input_filename
        character(len=ppohDEM_name_length):: input_filename2
        character(len=ppohDEM_name_length):: output_filename
        character(len=ppohDEM_name_length):: template_filename
        integer(kind=kint) :: gridX,gridY,gridZ
        type(ppohDEM_vec3) ::object_L, object_size, object_origin
        

    end type ppohDEM_fileinfo
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! voxelizer 
      
    
    
    type ppohDEM_single_mesh
        type(ppohDEM_vec3) :: v1,v2,v3
        type(ppohDEM_minmax) :: x,y,z 
    end type
    
    type ppohDEM_mesh
        integer :: n
        type(ppohDEM_single_mesh),pointer :: mesh(:)
!----------------------------------------------------------------
!  variables for distancevoxel() 
!----------------------------------------------------------------
!        real(kind=kreal),pointer :: distance_mesh(:)
        
        real(kind=kreal) :: Xmin, Xmax
        real(kind=kreal) :: Ymin, Ymax
        real(kind=kreal) :: Zmin, Zmax

!----------------------------------------------------------------

    end type
    
    type ppohDEM_gridco
        real(kind=kreal),pointer :: x(:),y(:),z(:)
    end type
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! converter 
    
    type ppohDEM_voxel
        integer(kind=kint) :: gridX,gridY,gridZ

!        integer(kind=kint),pointer :: value(:)
        integer(kind=kint),pointer :: value_3D(:,:,:)


!----------------------------------------------------------------
!  variables for distancecalculate() 
!----------------------------------------------------------------
!        real(kind=kreal),allocatable :: distance(:)
!        real(kind=kreal),pointer :: distance(:)
        real(kind=kreal),pointer :: distance_3D(:,:,:)

!        type(ppohDEM_vec3),pointer :: v_distance(:)

!        integer(kind=kint),pointer :: calculated(:)
        integer(kind=kint),pointer :: calculated_3D(:,:,:)

        



        real(kind=kreal) :: Lx, Ly, Lz

        real(kind=kreal) :: W
        real(kind=kreal) :: W2

        integer(kind=kint) :: cellX, cellY, cellZ

        real(kind=kreal) :: sizeX, sizeY, sizeZ
        real(kind=kreal) :: posX, posY, posZ


!----------------------------------------------------------------


    end type
      
    type ppohDEM_particle_template
        integer(kind=kint) :: n
        type(ppohDEM_vec3),pointer :: p(:)
    end type
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
end module ppohDEM_util





