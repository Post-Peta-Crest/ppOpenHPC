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
    !include 'mpif.h'
    include 'ppohDEM_precision.inc'
    

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! fundamental struct
    
    
    type ppohDEM_ivec3
        integer(kind=kint) :: x,y,z
    end type
    
    type ppohDEM_rvec3
        real(kind=kreal) :: x,y,z
    end type
    

    type ppohDEM_rvec4
        real(kind=kreal) :: x,y,z,w
    end type
    

    type ppohDEM_ivec2
        integer(kind=kint) :: x,y
    end type
    
    type ppohDEM_rvec2
        real(kind=kreal) :: x,y
    end type
    
    type ppohDEM_rvec6
        real(kind=kreal) :: x,y,z
        real(kind=kreal) :: vx,vy,vz
    end type
    
    
    type ppohDEM_pvec2
        real(kind=kreal) :: theta,phi
    end type
    
    type ppohDEM_rtens3
        real(kind=kreal) :: xx,xy,xz
        real(kind=kreal) :: yx,yy,yz
        real(kind=kreal) :: zx,zy,zz
    end type
    
    type ppohDEM_clist
        integer(kind=kint) :: n
        integer(kind=kint), pointer :: pid(:)
        type(ppohDEM_rvec3), pointer :: tforce(:)
        type(ppohDEM_rvec3), pointer :: dn(:)
    end type
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! physical variables
    
    type ppohDEM_particles
        integer(kind=kint) :: n
        real(kind=kreal), pointer :: radius(:)
        type(ppohDEM_rvec3), pointer :: pos(:)
        type(ppohDEM_rvec3), pointer :: vel(:)
        type(ppohDEM_rvec3), pointer :: force(:)
        type(ppohDEM_rvec3), pointer :: omega(:)
        type(ppohDEM_rvec3), pointer :: rforce(:)
        integer(kind=kint), pointer :: cellindex(:)
        integer(kind=kint), pointer :: newtoorig(:)
        type(ppohDEM_clist), pointer :: contact_list(:)
        type(ppohDEM_clist), pointer :: contact_list_wall(:)
    end type
    
    type ppohDEM_cells
        integer(kind=kint) :: n
        type(ppohDEM_rvec3) :: cellsize
        type(ppohDEM_ivec3) :: cellnum
        integer(kind=kint), pointer :: nei_cellindex(:)
        integer(kind=kint), pointer :: start_partindex(:)
        integer(kind=kint), pointer :: end_partindex(:)
    end type
    
    type ppohDEM_walls
        integer(kind=kint) :: n
        type(ppohDEM_rvec3), pointer :: point(:)
        type(ppohDEM_rvec3), pointer :: nvec(:)
    end type
    
    type ppohDEM_objects
        integer(kind=kint) :: gn
        type(ppohDEM_ivec3) :: gridnum
        type(ppohDEM_rvec3) :: gridsize
        real(kind=kreal), pointer :: distance(:)
        real(kind=kreal), pointer :: scaled_distance(:)


        type(ppohDEM_rvec3) :: vel
        type(ppohDEM_rvec4) :: omega
        type(ppohDEM_rvec3) :: centroid

        type(ppohDEM_ivec3) :: winstart
        type(ppohDEM_ivec3) :: winend

        type(ppohDEM_rvec3) :: winstart_r
        type(ppohDEM_rvec3) :: winend_r

        type(ppohDEM_rvec3) :: winmin_r
        type(ppohDEM_rvec3) :: winmax_r

        real(kind=kreal), pointer :: read_distance_3D(:,:,:)

        type(ppohDEM_rvec3), pointer :: winpos_3D(:,:,:) ! window position

        type(ppohDEM_rvec3), pointer :: move_3D(:,:,:) 

        type(ppohDEM_rvec3), pointer :: mesh_v1(:) ! mesh
        type(ppohDEM_rvec3), pointer :: mesh_v2(:) ! mesh
        type(ppohDEM_rvec3), pointer :: mesh_v3(:) ! mesh
        
        integer(kind=kint) :: n_mesh

    end type
    
    type ppohDEM_counter
        integer(kind=kint) :: itime
        real(kind=kreal) :: rtime
        integer(kind=kint) :: file_index
        real(kind=kreal) :: pos_accum
    end type
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! file information
    
    type ppohDEM_fileinfo
        integer(kind=kint) :: func
        character(len=ppohDEM_name_length):: input_filename
        character(len=ppohDEM_name_length):: output_filename

        character(len=ppohDEM_name_length):: input_params_filename

    end type ppohDEM_fileinfo
    
    
    type ppohDEM_comminfo
        integer(kind=kint) myrank
        integer(kind=kint) nprocs
    end type ppohDEM_comminfo
    
    
    
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! parameters
    
    type ppohDEM_parameters
        real(kind=kreal) :: Tstep,Tfin
        integer(kind=kint) :: Dstep,Dfin
        integer(kind=kint) :: Pnum  !!!!  Global parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        type(ppohDEM_rvec3) :: L 
        type(ppohDEM_ivec3) :: Cn 
        real(kind=kreal) :: Pi
        real(kind=kreal) :: Radius
        real(kind=kreal) :: MaxRadius
        real(kind=kreal) :: Mass
        real(kind=kreal) :: Im
        real(kind=kreal) :: Kn,Kt,En,Et
        real(kind=kreal) :: Fric_const
        real(kind=kreal) :: Gravity
        integer(kind=kint) :: Size_contact_list 
        integer(kind=kint) :: Size_contact_list_wall
        real(kind=kreal) :: Wall_decay 
        type(ppohDEM_ivec3) :: Obj_n  !!!! object !!!!!!!!!!!!!!!!!!
        real(kind=kreal) :: Obj_Kn,Obj_En

        type(ppohDEM_ivec3) :: winstart
        
        type(ppohDEM_rvec3) :: vel !vel
        type(ppohDEM_rvec4) :: omega !omega
        type(ppohDEM_rvec3) :: centroid !centroid


    end type
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
end module ppohDEM_util





