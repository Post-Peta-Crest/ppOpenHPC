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

subroutine ppohDEM_locate_particles_3dbox(parameters,particles,lx,ly,lz,x0,y0,z0,interval_ratio)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    real(kind=kreal) :: lx,ly,lz
    real(kind=kreal) :: x0,y0,z0
    real(kind=kreal) :: interval_ratio
    
    integer(kind=kint) :: nx,ny,nz
    real(kind=kreal) :: interval
    real(kind=kreal),allocatable :: rn(:)
    
    interval=2*parameters%Radius*interval_ratio
    nx=floor(lx/interval)
    ny=floor(ly/interval)
    nz=floor(lz/interval)
    
    if(nx*ny*nz<parameters%Pnum) parameters%Pnum=nx*ny*nz
    
    allocate(rn(particles%n*3))
    call random_number(rn)
    do k=1,nz
        do j=1,ny
            do i=1,nx
                index=i+nx*(j-1)+nx*ny*(k-1)
                if(index>parameters%Pnum) exit
                particles%pos(index)%x=x0+interval*(i-1)+parameters%Radius*interval_ratio+0.01*(rn(3*index-2)-0.5)
                particles%pos(index)%y=y0+interval*(j-1)+parameters%Radius*interval_ratio+0.01*(rn(3*index-1)-0.5)
                particles%pos(index)%z=z0+interval*(k-1)+parameters%Radius*interval_ratio+0.01*(rn(3*index)-0.5)
            enddo
        enddo
    enddo
    deallocate(rn)
    
    
end subroutine
    
subroutine ppohDEM_locate_particles_2dbox(parameters,particles,lx,lz,x0,z0,y0,interval_ratio)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    integer :: i,k
    integer :: index


    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    real(kind=kreal) :: lx,lz
    real(kind=kreal) :: x0,z0
    real(kind=kreal) :: y0
    real(kind=kreal) :: interval_ratio
    
    integer(kind=kint) :: nx,ny,nz
    real(kind=kreal) :: interval
    real(kind=kreal),allocatable :: rn(:)
    
    interval=2*parameters%Radius*interval_ratio
    
    nx=floor(lx/interval) 
    nz=floor(lz/interval)
    
    if(nx*nz<parameters%Pnum) parameters%Pnum=nx*nz
    
    allocate(rn(particles%n*3))
    call random_number(rn)
    do k=1,nz
        do i=1,nx
            index=i+nx*(k-1)
            if(index>parameters%Pnum) exit
            particles%pos(index)%x=x0+interval*(i-1)+parameters%Radius*interval_ratio+0.01*(rn(3*index-2)-0.5)
            particles%pos(index)%y=y0
            particles%pos(index)%z=z0+interval*(k-1)+parameters%Radius*interval_ratio+0.01*(rn(3*index)-0.5)
        enddo
    enddo
    deallocate(rn)
    
    
end subroutine
