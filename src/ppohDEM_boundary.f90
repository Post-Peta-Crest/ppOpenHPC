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

subroutine ppohDEM_boundary(parameters,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    call ppohDEM_box_boundary(parameters,particles)
     
    
end subroutine
    
    
subroutine ppohDEM_periodic_boundary(parameters,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    real(kind=kreal) :: lx,ly,lz
    
    
    do i=1,particles%n
        
        if (particles%pos(i)%x>=parameters%L%x) particles%pos(i)%x=particles%pos(i)%x-parameters%L%x
        if (particles%pos(i)%x<0) particles%pos(i)%x=particles%pos(i)%x+parameters%L%x
        if (particles%pos(i)%y>=parameters%L%y) particles%pos(i)%y=particles%pos(i)%y-parameters%L%y
        if (particles%pos(i)%y<0) particles%pos(i)%y=particles%pos(i)%y+parameters%L%y
        if (particles%pos(i)%z>=parameters%L%z) particles%pos(i)%z=particles%pos(i)%z-parameters%L%z
        if (particles%pos(i)%z<0) particles%pos(i)%z=particles%pos(i)%z+parameters%L%z
        
    enddo
     
    
end subroutine
    
    
subroutine ppohDEM_box_boundary(parameters,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    real(kind=kreal) :: lx,ly,lz
    
    
    do i=1,particles%n
        
        if(particles%pos(i)%x>=parameters%L%x-parameters%Radius)then
            particles%pos(i)%x=parameters%L%x-parameters%Radius
            particles%vel(i)%x=-parameters%Wall_decay*particles%vel(i)%x
            particles%vel(i)%y=parameters%Wall_decay*particles%vel(i)%y
            particles%vel(i)%z=parameters%Wall_decay*particles%vel(i)%z
        endif
        if(particles%pos(i)%x<parameters%Radius)then
            particles%pos(i)%x=parameters%Radius
            particles%vel(i)%x=-parameters%Wall_decay*particles%vel(i)%x
            particles%vel(i)%y=parameters%Wall_decay*particles%vel(i)%y
            particles%vel(i)%z=parameters%Wall_decay*particles%vel(i)%z
        endif
        if(particles%pos(i)%y>=parameters%L%y-parameters%Radius)then
            particles%pos(i)%y=parameters%L%y-parameters%Radius
            particles%vel(i)%x=parameters%Wall_decay*particles%vel(i)%x
            particles%vel(i)%y=-parameters%Wall_decay*particles%vel(i)%y
            particles%vel(i)%z=parameters%Wall_decay*particles%vel(i)%z
        endif
        if(particles%pos(i)%y<parameters%Radius)then
            particles%pos(i)%y=parameters%Radius
            particles%vel(i)%x=parameters%Wall_decay*particles%vel(i)%x
            particles%vel(i)%y=-parameters%Wall_decay*particles%vel(i)%y
            particles%vel(i)%z=parameters%Wall_decay*particles%vel(i)%z
        endif
        if(particles%pos(i)%z>=parameters%L%z-parameters%Radius)then
            particles%pos(i)%z=parameters%L%z-parameters%Radius
            particles%vel(i)%x=parameters%Wall_decay*particles%vel(i)%x
            particles%vel(i)%y=parameters%Wall_decay*particles%vel(i)%y
            particles%vel(i)%z=-parameters%Wall_decay*particles%vel(i)%z
        endif
        if(particles%pos(i)%z<parameters%Radius)then
            particles%pos(i)%z=parameters%Radius
            particles%vel(i)%x=parameters%Wall_decay*particles%vel(i)%x
            particles%vel(i)%y=parameters%Wall_decay*particles%vel(i)%y
            particles%vel(i)%z=-parameters%Wall_decay*particles%vel(i)%z
        endif
        
    enddo
     
    
end subroutine
    
    