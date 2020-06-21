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

subroutine ppohDEM_integrate_system(parameters,particles,cells,walls,objects)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_walls) :: walls
    type(ppohDEM_objects) :: objects
    
    
    call ppohDEM_calcforce(parameters,particles,cells,walls,objects)
    call ppohDEM_embedding(parameters,particles,cells,objects)
    
    call ppohDEM_Symplectic(parameters,particles,cells)
    !call ppohDEM_Verlet(parameters,particles,cells)
     
    
end subroutine
    
subroutine ppohDEM_Symplectic(parameters,particles,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    
    
    call ppohDEM_Symplectic1(parameters,particles)
    
end subroutine
subroutine ppohDEM_Symplectic1(parameters,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    t_step=parameters%Tstep
    t_step1=t_step/parameters%Mass
    
    
    
    !update velocity 
    do i=1,particles%n
        particles%vel(i)%x=particles%vel(i)%x+t_step1*(particles%force(i)%x)
        particles%vel(i)%y=particles%vel(i)%y+t_step1*particles%force(i)%y
        particles%vel(i)%z=particles%vel(i)%z+t_step1*(particles%force(i)%z-parameters%Gravity)
    enddo
    
    !update position
    do i=1,particles%n
        particles%pos(i)%x=particles%pos(i)%x+t_step*particles%vel(i)%x
        particles%pos(i)%y=particles%pos(i)%y+t_step*particles%vel(i)%y
        particles%pos(i)%z=particles%pos(i)%z+t_step*particles%vel(i)%z
    enddo
    
    !update rotational velocity 
    do i=1,particles%n
        t_step2=t_step/parameters%Im*particles%radius(i)
        particles%omega(i)%x=particles%omega(i)%x+t_step2*particles%rforce(i)%x
        particles%omega(i)%y=particles%omega(i)%y+t_step2*particles%rforce(i)%y
        particles%omega(i)%z=particles%omega(i)%z+t_step2*particles%rforce(i)%z
    enddo
    
    
end subroutine
     
    
subroutine ppohDEM_ppohDEM_Verlet(parameters,particles,cells,walls,objects)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_walls) :: walls
    type(ppohDEM_objects) :: objects
    
    call ppohDEM_calcforce(parameters,particles,cells,walls,objects)
    call ppohDEM_Verlet1(parameters,particles)
    call ppohDEM_calcforce(parameters,particles,cells,walls,objects)
    call ppohDEM_Verlet2(parameters,particles)
    
    
end subroutine
subroutine ppohDEM_Verlet1(parameters,particles) !Velocity Verlet

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    t_step=parameters%Tstep
    t_step1=t_step/parameters%Mass
    t_step2=t_step*t_step/parameters%Mass
    
    !update position
    do i=1,particles%n
        particles%pos(i)%x=particles%pos(i)%x+t_step*particles%vel(i)%x+0.5*t_step2*particles%force(i)%x
        particles%pos(i)%y=particles%pos(i)%y+t_step*particles%vel(i)%y+0.5*t_step2*particles%force(i)%y
        particles%pos(i)%z=particles%pos(i)%z+t_step*particles%vel(i)%z+0.5*t_step2*particles%force(i)%z
    enddo
    
    !update velocity half
    do i=1,particles%n
        particles%vel(i)%x=particles%vel(i)%x+0.5*t_step1*particles%force(i)%x
        particles%vel(i)%y=particles%vel(i)%y+0.5*t_step1*particles%force(i)%y
        particles%vel(i)%z=particles%vel(i)%z+0.5*t_step1*particles%force(i)%z
    enddo
    
    
end subroutine      
subroutine ppohDEM_Verlet2(parameters,particles) !Velocity Verlet

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    t_step=parameters%Tstep
    t_step1=t_step/parameters%Mass
    
    !update velocity latter half
    do i=1,particles%n
        particles%vel(i)%x=particles%vel(i)%x+0.5*t_step1*particles%force(i)%x
        particles%vel(i)%y=particles%vel(i)%y+0.5*t_step1*particles%force(i)%y
        particles%vel(i)%z=particles%vel(i)%z+0.5*t_step1*particles%force(i)%z
    enddo
    
    
end subroutine

    
    
    
