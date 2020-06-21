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
    
subroutine ppohDEM_allocate_particles(parameters,particles)
    
    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    integer :: i

    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    ! particles%n is already given
    
    allocate(particles%pos(particles%n))
    allocate(particles%vel(particles%n))
    allocate(particles%radius(particles%n))
    allocate(particles%omega(particles%n))
    allocate(particles%force(particles%n))
    allocate(particles%rforce(particles%n))
    allocate(particles%cellindex(particles%n))
    allocate(particles%newtoorig(particles%n))
    allocate(particles%contact_list(particles%n))
    allocate(particles%contact_list_wall(particles%n))
    do i=1,particles%n
        allocate(particles%contact_list(i)%pid(parameters%Size_contact_list))
        allocate(particles%contact_list(i)%tforce(parameters%Size_contact_list))
        allocate(particles%contact_list(i)%dn(parameters%Size_contact_list))
        allocate(particles%contact_list_wall(i)%pid(parameters%Size_contact_list_wall))
        allocate(particles%contact_list_wall(i)%tforce(parameters%Size_contact_list_wall))
        allocate(particles%contact_list_wall(i)%dn(parameters%Size_contact_list_wall))
    enddo
    
end subroutine  
subroutine ppohDEM_copy_particles(particles1,particles2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_particles) :: particles1,particles2
    integer(kind=kint) :: pnum
    
    pnum=particles2%n
    
    do i=1,pnum
        call copy_rvec3(particles1%pos(i),particles2%pos(i))
        call copy_rvec3(particles1%vel(i),particles2%vel(i))
        call copy_rvec3(particles1%omega(i),particles2%omega(i))
        particles1%radius(i)=particles2%radius(i)
        particles1%cellindex(i)=particles2%cellindex(i)
        particles1%contact_list(i)%n=particles2%contact_list(i)%n
        do k=1,particles1%contact_list(i)%n
            particles1%contact_list(i)%pid(k)=particles2%contact_list(i)%pid(k)
            call copy_rvec3(particles1%contact_list(i)%tforce(k),particles2%contact_list(i)%tforce(k))
            call copy_rvec3(particles1%contact_list(i)%dn(k),particles2%contact_list(i)%dn(k))
        enddo
        particles1%contact_list_wall(i)%n=particles2%contact_list_wall(i)%n
        do k=1,particles1%contact_list_wall(i)%n
            particles1%contact_list_wall(i)%pid(k)=particles2%contact_list_wall(i)%pid(k)
            call copy_rvec3(particles1%contact_list_wall(i)%tforce(k),particles2%contact_list_wall(i)%tforce(k))
            call copy_rvec3(particles1%contact_list_wall(i)%dn(k),particles2%contact_list_wall(i)%dn(k))
        enddo
    enddo
    
end subroutine
subroutine ppohDEM_deallocate_particles(particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_particles) :: particles
    
    deallocate(particles%pos)
    deallocate(particles%vel)
    deallocate(particles%radius)
    deallocate(particles%omega)
    deallocate(particles%force)
    deallocate(particles%rforce)
    deallocate(particles%cellindex)
    deallocate(particles%newtoorig)
    do i=1,particles%n
        deallocate(particles%contact_list(i)%pid)
        deallocate(particles%contact_list(i)%tforce)
        deallocate(particles%contact_list(i)%dn)
    enddo
    deallocate(particles%contact_list)
    do i=1,particles%n
        deallocate(particles%contact_list_wall(i)%pid)
        deallocate(particles%contact_list_wall(i)%tforce)
        deallocate(particles%contact_list_wall(i)%dn)
    enddo
    deallocate(particles%contact_list_wall)
    
end subroutine
subroutine ppohDEM_copy_contactlist(parameters,particles)    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    do i=1,particles%n
        particles%contact_list(i)%n=0
        do j=1,parameters%Size_contact_list
            particles%contact_list(i)%pid(j)=0
            call clear_rvec3(particles%contact_list(i)%tforce(j))
            call clear_rvec3(particles%contact_list(i)%dn(j))
        enddo
    enddo
    
end subroutine
subroutine ppohDEM_clear_contactlist(parameters,particles)    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    do i=1,particles%n
        particles%contact_list(i)%n=0
        do j=1,parameters%Size_contact_list
            particles%contact_list(i)%pid(j)=0
            call clear_rvec3(particles%contact_list(i)%tforce(j))
            call clear_rvec3(particles%contact_list(i)%dn(j))
        enddo
    enddo
    
end subroutine
subroutine ppohDEM_clear_contactlist_wall(parameters,particles)    
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    do i=1,particles%n
        particles%contact_list_wall(i)%n=0
        do j=1,parameters%Size_contact_list_wall
            particles%contact_list_wall(i)%pid(j)=0
            call clear_rvec3(particles%contact_list_wall(i)%tforce(j))
            call clear_rvec3(particles%contact_list_wall(i)%dn(j))
        enddo
    enddo
    
end subroutine
subroutine ppohDEM_checkparameters(parameters,particles,cells)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    
    if(cells%cellsize%x-2*parameters%MaxRadius<0.)then
        write(*,*) ' ppohDEM-Error: cell size is smaller than a particle diameter'
        stop
    endif
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    parameters%Dfin=ceiling(parameters%Tfin/(parameters%Tstep*parameters%Dstep))
    write(*,'(" Note: ppohDEM will output ",i3.3," data files")') parameters%Dfin
    
end subroutine  
