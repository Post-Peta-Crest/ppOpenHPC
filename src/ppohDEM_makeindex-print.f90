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


subroutine ppohDEM_check_update(parameters,particles,cells,counter)
    
    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_counter) :: counter
    
    call ppohDEM_accumulate_pos(parameters,particles,counter)


    if(counter%pos_accum>0.5*(cells%cellsize%x-2*parameters%MaxRadius))then
        call ppohDEM_update_index(parameters,particles,cells)
        counter%pos_accum=0.
    endif    
    
end subroutine
subroutine ppohDEM_update_index(parameters,particles,cells)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    
    call ppohDEM_calc_cellindex(parameters,particles,cells)
    call ppohDEM_sort_particles(parameters,particles,cells)
    call ppohDEM_clear_cellstartend(parameters,particles,cells)
    call ppohDEM_calc_cellstartend(parameters,particles,cells)
    
    
end subroutine
    
subroutine ppohDEM_calc_cellindex(parameters,particles,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    integer(kind=kint) :: cx,cy,cz
 
!    integer :: cx,cy,cz
    
    do i=1,particles%n

        if (particles%pos(i)%x .le. 0) then 
           write (*,"(1A,1I5,1F16.6)") 'i, particles%pos(i)%x: ', i, particles%pos(i)%x
        end if
        if (particles%pos(i)%y .le. 0) then 
           write (*,"(1A,1I5,1F16.6)") 'i, particles%pos(i)%y: ', i, particles%pos(i)%y
        end if
        if (particles%pos(i)%z .le. 0) then 
           write (*,"(1A,1I5,1F16.6)") 'i, particles%pos(i)%z: ', i, particles%pos(i)%z
        end if

        if (cells%cellsize%x .le. 0) then 
           write (*,"(1A,2I5)") 'i, cells%cellsize%x: ', i, cells%cellsize%x
        end if
        if (cells%cellsize%y .le. 0) then 
           write (*,"(1A,2I5)") 'i, cells%cellsize%y: ', i, cells%cellsize%y
        end if
        if (cells%cellsize%z .le. 0) then 
           write (*,"(1A,2I5)") 'i, cells%cellsize%z: ', i, cells%cellsize%z
        end if


        cx=floor(particles%pos(i)%x/cells%cellsize%x)
        cy=floor(particles%pos(i)%y/cells%cellsize%y)
        cz=floor(particles%pos(i)%z/cells%cellsize%z)
        particles%cellindex(i)=cells%cellnum%x*cells%cellnum%y*cz+cells%cellnum%x*cy+cx+1 !cell index start with 1

        if (particles%cellindex(i) .le. 0) then 
           write (*,"(1A,2I5)") 'i, particles%cellindex(i): ', i, particles%cellindex(i)
        end if

    enddo
    
end subroutine 
subroutine ppohDEM_sort_particles(parameters,particles,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_particles) :: tmp_particles
    
    
    !! allocate temporary array 
    tmp_particles%n=particles%n
    call ppohDEM_allocate_particles(parameters,tmp_particles)
    
    !! copy to temporary array
    call ppohDEM_copy_particles(tmp_particles,particles)
    
    !! sort by cellindex
    do i=1,particles%n
        tmp_particles%newtoorig(i)=i
    enddo
    call ppohDEM_iqsort2(tmp_particles%cellindex,tmp_particles%newtoorig,1,particles%n)
    
    !! move from temprary to regular array
    call ppohDEM_rearrange_index(particles,tmp_particles)
    
    !! deallocate temporary array
    call ppohDEM_deallocate_particles(tmp_particles)
    

end subroutine
subroutine ppohDEM_rearrange_index(particles,tmp_particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles,tmp_particles
    
    integer(kind=kint) :: tmp_newtoorig


    do i=1,particles%n
        call copy_rvec3(particles%pos(i),tmp_particles%pos(tmp_particles%newtoorig(i)))
        call copy_rvec3(particles%vel(i),tmp_particles%vel(tmp_particles%newtoorig(i)))
        call copy_rvec3(particles%omega(i),tmp_particles%omega(tmp_particles%newtoorig(i)))
        particles%radius(i)=tmp_particles%radius(tmp_particles%newtoorig(i))
        particles%contact_list(i)%n=tmp_particles%contact_list(tmp_particles%newtoorig(i))%n
        do k=1,particles%contact_list(i)%n
            particles%contact_list(i)%pid(k)=tmp_particles%newtoorig(tmp_particles%contact_list(tmp_particles%newtoorig(i))%pid(k))
            call copy_rvec3(particles%contact_list(i)%tforce(k),tmp_particles%contact_list(tmp_particles%newtoorig(i))%tforce(k))
            call copy_rvec3(particles%contact_list(i)%dn(k),tmp_particles%contact_list(tmp_particles%newtoorig(i))%dn(k))
        enddo
        particles%contact_list_wall(i)%n=tmp_particles%contact_list_wall(tmp_particles%newtoorig(i))%n

        do k=1,particles%contact_list_wall(i)%n
            particles%contact_list_wall(i)%pid(k)=tmp_particles%contact_list_wall(tmp_particles%newtoorig(i))%pid(k)
!            call copy_rvec3(particles%contact_list_wall(i)%tforce(k),tmp_particles%contact_list_wall(tmp_particles%newtoorig(i))%tforce(k))
            tmp_newtoorig = tmp_particles%newtoorig(i)
            call copy_rvec3(particles%contact_list_wall(i)%tforce(k), tmp_particles%contact_list_wall(tmp_newtoorig)%tforce(k))

            call copy_rvec3(particles%contact_list_wall(i)%dn(k),tmp_particles%contact_list_wall(tmp_particles%newtoorig(i))%dn(k))
        enddo
    enddo
    
end subroutine   
subroutine ppohDEM_clear_cellstartend(parameters,particles,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    integer(kind=kint) :: cx,cy,cz
    
    do i=1,cells%n
        cells%start_partindex(i)=0
        cells%end_partindex(i)=-1
    enddo
    
end subroutine
subroutine ppohDEM_calc_cellstartend(parameters,particles,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    
    
    i=1
    cells%start_partindex(particles%cellindex(i))=i
    
    do i=2,particles%n
        if (particles%cellindex(i).ne.particles%cellindex(i-1)) then
            cells%start_partindex(particles%cellindex(i))=i
            cells%end_partindex(particles%cellindex(i-1))=i-1
        endif
    enddo
    
    i=particles%n
    cells%end_partindex(particles%cellindex(i))=i

end subroutine
    
    
    
subroutine ppohDEM_accumulate_pos(parameters,particles,counter)   
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_counter) :: counter
    real(kind=kreal) :: max_vel,vel,length_rvec3
    
    max_vel=0
    do i=1,particles%n
        vel=length_rvec3(particles%vel(i))
        if(max_vel<vel) max_vel=vel
    enddo
    
    counter%pos_accum=counter%pos_accum+max_vel
    
    
end subroutine
