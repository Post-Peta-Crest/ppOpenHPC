!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohDEM                                          !!
!!         Version : 0.1.0                                            !!
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

subroutine ppohDEM_dataout (file_info,parameters,particles,counter)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    integer(kind=kint) :: file_index
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_counter) :: counter
    
            
    counter%itime=counter%itime+1
    counter%rtime=counter%rtime+parameters%Tstep
    
    if(mod(counter%itime,parameters%Dstep)==0)then
        
        write (*,'(" *** outputting   file:",i3.3)') counter%file_index+1
        counter%file_index=counter%file_index+1
        write(file_info%output_filename,'("./data/out",i3.3,".dat")') counter%file_index
        call ppohDEM_particles_fileout(file_info,particles)
    
    endif
        
        
end subroutine

subroutine ppohDEM_particles_fileout (file_info,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_particles) :: particles
    
    
    
    open (12, file=file_info%output_filename, status='unknown')
    
    
    
    do i=1,particles%n
        write(12,'(7e16.6)') particles%pos(i)%x,particles%pos(i)%y,particles%pos(i)%z, &
            particles%radius(i)
            !particles%vel(i)%x,particles%vel(i)%y,particles%vel(i)%z
            !particles%omega(i)%x,particles%omega(i)%y,particles%omega(i)%z
            !particles%force(i)%x,particles%force(i)%y,particles%force(i)%z, &
            !particles%rforce(i)%x,particles%rforce(i)%y,particles%rforce(i)%z
            !particles%vel(i)%x,particles%vel(i)%y,particles%vel(i)%z
            !particles%omega(i)%x,particles%omega(i)%y,particles%omega(i)%z
    enddo
    
    close(12)
    

    end subroutine
    
    
subroutine ppohDEM_partfileout (file_info,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_particles) :: particles
    
    
    
    open (12, file=file_info%output_filename, status='unknown')
    
    
    write(12,*) "#r0=0.5"
    write(12,*) "#box_sx=0.0"
    write(12,*) "#box_sy=0.0"
    write(12,*) "#box_sz=0.0"
    write(12,*) "#box_ex=11.0"
    write(12,*) "#box_ey=11.0"
    write(12,*) "#box_ez=11.0"
    
    do i=1,particles%n
        write(12,'(2i5,3e16.6)') i,0,particles%pos(i)%x,particles%pos(i)%y,particles%pos(i)%z
    enddo
    
    close(12)
    

end subroutine
    
    
subroutine ppohDEM_objects_fileout (file_info,objects)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_objects) :: objects
    
    
    
    open (12, file=file_info%output_filename, status='unknown')
    
    
    
    do k=1,objects%gridnum%z
        do j=1,objects%gridnum%y
            do i=1,objects%gridnum%x
                ijk=i+objects%gridnum%x*(j-1)+objects%gridnum%x*objects%gridnum%y*(k-1)
                write(12,'(1e16.6 )',advance='no') objects%scaled_distance(ijk)
            enddo
            write(12,*)
        enddo
        write(12,*)
    enddo
    
    close(12)
    

end subroutine
    
    
subroutine ppohDEM_distancecheck_fileout (file_info,parameters,objects)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_objects) :: objects
    real(kind=kreal) :: x,y,z
    
    
    open (12, file=file_info%output_filename, status='unknown')
    
    
    do k=1,objects%gridnum%z
        do j=1,objects%gridnum%y
            do i=1,objects%gridnum%x
                ijk=i+objects%gridnum%x*(j-1)+objects%gridnum%x*objects%gridnum%y*(k-1)
                x=objects%gridsize%x*(i-1)*parameters%L%x*0.9999999
                y=objects%gridsize%y*(j-1)*parameters%L%y*0.9999999
                z=objects%gridsize%z*(k-1)*parameters%L%z*0.9999999
                !write(12,'(1e16.6 )',advance='no') distance_from_objects2(parameters,objects,x,y,z)
                !write(12,'(4e16.6 )') x,y,z,distance_from_objects2(parameters,objects,x,y,z)
            enddo
            write(12,*)
        enddo
        write(12,*)
    enddo
    
    close(12)
    

end subroutine
    
    
    
    
    
    
subroutine ppohDEM_contactlist_fileout (file_info,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_particles) :: particles
    
    
    
    open (12, file=file_info%output_filename, status='unknown')
    
    
    
    do i=1,particles%n
        write(12,'(i5)',advance='no') i
        write(12,*)
        write(12,'(e16.6 )',advance='no') particles%pos(i)%x,particles%pos(i)%y,particles%pos(i)%z
        write(12,*)
        do j=1,particles%contact_list(i)%n
            write(12,'(i5 )',advance='no') particles%contact_list(i)%pid(j)
            write(12,'(e16.6 )',advance='no') particles%contact_list(i)%tforce(j)%x
            write(12,'(e16.6 )',advance='no') particles%contact_list(i)%tforce(j)%y
            write(12,'(e16.6 )',advance='no') particles%contact_list(i)%tforce(j)%z
        enddo
        write(12,*)
    enddo
    
    close(12)
    

    end subroutine
    
subroutine ppohDEM_contactlist_wall_fileout (file_info,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_particles) :: particles
    
    
    
    open (12, file=file_info%output_filename, status='unknown')
    
    
    
    do i=1,particles%n
        !write(12,'(i )',advance='no') i
        !write(12,*)
        !write(12,'(e16.6 )',advance='no') particles%pos(i)%x,particles%pos(i)%y,particles%pos(i)%z
        write(12,*)
        do j=1,particles%contact_list_wall(i)%n
            !write(12,'(i5 )',advance='no') particles%contact_list_wall(i)%pid(j)
            write(12,'(e16.6 )',advance='no') particles%contact_list_wall(i)%tforce(j)%x
            write(12,'(e16.6 )',advance='no') particles%contact_list_wall(i)%tforce(j)%y
            write(12,'(e16.6 )',advance='no') particles%contact_list_wall(i)%tforce(j)%z
        enddo
        write(12,*)
    enddo
    
    close(12)
    

end subroutine
