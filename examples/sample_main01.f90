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

program sample_main01  

    use  ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_walls) :: walls
    type(ppohDEM_objects) :: objects
    type(ppohDEM_counter) :: counter
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    write (*,*)
    write (*,*) '*** initializing ppohDEM.'
    write (*,*)

    call ppohDEM_loadfileinfo(file_info)
    
    call ppohDEM_setparameters(file_info,parameters)
!    call ppohDEM_loadfileinfo(file_info)
    
    
    call ppohDEM_pre(file_info,parameters,particles,cells,walls,objects,counter)

!    stop
!--------------------------------------------------------------------------------

    call ppohDEM_update_index(parameters,particles,cells)
    call ppohDEM_checkparameters(parameters,particles,cells)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    write (*,*)
    write (*,*) '*** simulation starts.'
    write (*,*)
    
    do 
     
        call ppohDEM_check_update(parameters,particles,cells,counter)
        call ppohDEM_integrate_system(parameters,particles,cells,walls,objects)
        call ppohDEM_dataout(file_info,parameters,particles,counter)

        call ppohDEM_check_objects(parameters,objects,counter)
    
        if(counter%rtime>parameters%Tfin) exit
        
    enddo
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    write (*,*)
    write (*,*) '*** finalizing.'
    write (*,*)
    
    call ppohDEM_exit(particles,cells,walls,objects)
    
      
    stop
    
end program sample_main01






