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


subroutine ppohDEM_pre (file_info,parameters,particles,cells,walls,objects,counter)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_walls) :: walls
    type(ppohDEM_objects) :: objects
    type(ppohDEM_counter) :: counter
    real(kind=kreal) :: max_radius
    
    
    !! set randomseed to clocktime
    call ppohDEM_prepare_randomseed()
    
    !! prepare particles
    call ppohDEM_prepare_particles_bare(parameters,particles)
    !call ppohDEM_prepare_particles_fromfile(file_info,parameters,particles)
    
    !! prepare cells
    call ppohDEM_prepare_cells(parameters,cells)
    
    !! prepare walls 
    call ppohDEM_prepare_walls(parameters,walls)
    
    !! prepare object
!    call ppohDEM_prepare_objects(parameters,objects)
    call ppohDEM_prepare_objects_fromfile(parameters,objects)
    
    !! others
    counter%itime=0
    counter%rtime=0.
    counter%file_index=0
    counter%pos_accum=0.
    parameters%MaxRadius=max_radius(parameters,particles)


!    objects%vel%x = 0 ! cell /s 
!    objects%vel%y = 0
!    objects%vel%z = 0

    objects%vel%x = parameters%vel%x
    objects%vel%y = parameters%vel%y
    objects%vel%z = parameters%vel%z


    write (*,"(1A,1F10.6)") 'objects%vel%x: ', objects%vel%x

!    objects%omega%x = 1
!    objects%omega%y = -1
!    objects%omega%z = 1
!    objects%omega%w = - parameters%Pi / 400 ! rad / s

!    objects%omega%x = 0
!    objects%omega%y = 0
!    objects%omega%z = 1
!    objects%omega%w = 0  ! rad / s

    objects%omega%x = parameters%omega%x
    objects%omega%y = parameters%omega%y
    objects%omega%z = parameters%omega%z
    objects%omega%w = - parameters%Pi / parameters%omega%w ! rad / s


!    objects%centroid%x = 22 ! coordinate 
!    objects%centroid%y = 22
!    objects%centroid%z = 11 

    objects%centroid%x = parameters%centroid%x
    objects%centroid%y = parameters%centroid%y
    objects%centroid%z =  parameters%centroid%z

    
end subroutine
    
subroutine ppohDEM_prepare_randomseed()

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    
    integer :: seedsize
    integer,allocatable :: seed(:)
    integer :: c 

    call system_clock(count=c) 
    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    call random_seed(get=seed)
    seed = c 
    call random_seed(put=seed)
    
end subroutine
subroutine ppohDEM_prepare_particles_bare(parameters,particles)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    integer :: i


    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    real(kind=kreal),allocatable :: rn(:)

    real(kind=kreal) :: lx,lz
    real(kind=kreal) :: x0,z0
    real(kind=kreal) :: y0
    real(kind=kreal) :: interval_ratio
    
    
        
    particles%n=parameters%Pnum
    call ppohDEM_allocate_particles(parameters,particles)

    lx = parameters%L%x*0.9
    lz = parameters%L%z*0.9
    x0 = 0.1d0
    z0 = 5.0d0
    y0 = 0.5*parameters%L%y
    interval_ratio = 1.5d0
    
    ! position
    !call ppohDEM_config3dbox(parameters,particles,parameters%L%x*0.3,parameters%L%y*0.3,parameters%L%z*0.9,0.1d0,0.1d0,0.1d0,1.5d0)
    !call ppohDEM_locate_particles_2dbox(parameters,particles,parameters%L%x*0.9,parameters%L%z*0.9,0.1d0,5.0d0,0.5*parameters%L%y,1.5d0)
    call ppohDEM_locate_particles_2dbox(parameters,particles,lx,lz,x0,z0,y0,interval_ratio)
    
    ! radius
    allocate(rn(particles%n*3))
    call random_number(rn)
    do i=1,particles%n
        particles%radius(i)=Parameters%Radius*(1+0.2*rn(i))
    enddo
    deallocate(rn)
    
    ! velocity, angular velocity
    do i=1,particles%n
        call clear_rvec3(particles%vel(i))
        call clear_rvec3(particles%omega(i))
    enddo
        
    ! contact list
    call ppohDEM_clear_contactlist(parameters,particles)
    call ppohDEM_clear_contactlist_wall(parameters,particles)
    
end subroutine
subroutine ppohDEM_prepare_particles_fromfile(file_info,parameters,particles)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_fileinfo) :: file_info
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    
    
    open (11, file=file_info%input_filename, status='unknown')
    read (11,*) particles%n
    
    call ppohDEM_allocate_particles(parameters,particles)
    
    do i=1,particles%n
        read (11,*) particles%pos(i)%x,particles%pos(i)%y,particles%pos(i)%z,particles%radius(i)
    enddo
    
    close(11)
    
    
    do i=1,particles%n
        call clear_rvec3(particles%vel(i))
        call clear_rvec3(particles%omega(i))
    enddo
        
    ! contact list
    call ppohDEM_clear_contactlist(parameters,particles)
    call ppohDEM_clear_contactlist_wall(parameters,particles)

end subroutine
subroutine ppohDEM_prepare_cells(parameters,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_cells) :: cells
    integer(kind=kint) :: total_cell_num
    
    cells%cellsize%x=parameters%L%x/parameters%Cn%x
    cells%cellsize%y=parameters%L%y/parameters%Cn%y
    cells%cellsize%z=parameters%L%z/parameters%Cn%z
    
    cells%cellnum%x=parameters%Cn%x
    cells%cellnum%y=parameters%Cn%y
    cells%cellnum%z=parameters%Cn%z
    
    total_cell_num=parameters%Cn%x*parameters%Cn%y*parameters%Cn%z
    cells%n=total_cell_num
    
    allocate(cells%start_partindex(total_cell_num))
    allocate(cells%end_partindex(total_cell_num))
    allocate(cells%nei_cellindex(total_cell_num*27))
    
    call make_search_cell(parameters,cells)

end subroutine 
subroutine make_search_cell(parameters,cells)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_cells) :: cells
    
	integer(kind=kint) :: ijk,pijk
	integer(kind=kint) :: mi,mj,mk
	integer(kind=kint) :: cn
	
	
	do k=1,cells%cellnum%z
        do j=1,cells%cellnum%y
            do i=1,cells%cellnum%x

				ijk=cells%cellnum%x*cells%cellnum%y*(k-1)+cells%cellnum%x*(j-1)+i
				cn=1

                do kk=-1,1
                    do jj=-1,1
                        do ii=-1,1

							mi=i+ii
							mj=j+jj
							mk=k+kk

							if(mi<=0) mi=mi+cells%cellnum%x
                            if(mi>cells%cellnum%x) mi=mi-cells%cellnum%x
                            if(mj<=0) mj=mj+cells%cellnum%y
                            if(mj>cells%cellnum%y) mj=mj-cells%cellnum%y
                            if(mk<=0) mk=mk+cells%cellnum%z
                            if(mk>cells%cellnum%z) mk=mk-cells%cellnum%z

							pijk=cells%cellnum%x*cells%cellnum%y*(mk-1)+cells%cellnum%x*(mj-1)+mi
							cells%nei_cellindex(27*(ijk-1)+cn)=pijk
							cn=cn+1
                            
						enddo
					enddo
				enddo


			enddo
		enddo
	enddo


end subroutine
subroutine ppohDEM_prepare_walls(parameters,walls)    
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_walls) :: walls
    type(ppohDEM_rvec3) :: make_rvec3
    
    walls%n=6
    allocate(walls%point(walls%n))
    allocate(walls%nvec(walls%n))
    
    z0=0.
    z1=1.
    walls%point(1)=make_rvec3(z0,z0,z0)
    walls%nvec(1)=make_rvec3(z1,z0,z0)
    walls%point(2)=make_rvec3(z0,z0,z0)
    walls%nvec(2)=make_rvec3(z0,z1,z0)
    walls%point(3)=make_rvec3(z0,z0,z0)
    walls%nvec(3)=make_rvec3(z0,z0,z1)
    walls%point(4)=make_rvec3(parameters%L%x,parameters%L%y,parameters%L%z)
    walls%nvec(4)=make_rvec3(-z1,z0,z0)
    walls%point(5)=make_rvec3(parameters%L%x,parameters%L%y,parameters%L%z)
    walls%nvec(5)=make_rvec3(z0,-z1,z0)
    walls%point(6)=make_rvec3(parameters%L%x,parameters%L%y,parameters%L%z)
    walls%nvec(6)=make_rvec3(z0,z0,-z1)

end subroutine
subroutine ppohDEM_prepare_objects(parameters,objects)    
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_objects) :: objects
    
    
    objects%gridnum%x=parameters%Obj_n%x
    objects%gridnum%y=parameters%Obj_n%y
    objects%gridnum%z=parameters%Obj_n%z
    objects%gridsize%x=parameters%L%x/(objects%gridnum%x-1)
    objects%gridsize%y=parameters%L%y/(objects%gridnum%y-1)
    objects%gridsize%z=parameters%L%z/(objects%gridnum%z-1)
    objects%gn=objects%gridnum%x*objects%gridnum%y*objects%gridnum%z
    allocate(objects%distance(objects%gn))
    
    
    !2D sphere
    do k=1,objects%gridnum%z
        do j=1,objects%gridnum%y
            do i=1,objects%gridnum%x
                ijk=i+(j-1)*objects%gridnum%x+(k-1)*objects%gridnum%x*objects%gridnum%y
                x=objects%gridsize%x*(i-1)
                y=objects%gridsize%y*(j-1)
                z=objects%gridsize%z*(k-1)
                x0=0.5*parameters%L%x
                y0=0.5*parameters%L%y
                z0=0.
                distx=(x-x0)
                disty=0.
                distz=(z-z0)
                objects%distance(ijk)=sqrt(distx*distx+disty*disty+distz*distz)-0.3*parameters%L%x
            enddo
        enddo
    enddo
    

end subroutine
    
    
