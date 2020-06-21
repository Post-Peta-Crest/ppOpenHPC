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

subroutine ppohDEM_calcforce(parameters,particles,cells,walls,objects)

    use ppohDEM_util
!    implicit real*8 (a-h,o-z)
    implicit none

    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_walls) :: walls
    type(ppohDEM_objects) :: objects
    integer(kind=kint) :: cn
    integer(kind=kint) :: nei_cell
    type(ppohDEM_rvec3) :: force_ij,rforce_ij
    type(ppohDEM_rvec3) :: add_rvec3,make_rvec3
    type(ppohDEM_clist) :: old_contact_list
    type(ppohDEM_clist) :: old_contact_list_wall

    integer(kind=kint) :: i, j, k
    
    do i=1,particles%n
        
        particles%force(i)=make_rvec3(0.d0,0.d0,0.d0)
        particles%rforce(i)=make_rvec3(0.d0,0.d0,0.d0)
        
        
        ! particle
        call prepare_contactlist(particles,i,old_contact_list)
        do cn=1,27    
            nei_cell=cells%nei_cellindex(27*(particles%cellindex(i)-1)+cn)
            do j=cells%start_partindex(nei_cell),cells%end_partindex(nei_cell)
                call calc_force_ij_DEM(parameters,particles,force_ij,rforce_ij,i,j,old_contact_list)
                particles%force(i)=add_rvec3(particles%force(i),force_ij)
                particles%rforce(i)=add_rvec3(particles%rforce(i),rforce_ij)
            enddo
        enddo
        call discard_contactlist(old_contact_list)
    
        ! wall
        call prepare_contactlist_wall(particles,i,old_contact_list_wall)
        do j=1,walls%n
            call calc_force_wall_DEM(parameters,particles,walls,force_ij,rforce_ij,i,j,old_contact_list_wall)
            particles%force(i)=add_rvec3(particles%force(i),force_ij)
            particles%rforce(i)=add_rvec3(particles%rforce(i),rforce_ij)
        enddo
        call discard_contactlist_wall(old_contact_list_wall)
        
        
    enddo
    
     
end subroutine
subroutine prepare_contactlist(particles,i,old_contact_list)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_particles) :: particles
    integer(kind=kint) :: i
    type(ppohDEM_clist) :: old_contact_list
    
    old_contact_list%n=particles%contact_list(i)%n
    allocate(old_contact_list%pid(old_contact_list%n))
    allocate(old_contact_list%tforce(old_contact_list%n))
    allocate(old_contact_list%dn(old_contact_list%n))
    do k=1,old_contact_list%n
        old_contact_list%pid(k)=particles%contact_list(i)%pid(k)
        old_contact_list%tforce(k)=particles%contact_list(i)%tforce(k)
        old_contact_list%dn(k)=particles%contact_list(i)%dn(k)
    enddo
    particles%contact_list(i)%n=0
    
        
end subroutine
subroutine discard_contactlist(old_contact_list)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_clist) :: old_contact_list
    
    deallocate(old_contact_list%pid)
    deallocate(old_contact_list%tforce)
    deallocate(old_contact_list%dn)
        
end subroutine
subroutine prepare_contactlist_wall(particles,i,old_contact_list_wall)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_particles) :: particles
    integer(kind=kint) :: i
    type(ppohDEM_clist) :: old_contact_list_wall
    
    old_contact_list_wall%n=particles%contact_list_wall(i)%n
    allocate(old_contact_list_wall%pid(old_contact_list_wall%n))
    allocate(old_contact_list_wall%tforce(old_contact_list_wall%n))
    allocate(old_contact_list_wall%dn(old_contact_list_wall%n))
    do k=1,old_contact_list_wall%n
        old_contact_list_wall%pid(k)=particles%contact_list_wall(i)%pid(k)
        old_contact_list_wall%tforce(k)=particles%contact_list_wall(i)%tforce(k)
        old_contact_list_wall%dn(k)=particles%contact_list_wall(i)%dn(k)
    enddo
    particles%contact_list_wall(i)%n=0
        
end subroutine
subroutine discard_contactlist_wall(old_contact_list_wall)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_clist) :: old_contact_list_wall
    
    deallocate(old_contact_list_wall%pid)
    deallocate(old_contact_list_wall%tforce)
    deallocate(old_contact_list_wall%dn)
        
end subroutine
    
subroutine calc_force_ij_simple(parameters,particles,force_ij,i,j)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_rvec3) :: force_ij
    integer(kind=kint) :: orig_i,orig_j
    type(ppohDEM_rvec3) :: dr,dv
    real(kind=kreal) :: value_dis
    real(kind=kreal) :: nv
    real(kind=kreal) :: a
    
    real(kind=kreal) :: length_rvec3,dot_rvec3
    type(ppohDEM_rvec3) :: add_rvec3,sub_rvec3,mulc_rvec3,make_rvec3
    
    if(i.eq.j)then
        force_ij=make_rvec3(0.d0,0.d0,0.d0)
    else
    
        !orig_i=particles%newtoorig(i) ! Use these if you use unchanged names.
        !orig_j=particles%newtoorig(j)
        
    
        dr=sub_rvec3(particles%pos(i),particles%pos(j))
    
        a=length_rvec3(dr)
        value_dis=particles%radius(i)+particles%radius(j)-a
    
        if ((value_dis>0).and.(a.ne.0)) then
            b=1./a
            dr=mulc_rvec3(dr,b)
            dv=sub_rvec3(particles%vel(i),particles%vel(j))
            nv=dot_rvec3(dv,dr)
            force_ij=mulc_rvec3(dr,parameters%Kn*value_dis-parameters%En*nv)
        else
            force_ij=make_rvec3(0.d0,0.d0,0.d0)
        endif
    
    endif
    
end subroutine   
    
    
subroutine calc_force_ij_DEM(parameters,particles,force_ij,rforce_ij,i,j,old_contact_list)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_rvec3) :: force_ij
    type(ppohDEM_rvec3) :: rforce_ij
    integer(kind=kint) :: i,j
    type(ppohDEM_clist) :: old_contact_list
    
    integer(kind=kint) :: orig_i,orig_j
    real(kind=kreal) :: value_dis
    real(kind=kreal) :: a
    type(ppohDEM_rvec3) :: dn
    real(kind=kreal) :: rihun
    type(ppohDEM_rvec3) :: ff,vc,vt
    real(kind=kreal) :: nv,fnn
    type(ppohDEM_rvec3) :: ff2,ff3
    
    real(kind=kreal) :: length_rvec3,dot_rvec3
    type(ppohDEM_rvec3) :: add_rvec3,sub_rvec3,mulc_rvec3,make_rvec3,cross_rvec3,negative_rvec3
    
    if(i.eq.j)then
        force_ij=make_rvec3(0.d0,0.d0,0.d0)
        rforce_ij=make_rvec3(0.d0,0.d0,0.d0)
    else
    
        !orig_i=particles%newtoorig(i) ! Use these if you use unchanged names.
        !orig_j=particles%newtoorig(j)
        
    
        dn=sub_rvec3(particles%pos(j),particles%pos(i))
    
        a=length_rvec3(dn)
        value_dis=particles%radius(i)+particles%radius(j)-a
    
        if ((value_dis>0).and.(a.ne.0)) then
            
            b=1./a
            dn=mulc_rvec3(dn,b)
            
            particles%contact_list(i)%n=particles%contact_list(i)%n+1
            particles%contact_list(i)%pid(particles%contact_list(i)%n)=j
            particles%contact_list(i)%dn(particles%contact_list(i)%n)=dn
            
            
            ! normal force !!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            rihun_i=particles%radius(i)-0.5*value_dis
            rihun_j=particles%radius(j)-0.5*value_dis
            ff=add_rvec3(mulc_rvec3(particles%omega(j),rihun_j),mulc_rvec3(particles%omega(i),rihun_i))
            vc=sub_rvec3(sub_rvec3(particles%vel(j),particles%vel(i)),cross_rvec3(ff,dn))
            
            
            nv=dot_rvec3(vc,dn)
            vt=sub_rvec3(vc,mulc_rvec3(dn,nv))
            fnn=-parameters%Kn*value_dis+parameters%En*nv
            force_ij=mulc_rvec3(dn,fnn)
            
            
            ! tangential force !!!!!!!!!!!!!!!!!!!!!!!!
            
            ff=mulc_rvec3(vt,parameters%Kt*parameters%Tstep)
            
            do k=1,old_contact_list%n+1
                if(k==old_contact_list%n+1)then
                    
                    particles%contact_list(i)%tforce(particles%contact_list(i)%n)=ff
                    ff=add_rvec3(ff,mulc_rvec3(vc,parameters%Et))
                    force_ij=add_rvec3(force_ij,ff)
                    rforce_ij=cross_rvec3(dn,ff)
                    exit
                    
                elseif(old_contact_list%pid(k)==j)then
                    
                    ff2=cross_rvec3(old_contact_list%dn(k),dn)
                    ff3=add_rvec3(old_contact_list%tforce(k),cross_rvec3(ff2,old_contact_list%tforce(k)))
                    ff=add_rvec3(ff3,ff)
                    particles%contact_list(i)%tforce(particles%contact_list(i)%n)=ff
                    ff=add_rvec3(ff,mulc_rvec3(vc,parameters%Et))
                    force_ij=add_rvec3(force_ij,ff)
                    rforce_ij=cross_rvec3(dn,ff)
                    exit
                    
                endif
            enddo
                    
            
        else
            force_ij=make_rvec3(0.d0,0.d0,0.d0)
            rforce_ij=make_rvec3(0.d0,0.d0,0.d0)
        endif
    
    endif
    
    end subroutine   
    
    
            
    
    
subroutine calc_force_wall_DEM(parameters,particles,walls,force_ij,rforce_ij,i,j,old_contact_list_wall)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_walls) :: walls
    type(ppohDEM_rvec3) :: force_ij
    type(ppohDEM_rvec3) :: rforce_ij
    integer(kind=kint) :: i,j
    type(ppohDEM_clist) :: old_contact_list_wall
    
    integer(kind=kint) :: orig_i,orig_j
    real(kind=kreal) :: value_dis
    real(kind=kreal) :: a
    type(ppohDEM_rvec3) :: dn,dr
    real(kind=kreal) :: rihun
    type(ppohDEM_rvec3) :: ff,vc
    real(kind=kreal) :: nv,fnn
    type(ppohDEM_rvec3) :: ff2,ff3
    type(ppohDEM_rvec3) :: vc2,tt,vt
    type(ppohDEM_rvec3) :: tdelta
    real(kind=kreal) :: f1,f2,f3
    
    real(kind=kreal) :: length_rvec3,dot_rvec3
    type(ppohDEM_rvec3) :: add_rvec3,sub_rvec3,mulc_rvec3,make_rvec3,cross_rvec3,negative_rvec3
    

    dr=sub_rvec3(walls%point(j),particles%pos(i))
    dn=negative_rvec3(walls%nvec(j))
    rihun_i=dot_rvec3(dr,dn)
        
    value_dis=particles%radius(i)-rihun_i
    
    if ((value_dis>0).and.(a.ne.0)) then
            
            
            particles%contact_list_wall(i)%n=particles%contact_list_wall(i)%n+1
            particles%contact_list_wall(i)%pid(particles%contact_list_wall(i)%n)=j
            particles%contact_list_wall(i)%dn(particles%contact_list_wall(i)%n)=dn
            
            
            ! normal force !!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            
            ff=mulc_rvec3(particles%omega(i),rihun_i)
            vc=negative_rvec3(add_rvec3(particles%vel(i),cross_rvec3(ff,dn)))
            
            nv=dot_rvec3(vc,dn)
            fnn=-parameters%Kn*value_dis+parameters%En*nv
            force_ij=mulc_rvec3(dn,fnn)
            
            
             ! tangential force !!!!!!!!!!!!!!!!!!!!!!!!
            
            ff=mulc_rvec3(sub_rvec3(vc,mulc_rvec3(dn,nv)),parameters%Kt*parameters%Tstep)
            
            do k=1,old_contact_list_wall%n+1
                if(k==old_contact_list_wall%n+1)then

                    particles%contact_list_wall(i)%tforce(particles%contact_list_wall(i)%n)=ff
                    ff=add_rvec3(ff,mulc_rvec3(vc,parameters%Et))
                    force_ij=add_rvec3(force_ij,ff)
                    rforce_ij=cross_rvec3(dn,ff)
                    exit
                    
                elseif(old_contact_list_wall%pid(k)==j)then
                    
                    ff3=add_rvec3(old_contact_list_wall%tforce(k),ff)
                    particles%contact_list_wall(i)%tforce(particles%contact_list_wall(i)%n)=ff3
                    ff3=add_rvec3(ff3,mulc_rvec3(vc,parameters%Et))
                    
                    f1=length_rvec3(ff3)
                    f2=abs(fnn)
                    if(f1>f2*parameters%Fric_const)then
                        f3=f2*parameters%Fric_const/f1
                        ff3=mulc_rvec3(ff3,f3)
                    endif
                    
                    force_ij=add_rvec3(force_ij,ff3)
                    rforce_ij=cross_rvec3(dn,ff3)
                    exit
                    
                endif
            enddo
            
                    
            
        else
            force_ij=make_rvec3(0.d0,0.d0,0.d0)
            rforce_ij=make_rvec3(0.d0,0.d0,0.d0)
        endif
    
end subroutine   
  
subroutine ppohDEM_calcforce_all2all(parameters,particles,cells,walls,objects)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_walls) :: walls
    type(ppohDEM_objects) :: objects
    integer(kind=kint) :: cn
    integer(kind=kint) :: nei_cell
    type(ppohDEM_rvec3) :: force_ij,rforce_ij
    type(ppohDEM_rvec3) :: add_rvec3,make_rvec3
    type(ppohDEM_clist) :: old_contact_list
    type(ppohDEM_clist) :: old_contact_list_wall
    
    do i=1,particles%n
        
        particles%force(i)=make_rvec3(0.d0,0.d0,0.d0)
        particles%rforce(i)=make_rvec3(0.d0,0.d0,0.d0)
        
        call prepare_contactlist(particles,i,old_contact_list)
        ! particle
            do j=1,particles%n
                call calc_force_ij_DEM(parameters,particles,force_ij,rforce_ij,i,j,old_contact_list)
                particles%force(i)=add_rvec3(particles%force(i),force_ij)
                particles%rforce(i)=add_rvec3(particles%rforce(i),rforce_ij)
            enddo
        call discard_contactlist(old_contact_list)
        
    
        ! wall
        call prepare_contactlist_wall(particles,i,old_contact_list_wall)
        do j=1,walls%n
            call calc_force_wall_DEM(parameters,particles,walls,force_ij,rforce_ij,i,j,old_contact_list_wall)
            particles%force(i)=add_rvec3(particles%force(i),force_ij)
            particles%rforce(i)=add_rvec3(particles%rforce(i),rforce_ij)
        enddo
        call discard_contactlist_wall(old_contact_list_wall)
        
    enddo
    
    !call ppohDEM_embedding(parameters,particles,cells,object)
    
     
end subroutine
