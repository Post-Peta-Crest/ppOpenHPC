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

subroutine ppohDEM_embedding(parameters,particles,cells,objects)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_cells) :: cells
    type(ppohDEM_objects) :: objects
    real(kind=kreal) :: vposx,vposy,vposz
    integer(kind=kint) :: cvx,cvy,cvz
    real(kind=kreal) :: pot_value
    type(ppohDEM_rvec3) :: force_from_objects
    type(ppohDEM_rvec3) :: add_rvec3
    
    do i=1,particles%n
        particles%force(i)=add_rvec3(particles%force(i),force_from_objects(parameters,particles,objects,i))
    enddo
    
end subroutine
    
function force_from_objects(parameters,particles,objects,index)

use ppohDEM_util
implicit real*8 (a-h,o-z)
type(ppohDEM_rvec3) :: force_from_objects
type(ppohDEM_parameters) :: parameters
type(ppohDEM_particles) :: particles
type(ppohDEM_objects) :: objects
integer(kind=kint) :: index

real(kind=kreal) :: vposx,vposy,vposz
integer(kind=kint) :: cvx,cvy,cvz
type(ppohDEM_rvec3) :: dn
real(kind=kreal) :: distance

distance=distance_from_objects(parameters,particles,objects,index)

if(particles%radius(index)-distance>0)then

! transform to object space
vposx=particles%pos(index)%x
vposy=particles%pos(index)%y
vposz=particles%pos(index)%z

cvx=floor(vposx/objects%gridsize%x)
cvy=floor(vposy/objects%gridsize%y)
cvz=floor(vposz/objects%gridsize%z)

vposx=mod(vposx,objects%gridsize%x)
vposy=mod(vposy,objects%gridsize%y)
vposz=mod(vposz,objects%gridsize%z)


ijk1=cvx+cvy*objects%gridnum%x+cvz*objects%gridnum%x*objects%gridnum%y+1
ijk2=ijk1+1
ijk3=ijk1+objects%gridnum%x
ijk4=ijk1+1+objects%gridnum%x
ijk5=ijk1+objects%gridnum%x*objects%gridnum%y
ijk6=ijk1+1+objects%gridnum%x*objects%gridnum%y
ijk7=ijk1+objects%gridnum%x+objects%gridnum%x*objects%gridnum%y
ijk8=ijk1+1+objects%gridnum%x+objects%gridnum%x*objects%gridnum%y

dn%x=objects%distance(ijk1)*(-1.)*(1.-vposy)*(1.-vposz)
dn%x=dn%x+objects%distance(ijk2)*(1.-vposy)*(1.-vposz)
dn%x=dn%x+objects%distance(ijk3)*(-1.)*vposy*(1.-vposz)
dn%x=dn%x+objects%distance(ijk4)*vposy*(1.-vposz)
dn%x=dn%x+objects%distance(ijk5)*(-1.)*(1.-vposy)*vposz
dn%x=dn%x+objects%distance(ijk6)*(1.-vposy)*vposz
dn%x=dn%x+objects%distance(ijk7)*(-1.)*vposy*vposz
dn%x=dn%x+objects%distance(ijk8)*vposy*vposz

dn%y=objects%distance(ijk1)*(1.-vposx)*(-1.)*(1.-vposz)
dn%y=dn%y+objects%distance(ijk2)*vposx*(-1.)*(1.-vposz)
dn%y=dn%y+objects%distance(ijk3)*(1.-vposx)*(1.-vposz)
dn%y=dn%y+objects%distance(ijk4)*vposx*(1.-vposz)
dn%y=dn%y+objects%distance(ijk5)*(1.-vposx)*(-1.)*vposz
dn%y=dn%y+objects%distance(ijk6)*vposx*(-1.)*vposz
dn%y=dn%y+objects%distance(ijk7)*(1.-vposx)*vposz
dn%y=dn%y+objects%distance(ijk8)*vposx*vposz

dn%z=objects%distance(ijk1)*(1.-vposx)*(1.-vposy)*(-1.)
dn%z=dn%z+objects%distance(ijk2)*vposx*(1.-vposy)*(-1.)
dn%z=dn%z+objects%distance(ijk3)*(1.-vposx)*vposy*(-1.)
dn%z=dn%z+objects%distance(ijk4)*vposx*vposy*(-1.)
dn%z=dn%z+objects%distance(ijk5)*(1.-vposx)*(1.-vposy)
dn%z=dn%z+objects%distance(ijk6)*vposx*(1.-vposy)
dn%z=dn%z+objects%distance(ijk7)*(1.-vposx)*vposy
dn%z=dn%z+objects%distance(ijk8)*vposx*vposy



call normalize_rvec3(dn)

force_from_objects%x=dn%x*(parameters%Obj_Kn*(particles%radius(index)-distance)-parameters%En*particles%vel(index)%x)
force_from_objects%y=dn%y*(parameters%Obj_Kn*(particles%radius(index)-distance)-parameters%En*particles%vel(index)%y)
force_from_objects%z=dn%z*(parameters%Obj_Kn*(particles%radius(index)-distance)-parameters%En*particles%vel(index)%z)

else

force_from_objects%x=0.0
force_from_objects%y=0.0
force_from_objects%z=0.0

endif


end function

function force_from_objects_old(parameters,particles,objects,index)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: force_from_objects
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_objects) :: objects
    integer(kind=kint) :: index
    
    real(kind=kreal) :: vposx,vposy,vposz
    integer(kind=kint) :: cvx,cvy,cvz
    type(ppohDEM_rvec3) :: dn
    real(kind=kreal) :: distance
    
    distance=distance_from_objects(parameters,particles,objects,index)
    
    if(particles%radius(index)-distance>0)then
    
        ! transform to object space
        vposx=particles%pos(index)%x
        vposy=particles%pos(index)%y
        vposz=particles%pos(index)%z
    
        cvx=floor(vposx/objects%gridsize%x)
        cvy=floor(vposy/objects%gridsize%y)
        cvz=floor(vposz/objects%gridsize%z)
    
        vposx=mod(vposx,objects%gridsize%x)
        vposy=mod(vposy,objects%gridsize%y)
        vposz=mod(vposz,objects%gridsize%z)
        
    
	    ijk1=cvx+cvy*objects%gridnum%x+cvz*objects%gridnum%x*objects%gridnum%y+1
	    ijk2=ijk1+1
	    ijk3=ijk1+objects%gridnum%x
	    ijk4=ijk1+1+objects%gridnum%x
	    ijk5=ijk1+objects%gridnum%x*objects%gridnum%y
	    ijk6=ijk1+1+objects%gridnum%x*objects%gridnum%y
	    ijk7=ijk1+objects%gridnum%x+objects%gridnum%x*objects%gridnum%y
	    ijk8=ijk1+1+objects%gridnum%x+objects%gridnum%x*objects%gridnum%y

	    dn%x=objects%distance(ijk1)*(-1.)*(1.-vposy)*(1.-vposz)
	    dn%x=dn%x+objects%distance(ijk2)*(1.-vposy)*(1.-vposz)
	    dn%x=dn%x+objects%distance(ijk3)*(-1.)*vposy*(1.-vposz)
	    dn%x=dn%x+objects%distance(ijk4)*vposy*(1.-vposz)
	    dn%x=dn%x+objects%distance(ijk5)*(-1.)*(1.-vposy)*vposz
	    dn%x=dn%x+objects%distance(ijk6)*(1.-vposy)*vposz
	    dn%x=dn%x+objects%distance(ijk7)*(-1.)*vposy*vposz
	    dn%x=dn%x+objects%distance(ijk8)*vposy*vposz
    
	    dn%y=objects%distance(ijk1)*(1.-vposx)*(-1.)*(1.-vposz)
	    dn%y=dn%y+objects%distance(ijk2)*vposx*(-1.)*(1.-vposz)
	    dn%y=dn%y+objects%distance(ijk3)*(1.-vposx)*(1.-vposz)
	    dn%y=dn%y+objects%distance(ijk4)*vposx*(1.-vposz)
	    dn%y=dn%y+objects%distance(ijk5)*(1.-vposx)*(-1.)*vposz
	    dn%y=dn%y+objects%distance(ijk6)*vposx*(-1.)*vposz
	    dn%y=dn%y+objects%distance(ijk7)*(1.-vposx)*vposz
	    dn%y=dn%y+objects%distance(ijk8)*vposx*vposz
    
	    dn%z=objects%distance(ijk1)*(1.-vposx)*(1.-vposy)*(-1.)
	    dn%z=dn%z+objects%distance(ijk2)*vposx*(1.-vposy)*(-1.)
	    dn%z=dn%z+objects%distance(ijk3)*(1.-vposx)*vposy*(-1.)
	    dn%z=dn%z+objects%distance(ijk4)*vposx*vposy*(-1.)
	    dn%z=dn%z+objects%distance(ijk5)*(1.-vposx)*(1.-vposy)
	    dn%z=dn%z+objects%distance(ijk6)*vposx*(1.-vposy)
	    dn%z=dn%z+objects%distance(ijk7)*(1.-vposx)*vposy
	    dn%z=dn%z+objects%distance(ijk8)*vposx*vposy
        
        
    
        call normalize_rvec3(dn)
        
        force_from_objects%x=dn%x*parameters%Obj_Kn*(particles%radius(index)-distance)
        force_from_objects%y=dn%y*parameters%Obj_Kn*(particles%radius(index)-distance)
        force_from_objects%z=dn%z*parameters%Obj_Kn*(particles%radius(index)-distance)
    
    else
        
        force_from_objects%x=0.0        
        force_from_objects%y=0.0        
        force_from_objects%z=0.0
        
    endif
    
    
end function
   

function distance_from_objects(parameters,particles,objects,index)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    real(kind=kreal) :: distance_from_objects
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    type(ppohDEM_objects) :: objects
    integer(kind=kint) :: index
    
    real(kind=kreal) :: vposx,vposy,vposz
    integer(kind=kint) :: cvx,cvy,cvz
    
    ! transform to object space
    vposx=particles%pos(index)%x
    vposy=particles%pos(index)%y
    vposz=particles%pos(index)%z
    
    cvx=floor(vposx/objects%gridsize%x)
    cvy=floor(vposy/objects%gridsize%y)
    cvz=floor(vposz/objects%gridsize%z)
    
    vposx=mod(vposx,objects%gridsize%x)
    vposy=mod(vposy,objects%gridsize%y)
    vposz=mod(vposz,objects%gridsize%z)
        
	ijk1=cvx+cvy*objects%gridnum%x+cvz*objects%gridnum%x*objects%gridnum%y+1
	ijk2=ijk1+1
	ijk3=ijk1+objects%gridnum%x
	ijk4=ijk1+1+objects%gridnum%x
	ijk5=ijk1+objects%gridnum%x*objects%gridnum%y
	ijk6=ijk1+1+objects%gridnum%x*objects%gridnum%y
	ijk7=ijk1+objects%gridnum%x+objects%gridnum%x*objects%gridnum%y
	ijk8=ijk1+1+objects%gridnum%x+objects%gridnum%x*objects%gridnum%y

	distance_from_objects=objects%distance(ijk1)*(1.-vposx)*(1.-vposy)*(1.-vposz)
	distance_from_objects=distance_from_objects+objects%distance(ijk2)*vposx*(1.-vposy)*(1.-vposz)
	distance_from_objects=distance_from_objects+objects%distance(ijk3)*(1.-vposx)*vposy*(1.-vposz)
	distance_from_objects=distance_from_objects+objects%distance(ijk4)*vposx*vposy*(1.-vposz)
	distance_from_objects=distance_from_objects+objects%distance(ijk5)*(1.-vposx)*(1.-vposy)*vposz
	distance_from_objects=distance_from_objects+objects%distance(ijk6)*vposx*(1.-vposy)*vposz
	distance_from_objects=distance_from_objects+objects%distance(ijk7)*(1.-vposx)*vposy*vposz
	distance_from_objects=distance_from_objects+objects%distance(ijk8)*vposx*vposy*vposz
        

end function
   
