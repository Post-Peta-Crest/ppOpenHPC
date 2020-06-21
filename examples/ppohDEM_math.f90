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
    
    
    
function length_rvec3 (v1)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1
    real(kind=kreal) :: length_rvec3
    
    length_rvec3=sqrt(v1%x*v1%x+v1%y*v1%y+v1%z*v1%z)
    
    return
    
end function length_rvec3
subroutine normalize_rvec3(vec)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: vec
    real(kind=kreal) :: tmp
    real(kind=kreal) :: length_rvec3
    
    tmp=length_rvec3(vec)
    
    if(tmp==0)then
        vec%x=0.0
        vec%y=0.0
        vec%z=0.0
    else
        vec%x=vec%x/tmp
        vec%y=vec%y/tmp
        vec%z=vec%z/tmp
    endif
        
end subroutine
    
subroutine clear_rvec3(v1)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1
    
    v1%x=0.
    v1%y=0.
    v1%z=0.
    
end subroutine
    
function make_rvec3(x,y,z)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    real(kind=kreal) :: x,y,z
    type(ppohDEM_rvec3) :: make_rvec3
    
    make_rvec3%x=x
    make_rvec3%y=y
    make_rvec3%z=z
    
end function
function negative_rvec3(v1)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1
    type(ppohDEM_rvec3) :: negative_rvec3
    
    negative_rvec3%x=-v1%x
    negative_rvec3%y=-v1%y
    negative_rvec3%z=-v1%z
    
end function
    
function add_rvec3(v1,v2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1,v2
    type(ppohDEM_rvec3) :: add_rvec3
    

    add_rvec3%x=v1%x+v2%x
    add_rvec3%y=v1%y+v2%y
    add_rvec3%z=v1%z+v2%z
    
end function
function sub_rvec3(v1,v2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1,v2
    type(ppohDEM_rvec3) :: sub_rvec3
    
    sub_rvec3%x=v1%x-v2%x
    sub_rvec3%y=v1%y-v2%y
    sub_rvec3%z=v1%z-v2%z
    
end function
function dot_rvec3(v1,v2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1,v2
    real(kind=kreal) :: dot_rvec3
    
    dot_rvec3=(v1%x*v2%x+v1%y*v2%y+v1%z*v2%z)
    
end function
function mulc_rvec3(v1,c)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: v1
    real(kind=kreal) :: c
    type(ppohDEM_rvec3) :: mulc_rvec3
    
    mulc_rvec3%x=c*v1%x
    mulc_rvec3%y=c*v1%y
    mulc_rvec3%z=c*v1%z
    
    end function
    

    
    
subroutine copy_rvec3(vec1,vec2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: vec1,vec2
    
    vec1%x=vec2%x
    vec1%y=vec2%y
    vec1%z=vec2%z
    
end subroutine
subroutine copy_pvec2(vec1,vec2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_pvec2) :: vec1,vec2
    
    vec1%theta=vec2%theta
    vec1%phi=vec2%phi
    
end subroutine
    
    
function maximal_eigenvector(tens)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rtens3) :: tens
    type(ppohDEM_rvec3) :: maximal_eigenvector
    real(kind=kreal) :: x(3),y(3)
    real(kind=kreal) :: rn(3)
    real(kind=kreal) :: lambda,t_lambda
    
    
    call random_number(rn(3))
    do i=1,3
        x(i)=rn(i)
    enddo
    
    
    do n=1,100
        y(1)=tens%xx*x(1)+tens%xy*x(2)+tens%xz*x(3)
        y(2)=tens%yx*x(1)+tens%yy*x(2)+tens%yz*x(3)
        y(3)=tens%zx*x(1)+tens%zy*x(2)+tens%zz*x(3)
        t_lambda=lambda
        lambda=dot_product(y,x)/dot_product(x,x)
        if(abs(t_lambda-lambda)<1e-6)then
            exit
        endif
        x=y/dot_product(y,y)  
    enddo
    
    maximal_eigenvector%x=y(1)
    maximal_eigenvector%y=y(2)
    maximal_eigenvector%z=y(3)
    
end
    
function cross_rvec3(vec1,vec2)

    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_rvec3) :: vec1,vec2
    type(ppohDEM_rvec3) :: cross_rvec3
    
    cross_rvec3%x=vec1%y*vec2%z-vec1%z*vec2%y
    cross_rvec3%y=vec1%z*vec2%x-vec1%x*vec2%z
    cross_rvec3%z=vec1%x*vec2%y-vec1%y*vec2%x
    
    end function
    
function max_radius(parameters,particles)
    
    use ppohDEM_util
    implicit real*8 (a-h,o-z)
    type(ppohDEM_parameters) :: parameters
    type(ppohDEM_particles) :: particles
    real(kind=kreal) :: max_radius
    
    max_radius=particles%radius(1)
    do i=2,particles%n
        if(max_radius<particles%radius(i))then
            max_radius=particles%radius(i)
        endif
    enddo
    
end function