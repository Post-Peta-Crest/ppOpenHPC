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

recursive subroutine ppohDEM_rqsort(vec,first,last)

    use ppohDEM_util
    implicit none
    real(kind=kreal) :: vec(*)
    integer(kind=kint) :: first,last
    
    integer(kind=kint) :: i,j
    real(kind=kreal) :: piv
    real(kind=kreal) :: tmp
     
    if(first<last)then
    
        piv=vec((first+last)/2)
        i=first
        j=last
        do
            do while(vec(i)<piv)
                i=i+1
            enddo
            do while(piv<vec(j))
                j=j-1
            enddo
            if(i>=j)exit
            tmp=vec(i)
            vec(i)=vec(j)
            vec(j)=tmp
            i=i+1
            j=j-1
        enddo
        if(first<i-1)then
            call ppohDEM_rqsort(vec,first,i-1)
        endif
        if(j+1<last)then
            call ppohDEM_rqsort(vec,j+1,last)
        endif
        
    endif
    
end subroutine   
recursive subroutine ppohDEM_iqsort(vec,first,last)

    use ppohDEM_util
    implicit none
    integer(kind=kint) :: vec(*)
    integer(kind=kint) :: first,last
    
    integer(kind=kint) :: i,j
    integer(kind=kint) :: piv
    integer(kind=kint) :: tmp
     
    piv=vec((first+last)/2)
    i=first
    j=last
    do
        do while(vec(i)<piv)
            i=i+1
        enddo
        do while(piv<vec(j))
            j=j-1
        enddo
        if(i>=j)exit
        tmp=vec(i)
        vec(i)=vec(j)
        vec(j)=tmp
        i=i+1
        j=j-1
    enddo
    
    if(first<i-1)then
        call ppohDEM_iqsort(vec,first,i-1)
    endif
    if(j+1<last)then
        call ppohDEM_iqsort(vec,j+1,last)
    endif
    
end subroutine
    
recursive subroutine ppohDEM_iqsort2(vec,vec2,first,last)

    use ppohDEM_util
    implicit none
    integer(kind=kint) :: vec(*),vec2(*)
    integer(kind=kint) :: first,last
    
    integer(kind=kint) :: i,j
    integer(kind=kint) :: piv
    integer(kind=kint) :: tmp
     
    piv=vec((first+last)/2)
    i=first
    j=last
    do
        do while(vec(i)<piv)
            i=i+1
        enddo
        do while(piv<vec(j))
            j=j-1
        enddo
        if(i>=j)exit
        tmp=vec(i)
        vec(i)=vec(j)
        vec(j)=tmp
        tmp=vec2(i)
        vec2(i)=vec2(j)
        vec2(j)=tmp
        i=i+1
        j=j-1
    enddo
    
    if(first<i-1)then
        call ppohDEM_iqsort2(vec,vec2,first,i-1)
    endif
    if(j+1<last)then
        call ppohDEM_iqsort2(vec,vec2,j+1,last)
    endif
    
end subroutine