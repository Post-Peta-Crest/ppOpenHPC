!=====================================================================!!
!!                                                                    !!
!!   Software Name : ppOpen-APPL/AMR-FDM (ppohAMRFDM)                 !!
!!         Version : 0.3.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohAMRFDM.                               !!
!!     ppohAMRFDM is a free software, you can use it under the terms  !!
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
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Masaharu Matsumoto, The University of Tokyo  !!
!!                       matsumoto(at)cc.u-tokyo.ac.jp           >    !!
!!                                                                    !!
!!====================================================================!!

subroutine ppohAMRFDM_connect_oct(iLv,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::ID,index
  type(st_ppohAMRFDM_octset),pointer::p0
  if(iLv>=st_param%LvMax) return 
  do ID=1,2
     if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_meshset,ID,iLv)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           p0 => st_meshset%Mesh(index)
           if(p0%iFLG(1)>0) then
              ! -- initialize connection --
              ! - for octCh1 -
              nullify(p0%octCh1%octNb1); nullify(p0%octCh1%octNb2)
              nullify(p0%octCh1%octNb3); nullify(p0%octCh1%octNb4)
              nullify(p0%octCh1%octNb5); nullify(p0%octCh1%octNb6)
              ! - for octCh2 -
              nullify(p0%octCh2%octNb1); nullify(p0%octCh2%octNb2)
              nullify(p0%octCh2%octNb3); nullify(p0%octCh2%octNb4)
              nullify(p0%octCh2%octNb5); nullify(p0%octCh2%octNb6)
              ! - for octCh3 -
              nullify(p0%octCh3%octNb1); nullify(p0%octCh3%octNb2)
              nullify(p0%octCh3%octNb3); nullify(p0%octCh3%octNb4)
              nullify(p0%octCh3%octNb5); nullify(p0%octCh3%octNb6)
              ! - for octCh4 -
              nullify(p0%octCh4%octNb1); nullify(p0%octCh4%octNb2)
              nullify(p0%octCh4%octNb3); nullify(p0%octCh4%octNb4)
              nullify(p0%octCh4%octNb5); nullify(p0%octCh4%octNb6)
              ! - for octCh5 -
              nullify(p0%octCh5%octNb1); nullify(p0%octCh5%octNb2)
              nullify(p0%octCh5%octNb3); nullify(p0%octCh5%octNb4)
              nullify(p0%octCh5%octNb5); nullify(p0%octCh5%octNb6)
              ! - for octCh6 -
              nullify(p0%octCh6%octNb1); nullify(p0%octCh6%octNb2)
              nullify(p0%octCh6%octNb3); nullify(p0%octCh6%octNb4)
              nullify(p0%octCh6%octNb5); nullify(p0%octCh6%octNb6)
              ! - for octCh7 -
              nullify(p0%octCh7%octNb1); nullify(p0%octCh7%octNb2)
              nullify(p0%octCh7%octNb3); nullify(p0%octCh7%octNb4)
              nullify(p0%octCh7%octNb5); nullify(p0%octCh7%octNb6)
              ! - for octCh8 -
              nullify(p0%octCh8%octNb1); nullify(p0%octCh8%octNb2)
              nullify(p0%octCh8%octNb3); nullify(p0%octCh8%octNb4)
              nullify(p0%octCh8%octNb5); nullify(p0%octCh8%octNb6)
              if(p0%iFLG(1)>=2) then 
                 ! - for octCh1 -
                 p0%octCh1%octNb1 => p0%octNb1%octCh2 
                 p0%octCh1%octNb2 => p0       %octCh2
                 p0%octCh1%octNb3 => p0%octNb3%octCh3
                 p0%octCh1%octNb4 => p0       %octCh3
                 p0%octCh1%octNb5 => p0%octNb5%octCh5
                 p0%octCh1%octNb6 => p0       %octCh5
                 ! - for octCh2 -
                 p0%octCh2%octNb1 => p0       %octCh1
                 p0%octCh2%octNb2 => p0%octNb2%octCh1
                 p0%octCh2%octNb3 => p0%octNb3%octCh4
                 p0%octCh2%octNb4 => p0       %octCh4
                 p0%octCh2%octNb5 => p0%octNb5%octCh6
                 p0%octCh2%octNb6 => p0       %octCh6
                 ! - for octCh3 -
                 p0%octCh3%octNb1 => p0%octNb1%octCh4
                 p0%octCh3%octNb2 => p0       %octCh4
                 p0%octCh3%octNb3 => p0       %octCh1
                 p0%octCh3%octNb4 => p0%octNb4%octCh1
                 p0%octCh3%octNb5 => p0%octNb5%octCh7
                 p0%octCh3%octNb6 => p0       %octCh7
                 ! - for octCh4 -
                 p0%octCh4%octNb1 => p0       %octCh3
                 p0%octCh4%octNb2 => p0%octNb2%octCh3
                 p0%octCh4%octNb3 => p0       %octCh2
                 p0%octCh4%octNb4 => p0%octNb4%octCh2
                 p0%octCh4%octNb5 => p0%octNb5%octCh8
                 p0%octCh4%octNb6 => p0       %octCh8
                 ! - for octCh5 -
                 p0%octCh5%octNb1 => p0%octNb1%octCh6 
                 p0%octCh5%octNb2 => p0       %octCh6
                 p0%octCh5%octNb3 => p0%octNb3%octCh7
                 p0%octCh5%octNb4 => p0       %octCh7
                 p0%octCh5%octNb5 => p0       %octCh1
                 p0%octCh5%octNb6 => p0%octNb6%octCh1
                 ! - for octCh6 -
                 p0%octCh6%octNb1 => p0       %octCh5
                 p0%octCh6%octNb2 => p0%octNb2%octCh5
                 p0%octCh6%octNb3 => p0%octNb3%octCh8
                 p0%octCh6%octNb4 => p0       %octCh8
                 p0%octCh6%octNb5 => p0       %octCh2
                 p0%octCh6%octNb6 => p0%octNb6%octCh2
                 ! - for octCh7 -
                 p0%octCh7%octNb1 => p0%octNb1%octCh8
                 p0%octCh7%octNb2 => p0       %octCh8
                 p0%octCh7%octNb3 => p0       %octCh5
                 p0%octCh7%octNb4 => p0%octNb4%octCh5
                 p0%octCh7%octNb5 => p0       %octCh3
                 p0%octCh7%octNb6 => p0%octNb6%octCh3
                 ! - for octCh8 -
                 p0%octCh8%octNb1 => p0       %octCh7
                 p0%octCh8%octNb2 => p0%octNb2%octCh7
                 p0%octCh8%octNb3 => p0       %octCh6
                 p0%octCh8%octNb4 => p0%octNb4%octCh6
                 p0%octCh8%octNb5 => p0       %octCh4
                 p0%octCh8%octNb6 => p0%octNb6%octCh4
              endif
              if(p0%iFLG(1)==1) then
                 ! - for octCh1 -
                 p0%octCh1%octNb1 => p0%octCh1
                 p0%octCh1%octNb2 => p0%octCh2 !
                 p0%octCh1%octNb3 => p0%octCh1 
                 p0%octCh1%octNb4 => p0%octCh3 !
                 p0%octCh1%octNb5 => p0%octCh1 
                 p0%octCh1%octNb6 => p0%octCh5 !
                 ! - for octCh2 -
                 p0%octCh2%octNb1 => p0%octCh1 !
                 p0%octCh2%octNb2 => p0%octCh2
                 p0%octCh2%octNb3 => p0%octCh2
                 p0%octCh2%octNb4 => p0%octCh4 !
                 p0%octCh2%octNb5 => p0%octCh2
                 p0%octCh2%octNb6 => p0%octCh6 !
                 ! - for octCh3 -
                 p0%octCh3%octNb1 => p0%octCh3 
                 p0%octCh3%octNb2 => p0%octCh4 !
                 p0%octCh3%octNb3 => p0%octCh1 !
                 p0%octCh3%octNb4 => p0%octCh3
                 p0%octCh3%octNb5 => p0%octCh3
                 p0%octCh3%octNb6 => p0%octCh7 !
                 ! - for octCh4 -
                 p0%octCh4%octNb1 => p0%octCh3 !
                 p0%octCh4%octNb2 => p0%octCh4
                 p0%octCh4%octNb3 => p0%octCh2 !
                 p0%octCh4%octNb4 => p0%octCh4
                 p0%octCh4%octNb5 => p0%octCh4
                 p0%octCh4%octNb6 => p0%octCh8 !
                 ! - for octCh5 -
                 p0%octCh5%octNb1 => p0%octCh5   
                 p0%octCh5%octNb2 => p0%octCh6 !
                 p0%octCh5%octNb3 => p0%octCh5
                 p0%octCh5%octNb4 => p0%octCh7 !
                 p0%octCh5%octNb5 => p0%octCh1 !
                 p0%octCh5%octNb6 => p0%octCh5
                 ! - for octCh6 -
                 p0%octCh6%octNb1 => p0%octCh5 !
                 p0%octCh6%octNb2 => p0%octCh6
                 p0%octCh6%octNb3 => p0%octCh6
                 p0%octCh6%octNb4 => p0%octCh8 !
                 p0%octCh6%octNb5 => p0%octCh2 !
                 p0%octCh6%octNb6 => p0%octCh6
                 ! - for octCh7 -
                 p0%octCh7%octNb1 => p0%octCh7 
                 p0%octCh7%octNb2 => p0%octCh8 !
                 p0%octCh7%octNb3 => p0%octCh5 !
                 p0%octCh7%octNb4 => p0%octCh7
                 p0%octCh7%octNb5 => p0%octCh3 !
                 p0%octCh7%octNb6 => p0%octCh7
                 ! - for octCh8 -
                 p0%octCh8%octNb1 => p0%octCh7 !
                 p0%octCh8%octNb2 => p0%octCh8
                 p0%octCh8%octNb3 => p0%octCh6 !
                 p0%octCh8%octNb4 => p0%octCh8
                 p0%octCh8%octNb5 => p0%octCh4 !
                 p0%octCh8%octNb6 => p0%octCh8
                 if(p0%octNb1%iFLG(1)>=1) then
                    p0%octCh1%octNb1 => p0%octNb1%octCh2
                    p0%octCh3%octNb1 => p0%octNb1%octCh4
                    p0%octCh5%octNb1 => p0%octNb1%octCh6
                    p0%octCh7%octNb1 => p0%octNb1%octCh8
                 endif
                 if(p0%octNb2%iFLG(1)>=1) then
                    p0%octCh2%octNb2 => p0%octNb2%octCh1
                    p0%octCh4%octNb2 => p0%octNb2%octCh3
                    p0%octCh6%octNb2 => p0%octNb2%octCh5
                    p0%octCh8%octNb2 => p0%octNb2%octCh7
                 endif
                 if(p0%octNb3%iFLG(1)>=1) then
                    p0%octCh1%octNb3 => p0%octNb3%octCh3
                    p0%octCh2%octNb3 => p0%octNb3%octCh4
                    p0%octCh5%octNb3 => p0%octNb3%octCh7
                    p0%octCh6%octNb3 => p0%octNb3%octCh8
                 endif
                 if(p0%octNb4%iFLG(1)>=1) then
                    p0%octCh3%octNb4 => p0%octNb4%octCh1
                    p0%octCh4%octNb4 => p0%octNb4%octCh2
                    p0%octCh7%octNb4 => p0%octNb4%octCh5
                    p0%octCh8%octNb4 => p0%octNb4%octCh6
                 endif
                 if(p0%octNb5%iFLG(1)>=1) then
                    p0%octCh1%octNb5 => p0%octNb5%octCh5
                    p0%octCh2%octNb5 => p0%octNb5%octCh6
                    p0%octCh3%octNb5 => p0%octNb5%octCh7
                    p0%octCh4%octNb5 => p0%octNb5%octCh8
                 endif
                 if(p0%octNb6%iFLG(1)>=1) then
                    p0%octCh5%octNb6 => p0%octNb6%octCh1
                    p0%octCh6%octNb6 => p0%octNb6%octCh2
                    p0%octCh7%octNb6 => p0%octNb6%octCh3
                    p0%octCh8%octNb6 => p0%octNb6%octCh4
                 endif
              endif
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  do ID=3,4
     if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_meshset,ID,iLv)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           p0 => st_meshset%GMesh(index)
           if(p0%iFLG(1)>0) then
              if(p0%iFLG(1)>=2) then ! This means that the cell is not at the edge.
                 ! - for octNb1 -                   
                 if(p0%octNb1%octN==p0%octN) then
                    p0%octCh1%octNb1 => p0%octCh1 
                    p0%octCh3%octNb1 => p0%octCh3 
                    p0%octCh5%octNb1 => p0%octCh5   
                    p0%octCh7%octNb1 => p0%octCh7 
                 else
                    p0%octCh1%octNb1 => p0%octNb1%octCh2     
                    p0%octCh3%octNb1 => p0%octNb1%octCh4   
                    p0%octCh5%octNb1 => p0%octNb1%octCh6 
                    p0%octCh7%octNb1 => p0%octNb1%octCh8
                 endif
                 ! - for octNb2 -
                 if(p0%octNb2%octN==p0%octN) then
                    p0%octCh2%octNb2 => p0%octCh2
                    p0%octCh4%octNb2 => p0%octCh4
                    p0%octCh6%octNb2 => p0%octCh6
                    p0%octCh8%octNb2 => p0%octCh8
                 else
                    p0%octCh2%octNb2 => p0%octNb2%octCh1
                    p0%octCh4%octNb2 => p0%octNb2%octCh3
                    p0%octCh6%octNb2 => p0%octNb2%octCh5
                    p0%octCh8%octNb2 => p0%octNb2%octCh7
                 endif
                 ! - for octNb3 -
                 if(p0%octNb3%octN==p0%octN) then
                    p0%octCh1%octNb3 => p0%octCh1 
                    p0%octCh2%octNb3 => p0%octCh2
                    p0%octCh5%octNb3 => p0%octCh5
                    p0%octCh6%octNb3 => p0%octCh6
                 else
                    p0%octCh1%octNb3 => p0%octNb3%octCh3
                    p0%octCh2%octNb3 => p0%octNb3%octCh4
                    p0%octCh5%octNb3 => p0%octNb3%octCh7
                    p0%octCh6%octNb3 => p0%octNb3%octCh8
                 endif
                 ! - for octNb4 -
                 if(p0%octNb4%octN==p0%octN) then
                    p0%octCh3%octNb4 => p0%octCh3
                    p0%octCh4%octNb4 => p0%octCh4
                    p0%octCh7%octNb4 => p0%octCh7
                    p0%octCh8%octNb4 => p0%octCh8
                 else
                    p0%octCh3%octNb4 => p0%octNb4%octCh1
                    p0%octCh4%octNb4 => p0%octNb4%octCh2
                    p0%octCh7%octNb4 => p0%octNb4%octCh5
                    p0%octCh8%octNb4 => p0%octNb4%octCh6
                 endif
                 ! - for octNb5 -
                 if(p0%octNb5%octN==p0%octN) then
                    p0%octCh1%octNb5 => p0%octCh1 
                    p0%octCh2%octNb5 => p0%octCh2
                    p0%octCh3%octNb5 => p0%octCh3
                    p0%octCh4%octNb5 => p0%octCh4
                 else
                    p0%octCh1%octNb5 => p0%octNb5%octCh5
                    p0%octCh2%octNb5 => p0%octNb5%octCh6
                    p0%octCh3%octNb5 => p0%octNb5%octCh7
                    p0%octCh4%octNb5 => p0%octNb5%octCh8
                 endif
                 ! - for octNb6 -
                 if(p0%octNb6%octN==p0%octN) then
                    p0%octCh5%octNb6 => p0%octCh5
                    p0%octCh6%octNb6 => p0%octCh6
                    p0%octCh7%octNb6 => p0%octCh7
                    p0%octCh8%octNb6 => p0%octCh8
                 else
                    p0%octCh5%octNb6 => p0%octNb6%octCh1
                    p0%octCh6%octNb6 => p0%octNb6%octCh2
                    p0%octCh7%octNb6 => p0%octNb6%octCh3
                    p0%octCh8%octNb6 => p0%octNb6%octCh4
                 endif
                 ! - for octCh1 -
                 p0%octCh1%octNb2 => p0%octCh2
                 p0%octCh1%octNb4 => p0%octCh3
                 p0%octCh1%octNb6 => p0%octCh5
                 ! - for octCh2 -
                 p0%octCh2%octNb1 => p0%octCh1
                 p0%octCh2%octNb4 => p0%octCh4
                 p0%octCh2%octNb6 => p0%octCh6
                 ! - for octCh3 -
                 p0%octCh3%octNb2 => p0%octCh4
                 p0%octCh3%octNb3 => p0%octCh1
                 p0%octCh3%octNb6 => p0%octCh7
                 ! - for octCh4 -
                 p0%octCh4%octNb1 => p0%octCh3
                 p0%octCh4%octNb3 => p0%octCh2
                 p0%octCh4%octNb6 => p0%octCh8
                 ! - for octCh5 -
                 p0%octCh5%octNb2 => p0%octCh6
                 p0%octCh5%octNb4 => p0%octCh7
                 p0%octCh5%octNb5 => p0%octCh1
                 ! - for octCh6 -
                 p0%octCh6%octNb1 => p0%octCh5           
                 p0%octCh6%octNb4 => p0%octCh8
                 p0%octCh6%octNb5 => p0%octCh2
                 ! - for octCh7 -
                 p0%octCh7%octNb2 => p0%octCh8
                 p0%octCh7%octNb3 => p0%octCh5
                 p0%octCh7%octNb5 => p0%octCh3
                 ! - for octCh8 -
                 p0%octCh8%octNb1 => p0%octCh7
                 p0%octCh8%octNb3 => p0%octCh6
                 p0%octCh8%octNb5 => p0%octCh4
              endif
              if(p0%iFLG(1)==1) then !This is at the edge of overlap region
                 ! - for octCh1 -
                 p0%octCh1%octNb1 => p0%octCh1
                 p0%octCh1%octNb2 => p0%octCh2 !
                 p0%octCh1%octNb3 => p0%octCh1 
                 p0%octCh1%octNb4 => p0%octCh3 !
                 p0%octCh1%octNb5 => p0%octCh1 
                 p0%octCh1%octNb6 => p0%octCh5 !
                 ! - for octCh2 -
                 p0%octCh2%octNb1 => p0%octCh1 !
                 p0%octCh2%octNb2 => p0%octCh2
                 p0%octCh2%octNb3 => p0%octCh2
                 p0%octCh2%octNb4 => p0%octCh4 !
                 p0%octCh2%octNb5 => p0%octCh2
                 p0%octCh2%octNb6 => p0%octCh6 !
                 ! - for octCh3 -
                 p0%octCh3%octNb1 => p0%octCh3 
                 p0%octCh3%octNb2 => p0%octCh4 !
                 p0%octCh3%octNb3 => p0%octCh1 !
                 p0%octCh3%octNb4 => p0%octCh3
                 p0%octCh3%octNb5 => p0%octCh3
                 p0%octCh3%octNb6 => p0%octCh7 !
                 ! - for octCh4 -
                 p0%octCh4%octNb1 => p0%octCh3 !
                 p0%octCh4%octNb2 => p0%octCh4
                 p0%octCh4%octNb3 => p0%octCh2 !
                 p0%octCh4%octNb4 => p0%octCh4
                 p0%octCh4%octNb5 => p0%octCh4
                 p0%octCh4%octNb6 => p0%octCh8 !
                 ! - for octCh5 -
                 p0%octCh5%octNb1 => p0%octCh5   
                 p0%octCh5%octNb2 => p0%octCh6 !
                 p0%octCh5%octNb3 => p0%octCh5
                 p0%octCh5%octNb4 => p0%octCh7 !
                 p0%octCh5%octNb5 => p0%octCh1 !
                 p0%octCh5%octNb6 => p0%octCh5
                 ! - for octCh6 -
                 p0%octCh6%octNb1 => p0%octCh5 !
                 p0%octCh6%octNb2 => p0%octCh6
                 p0%octCh6%octNb3 => p0%octCh6
                 p0%octCh6%octNb4 => p0%octCh8 !
                 p0%octCh6%octNb5 => p0%octCh2 !
                 p0%octCh6%octNb6 => p0%octCh6
                 ! - for octCh7 -
                 p0%octCh7%octNb1 => p0%octCh7 
                 p0%octCh7%octNb2 => p0%octCh8 !
                 p0%octCh7%octNb3 => p0%octCh5 !
                 p0%octCh7%octNb4 => p0%octCh7
                 p0%octCh7%octNb5 => p0%octCh3 !
                 p0%octCh7%octNb6 => p0%octCh7
                 ! - for octCh8 -
                 p0%octCh8%octNb1 => p0%octCh7 !
                 p0%octCh8%octNb2 => p0%octCh8
                 p0%octCh8%octNb3 => p0%octCh6 !
                 p0%octCh8%octNb4 => p0%octCh8
                 p0%octCh8%octNb5 => p0%octCh4 !
                 p0%octCh8%octNb6 => p0%octCh8
                 ! -- In case there are children in octNb1 --
                 if(p0%octNb1%iFLG(1)>=1 .and. p0%octNb1%octN/=p0%octN) then
                    p0%octCh1%octNb1 => p0%octNb1%octCh2
                    p0%octCh3%octNb1 => p0%octNb1%octCh4
                    p0%octCh5%octNb1 => p0%octNb1%octCh6
                    p0%octCh7%octNb1 => p0%octNb1%octCh8
                 endif
                 if(p0%octNb2%iFLG(1)>=1 .and. p0%octNb2%octN/=p0%octN) then !
                    p0%octCh2%octNb2 => p0%octNb2%octCh1
                    p0%octCh4%octNb2 => p0%octNb2%octCh3
                    p0%octCh6%octNb2 => p0%octNb2%octCh5
                    p0%octCh8%octNb2 => p0%octNb2%octCh7
                 endif
                 if(p0%octNb3%iFLG(1)>=1 .and. p0%octNb3%octN/=p0%octN) then
                    p0%octCh1%octNb3 => p0%octNb3%octCh3
                    p0%octCh2%octNb3 => p0%octNb3%octCh4
                    p0%octCh5%octNb3 => p0%octNb3%octCh7
                    p0%octCh6%octNb3 => p0%octNb3%octCh8
                 endif
                 if(p0%octNb4%iFLG(1)>=1 .and. p0%octNb4%octN/=p0%octN) then
                    p0%octCh3%octNb4 => p0%octNb4%octCh1
                    p0%octCh4%octNb4 => p0%octNb4%octCh2
                    p0%octCh7%octNb4 => p0%octNb4%octCh5
                    p0%octCh8%octNb4 => p0%octNb4%octCh6
                 endif
                 if(p0%octNb5%iFLG(1)>=1 .and. p0%octNb5%octN/=p0%octN) then
                    p0%octCh1%octNb5 => p0%octNb5%octCh5
                    p0%octCh2%octNb5 => p0%octNb5%octCh6
                    p0%octCh3%octNb5 => p0%octNb5%octCh7
                    p0%octCh4%octNb5 => p0%octNb5%octCh8
                 endif
                 if(p0%octNb6%iFLG(1)>=1 .and. p0%octNb6%octN/=p0%octN) then
                    p0%octCh5%octNb6 => p0%octNb6%octCh1
                    p0%octCh6%octNb6 => p0%octNb6%octCh2
                    p0%octCh7%octNb6 => p0%octNb6%octCh3
                    p0%octCh8%octNb6 => p0%octNb6%octCh4
                 endif
              endif
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  return
end subroutine ppohAMRFDM_connect_oct
