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

subroutine ppohAMRFDM_average(iLv,iFs,iFe,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv,iFs,iFe
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::index,k,is,ie
  type(st_ppohAMRFDM_octset),pointer::p0
  if(iLv==st_param%LvMax) return
  if(st_meshset%MaxID(1,iLv)<=st_meshset%MinID(1,iLv).and.&
       st_meshset%MaxID(3,iLv)<=st_meshset%MinID(3,iLv)) return 
  is=1; ie=st_param%npy
  if(st_meshset%MaxID(1,iLv)>st_meshset%MinID(1,iLv)) then
     !$omp parallel do default(none) &
     !$omp& private(index,p0,k) &
     !$omp& shared(st_meshset,iLv,iFs,iFe,is,ie)
     do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
        p0 => st_meshset%Mesh(index)
        if((p0%iFLG(1)>=iFs).and.(p0%iFLG(1)<=iFe)) then
           do k=is,ie
              p0%F(k)=(p0%octCh1%F(k)+p0%octCh2%F(k) &
                   +p0%octCh3%F(k)+p0%octCh4%F(k) &
                   +p0%octCh5%F(k)+p0%octCh6%F(k) &
                   +p0%octCh7%F(k)+p0%octCh8%F(k))*0.125d0
           enddo
        endif
     enddo
     !$omp end parallel do
  endif
  if(st_meshset%MaxID(3,iLv)>st_meshset%MinID(3,iLv))then
     !$omp parallel do default(none) &
     !$omp& private(index,p0,k) &
     !$omp& shared(st_meshset,iLv,iFs,iFe,is,ie)
     do index=st_meshset%MinID(3,iLv),st_meshset%MaxID(3,iLv)
        p0 => st_meshset%GMesh(index)
        if((p0%iFLG(1)>=iFs).and.(p0%iFLG(1)<=iFe)) then
           do k=is,ie 
              p0%F(k)=(p0%octCh1%F(k)+p0%octCh2%F(k) &
                   +p0%octCh3%F(k)+p0%octCh4%F(k) &
                   +p0%octCh5%F(k)+p0%octCh6%F(k) &
                   +p0%octCh7%F(k)+p0%octCh8%F(k))*0.125d0
           enddo
        endif
     enddo
     !$omp end parallel do
  endif
  return
end subroutine ppohAMRFDM_average
