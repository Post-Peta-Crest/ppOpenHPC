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

subroutine ppohAMRFDM_set_buffer_ave(iLv,iF1,iF2,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv,iF1,iF2
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::index,ID
  type(st_ppohAMRFDM_octset),pointer::p0
  do ID=1,4
     if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_meshset,ID,iLv,iF1,iF2)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           if(ID==1.or.ID==2) then
              p0 => st_meshset%Mesh(index)
           else if(ID==3.or.ID==4) then
              p0 => st_meshset%GMesh(index)
           endif
           if(p0%iFLG(1)==iF1) then
              p0%iFLG(1)=iF2
              if(p0%octNb1%iFLG(1)<iF1) p0%iFLG(1)=iF1
              if(p0%octNb2%iFLG(1)<iF1) p0%iFLG(1)=iF1
              if(p0%octNb3%iFLG(1)<iF1) p0%iFLG(1)=iF1
              if(p0%octNb4%iFLG(1)<iF1) p0%iFLG(1)=iF1
              if(p0%octNb5%iFLG(1)<iF1) p0%iFLG(1)=iF1
              if(p0%octNb6%iFLG(1)<iF1) p0%iFLG(1)=iF1
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  return
end subroutine ppohAMRFDM_set_buffer_ave
