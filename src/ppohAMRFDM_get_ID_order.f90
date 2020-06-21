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

subroutine ppohAMRFDM_get_ID_order(st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::iLv,index,iFs,iFe,ID
  type(st_ppohAMRFDM_octset),pointer::p0
  ! -- set flag region --
  iFs=0; iFe=st_param%nfg-1
  ! -- initialize --
  st_param%nID(:)=0
  do iLv=0,st_param%LvMax
     if(allocated(st_meshset%getID(iLv)%IDsq)) then
        deallocate(st_meshset%getID(iLv)%IDsq)
     endif
  enddo     
  ! -- get order --
  do iLv=0,st_param%LvMax
     ! -- get information for IDs --
     if(st_meshset%MinID(1,iLv)>=st_meshset%MaxID(1,iLv)) then
        st_param%nID(iLv)=0
     else
        ID=0
        ! -- count ID in each Lv --
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_meshset,iLv,iFs,iFe) &
        !$omp& reduction(+:ID)
        do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
           p0 => st_meshset%Mesh(index)
           if((p0%iFLG(1)>=iFs).and.(p0%iFLG(1)<=iFe)) then
              ID=ID+1
           endif
        enddo
        !$omp end parallel do
        st_param%nID(iLv)=ID
        ! -- allocate ID buffer --
        allocate(st_meshset%getID(iLv)%IDsq(1:st_param%nID(iLv)))
        ! -- get ID --
        ID=0
        do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
           p0 => st_meshset%Mesh(index)
           if((p0%iFLG(1)>=iFs).and.(p0%iFLG(1)<=iFe)) then
              ID=ID+1
              st_meshset%getID(iLv)%IDsq(ID)=index
           endif
        enddo
     endif
  enddo
  return
end subroutine ppohAMRFDM_get_ID_order
