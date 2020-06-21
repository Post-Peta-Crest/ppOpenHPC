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

subroutine ppohAMRFDM_findCPU(MrtN,CPU,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer,intent(in)::MrtN
  integer,intent(out)::CPU
  integer::index,sID,eID,midian,loopnum
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  CPU=-1
  if(MrtN<=st_comm_info%Mn2CPU(1)) then
     CPU=0
  else
     sID=1; eID=st_comm_info%nprocs; midian=sID+(eID-sID)/2
     loopnum=int(log(dble(st_comm_info%nprocs))/log(2.0d0))+1
     do index=1,loopnum
        if(MrtN>=st_comm_info%Mn2CPU(midian)+1.and.&
             MrtN<=st_comm_info%Mn2CPU(midian+1))then
           CPU=midian
        else if(MrtN>st_comm_info%Mn2CPU(midian+1))then
           sID=midian
           eID=eID
           midian=sID+(eID-sID)/2
        else if(MrtN<st_comm_info%Mn2CPU(midian)+1)then
           sID=sID
           eID=midian
           midian=sID+(eID-sID)/2
        endif
     enddo
  endif
  return
end subroutine ppohAMRFDM_findCPU
