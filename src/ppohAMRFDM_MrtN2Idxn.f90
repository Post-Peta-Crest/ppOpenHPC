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

subroutine ppohAMRFDM_MrtN2Idxn(MrtN,iPOS)
  implicit none
  ! - global variables -
  integer,intent(in)::MrtN
  integer,intent(out)::iPOS(1:3)
  ! - local variables -
  integer,parameter::max=10
  integer::Mn,a,b,c
  integer::nx(1:max),ny(1:max),nz(1:max)
  integer::index,ix,iy,iz
  nx=0; ny=0; nz=0
  ix=0; iy=0; iz=0
  Mn=MrtN-1
  do index=max,1,-1
     a=2**(3*index-1); b=2**(3*index-2); c=2**(3*index-3)
     nz(index)=Mn/a; Mn=Mn-nz(index)*a
     ny(index)=Mn/b; Mn=Mn-ny(index)*b
     nx(index)=Mn/c; Mn=Mn-nx(index)*c
  enddo
  do index=max,1,-1
     a=2**(index-1)
     ix=ix+nx(index)*a; iy=iy+ny(index)*a; iz=iz+nz(index)*a
  enddo
  iPOS(1)=ix+1; iPOS(2)=iy+1; iPOS(3)=iz+1
  return
end subroutine PpohAMRFDM_MrtN2Idxn
