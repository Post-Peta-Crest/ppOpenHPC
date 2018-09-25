!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohVIS_base                                     !!
!!         Version : 0.2.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohFVM.                                  !!
!!     ppohFVM is a free software, you can use it under the terms     !!
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
!!   Copyright (c) 2013 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!
module ppohVIS_BASE_Lib_F
  use ppohVIS_BASE_Config_F

  implicit none
  private
  public :: ppohVIS_BASE_GetDistFileName
contains

subroutine ppohVIS_BASE_GetDistFileName(cSrc, iRank, cDst)
  character(len=*),    intent(in)    :: cSrc
  integer(kind=IKind), intent(in)    :: iRank
  character(len=*),    intent(inout) :: cDst

  character(len=80) :: cRank

  cRank = ""
  write(unit=cRank, fmt=*) iRank

  cDst = ""
  cDst = trim(adjustl(cSrc))//"."//trim(adjustl(cRank))
end subroutine ppohVIS_BASE_GetDistFileName

end module ppohVIS_BASE_Lib_F