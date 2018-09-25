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
module ppohVIS_BASE_ControlFile_F
  use ppohVIS_BASE_Config_F
  use ppohVIS_BASE_Control_F

  implicit none
  private
  public :: PPOHVIS_BASE_GETCONTROL
contains

subroutine PPOHVIS_BASE_GETCONTROL(cFileName, pControl, iErr)
  character(len=PPOHVIS_BASE_FILE_NAME_LEN), intent(in)    :: cFileName
  type(ppohVIS_BASE_stControl),              intent(out)   :: pControl
  integer(kind=IKind),                       intent(inout) :: iErr

  integer(kind=IKind) :: iRc


  !... initialize
  call ppohvis_base_get_control_init_if(iRc)
  if(iRc /= 0) goto 900

  !... read control file by 'C' function
  call ppohvis_base_get_control_if(cFileName, iRc)
  if(iRc /= 0) goto 900

  !... copy variables from 'C' region to 'FORTRAN' region
  call ppohvis_base_get_control_get_refinecontrol_if(                          &
                                             pControl%Refine%AvailableMemory,  &
                                             pControl%Refine%MaxRefineLevel,   &
                                             pControl%Refine%MaxVoxelCount,    &
                                             iRc)
  if(iRc /= 0) goto 900

  call ppohvis_base_get_control_get_simplecontrol_if(                          &
                                             pControl%Simple%ReductionRate,    &
                                             iRc)
  if(iRc /= 0) goto 900

  !... finalize
  call ppohvis_base_get_control_finalize_if(iRc)
  if(iRc /= 0) goto 900


  iErr = 0
  return

900 continue
  iErr = iRc
  return
end subroutine PPOHVIS_BASE_GETCONTROL

end module ppohVIS_BASE_ControlFile_F
