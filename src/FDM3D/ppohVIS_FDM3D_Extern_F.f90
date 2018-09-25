!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohVIS_FDM3D                                    !!
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
module ppohVIS_FDM3D_Extern_F
  use ppohVIS_BASE_Config_F
  use ppohVIS_BASE_Control_F
  use ppohVIS_BASE_ControlFile_F
  use ppohVIS_BASE_Mesh_F
  use ppohVIS_FDM3D_StrGrid_F
  use ppohVIS_BASE_Result_F


  private
  public :: PPOHVIS_FDM3D_INIT
  public :: PPOHVIS_FDM3D_GETCONTROL
  public :: PPOHVIS_FDM3D_SETSTRGRID
  public :: PPOHVIS_FDM3D_VISUALIZE
  public :: PPOHVIS_FDM3D_FINALIZE
contains


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!
! initialization
!
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
subroutine PPOHVIS_FDM3D_INIT(comm, iErr)
  include "mpif.h"
  integer(kind=IKind), intent(in)    :: comm
  integer(kind=IKind), intent(inout) :: iErr

  integer(kind=IKind) :: iRc


  call ppohvis_fdm3d_init_init_if(iRc)
  if(iRc /= 0) goto 900

  call ppohvis_fdm3d_init_if(comm, iRc)
  if(iRc /= 0) goto 900

  call ppohvis_fdm3d_init_finalize_if(iRc)
  if(iRc /= 0) goto 900


  iErr = 0
  return

900 continue
  iErr = iRc
  return
end subroutine PPOHVIS_FDM3D_INIT


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!
! get control
!
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
subroutine PPOHVIS_FDM3D_GETCONTROL(cFileName, pControl, iErr)
  character(len=PPOHVIS_BASE_FILE_NAME_LEN), intent(in)    :: cFileName
  type(ppohVIS_BASE_stControl),              intent(out)   :: pControl
  integer(kind=IKind),                       intent(inout) :: iErr

  call PPOHVIS_BASE_GETCONTROL(cFileName, pControl, iErr)

  return
end subroutine PPOHVIS_FDM3D_GETCONTROL


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!
! set mesh (dummy for 3DFDM)
!
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
subroutine PPOHVIS_FDM3D_SETMESH(pMesh, iErr)
  type(ppohVIS_BASE_stMesh), pointer       :: pMesh
  integer(kind=IKind),        intent(inout) :: iErr

  iErr = 0
  return
end subroutine PPOHVIS_FDM3D_SETMESH


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!
! set structure grid
!
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
subroutine PPOHVIS_FDM3D_SETSTRGRID(pGrid, iErr)
  type(ppohVIS_FDM3D_stStrGrid), intent(in)  :: pGrid
  integer(kind=IKind),           intent(out) :: iErr

  integer(kind=IKind) :: iRc


  !... initialize
  call ppohvis_fdm3d_set_strgrid_init_if(iRc)
  if(iRc /= 0) goto 900

  !... copy variables from 'FORTRAN' region to 'C' region
  call ppohvis_fdm3d_set_strgrid_set_strgrid_if(                               &
                              pGrid%NumX, pGrid%NumY, pGrid%NumZ,              &
                              pGrid%DeltaX, pGrid%DeltaY, pGrid%DeltaZ,        &
                              pGrid%OriginX, pGrid%OriginY, pGrid%OriginZ, iRc)
  if(iRc /= 0) goto 900

  !... execute 'C' function
  call ppohvis_fdm3d_set_strgrid_if(iRc)
  if(iRc /= 0) goto 900

  !... finalize
  call ppohvis_fdm3d_set_strgrid_finalize_if(iRc)
  if(iRc /= 0) goto 900

  iErr = 0
  return

900 continue
  iErr = iRc
  return
end subroutine PPOHVIS_FDM3D_SETSTRGRID


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!
! visualization
!
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
subroutine PPOHVIS_FDM3D_VISUALIZE(pResultNode, pResultElem, pControl,         &
                                   cFileHeader, iStep, iErr)
  type(ppohVIS_BASE_stResultCollection), intent(in)    :: pResultNode
  type(ppohVIS_BASE_stResultCollection), intent(in)    :: pResultElem
  type(ppohVIS_BASE_stControl),          intent(in)    :: pControl
  character(len=ppohVIS_BASE_LABEL_LEN), intent(in)    :: cFileHeader
  integer(kind=IKind),                   intent(in)    :: iStep
  integer(kind=IKind),                   intent(inout) :: iErr

  integer(kind=IKind) :: ResultNodeCount, ResultElemCount, iRc


  !... initialize
  call ppohvis_fdm3d_visualize_init_if(iRc)
  if(iRc /= 0) goto 900

  !... copy node result info. from 'FORTRAN' region to 'C' region
!  if(.NOT. ALLOCATED(pResultNode%Results)) then
!    ResultNodeCount = 0
!  else
    ResultNodeCount = pResultNode%ListCount
!  endif

  call ppohvis_fdm3d_visualize_set_resultcollection_listcount_if(              &
                                 ppohVIS_BASE_ResultNode, ResultNodeCount, iRc)
  if(iRc /= 0) goto 900

  do i= 1, ResultNodeCount
    call ppohvis_fdm3d_visualize_set_resultcollection_result_if(               &
                                         ppohVIS_BASE_ResultNode, i,           &
                                         pResultNode%Results(i)%ItemCount,     &
                                         pResultNode%Results(i)%FreedomCount,  &
                                         pResultNode%Results(i)%Label,         &
                                         pResultNode%Results(i)%Value,         &
                                         iRc)
    if(iRc /= 0) goto 900
  enddo

  !... copy element result info. from 'FORTRAN' region to 'C' region
!  if(.NOT. ALLOCATED(pResultElem%Results)) then
!    ResultElemCount = 0
!  else
    ResultElemCount = pResultElem%ListCount
!  endif

  call ppohvis_fdm3d_visualize_set_resultcollection_listcount_if(              &
                             ppohVIS_BASE_ResultElement, ResultElemCount, iRc)
  if(iRc /= 0) goto 900

  do i= 1, ResultElemCount
    call ppohvis_fdm3d_visualize_set_resultcollection_result_if(               &
                                         ppohVIS_BASE_ResultElement, i,        &
                                         pResultElem%Results(i)%ItemCount,     &
                                         pResultElem%Results(i)%FreedomCount,  &
                                         pResultElem%Results(i)%Label,         &
                                         pResultElem%Results(i)%Value,         &
                                         iRc)
    if(iRc /= 0) goto 900
  enddo

  !... copy control info. from 'FORTRAN' region to 'C' region
  call ppohvis_fdm3d_visualize_set_refinecontrol_if(                           &
                                             pControl%Refine%AvailableMemory,  &
                                             pControl%Refine%MaxRefineLevel,   &
                                             pControl%Refine%MaxVoxelCount,    &
                                             iRc)
  if(iRc /= 0) goto 900

  !... execute 'C' function
  call ppohvis_fdm3d_visualize_if(cFileHeader, iStep, iRc)
  if(iRc /= 0) goto 900

  !... finalize
  call ppohvis_fdm3d_visualize_finalize_if(iRc)
  if(iRc /= 0) goto 900


  iErr = 0
  return

900 continue
  iErr = iRc
  return
end subroutine PPOHVIS_FDM3D_VISUALIZE


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!
! finalization
!
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
subroutine PPOHVIS_FDM3D_FINALIZE(iErr)
  integer(kind=IKind), intent(inout) :: iErr

  integer(kind=IKind) :: iRc

  call ppohvis_fdm3d_finalize_init_if(iRc)
  if(iRc /= 0) goto 900

  call ppohvis_fdm3d_finalize_if(iRc)
  if(iRc /= 0) goto 900

  call ppohvis_fdm3d_finalize_finalize_if(iRc)
  if(iRc /= 0) goto 900


  iErr = 0
  return

900 continue
  iErr = iRc
  return
end subroutine PPOHVIS_FDM3D_FINALIZE

end module ppohVIS_FDM3D_Extern_F

