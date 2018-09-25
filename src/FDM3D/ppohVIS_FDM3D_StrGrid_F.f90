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
module ppohVIS_FDM3D_StrGrid_F
  use ppohVIS_BASE_Config_F

  implicit none
  private

  type, public :: ppohVIS_FDM3D_stStrGrid
    integer(kind=IKind) :: NumX
    integer(kind=IKind) :: NumY
    integer(kind=IKind) :: NumZ
    real(kind=RKind)    :: DeltaX
    real(kind=RKind)    :: DeltaY
    real(kind=RKind)    :: DeltaZ
    real(kind=RKind)    :: OriginX
    real(kind=RKind)    :: OriginY
    real(kind=RKind)    :: OriginZ
  end type ppohVIS_FDM3D_stStrGrid

end module ppohVIS_FDM3D_StrGrid_F
