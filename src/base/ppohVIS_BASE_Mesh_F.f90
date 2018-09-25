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
module ppohVIS_BASE_Mesh_F
  use ppohVIS_BASE_Config_F


  type, public :: ppohVIS_BASE_stMeshNode
    integer(kind=IKind)                            :: Count
    integer(kind=IKind), dimension(:), allocatable :: ID
    real(kind=RKind),    dimension(:), allocatable :: Coords
  end type ppohVIS_BASE_stMeshNode

  integer(kind=IKind), parameter :: ppohVIS_BASE_Line2   = 0
  integer(kind=IKind), parameter :: ppohVIS_BASE_Line3   = 1
  integer(kind=IKind), parameter :: ppohVIS_BASE_Tria3   = 2
  integer(kind=IKind), parameter :: ppohVIS_BASE_Tria6   = 3
  integer(kind=IKind), parameter :: ppohVIS_BASE_Quad4   = 4
  integer(kind=IKind), parameter :: ppohVIS_BASE_Quad8   = 5
  integer(kind=IKind), parameter :: ppohVIS_BASE_Tetra4  = 6
  integer(kind=IKind), parameter :: ppohVIS_BASE_Tetra10 = 7
  integer(kind=IKind), parameter :: ppohVIS_BASE_Penta6  = 8
  integer(kind=IKind), parameter :: ppohVIS_BASE_Penta15 = 9
  integer(kind=IKind), parameter :: ppohVIS_BASE_Hexa8   = 10
  integer(kind=IKind), parameter :: ppohVIS_BASE_Hexa20  = 11
  integer(kind=IKind), parameter :: ppohVIS_BASE_Unknown = 12

  type, public :: ppohVIS_BASE_stMeshElement
    integer(kind=IKind)                            :: Count
    integer(kind=IKind), dimension(:), allocatable :: ID
    integer(kind=IKind), dimension(:), allocatable :: Topology
    integer(kind=IKind), dimension(:), allocatable :: NodeIndex
    integer(kind=IKind), dimension(:), allocatable :: Node
    real(kind=RKind),    dimension(:), allocatable :: Gravity
  end type ppohVIS_BASE_stMeshElement

  type, public :: ppohVIS_BASE_stMesh
    type(ppohVIS_BASE_stMeshNode),    pointer :: Node
    type(ppohVIS_BASE_stMeshElement), pointer :: Element
  end type ppohVIS_BASE_stMesh

end module ppohVIS_BASE_Mesh_F

