!!====================================================================!!
!!                                                                    !!
!!   Software Name : hybNS on ppOpen-APPL/FVM                         !!
!!         Version : 0.3.0                                            !!
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
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

      module HYBRID
      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      character(len=ppohFVM_name_len)  :: GRIDFIL, RESTFIL, RESIDFIL
      character(len=ppohFVM_name_len) :: INPFIL, UCDFIL, UCDdum
      character(len=ppohFVM_name_len) :: HEADgrid, HEADrest, HEADucd
      logical            :: RESTART, STEADY
!C
!C-- MPI etc.
      integer:: PETOT, my_rank, PEsmpTOT
             
!C
!C-- TIME STEPs & CONVERGENCE PARAMETERs
      real(kind=ppohFVM_kreal) :: OMEGA, TIMEMAX, DTMAX, DTMIN, TIME
      real(kind=ppohFVM_kreal), dimension(:), allocatable :: DTNOD

      integer(kind=ppohFVM_kint) :: iter, ITER_OLD, MAXITER, BCwalID
      integer(kind=ppohFVM_kint) :: NFREQ_HIS, NFREQ_RES

!C
!C-- FLOW PROPERTYs
      real(kind=ppohFVM_kreal) :: GAM, GM1, QINF, PINF, UINF, VINF, WINF, CINF, &
     &                    RINF, QINF0, PINF0, VISKINF0, UINF0, VINF0,           &
     &                    WINF0, CINF0, RINF0, SIGMA2, SIGMA4,                  &
     &                    REYN, PRL, PRT, C2TREF, XLENGTH

!C
!C-- CELL METRICs & POINTERs
      integer(kind=ppohFVM_kint), pointer::                             &
     &  NODgrpITEM (:), CELgrpITEM (:), SUFgrpITEM (:,:),               &
     &  NODgrpSTACK(:), CELgrpSTACK(:), SUFgrpSTACK(:),                 &
     &  globalNOD(:), globalCEL(:)

      integer(kind=ppohFVM_kint ), pointer:: ICELindex(:),ICELptr(:),ICELTYP(:)
      integer(kind=ppohFVM_kint ), pointer:: IDcell(:,:), IDnode(:,:) 
      integer(kind=ppohFVM_kint ), pointer:: intCELlist(:)
      real   (kind=ppohFVM_kreal), pointer:: XYZ(:,:)
      integer(kind=ppohFVM_kint) , pointer:: intCELflag(:)

      real(kind=ppohFVM_kreal), pointer:: SAREA  (:,:)
      real(kind=ppohFVM_kreal), pointer:: VOLNOD (:)
      real(kind=ppohFVM_kreal), pointer:: VOLCEL (:)

      integer(kind=ppohFVM_kint) :: ICELTOT, ICELTOTint

      integer(kind=ppohFVM_kint ), dimension(:), allocatable ::  NODCEL
      integer(kind=ppohFVM_kint ), dimension(:), allocatable :: REVNEIB

      integer(kind=ppohFVM_kint ) :: CELgrpTOT, NODgrpTOT, SUFgrpTOT,   &
     &                       NODTOT, NODTOTint, NODTOTglobal

      character(len=ppohFVM_name_len),pointer::                         &
     &              NODgrpNAME(:), CELgrpNAME(:), SUFgrpNAME(:)


!C
!C-- PARAMETERs
      real(kind=ppohFVM_kreal) :: O3rd, O6th

!C
!C-- TETRAHEDERAL
      integer(kind=ppohFVM_kint) :: ICELTOTtetra
      integer(kind=ppohFVM_kint), pointer:: IDtetra(:)

!C
!C-- PRISMs
      integer(kind=ppohFVM_kint) :: ICELTOTprism
      integer(kind=ppohFVM_kint), pointer:: IDprism(:)

!C
!C-- BOUNDARY POINTERs
      integer(kind=ppohFVM_kint) :: IBFFDTOT, IBWALTOT
      integer(kind=ppohFVM_kint), dimension(:  ), allocatable :: IBFFD
      integer(kind=ppohFVM_kint), dimension(:,:), allocatable :: IBWAL
      real   (kind=ppohFVM_kreal),dimension(:,:), allocatable :: VELGRID

!C
!C-- ADAPTATION POINTERs
      integer(kind=ppohFVM_kint), pointer :: WhenNODE(:), WhenCELL(:)
      integer(kind=ppohFVM_kint), pointer :: ADAPT_LEV(:), ADAPT_TYP(:)
      integer(kind=ppohFVM_kint), pointer :: ADAPT_PAR_type(:)
      integer(kind=ppohFVM_kint), pointer :: ADAPT_PAR(:,:), ADAPT_CHI(:,:)
      integer(kind=ppohFVM_kint), pointer :: ADAPT_CHI_index(:)

      integer(kind=ppohFVM_kint) :: CGlevel, ADAPlevel

!C
!C-- ACTIVE POINTERs
      integer(kind=ppohFVM_kint) :: ACTtetraTOT, ACTprismTOT
      integer(kind=ppohFVM_kint),pointer:: ACTtetra(:), ACTprism(:)

!C
!C-- EDGE METRICs & POINTERs
      real(kind=ppohFVM_kreal), pointer:: EAREA (:,:)
      real(kind=ppohFVM_kreal), pointer:: VOLEDG(:)

      integer(kind=ppohFVM_kint) :: IEDGTOT, ACTedgeTOT, COLORedgeTOT
      integer(kind=ppohFVM_kint), pointer :: IEDGNOD(:,:)
      integer(kind=ppohFVM_kint), pointer :: ACTedge(:)

      integer(kind=ppohFVM_kint), pointer :: COLORedgeINDEX(:)
      integer(kind=ppohFVM_kint), pointer :: COLORedgeITEM (:)

!C
!C-- VARIABLEs
      real   (kind=ppohFVM_kreal),dimension(:,:), allocatable :: U, DU
      real   (kind=ppohFVM_kreal),dimension(:  ), allocatable :: P
      real   (kind=ppohFVM_kreal),dimension(:  ), allocatable :: VISCL, VISCT
      real   (kind=ppohFVM_kreal),dimension(:,:), allocatable :: FCV, GCV, HCV, XCV
      real   (kind=ppohFVM_kreal),dimension(:,:), allocatable :: ZCV
      real   (kind=ppohFVM_kreal),dimension(:,:), allocatable :: PU

      end module HYBRID


