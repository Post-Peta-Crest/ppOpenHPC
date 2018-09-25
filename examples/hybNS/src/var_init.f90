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

!C
!C***
!C*** VAR_INIT
!C***
!C
!C    init. VARs & ARRAYs
!C
      subroutine VAR_INIT

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      OMEGA  = 0.d0

      ITER    = 0
      ITER_OLD= 0

      MAXITER = 0

      GAM   = 0.d0
      GM1   = 0.d0

      QINF  = 0.d0
      PINF  = 0.d0
      UINF  = 0.d0
      VINF  = 0.d0
      WINF  = 0.d0
      CINF  = 0.d0

      SIGMA2 = 0.d0
      SIGMA4 = 0.d0

      REYN  = 0.d0
      PRL   = 0.d0
      PRT   = 0.d0
      C2TREF= 0.d0

       NODTOT  = 0 
      ICELTOT  = 0
      IEDGTOT  = 0
      ILAYTOT  = 0
      IFACTOT  = 0 

      IBNODTOT = 0 
      IBEDGTOT = 0

      IBINL_TOT= 0
      IBOUT_TOT= 0
      IBTOP_TOT= 0
      IBBOT_TOT= 0
      IBOBJ_TOT= 0

      IBYMIN_TOT= 0
      IBYMAX_TOT= 0

      ACTtetraTOT= 0
      ACTprismTOT= 0

      return
      end




