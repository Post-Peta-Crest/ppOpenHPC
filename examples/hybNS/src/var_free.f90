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
!C*** VAR_FREE
!C***
!C
!C    FREE-STREAM var's
!C
      subroutine VAR_FREE

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)

      GAM   = 1.40d0
      GM1   = GAM - 1.0d0

      Rinf= 1.00d0
      Pinf= 1.d0/GAM
      Cinf= 1.0d0

      Cinf0= dsqrt(GAM*Pinf/Rinf0)
      REYN = 1.d0*Cinf0/VISKinf0

      Uinf= Uinf0 / Cinf0
      Vinf= Vinf0 / Cinf0
      Winf= Winf0 / Cinf0

      Qinf= dsqrt(Uinf**2+Vinf**2+Winf**2)

      if (my_rank.eq.0) write (*,1000) Cinf0, Qinf, REYN
 1000 format('Cinf', 1pe12.3, '  Mach #', 1pe12.3,'  REYN', 1pe12.3)

      if ( Qinf.ge.1.0d0 ) then
!        SUBSONIC= .false.
       else
!        SUBSONIC= .true.
      endif

      return
      end


