!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohDEM                                          !!
!!         Version : 0.2.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohDEM.                                  !!
!!     ppohDEM is a free software, you can use it under the terms     !!
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
!!   Copyright (c) 2014 <Miki Yamamoto Matsuo, JAMSTEC                !!
!!                       mikiy(at)jamstec.go.jp                       !!
!!                                                                    !!
!!====================================================================!!

      integer, parameter   ::  kint  = 4 
      integer, parameter   ::  kreal = 8
      integer, parameter   ::  ppohDEM_name_length = 80
	  
	  real(4), parameter   ::  FLT_MAX=+3.402823E+38
	  real(4), parameter   ::  FLT_MIN=-3.402823E+38
!	  real(8), parameter   ::  DBL_MAX=+1.797693134862316D+308
!	  real(8), parameter   ::  DBL_MIN=-1.797693134862316D+308

!  gfortran error: Real constant overflows
!
!  real(8), parameter   ::  DBL_MAX=+1.797693134862316D+308
!  Error: Real constant overflows its kind at (1)

	  real(8), parameter   ::  DBL_MAX=+1.79769313486231D+308
	  real(8), parameter   ::  DBL_MIN=-1.79769313486231D+308
