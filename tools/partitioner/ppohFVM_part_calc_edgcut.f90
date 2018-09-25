!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM-Tool/Partitioner                         !!
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

      subroutine ppohFVM_part_CALC_EDGCUT
      use m_ppohFVM_part_partitioner
!C
!C-- calc. EDGECUT

      IEDGCUT= 0
      do ie= 1, IEDGTOT
        in1= IEDGNOD(ie,1)
        in2= IEDGNOD(ie,2)
        ig1= IGROUP(in1)
        ig2= IGROUP(in2)
        if (ig1.ne.ig2) IEDGCUT= IEDGCUT + 1
      enddo

      write ( *,'(/,"TOTAL EDGE     #   ", i8)') IEDGTOT
      write ( *,'(  "TOTAL EDGE CUT #   ", i8)') IEDGCUT
      write (21,'(/,"TOTAL EDGE     #   ", i8)') IEDGTOT
      write (21,'(  "TOTAL EDGE CUT #   ", i8)') IEDGCUT

      return
      end



