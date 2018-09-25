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

      subroutine ppohFVM_part_NEIB_PE
      use m_ppohFVM_part_partitioner

      do ip= 1, NP
        NEIBPETOT(ip)= 0
      do is= ISTACKC(ip-1)+1, ISTACKC(ip)
        icel= NPCID(is)
      do  k= 1, NODELM(icel)
        in= ICELNOD(icel,k)
        ig= IGROUP (in)

        if (ig.ne.ip) call ppohFVM_part_FIND_NEIBPE (ig, ip)
      enddo
      enddo
      enddo
      
      write ( *,'(/," PE/NEIB-PE#    NEIB-PEs")')
      write (21,'(/," PE/NEIB-PE#    NEIB-PEs")')
      do ip= 1, NP
        write ( *,'(i3,i4,5x, 31i4)') ip, NEIBPETOT(ip),                &
     &           (NEIBPE(ip,k),k=1,NEIBPETOT(ip))
        write (21,'(i3,i4,5x, 31i4)') ip, NEIBPETOT(ip),                &
     &           (NEIBPE(ip,k),k=1,NEIBPETOT(ip))
      enddo

      return
      end




      subroutine ppohFVM_part_FIND_NEIBPE (ig, ip)
      use m_ppohFVM_part_partitioner

      do inei= 1, NEIBPETOT(ip)
        if (ig.eq.NEIBPE(ip,inei)) return
      enddo

                NEIBPETOT(ip) = NEIBPETOT(ip) + 1
      NEIBPE(ip,NEIBPETOT(ip))= ig

      return
      end
