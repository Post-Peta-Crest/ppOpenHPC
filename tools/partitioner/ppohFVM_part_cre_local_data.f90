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

      subroutine ppohFVM_part_CRE_LOCAL_DATA
      use m_ppohFVM_part_partitioner


      N2n= 2 * max (IELMTOT, N)
      N2c= 2 * max (IELMTOT, N)
      allocate (NPNID(N2n))
      allocate (NPCID(N2c))

      ISTACKN(0)= 0
      ISTACKC(0)= 0

  100 continue
      do ip= 1, NP
        do icel= 1, IELMTOT
          ISTACK(icel)= 0
        enddo

        do icel= 1, IELMTOT
        do    k= 1, NODELM(icel)
          in= ICELNOD(icel,k)
          ig= IGROUP (in)

          if (ig.eq.ip) ISTACK(icel)= 1
        enddo
        enddo

        icou= 0
        do icel= 1, IELMTOT
          if (ISTACK(icel).eq.1) then
            icou= icou + 1
              is= ISTACKC(ip-1) + icou
              if (is.gt.N2c) then
                deallocate (NPCID)
                N2c= N2c * 11/10 + 1
                  allocate (NPCID(N2c))
                goto 100
              endif
            ISTACKC(ip)= is
              NPC  (ip)= icou
              NPCID(is)= icel
          endif
        enddo
      enddo

      do ip= 1, NP
        NPN(ip)= 0
      enddo

      do i= 1, N
        ig= IGROUP(i)
        NPN(ig)= NPN(ig) + 1
      enddo

      do ip= 1, NP
        ISTACKN(ip)= ISTACKN(ip-1) + NPN(ip)
        NPN(ip)= 0
      enddo

      do i= 1, N
        ip = IGROUP(i)
        icou= NPN(ip) + 1
        is = ISTACKN(ip-1) + icou
              if (is.gt.N2n) then
                deallocate (NPNID)
                N2n= N2n * 11/10 + 1
                  allocate (NPCID(N2n))
                goto 100
              endif
          NPN(ip)= icou
        NPNID(is)= i
      enddo

      MAXN= NPN(1)
      MINN= NPN(1)
      MAXC= NPC(1)
      MINC= NPC(1)
      
      do ip= 2, NP
        MAXN= max (MAXN,NPN(ip))
        MINN= min (MINN,NPN(ip))
        MAXC= max (MAXC,NPC(ip))
        MINC= min (MINC,NPC(ip))
      enddo

      write ( *,'(/,"TOTAL NODE     #   ", i8)') N
      write ( *,'(  "TOTAL CELL     #   ", i8)') IELMTOT
      write ( *,'(/," PE    NODE#   CELL#")')
      write (21,'(/,"TOTAL NODE     #   ", i8)') N
      write (21,'(  "TOTAL CELL     #   ", i8)') IELMTOT
      write (21,'(/," PE    NODE#   CELL#")')

      do ip= 1, NP
        write ( *,'(i3,5i8)') ip, NPN(ip), NPC(ip)
        write (21,'(i3,5i8)') ip, NPN(ip), NPC(ip)
      enddo

      write ( *,'(/,"MAX.node/PE        ", i8)') MAXN
      write ( *,'(  "MIN.node/PE        ", i8)') MINN
      write ( *,'(  "MAX.cell/PE        ", i8)') MAXC
      write ( *,'(  "MIN.cell/PE        ", i8)') MINC
      write (21,'(/,"MAX.node/PE        ", i8)') MAXN
      write (21,'(  "MIN.node/PE        ", i8)') MINN
      write (21,'(  "MAX.cell/PE        ", i8)') MAXC
      write (21,'(  "MIN.cell/PE        ", i8)') MINC

      return
      end







