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

      subroutine ppohFVM_part_PARASET
      use m_ppohFVM_part_partitioner

      N2= max (IELMTOT, N)
      allocate (IGROUP(N2))

!C
!C-- RCB/GREEDY

      do 
        write (*,'(/,"# select PARTITIONING METHOD")')
        write (*,'(  "  RCB                  (1)")')
        write (*,'(  "  K-METIS              (2)")')
        write (*,'(  "  P-METIS              (3)")')
        write (*,*) ' '
        write (*,*) 'Please TYPE 1-3 !!'
        write (*,'(/,">>>")')
        read  (*,*)  NTYP
        if (NTYP.ge.1 .and. NTYP.le.3) exit
      enddo

      if (NTYP.eq.1) then
        do 
          write (*,'(/,"*** RECURSIVE COORDINATE BISECTION (RCB)")')
          write (*,*) 'How many partitions (2**n)?'
          write (*,'(/,">>>")')
          read  (*,*)  NPOWER
          if (NPOWER.ge.0) exit
        enddo
        NP= 2**NPOWER
      endif

      if (NTYP.ge.2) then
        do
          write (*,'(/,"*** K-METIS/P-METIS")')
          write (*,*      ) 'NUMBER of regions ?'
          write (*,'(/,">>>")')
          read  (*,*)  NP
          if (NP.ge.1) exit
        enddo
      endif

      write (*,'(//,"*** ",i3," REGIONS",//)') NP

      if (NP.gt.N) call ppohFVM_part_ERROR_EXIT(32,N)

!C
!C-- allocation
      allocate (STACK_EXPORT(0:NP,NP))
      allocate (STACK_IMPORT(0:NP,NP))
      allocate (NPN(NP))
      allocate (NPC(NP))
      allocate (ISTACKN(0:NP))
      allocate (ISTACKC(0:NP))
      allocate (NEIBPE   (NP,NP))
      allocate (NEIBPETOT(NP))
      allocate (   NODTOT(NP))
      allocate (INTNODTOT(NP))

      allocate (IWORK(0:N2))
      allocate (IMASK(-N2:+N2))
      allocate (IDEAD (N2))
      allocate (ISTACK(N2))

      allocate (ICOND1(N2))
      allocate (ICOND2(N2))

      allocate (INODLOCAL(N2))

      allocate (NOD_IMPORT(  N2))

!C
!C-- FILE NAME
      write (*,'(a)') '# HEADER of the OUTPUT file ?'
      write (*,'(/,">>>")')
      read  (*,'(a80)') HEADER

      allocate (FILNAME(NP))

      do my_rank= 0, NP-1
        call ppohFVM_part_DEFINE_FILE_NAME (HEADER, my_rank, FILNAME(my_rank+1))
      enddo

!C
!C-- PARAMETER SET
 
      inum   = N / NP
      idev   = N - inum * NP

      do ip= 1, NP
        NPN(ip)= inum
      enddo

      do ip= 1, idev
        NPN(ip)= NPN(ip) + 1
      enddo

      do i= 1, N
        if (NTYP.ne.3) IGROUP(i)= 0
        RHO   (i)= 0
        IMASK (i)= 0
        IDEAD (i)= 0
      enddo

      return

 998  continue
        call ppohFVM_part_ERROR_EXIT (21,0)
 999  continue
        call ppohFVM_part_ERROR_EXIT (22,0)
      end
