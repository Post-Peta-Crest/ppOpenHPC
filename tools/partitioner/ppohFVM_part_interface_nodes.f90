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

      subroutine ppohFVM_part_INTERFACE_NODES
      use m_ppohFVM_part_partitioner

      integer(kind=kint), dimension(:)  , allocatable :: IWKM
      integer(kind=kint), dimension(:)  , allocatable :: iwkbuf
      integer(kind=kint), dimension(:,:), allocatable :: WHOAMI

      character*80 LINE

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      write ( *,*) ' '
!C
!C    pointer WHOAMI defines MY ID in my neighbor's neighboring
!C    PE array.
!C
!C    WHOAMI(ip,j) : ip-th PE's ID in his/her j-th neighboring PE's
!C                   neighboring PE array
!C

      allocate (WHOAMI(NP,NP), iwkbuf(100))
      allocate (IWKM  (NP)   )

      do ip= 1, NP
        STACK_IMPORT(0,ip)= 0
      do jp= 1, NP
        WHOAMI(ip,jp)= 0
      enddo
      enddo

      do ip  = 1, NP
      do inei= 1, NEIBPETOT(ip)
        ip1= NEIBPE(ip,inei)
        do in1= 1, NEIBPETOT(ip1)
          if (NEIBPE(ip1,in1).eq.ip) WHOAMI(ip,inei)= in1
        enddo
      enddo
      enddo
!C===

!C
!C +---------------------------------------------+
!C | create INITIAL FILE : LOCAL/IMPORT pointers |
!C +---------------------------------------------+
!C===
      do ip= 1, NP
      open (11,file=FILNAME(ip), status='unknown', form='unformatted')
!C
!C-- define INTERIOR and EXTERIOR NODEs

        do i= 1, N
          IMASK(i)= 0
        enddo

        icou= 0
        
           NODTOT(ip)= NPN(ip)
        INTNODTOT(ip)= NPN(ip)

        do is= ISTACKN(ip-1)+1, ISTACKN(ip)
            in= NPNID(is)
          icou= icou + 1
          INODLOCAL(icou)= in
        enddo

        do is= ISTACKC(ip-1)+1, ISTACKC(ip)
          icel= NPCID(is)
        do k = 1, NODELM(icel)
          in= ICELNOD(icel,k)
          ig= IGROUP (in)

          if (ig.ne.ip) call ppohFVM_part_MARK_INT (in,ip)
        enddo
        enddo

!C ver.0.2.0 - start
        IMASK= 0
        do inei= 1, NEIBPETOT(ip)
          ig= NEIBPE(ip,inei)
          do it= INTNODTOT(ip)+1, NODTOT(ip)
            in= INODLOCAL(it)
            if (IGROUP(in).eq.ig) then
                    icou = icou + 1
              IMASK(icou)= in
            endif
          enddo
       enddo

       do it= INTNODTOT(ip)+1, NODTOT(ip)
         INODLOCAL(it)= IMASK(it)
       enddo
!C ver.0.2.0 - end
       
          write (11) ip
          write (11) NODTOT(ip)
          write (11) INTNODTOT(ip)
          write (11) NEIBPETOT(ip)
          write (11) (NEIBPE(ip,inei),inei= 1, NEIBPETOT(ip))

          do in= 1, NODTOT(ip)
            in1= INODLOCAL(in)
            write (11) in, in1, (XYZ(in1,k),k=1,3), IGROUP(in1)
          enddo

!C
!C-- ASSEMBLE EXTERIOR NODEs
!C            IMPORT pointers

        STACK_IMPORT(0,ip)= 0
        do inei= 1, NEIBPETOT(ip)
          icou= 0
            ig= NEIBPE(ip,inei)
          do it= INTNODTOT(ip)+1, NODTOT(ip)
            in= INODLOCAL(it)
            if (IGROUP(in).eq.ig) then
                    icou = icou + 1
              IMASK(icou)= it
            endif
          enddo

          STACK_IMPORT(inei,ip)= STACK_IMPORT(inei-1,ip) + icou
          do ic= 1, icou
            is= ic + STACK_IMPORT(inei-1,ip)
            NOD_IMPORT(is)= IMASK(ic)
          enddo
        enddo

          write (11) (STACK_IMPORT(inei,ip), inei= 1, NEIBPETOT(ip))    

          do is= 1, STACK_IMPORT(NEIBPETOT(ip),ip)
            nodL= NOD_IMPORT(is)
            nodG= INODLOCAL(nodL)
            write (11) nodL, nodG, IGROUP(nodG)
          enddo

          close (11)
      enddo
!C===

!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===

!C
!C-- INIT.

      do ip= 1, NP
        STACK_EXPORT(0,ip)= 0
      do inei= 1, NEIBPETOT(ip)
        ig  = NEIBPE(ip,inei)
        iobj= WHOAMI (ip,inei)
        STACK_EXPORT(inei,ip)= STACK_EXPORT(inei-1,ip) +                &
     &                         STACK_IMPORT(iobj,ig) -                  &
     &                               STACK_IMPORT(iobj-1,ig)
      enddo
      enddo

      do ip= 1, NP
        open   (11,file=FILNAME(ip),status='unknown', form='unformatted')
        rewind (11)
          read (11) ID
          read (11) N1
          read (11) N2
          read (11) N3         
          read (11) (IWKM(i),i=1,N3)

          do in= 1, N1
            read (11) inK0, INODLOCAL(in), XX, YY, ZZ
          enddo

          read (11) (STACK_IMPORT(inei,ip), inei= 1, NEIBPETOT(ip))

          do is= 1, STACK_IMPORT(NEIBPETOT(ip),ip)
            read (11) NOD_IMPORT(is)
          enddo

        close (11)          

!C
!C-- "masking" with GLOBAL NODE ID

        open   (12,file=FILNAME(ip),status='unknown',form='unformatted')
        rewind (12)
          write (12) ID
          write (12) N1
          write (12) N2
          write (12) N3         
          write (12) (IWKM(i),i=1,N3)

          do in= 1, N1
            in1= INODLOCAL(in)
            write (12) in, in1, (XYZ(in1,k),k=1,3), IGROUP(in1)
          enddo

          write (12) (STACK_IMPORT(inei,ip), inei= 1, NEIBPETOT(ip))
          do is= 1, STACK_IMPORT(NEIBPETOT(ip),ip)
            nodL= NOD_IMPORT(is)
            nodG= INODLOCAL(nodL)
            write (12) nodL, nodG, IGROUP(nodG)
          enddo

        do i= 1, NODTOT(ip)
                in = INODLOCAL(i)
          IMASK(in)= i
        enddo
          
        write (12) (STACK_EXPORT(inei,ip), inei= 1, NEIBPETOT(ip))
       
        do inei= 1, NEIBPETOT(ip)
          ig= NEIBPE(ip,inei)
          open   (11,file=FILNAME(ig),status='unknown',form='unformatted')
          rewind (11)

          iobj= WHOAMI (ip,inei)

          read (11) ID
          read (11) N1
          read (11) N2
          read (11) N3         
          read (11) (IWKM(i),i=1,N3)

          do i= 1, N1
            read (11) iip, INODLOCAL(i)
          enddo

!C
!C-- process NEIGHBOR's IMPORT pointers

          STACK_IMPORT(0,ig)= 0
          read (11) (STACK_IMPORT(k,ig), k=1, N3)
          do is= 1, STACK_IMPORT(N3,ig)
            read (11) i1,NOD_IMPORT(is)          
          enddo

!C
!C-- TRANSFER INFO. into "MY" EXPORT pointers
          N2n= 2*max(N,IELMTOT)
          allocate (NOD_EXPORT(N2n))
  100     continue

          ISTART= STACK_EXPORT(inei-1,ip)
          do is= STACK_IMPORT(iobj-1,ig)+1, STACK_IMPORT(iobj,ig)
            icur= is - STACK_IMPORT(iobj-1,ig)
            inod= NOD_IMPORT(is)

            if (ISTART+icur.gt.N2n) then
              deallocate (NOD_EXPORT)
              N2n= N2n * 11/10 + 1
              allocate (NOD_EXPORT(N2n))
              goto 100
            endif
           
            NOD_EXPORT(ISTART+icur)= IMASK(is)
            write (12) IMASK(inod), inod, ig
          enddo
          deallocate (NOD_EXPORT)
        enddo

        write (12) NPC(ip)
        do is= ISTACKC(ip-1)+1, ISTACKC(ip)
           is0 = is - ISTACKC(ip-1)
           icel= NPCID(is)

           do kkk= 1, NODELM(icel)
             iwkbuf(kkk)= IMASK(ICELNOD(icel,kkk))
           enddo

           write (12) is0, icel, IELMTYP(icel),                         &
     &               (iwkbuf(kkk), kkk= 1, NODELM(icel))
        enddo
       
        close (11)
        close (12)

      enddo

      deallocate (iwkbuf)

      return
      end

      subroutine ppohFVM_part_MARK_INT (in,ip)
      use m_ppohFVM_part_partitioner

      if (IMASK(in).eq.1) then
        return
       else
        IMASK(in)= 1
        NODTOT(ip)= NODTOT(ip) + 1
               ii = NODTOT(ip)
        INODLOCAL(ii)= in
      endif
      return
      end



