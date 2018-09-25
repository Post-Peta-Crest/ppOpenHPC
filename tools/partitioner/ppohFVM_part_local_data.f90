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

      subroutine ppohFVM_part_LOCAL_DATA
      use m_ppohFVM_part_partitioner

      character*80 LINE
      integer(kind=kint), dimension(:)  , allocatable :: IWKM
      integer(kind=kint), dimension(:)  , allocatable :: IMASK1, IMASK2
      integer(kind=kint), dimension(:)  , allocatable :: IELMLOCAL

!C
!C-- init.
      allocate (IWKM  (NP))

      N2= max (IELMTOT, N)
      allocate (IMASK1(N2))
      allocate (IMASK2(N2))

      allocate (IELMLOCAL(IELMTOT))

      INODTOTG= N       
      IELMTOTG= IELMTOT

      do ip= 1, NP

!C
!C +--------------------------+
!C | read INITIAL LOCAL files |
!C +--------------------------+
!C===
      open (11,file=FILNAME(ip), status='unknown',form='unformatted')
        rewind (11)
          read (11) ID
          read (11) INODTOT
          read (11) N2
          read (11) N3         
          read (11) (IWKM(i),i=1,N3)

          do i= 1, INODTOT
            read (11) iip, INODLOCAL(i), (XYZ(i,k),k=1,3)
          enddo

          STACK_IMPORT(0,ip)= 0
          read (11) (STACK_IMPORT(k,ip), k=1, N3)
          do is= 1, STACK_IMPORT(N3,ip)
            read (11) NOD_IMPORT(is)          
          enddo

          STACK_EXPORT(0,ip)= 0
          allocate (NOD_EXPORT(STACK_EXPORT(N3,ip)))
          read (11) (STACK_EXPORT(k,ip), k=1, N3)
          do is= 1, STACK_EXPORT(N3,ip)
            read (11) NOD_EXPORT(is)          
          enddo

          read (11) IELMTOT
          do i= 1, IELMTOT
            read (11) iip, IELMLOCAL(i), IWORK(i),                      &
     &           (ICELNOD(i,k), k=1,NODELM(IELMLOCAL(i)))
          enddo
        close (11)
!C===

!C
!C +-----------------+
!C | LOCAL NUMBERING |
!C +-----------------+
!C===
          do i= 1, INODTOTG
            IMASK1(i)= 0
          enddo

          do i= 1, IELMTOTG
            IMASK2(i)= 0
          enddo

          do i= 1, INODTOT
            in= INODLOCAL(i)
            IMASK1(in)= i 
          enddo

          do i= 1, IELMTOT
            in= IELMLOCAL(i)
            IMASK2(in)= i 
          enddo

          ELMGRPSTACK(0)= 0
          do ig= 1, ELMGRPTOT
            icou= 0
            do is= ELMGRPSTACKG(ig-1)+1,ELMGRPSTACKG(ig)
              i= ELMGRPITEMG(is)
              if (IMASK2(i).ne.0) then
                icou= icou + 1
                  in= ELMGRPSTACK(ig-1) + icou
                ELMGRPITEM(in)= IMASK2(i)
              endif
            enddo
            ELMGRPSTACK(ig)= ELMGRPSTACK(ig-1) + icou
          enddo

          NODGRPSTACK(0)= 0
          do ig= 1, NODGRPTOT
            icou= 0
            do is= NODGRPSTACKG(ig-1)+1,NODGRPSTACKG(ig)
              i= NODGRPITEMG(is)
              if (IMASK1(i).ne.0) then
                icou= icou + 1
                  in= NODGRPSTACK(ig-1) + icou
                NODGRPITEM(in)= IMASK1(i)
              endif
            enddo
            NODGRPSTACK(ig)= NODGRPSTACK(ig-1) + icou
          enddo

          SUFGRPSTACK(0)= 0
          do ig= 1, SUFGRPTOT
            icou= 0
            do is= SUFGRPSTACKG(ig-1)+1,SUFGRPSTACKG(ig)
              i= SUFGRPITEMG(1,is)
              if (IMASK2(i).ne.0) then
                icou= icou + 1
                  in= SUFGRPSTACK(ig-1) + icou
                SUFGRPITEM(in,1)= IMASK2(i)
                SUFGRPITEM(in,2)= SUFGRPITEMG(2,is)
              endif
             enddo
            SUFGRPSTACK(ig)= SUFGRPSTACK(ig-1) + icou
          enddo
!C===

!C
!C +-------------------------+
!C | write FINAL LOCAL files |
!C +-------------------------+
!C===
        open (12,file=FILNAME(ip), status='unknown', form='formatted')

        rewind (12)
!C
!C-- section 1.
          write (* ,'("PE:", i5, 2i10)') ip, INODTOT, N2

          write(12,'(10i10)')  CoarseGridLevels
          write(12,'(10i10)')  HOWmanyADAPTATIONs
          write(12,'(10i10)')  ip-1
          write(12,'(10i10)')  N3
          write(12,'(10i10)') (IWKM(inei)-1,inei=1,N3)
          write(12,'(10i10)')  IMATTOT
!C
!C-- section 2.: element
          write(12,'(10i10)') INODTOT, N2

          do i= 1, INODTOT
            in= INODLOCAL(i)
            write (12,'(3i10,3(1pe16.6))')                              &
     &      HOME_NODE(in,2), HOME_NODE(in,1), WhenIwasRefinedN,         &
     &      (XYZ(i,k),k=1,3)
          enddo

          write(12,'(10i10)') NPC(ip), nELEM_internal(ip)
          write(12,'(10i10)') (IELMTYP(IELMLOCAL(i)), i=1,NPC(ip))

          ELEM_internal_LIST= 0
          icou= 0
          do i= 1, IELMTOT
            icel= IELMLOCAL(i)
            if (HOME_ELEM(icel,1)+1 .eq. ip) then
              icou= icou + 1
              ELEM_internal_LIST(icou)= i
            endif
            if (IMATTOT.eq.0) then
              write (12,'(10i10)')                                        &
     &        HOME_ELEM(icel,2), HOME_ELEM(icel,1), WhenIwasRefinedE,     &
     &        IELMMAT(icel),                                              &
     &       (ICELNOD(i,k),k=1,NODELM(icel))
             else
              if (NODELM(icel).eq.4) then
                write (12,'(8i10,10(1pe16.6))')                             &
     &          HOME_ELEM(icel,2), HOME_ELEM(icel,1), WhenIwasRefinedE,     &
     &          IELMMAT(icel),                                              &
     &          (ICELNOD(i,k),k=1,4), (ELMMAT(icel,kk), kk= 1,IMATTOT)
               else
                write (12,'(10i10,10(1pe16.6))')                            &
     &          HOME_ELEM(icel,2), HOME_ELEM(icel,1), WhenIwasRefinedE,     &
     &          IELMMAT(icel),                                              &
     &          (ICELNOD(i,k),k=1,6), (ELMMAT(icel,kk), kk= 1,IMATTOT)
              endif
            endif
          enddo

          do i= 1, IELMTOT
            write (12,'(40i10)')                                        &
     &        adapt_type, adapt_level, adapt_par2, adapt_par1,          &
     &        adapt_par_type,                                           &
     &        adapt_chi2, adapt_chi1,  adapt_chi2, adapt_chi1,          &
     &        adapt_chi2, adapt_chi1,  adapt_chi2, adapt_chi1,          &
     &        adapt_chi2, adapt_chi1,  adapt_chi2, adapt_chi1,          &
     &        adapt_chi2, adapt_chi1,  adapt_chi2, adapt_chi1
          enddo

          write (12,'(10i10)')                                          &
     &          (ELEM_internal_LIST(i), i=1,nELEM_internal(ip))

!C
!C-- section 3.
          STACK_IMPORT(0,ip)= 0
            write (12,'(10i10)') (STACK_IMPORT(k,ip), k=1, N3)
            if (N3.ne.0) then
              write (12,'(10i10)')                                      &
     &              (NOD_IMPORT(is), is= 1, STACK_IMPORT(N3,ip))
            endif
            
            write(12,'(10i10)') (STACK_EXPORT(inei,ip), inei= 1, N3) 
            if (N3.ne.0) then
              write (12,'(10i10)')                                      &
     &              (NOD_EXPORT(is), is= 1, STACK_EXPORT(N3,ip))
            endif
          deallocate (NOD_EXPORT)

!C
!C-- section 4.
          call ppohFVM_part_DATA_COMPRESS (NODGRPTOT, IGTOT, NODGRPSTACK, IWORK)
!          if (IWORK(IGTOT).eq.0) IGTOT= 0
          IGTOT= NODGRPTOT

          write (12,'(10i10)')  IGTOT

          if (IGTOT.ne.0) then
          write (12,'(10i10)') (IWORK(ig), ig=1,IGTOT)
          do ig= 1, NODGRPTOT
            nn= NODGRPSTACK(ig) - NODGRPSTACK(ig-1)
            write (12,'(a80)')    NODGRPNAME(ig)
            if (nn.ne.0) then
            write (12,'(10i10)') (NODGRPITEM(is),                       &
     &                  is= NODGRPSTACK(ig-1)+1, NODGRPSTACK(ig) )
            endif
          enddo
          endif

          call ppohFVM_part_DATA_COMPRESS (ELMGRPTOT, IGTOT, ELMGRPSTACK, IWORK)
!          if (IWORK(IGTOT).eq.0) IGTOT= 0
          IGTOT= ELMGRPTOT
          write (12,'(10i10)') IGTOT

          if (IGTOT.ne.0) then
          write (12,'(10i10)') (IWORK(ig), ig=1,IGTOT)
          do ig= 1, ELMGRPTOT
            nn= ELMGRPSTACK(ig) - ELMGRPSTACK(ig-1)
            write (12,'(a80)')    ELMGRPNAME(ig)
            if (nn.ne.0) then
            write (12,'(10i10)') (ELMGRPITEM(is),                       &
     &                  is= ELMGRPSTACK(ig-1)+1, ELMGRPSTACK(ig) )
            endif
          enddo 
          endif

          call ppohFVM_part_DATA_COMPRESS (SUFGRPTOT, IGTOT, SUFGRPSTACK, IWORK)
!          if (IWORK(IGTOT).eq.0) IGTOT= 0
          IGTOT= SUFGRPTOT
          write (12,'(10i10)') IGTOT

          if (IGTOT.ne.0) then
          write (12,'(10i10)') (IWORK(ig), ig=1,IGTOT)
          do ig= 1, SUFGRPTOT
            nn= SUFGRPSTACK(ig) - SUFGRPSTACK(ig-1)
              write (12,'(a80)') SUFGRPNAME (ig)
            if (nn.ne.0) then
              write (12,'(10i10)')(SUFGRPITEM(is,1),                    &
     &                   is= SUFGRPSTACK(ig-1)+1, SUFGRPSTACK(ig) )
              write (12,'(10i10)')(SUFGRPITEM(is,2),                    &
     &                   is= SUFGRPSTACK(ig-1)+1, SUFGRPSTACK(ig) )
            endif
          enddo
          endif

        close (12)
!C===
      enddo

      return
      end

      subroutine ppohFVM_part_DATA_COMPRESS (I1, I2, I_INN, I_OUT)
      integer  I_INN(0:I1), I_OUT(0:I1)

      ITOT= I1
      do i= 0, I1
        I_OUT(i)= I_INN(i)
      enddo

!      do i= 1, I1
!        ip= I_OUT(i)
!        if (ip.eq.I_OUT(i-1)) then
!          ITOT= ITOT - 1
!          do j= i, ITOT
!            I_OUT(j)= I_OUT(j+1)
!          enddo   
!        endif      
!        if (i.eq.ITOT) exit
!      enddo   

      I2= ITOT

      return
      end
