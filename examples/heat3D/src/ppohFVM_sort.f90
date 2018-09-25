!!====================================================================!!
!!                                                                    !!
!!   Software Name : ppohFVM                                          !!
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
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Kengo Nakajima, The University of Tokyo      !!
!!                       nakajima(at)cc.u-tokyo.ac.jp           >     !!
!!                                                                    !!
!!====================================================================!!

!C
!C***
!C*** ppohFVM_mSORT
!C***
!C
      subroutine  ppohFVM_mSORT (STEM,INUM,NN)
      integer*4  STEM(NN), INUM(NN)

      do ii = 1,NN
        INUM(ii)= ii
      enddo

      do ii= 1,NN-1
!CDIR NOVECTOR
      do jj= 1,NN-ii
        if (STEM(INUM(jj)) .lt. STEM(INUM(jj+1))) then
          ITEM      = INUM(jj+1)
          INUM(jj+1)= INUM(jj)
          INUM(jj)  = ITEM
        endif
      enddo
      enddo

      return
      end

!C
!C***
!C***  ppohFVM_matconSORT
!C***
!C
      subroutine  ppohFVM_matconSORT (STEM, INUM, N, NN)

      integer(kind=4), dimension(N)                :: STEM, INUM
      integer(kind=4), dimension(:,:), allocatable :: ISTACK

      allocate (ISTACK(0:NN+1,2))
      ISTACK(0,1)= 0
      ISTACK(0,2)= 0

!CDIR NODEP
      do i= 1, N
        INUM(i)= i
        STEM(i)= STEM(i) + 1
      enddo

!CDIR NODEP
      do i= 1, NN+1
        ISTACK(i,1)= 0
      enddo

      ICONmax= -N
!CDIR NOVECTOR
      do i= 1, N
        ii= STEM(i)
        ICONmax= max(ii,ICONmax)
        ISTACK(ii,1)= ISTACK(ii,1) + 1
      enddo

!CDIR NOVECTOR
      do k= 1, ICONmax
        ISTACK(k,1)= ISTACK(k-1,1) + ISTACK(k,1)
        ISTACK(k,2)= ISTACK(k  ,1)
      enddo

      ISTACK(0,2)= ISTACK(1,2)
!CDIR NOVECTOR
      do k= 1, ICONmax
        ik1= ICONmax - k
        ik2= ik1     + 1
        ISTACK(k,1)= ISTACK(ik2,2) - ISTACK(ik1,2) + ISTACK(k-1,1)
      enddo

!CDIR NODEP
      do k= 1, ICONmax
        ISTACK(k,2)= 0
      enddo

!CDIR NOVECTOR
      do i= 1, N
        ii  = STEM(i)
        icon= ISTACK(ii,2) + 1
        ISTACK(ii,2)= icon
        INUM(ISTACK(ICONmax-ii+1-1,1)+icon)= i
      enddo

      do i= 1, N
        STEM(i)= STEM(i) - 1
      enddo

      deallocate (ISTACK)

      return
      end
