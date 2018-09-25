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
!C*** RESIDUAL
!C***
!C
!C    CALCULATES THE CHANGEs in the STATE-VECTOR
!C    over ALL edges
!C    
!C      INVISCID terms
!C      VISCOUS  terms
!C    

      subroutine RESIDUAL (st_comm_info)

      use m_ppohFVM_util
      use HYBRID
      implicit REAL*8 (A-H,O-Z)
      type (st_ppohFVM_comm_info)  :: st_comm_info

      data iflag/0/
      real(kind=8), save :: tt1, tt2, tt3

      if (iflag.eq.0) then
        tt1= 0.d0
        tt2= 0.d0
        tt3= 0.d0
        iflag= 1
      endif

!C
!C-- init.

      w1= MPI_Wtime()
       
!C
!C +-----------------------------+
!C | VALUEs & FLUXs at each NODE |
!C +-----------------------------+
!C===

!$omp parallel do private (in,Rnod,VR,RUnod,RVnod,RWnod,Unod,Vnod,Wnod,Enod,Pnod,Hnod,Ugrid,Vgrid,Wgrid,UREL,VREL,WREL)
      do in= 1, NODTOT
!C
!C-- PRIMITIVE VARIABLEs
        Rnod= U(1,in)
        VR  = 1.d0 / Rnod

        RUnod= U(2,in)
        RVnod= U(3,in)
        RWnod= U(4,in)

        Unod= RUnod * VR
        Vnod= RVnod * VR
        Wnod= RWnod * VR

        Enod= U(5,in)
        Pnod= P(in)

        Hnod= Enod + Pnod

        Ugrid= VELgrid(1,in)
        Vgrid= VELgrid(2,in)
        Wgrid= VELgrid(3,in)

!C
!C-- INVISCID FLUX VECTORs
        UREL= Unod - Ugrid
        VREL= Vnod - Vgrid
        WREL= Wnod - Wgrid

        FCV(1,in)=  Rnod * UREL
        FCV(2,in)= RUnod * UREL + Pnod
        FCV(3,in)= RVnod * UREL
        FCV(4,in)= RWnod * UREL
        FCV(5,in)=  Hnod * UREL + Pnod * Ugrid

        GCV(1,in)=  Rnod * VREL
        GCV(2,in)= RUnod * VREL
        GCV(3,in)= RVnod * VREL + Pnod
        GCV(4,in)= RWnod * VREL
        GCV(5,in)=  Hnod * VREL + Pnod * Vgrid

        HCV(1,in)=  Rnod * WREL
        HCV(2,in)= RUnod * WREL
        HCV(3,in)= RVnod * WREL
        HCV(4,in)= RWnod * WREL + Pnod
        HCV(5,in)=  Hnod * WREL + Pnod * Wgrid

        PU(1,in)= Rnod

        PU(2,in)= Unod
        PU(3,in)= Vnod
        PU(4,in)= Wnod

        PU(5,in)= GAM * Pnod * VR
      
      enddo
!C===
      w2= MPI_Wtime()
      call EDGE_RESID
      w3= MPI_Wtime()
      call SMOOTH     (st_comm_info)
      w4= MPI_Wtime()

!      tt1= tt1 + w2 - w1
!      tt2= tt2 + w3 - w2
!      tt3= tt3 + w4 - w3

!      if (my_rank.eq.0) then
!        write (*,'(3(1pe16.6))') tt1, tt2, tt3
!      endif

      call BOUNDARY   (st_comm_info)


      return
      end









