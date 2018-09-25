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
!C*** ppohFVM_edge_metrics_tetra_341
!C***
!C
!C    EDGE info. in tetrahedra
!C

      subroutine ppohFVM_edge_metrics_tetra_341  (st_local_mesh, st_comm_info, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

       NODTOT= st_local_mesh%n_node
      ICELTOT= st_local_mesh%n_elem
      IEDGTOT= st_edge_info%n_edge
!C
!C +------------+
!C | CELL VOUME |
!C +------------+
!C===
      do icel0= 1, st_local_mesh%n_ACTtetra_341
        icel= st_local_mesh%ACTtetra_341_id(icel0)
          VC= st_local_mesh%volc (icel)

        iS = st_local_mesh%index_elem(icel-1)
        n1= st_local_mesh%ptr_elem(iS+1)
        n2= st_local_mesh%ptr_elem(iS+2)
        n3= st_local_mesh%ptr_elem(iS+3)
        n4= st_local_mesh%ptr_elem(iS+4)

        X21= st_local_mesh%node(1,n2) - st_local_mesh%node(1,n1)
        X31= st_local_mesh%node(1,n3) - st_local_mesh%node(1,n1)
        X41= st_local_mesh%node(1,n4) - st_local_mesh%node(1,n1)

        Y21= st_local_mesh%node(2,n2) - st_local_mesh%node(2,n1)
        Y31= st_local_mesh%node(2,n3) - st_local_mesh%node(2,n1)
        Y41= st_local_mesh%node(2,n4) - st_local_mesh%node(2,n1)

        Z21= st_local_mesh%node(3,n2) - st_local_mesh%node(3,n1)
        Z31= st_local_mesh%node(3,n3) - st_local_mesh%node(3,n1)
        Z41= st_local_mesh%node(3,n4) - st_local_mesh%node(3,n1)

        st_local_mesh%volc(icel)= ppohFVM_O6th *                           &
     &     dabs( X21*Y31*Z41 + Y21*Z31*X41 + Z21*X31*Y41                &
     &         - Z21*Y31*X41 - X21*Z31*Y41 - Y21*X31*Z41 )
        if (st_local_mesh%volc(icel).le.0.d0) call ppohFVM_error_exit (81)
      enddo
!C===

!C
!C +-----------------------+
!C | EDGE PROJECTION AREA |
!C +-----------------------+
!C===
      do icel0= 1, st_local_mesh%n_ACTtetra_341
        icel= st_local_mesh%ACTtetra_341_id(icel0)
          VC= st_local_mesh%volc (icel)

        iS = st_local_mesh%index_elem(icel-1)
        n1= st_local_mesh%ptr_elem(iS+1)
        n2= st_local_mesh%ptr_elem(iS+2)
        n3= st_local_mesh%ptr_elem(iS+3)
        n4= st_local_mesh%ptr_elem(iS+4)

        ip1= st_local_mesh%node_id(n1,2) 
        ip2= st_local_mesh%node_id(n2,2)
        ip3= st_local_mesh%node_id(n3,2)
        ip4= st_local_mesh%node_id(n4,2)

        XC= 0.25d0 * ( st_local_mesh%node(1,n1)+st_local_mesh%node(1,n2)+  &
     &                 st_local_mesh%node(1,n3)+st_local_mesh%node(1,n4) )
        YC= 0.25d0 * ( st_local_mesh%node(2,n1)+st_local_mesh%node(2,n2)+  &
     &                 st_local_mesh%node(2,n3)+st_local_mesh%node(2,n4) )
        ZC= 0.25d0 * ( st_local_mesh%node(3,n1)+st_local_mesh%node(3,n2)+  &
     &                 st_local_mesh%node(3,n3)+st_local_mesh%node(3,n4) )

        V012= 0.50d0 *  st_local_mesh%volc(icel)

        if (ip1.eq.st_comm_info%my_rank.or.ip2.eq.st_comm_info%my_rank) then
          call ppohFVM_edge_cre     (st_edge_info, n1, n2, iedg, 1, ICELTOT, NODTOT, IEDGTOT)
          call ppohFVM_edge_tet_341 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n1, n2, n3, n4)
          st_edge_info%vol(iedg)= st_edge_info%vol(iedg) + V012
        endif

        if (ip1.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) then
          call ppohFVM_edge_cre     (st_edge_info, n1, n3, iedg, 1, ICELTOT, NODTOT, IEDGTOT)
          call ppohFVM_edge_tet_341 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n1, n3, n2, n4)
          st_edge_info%vol(iedg)= st_edge_info%vol(iedg) + V012
        endif

        if (ip1.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) then
          call ppohFVM_edge_cre     (st_edge_info, n1, n4, iedg, 1, ICELTOT, NODTOT, IEDGTOT)
          call ppohFVM_edge_tet_341 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n1, n4, n2, n3)
          st_edge_info%vol(iedg)= st_edge_info%vol(iedg) + V012
        endif

        if (ip2.eq.st_comm_info%my_rank.or.ip3.eq.st_comm_info%my_rank) then
          call ppohFVM_edge_cre     (st_edge_info, n2, n3, iedg, 1, ICELTOT, NODTOT, IEDGTOT)
          call ppohFVM_edge_tet_341 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n2, n3, n1, n4)
          st_edge_info%vol(iedg)= st_edge_info%vol(iedg) + V012
        endif

        if (ip2.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) then
          call ppohFVM_edge_cre     (st_edge_info, n2, n4, iedg, 1, ICELTOT, NODTOT, IEDGTOT)
          call ppohFVM_edge_tet_341 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n2, n4, n1, n3)
          st_edge_info%vol(iedg)= st_edge_info%vol(iedg) + V012
        endif

        if (ip3.eq.st_comm_info%my_rank.or.ip4.eq.st_comm_info%my_rank) then
          call ppohFVM_edge_cre     (st_edge_info, n3, n4, iedg, 1, ICELTOT, NODTOT, IEDGTOT)
          call ppohFVM_edge_tet_341 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n3, n4, n1, n2)
          st_edge_info%vol(iedg)= st_edge_info%vol(iedg) + V012
        endif
      enddo
!C==
      return
      end
!C
!C***
!C*** ppohFVM_edge_tet_341
!C***
!C
!C    TETRAHEDRAL EDGE METRICs
!C

      subroutine ppohFVM_edge_tet_341                                           &
                 (st_local_mesh, st_edge_info, iedg, XC, YC, ZC, n1, n2, n3, n4)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_edge_info)  :: st_edge_info

        n1k= st_edge_info%edgnod(1,iedg)
        n2k= st_edge_info%edgnod(2,iedg)

        EVECX= st_local_mesh%node(1,n2k) - st_local_mesh%node(1,n1k)
        EVECY= st_local_mesh%node(2,n2k) - st_local_mesh%node(2,n1k)
        EVECZ= st_local_mesh%node(3,n2k) - st_local_mesh%node(3,n1k)
        call ppohFVM_normal (EVECX, EVECY, EVECZ, XMAG)

        EX0= 0.5d0 * (st_local_mesh%node(1,n1) + st_local_mesh%node(1,n2))
        EY0= 0.5d0 * (st_local_mesh%node(2,n1) + st_local_mesh%node(2,n2))
        EZ0= 0.5d0 * (st_local_mesh%node(3,n1) + st_local_mesh%node(3,n2))

        X1= ppohFVM_O3rd*(st_local_mesh%node(1,n1)+st_local_mesh%node(1,n2)+   &
     &                    st_local_mesh%node(1,n3))
        Y1= ppohFVM_O3rd*(st_local_mesh%node(2,n1)+st_local_mesh%node(2,n2)+   &
     &                    st_local_mesh%node(2,n3))
        Z1= ppohFVM_O3rd*(st_local_mesh%node(3,n1)+st_local_mesh%node(3,n2)+   &
     &                    st_local_mesh%node(3,n3))

        X2= ppohFVM_O3rd*(st_local_mesh%node(1,n1)+st_local_mesh%node(1,n2)+   &
     &                    st_local_mesh%node(1,n4))
        Y2= ppohFVM_O3rd*(st_local_mesh%node(2,n1)+st_local_mesh%node(2,n2)+   &
     &                    st_local_mesh%node(2,n4))
        Z2= ppohFVM_O3rd*(st_local_mesh%node(3,n1)+st_local_mesh%node(3,n2)+   &
     &                    st_local_mesh%node(3,n4))
     
!C-    DISTRIBUTION 1
        DX1= XC - EX0
        DY1= YC - EY0
        DZ1= ZC - EZ0

        DX2= X1 - EX0
        DY2= Y1 - EY0
        DZ2= Z1 - EZ0

        call ppohFVM_cross(DX1,DY1,DZ1,DX2,DY2,DZ2,AX,AY,AZ,AREA)
        DOT= EVECX * AX + EVECY * AY + EVECZ * AZ

        if (dabs(DOT).lt.1.e-3) DOT= 0.d0
        if (DOT .lt. 0.d0) then
          AX= -AX
          AY= -AY
          AZ= -AZ
        endif

        st_edge_info%area(1,iedg)= st_edge_info%area(1,iedg) + AX * AREA
        st_edge_info%area(2,iedg)= st_edge_info%area(2,iedg) + AY * AREA
        st_edge_info%area(3,iedg)= st_edge_info%area(3,iedg) + AZ * AREA

!C-    DISTRIBUTION 2
        DX1= XC - EX0
        DY1= YC - EY0
        DZ1= ZC - EZ0

        DX2= X2 - EX0
        DY2= Y2 - EY0
        DZ2= Z2 - EZ0

        call ppohFVM_cross(DX1,DY1,DZ1,DX2,DY2,DZ2,AX,AY,AZ,AREA)
        DOT= EVECX * AX + EVECY * AY + EVECZ * AZ

        if (dabs(DOT).lt.1.e-3) DOT= 0.d0
        if (DOT .lt. 0.d0) then
          AX= -AX
          AY= -AY
          AZ= -AZ
        endif

        st_edge_info%area(1,iedg)= st_edge_info%area(1,iedg) + AX * AREA
        st_edge_info%area(2,iedg)= st_edge_info%area(2,iedg) + AY * AREA
        st_edge_info%area(3,iedg)= st_edge_info%area(3,iedg) + AZ * AREA

      return
      end



