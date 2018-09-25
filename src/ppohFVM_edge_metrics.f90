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
!C***  ppohFVM_edge_metrics
!C***
!C
!C     calc. EDGE METRICS
!C
      subroutine ppohFVM_edge_metrics (st_local_mesh, st_comm_info, st_edge_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_edge_info)  :: st_edge_info

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      call ppohFVM_edge_metrics_init (st_local_mesh, st_comm_info, st_edge_info)
      call ppohFVM_edge_color        (st_local_mesh, st_comm_info, st_edge_info)
!C===

!C
!C +--------------+
!C | EDGE metrics |
!C +--------------+
!C===
      allocate (st_edge_info%area(4,st_edge_info%n_edge))
      allocate (st_edge_info%vol (  st_edge_info%n_edge))

      st_edge_info%area= 0.d0
      st_edge_info%vol = 0.d0

      allocate (st_local_mesh%volc(st_local_mesh%n_elem))

      if (st_local_mesh%n_ACTtetra_341.ne.0) then
        call ppohFVM_edge_metrics_tetra_341 (st_local_mesh, st_comm_info, st_edge_info)
      endif

      if (st_local_mesh%n_ACTprism_351.ne.0) then
        call ppohFVM_prism_351_metrics      (st_local_mesh, st_edge_info)
        call ppohFVM_edge_metrics_prism_351 (st_local_mesh, st_comm_info, st_edge_info)
      endif

      if (st_local_mesh%n_ACThexa_361.ne.0) then
        call ppohFVM_hexa_361_metrics      (st_local_mesh, st_edge_info)
        call ppohFVM_edge_metrics_hexa_361 (st_local_mesh, st_comm_info, st_edge_info)
      endif
!C===

!C
!C +-------------------+
!C | DUAL-cell metrics |
!C +-------------------+
!C===
      allocate (st_local_mesh%voln (  st_local_mesh%n_node))
      allocate (st_local_mesh%sarea(3,st_local_mesh%n_node))

      st_local_mesh%voln = 0.d0
      st_local_mesh%sarea= 0.d0

      do ie= 1, st_edge_info%n_edge
        in1= st_edge_info%edgnod(1,ie)
        in2= st_edge_info%edgnod(2,ie)

        SX= 0.50d0 * dabs(st_edge_info%area(1,ie))
        SY= 0.50d0 * dabs(st_edge_info%area(2,ie))
        SZ= 0.50d0 * dabs(st_edge_info%area(3,ie))

        st_local_mesh%sarea(1,in1)= st_local_mesh%sarea(1,in1) + SX
        st_local_mesh%sarea(2,in1)= st_local_mesh%sarea(2,in1) + SY
        st_local_mesh%sarea(3,in1)= st_local_mesh%sarea(3,in1) + SZ

        st_local_mesh%sarea(1,in2)= st_local_mesh%sarea(1,in2) + SX
        st_local_mesh%sarea(2,in2)= st_local_mesh%sarea(2,in2) + SY
        st_local_mesh%sarea(3,in2)= st_local_mesh%sarea(3,in2) + SZ
      enddo

      do icel0= 1, st_local_mesh%n_ACTtetra_341
        icel= st_local_mesh%ACTtetra_341_id(icel0)
          VC= st_local_mesh%volc (icel)

         iS = st_local_mesh%index_elem(icel-1)
         in1= st_local_mesh%ptr_elem(iS+1)
         in2= st_local_mesh%ptr_elem(iS+2)
         in3= st_local_mesh%ptr_elem(iS+3)
         in4= st_local_mesh%ptr_elem(iS+4)

         TERM= 0.25d0 * VC
         st_local_mesh%voln(in1)= st_local_mesh%voln(in1) + TERM
         st_local_mesh%voln(in2)= st_local_mesh%voln(in2) + TERM
         st_local_mesh%voln(in3)= st_local_mesh%voln(in3) + TERM
         st_local_mesh%voln(in4)= st_local_mesh%voln(in4) + TERM
      enddo

      do icel0= 1, st_local_mesh%n_ACTprism_351
        icel= st_local_mesh%ACTprism_351_id(icel0)
          VC= st_local_mesh%volc (icel)

         iS = st_local_mesh%index_elem(icel-1)
         in1= st_local_mesh%ptr_elem(iS+1)
         in2= st_local_mesh%ptr_elem(iS+2)
         in3= st_local_mesh%ptr_elem(iS+3)
         in4= st_local_mesh%ptr_elem(iS+4)
         in5= st_local_mesh%ptr_elem(iS+5)
         in6= st_local_mesh%ptr_elem(iS+6)

         TERM= ppohFVM_O6th * VC
         st_local_mesh%voln(in1)= st_local_mesh%voln(in1) + TERM
         st_local_mesh%voln(in2)= st_local_mesh%voln(in2) + TERM
         st_local_mesh%voln(in3)= st_local_mesh%voln(in3) + TERM
         st_local_mesh%voln(in4)= st_local_mesh%voln(in4) + TERM
         st_local_mesh%voln(in5)= st_local_mesh%voln(in5) + TERM
         st_local_mesh%voln(in6)= st_local_mesh%voln(in6) + TERM
      enddo

      do icel0= 1, st_local_mesh%n_ACThexa_361
        icel= st_local_mesh%ACThexa_361_id(icel0)
          VC= st_local_mesh%volc (icel)

         iS = st_local_mesh%index_elem(icel-1)
         in1= st_local_mesh%ptr_elem(iS+1)
         in2= st_local_mesh%ptr_elem(iS+2)
         in3= st_local_mesh%ptr_elem(iS+3)
         in4= st_local_mesh%ptr_elem(iS+4)
         in5= st_local_mesh%ptr_elem(iS+5)
         in6= st_local_mesh%ptr_elem(iS+6)
         in7= st_local_mesh%ptr_elem(iS+7)
         in8= st_local_mesh%ptr_elem(iS+8)

         TERM= ppohFVM_O8th * VC
         st_local_mesh%voln(in1)= st_local_mesh%voln(in1) + TERM
         st_local_mesh%voln(in2)= st_local_mesh%voln(in2) + TERM
         st_local_mesh%voln(in3)= st_local_mesh%voln(in3) + TERM
         st_local_mesh%voln(in4)= st_local_mesh%voln(in4) + TERM
         st_local_mesh%voln(in5)= st_local_mesh%voln(in5) + TERM
         st_local_mesh%voln(in6)= st_local_mesh%voln(in6) + TERM
         st_local_mesh%voln(in7)= st_local_mesh%voln(in7) + TERM
         st_local_mesh%voln(in8)= st_local_mesh%voln(in8) + TERM
      enddo
            
      do ie= 1, st_edge_info%n_edge
        st_edge_info%area(4,ie)= dsqrt(st_edge_info%area(1,ie)**2+            &
     &                              st_edge_info%area(2,ie)**2+            &
     &                              st_edge_info%area(3,ie)**2)    
      enddo
!C===

!C
!C-- COMMUNICATION
      call ppohFVM_update_1_R (st_comm_info, st_local_mesh%voln, st_local_mesh%n_node, st_local_mesh%n_internal)

      return
      end

