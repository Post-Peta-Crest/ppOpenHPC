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
!C*** ppohFVM_mat_ass_color
!C***
!C
      subroutine ppohFVM_mat_ass_color (st_local_mesh, st_comm_info, st_matrix_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix

      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh)  :: st_local_mesh
      type (st_ppohFVM_comm_info)   :: st_comm_info
      type (st_ppohFVM_matrix_info) :: st_matrix_info

      integer, dimension(:,:), allocatable :: IWKX
!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      N = st_local_mesh%n_internal
      NP= st_local_mesh%n_node
!C===

!C
!C +--------------+
!C | HEX coloring |
!C +--------------+
!C===
      nnp= max(NP,st_local_mesh%n_ACThexa_361)

      allocate (st_matrix_info%hexa_361_color_index(0:st_local_mesh%n_ACThexa_361))
      allocate (st_matrix_info%hexa_361_color_item (st_local_mesh%n_ACThexa_361))
      allocate (IWKX(nnp,3))

      IWKX= 0
      icou= 0
      do icol= 1, st_local_mesh%n_ACThexa_361
        do i= 1, NP
          IWKX(i,1)= 0
        enddo
        do icel0= 1, st_local_mesh%n_ACThexa_361
          if (IWKX(icel0,2).eq.0) then
            icel= st_local_mesh%ACThexa_361_id(icel0)
             in1= st_local_mesh%ptr_elem(8*icel-7)
             in2= st_local_mesh%ptr_elem(8*icel-6)
             in3= st_local_mesh%ptr_elem(8*icel-5)
             in4= st_local_mesh%ptr_elem(8*icel-4)
             in5= st_local_mesh%ptr_elem(8*icel-3)
             in6= st_local_mesh%ptr_elem(8*icel-2)
             in7= st_local_mesh%ptr_elem(8*icel-1)
             in8= st_local_mesh%ptr_elem(8*icel  )

             ip1= IWKX(in1,1)
             ip2= IWKX(in2,1)
             ip3= IWKX(in3,1)
             ip4= IWKX(in4,1)
             ip5= IWKX(in5,1)
             ip6= IWKX(in6,1)
             ip7= IWKX(in7,1)
             ip8= IWKX(in8,1)

            isum= ip1 + ip2 + ip3 + ip4 + ip5 + ip6 + ip7 + ip8
            if (isum.eq.0) then 
              icou= icou + 1
              IWKX(icol, 3)= icou
              IWKX(icel0,2)= icol
              st_matrix_info%hexa_361_color_item(icou)= icel

              IWKX(in1,1)= 1
              IWKX(in2,1)= 1
              IWKX(in3,1)= 1
              IWKX(in4,1)= 1
              IWKX(in5,1)= 1
              IWKX(in6,1)= 1
              IWKX(in7,1)= 1
              IWKX(in8,1)= 1
              if (icou.eq.st_local_mesh%n_ACThexa_361) goto 100            
            endif
          endif
        enddo
      enddo

 100  continue

      st_matrix_info%hexa_361_color_tot= icol
      IWKX(0                        ,3)= 0
      IWKX(st_matrix_info%hexa_361_color_tot,3)= st_local_mesh%n_ACThexa_361

      do icol= 0, st_matrix_info%hexa_361_color_tot
        st_matrix_info%hexa_361_color_index(icol)= IWKX(icol,3)
      enddo

      if (st_comm_info%my_rank.eq.0) then
        write (*,'(a,2i8)') '### Number of Element Colors (HEXA)', st_matrix_info%hexa_361_color_tot
      endif
      deallocate (IWKX)
!C===
      return
      end
