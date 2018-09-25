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

      subroutine ppohFVM_INPUT_GRID_b (st_local_mesh, st_grp_data, st_comm_info, st_file_info)

      use m_ppohFVM_util
      implicit REAL*8 (A-H,O-Z)

      integer(kind=ppohFVM_kint):: errno
      integer(kind=ppohFVM_kint):: NODarray, CELarray

      type (st_ppohFVM_local_mesh) :: st_local_mesh
      type (st_ppohFVM_grp_data)   :: st_grp_data
      type (st_ppohFVM_comm_info)  :: st_comm_info
      type (st_ppohFVM_file_info)  :: st_file_info

      character*1 LINE(132)

      IUNIT= 11
      open (IUNIT,file= st_file_info%file(1), status='unknown', form='unformatted')
!C
!C-- Parallel info.

        read (IUNIT) st_local_mesh%CoarseGridLevels
        read (IUNIT) st_local_mesh%HOWmanyADAPTATIONs
        read (IUNIT) myID
        read (IUNIT) st_comm_info%n_neighbor_pe

      allocate (st_comm_info%neighbor_pe                              &
     &                        (st_comm_info%n_neighbor_pe))
        read (IUNIT) (st_comm_info%neighbor_pe(k),                    &
     &                k= 1, st_comm_info%n_neighbor_pe)

        read (IUNIT) st_local_mesh%n_material

      NEIBPETOT= st_comm_info%n_neighbor_pe

!C
!C-- NODE info.
        read (IUNIT) st_local_mesh%n_node, st_local_mesh%n_internal

        NODarray= st_local_mesh%n_node

        allocate (st_local_mesh%node(3,NODarray))
        allocate (st_local_mesh%WhenIwasRefined_node(NODarray))
        allocate (st_local_mesh%node_id(NODarray,2))

        do i= 1, st_local_mesh%n_node
          read (IUNIT)                                                 &
     &          st_local_mesh%node_id(i,1),st_local_mesh%node_id(i,2), &
     &          st_local_mesh%WhenIwasRefined_node(i),                 &
     &         (st_local_mesh%node(k,i),k =1,3)            
        enddo
!C
!C-- ELEMENT Info.
        read (IUNIT) st_local_mesh%n_elem, st_local_mesh%ne_internal

        CELarray= st_local_mesh%n_elem

        allocate (st_local_mesh%elem_id  (CELarray,2))

        if (st_local_mesh%n_material.ne.0) then
          allocate (st_local_mesh%material(CELarray,st_local_mesh%n_material))
        endif
        allocate (st_local_mesh%mat_id   (CELarray  ))
        allocate (st_local_mesh%elem_type(CELarray  ))
        allocate (st_local_mesh%ptr_elem   (CELarray*8))
        allocate (st_local_mesh%index_elem    (0:CELarray))
        allocate (st_local_mesh%index_children(0:CELarray))

        allocate (st_local_mesh%WhenIwasRefined_elem(CELarray))
        allocate (st_local_mesh%ne_internal_list(CELarray  ))

        allocate (st_local_mesh%adaptation_parent_type (CELarray))

        allocate (st_local_mesh%adaptation_type (CELarray))
        allocate (st_local_mesh%adaptation_level(CELarray))

        allocate (st_local_mesh%adaptation_parent  (CELarray,2))
        allocate (st_local_mesh%adaptation_children(CELarray*8,2))

        read (IUNIT) (st_local_mesh%elem_type(i), i= 1, st_local_mesh%n_elem)


        st_local_mesh%index_elem    = 0
        st_local_mesh%index_children= 0
        do icel= 1, st_local_mesh%n_elem
          ityp= st_local_mesh%elem_type(icel)

          if (ityp.eq.341) then
            icon= 4
            ichi= 8
          endif

          if (ityp.eq.351) then
            icon= 6
            ichi= 4
          endif

          if (ityp.eq.361) then
            icon=  8
            ichi=  0
          endif

          st_local_mesh%index_elem(icel)=                              &
     &          st_local_mesh%index_elem(icel-1) + icon

          st_local_mesh%index_children(icel)=                          &
     &          st_local_mesh%index_children(icel-1) + ichi
        enddo

        if (st_local_mesh%n_material.eq.0) then
          do icel= 1, st_local_mesh%n_elem
            iS  = st_local_mesh%index_elem(icel-1)
            icon= st_local_mesh%index_elem(icel) -                       &
     &            st_local_mesh%index_elem(icel-1)
            read (IUNIT)                                                 &
     &            st_local_mesh%elem_id(icel,1),                         &
     &            st_local_mesh%elem_id(icel,2),                         &
     &            st_local_mesh%WhenIwasRefined_elem(icel),              &
     &            st_local_mesh%mat_id(icel),                            &
     &           (st_local_mesh%ptr_elem(k),k=iS+1, iS+icon)          
            do kk= iS+1, iS+icon
              in= st_local_mesh%ptr_elem(kk)
              if (in.le.0) call ppohFVM_error_exit(1005)
            enddo
          enddo
         else
          do icel= 1, st_local_mesh%n_elem
            iS  = st_local_mesh%index_elem(icel-1)
            icon= st_local_mesh%index_elem(icel) -                       &
     &            st_local_mesh%index_elem(icel-1)
            read (IUNIT)                                                 &
     &            st_local_mesh%elem_id(icel,1),                         &
     &            st_local_mesh%elem_id(icel,2),                         &
     &            st_local_mesh%WhenIwasRefined_elem(icel),              &
     &            st_local_mesh%mat_id(icel),                            &
     &           (st_local_mesh%ptr_elem(k),k=iS+1, iS+icon),            &
     &           (st_local_mesh%material(icel,kk),kk= 1, st_local_mesh%n_material)
            do kk= iS+1, iS+icon
              in= st_local_mesh%ptr_elem(kk)
              if (in.le.0) call ppohFVM_error_exit(1005)
            enddo
          enddo
        endif
!C
!C-- ADAPTATION info.
        do icel= 1, st_local_mesh%n_elem

          iS  = st_local_mesh%index_children(icel-1)
          icon= st_local_mesh%index_children(icel) -                   &
     &          st_local_mesh%index_children(icel-1)

          read (IUNIT)                                                 &
     &          st_local_mesh%adaptation_type  (icel),                 &
     &          st_local_mesh%adaptation_level (icel),                 &
     &          st_local_mesh%adaptation_parent(icel,1),               &
     &          st_local_mesh%adaptation_parent(icel,2),               &
     &          st_local_mesh%adaptation_parent_type(icel),            &
     &        ((st_local_mesh%adaptation_children(k,kk),               &
     &          kk=1,2), k=iS+1, iS+icon)          
        enddo

        read (IUNIT)                                                   &
     &       (st_local_mesh%ne_internal_list(i),                       &
     &        i= 1,st_local_mesh%ne_internal)


!C
!C-- IMPORT/EXPORT
      allocate (st_comm_info%import_index(0:NEIBPETOT))
      allocate (st_comm_info%export_index(0:NEIBPETOT))

      st_comm_info%import_index(0)= 0
      st_comm_info%export_index(0)= 0

        read (IUNIT) (st_comm_info%import_index(k), k= 1, NEIBPETOT)
      nn= st_comm_info%import_index(NEIBPETOT)
      allocate (st_comm_info%import_item(nn))

      if (NEIBPETOT.ne.0) then 
        read (IUNIT) (st_comm_info%import_item(i), i= 1, nn)
      endif

      read (IUNIT) (st_comm_info%export_index(k), k= 1, NEIBPETOT)
      nn= st_comm_info%export_index(NEIBPETOT)
      allocate (st_comm_info%export_item(nn))

      if (NEIBPETOT.ne.0) then 
        read (IUNIT) (st_comm_info%export_item(i), i= 1, nn)
      endif

!C
!C-- BOUNDARY Info. : NODE group
        read  (IUNIT) st_grp_data%node_grp%n_enum_grp
          N1= st_grp_data%node_grp%n_enum_grp
          allocate (st_grp_data%node_grp%enum_grp_name(N1))
          allocate (st_grp_data%node_grp%enum_grp_index(0:N1))
          st_grp_data%node_grp%enum_grp_index(0)= 0

        if (st_grp_data%node_grp%n_enum_grp.ne.0) then
          read  (IUNIT)                                                &
     &          (st_grp_data%node_grp%enum_grp_index(ig),              &
     &           ig= 1,st_grp_data%node_grp%n_enum_grp)
          N2= st_grp_data%node_grp%enum_grp_index(N1)
          allocate (st_grp_data%node_grp%enum_grp_node(N2))


          do ig= 1, st_grp_data%node_grp%n_enum_grp
            read  (IUNIT)                                              &
     &             st_grp_data%node_grp%enum_grp_name(ig)

              nn= st_grp_data%node_grp%enum_grp_index(ig) -            &
     &            st_grp_data%node_grp%enum_grp_index(ig-1)
              if (nn.ne.0) then
              read (IUNIT)                                             &
     &             (st_grp_data%node_grp%enum_grp_node(is),            &
     &              is= st_grp_data%node_grp%enum_grp_index(ig-1)+1,   &
     &                  st_grp_data%node_grp%enum_grp_index(ig))
              endif
          enddo
        endif

!C
!C-- BOUNDARY Info. : ELEMENT group
        read  (IUNIT) st_grp_data%elem_grp%n_enum_grp
          N1= st_grp_data%elem_grp%n_enum_grp
          allocate (st_grp_data%elem_grp%enum_grp_name(N1))
          allocate (st_grp_data%elem_grp%enum_grp_index(0:N1))
          st_grp_data%elem_grp%enum_grp_index(0)= 0

        if (st_grp_data%elem_grp%n_enum_grp.ne.0) then
          read  (IUNIT)                                                &
     &          (st_grp_data%elem_grp%enum_grp_index(ig),              &
     &           ig= 1,st_grp_data%elem_grp%n_enum_grp)
          N2= st_grp_data%elem_grp%enum_grp_index(N1)
          allocate (st_grp_data%elem_grp%enum_grp_node(N2))


          do ig= 1, st_grp_data%elem_grp%n_enum_grp
            read  (IUNIT)                                              &
     &             st_grp_data%elem_grp%enum_grp_name(ig)

              nn= st_grp_data%elem_grp%enum_grp_index(ig) -            &
     &            st_grp_data%elem_grp%enum_grp_index(ig-1)
              if (nn.ne.0) then
              read (IUNIT)                                             &
     &             (st_grp_data%elem_grp%enum_grp_node(is),            &
     &              is= st_grp_data%elem_grp%enum_grp_index(ig-1)+1,   &
     &                  st_grp_data%elem_grp%enum_grp_index(ig))
              endif
          enddo
        endif

!C
!C-- BOUNDARY Info. : SURFACE group
        read  (IUNIT) st_grp_data%surf_grp%n_surf_grp
          N1= st_grp_data%surf_grp%n_surf_grp
          allocate (st_grp_data%surf_grp%surf_grp_name(N1))
          allocate (st_grp_data%surf_grp%surf_grp_index(0:N1))
          st_grp_data%surf_grp%surf_grp_index(0)= 0

        if (st_grp_data%surf_grp%n_surf_grp.ne.0) then
          read  (IUNIT)                                                &
     &          (st_grp_data%surf_grp%surf_grp_index(ig),              &
     &           ig= 1,st_grp_data%surf_grp%n_surf_grp)
          N2= st_grp_data%surf_grp%surf_grp_index(N1)
          allocate (st_grp_data%surf_grp%surf_grp_node(N2,2))


          do ig= 1, st_grp_data%surf_grp%n_surf_grp
            read  (IUNIT)                                              &
     &             st_grp_data%surf_grp%surf_grp_name(ig)

              nn= st_grp_data%surf_grp%surf_grp_index(ig) -            &
     &            st_grp_data%surf_grp%surf_grp_index(ig-1)
              if (nn.ne.0) then
              read (IUNIT)                                             &
     &             (st_grp_data%surf_grp%surf_grp_node(is,1),          &
     &              is= st_grp_data%surf_grp%surf_grp_index(ig-1)+1,   &
     &                  st_grp_data%surf_grp%surf_grp_index(ig))
              read (IUNIT)                                             &
     &             (st_grp_data%surf_grp%surf_grp_node(is,2),          &
     &              is= st_grp_data%surf_grp%surf_grp_index(ig-1)+1,   &
     &                  st_grp_data%surf_grp%surf_grp_index(ig))
              endif
          enddo
        endif

      close (IUNIT)

      st_local_mesh%n_node_global= st_local_mesh%n_internal 
      call ppohFVM_Allreduce_I (st_comm_info, st_local_mesh%n_node_global, ppohFVM_sum)

      ppohFVM_O3rd= 1.d0/3.d0
      ppohFVM_O6th= 1.d0/6.d0
      ppohFVM_O8th= 0.125d0

      return

 998  continue
      call ppohFVM_error_exit (11)

 999  continue
      call ppohFVM_error_exit (12)

      end subroutine ppohFVM_INPUT_GRID_b
