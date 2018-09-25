!C
!C***
!C*** LOAD_MESH
!C***
!C
!C    init. mesh arrays and generates EDGEs
!C
      subroutine LOAD_MESH (local_mesh, grp_data, comm_info)

      use m_ppohFVM_util
      use pfem_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh) :: local_mesh
      type (st_ppohFVM_grp_data)   ::   grp_data
      type (st_ppohFVM_comm_info)  ::  comm_info

!C
!C +-----------------------------+
!C | POINTER copy and allocation |
!C +-----------------------------+
!C===

!C
!C-- Parallel Info.
      NEIBPETOT =  comm_info%n_neighbor_pe
      NEIBPE    => comm_info%neighbor_pe

      IMPORT_INDEX => comm_info%import_index
      IMPORT_ITEM  => comm_info%import_item
      EXPORT_INDEX => comm_info%export_index
      EXPORT_ITEM  => comm_info%export_item

!C
!C-- MESH
      NP= local_mesh%n_node
      N = local_mesh%n_internal
      ICELTOT    = local_mesh%n_elem
      ICELTOT_INT= local_mesh%ne_internal

      NODE_ID   => local_mesh%node_id
      ELEM_ID   => local_mesh%elem_id

      intELEM_list => local_mesh%ne_internal_list

      allocate (XYZ(NP,3))
      do i= 1, NP
        XYZ(i,1)= local_mesh%node(1,i)
        XYZ(i,2)= local_mesh%node(2,i)
        XYZ(i,3)= local_mesh%node(3,i)
      enddo
      
      allocate (ICELNOD(ICELTOT,8))
      do icel= 1, ICELTOT
        ICELNOD(icel,1)= local_mesh%ptr_elem(8*icel-7)
        ICELNOD(icel,2)= local_mesh%ptr_elem(8*icel-6)
        ICELNOD(icel,3)= local_mesh%ptr_elem(8*icel-5)
        ICELNOD(icel,4)= local_mesh%ptr_elem(8*icel-4)
        ICELNOD(icel,5)= local_mesh%ptr_elem(8*icel-3)
        ICELNOD(icel,6)= local_mesh%ptr_elem(8*icel-2)
        ICELNOD(icel,7)= local_mesh%ptr_elem(8*icel-1)
        ICELNOD(icel,8)= local_mesh%ptr_elem(8*icel-0)
      enddo

      if (N.le.0)       call ppohFVM_error_exit(1001)
      if (ICELTOT.le.0) call ppohFVM_error_exit(1001)

      NODGRP_NAME => grp_data%node_grp%enum_grp_name
      NODGRP_ITEM => grp_data%node_grp%enum_grp_node
      NODGRP_INDEX=> grp_data%node_grp%enum_grp_index

      NODGRPtot = grp_data%node_grp%n_enum_grp
!C===
      return

 998  continue
      call ppohFVM_error_exit (11)

 999  continue
      call ppohFVM_error_exit (12)

      end subroutine LOAD_MESH
