!C
!C***
!C*** INPUT_CNTL
!C***
!C
      subroutine INPUT_CNTL (file_info, comm_info, edge_info, 
     &                       matrix_info, solver_info,
     &                       vis_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix
      use pfem_util

      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_file_info)   ::   file_info
      type (st_ppohFVM_comm_info)   ::   comm_info
      type (st_ppohFVM_edge_info)   ::   edge_info
      type (st_ppohFVM_matrix_info) :: matrix_info
      type (st_ppohFVM_solver_info) :: solver_info
      type (st_ppohFVM_vis_info)    ::    vis_info

      if (my_rank.eq.0) then
        open (11,file= 'INPUT.DAT', status='unknown')
        read (11,'(a80)') HEADER
        read (11,*) ITER
        read (11,*) COND, QVOL
        read (11,*) RESID
        read (11,*) file_info%mesh_asci
        read (11,*) vis_info%n_cell_ucd_reg_hexa_361_1
        close (11)

        write (*,'(a80)') HEADER
        write (*,*) file_info%mesh_asci
        write (*,*) ' '
      endif

      call ppohFVM_Bcast_C (comm_info, HEADER, 80, 0)
      call ppohFVM_Bcast_I (comm_info, ITER , 0)
      call ppohFVM_Bcast_R (comm_info, COND , 0)
      call ppohFVM_Bcast_R (comm_info, QVOL , 0)
      call ppohFVM_Bcast_R (comm_info, RESID, 0)
      call ppohFVM_Bcast_L (comm_info, file_info%mesh_asci, 0)
      call ppohFVM_Bcast_I (comm_info, 
     &                      vis_info%n_cell_ucd_reg_hexa_361_1, 0)

      edge_info%use_edges= .false.

      solver_info%RESID= RESID
      solver_info%ITER = ITER

      matrix_info%TYPE               = 2
      matrix_info%BLOCKsize          = 1
      matrix_info%DomainDecomposition= 0
      solver_info%PRECOND            = 1

      return
      end
