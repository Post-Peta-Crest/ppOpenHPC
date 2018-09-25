      program heat3Dp

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix

      use pfem_util

      implicit REAL*8(A-H,O-Z) 
      type (st_ppohFVM_file_info)   ::   file_info
      type (st_ppohFVM_local_mesh)  ::  local_mesh
      type (st_ppohFVM_grp_data)    ::    grp_data
      type (st_ppohFVM_comm_info)   ::   comm_info
      type (st_ppohFVM_edge_info)   ::   edge_info
      type (st_ppohFVM_matrix_info) :: matrix_info
      type (st_ppohFVM_solver_info) :: solver_info
      type (st_ppohFVM_vis_info)    ::    vis_info
!C
!C +-------+
!C | INIT. |
!C +-------+
!C=== 
      call ppohFVM_Init (file_info, comm_info, edge_info)

      PETOT   = comm_info%PETOT
      PEsmpTOT= comm_info%PEsmpTOT
      my_rank = comm_info%my_rank

      call INPUT_CNTL (file_info, comm_info, edge_info, matrix_info,    &
     &                 solver_info, vis_info)
      call ppohFVM_dist_file (file_info, comm_info, 
     &                        HEADER, 80, 0, my_rank, 1)

      call ppohFVM_pre (file_info, local_mesh, grp_data, comm_info, 
     &                                                   edge_info)
      call LOAD_MESH   (           local_mesh, grp_data, comm_info)
!C===

!C
!C +---------------------+
!C | matrix connectivity |
!C +---------------------+
!C===
      START_TIME= MPI_WTIME()
      call ppohFVM_mat_con       (local_mesh, comm_info, matrix_info)
      call ppohFVM_mat_ass_color (local_mesh, comm_info, matrix_info)
      END_TIME= MPI_WTIME()
      if (comm_info%my_rank.eq.0) then
        write (*, '("*** matrix conn. ", 1pe16.6, " sec.")')            &
     &         END_TIME-START_TIME
      endif
!C===
      
!C
!C +-----------------+
!C | MATRIX assemble |
!C +-----------------+
!C===
      START_TIME= MPI_WTIME()

      call MAT_ASS_MAIN (local_mesh, matrix_info)
      call MAT_ASS_BC   (local_mesh, matrix_info)

      END_TIME= MPI_WTIME()
      if (my_rank.eq.0) then
        write (*, '("*** matrix ass.  ", 1pe16.6, " sec.",/)')          &
     &         END_TIME-START_TIME
      endif
!C===

!C
!C +--------+
!C | SOLVER |
!C +--------+
!C===
      START_TIME= MPI_WTIME()
      call ppohFVM_solver11 (local_mesh, comm_info, matrix_info, 
     &                                              solver_info)
      END_TIME= MPI_WTIME()

      if (my_rank.eq.0) then
        write (*,'(1pe16.6)') matrix_info%X(1) 
        write (*, '("*** real  COMP.  ", 1pe16.6, " sec.",/)')          &
     &         END_TIME-START_TIME
      endif
!C===

!C
!C +--------+
!C | OUTPUT |
!C +--------+
!C===
      call ppohFVM_ucd_regular_hexa_1 (local_mesh, comm_info,           &
     &                                 matrix_info, vis_info)
      if (my_rank.eq.0) then
         write (*,'(1pe16.6)') matrix_info%X(1) 
      endif

!C===
      call ppohFVM_Finalize (comm_info)

      end program heat3Dp
