!C
!C***
!C*** MAT_ASS_BC
!C***
!C
      subroutine MAT_ASS_BC (local_mesh, matrix_info)

      use m_ppohFVM_util
      use m_ppohFVM_util_matrix
      use pfem_util
      implicit REAL*8 (A-H,O-Z)

      type (st_ppohFVM_local_mesh)  ::  local_mesh
      type (st_ppohFVM_matrix_info) :: matrix_info

      allocate (IWKX(NP,2))
      IWKX= 0

!C
!C== Z=Zmax

!$omp parallel do private(in)
      do in= 1, NP
        IWKX(in,1)= 0
      enddo

      ib0= -1
      do ib0= 1, NODGRPtot
        if (NODGRP_NAME(ib0).eq.'Zmax') exit
      enddo

!$omp parallel do private(ib,in)
      do ib= NODGRP_INDEX(ib0-1)+1, NODGRP_INDEX(ib0)
        in= NODGRP_ITEM(ib)
        IWKX(in,1)= 1
      enddo

!$omp parallel do private(in,iS,iE,k)
      do in= 1, NP
        if (IWKX(in,1).eq.1) then
          matrix_info%RHS(in)= 0.d0
          matrix_info%D(in)  = 1.d0
          
          iS= matrix_info%index(in-1) + 1
          iE= matrix_info%index(in  )
          do k= iS, iE
            matrix_info%AMAT(k)= 0.d0
          enddo
        endif
      enddo

!$omp parallel do private(in,iS,iE,k)
      do in= 1, NP
        iS= matrix_info%index(in-1) + 1
        iE= matrix_info%index(in  )
        do k= iS, iE
          if (IWKX(matrix_info%item(k),1).eq.1) then
            matrix_info%AMAT(k)= 0.d0
          endif
        enddo
      enddo
!C==
      return
      end
