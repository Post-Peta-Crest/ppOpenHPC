!#######################################################################
!##########                                                   ##########
!##########    ����NAVIER-STOKES�������̗L���v�f���        ##########
!##########                   �T���v���v���O����              ##########
!##########                                                   ##########
!#######################################################################
!						Copyright    K. Hatanaka
!        �v���O������                   :   SAMPLE.FOR
!        ���b�V���f�[�^�t�@�C����       :   MESH.DAT     (UNIT=10)
!        ���E�����t�@�C����             :   BC.DAT       (UNIT=11)
!        ���������t�@�C����             :   INIT.DAT     (UNIT=12)
!        �v�Z�����t�@�C����             :   INPUT.DAT    (UNIT=15)
!        �v�Z���ʏo�̓t�@�C����         :   OUTPUT.DAT   (UNIT=20)
!
!
!#######################################################################

!  Original program
!  Copyright (c) 1998 HATANAKA Katsumori
!
!  Modified by ppohFEM
!  Copyright (c) 2016 The University of Tokyo, 
!                     Graduate School of Frontier Science



program main

use ppohFEM

       implicit none

      real(8), pointer :: XY(:,:)            !(2,nnode)
      real(8), pointer :: UV1(:,:), UV2(:,:) !(2,nnode)
      real(8), pointer :: PP1(:),   PP2(:)
      real(8), pointer :: AMB(:)
!
      integer, pointer ::  NODC(:,:)                 !(3,nelem)
      real(8), pointer ::  BB(:,:), CC(:,:)          !(3,nelem)
      real(8), pointer ::  AREA(:), AA03(:), AA12(:) !(nelem)
!
      integer, pointer ::  IUB(:), IVB(:), IPB(:), IENT(:)
      real(8), pointer ::  FUB(:), FVB(:), FPB(:)
!
      real(8) :: delt, reiv
      integer :: ista, iend, iout, ifout
!
      integer :: NX, MX
      integer :: NUB, NVB, NPB, NENT
!
      integer :: istep, IFLG
      integer :: i,j
!
      real(8) :: Tstart, Tend

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! ppohFEM setup
!
        integer, parameter :: idx_mesh       = 1

        integer, parameter :: idx_mat_v      = 1
        integer, parameter :: idx_mat_p      = 2
        integer, parameter :: idx_mat_p_org  = 3

        integer, parameter :: idx_result_data = 1

        integer :: nnode, nelem

        integer :: itype, ic_type, nn, iS, iE, icel, inode

        integer :: node_item_ndof(3)
        character(len=PPOHFEM_NAME_LEN) :: node_item_label(3)

        ! =============== INITIALIZE ===================

        Tstart = 0.0
        Tend   = 0.0

        call ppohFEM_init

! read mesh 
        call ppohFEM_get_mesh(idx_mesh)
        nnode = ppohFEM_get_n_node(idx_mesh)
        nelem = ppohFEM_get_n_elem(idx_mesh)
        allocate(XY(2,nnode))
        allocate(NODC(3,nelem))
        allocate(AMB(nnode))
        allocate(BB(3,nelem), CC(3,nelem))
        allocate(AREA(nelem), AA03(nelem), AA12(nelem))

! matrix allocation
! for velocity
        call ppohFEM_nullify_matrix( idx_mat_v )
        call ppohFEM_mat_con(idx_mesh, idx_mat_v)
        call ppohFEM_set_mat_ndof(idx_mat_v, 2)  ! U, V
        call ppohFEM_mat_init( idx_mat_v )
        call ppohFEM_mat_clear(idx_mat_v)

! for pressure original matrix
        call ppohFEM_nullify_matrix( idx_mat_p_org )
        call ppohFEM_mat_con(idx_mesh, idx_mat_p_org)
        call ppohFEM_set_mat_ndof(idx_mat_p_org, 1)
        call ppohFEM_mat_init( idx_mat_p_org )
        call ppohFEM_mat_clear(idx_mat_p_org)

! for result
        call ppohFEM_visualize_init()
        node_item_ndof(1) = 1
        node_item_label(1) = 'U'
        node_item_ndof(2) = 1
        node_item_label(2) = 'V'
        node_item_ndof(3) = 1
        node_item_label(3) = 'P'
        call ppohFEM_visualize_result_data_init(idx_mesh, idx_result_data, node_item_ndof=node_item_ndof, node_item_label=node_item_label)

!
!     --- �v�Z�����̓��� ---
!
      CALL DATA05 (DELT,    REIV,    ISTA,    IEND,    IOUT, &
     &             IFOUT )
!
!     --- ���b�V���f�[�^�̓��� ---
!
      CALL MSDATA (NX,      MX,      XY,      NODC,  idx_mesh)

!
!     --- ���E�����f�[�^�̓��� ---
!
      CALL BDDATA (NUB,     NVB,     NPB,     NENT,  &
     &             IUB,     IVB,     IPB,     IENT,  &
     &             FUB,     FVB,     FPB, idx_mesh)
!
!     --- ���������̐ݒ� ---
!
      allocate(UV1(2,NX))
      allocate(UV2(2,NX))
      allocate(PP1(NX))
      allocate(PP2(NX))

      CALL INITIA (ISTA,    NX,      UV1,     PP2)
!
!     --- ��Ԋ֐��̔��W���̌v�Z ---
!
      CALL MAKABC (NX,      MX,      NODC,    XY,  &
     &             BB,      CC,      AMB,     AREA,  &
     &             AA03,    AA12)
!
!     --- ���v���V�A��Matrix�̍쐬 ---
!
      CALL SUPER  (MX, BB, CC, AREA, idx_mesh, idx_mat_p_org )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! setup solver parameters (they are copied to new matrix)
!
      call ppohFEM_solver_set_method(idx_mat_p_org, 1) !CG
      call ppohFEM_solver_set_precond(idx_mat_p_org, 1) !SSOR
      call ppohFEM_solver_set_num_iteration(idx_mat_p_org, 10000) !iteration count
      call ppohFEM_solver_set_residual(idx_mat_p_org, 1.0D-8) ! convergence criteria
      call ppohFEM_solver_set_iterlog(idx_mat_p_org, .false.) ! show iteration log
      call ppohFEM_solver_set_timelog(idx_mat_p_org, .false.) ! show time log


!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      Tstart = ppohFEM_Wtime()

!
!     --- ���Ԑϕ�LOOP ---
!==========================================
      DO  1000  ISTEP = ISTA ,  IEND
!==========================================
!
!     --- ���ԗ����̌v�Z ---
!
      CALL STEP01 (NX,      MX,      NODC,    BB,      CC, &
     &             AREA,    AA12,    UV1,     UV2, &
     &             AMB,     DELT,    REIV)
!
!     --- ���ԗ������E�����̃Z�b�g ---
!
      CALL VBOUND (NUB,     NVB,     IUB,     IVB,     FUB, &
     &             FVB,     NENT,    IENT,    UV2)

!
!     --- ���̓|�A�\�����̉׏d���̍쐬 ---
!
! copy original matrix to current matrix and setup equation
      call ppohFEM_mat_copy(idx_mesh, idx_mat_p_org, idx_mat_p)

      CALL STEP02 (NX,      MX,      NODC,    BB,      CC, &
     &             AA03,    UV2,     PP1,     DELT,    idx_mesh, idx_mat_p)
!
!     --- ���͋��E�����̐ݒ� ---
!
      do i=1, NPB
        call ppohFEM_mat_ass_bc(idx_mat_p, IPB(i), 1, FPB(i))
      end do
!
! *** ICCG ***
!
      call ppohFEM_solve_11(idx_mesh, idx_mat_p)

      do i=1, ppohFEM_get_n_node(idx_mesh)
        PP2(i) = ppohFEM_get_stiffMAT_X(idx_mat_p, i, 1)
      end do
!
      DO i=1,NPB
        PP2(IPB(i))=FPB(i)
      end do
!
!     --- �����̌v�Z ---
!
      UV1(:,:) = UV2(:,:)
!
      CALL STEP03 (NX,      MX,      NODC,    BB,      CC, &
     &             AA03,    UV1,     UV2,     PP2,     AMB, DELT)
! update vector
      call ppohFEM_update_1_R(idx_mesh, UV2(1,:), NX)
      call ppohFEM_update_1_R(idx_mesh, UV2(2,:), NX)
!
!     --- �������E�����̃Z�b�g ---
!
      CALL VBOUND (NUB,     NVB,     IUB,     IVB,     FUB, &
     &             FVB,     NENT,    IENT,    UV2)
!
!     --- �v�Z���ʂ̏o�́i�W���o�́j ---
!
      CALL OUTPUT (NX,      UV2,     PP2,      ISTEP,  IOUT, &
     &             DELT,    REIV)
!
!     --- �v�Z���ʂ̏o�́i�t�@�C���j ---
!
      CALL FILOUT (NX,      MX,      UV2,     PP2,      ISTEP,  IFOUT, &
     &             DELT,    REIV,    IFLG, XY, NODC)
!
!     --- ���Ԑϕ��̂��߂̏��������̓���ւ� ---
!
      UV1(:,:) = UV2(:,:)
      PP1(:)   = PP2(:) !CALL DCHNG1 (NX,      PP1,     PP2)
!
!     --------
 1000 CONTINUE
!     --------
!
      CLOSE(20)
!
        ! =============== FINALIZE =====================
      Tend = ppohFEM_Wtime()
      write(*,*) 'TOTAL TIME for ', IEND - ISTA + 1 , ' step (sec): ', Tend - Tstart

      call ppohFEM_visualize_finalize
      call ppohFEM_finalize

      STOP
!      END

contains

!====================================================================
      SUBROUTINE DATA05 (DELT,    REIV,    ISTA,    IEND,    IOUT, &
     &                   IFOUT )
!====================================================================
!     �v�Z�����̓��̓T�u���[�`��
!
!         DELT       �E�E�E�@�������ԑ�����
!         RE         �E�E�E�@REYNOLDS��
!         REIV       �E�E�E�@REYNOLDS���̋t��
!         ISTA       �E�E�E�@�v�Z�J�nSTEP��
!         IEND       �E�E�E�@�v�Z�I��STEP��
!         IOUT       �E�E�E�@�v�Z���ʏo��STEP���i�W���o�́j
!         IFOUT      �E�E�E�@�v�Z���ʏo��STEP���i�t�@�C���j
!
!====================================================================
!
      implicit none
!
      real(8), intent(OUT) :: DELT, REIV
      integer, intent(OUT) :: ISTA, IEND, IOUT, IFOUT
!
      real(8) :: RE
!
      OPEN(UNIT=15,FILE='input.dat',STATUS='OLD')
!
      READ(15,'(A80)') 
      READ(15,*)  DELT,    RE
      READ(15,*)  ISTA,    IEND,    IOUT,    IFOUT
!
      CLOSE(15)
!
      REIV = 1.0 / RE
!
      WRITE(6,601)  DELT,     RE,      ISTA,     IEND, &
     &              IOUT,     IFOUT
!
  601 FORMAT(1H ,//,10X,'TIME INCREMENT       ( DELT) = ',F20.8,/, &
     &              10X,'REYNOLDS NUMBER      (   RE) = ',F20.8,/, &
     &              10X,'FIRST STEP           ( ISTA) = ',I20,/, &
     &              10X,'LAST  STEP           ( IEND) = ',I20,/, &
     &              10X,'INTERVAL OF LIST 0UT ( IOUT) = ',I20,/, &
     &              10X,'INTERVAL OF FILE OUT (IFOUT) = ',I20,/)
!
      RETURN
      END subroutine

!====================================================================
      SUBROUTINE MSDATA (NX,      MX,      XY,      NODC, idx_mesh)
!====================================================================
!     ���b�V���f�[�^�̓��̓T�u���[�`��
!
!         NX         �E�E�E�@���ߓ_��
!         MX         �E�E�E�@���v�f��
!         XY         �E�E�E�@�ߓ_��X,Y���W
!         NODC       �E�E�E�@�v�f�̓K������
!
!====================================================================

      use ppohFEM

      implicit none

! argument
      integer, intent(OUT) :: NX, MX
      real(8), pointer, intent(OUT) :: XY(:,:)
      integer, pointer, intent(OUT) :: NODC(:,:)
      integer, intent(IN)  :: idx_mesh

! internal 
      integer :: i,j,k
      real(8) :: XYZ(3)


! get number of node and element
        NX=ppohFEM_get_n_node(idx_mesh)
        MX=ppohFEM_get_n_elem(idx_mesh)

! node loop for all
        do i=1,NX
          call ppohFEM_get_node_coord(idx_mesh, i, XYZ)
          XY(1,i)=XYZ(1)
          XY(2,i)=XYZ(2)
        end do

! element loop
           do i= 1, MX

!node loop in element
              do j=1,3 ! number of nodes in triangle element
                 k = ppohFEM_get_node_item_of_element(idx_mesh, i, j)   
                 NODC(j,i)=k
              enddo

           enddo

      RETURN
      END subroutine

!====================================================================
      SUBROUTINE BDDATA (NUB,     NVB,     NPB,     NENT,  &
     &                   IUB,     IVB,     IPB,     IENT, &
     &                   FUB,     FVB,     FPB,     idx_mesh)
!====================================================================
!     ���E�����̓��̓T�u���[�`��
!
!         NUB, NVB, NPB    �E�E�E�@U,V,P ��NON-SLIP���E������
!         IUB, IVB, IPB    �E�E�E�@U,V,P ��NON-SLIP���E�����ߓ_�ԍ�
!         FUB, FVB, FPB    �E�E�E�@U,V,P �̋��E�����l
!         NENT, IENT       �E�E�E�@�������E����(U�̂݁j
!                                ��X���������݂̗̂������E
!                                �iY������0�Ƃ����ꍇ�j
!
!====================================================================
!
      use ppohFEM

      implicit none
!
      integer, intent(OUT)          :: NENT, NUB, NVB, NPB
      integer, pointer, intent(OUT) :: IENT(:), IUB(:), IVB(:), IPB(:)
      real(8), pointer, intent(OUT) ::          FUB(:), FVB(:), FPB(:)

      character(PPOHFEM_NAME_LEN) :: node_grp_id_name
      integer :: node_grp_id, n_node

      integer, intent(IN) :: idx_mesh

      
!
!     --- ���E�����t�@�C���̓ǂݍ��� ---
!

! entry boundary condition
      node_grp_id_name = 'NGENTU'
      node_grp_id = ppohFEM_node_grp_name_to_id(idx_mesh, node_grp_id_name)
      n_node = ppohFEM_get_num_nodes_in_node_group(idx_mesh, node_grp_id)
      NENT = n_node
      allocate(IENT(NENT))
      do i=1, n_node
        IENT(i)=ppohFEM_get_node_item_in_node_group(idx_mesh, node_grp_id, i)
      end do

! U boundary condition
      node_grp_id_name = 'NGNSU'
      node_grp_id = ppohFEM_node_grp_name_to_id(idx_mesh, node_grp_id_name)
      n_node = ppohFEM_get_num_nodes_in_node_group(idx_mesh, node_grp_id)
      NUB = n_node
      allocate(IUB(NUB), FUB(NUB))
      do i=1, n_node
        IUB(i)=ppohFEM_get_node_item_in_node_group(idx_mesh, node_grp_id, i)
      end do

! V boundary condition
      node_grp_id_name = 'NGNSV'
      node_grp_id = ppohFEM_node_grp_name_to_id(idx_mesh, node_grp_id_name)
      n_node = ppohFEM_get_num_nodes_in_node_group(idx_mesh, node_grp_id)
      NVB = n_node
      allocate(IVB(NVB), FVB(NVB))
      do i=1, n_node
        IVB(i)=ppohFEM_get_node_item_in_node_group(idx_mesh, node_grp_id, i)
      end do

! pressure boundary condition
      node_grp_id_name = 'NGNSP'
      node_grp_id = ppohFEM_node_grp_name_to_id(idx_mesh, node_grp_id_name)
      n_node = ppohFEM_get_num_nodes_in_node_group(idx_mesh, node_grp_id)
      NPB = n_node
      allocate(IPB(NPB), FPB(NPB))
      do i=1, n_node
        IPB(i)=ppohFEM_get_node_item_in_node_group(idx_mesh, node_grp_id, i)
      end do

!
!     --- ZERO ���E�����l�̐ݒ� ---
!
      FUB(:)=0.0
      FVB(:)=0.0
      FPB(:)=0.0
!
      WRITE(6,601) NENT, NUB, NVB, NPB
!
  501 FORMAT(16I5)
  601 FORMAT(1H ,//, &
     &       10X,'TOTAL NUM. OF ENTRANCE B.C.       (NENT) =',I5,/, &
     &       10X,'TOTAL NUM. OF NON-SLIP B.C. (U)   ( NUB) =',I5,/, &
     &       10X,'TOTAL NUM. OF NON-SLIP B.C. (V)   ( NVB) =',I5,/, &
     &       10X,'TOTAL NUM. OF B.C. FOR PRESSURE   ( NPB) =',I5,//)
!
      RETURN
      END subroutine

!====================================================================
      SUBROUTINE INITIA (ISTA,    NX,    UV,    PP)
!====================================================================
!     ���������̐ݒ�T�u���[�`��
!
!         NX               �E�E�E�@���ߓ_��
!         ISTA             �E�E�E�@�v�Z�J�nSTEP��
!         UV, PP           �E�E�E�@U,V,P �̏����ݒ�l
!
!====================================================================
!
      implicit none
!
      integer, intent(in) :: ista ! initial step (?)
      integer, intent(in) :: NX   ! number of local node
      real(8), pointer, intent(OUT) ::  UV(:,:), PP(:) ! local node index

      integer :: ISTA1
      integer :: n_global_node
      real(8), allocatable :: U_global(:), V_global(:), P_global(:)
!
      IF (ISTA .NE. 1)  GOTO 1
!
!     --- �ϐ��̃[���N���A�[ ---
!
      UV(:,:) = 0.0
      PP(:)=0.0 
      GOTO 2 
!
!     --- �����������t�@�C�����ǂݍ��� ---
!
    1 ISTA1 = ISTA - 1

      n_global_node = ppohFEM_get_n_global_node(idx_mesh)
      allocate(U_global(n_global_node))
      allocate(V_global(n_global_node))
      allocate(P_global(n_global_node))

      OPEN(UNIT=12,FILE='init.dat',STATUS='OLD')
!
  100 READ(12,501,END=999) ISTEP
      READ(12,502) (I,U_global(I), V_global(I), P_global(I), J=1,n_global_node)
      IF (ISTEP .NE. ISTA1)  GOTO 100
      CLOSE(12)

      do i=1, NX
        UV(1,i) = U_global(ppohFEM_get_global_node_ID(idx_mesh, i))
        UV(2,i) = V_global(ppohFEM_get_global_node_ID(idx_mesh, i))
        PP(i)   = P_global(ppohFEM_get_global_node_ID(idx_mesh, i))
      end do

      GOTO 2 
!
  999 WRITE(6,601)
  501 FORMAT(I10)
  502 FORMAT(I5,3E15.0)
  601 FORMAT(1H1,//,10X,'*******************************',/, &
     &              10X,'***    STOP-->SUB.INITIA    ***',/, &
     &              10X,'*******************************')
      STOP
!
    2 RETURN
      END subroutine

!====================================================================
      SUBROUTINE MAKABC (NX,      MX,      NODC,    XY,      BB, &
     &                   CC,      AMB,     AREA,    AA03,    AA12)
!====================================================================
!     �v�f���Ƃ̕�Ԋ֐��̔��W���v�Z�T�u���[�`��
!
!         NX               �E�E�E�@���ߓ_��
!         MX               �E�E�E�@���v�f��
!         NODC             �E�E�E�@�v�f�̓K������
!         BB               �E�E�E�@��Ԋ֐��̔��W��(X)
!         CC               �E�E�E�@��Ԋ֐��̔��W��(Y)
!         AREA             �E�E�E�@�v�f�̖ʐ�
!         AA03, AA12       �E�E�E�@AREA/3, AREA/12
!         AMB              �E�E�E�@���ʍs��̏W�����s���
!                                  �Ίp�����̋t��
!
!====================================================================
!
      implicit none
!
      integer, intent(IN)  ::  NX, MX
      real(8), intent(IN)  ::  XY(:,:)
      real(8), intent(OUT) ::  AMB(:)
      integer, intent(IN)  ::  NODC(:,:)
      real(8), intent(OUT) ::  BB(:,:),     CC(:,:)
      real(8), intent(OUT) ::  AREA(:),     AA03(:),     AA12(:)
!
      integer :: I1, I2, I3
      real(8) :: X1, X2, X3, Y1, Y2, Y3, A2
!
      AMB(:)=0.0
!
      DO 100 IE = 1, MX
      I1 = NODC(1,IE)
      I2 = NODC(2,IE)
      I3 = NODC(3,IE)
      X1 = XY(1,I1)
      X2 = XY(1,I2)
      X3 = XY(1,I3)
      Y1 = XY(2,I1)
      Y2 = XY(2,I2)
      Y3 = XY(2,I3)
!
!     --- �ʐς̌v�Z ---
!
      A2 = X1 * (Y2 - Y3) + X2 * (Y3 - Y1) + X3 * (Y1 - Y2)
      AREA(IE) = A2 * 0.5
      AA03(IE) = AREA(IE) / 3.0
      AA12(IE) = AREA(IE) / 12.0
      A2 = 1.0 / A2
!
!     --- ���W���̌v�Z ---
!
      BB(1,IE) = (Y2 - Y3) * A2
      BB(2,IE) = (Y3 - Y1) * A2
      BB(3,IE) = (Y1 - Y2) * A2
      CC(1,IE) = (X3 - X2) * A2
      CC(2,IE) = (X1 - X3) * A2
      CC(3,IE) = (X2 - X1) * A2
!
!     --- �W�����s��̑Ίp�����̏d�ˍ��킹 ---
!
      AMB(I1) = AA03(IE) + AMB(I1)
      AMB(I2) = AA03(IE) + AMB(I2)
      AMB(I3) = AA03(IE) + AMB(I3)
  100 CONTINUE
!
!     --- �W�����s��̑Ίp�����̋t�� ---
!
      DO 200 I = 1, NX
      AMB(I) = 1.0 / AMB(I)
  200 CONTINUE
!
      RETURN
      END subroutine

!====================================================================
      SUBROUTINE STEP01 (NX,      MX,      NODC,    BB,      CC, &
     &                   AREA,    AA12,    UV1,     UV2,     AMB, &
     &                   DELT,    REIV)
!====================================================================
!     ���ԗ����̌v�Z�T�u���[�`��
!
!         NX       �@�E�E�E�@���ߓ_��
!         MX       �@�E�E�E�@���v�f��
!     �@�@UV1      �@�E�E�E�@���� U, V   �i���m�j
!     �@�@UV2      �@�E�E�E�@���ԗ����@�@�i���m�ϐ��j
!         AMB      �@�E�E�E�@���ʍs��̏W�����s��
!         NODC       �E�E�E�@�v�f�̓K������
!         BB         �E�E�E�@��Ԋ֐��̔��W��(X)
!         CC         �E�E�E�@��Ԋ֐��̔��W��(Y)
!         AREA       �E�E�E�@�v�f�̖ʐ�
!         AA03, AA12 �E�E�E�@AREA/3, AREA/12
!         DELT       �E�E�E�@�������ԑ�����
!         REIV       �E�E�E�@REYNOLDS���̋t��
!         
!====================================================================
!
      implicit none
!
      integer, intent(in)  :: NX, MX
      integer, intent(in)  :: NODC(:,:)
      real(8), intent(in)  :: BB(:,:), CC(:,:)
      real(8), intent(in)  :: AREA(:), AA12(:)
      real(8), intent(in)  :: UV1(:,:)
      real(8), intent(out) :: UV2(:,:)
      real(8), intent(in)  :: AMB(:)
      real(8), intent(in)  :: DELT, REIV
!
      integer :: I1, I2, I3
      real(8) :: A01, A12, BB1, BB2, BB3, CC1, CC2, CC3
      real(8) :: UU1, UU2, UU3, VV1, VV2, VV3
      real(8) :: U123, V123, BU, BV, CU, CV
      real(8) :: UKK1, UKK2, UKK3, VKK1, VKK2, VKK3
      real(8) :: AVIS, CBUV
      real(8) :: USS1, USS2, USS3
      real(8) :: VSS1, VSS2, VSS3
      real(8) :: SS

      UV2(:,:) = 0.0
!    
      DO 1000 IE = 1, MX
      I1 = NODC(1,IE)
      I2 = NODC(2,IE)
      I3 = NODC(3,IE)
      A01 = AREA(IE)
      A12 = AA12(IE)
      BB1 = BB(1,IE)
      BB2 = BB(2,IE)
      BB3 = BB(3,IE)
      CC1 = CC(1,IE)
      CC2 = CC(2,IE)
      CC3 = CC(3,IE)
      UU1 = UV1(1,I1)
      UU2 = UV1(1,I2)
      UU3 = UV1(1,I3)
      VV1 = UV1(2,I1)
      VV2 = UV1(2,I2)
      VV3 = UV1(2,I3)
!
!    --- �ڗ����̍쐬 ---
!
      U123 = UU1 + UU2 + UU3
      V123 = VV1 + VV2 + VV3
      BU = BB1 * UU1 + BB2 * UU2 + BB3 * UU3
      BV = BB1 * VV1 + BB2 * VV2 + BB3 * VV3
      CU = CC1 * UU1 + CC2 * UU2 + CC3 * UU3
      CV = CC1 * VV1 + CC2 * VV2 + CC3 * VV3
      UKK1 = (BU * (U123 + UU1) + CU * (V123 + VV1)) * A12
      UKK2 = (BU * (U123 + UU2) + CU * (V123 + VV2)) * A12
      UKK3 = (BU * (U123 + UU3) + CU * (V123 + VV3)) * A12
      VKK1 = (BV * (U123 + UU1) + CV * (V123 + VV1)) * A12
      VKK2 = (BV * (U123 + UU2) + CV * (V123 + VV2)) * A12
      VKK3 = (BV * (U123 + UU3) + CV * (V123 + VV3)) * A12
!
!    --- �S�����̍쐬 ---
!
      AVIS = A01 * REIV
      CBUV = CU + BV
      USS1 = (2.0 * BB1 * BU + CC1 * CBUV) * AVIS
      USS2 = (2.0 * BB2 * BU + CC2 * CBUV) * AVIS
      USS3 = (2.0 * BB3 * BU + CC3 * CBUV) * AVIS
      VSS1 = (2.0 * CC1 * CV + BB1 * CBUV) * AVIS
      VSS2 = (2.0 * CC2 * CV + BB2 * CBUV) * AVIS
      VSS3 = (2.0 * CC3 * CV + BB3 * CBUV) * AVIS
!
!    --- ���m���̏d�ˍ��킹 ---
!
      UV2(1,I1) = UKK1 + USS1 + UV2(1,I1)
      UV2(1,I2) = UKK2 + USS2 + UV2(1,I2)
      UV2(1,I3) = UKK3 + USS3 + UV2(1,I3)
      UV2(2,I1) = VKK1 + VSS1 + UV2(2,I1)
      UV2(2,I2) = VKK2 + VSS2 + UV2(2,I2)
      UV2(2,I3) = VKK3 + VSS3 + UV2(2,I3)
 1000 CONTINUE
!
!    --- ���ԗ����̌v�Z ---
!
      DO 2000 I = 1, NX
      SS = AMB(I) * DELT
      UV2(1,I) = UV1(1,I) - UV2(1,I) * SS
      UV2(2,I) = UV1(2,I) - UV2(2,I) * SS
 2000 CONTINUE
!
      RETURN
      END subroutine

!====================================================================
      SUBROUTINE STEP02 (NX,      MX,      NODC,    BB,      CC, &
     &                   AA03,    UV1,     PP,      DELT,    &
     &                   idx_mesh, idx_mat_p)
!====================================================================
!     ���̓|�A�\���������̉E�Ӂi�׏d���j�̌v�Z�T�u���[�`��
!
!         NX       �@�E�E�E�@���ߓ_��
!         MX       �@�E�E�E�@���v�f��
!     �@�@UV1      �@�E�E�E�@���ԗ���
!         PP         �E�E�E�@���̓|�A�\���������̉׏d��
!         NODC       �E�E�E�@�v�f�̓K������
!         BB         �E�E�E�@��Ԋ֐��̔��W��(X)
!         CC         �E�E�E�@��Ԋ֐��̔��W��(Y)
!         AA03       �E�E�E�@�v�f�̖ʐ�/3
!         DELT       �E�E�E�@�������ԑ�����
!         
!====================================================================
!
      use ppohFEM

      implicit none
!
      integer, intent(in)  :: NX, MX
      integer, intent(in)  :: NODC(:,:) ! NODC(3,*)
      real(8), intent(in)  :: UV1(:,:) ! UV1(2,*)
      real(8), intent(in)  :: AA03(:), BB(:,:), CC(:,:) ! AA03(*), BB(3,*), CC(3,*)
      real(8), intent(in)  :: delt

      real(8), intent(out) :: PP(:) 
      integer, intent(in)  :: idx_mesh, idx_mat_p

      integer :: I1, I2, I3
      real(8) :: DEDT, BB1, BB2, BB3, CC1, CC2, CC3, A03
      real(8) :: UU1, UU2, UU3, VV1, VV2, VV3, BU, CV, BCUV

      
!
      PP(:)=0.0
      DEDT = 1.0 / DELT
!
      DO 100 IE = 1, MX
      I1 = NODC(1,IE)
      I2 = NODC(2,IE)
      I3 = NODC(3,IE)
      BB1 = BB(1,IE)
      BB2 = BB(2,IE)
      BB3 = BB(3,IE)
      CC1 = CC(1,IE)
      CC2 = CC(2,IE)
      CC3 = CC(3,IE)
      A03 = AA03(IE)
      UU1 = UV1(1,I1)
      UU2 = UV1(1,I2)
      UU3 = UV1(1,I3)
      VV1 = UV1(2,I1)
      VV2 = UV1(2,I2)
      VV3 = UV1(2,I3)
!
      BU = BB1 * UU1 + BB2 * UU2 + BB3 * UU3
      CV = CC1 * VV1 + CC2 * VV2 + CC3 * VV3
      BCUV = ( BU + CV ) * DEDT * A03
!
      PP(I1) = - BCUV + PP(I1)
      PP(I2) = - BCUV + PP(I2)
      PP(I3) = - BCUV + PP(I3)
  100 CONTINUE
!
      do i=1, ppohFEM_get_n_node(idx_mesh)
        call ppohFEM_set_stiffMAT_B_direct_index(idx_mat_p, i, PP(i))
      end do


      RETURN
      END subroutine

!====================================================================
      SUBROUTINE STEP03 (NX,      MX,      NODC,    BB,      CC, &
     &                   AA03,    UV1,     UV2,     PP,      AMB, &
     &                   DELT)
!====================================================================
!     �����̌v�Z�T�u���[�`��
!
!         NX       �@�E�E�E�@���ߓ_��
!         MX       �@�E�E�E�@���v�f��
!         NODC       �E�E�E�@�v�f�̓K������
!         BB         �E�E�E�@��Ԋ֐��̔��W��(X)
!         CC         �E�E�E�@��Ԋ֐��̔��W��(Y)
!         AA03       �E�E�E�@�v�f�̖ʐ�/3
!         UV1        �E�E�E�@���ԗ���
!         UV2        �E�E�E�@�����i���m�ϐ��j
!         PP         �E�E�E�@����
!         AMB        �E�E�E�@�W�����s��Ίp�����̋t�s��
!         DELT       �E�E�E�@�������ԑ�����
!         
!====================================================================
!
      IMPLICIT NONE

! argument
      real(8) ::  UV1(:,:)
      real(8) ::  UV2(:,:)
      real(8) ::  PP(:)
      real(8) ::  AMB(:)
      integer ::  NODC(:,:)
      real(8) ::  BB(:,:)
      real(8) ::  CC(:,:)
      real(8) ::  AA03(:)
      integer ::  NX, MX
      real(8) ::  DELT

! internal
      integer :: I1, I2, I3
      real(8) :: BB1, BB2, BB3
      real(8) :: CC1, CC2, CC3
      real(8) :: PP1, PP2, PP3
      real(8) :: PPB
      real(8) :: PPC
      real(8) :: SS
      real(8) :: A03

!
      UV2(:,:)=0.0
!
      DO 100 IE = 1, MX
      I1 = NODC(1,IE)
      I2 = NODC(2,IE)
      I3 = NODC(3,IE)
      A03 = AA03(IE)
      BB1 = BB(1,IE)
      BB2 = BB(2,IE)
      BB3 = BB(3,IE)
      CC1 = CC(1,IE)
      CC2 = CC(2,IE)
      CC3 = CC(3,IE)
      PP1 = PP(I1)
      PP2 = PP(I2)
      PP3 = PP(I3)
!
      PPB = (PP1 * BB1 + PP2 * BB2 + PP3 * BB3) * A03
      PPC = (PP1 * CC1 + PP2 * CC2 + PP3 * CC3) * A03
!
      UV2(1,I1) = PPB + UV2(1,I1)
      UV2(1,I2) = PPB + UV2(1,I2)
      UV2(1,I3) = PPB + UV2(1,I3)
      UV2(2,I1) = PPC + UV2(2,I1)
      UV2(2,I2) = PPC + UV2(2,I2)
      UV2(2,I3) = PPC + UV2(2,I3)
  100 CONTINUE
!
      DO 200 I = 1, NX
      SS = AMB(I) * DELT
      UV2(1,I) = UV1(1,I) - UV2(1,I) * SS
      UV2(2,I) = UV1(2,I) - UV2(2,I) * SS
  200 CONTINUE
!
      RETURN
      END subroutine
!====================================================================
      SUBROUTINE VBOUND (NUB,     NVB,     IUB,     IVB,     FUB, &
     &                   FVB,     NENT,    IENT,    UV)
!====================================================================
!     �����̋��E�����T�u���[�`��
!
!     �@�@UU, VV �@�@�E�E�E�@����
!         NUB, NVB   �E�E�E�@U,V ��NON-SLIP���E������
!         IUB, IVB   �E�E�E�@U,V ��NON-SLIP���E�����ߓ_�ԍ�
!         FUB, FVB   �E�E�E�@U,V �̋��E�����l
!         NENT, IENT       �E�E�E�@�������E����(U�̂݁j
!                                ��X���������݂̗̂������E
!                                �iY������0�Ƃ����ꍇ�j
!         
!====================================================================
!
      implicit none
!
      integer, intent(in)  :: NUB, NVB
      integer, intent(in)  :: IUB(:), IVB(:)
      real(8), intent(in)  :: FUB(:), FVB(:)
      integer, intent(in)  :: NENT
      integer, intent(in)  :: IENT(:)
      real(8), intent(out) :: UV(:,:)
!
      integer :: II
!
!     --- U �̋��E���� ---
!
      IF (NUB .EQ. 0)  GOTO 11
      DO 1 I = 1, NUB
      II = IUB(I)
      UV(1,II) = FUB(I)
    1 CONTINUE
!
!     --- V �̋��E���� ---
!
   11 IF (NVB .EQ. 0)  GOTO 12
      DO 2 I = 1, NVB
      II = IVB(I)
      UV(2,II) = FVB(I)
    2 CONTINUE
   12 CONTINUE
!
!     --- �������E���� �iU �̂݁j ---
!                                 ������̗��ł͖����������� U=1.0
!                                   �𗬓����E�ɉۂ����D
!
      DO 3 I=1,NENT
      II=IENT(I)
    3 UV(1,II) = 1.0
!
      RETURN
      END subroutine
!====================================================================
      SUBROUTINE OUTPUT (NX,      UV,      PP,      ISTEP, &
     &                   IOUT,    DELT,    REIV)
!====================================================================
!     �v�Z���ʂ̏o�̓T�u���[�`���i�W���o�́j
!
!         NX       �@�E�E�E�@���ߓ_��
!     �@�@UV, PP     �E�E�E�@�����C���́i�v�Z���ʁj
!         DELT       �E�E�E�@�������ԑ�����
!         REIV       �E�E�E�@REYNOLDS���̋t��
!         ISTEP      �E�E�E�@STEP��
!         
!====================================================================
!
      implicit none
!
      integer, intent(in) :: NX
      real(8), intent(in) :: UV(:,:), PP(:)
      integer, intent(in) :: ISTEP, IOUT
      real(8), intent(in) :: DELT, REIV

      real(8) :: TIME, RE
!
      IF (MOD (ISTEP, IOUT) .NE. 0)  RETURN
!
      TIME = ISTEP * DELT
      RE = 1.0 / REIV
!
      WRITE( 6,601) ISTEP, TIME, RE
      WRITE( 6,602)
      WRITE( 6,603) (I, UV(1,I), UV(2,I), PP(I), I = 1, NX)
!
  601 FORMAT(1H1,//,5X,I10,' STEP     TIME=',F15.8,' SEC.', &
     &              5X, 'REYNOLDS NUM.=',F15.2,//)
  602 FORMAT(1H ,10X,3('        IN    (UU)      (VV)      (PP)  '))
  603 FORMAT((1H ,10X,3(5X,I5,3F10.4)))
!
      RETURN
      END subroutine
!====================================================================
      SUBROUTINE FILOUT (NX,  MX, UV,      PP,      ISTEP, &
     &                   IFOUT,   DELT,    REIV,    IFLG, XY, NODC)
!====================================================================
!     �v�Z���ʂ̏o�̓T�u���[�`���i�t�@�C���j
!
!         NX       �@�E�E�E�@���ߓ_��
!     �@�@UV,  PP    �E�E�E�@�����C���́i�v�Z���ʁj
!         DELT       �E�E�E�@�������ԑ�����
!         REIV       �E�E�E�@REYNOLDS���̋t��
!         ISTEP      �E�E�E�@STEP��
!         IFLG       �E�E�E�@FILE OPEN �̂��߂̃t���b�O
!         
!====================================================================
!
      implicit none

      integer, intent(in)    :: NX
      integer, intent(in)    :: MX
      integer, intent(in)    :: ISTEP
      integer, intent(in)    :: IFOUT
      real(8), intent(in)    :: DELT
      real(8), intent(in)    :: REIV
      integer, intent(in)    :: IFLG
      real(8), intent(in)    :: XY(:,:)
      integer, intent(in)    :: NODC(:,:)

      real(8), intent(in) ::  UV(:,:) !UV(2,*)
      real(8), intent(in) ::  PP(:)
!
      integer        :: i
      real(8)        :: time, re
      character(200) :: filename
!
      IF (MOD (ISTEP, IFOUT) .NE. 0)  RETURN
!
      TIME = ISTEP * DELT
      RE = 1.0 / REIV

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! AVS UCD format by ppohFEM

      do i=1, ppohFEM_get_n_node(idx_mesh)
        call ppohFEM_visualize_set_node_item_val(idx_result_data, i, 1, 1, UV(1,i))
        call ppohFEM_visualize_set_node_item_val(idx_result_data, i, 2, 1, UV(2,i))
        call ppohFEM_visualize_set_node_item_val(idx_result_data, i, 3, 1, PP(i))
      end do

      call ppohFEM_visualize(idx_mesh, idx_result_data, ISTEP, IEND-ISTA, IFOUT)

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      RETURN
      END subroutine
!******************************************************************
      SUBROUTINE SUPER (MX, BB, CC, AREA, idx_mesh, idx_mat_p )
!******************************************************************
!
      use ppohFEM

      implicit none
!
      integer, intent(IN)  :: MX
      real(8), intent(in)  :: AREA(:)
      real(8), intent(in)  :: BB(:,:), CC(:,:)

      integer, intent(in)  :: idx_mesh, idx_mat_p

      real(8) :: ESS(3,3)

      real(8) :: A1
      integer :: IELEM, II, JJ, KK, K, L, M
      integer :: nn
      integer :: nodLOCAL(3) ! node id list for current element

!
      DO 1000 IELEM=1,MX
      A1 = AREA(IELEM)
      ESS(:,:) = 0.0
      DO 100 L=1,3
      DO 100 M=1,3
         ESS(L,M)=ESS(L,M) &
     &           +(BB(L,IELEM)*BB(M,IELEM) &
     &           + CC(L,IELEM)*CC(M,IELEM))*A1
  100 CONTINUE

! now use ppohFEM to make global (stiffness) matrix
! set node id for current element
      nodLOCAL(1) = ppohFEM_get_node_item_of_element(idx_mesh, ielem, 1)
      nodLOCAL(2) = ppohFEM_get_node_item_of_element(idx_mesh, ielem, 2)
      nodLOCAL(3) = ppohFEM_get_node_item_of_element(idx_mesh, ielem, 3)

! number of nodes in triangle element
      nn = 3

! set global matrix
      call ppohFEM_mat_ass_elem(idx_mat_p, nn, nodLOCAL, ESS)

!
 1000 CONTINUE
!
      RETURN
      END subroutine
end program main
