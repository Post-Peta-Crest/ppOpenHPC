!#######################################################################
!##########                                                   ##########
!##########    非定常NAVIER-STOKES方程式の有限要素解析        ##########
!##########                   サンプルプログラム              ##########
!##########                                                   ##########
!#######################################################################
!						Copyright    K. Hatanaka
!        プログラム名                   :   SAMPLE.FOR
!        メッシュデータファイル名       :   MESH.DAT     (UNIT=10)
!        境界条件ファイル名             :   BC.DAT       (UNIT=11)
!        初期条件ファイル名             :   INIT.DAT     (UNIT=12)
!        計算条件ファイル名             :   INPUT.DAT    (UNIT=15)
!        計算結果出力ファイル名         :   OUTPUT.DAT   (UNIT=20)
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
!     --- 計算条件の入力 ---
!
      CALL DATA05 (DELT,    REIV,    ISTA,    IEND,    IOUT, &
     &             IFOUT )
!
!     --- メッシュデータの入力 ---
!
      CALL MSDATA (NX,      MX,      XY,      NODC,  idx_mesh)

!
!     --- 境界条件データの入力 ---
!
      CALL BDDATA (NUB,     NVB,     NPB,     NENT,  &
     &             IUB,     IVB,     IPB,     IENT,  &
     &             FUB,     FVB,     FPB, idx_mesh)
!
!     --- 初期条件の設定 ---
!
      allocate(UV1(2,NX))
      allocate(UV2(2,NX))
      allocate(PP1(NX))
      allocate(PP2(NX))

      CALL INITIA (ISTA,    NX,      UV1,     PP2)
!
!     --- 補間関数の微係数の計算 ---
!
      CALL MAKABC (NX,      MX,      NODC,    XY,  &
     &             BB,      CC,      AMB,     AREA,  &
     &             AA03,    AA12)
!
!     --- ラプラシアンMatrixの作成 ---
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
!     --- 時間積分LOOP ---
!==========================================
      DO  1000  ISTEP = ISTA ,  IEND
!==========================================
!
!     --- 中間流速の計算 ---
!
      CALL STEP01 (NX,      MX,      NODC,    BB,      CC, &
     &             AREA,    AA12,    UV1,     UV2, &
     &             AMB,     DELT,    REIV)
!
!     --- 中間流速境界条件のセット ---
!
      CALL VBOUND (NUB,     NVB,     IUB,     IVB,     FUB, &
     &             FVB,     NENT,    IENT,    UV2)

!
!     --- 圧力ポアソン式の荷重項の作成 ---
!
! copy original matrix to current matrix and setup equation
      call ppohFEM_mat_copy(idx_mesh, idx_mat_p_org, idx_mat_p)

      CALL STEP02 (NX,      MX,      NODC,    BB,      CC, &
     &             AA03,    UV2,     PP1,     DELT,    idx_mesh, idx_mat_p)
!
!     --- 圧力境界条件の設定 ---
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
!     --- 流速の計算 ---
!
      UV1(:,:) = UV2(:,:)
!
      CALL STEP03 (NX,      MX,      NODC,    BB,      CC, &
     &             AA03,    UV1,     UV2,     PP2,     AMB, DELT)
! update vector
      call ppohFEM_update_1_R(idx_mesh, UV2(1,:), NX)
      call ppohFEM_update_1_R(idx_mesh, UV2(2,:), NX)
!
!     --- 流速境界条件のセット ---
!
      CALL VBOUND (NUB,     NVB,     IUB,     IVB,     FUB, &
     &             FVB,     NENT,    IENT,    UV2)
!
!     --- 計算結果の出力（標準出力） ---
!
      CALL OUTPUT (NX,      UV2,     PP2,      ISTEP,  IOUT, &
     &             DELT,    REIV)
!
!     --- 計算結果の出力（ファイル） ---
!
      CALL FILOUT (NX,      MX,      UV2,     PP2,      ISTEP,  IFOUT, &
     &             DELT,    REIV,    IFLG, XY, NODC)
!
!     --- 時間積分のための初期条件の入れ替え ---
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
!     計算条件の入力サブルーチン
!
!         DELT       ・・・　微小時間増分量
!         RE         ・・・　REYNOLDS数
!         REIV       ・・・　REYNOLDS数の逆数
!         ISTA       ・・・　計算開始STEP数
!         IEND       ・・・　計算終了STEP数
!         IOUT       ・・・　計算結果出力STEP数（標準出力）
!         IFOUT      ・・・　計算結果出力STEP数（ファイル）
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
!     メッシュデータの入力サブルーチン
!
!         NX         ・・・　総節点数
!         MX         ・・・　総要素数
!         XY         ・・・　節点のX,Y座標
!         NODC       ・・・　要素の適合条件
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
!     境界条件の入力サブルーチン
!
!         NUB, NVB, NPB    ・・・　U,V,P のNON-SLIP境界条件個数
!         IUB, IVB, IPB    ・・・　U,V,P のNON-SLIP境界条件節点番号
!         FUB, FVB, FPB    ・・・　U,V,P の境界条件値
!         NENT, IENT       ・・・　流入境界条件(Uのみ）
!                                ※X方向成分のみの流入境界
!                                （Y方向は0とした場合）
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
!     --- 境界条件ファイルの読み込み ---
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
!     --- ZERO 境界条件値の設定 ---
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
!     初期条件の設定サブルーチン
!
!         NX               ・・・　総節点数
!         ISTA             ・・・　計算開始STEP数
!         UV, PP           ・・・　U,V,P の初期設定値
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
!     --- 変数のゼロクリアー ---
!
      UV(:,:) = 0.0
      PP(:)=0.0 
      GOTO 2 
!
!     --- 初期条件をファイルより読み込む ---
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
!     要素ごとの補間関数の微係数計算サブルーチン
!
!         NX               ・・・　総節点数
!         MX               ・・・　総要素数
!         NODC             ・・・　要素の適合条件
!         BB               ・・・　補間関数の微係数(X)
!         CC               ・・・　補間関数の微係数(Y)
!         AREA             ・・・　要素の面積
!         AA03, AA12       ・・・　AREA/3, AREA/12
!         AMB              ・・・　質量行列の集中化行列の
!                                  対角成分の逆数
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
!     --- 面積の計算 ---
!
      A2 = X1 * (Y2 - Y3) + X2 * (Y3 - Y1) + X3 * (Y1 - Y2)
      AREA(IE) = A2 * 0.5
      AA03(IE) = AREA(IE) / 3.0
      AA12(IE) = AREA(IE) / 12.0
      A2 = 1.0 / A2
!
!     --- 微係数の計算 ---
!
      BB(1,IE) = (Y2 - Y3) * A2
      BB(2,IE) = (Y3 - Y1) * A2
      BB(3,IE) = (Y1 - Y2) * A2
      CC(1,IE) = (X3 - X2) * A2
      CC(2,IE) = (X1 - X3) * A2
      CC(3,IE) = (X2 - X1) * A2
!
!     --- 集中化行列の対角成分の重ね合わせ ---
!
      AMB(I1) = AA03(IE) + AMB(I1)
      AMB(I2) = AA03(IE) + AMB(I2)
      AMB(I3) = AA03(IE) + AMB(I3)
  100 CONTINUE
!
!     --- 集中化行列の対角成分の逆数 ---
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
!     中間流速の計算サブルーチン
!
!         NX       　・・・　総節点数
!         MX       　・・・　総要素数
!     　　UV1      　・・・　流速 U, V   （既知）
!     　　UV2      　・・・　中間流速　　（未知変数）
!         AMB      　・・・　質量行列の集中化行列
!         NODC       ・・・　要素の適合条件
!         BB         ・・・　補間関数の微係数(X)
!         CC         ・・・　補間関数の微係数(Y)
!         AREA       ・・・　要素の面積
!         AA03, AA12 ・・・　AREA/3, AREA/12
!         DELT       ・・・　微小時間増分量
!         REIV       ・・・　REYNOLDS数の逆数
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
!    --- 移流項の作成 ---
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
!    --- 粘性項の作成 ---
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
!    --- 既知項の重ね合わせ ---
!
      UV2(1,I1) = UKK1 + USS1 + UV2(1,I1)
      UV2(1,I2) = UKK2 + USS2 + UV2(1,I2)
      UV2(1,I3) = UKK3 + USS3 + UV2(1,I3)
      UV2(2,I1) = VKK1 + VSS1 + UV2(2,I1)
      UV2(2,I2) = VKK2 + VSS2 + UV2(2,I2)
      UV2(2,I3) = VKK3 + VSS3 + UV2(2,I3)
 1000 CONTINUE
!
!    --- 中間流速の計算 ---
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
!     圧力ポアソン方程式の右辺（荷重項）の計算サブルーチン
!
!         NX       　・・・　総節点数
!         MX       　・・・　総要素数
!     　　UV1      　・・・　中間流速
!         PP         ・・・　圧力ポアソン方程式の荷重項
!         NODC       ・・・　要素の適合条件
!         BB         ・・・　補間関数の微係数(X)
!         CC         ・・・　補間関数の微係数(Y)
!         AA03       ・・・　要素の面積/3
!         DELT       ・・・　微小時間増分量
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
!     流速の計算サブルーチン
!
!         NX       　・・・　総節点数
!         MX       　・・・　総要素数
!         NODC       ・・・　要素の適合条件
!         BB         ・・・　補間関数の微係数(X)
!         CC         ・・・　補間関数の微係数(Y)
!         AA03       ・・・　要素の面積/3
!         UV1        ・・・　中間流速
!         UV2        ・・・　流速（未知変数）
!         PP         ・・・　圧力
!         AMB        ・・・　集中化行列対角成分の逆行列
!         DELT       ・・・　微小時間増分量
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
!     流速の境界条件サブルーチン
!
!     　　UU, VV 　　・・・　流速
!         NUB, NVB   ・・・　U,V のNON-SLIP境界条件個数
!         IUB, IVB   ・・・　U,V のNON-SLIP境界条件節点番号
!         FUB, FVB   ・・・　U,V の境界条件値
!         NENT, IENT       ・・・　流入境界条件(Uのみ）
!                                ※X方向成分のみの流入境界
!                                （Y方向は0とした場合）
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
!     --- U の境界条件 ---
!
      IF (NUB .EQ. 0)  GOTO 11
      DO 1 I = 1, NUB
      II = IUB(I)
      UV(1,II) = FUB(I)
    1 CONTINUE
!
!     --- V の境界条件 ---
!
   11 IF (NVB .EQ. 0)  GOTO 12
      DO 2 I = 1, NVB
      II = IVB(I)
      UV(2,II) = FVB(I)
    2 CONTINUE
   12 CONTINUE
!
!     --- 流入境界条件 （U のみ） ---
!                                 ※今回の例題では無次元流速で U=1.0
!                                   を流入境界に課した．
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
!     計算結果の出力サブルーチン（標準出力）
!
!         NX       　・・・　総節点数
!     　　UV, PP     ・・・　流速，圧力（計算結果）
!         DELT       ・・・　微小時間増分量
!         REIV       ・・・　REYNOLDS数の逆数
!         ISTEP      ・・・　STEP数
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
!     計算結果の出力サブルーチン（ファイル）
!
!         NX       　・・・　総節点数
!     　　UV,  PP    ・・・　流速，圧力（計算結果）
!         DELT       ・・・　微小時間増分量
!         REIV       ・・・　REYNOLDS数の逆数
!         ISTEP      ・・・　STEP数
!         IFLG       ・・・　FILE OPEN のためのフラッグ
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
