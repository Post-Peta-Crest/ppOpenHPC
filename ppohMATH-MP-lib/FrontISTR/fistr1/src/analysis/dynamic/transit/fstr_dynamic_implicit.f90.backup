!======================================================================!
!                                                                      !
! Software Name : FrontISTR Ver. 3.5                                   !
!                                                                      !
!      Module Name : Dynamic Transit Analysis                          !
!                                                                      !
!            Written by Tomotaka Ogasawara (Univ. of Tokyo)            !
!                       Xi YUAN( AdvanceSoft )                         !
!                                                                      !
!      Contact address :  IIS,The University of Tokyo, CISS            !
!                                                                      !
!      "Structural Analysis for Large Scale Assembly"                  !
!                                                                      !
!======================================================================!
!> \brief This module contains subroutines for implicit dynamic analysis

module fstr_dynamic_implicit

use m_fstr
use lczparm
use m_static_lib
use m_dynamic_output
use m_fstr_EIG_setMASS
use m_dynamic_mat_ass_bc_ac
use m_dynamic_mat_ass_bc
use m_dynamic_mat_ass_bc_vl
use m_dynamic_mat_ass_load
use m_static_mat_ass_main
use fstr_matrix_con_contact
use m_fstr_restart


!-------- for couple -------
use m_dynamic_mat_ass_couple
use m_fstr_rcap_io

! for Jcup
use m_ppohMATHMP_FEM
use fstr_setup_util



contains

  subroutine fstr_solve_dynamic_implicit(hecMESH,hecMAT,fstrSOLID,myEIG   &
                                      ,fstrDYNAMIC,fstrRESULT,fstrPARAM &
                                      ,fstrCPL, restrt_step_num )

    implicit none
!C
!C-- global variable
!C
      type ( hecmwST_local_mesh  ) :: hecMESH
      type ( hecmwST_matrix      ) :: hecMAT
      type ( lczparam            ) :: myEIG
      type ( fstr_solid          ) :: fstrSOLID
      type ( hecmwST_result_data ) :: fstrRESULT
      type ( fstr_param          ) :: fstrPARAM
      type ( fstr_dynamic        ) :: fstrDYNAMIC
      type (fstrST_matrix_contact_lagrange)  :: fstrMAT  !< type fstrST_matrix_contact_lagrange
      type ( fstr_couple         ) :: fstrCPL         !for COUPLE

      type ( hecmwST_matrix      ) :: MAT0
!C
!C-- local variable
!C
    integer(kind=kint) :: nnod, ndof, nn, numnp, n_nod_dof
    integer(kind=kint) :: i, j, ids, ide, ims, ime, kk, idm, imm
    integer(kind=kint) :: kkk0, kkk1
    integer(kind=kint) :: ierror, idummy
    integer(kind=kint) :: iiii5, iexit
    integer(kind=kint) :: revocap_flag
    real(kind=kreal),pointer :: prevB(:)

    real(kind=kreal) :: a1, a2, a3, b1, b2, b3, c1, c2
    real(kind=kreal) :: bsize
    real :: time_1, time_2

    integer(kind=kint) :: restrt_step_num
    integer(kind=kint) :: restrt_step(1)

    real(kind=kreal), parameter :: PI = 3.14159265358979323846D0

!C*-------- solver control -----------*
      logical :: ds = .false. !using Direct Solver or not

!C*-------- for post process -----------*
    integer(kind=kint) :: rank
    character(128) :: bdfn ! Binary Displacement File Name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! variables for ppoh coupling 
!

! for Jcup
integer :: num_of_coupling_node
integer, allocatable :: coupling_node_index(:)
real(kind=4), allocatable :: delta_x(:), delta_y(:), delta_z(:)
integer(kind=kint) :: num_of_step

! for boundary condition from Jcup
real(kind=kreal), allocatable :: bnd_jc_x(:), bnd_jc_y(:), bnd_jc_z(:)

! for boundary condition node group
character(len=HECMW_NAME_LEN), allocatable :: couple_grp_name(:) ! name of node group for coupling boundary
integer(kind=kint), allocatable :: couple_grp_id(:) ! group id of node group for coupling boundary
integer(kind=kint) :: iS0, iE0, ik, in
integer(kind=kint) :: n_internal_coupling_node ! number of internal nodes on coupling node group
integer(kind=kint) :: n_external_coupling_node ! number of external nodes on coupling node group
integer(kind=kint), allocatable :: internal_coupling_node(:) ! local node ID of internal coupling node
integer(kind=kint), allocatable :: external_coupling_node(:) ! local node ID of external coupling node
integer(kind=kint) :: iicn, iecn

! for substep division
integer(kind=kint) :: substep
integer(kind=kint), parameter :: max_substep = 5 ! interval to call ppohMATHMP coupler in main timestep loop of FEM


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call fapp_start("FEM_INIT",1,1)   ! DEBUG profiler start INITIALIZE
call start_collection("FEM_INIT") ! DEBUG profiler start INITIALIZE

write(*,*) 'fstr_dynamic_implicit: max_substep : ', max_substep ! DEBUG

! in case of direct solver
      if (hecMAT%Iarray(99) .eq. 2) then
        ds = .true.
      end if

!--
    hecMAT%NDOF=hecMESH%n_dof

    nnod=hecMESH%n_node
    ndof=hecMAT%NDOF
    nn=ndof*ndof
    n_nod_dof = nnod*ndof
!!
!!-- initial value
!!
      call cpu_time(time_1)

	MAT0%NDOF=hecMESH%n_dof
	MAT0%N=hecMAT%N
	MAT0%NP = hecMAT%NP
	MAT0%NPU = hecMAT%NPU
	MAT0%NPL = hecMAT%NPL

    call hecMAT_init( MAT0)
    allocate( MAT0%indexU(0:hecMAT%NP)      ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to allocation error <fstr_solve_LINEAR_DYNAMIC, indexU>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    allocate( MAT0%indexL(0:hecMAT%NP)      ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to allocation error <fstr_solve_LINEAR_DYNAMIC, indexL>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    allocate( MAT0%itemU(hecMAT%NPU)        ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to allocation error <fstr_solve_LINEAR_DYNAMIC, itemU>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    allocate( MAT0%itemL(hecMAT%NPL)        ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to allocation error <fstr_solve_LINEAR_DYNAMIC, itemL>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if

    if( fstrPARAM%fg_couple == 1) then
      if( fstrPARAM%fg_couple_type==5 .or. &
          fstrPARAM%fg_couple_type==6 ) then
        allocate( prevB(hecMAT%NP*ndof)      ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to allocation error <fstr_solve_LINEAR_DYNAMIC, prevB>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            endif
      endif
    endif

!C
!C-- check parameters
!C
         if(dabs(fstrDYNAMIC%beta) .lt. 1.0e-20) then
              if( hecMESH%my_rank .eq. 0 ) then
                 write(imsg,*) 'stop due to fstrDYNAMIC%beta = 0'
              end if
              call hecmw_abort( hecmw_comm_get_comm())
         end if

!C
!C-- matrix [KL] & [M]
!C-- matrix [KL]

      call fstr_mat_ass_main (hecMESH, hecMAT, fstrSOLID)

    do j = 1 ,nn*hecMAT%NP
      MAT0%D(j)   = hecMAT%D(j)
    end do
    do j = 1 ,nn*hecMAT%NPU
      MAT0%AU(j)  = hecMAT%AU(j)
    end do
    do j = 1 ,nn*hecMAT%NPL
      MAT0%AL(j)  = hecMAT%AL(j)
    end do
    do j = 0 ,hecMAT%NP
      MAT0%indexU(j) = hecMAT%indexU(j)
    end do
    do j = 0 ,hecMAT%NP
      MAT0%indexL(j) = hecMAT%indexL(j)
    end do
    do j = 1 ,hecMAT%NPU
      MAT0%itemU(j)  = hecMAT%itemU(j)
    end do
    do j = 1 ,hecMAT%NPL
      MAT0%itemL(j)  = hecMAT%itemL(j)
    end do

!C-- matrix [M]
!C-- lumped mass matrix
             if(fstrDYNAMIC%idx_mas .eq. 1) then

                call setMASS(IDBG,hecMESH,hecMAT,myEIG)

!C-- consistent mass matrix
               else if(fstrDYNAMIC%idx_mas .eq. 2) then
                 if( hecMESH%my_rank .eq. 0 ) then
                    write(imsg,*) 'stop: consistent mass matrix is not yet available !'
                 end if
                 call hecmw_abort( hecmw_comm_get_comm())
             end if
!C--
      hecMAT%Iarray(98) = 1   !Assmebly complete
      hecMAT%Iarray(97) = 1   !Need numerical factorization
!C
!C
!C-- time step loop
!C
    a1 = .5d0/fstrDYNAMIC%beta - 1.d0                              
    a2 = 1.d0/(fstrDYNAMIC%beta*fstrDYNAMIC%t_delta)
    a3 = 1.d0/(fstrDYNAMIC%beta*fstrDYNAMIC%t_delta**2)
    b1 = ( .5d0*fstrDYNAMIC%ganma/fstrDYNAMIC%beta - 1.d0 )*fstrDYNAMIC%t_delta
    b2 = fstrDYNAMIC%ganma/fstrDYNAMIC%beta - 1.d0
    b3 = fstrDYNAMIC%ganma/(fstrDYNAMIC%beta*fstrDYNAMIC%t_delta)

    c1 = 1.d0 + fstrDYNAMIC%ray_k*fstrDYNAMIC%ganma/(fstrDYNAMIC%beta*fstrDYNAMIC%t_delta)
    c2 = 1.d0/(fstrDYNAMIC%beta*fstrDYNAMIC%t_delta**2) + &
        fstrDYNAMIC%ray_m*fstrDYNAMIC%ganma/(fstrDYNAMIC%beta*fstrDYNAMIC%t_delta)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! set up Jcup
!


        call ppohMATHMP_FEM_get_num_of_coupling_nodes(num_of_coupling_node) 
        allocate(coupling_node_index(num_of_coupling_node))
        allocate(delta_x(num_of_coupling_node))
        allocate(delta_y(num_of_coupling_node))
        allocate(delta_z(num_of_coupling_node))
        allocate(bnd_jc_x(num_of_coupling_node))
        allocate(bnd_jc_y(num_of_coupling_node))
        allocate(bnd_jc_z(num_of_coupling_node))
        bnd_jc_x = 0.0d0
        bnd_jc_y = 0.0d0
        bnd_jc_z = 0.0d0


        call ppohMATHMP_FEM_get_local_node_index(num_of_coupling_node, coupling_node_index) 

        substep = max_substep ! to communicate with coupler in first step


write(*,*) 'num_of_coupling_node', num_of_coupling_node ! DEBUG
write(*,*) 'coupling_node_index',  coupling_node_index ! DEBUG

!
! end setup Jcup
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! setup boundary group
!




    allocate( couple_grp_name(1))
    allocate( couple_grp_id(1))
!    couple_grp_name(1)='NGGNDBNDALL' ! use this in final version
    couple_grp_name(1)='NGPPOHCPL'  ! mesh file must have this node group for coupling boundary
    couple_grp_id(1)=0
    call node_grp_name_to_id_ex( hecMESH, '!BOUNDARY', 1, couple_grp_name, couple_grp_id)

    iS0 = hecMESH%node_group%grp_index(couple_grp_id(1)-1) + 1
    iE0 = hecMESH%node_group%grp_index(couple_grp_id(1)  )

    n_internal_coupling_node = 0
    n_external_coupling_node = 0

    do ik = iS0, iE0
      in = hecMESH%node_group%grp_item(ik)
      if (in .le. hecMESH%nn_internal) then
         n_internal_coupling_node = n_internal_coupling_node + 1
      else if ((in .gt. hecMESH%nn_internal) .and. (in .le. hecMESH%n_node)) then
         n_external_coupling_node = n_external_coupling_node + 1
      else
        write(*,*) "fstr_solve_dynamic_implicit::never come here"
        call hecmw_abort( hecmw_comm_get_comm())
      end if 

    end do


    allocate(internal_coupling_node(n_internal_coupling_node))
    allocate(external_coupling_node(n_external_coupling_node))

    iicn=1
    iecn=1

    do ik = iS0, iE0
      in = hecMESH%node_group%grp_item(ik)
      if (in .le. hecMESH%nn_internal) then
         internal_coupling_node(iicn)=in
         iicn = iicn+1
      else if ((in .gt. hecMESH%nn_internal) .and. (in .le. hecMESH%n_node)) then
         external_coupling_node(iecn)=in
         iecn = iecn+1
      else
        write(*,*) "fstr_solve_dynamic_implicit::never come here"
        call hecmw_abort( hecmw_comm_get_comm()) 
      end if 

    end do



!
! end setup boundary group
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!
!!    step = 0
!!
!
!! do not output anything (output it at post procedure)
!
!C-- output initial condition
!    if( restrt_step_num .eq. 1 ) then
!
!!C-- output new displacement, velocity and accelaration
!!      call fstr_dynamic_Output(hecMESH, fstrSOLID, fstrDYNAMIC)
!!C-- output result of monitoring node
!!      call dynamic_output_monit(hecMESH, fstrPARAM, fstrDYNAMIC, myEIG, fstrSOLID)
!    end if


!!
!!    step = 1,2,....,fstrDYNAMIC%n_step
!!

! output initial value of hecMAT%B ! DEBUG
!write(*,*) 'fstr_dynamic_implicit initial value' !DEBUG
!do iiii5 = 1,hecMAT%NP*ndof !DEBUG
!  write(*,*) 'hecMAT%B i, value ', iiii5, hecMAT%B(iiii5) !DEBUG
!enddo !DEBUG

! initialize VEC1, 2, 3 it's  required
fstrDYNAMIC%VEC1 = 0.0d0 
fstrDYNAMIC%VEC2 = 0.0d0 
fstrDYNAMIC%VEC3 = 0.0d0 

call ppohMATHMP_FEM_init_time() !?~H~]?~\~_?~Y~B?~H??~A?設?~Z ?
call ppohMATHMP_FEM_start_integration()

call stop_collection("FEM_INIT") ! DEBUG profiler stop INITIALIZE
call fapp_stop("FEM_INIT",1,1)   ! DEBUG profiler stop INITIALIZE

    do i= restrt_step_num, fstrDYNAMIC%n_step

call fapp_start("FEM_MISC1",1,1)   ! DEBUG profiler start MISC1
call start_collection("FEM_MISC1") ! DEBUG profiler start MISC1

write(*,*) 'FEM global step ', i !DEBUG
!
!        time_1 =  hecmw_Wtime()
!
       fstrDYNAMIC%i_step = i
       fstrDYNAMIC%t_curr = fstrDYNAMIC%t_delta * i

!C-- matrix [A]
      do j = 1 ,nn*hecMAT%NP
        hecMAT%D(j)  = c1*MAT0%D(j)
      end do
      do j = 1 ,nn*hecMAT%NPU
        hecMAT%AU(j) = c1*MAT0%AU(j)
      end do
      do j = 1 ,nn*hecMAT%NPL
        hecMAT%AL(j) = c1*MAT0%AL(j)
      end do
!
      do j=1,nnod
        do kk=1,ndof
          idm = nn*(j-1)+1 + (ndof+1)*(kk-1)
          imm = ndof*(j-1) + kk
          hecMAT%D(idm) = hecMAT%D(idm) + c2*myEIG%mass(imm)
        end do
      end do
!C
!C-- {vec1}={vec m1}, {vec2}={vec m2}
!C--
      do j = 1 ,ndof*nnod
        fstrDYNAMIC%VEC1(j) = a1*fstrDYNAMIC%ACC(j,1) + a2*fstrDYNAMIC%VEL(j,1) + &
                            a3*fstrDYNAMIC%DISP(j,1)
        fstrDYNAMIC%VEC2(j) = b1*fstrDYNAMIC%ACC(j,1) + b2*fstrDYNAMIC%VEL(j,1) + &
                            b3*fstrDYNAMIC%DISP(j,1)
      end do
!C-- {vec3}={vec k}=[KL]{vec2}

!       call matvec(fstrDYNAMIC%VEC3,fstrDYNAMIC%VEC2,hecMAT,ndof,D,AU,AL)
      if( hecMESH%n_dof .eq. 3 ) then
        call hecmw_matvec_33 (hecMESH, MAT0, fstrDYNAMIC%VEC2, fstrDYNAMIC%VEC3)
      else if( hecMESH%n_dof .eq. 2 ) then
        call hecmw_matvec_22 (hecMESH, MAT0, fstrDYNAMIC%VEC2, fstrDYNAMIC%VEC3, nnod)
      else if( hecMESH%n_dof .eq. 6 ) then
!!      call hecmw_matvec_11 (hecMESH, MAT0, fstrDYNAMIC%VEC2, fstrDYNAMIC%VEC3, n_nod_dof)
        call matvec(fstrDYNAMIC%VEC3, fstrDYNAMIC%VEC2, hecMAT, ndof, MAT0%D, MAT0%AU, MAT0%AL)
      end if
!C
!C-- mechanical boundary condition

!! output initial value of hecMAT%B ! DEBUG
!write(*,*) 'fstr_dynamic_implicit :: 434 before dynamic_mat_ass_load' !DEBUG
!do iiii5 = 1,hecMAT%NP*ndof !DEBUG
!  write(*,*) 'hecMAT%B i, value ', iiii5, hecMAT%B(iiii5) !DEBUG
!enddo !DEBUG

      call dynamic_mat_ass_load (hecMESH, hecMAT, fstrSOLID, fstrDYNAMIC)

!! output initial value of hecMAT%B ! DEBUG
!write(*,*) 'fstr_dynamic_implicit :: 442 after dynamic_mat_ass_load' !DEBUG
!do iiii5 = 1,hecMAT%NP*ndof !DEBUG
!  write(*,*) 'hecMAT%B i, value ', iiii5, hecMAT%B(iiii5) !DEBUG
!enddo !DEBUG

!! DEBUG
!write(*,*) 'fstrDYNAMIC%ray_m', fstrDYNAMIC%ray_m !DEBUG
!write(*,*) 'fstrDYNAMIC%ray_k', fstrDYNAMIC%ray_k !DEBUG
!do j = 1 ,ndof*nnod !DEBUG
!write(*,*) 'myEIG%mass(j)',myEIG%mass(j) !DEBUG
!write(*,*) 'fstrDYNAMIC%VEC1(j)', fstrDYNAMIC%VEC1(j) !DEBUG
!write(*,*) 'fstrDYNAMIC%VEC2(j)', fstrDYNAMIC%VEC2(j) !DEBUG 
!write(*,*) 'fstrDYNAMIC%VEC3(j)', fstrDYNAMIC%VEC3(j) !DEBUG
!end do ! DEBUG

      do j = 1 ,ndof*nnod
        hecMAT%B(j) = hecMAT%B(j) + myEIG%mass(j)*( fstrDYNAMIC%VEC1(j) + fstrDYNAMIC%ray_m*  &
                    fstrDYNAMIC%VEC2(j) ) + fstrDYNAMIC%ray_k*fstrDYNAMIC%VEC3(j)
      end do


!! output initial value of hecMAT%B ! DEBUG
!write(*,*) 'fstr_dynamic_implicit :: 453 after update by some eq' !DEBUG
!do iiii5 = 1,hecMAT%NP*ndof !DEBUG
!  write(*,*) 'hecMAT%B i, value ', iiii5, hecMAT%B(iiii5) !DEBUG
!enddo !DEBUG

!C ********************************************************************************
!C for couple analysis
        if( fstrPARAM%fg_couple == 1 ) then
          if( fstrPARAM%fg_couple_type==5 .or. &
              fstrPARAM%fg_couple_type==6 ) then
            do j = 1, hecMAT%NP * ndof
              prevB(j) = hecMAT%B(j)
            enddo
          endif
        endif
    do
        if( fstrPARAM%fg_couple == 1 ) then
          if( fstrPARAM%fg_couple_type==1 .or. &
              fstrPARAM%fg_couple_type==3 .or. &
              fstrPARAM%fg_couple_type==5 ) call fstr_rcap_get( fstrCPL )
          if( fstrPARAM%fg_couple_first /= 0 ) then
            bsize = DFLOAT( i ) / DFLOAT( fstrPARAM%fg_couple_first )
            if( bsize > 1.0 ) bsize = 1.0
            do kkk0 = 1, fstrCPL%coupled_node_n
              kkk1 = 3 * kkk0
              fstrCPL%trac(kkk1-2) = bsize * fstrCPL%trac(kkk1-2)
              fstrCPL%trac(kkk1-1) = bsize * fstrCPL%trac(kkk1-1)
              fstrCPL%trac(kkk1  ) = bsize * fstrCPL%trac(kkk1  )
            enddo
          endif
          if( fstrPARAM%fg_couple_window > 0 ) then
            j = i - restrt_step_num + 1
            kk = fstrDYNAMIC%n_step - restrt_step_num + 1
            bsize = 0.5*(1.0-cos(2.0*PI*DFLOAT(j)/DFLOAT(kk)))
            do kkk0 = 1, fstrCPL%coupled_node_n
              kkk1 = 3 * kkk0
              fstrCPL%trac(kkk1-2) = bsize * fstrCPL%trac(kkk1-2)
              fstrCPL%trac(kkk1-1) = bsize * fstrCPL%trac(kkk1-1)
              fstrCPL%trac(kkk1  ) = bsize * fstrCPL%trac(kkk1  )
            enddo
          endif
          call dynamic_mat_ass_couple( hecMESH, hecMAT, fstrSOLID, fstrCPL )
        endif
!C ********************************************************************************

!C
!C-- geometrical boundary condition



!        call dynamic_mat_ass_bc   (hecMESH, hecMAT, fstrSOLID, fstrDYNAMIC, fstrPARAM, fstrMAT)

        call dynamic_mat_ass_bc_vl(hecMESH, hecMAT, fstrSOLID, fstrDYNAMIC, fstrPARAM, fstrMAT)
        call dynamic_mat_ass_bc_ac(hecMESH, hecMAT, fstrSOLID, fstrDYNAMIC, fstrPARAM, fstrMAT)

call stop_collection("FEM_MISC1") ! DEBUG profiler stop MISC1
call fapp_stop("FEM_MISC1",1,1)   ! DEBUG profiler stop MISC1


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! get boundary condition from Jcup 
!
call fapp_start("FEM_COUPLER",1,1)   ! DEBUG profiler start FEM_COUPLER
call start_collection("FEM_COUPLER") ! DEBUG profiler start FEM_COUPLER


          if (substep .eq. max_substep) then


            call ppohMATHMP_FEM_set_time()

            delta_x = 0.d0
            call ppohMATHMP_FEM_get_value(num_of_coupling_node, delta_x, 1)
!          call ppohMATHMP_FEM_write_value(i, num_of_coupling_node, delta_x, 1) !DEBUG

            delta_y = 0.d0
            call ppohMATHMP_FEM_get_value(num_of_coupling_node, delta_y, 2)

            delta_z = 0.d0
            call ppohMATHMP_FEM_get_value(num_of_coupling_node, delta_z, 3)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!write(*,*) 'max delta_x ', maxval(delta_x) !DEBUG
!write(*,*) 'mix delta_x ', minval(delta_x) !DEBUG
!write(*,*) 'max delta_y ', maxval(delta_y) !DEBUG
!write(*,*) 'mix delta_y ', minval(delta_y) !DEBUG
!write(*,*) 'max delta_z ', maxval(delta_z) !DEBUG
!write(*,*) 'mix delta_z ', minval(delta_z) !DEBUG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            delta_x(:) = delta_x(:) / max_substep
            delta_y(:) = delta_y(:) / max_substep
            delta_z(:) = delta_z(:) / max_substep


           substep = 0
          end if

call stop_collection("FEM_COUPLER") ! DEBUG profiler start FEM_COUPLER
call fapp_stop("FEM_COUPLER",1,1)   ! DEBUG profiler start FEM_COUPLER

call fapp_start("FEM_MISC2",1,1)   ! DEBUG profiler start MISC2
call start_collection("FEM_MISC2") ! DEBUG profiler start MISC2

          bnd_jc_x(:) = bnd_jc_x(:) + delta_x(:)
          bnd_jc_y(:) = bnd_jc_y(:) + delta_y(:)
          bnd_jc_z(:) = bnd_jc_z(:) + delta_z(:)

          substep = substep + 1


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! set boundary condition from Jcup 
!

!! output initial value of hecMAT%B ! DEBUG
!write(*,*) 'fstr_dynamic_implicit :: 552 before call hecmw_mat_ass_bc' !DEBUG
!do iiii5 = 1,hecMAT%NP*ndof !DEBUG
!  write(*,*) 'hecMAT%B i, value ', iiii5, hecMAT%B(iiii5) !DEBUG
!enddo !DEBUG

! internal node
do j=1, n_internal_coupling_node
  call hecmw_mat_ass_bc(hecMAT, internal_coupling_node(j), 1, bnd_jc_x(j)) ! X
  call hecmw_mat_ass_bc(hecMAT, internal_coupling_node(j), 2, bnd_jc_y(j)) ! Y
  call hecmw_mat_ass_bc(hecMAT, internal_coupling_node(j), 3, bnd_jc_z(j)) ! Z
end do

!! output initial value of hecMAT%B ! DEBUG
!write(*,*) 'fstr_dynamic_implicit :: 565 after call hecmw_mat_ass_bc' !DEBUG
!do iiii5 = 1,hecMAT%NP*ndof !DEBUG
!  write(*,*) 'hecMAT%B i, value ', iiii5, hecMAT%B(iiii5) !DEBUG
!enddo !DEBUG

! external node

! set value from coupler of internal node to X
do j=1, n_internal_coupling_node
  hecMAT%X(internal_coupling_node(j)*3-2)=bnd_jc_x(j) ! X
  hecMAT%X(internal_coupling_node(j)*3-1)=bnd_jc_y(j) ! Y
  hecMAT%X(internal_coupling_node(j)*3  )=bnd_jc_z(j) ! Z
end do

! exchange X
! get boundary value of external node from other rank
  call hecmw_update_3_R (hecMESH, hecMAT%X, hecMAT%NP)


! exchange B
! get boundary value of external node from other rank
!  call hecmw_update_3_R (hecMESH, hecMAT%B, hecMAT%NP)



! set boundary condition for external node
 do j=1, n_external_coupling_node
   call hecmw_mat_ass_bc(hecMAT, external_coupling_node(j), 1, hecMAT%X(external_coupling_node(j)*3-2)) ! X
   call hecmw_mat_ass_bc(hecMAT, external_coupling_node(j), 2, hecMAT%X(external_coupling_node(j)*3-1)) ! Y
   call hecmw_mat_ass_bc(hecMAT, external_coupling_node(j), 3, hecMAT%X(external_coupling_node(j)*3  )) ! Z
 end do


!
! end set boundary condition from Jcup
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!C
!C-- RHS LOAD VECTOR CHECK
!C
        numnp=hecMAT%NP
        bsize=0.0
        do iiii5 = 1,numnp*ndof
          bsize=bsize+hecMAT%B(iiii5)**2
        enddo


!C-- Gather RHS vector
!C
        if (.not. ds) then !In case of Direct Solver prevent MPI
          call hecmw_allREDUCE_R1( hecMESH,bsize,hecmw_sum )
        end if


        iexit = 0
        if( bsize < 1.0e-31 ) then
          iexit = 1
!          if( hecMESH%my_rank .eq. 0 ) then
!            WRITE(IMSG,*) '###Load Vector Error!'
!          endif
!          call hecmw_abort( hecmw_comm_get_comm()) ! Do not abort
        endif


call stop_collection("FEM_MISC2") ! DEBUG profiler stop FEM_MISC2
call fapp_stop("FEM_MISC2",1,1)   ! DEBUG profiler stop FEM_MISC2

!C
!C-- linear solver [A]{X} = {B}
!C

call fapp_start("FEM_SOLVER",1,1)   ! DEBUG profiler start FEM_SOLVER
call start_collection("FEM_SOLVER") ! DEBUG profiler start FEM_SOLVER
        if( iexit .eq. 1 ) then
          hecMAT%X = 0.0
        else
          CALL solve_LINEQ(hecMESH,hecMAT,imsg)
        end if

call stop_collection("FEM_SOLVER") ! DEBUG profiler stop FEM_SOLVER
call fapp_stop("FEM_SOLVER",1,1)   ! DEBUG profiler stop FEM_SOLVER

call fapp_start("FEM_MISC3",1,1)   ! DEBUG profiler start FEM_MISC3
call start_collection("FEM_MISC3") ! DEBUG profiler start FEM_MISC3

!C *****************************************************
!C for couple analysis
      if( fstrPARAM%fg_couple == 1 ) then
        if( fstrPARAM%fg_couple_type>1 ) then
          do j=1, fstrCPL%coupled_node_n
            if( fstrCPL%dof == 3 ) then
              kkk0 = j*3
              kkk1 = fstrCPL%coupled_node(j)*3

              fstrCPL%disp (kkk0-2) = hecMAT%X(kkk1-2)
              fstrCPL%disp (kkk0-1) = hecMAT%X(kkk1-1)
              fstrCPL%disp (kkk0  ) = hecMAT%X(kkk1  )

              fstrCPL%velo (kkk0-2) = -b1*fstrDYNAMIC%ACC(kkk1-2,1) - b2*fstrDYNAMIC%VEL(kkk1-2,1) + &
                                       b3*( hecMAT%X(kkk1-2) - fstrDYNAMIC%DISP(kkk1-2,1) )
              fstrCPL%velo (kkk0-1) = -b1*fstrDYNAMIC%ACC(kkk1-1,1) - b2*fstrDYNAMIC%VEL(kkk1-1,1) + &
                                       b3*( hecMAT%X(kkk1-1) - fstrDYNAMIC%DISP(kkk1-1,1) )
              fstrCPL%velo (kkk0  ) = -b1*fstrDYNAMIC%ACC(kkk1,1) - b2*fstrDYNAMIC%VEL(kkk1,1) + &
                                       b3*( hecMAT%X(kkk1) - fstrDYNAMIC%DISP(kkk1,1) )
              fstrCPL%accel(kkk0-2) = -a1*fstrDYNAMIC%ACC(kkk1-2,1) - a2*fstrDYNAMIC%VEL(kkk1-2,1) + &
                                       a3*( hecMAT%X(kkk1-2) - fstrDYNAMIC%DISP(kkk1-2,1) )
              fstrCPL%accel(kkk0-1) = -a1*fstrDYNAMIC%ACC(kkk1-1,1) - a2*fstrDYNAMIC%VEL(kkk1-1,1) + &
                                       a3*( hecMAT%X(kkk1-1) - fstrDYNAMIC%DISP(kkk1-1,1) )
              fstrCPL%accel(kkk0  ) = -a1*fstrDYNAMIC%ACC(kkk1,1) - a2*fstrDYNAMIC%VEL(kkk1,1) + &
                                       a3*( hecMAT%X(kkk1) - fstrDYNAMIC%DISP(kkk1,1) )
            else
              kkk0 = j*2
              kkk1 = fstrCPL%coupled_node(j)*2

              fstrCPL%disp (kkk0-1) = hecMAT%X(kkk1-1)
              fstrCPL%disp (kkk0  ) = hecMAT%X(kkk1  )

              fstrCPL%velo (kkk0-1) = -b1*fstrDYNAMIC%ACC(kkk1-1,1) - b2*fstrDYNAMIC%VEL(kkk1-1,1) + &
                                       b3*( hecMAT%X(kkk1-1) - fstrDYNAMIC%DISP(kkk1-1,1) )
              fstrCPL%velo (kkk0  ) = -b1*fstrDYNAMIC%ACC(kkk1,1) - b2*fstrDYNAMIC%VEL(kkk1,1) + &
                                       b3*( hecMAT%X(kkk1) - fstrDYNAMIC%DISP(kkk1,1) )
              fstrCPL%accel(kkk0-1) = -a1*fstrDYNAMIC%ACC(kkk1-1,1) - a2*fstrDYNAMIC%VEL(kkk1-1,1) + &
                                       a3*( hecMAT%X(kkk1-1) - fstrDYNAMIC%DISP(kkk1-1,1) )
              fstrCPL%accel(kkk0  ) = -a1*fstrDYNAMIC%ACC(kkk1,1) - a2*fstrDYNAMIC%VEL(kkk1,1) + &
                                       a3*( hecMAT%X(kkk1) - fstrDYNAMIC%DISP(kkk1,1) )
            endif
          end do
          call fstr_rcap_send( fstrCPL )
        endif

        select case ( fstrPARAM%fg_couple_type )
        case (4)
          call fstr_rcap_get( fstrCPL )
        case (5)
          call fstr_get_convergence( revocap_flag )
          if( revocap_flag==0 ) then
            do j = 1, hecMAT%NP * ndof
              hecMAT%B(j) = prevB(j)
            enddo
            cycle
          endif
        case (6)
          call fstr_get_convergence( revocap_flag )
          if( revocap_flag==0 ) then
            do j = 1, hecMAT%NP * ndof
              hecMAT%B(j) = prevB(j)
            enddo
            call fstr_rcap_get( fstrCPL )
            cycle
          else
            if( i /= fstrDYNAMIC%n_step ) call fstr_rcap_get( fstrCPL )
          endif
        end select
      endif
      exit
    enddo
!C *****************************************************

!C
!C-- new displacement, velocity and accelaration
!C
    do j = 1 ,ndof*nnod
      fstrDYNAMIC%DISP(j,2) = hecMAT%X(j)
      fstrDYNAMIC%ACC (j,2) = -a1*fstrDYNAMIC%ACC(j,1) - a2*fstrDYNAMIC%VEL(j,1) + &
                               a3*( fstrDYNAMIC%DISP(j,2) - fstrDYNAMIC%DISP(j,1) )
      fstrDYNAMIC%VEL (j,2) = -b1*fstrDYNAMIC%ACC(j,1) - b2*fstrDYNAMIC%VEL(j,1) + &
                               b3*( fstrDYNAMIC%DISP(j,2) - fstrDYNAMIC%DISP(j,1) )

      fstrDYNAMIC%DISP(j,1) = fstrDYNAMIC%DISP(j,2)
      fstrDYNAMIC%ACC (j,1) = fstrDYNAMIC%ACC (j,2)
      fstrDYNAMIC%VEL (j,1) = fstrDYNAMIC%VEL (j,2)
    end do

!!! restart  !!!
    if( fstrDYNAMIC%restart_nout > 0 .and. &
        (mod(i,fstrDYNAMIC%restart_nout).eq.0 .or. i.eq.fstrDYNAMIC%n_step) ) then
      call fstr_write_restart_dyna_linear(i,fstrDYNAMIC)
    end if

! do not output anything as AVS file.
!C-- output new displacement, velocity and accelaration
!      call fstr_dynamic_Output(hecMESH, fstrSOLID, fstrDYNAMIC)
!
!C-- output result of monitoring node
!      call dynamic_output_monit(hecMESH, fstrPARAM, fstrDYNAMIC, myEIG, fstrSOLID)


!! output displacement as binary file for post procdeure
if( IVISUAL==1 .and. (mod(i,fstrSOLID%output_ctrl(4)%freqency)==0) ) then
  rank=hecmw_comm_get_rank()
  write(bdfn, '("disp.",i6.6,".",i9.9)') ,rank, i ! disp.#rank.#step
  open(IBDF, file=trim(bdfn), form='unformatted', status='replace')
  write(IBDF) hecMAT%X
  close(IBDF)
end if

call stop_collection("FEM_MISC3") ! DEBUG profiler stop FEM_MISC3
call fapp_stop("FEM_MISC3",1,1)   ! DEBUG profiler stop FEM_MISC3

    enddo
!C
!C-- end of time step loop
!C-- deallocate

    deallocate( MAT0%indexU        ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error <fstr_solve_LINEAR_DYNAMIC, indexU>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    deallocate( MAT0%indexL        ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error <fstr_solve_LINEAR_DYNAMIC, indexL>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    deallocate( MAT0%itemU         ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error <fstr_solve_LINEAR_DYNAMIC, itemU>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    deallocate( MAT0%itemL         ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error <fstr_solve_LINEAR_DYNAMIC, itemL>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
    call hecMAT_finalize( MAT0 )

    if( fstrPARAM%fg_couple == 1) then
      if( fstrPARAM%fg_couple_type==5 .or. &
          fstrPARAM%fg_couple_type==6 ) then
        deallocate( prevB      ,STAT=ierror )
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error <fstr_solve_LINEAR_DYNAMIC, prevB>'
              write(idbg,*) '  rank = ', hecMESH%my_rank,'  ierror = ',ierror
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            endif
      endif
    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! finalize Jcup
!
        call ppohMATHMP_FEM_finalize(.false.)


!C-- end of implicit dynamic analysis
!C
    call cpu_time(time_2)
    if( hecMESH%my_rank == 0 ) then
        write(ISTA,'(a,f10.2)') '         solve (sec) :', time_2 - time_1
    end if

!C
  end subroutine fstr_solve_dynamic_implicit


end module fstr_dynamic_implicit
