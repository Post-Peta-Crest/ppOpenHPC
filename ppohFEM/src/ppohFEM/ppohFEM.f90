!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/

module ppohFEM

use hecmw
use elementInfo

implicit none

! size of some array
integer(kind=kint),parameter :: PPOHFEM_NAME_LEN     = HECMW_NAME_LEN    
integer(kind=kint),parameter :: PPOHFEM_HEADER_LEN   = HECMW_HEADER_LEN  
integer(kind=kint),parameter :: PPOHFEM_MSG_LEN      = HECMW_MSG_LEN     
integer(kind=kint),parameter :: PPOHFEM_FILENAME_LEN = HECMW_FILENAME_LEN

! keywords for MPI communication
integer(kind=kint),parameter :: ppohFEM_sum              = hecmw_sum             
integer(kind=kint),parameter :: ppohFEM_prod             = hecmw_prod            
integer(kind=kint),parameter :: ppohFEM_max              = hecmw_max             
integer(kind=kint),parameter :: ppohFEM_min              = hecmw_min             
integer(kind=kint),parameter :: ppohFEM_integer          = hecmw_integer         
integer(kind=kint),parameter :: ppohFEM_single_precision = hecmw_single_precision
integer(kind=kint),parameter :: ppohFEM_double_precision = hecmw_double_precision
integer(kind=kint),parameter :: ppohFEM_character        = hecmw_character       


! subroutines

! variables !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

private stiffMAT, mesh

integer, parameter :: NUM_STIFF_MAT = 10 ! number of stiffness matrices kept on the ppohFEM module
integer, parameter :: NUM_MESH      = 10 ! number of mesh kept on the ppohFEM module
integer, parameter :: NUM_RESULT    = 10 ! number of result structure kept on the ppohFEM module
integer, parameter :: NUM_SOLVER_METHOD = 1 ! currently CG solver only

! stiffness matrix. Format is same as hecmw2 hecmwST_matrix. 
! This matrix is used in ppohFEM middleware internaly and removed from arguments of hecmw subroutines.
type (hecmwST_matrix ), pointer :: stiffMAT_array(:)
type (hecmwST_matrix ), pointer :: stiffMAT
integer :: idx_current_stiffMAT

!mesh
type(hecmwST_local_mesh), pointer :: mesh_array(:)
type(hecmwST_local_mesh), pointer :: mesh
integer :: idx_current_mesh

!result
type(hecmwST_result_data), pointer :: result_data_array(:)
type(hecmwST_result_data), pointer :: result_data
integer :: idx_current_result_data



private idbg
integer, parameter :: idbg=52

contains


! wrapper !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine ppohFEM_init
  allocate(stiffMAT_array(NUM_STIFF_MAT))
  idx_current_stiffMAT = 1
  stiffMAT => stiffMAT_array(idx_current_stiffMAT)

  allocate(mesh_array(NUM_MESH))
  idx_current_mesh = 1
  mesh => mesh_array(idx_current_mesh)

  allocate(result_data_array(NUM_RESULT))
  idx_current_result_data = 1
  result_data => result_data_array(idx_current_result_data)

  call hecmw_init()
end subroutine ppohFEM_init

function ppohFEM_comm_get_rank() result(rank)
  integer(kind=kint) :: rank
  rank = hecmw_comm_get_rank()
end function ppohFEM_comm_get_rank

function ppohFEM_comm_get_size() result(comm_size)
  integer(kind=kint) :: comm_size
  comm_size = hecmw_comm_get_size()
end function ppohFEM_comm_get_size

function ppohFEM_Wtime()
  real(kind=kreal) ppohFEM_Wtime
  external  hecmw_Wtime_fi
  real(kind=kreal) hecmw_Wtime_fi
  ppohFEM_Wtime = hecmw_Wtime_fi()
end function ppohFEM_Wtime

subroutine ppohFEM_get_mesh(idx_mesh)
  integer, intent(IN) :: idx_mesh
  character(len=HECMW_FILENAME_LEN)      :: name_ID
  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if
  
  name_ID = 'fstrMSH'
  mesh => mesh_array(idx_mesh)
  call hecmw_get_mesh(name_ID, mesh)
  return
end subroutine ppohFEM_get_mesh

subroutine ppohFEM_finalize
  call hecmw_finalize()
end subroutine ppohFEM_finalize

subroutine ppohFEM_nullify_matrix( idx_mat )
  integer, intent(IN)             :: idx_mat
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT)) then
    call hecmw_nullify_matrix(stiffMAT_array(idx_mat))
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_nullify_matrix


subroutine ppohFEM_nullify_result_data( P )
  type( hecmwST_result_data ) :: P
  call hecmw_nullify_result_data( P )
end subroutine ppohFEM_nullify_result_data


subroutine ppohFEM_mat_con ( idx_mesh, idx_mat )
!  type (hecmwST_local_mesh), intent(IN)  :: hecMESH
  integer, intent(IN)                    :: idx_mesh
  integer, intent(IN)                    :: idx_mat
  if ((idx_mat  .ge. 1) .and. (idx_mat  .le. NUM_STIFF_MAT)  .and. &
&     (idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)     )  then
    call hecmw_mat_con ( mesh_array(idx_mesh), stiffMAT_array(idx_mat) )
    stiffMAT_array(idx_mat)%NDOF = mesh_array(idx_mesh)%n_dof
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_mat_con


subroutine ppohFEM_ctrl_get_control_file(name_ID, filename)
  character(len=HECMW_NAME_LEN),     intent(IN)  :: name_ID
  character(len=HECMW_FILENAME_LEN), intent(OUT) :: filename
  call hecmw_ctrl_get_control_file(name_ID, filename)
end subroutine ppohFEM_ctrl_get_control_file

! clear stiffness matrix elements and X and B vector
subroutine ppohFEM_mat_clear(idx_mat)
  integer, optional, intent(IN) :: idx_mat ! index of stiffness matrices. 
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))then
    call hecmw_mat_clear(stiffMAT_array(idx_mat))
    stiffMAT_array(idx_mat)%X = 0.0
    stiffMAT_array(idx_mat)%B = 0.0
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_mat_clear

!clear B vector in stiffness matrix
subroutine ppohFEM_mat_clear_b(idx_mat)
  integer, optional, intent(IN) :: idx_mat ! index of stiffness matrices. 
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))then
    stiffMAT_array(idx_mat)%B = 0.0
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_mat_clear_b

!copy stiffness matrix from src to dest
!arrangement of non-zero elements in matrix is given by mesh structure specified by idx_mesh
!member variables of destination matrix will be deallocated and override.
subroutine ppohFEM_mat_copy(idx_mesh, idx_mat_src, idx_mat_dest)
  integer, intent(IN) :: idx_mesh
  integer, intent(IN) :: idx_mat_src
  integer, intent(IN) :: idx_mat_dest
  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if
  if (.not. ((idx_mat_src .ge. 1) .and. (idx_mat_src .le. NUM_STIFF_MAT))) then
    write(*,*) 'source matrix index out of bounds'
    call ppohFEM_abort
  end if
  if (.not. ((idx_mat_dest .ge. 1) .and. (idx_mat_dest .le. NUM_STIFF_MAT))) then
    write(*,*) 'destination matrix index out of bounds'
    call ppohFEM_abort
  end if

! deallocate, allocate, zero-clear destination matrix
  call ppohFEM_hecMAT_finalize(stiffMAT_array(idx_mat_dest))
  call ppohFEM_nullify_matrix( idx_mat_dest )
  call ppohFEM_mat_con(idx_mesh, idx_mat_dest)
  call ppohFEM_set_mat_ndof(idx_mat_dest, ppohFEM_get_mat_ndof(idx_mat_src))
  call ppohFEM_mat_init( idx_mat_dest )
  call ppohFEM_mat_clear(idx_mat_dest)

! copy variables from src to destination
  stiffMAT_array(idx_mat_dest)%N      = stiffMAT_array(idx_mat_src)%N
  stiffMAT_array(idx_mat_dest)%NP     = stiffMAT_array(idx_mat_src)%NP
  stiffMAT_array(idx_mat_dest)%NPL    = stiffMAT_array(idx_mat_src)%NPL
  stiffMAT_array(idx_mat_dest)%NPU    = stiffMAT_array(idx_mat_src)%NPU
  stiffMAT_array(idx_mat_dest)%NDOF   = stiffMAT_array(idx_mat_src)%NDOF

  stiffMAT_array(idx_mat_dest)%D(:)      = stiffMAT_array(idx_mat_src)%D(:)
  stiffMAT_array(idx_mat_dest)%AU(:)     = stiffMAT_array(idx_mat_src)%AU(:)
  stiffMAT_array(idx_mat_dest)%AL(:)     = stiffMAT_array(idx_mat_src)%AL(:)
  stiffMAT_array(idx_mat_dest)%indexU(:) = stiffMAT_array(idx_mat_src)%indexU(:)
  stiffMAT_array(idx_mat_dest)%indexL(:) = stiffMAT_array(idx_mat_src)%indexL(:)
  stiffMAT_array(idx_mat_dest)%itemU(:)  = stiffMAT_array(idx_mat_src)%itemU(:)
  stiffMAT_array(idx_mat_dest)%itemL(:)  = stiffMAT_array(idx_mat_src)%itemL(:)
  stiffMAT_array(idx_mat_dest)%Iarray(:) = stiffMAT_array(idx_mat_src)%Iarray(:)
  stiffMAT_array(idx_mat_dest)%Rarray(:) = stiffMAT_array(idx_mat_src)%Rarray(:)

end subroutine ppohFEM_mat_copy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! node handling 
!
integer function ppohFEM_get_n_node(idx_mesh)
! return n_node (number of nodes include external node) of idx_mesh'th mesh
  integer, intent(IN) :: idx_mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_n_node = mesh_array(idx_mesh)%n_node
end function ppohFEM_get_n_node

integer function ppohFEM_get_nn_internal(idx_mesh)
! return nn_internal (number of internal nodes) of idx_mesh'th mesh
  integer, intent(IN) :: idx_mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_nn_internal = mesh_array(idx_mesh)%nn_internal
end function ppohFEM_get_nn_internal

integer function ppohFEM_get_n_global_node(idx_mesh)
! return number of global nodes of idx_mesh'th mesh
  integer, intent(IN) :: idx_mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_n_global_node = mesh_array(idx_mesh)%nn_internal
  call hecmw_allREDUCE_I1( mesh_array(idx_mesh), ppohFEM_get_n_global_node, hecmw_sum )
end function ppohFEM_get_n_global_node

integer function ppohFEM_get_global_node_ID(idx_mesh, idx_node)
  integer, intent(IN) :: idx_mesh
  integer, intent(IN) :: idx_node

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node .ge. 1) .and. (idx_node .le. mesh_array(idx_mesh)%n_node))) then
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_global_node_ID = mesh_array(idx_mesh)%global_node_ID(idx_node)
end function ppohFEM_get_global_node_ID


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! element handling 
!

integer function ppohFEM_get_n_elem(idx_mesh)
  integer, intent(IN) :: idx_mesh ! index of mesh
  if (  ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) ) then
    ppohFEM_get_n_elem = mesh_array(idx_mesh)%n_elem
  else
    call ppohFEM_abort
  end if
  
end function ppohFEM_get_n_elem

integer function ppohFEM_get_n_elem_type(idx_mesh)
  integer, intent(IN) :: idx_mesh ! index of mesh
  if (  ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) ) then
    ppohFEM_get_n_elem_type = mesh_array(idx_mesh)%n_elem_type
  else
    call ppohFEM_abort
  end if
  
end function ppohFEM_get_n_elem_type

integer function ppohFEM_get_elem_type_index(idx_mesh, itype)
!return last index of element of given element type index on idx_mesh's mesh
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: itype    ! index of existing element type
  if (  ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) &
& .and. ((itype .ge. 0) .and. (itype .le. mesh_array(idx_mesh)%n_elem_type))) then
    ppohFEM_get_elem_type_index = mesh_array(idx_mesh)%elem_type_index(itype)
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_elem_type_index

integer function ppohFEM_get_elem_type_item(idx_mesh, itype)
! return last index of element item of itype'th type element of idx_mesh'th mesh
! return 0 if itype is 0
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: itype    ! index of existing element type
  
  if (itype .eq. 0) then
    ppohFEM_get_elem_type_item = 0
    return
  end if

  if (  ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) &
& .and. ((itype .ge. 1) .and. (itype .le. mesh_array(idx_mesh)%n_elem_type))) then
    ppohFEM_get_elem_type_item = mesh_array(idx_mesh)%elem_type_item(itype)
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_elem_type_item

integer function ppohFEM_get_elem_type(idx_mesh, idx_elem)
! return element type of idx_elem'th element
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: idx_elem ! index of element (local element id)
  
  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem .ge. 1) .and. (idx_elem .le. mesh_array(idx_mesh)%n_elem))) then
    write(*,*) 'elem index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_elem_type = mesh_array(idx_mesh)%elem_type(idx_elem)

end function ppohFEM_get_elem_type

integer function ppohFEM_get_global_elem_ID(idx_mesh, idx_elem)
! return global element ID of given local element ID
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: idx_elem ! index of element (local ID)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem .ge. 1) .and. (idx_elem .le. mesh_array(idx_mesh)%n_elem))) then
    write(*,*) 'elem index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_global_elem_ID = mesh_array(idx_mesh)%global_elem_ID(idx_elem)
end function ppohFEM_get_global_elem_ID


logical function ppohFEM_is_etype_link(etype)
  integer, intent(IN) :: etype
  ppohFEM_is_etype_link = hecmw_is_etype_link(etype)
end function ppohFEM_is_etype_link

logical function ppohFEM_is_etype_beam(etype)
  integer, intent(IN) :: etype
  ppohFEM_is_etype_beam = hecmw_is_etype_beam(etype)
end function ppohFEM_is_etype_beam

logical function ppohFEM_is_etype_rod(etype)
  integer, intent(IN) :: etype
  ppohFEM_is_etype_rod = hecmw_is_etype_rod(etype)
end function ppohFEM_is_etype_rod

logical function ppohFEM_is_etype_solid(etype)
  integer, intent(IN) :: etype
  ppohFEM_is_etype_solid = hecmw_is_etype_solid(etype)
end function ppohFEM_is_etype_solid

logical function ppohFEM_is_etype_shell(etype)
  integer, intent(IN) :: etype
  ppohFEM_is_etype_shell = hecmw_is_etype_shell(etype)
end function ppohFEM_is_etype_shell

logical function ppohFEM_is_etype_surface(etype)
  integer, intent(IN) :: etype
  ppohFEM_is_etype_surface = hecmw_is_etype_surface(etype)
end function ppohFEM_is_etype_surface

integer function ppohFEM_get_max_node_of_elem_type(etype)
!return number of nodes of element type given by etype
  integer, intent(IN) :: etype
  ppohFEM_get_max_node_of_elem_type = hecmw_get_max_node(etype)
end function ppohFEM_get_max_node_of_elem_type

integer function ppohFEM_get_node_item_of_element(idx_mesh, idx_elem, idx_node)
!return idx_node'th node ID of idx_elem'th element
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: idx_elem ! index of element
  integer, intent(IN) :: idx_node ! index of node of idx_elem'th element

  integer :: i, iS

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem .ge. 1) .and. (idx_elem .le. mesh_array(idx_mesh)%n_elem))) then
    write(*,*) 'element index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node .ge. 1) .and. (idx_node .le. hecmw_get_max_node(mesh_array(idx_mesh)%elem_type(idx_elem))))) then
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_elem .eq. 1) then
    iS = 0
  else
    iS = mesh_array(idx_mesh)%elem_node_index(idx_elem-1) ! point last node of previous element
  end if
  i = iS + idx_node

  ppohFEM_get_node_item_of_element = mesh_array(idx_mesh)%elem_node_item(i)    ! should true
end function ppohFEM_get_node_item_of_element

integer function ppohFEM_get_rank_assigned_to_element(idx_mesh, idx_elem)
! return assigned rank to idx_elem'th element (use in output routines)
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: idx_elem ! index of element (local ID)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem .ge. 1) .and. (idx_elem .le. mesh_array(idx_mesh)%n_elem))) then
    write(*,*) 'elem index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_rank_assigned_to_element = mesh_array(idx_mesh)%elem_ID(idx_elem*2)
end function ppohFEM_get_rank_assigned_to_element

integer function ppohFEM_get_local_elem_ID(idx_mesh, idx_elem)
! return local element ID (?) to idx_elem'th element (use in output routines)
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: idx_elem ! index of element (local ID)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem .ge. 1) .and. (idx_elem .le. mesh_array(idx_mesh)%n_elem))) then
    write(*,*) 'elem index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_local_elem_ID = mesh_array(idx_mesh)%elem_ID(idx_elem*2-1)
end function ppohFEM_get_local_elem_ID

integer function ppohFEM_get_n_material(idx_mesh)
!return number of materials (mesh%material%n_mat) of idx_mesh'th mesh
  integer, intent(IN) :: idx_mesh ! index of mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_n_material = mesh_array(idx_mesh)%material%n_mat
end function ppohFEM_get_n_material


! quadpoint, derivative, shapefunction, weight function !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function ppohFEM_get_num_quadpoints_of_element( fetype)
  integer, intent(in)           :: fetype    !< element type
  ppohFEM_get_num_quadpoints_of_element = NumOfQuadPoints(fetype)
end function ppohFEM_get_num_quadpoints_of_element

subroutine ppohFEM_get_quadpoints_of_element(fetype, np, pos)
  !> Fetch the coordinate of gauss point
  integer, intent(in)           :: fetype    !< element type
  integer, intent(in)           :: np        !< number of curr quadrature point
  real(kind=kreal), intent(out) :: pos(:)    !< natural coord of curr quadrature point
  call getQuadPoint(fetype, np, pos)
end subroutine ppohFEM_get_quadpoints_of_element

subroutine ppohFEM_get_global_deriv( fetype, nn, localcoord, elecoord, det, gderiv )
  !> Calculate shape derivative in global coordinate system
      integer, intent(in)           :: fetype          !< element type
      integer, intent(in)           :: nn              !< number of elemental nodes
      real(kind=kreal), intent(in)  :: localcoord(:)   !< curr position with natural coord
      real(kind=kreal), intent(in)  :: elecoord(:,:)   !< nodal coord of curr element
      real(kind=kreal), intent(out) :: det             !< nodal coord of curr element
      real(kind=kreal), intent(out) :: gderiv(:,:)     !< shape deivative in global coordinate system
  call getGlobalDeriv( fetype, nn, localcoord, elecoord, det, gderiv )
end subroutine ppohFEM_get_global_deriv

subroutine ppohFEM_get_shape_func( fetype, localcoord, func )
  !> Calculate the shape function in natural coodinate system
      integer, intent(in)           :: fetype            !< input element type
      real(kind=kreal), intent(in)  :: localcoord(:)     !< natural points
      real(kind=kreal), intent(out) :: func(:)           !< shape function
  call getShapeFunc(fetype, localcoord, func)
end subroutine ppohFEM_get_shape_func

real(kind=kreal) function ppohFEM_get_weight(fetype, np)
      integer, intent(in)           :: fetype       !< element type
      integer, intent(in)           :: np           !< number of curr quadrature point
  ppohFEM_get_weight = getWeight(fetype, np)
end function ppohFEM_get_weight

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ppohFEM_get_node_coord(idx_mesh, nodeid, XYZ)
! return XYZ coordination of nodeid'th node
! XYZ(1) x coordinate
! XYZ(2) y coordinate
! XYZ(3) z coordinate
  integer, intent(IN)           :: idx_mesh
  integer, intent(IN)           :: nodeid  ! local node ID
  real(kind=kreal), intent(OUT), dimension(3) :: XYZ

  if (  ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) &
& .and. ((nodeid .ge. 1) .and. (nodeid .le. mesh_array(idx_mesh)%n_node))) then
    XYZ(1)=mesh_array(idx_mesh)%node(3*nodeid-2) ! X coord
    XYZ(2)=mesh_array(idx_mesh)%node(3*nodeid-1) ! Y coord
    XYZ(3)=mesh_array(idx_mesh)%node(3*nodeid  ) ! Z coord
  else
    call ppohFEM_abort
  end if

end subroutine ppohFEM_get_node_coord

integer function ppohFEM_get_section_ID(idx_mesh, idx_elem)
! return section id of idx_elem'th element
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: idx_elem ! index of element

  if ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) then
    ppohFEM_get_section_ID = mesh_array(idx_mesh)%section_ID(idx_elem)
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_section_ID

integer(kind=kint) function ppohFEM_get_sect_R_index(idx_mesh, isect)
! return head index of given section
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: isect    ! index of section (it should set as 'isect -1' on application...)

  if ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) then
    ppohFEM_get_sect_R_index = mesh_array(idx_mesh)%section%sect_R_index(isect)
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_sect_R_index

real(kind=kreal) function ppohFEM_get_sect_R_item(idx_mesh, ihead)
! return thickness of shell element of given section
  integer, intent(IN) :: idx_mesh ! index of mesh
  integer, intent(IN) :: ihead    ! index of head element of current section (it should set as 'ihead +1' on application...)

  if ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) then
    ppohFEM_get_sect_R_item = mesh_array(idx_mesh)%section%sect_R_item(ihead)
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_sect_R_item

integer function ppohFEM_get_sect_orien_ID(idx_mesh, idx_section)
! return section orientation id of idx_section'th section
  integer, intent(IN) :: idx_mesh    ! index of mesh
  integer, intent(IN) :: idx_section ! index of section

  if ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) then
    ppohFEM_get_sect_orien_ID = mesh_array(idx_mesh)%section%sect_orien_ID(idx_section)
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_sect_orien_ID

integer function ppohFEM_get_sect_mat_ID_item(idx_mesh, idx_section)
! return hecMESH%section%sect_mat_ID_item
  integer, intent(IN) :: idx_mesh    ! index of mesh
  integer, intent(IN) :: idx_section ! index of element

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_sect_mat_ID_item = mesh_array(idx_mesh)%section%sect_mat_ID_item(idx_section)
end function ppohFEM_get_sect_mat_ID_item

integer function ppohFEM_get_sect_opt(idx_mesh, idx_section)
! return hecMESH%section%sect_opt(idx_section)
  integer, intent(IN) :: idx_mesh    ! index of mesh
  integer, intent(IN) :: idx_section ! index of element

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_sect_opt = mesh_array(idx_mesh)%section%sect_opt(idx_section)
end function ppohFEM_get_sect_opt

integer function ppohFEM_get_n_sect(idx_mesh)
! return hecMESH%section%n_sect
  integer, intent(IN) :: idx_mesh    ! index of mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_n_sect = mesh_array(idx_mesh)%section%n_sect
end function ppohFEM_get_n_sect

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! node group
!


integer function ppohFEM_get_num_node_group(idx_mesh)
! return number of node group of idx_mesh'th hecMESH
  integer, intent(IN) :: idx_mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_num_node_group = mesh_array(idx_mesh)%node_group%n_grp

end function ppohFEM_get_num_node_group

integer function ppohFEM_get_num_nodes_in_node_group(idx_mesh, idx_node_grp)
! return number of nodes in idx_node_grp'th node group

  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_node_grp ! index of node group

  integer :: ista, iend

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node_grp .ge. 1) .and. (idx_node_grp .le. mesh_array(idx_mesh)%node_group%n_grp))) then
    write(*,*) 'node group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_node_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%node_group%grp_index(idx_node_grp - 1)
  end if

  iend = mesh_array(idx_mesh)%node_group%grp_index(idx_node_grp)

  ppohFEM_get_num_nodes_in_node_group = iend - ista
end function ppohFEM_get_num_nodes_in_node_group


integer function ppohFEM_get_node_item_in_node_group(idx_mesh, idx_node_grp, idx_item)
  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_node_grp ! index of node group
  integer, intent(IN) :: idx_item     ! index of node in idx_node_grp'th node group

  integer :: ista, iend, num_nodes

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node_grp .ge. 1) .and. (idx_node_grp .le. mesh_array(idx_mesh)%node_group%n_grp))) then
    write(*,*) 'node group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_node_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%node_group%grp_index(idx_node_grp - 1)
  end if
  iend = mesh_array(idx_mesh)%node_group%grp_index(idx_node_grp)

  num_nodes = iend - ista

  if (.not. ((idx_item .ge. 1) .and. (idx_item .le. num_nodes))) then
    write(*,*) 'item index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_node_item_in_node_group = mesh_array(idx_mesh)%node_group%grp_item(ista + idx_item)
end function ppohFEM_get_node_item_in_node_group

integer function ppohFEM_node_grp_name_to_id( idx_mesh, grp_id_name)
        implicit none
        integer, intent(IN) :: idx_mesh
        character(HECMW_NAME_LEN), intent(IN) :: grp_id_name

        integer(kind=kint) :: i, id

        if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
          write(*,*) 'mesh index out of bounds'
          call ppohFEM_abort
        end if

        ppohFEM_node_grp_name_to_id = -1
        do id = 1, ppohFEM_get_num_node_group(idx_mesh)
           if( ppohFEM_streqr(mesh_array(idx_mesh)%node_group%grp_name(id),grp_id_name)) then
              ppohFEM_node_grp_name_to_id = id
              exit
           end if
        end do
        if( ppohFEM_node_grp_name_to_id == -1 ) then
          write(*,*) '### Error:  Node group "', grp_id_name,'" does not exist.'
          call ppohFEM_abort()
        end if
end function ppohFEM_node_grp_name_to_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! element group
!

integer function ppohFEM_get_num_element_group(idx_mesh)
! return number of element group of idx_mesh'th hecMESH
  integer, intent(IN) :: idx_mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_num_element_group = mesh_array(idx_mesh)%elem_group%n_grp

end function ppohFEM_get_num_element_group

integer function ppohFEM_get_num_elements_in_element_group(idx_mesh, idx_elem_grp)
! return number of elements in idx_element_grp'th element group

  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_elem_grp ! index of element group

  integer :: ista, iend

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem_grp .ge. 1) .and. (idx_elem_grp .le. mesh_array(idx_mesh)%elem_group%n_grp))) then
    write(*,*) 'element group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_elem_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%elem_group%grp_index(idx_elem_grp - 1)
  end if

  iend = mesh_array(idx_mesh)%elem_group%grp_index(idx_elem_grp)

  ppohFEM_get_num_elements_in_element_group = iend - ista
end function ppohFEM_get_num_elements_in_element_group

integer function ppohFEM_get_element_item_in_element_group(idx_mesh, idx_elem_grp, idx_item)
  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_elem_grp ! index of element group
  integer, intent(IN) :: idx_item     ! index of element in idx_elem_grp'th element group

  integer :: ista, iend, num_elements

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_elem_grp .ge. 1) .and. (idx_elem_grp .le. mesh_array(idx_mesh)%elem_group%n_grp))) then
    write(*,*) 'element group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_elem_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%elem_group%grp_index(idx_elem_grp - 1)
  end if
  iend = mesh_array(idx_mesh)%elem_group%grp_index(idx_elem_grp)

  num_elements = iend - ista

  if (.not. ((idx_item .ge. 1) .and. (idx_item .le. num_elements))) then
    write(*,*) 'item index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_element_item_in_element_group = mesh_array(idx_mesh)%elem_group%grp_item(ista + idx_item)
end function ppohFEM_get_element_item_in_element_group

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! surface group
!

integer function ppohFEM_get_num_surf_group(idx_mesh)
! return number of surface group of idx_mesh'th hecMESH
  integer, intent(IN) :: idx_mesh

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_num_surf_group = mesh_array(idx_mesh)%surf_group%n_grp

end function ppohFEM_get_num_surf_group


integer function ppohFEM_get_num_items_in_surf_group(idx_mesh, idx_surf_grp)
! return number of items in idx_surf_grp'th surf group

  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_surf_grp ! index of node group

  integer :: ista, iend

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_surf_grp .ge. 1) .and. (idx_surf_grp .le. mesh_array(idx_mesh)%surf_group%n_grp))) then
    write(*,*) 'surface group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_surf_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%surf_group%grp_index(idx_surf_grp - 1)
  end if

  iend = mesh_array(idx_mesh)%surf_group%grp_index(idx_surf_grp)

  ppohFEM_get_num_items_in_surf_group = iend - ista
end function ppohFEM_get_num_items_in_surf_group

integer function ppohFEM_get_item_element_in_surf_group(idx_mesh, idx_surf_grp, idx_item)
! return element ID (local ID) of idx_item'th item in idx_surf_grp'th surface group
  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_surf_grp ! index of surface group
  integer, intent(IN) :: idx_item     ! index of item (element ID and surface number gives one item) in idx_surf_grp'th surf group

  integer :: ista, iend, num_items

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_surf_grp .ge. 1) .and. (idx_surf_grp .le. mesh_array(idx_mesh)%surf_group%n_grp))) then
    write(*,*) 'surface group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_surf_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%surf_group%grp_index(idx_surf_grp - 1)
  end if
  iend = mesh_array(idx_mesh)%surf_group%grp_index(idx_surf_grp)

  num_items = iend - ista

  if (.not. ((idx_item .ge. 1) .and. (idx_item .le. num_items))) then
    write(*,*) 'item index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_item_element_in_surf_group = mesh_array(idx_mesh)%surf_group%grp_item(2*(ista + idx_item)-1)
end function ppohFEM_get_item_element_in_surf_group

integer function ppohFEM_get_item_ltype_in_surf_group(idx_mesh, idx_surf_grp, idx_item)
! return ltype (type of force. local to the element) of idx_item'th item in idx_surf_grp'th surface group
  integer, intent(IN) :: idx_mesh     ! index of mesh
  integer, intent(IN) :: idx_surf_grp ! index of surface group
  integer, intent(IN) :: idx_item     ! index of item (element ID and surface number gives one item) in idx_surf_grp'th surf group

  integer :: ista, iend, num_items

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_surf_grp .ge. 1) .and. (idx_surf_grp .le. mesh_array(idx_mesh)%surf_group%n_grp))) then
    write(*,*) 'surface group index out of bounds'
    call ppohFEM_abort
  end if

  if (idx_surf_grp .eq. 1) then
    ista = 0
  else
    ista = mesh_array(idx_mesh)%surf_group%grp_index(idx_surf_grp - 1)
  end if
  iend = mesh_array(idx_mesh)%surf_group%grp_index(idx_surf_grp)

  num_items = iend - ista

  if (.not. ((idx_item .ge. 1) .and. (idx_item .le. num_items))) then
    write(*,*) 'item index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_item_ltype_in_surf_group = mesh_array(idx_mesh)%surf_group%grp_item(2*(ista + idx_item))
end function ppohFEM_get_item_ltype_in_surf_group

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! stiffness matrix
!

!C ----------------------------------------------------------------------------
        subroutine ppohFEM_mat_init( idx_mat  )
! ----------------------------------------------------------------------------
!  Purpose: Memeory allocation 
!           July.6, 2009  YUAN Xi
! ----------------------------------------------------------------------------
        type( hecmwST_matrix ), pointer :: hecMAT
        integer ::  ndof, nn, ierror
        integer, intent(IN) :: idx_mat
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT)) then
    hecMAT => stiffMAT_array(idx_mat)
  else
    call ppohFEM_abort
  end if

        ndof = hecMAT%NDOF
        nn = ndof*ndof
        allocate (hecMAT%AL(nn*hecMAT%NPL)        ,STAT=ierror )
        if( ierror /= 0 ) then
            write(*,*) "##ERROR : not enough memory"
            write(idbg,*) 'stop due to allocation error'
            call flush(idbg)
            call hecmw_abort( hecmw_comm_get_comm() )
        end if
        allocate (hecMAT%AU(nn*hecMAT%NPU)        ,STAT=ierror )
        if( ierror /= 0 ) then
            write(*,*) "##ERROR : not enough memory"
            write(idbg,*) 'stop due to allocation error'
            call flush(idbg)
            call hecmw_abort( hecmw_comm_get_comm() )
        end if
        allocate (hecMAT%B(ndof*hecMAT%NP)          ,STAT=ierror )
        if( ierror /= 0 ) then
            write(*,*) "##ERROR : not enough memory"
            write(idbg,*) 'stop due to allocation error'
            call flush(idbg)
            call hecmw_abort( hecmw_comm_get_comm() )
        end if
        hecMAT%B(:)=0.d0
        allocate (hecMAT%D(nn*hecMAT%NP)          ,STAT=ierror )
        if( ierror /= 0 ) then
            write(*,*) "##ERROR : not enough memory"
            write(idbg,*) 'stop due to allocation error'
            call flush(idbg)
            call hecmw_abort( hecmw_comm_get_comm() )
        end if
        allocate (hecMAT%X(ndof*hecMAT%NP)          ,STAT=ierror )
        if( ierror /= 0 ) then
            write(*,*) "##ERROR : not enough memory"
            write(idbg,*) 'stop due to allocation error'
            call flush(idbg)
            call hecmw_abort( hecmw_comm_get_comm() )
        end if
        allocate (hecMAT%ALU(nn*hecMAT%N)         ,STAT=ierror )
        if( ierror /= 0 ) then
            write(*,*) "##ERROR : not enough memory"
            write(idbg,*) 'stop due to allocation error'
            call flush(idbg)
            call hecmw_abort( hecmw_comm_get_comm() )
        endif
        call hecmw_cmat_init( hecMAT%cmat )
        end subroutine ppohFEM_mat_init

! ----------------------------------------------------------------------------
        subroutine ppohFEM_hecMAT_finalize( hecMAT )
! ----------------------------------------------------------------------------
!  Purpose: Memeory allocation 
!           July.6, 2009  YUAN Xi
! ----------------------------------------------------------------------------
        type( hecmwST_matrix ) :: hecMAT
        integer ::  ndof, nn, ierror
        ndof = hecMAT%NDOF
        nn = ndof*ndof
        if( associated(hecMAT%AL) ) then
            deallocate(hecMAT%AL                  ,STAT=ierror)
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error'
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
        endif
        if( associated(hecMAT%AU) ) then
            deallocate(hecMAT%AU                  ,STAT=ierror)
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error'
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
        endif
        if( associated(hecMAT%B) ) then
            deallocate(hecMAT%B                   ,STAT=ierror)
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error'
             call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
        endif
        if( associated(hecMAT%D) ) then
            deallocate(hecMAT%D                   ,STAT=ierror)
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error'
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
        endif
        if( associated(HECMAT%X) ) then
            deallocate(hecMAT%X                   ,STAT=ierror)
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error'
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
        endif
        if( associated(hecMAT%ALU) ) then 
            deallocate(hecMAT%ALU                 ,STAT=ierror)
            if( ierror /= 0 ) then
              write(idbg,*) 'stop due to deallocation error'
              call flush(idbg)
              call hecmw_abort( hecmw_comm_get_comm())
            end if
        endif
        call hecmw_cmat_finalize(hecmAT%cmat)
end subroutine ppohFEM_hecMAT_finalize

integer function ppohFEM_get_mat_ndof(idx_mat)
  integer, intent(IN) :: idx_mat ! index of stiffness matrices
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))then
    ppohFEM_get_mat_ndof = stiffMAT_array(idx_mat)%NDOF
  else
    call ppohFEM_abort
  end if
end function ppohFEM_get_mat_ndof

subroutine ppohFEM_set_mat_ndof(idx_mat, arg_ndof)
  integer, intent(IN) :: idx_mat
  integer, intent(IN) :: arg_ndof
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))then
    stiffMAT_array(idx_mat)%NDOF = arg_ndof
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_set_mat_ndof

integer function ppohFEM_get_np(idx_mat)
  integer, intent(IN) :: idx_mat ! index of stiffness matrices
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_np = stiffMAT_array(idx_mat)%NP
end function ppohFEM_get_np

subroutine ppohFEM_abort
  call hecmw_abort(hecmw_comm_get_comm())
end subroutine ppohFEM_abort

real(kind=kreal) function ppohFEM_get_rhs_norm(idx_mat)
  integer, intent(IN) :: idx_mat
  integer :: i
ppohFEM_get_rhs_norm=0.0
do i=1, stiffMAT_array(idx_mat)%NP * stiffMAT_array(idx_mat)%NDOF
  ppohFEM_get_rhs_norm=ppohFEM_get_rhs_norm+stiffMAT_array(idx_mat)%B(i)**2
enddo
call hecmw_allREDUCE_R1( mesh, ppohFEM_get_rhs_norm, hecmw_sum )
end function ppohFEM_get_rhs_norm

real(kind=kreal) function ppohFEM_get_stiffMAT_B(idx_mat, idx_node, idx_dof)
! get right hand side vector of equation with idx_mat'th stiffness matrix
  integer(kind=kint), intent(IN) :: idx_mat  ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: idx_node ! local node ID
  integer(kind=kint), intent(IN) :: idx_dof  ! index of degree of freedom (ex: 1:X, 2:Y, 3:Z...) 

  integer :: ndof

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node .ge. 1) .and. (idx_node .le. stiffMAT_array(idx_mat)%NP ))) then ! NP is number of all nodes 
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)
  ppohFEM_get_stiffMAT_B = stiffMAT_array(idx_mat)%B(ndof*(idx_node-1) + idx_dof)

end function ppohFEM_get_stiffMAT_B

real(kind=kreal) function ppohFEM_get_stiffMAT_B_direct_index(idx_mat, idx_b)
! get right hand side vector of equation with idx_mat'th stiffness matrix
  integer(kind=kint), intent(IN) :: idx_mat  ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: idx_b    ! index of rhs array

  integer :: ndof

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)

  if (.not. ((idx_b .ge. 1) .and. (idx_b .le. stiffMAT_array(idx_mat)%NP*ndof ))) then ! NP is number of all nodes 
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_stiffMAT_B_direct_index = stiffMAT_array(idx_mat)%B(idx_b)

end function ppohFEM_get_stiffMAT_B_direct_index

subroutine ppohFEM_set_stiffMAT_B(idx_mat, idx_node, idx_dof, val)
! set right hand side vector of equation with idx_mat'th stiffness matrix
  integer(kind=kint), intent(IN) :: idx_mat  ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: idx_node ! local node ID
  integer(kind=kint), intent(IN) :: idx_dof  ! index of degree of freedom (ex: 1:X, 2:Y, 3:Z...) 
  real(kind=kreal),   intent(IN) :: val      ! scalar value for right hand side vector item

  integer :: ndof

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node .ge. 1) .and. (idx_node .le. stiffMAT_array(idx_mat)%NP ))) then ! NP is number of all nodes 
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)
  stiffMAT_array(idx_mat)%B(ndof*(idx_node-1) + idx_dof) = val

end subroutine ppohFEM_set_stiffMAT_B


subroutine ppohFEM_set_stiffMAT_B_direct_index(idx_mat, idx_b, val)
! set right hand side vector of equation with idx_mat'th stiffness matrix. array index of b is given directly
  integer(kind=kint), intent(IN) :: idx_mat  ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: idx_b    ! index of rhs array
  real(kind=kreal),   intent(IN) :: val      ! scalar value for right hand side vector item

  integer :: ndof

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)

  if (.not. ((idx_b .ge. 1) .and. (idx_b .le. stiffMAT_array(idx_mat)%NP*ndof ))) then ! NP is number of all nodes 
    write(*,*) 'node b out of bounds'
    call ppohFEM_abort
  end if

  stiffMAT_array(idx_mat)%B(idx_b) = val

end subroutine ppohFEM_set_stiffMAT_B_direct_index

real(kind=kreal) function ppohFEM_get_stiffMAT_X(idx_mat, idx_node, idx_dof)
! get X vector of equation with idx_mat'th stiffness matrix
  integer(kind=kint), intent(IN) :: idx_mat  ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: idx_node ! local node ID
  integer(kind=kint), intent(IN) :: idx_dof  ! index of degree of freedom (ex: 1:X, 2:Y, 3:Z...) 

  integer :: ndof

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_node .ge. 1) .and. (idx_node .le. stiffMAT_array(idx_mat)%NP ))) then ! NP is number of all nodes 
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)
  ppohFEM_get_stiffMAT_X = stiffMAT_array(idx_mat)%X(ndof*(idx_node-1) + idx_dof)

end function ppohFEM_get_stiffMAT_X

real(kind=kreal) function ppohFEM_get_stiffMAT_X_direct_index(idx_mat, idx_x)
! get right hand side vector of equation with idx_mat'th stiffness matrix
  integer(kind=kint), intent(IN) :: idx_mat  ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: idx_x    ! index of X array

  integer :: ndof

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)

  if (.not. ((idx_x .ge. 1) .and. (idx_x .le. stiffMAT_array(idx_mat)%NP*ndof ))) then ! NP is number of all nodes 
    write(*,*) 'node index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_get_stiffMAT_X_direct_index = stiffMAT_array(idx_mat)%X(idx_x)

end function ppohFEM_get_stiffMAT_X_direct_index

real(kind=kreal) function ppohFEM_get_hecMAT_matrix_element_value(idx_mat, i_row_node, j_col_node, k_row_dof, l_col_dof)
  integer, intent(IN) :: idx_mat ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: i_row_node ! row of matrix element in stiffness matrix. Local node ID
  integer(kind=kint), intent(IN) :: j_col_node ! column of matrix element in stiffness matrix. Local node ID
  integer(kind=kint), intent(IN) :: k_row_dof
  integer(kind=kint), intent(IN) :: l_col_dof

  integer :: ndof, idx_mat_elem

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)

  if (i_row_node .eq. j_col_node) then
    idx_mat_elem = ndof**2 * (i_row_node - 1) + ndof * (k_row_dof - 1) + l_col_dof
    ppohFEM_get_hecMAT_matrix_element_value = stiffMAT_array(idx_mat)%D(idx_mat_elem)
  else
    write(*,*) 'currently non diagonal element is not implemented in ppohFEM_get_hecMAT_matrix_element_value'
    call ppohFEM_abort ! currently non diagonal value is not implemented !TODO
  end if

end function ppohFEM_get_hecMAT_matrix_element_value

subroutine ppohFEM_set_hecMAT_matrix_element_value(idx_mat, i_row_node, j_col_node, k_row_dof, l_col_dof, val)
  integer, intent(IN) :: idx_mat ! index of stiffness matrix
  integer(kind=kint), intent(IN) :: i_row_node ! row of matrix element in stiffness matrix. Local node ID
  integer(kind=kint), intent(IN) :: j_col_node ! column of matrix element in stiffness matrix. Local node ID
  integer(kind=kint), intent(IN) :: k_row_dof
  integer(kind=kint), intent(IN) :: l_col_dof
  real(kind=kreal),   intent(IN) :: val

  integer :: ndof, idx_mat_elem

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    call ppohFEM_abort
  end if

  ndof = ppohFEM_get_mat_ndof(idx_mat)

  if (i_row_node .eq. j_col_node) then
    idx_mat_elem = ndof**2 * (i_row_node - 1) + ndof * (k_row_dof - 1) + l_col_dof
    stiffMAT_array(idx_mat)%D(idx_mat_elem) = val
  else
    write(*,*) 'currently non diagonal element is not implemented in ppohFEM_get_hecMAT_matrix_element_value'
    call ppohFEM_abort ! currently non diagonal value is not implemented !TODO
  end if

end subroutine ppohFEM_set_hecMAT_matrix_element_value

subroutine ppohFEM_mat_ass_elem(idx_mat, nn, nodLOCAL, stiffness)
  integer(kind=kint), intent(IN) :: idx_mat
  integer(kind=kint), intent(IN) :: nn
  integer(kind=kint), intent(IN) :: nodLOCAL(:)
  real(kind=kreal), intent(IN) :: stiffness(:, :)
  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT)) then
    call hecmw_mat_ass_elem(stiffMAT_array(idx_mat), nn, nodLOCAL, stiffness)
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_mat_ass_elem


subroutine ppohFEM_allREDUCE_R(idx_mesh, VAL, n, ntag)
  integer, intent(IN) :: idx_mesh
  real(kind=kreal), intent(INOUT) :: VAL(:)
  integer, intent(IN)    :: n
  integer, intent(IN)    :: ntag

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_allREDUCE_R(mesh_array(idx_mesh), VAL, n, ntag)
end subroutine ppohFEM_allREDUCE_R

subroutine ppohFEM_allREDUCE_I(idx_mesh, VAL, n, ntag)
  integer, intent(IN) :: idx_mesh
  integer, intent(INOUT) :: VAL(:)
  integer, intent(IN)    :: n
  integer, intent(IN)    :: ntag

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if


  call hecmw_allREDUCE_I(mesh_array(idx_mesh), VAL, n, ntag)
end subroutine ppohFEM_allREDUCE_I

subroutine ppohFEM_update_1_R(idx_mesh, VAL, n)
  integer(kind=kint), intent(IN)    :: idx_mesh, n
  real(kind=kreal),   intent(INOUT) :: VAL(:)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_update_1_R(mesh_array(idx_mesh), VAL, n)
end subroutine ppohFEM_update_1_R

subroutine ppohFEM_update_2_R(idx_mesh, VAL, n)
  integer(kind=kint), intent(IN)    :: idx_mesh, n
  real(kind=kreal),   intent(INOUT) :: VAL(:)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_update_2_R(mesh_array(idx_mesh), VAL, n)
end subroutine ppohFEM_update_2_R

subroutine ppohFEM_update_3_R(idx_mesh, VAL, n)
  integer(kind=kint), intent(IN)     :: idx_mesh, n
  real(kind=kreal),   intent(INOUT)  :: VAL(:)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_update_3_R(mesh_array(idx_mesh), VAL, n)
end subroutine ppohFEM_update_3_R

subroutine ppohFEM_update_6_R(idx_mesh, VAL, n)
  integer(kind=kint), intent(IN)    :: idx_mesh, n
  real(kind=kreal),   intent(INOUT) :: VAL(:)

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_update_6_R(mesh_array(idx_mesh), VAL, n)
end subroutine ppohFEM_update_6_R

subroutine ppohFEM_result_read_by_name(idx_mesh, name_ID, nstep, tstep, result)
   integer(kind=kint), intent(IN) :: idx_mesh
   character(len=HECMW_NAME_LEN) :: name_ID
   integer(kind=kint) :: nstep, tstep
   type(hecmwST_result_data) :: result

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_result_read_by_name(mesh_array(idx_mesh), name_ID, nstep, tstep, result)
end subroutine ppohFEM_result_read_by_name

subroutine ppohFEM_mat_ass_bc(idx_mat, inode, idof, RHS)
! set boundary condition
  integer(kind=kint), intent(IN) :: idx_mat  ! stiffness matrix index
  integer(kind=kint), intent(IN) :: inode    ! local node ID
  integer(kind=kint), intent(IN) :: idof     ! degree of freedom ID
  real(kind=kreal),   intent(IN) :: RHS      ! right hand side vector value
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_mat_ass_bc(stiffMAT_array(idx_mat), inode, idof, RHS)
end subroutine ppohFEM_mat_ass_bc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! export  hecMESH, hecMAT
! (internal use only. these should be private)
!

subroutine ppohFEM_export_hecMAT(p_MAT, idx_mat)
! return matrix pointer

! return idx_mat'th stiffness matrix 

  type (hecmwST_matrix ), pointer :: p_MAT
  integer, intent(IN)             :: idx_mat

  if ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT)) then
    p_MAT => stiffMAT_array(idx_mat)
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_export_hecMAT

subroutine ppohFEM_export_hecMESH(p_MESH, idx_mesh)
! return mesh pointer

! return idx_mesh'th mesh

  type (hecmwST_local_mesh ), pointer :: p_MESH
  integer, intent(IN)             :: idx_mesh

  if ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH)) then
    p_MESH => mesh_array(idx_mesh)
  else
    call ppohFEM_abort
  end if
end subroutine ppohFEM_export_hecMESH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! solver
!

subroutine ppohFEM_solver_set_method(idx_mat, method)
! set solver method in variable "method" (currently 1:CG only)
  integer, intent(IN) :: idx_mat
  integer, intent(IN) :: method
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. ((method .ge. 1) .and. (method .le. NUM_SOLVER_METHOD))) then
    write(*,*) 'solver method is not supported'
    call ppohFEM_abort
  end if

! default parameters for iteration solver
  stiffMAT_array(idx_mat)%Iarray(1) =  10000    ! = nier 
  stiffMAT_array(idx_mat)%Iarray(3) =    1    ! = precond
  stiffMAT_array(idx_mat)%Iarray(4) =    0    ! = nset
  stiffMAT_array(idx_mat)%Iarray(5) =    1    ! = iterpremax
  stiffMAT_array(idx_mat)%Iarray(6) =   10    ! = nrest
  stiffMAT_array(idx_mat)%Iarray(7) =    0    ! = scaling
  stiffMAT_array(idx_mat)%Iarray(21)=    0    ! = iterlog
  stiffMAT_array(idx_mat)%Iarray(22)=    0    ! = timelog
  stiffMAT_array(idx_mat)%Iarray(31)=    0    ! = dumptype
  stiffMAT_array(idx_mat)%Iarray(32)=    0    ! = dumpexit
  stiffMAT_array(idx_mat)%Iarray(33)=    0    ! = usejad
  stiffMAT_array(idx_mat)%Iarray(34)=   10    ! = ncolor_in

  stiffMAT_array(idx_mat)%Rarray(1) =  1.0e-8 ! = resid 
  stiffMAT_array(idx_mat)%Rarray(2) =  1.0    ! = sigma_diag
  stiffMAT_array(idx_mat)%Rarray(3) =  0.0    ! = sigma 
  stiffMAT_array(idx_mat)%Rarray(4) =  0.1    ! = thresh
  stiffMAT_array(idx_mat)%Rarray(5) =  0.1    ! = filter
  stiffMAT_array(idx_mat)%Rarray(11)=  1.0e+4 ! = penalty
 
! set method
  stiffMAT_array(idx_mat)%Iarray(2)  = method
  stiffMAT_array(idx_mat)%Iarray(99) = 1       ! use iterative solver

  return
end subroutine ppohFEM_solver_set_method

subroutine ppohFEM_solver_set_precond(idx_mat, precond)
! set preconditioning method in variable "precond"
! 1,2 : SSOR
! 3   : Diagonal scaling
! 10  : Block ILU (3 degree of freedom only)

  integer, intent(IN) :: idx_mat
  integer, intent(IN) :: precond
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. (     (precond .eq.  1) .or. (precond .eq. 2) & ! SSOR
              .or. (precond .eq.  3)                       & ! diagonal scaling
              .or. (precond .eq. 10) .or. (precond .eq. 11) .or. (precond .eq. 12) & ! Block ILU
             )) then           
    write(*,*) 'precond method is not supported'
    call ppohFEM_abort
  end if

  stiffMAT_array(idx_mat)%Iarray(3) = precond

  return
end subroutine ppohFEM_solver_set_precond

subroutine ppohFEM_solver_set_num_iteration(idx_mat, iter)
! set iteration number of iterative solver in variable "iter"
  integer, intent(IN) :: idx_mat
  integer, intent(IN) :: iter
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. (iter .ge. 1) ) then
    write(*,*) 'iteration number must be positive'
    call ppohFEM_abort
  end if

  stiffMAT_array(idx_mat)%Iarray(1) = iter
end subroutine ppohFEM_solver_set_num_iteration

subroutine ppohFEM_solver_set_residual(idx_mat, resid)
! set residiual for convergence of iterative solver
  integer, intent(IN) :: idx_mat
  real(8), intent(IN) :: resid
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. (resid .gt. 0) ) then
    write(*,*) 'residual must be positive'
    call ppohFEM_abort
  end if

  stiffMAT_array(idx_mat)%Rarray(1) = resid       ! iterative solver

  return
end subroutine ppohFEM_solver_set_residual

subroutine ppohFEM_solver_set_iterlog(idx_mat, iterlog)
! set iteration log
  integer, intent(IN) :: idx_mat
  logical, intent(IN) :: iterlog
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( iterlog ) then
    stiffMAT_array(idx_mat)%Iarray(21) = 1 ! show iteration log
  else
    stiffMAT_array(idx_mat)%Iarray(21) = 0 ! do not show iteration log
  end if
  return
end subroutine ppohFEM_solver_set_iterlog

subroutine ppohFEM_solver_set_timelog(idx_mat, timelog)
! set iteration log
  integer, intent(IN) :: idx_mat
  logical, intent(IN) :: timelog
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( timelog ) then
    stiffMAT_array(idx_mat)%Iarray(22) = 1 ! show iteration log
  else
    stiffMAT_array(idx_mat)%Iarray(22) = 0 ! do not show iteration log
  end if
  return
end subroutine ppohFEM_solver_set_timelog

integer function ppohFEM_solver_get_method(idx_mat)
! get solver method. 1 is CG
  integer, intent(IN) :: idx_mat
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_solver_get_method = stiffMAT_array(idx_mat)%Iarray(2)

  return
end function ppohFEM_solver_get_method

integer function ppohFEM_solver_get_precond(idx_mat)
! get preconditioning method in variable "precond"
! 1,2 : SSOR
! 3   : Diagonal scaling
! 10  : Block ILU (3 degree of freedom only)

  integer, intent(IN) :: idx_mat
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_solver_get_precond = stiffMAT_array(idx_mat)%Iarray(3)

  return
end function ppohFEM_solver_get_precond

integer function ppohFEM_solver_get_num_iteration(idx_mat)
! get iteration number of iterative solver 
  integer, intent(IN) :: idx_mat
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_solver_get_num_iteration = stiffMAT_array(idx_mat)%Iarray(1)
end function ppohFEM_solver_get_num_iteration

real(8) function ppohFEM_solver_get_residual(idx_mat)
! get residiual for convergence of iterative solver
  integer, intent(IN) :: idx_mat
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  ppohFEM_solver_get_residual = stiffMAT_array(idx_mat)%Rarray(1)

  return
end function ppohFEM_solver_get_residual

logical function ppohFEM_solver_get_iterlog(idx_mat)
! get iteration log or not
  integer, intent(IN) :: idx_mat
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( stiffMAT_array(idx_mat)%Rarray(21) .eq. 1 ) then
    ppohFEM_solver_get_iterlog = .true.
  else
    ppohFEM_solver_get_iterlog = .false.
  end if
  return
end function ppohFEM_solver_get_iterlog

logical function ppohFEM_solver_get_timelog(idx_mat)
! get iteration log
  integer, intent(IN) :: idx_mat
  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if ( stiffMAT_array(idx_mat)%Rarray(22) .eq. 1 ) then
    ppohFEM_solver_get_timelog = .true.
  else
    ppohFEM_solver_get_timelog = .false.
  end if
  return
end function ppohFEM_solver_get_timelog


logical function ppohFEM_is_iterative_solver(idx_mat)
! return true if iterative solver 
  integer, intent(IN) :: idx_mat

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  if (stiffMAT_array(idx_mat)%Iarray(99) .eq. 1) then
    ppohFEM_is_iterative_solver = .true.
  else
    ppohFEM_is_iterative_solver = .false.
  end if
end function ppohFEM_is_iterative_solver
subroutine ppohFEM_solve_11(idx_mesh, idx_mat)
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: idx_mat   ! stiffness matrix index

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_solve_11(mesh_array(idx_mesh), stiffMAT_array(idx_mat))
  return
end subroutine ppohFEM_solve_11

subroutine ppohFEM_solve_22(idx_mesh, idx_mat)
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: idx_mat   ! stiffness matrix index

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_solve_22(mesh_array(idx_mesh), stiffMAT_array(idx_mat))
  return
end subroutine ppohFEM_solve_22

subroutine ppohFEM_solve_33(idx_mesh, idx_mat)
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: idx_mat   ! stiffness matrix index

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_solve_33(mesh_array(idx_mesh), stiffMAT_array(idx_mat))
  return
end subroutine ppohFEM_solve_33

subroutine ppohFEM_solve_44(idx_mesh, idx_mat)
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: idx_mat   ! stiffness matrix index

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_solve_44(mesh_array(idx_mesh), stiffMAT_array(idx_mat))
  return
end subroutine ppohFEM_solve_44

subroutine ppohFEM_solve_66(idx_mesh, idx_mat)
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: idx_mat   ! stiffness matrix index

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if ( .not. ((idx_mat .ge. 1) .and. (idx_mat .le. NUM_STIFF_MAT))) then
    write(*,*) 'matrix index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_solve_66(mesh_array(idx_mesh), stiffMAT_array(idx_mat))
  return
end subroutine ppohFEM_solve_66

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! output
!

subroutine ppohFEM_result_init(idx_mesh, maxstep, istep, header)
! initialize output
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: maxstep
  integer(kind=kint), intent(IN) :: istep
  character(len=HECMW_HEADER_LEN), intent(IN) :: header

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_result_init( mesh_array(idx_mesh), maxstep, istep, header )
end subroutine ppohFEM_result_init

subroutine ppohFEM_result_add(node_or_elem, n_dof, label, data)
 integer(kind=kint) :: node_or_elem, n_dof, ierr
 character(len=HECMW_NAME_LEN) :: label
 real(kind=kreal) :: data(:)

 call hecmw_result_add(node_or_elem, n_dof, label, data)
end subroutine ppohFEM_result_add

subroutine ppohFEM_result_write_by_name(name_ID)
  character(len=HECMW_NAME_LEN), intent(IN) :: name_ID
  call hecmw_result_write_by_name(name_ID)
end subroutine ppohFEM_result_write_by_name

subroutine ppohFEM_result_write_by_addfname( name_ID, addfname )
  character(len=HECMW_NAME_LEN), intent(IN) :: name_ID
  character(len=HECMW_NAME_LEN), intent(IN) :: addfname
  call hecmw_result_write_by_addfname( name_ID, addfname )
end subroutine ppohFEM_result_write_by_addfname

subroutine ppohFEM_result_finalize
  call hecmw_result_finalize
end subroutine ppohFEM_result_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! visualize
!

subroutine ppohFEM_visualize_init()
! initialize visualizer
  call hecmw_visualize_init()
end subroutine ppohFEM_visualize_init

subroutine ppohFEM_visualize_result_data_init(idx_mesh, idx_result_data, node_item_ndof, elem_item_ndof, node_item_label, elem_item_label)
! initialize result data structure.
!
! argument node_item_ndof contains number of items on a node and number of degree of freedom for each item.
!
! example of node_item_ndof
!
!XYZ
!allocate node_item_ndof(1)
!node_item_ndof(1)=3 !XYZ
!
!XYZ, pressure
!allocate node_item_ndof(2)
!node_item_ndof(1)=3 !XYZ
!node_item_ndof(2)=1 !P
!
!XYZ, pressure, UVW flow
!allocate node_item_ndof(3)
!node_item_ndof(1)=3 !XYZ
!node_item_ndof(2)=1 !P
!node_item_ndof(3)=3 !UVW
!
! elem_item_ndof also have ndof information items on a element.
! arguments for node or element are optional. 
! keyword argument can be used. 
!
! ex1: items on node only
! call ppohFEM_visualize_result_data_init(idx_mesh, idx_result_data, node_item_ndof=ndof, node_item_label=label)
!
! ex2: items on element only
! call ppohFEM_visualize_result_data_init(idx_mesh, idx_result_data, elem_item_ndof=ndof, elem_item_label=label)
!
! ex3: items both on node and element 
! call ppohFEM_visualize_result_data_init(idx_mesh, idx_result_data, node_item_ndof, node_item_label, elem_item_ndof, elem_item_label)

  integer, intent(IN) :: idx_mesh
  integer, intent(IN) :: idx_result_data
  integer, optional, intent(IN) :: node_item_ndof(:)
  integer, optional, intent(IN) :: elem_item_ndof(:)
  character(len=PPOHFEM_NAME_LEN), optional, intent(IN) :: node_item_label(:)
  character(len=PPOHFEM_NAME_LEN), optional, intent(IN) :: elem_item_label(:)

  type(hecmwST_result_data), pointer :: result_data
  integer :: nn_comp, ne_comp

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_result_data .ge. 1) .and. (idx_result_data .le. NUM_RESULT))) then
    write(*,*) 'result index out of bounds'
    call ppohFEM_abort
  end if

  result_data => result_data_array(idx_result_data)


  call hecmw_nullify_result_data(result_data)

  
  if (present(node_item_ndof)) then
    nn_comp = size(node_item_ndof)
    allocate( result_data%nn_dof(nn_comp))
    allocate( result_data%node_label(nn_comp) )
    allocate( result_data%node_val_item(sum(node_item_ndof)*ppohFEM_get_n_node(idx_mesh)) )
    result_data%nn_component = nn_comp
    result_data%nn_dof(:)=node_item_ndof(:)
    result_data%node_label(:)=node_item_label(:)
  else
    result_data%nn_component = 0
  end if
  
  if (present(elem_item_ndof)) then
    ne_comp = size(elem_item_ndof)
    allocate( result_data%ne_dof(ne_comp))
    allocate( result_data%elem_label(ne_comp))
    allocate( result_data%elem_val_item(sum(elem_item_ndof)*ppohFEM_get_n_elem(idx_mesh)) )
    result_data%ne_component = ne_comp
    result_data%ne_dof(:)=elem_item_ndof(:)
    result_data%elem_label(:)=elem_item_label(:)
  else
    result_data%ne_component = 0
  end if

end subroutine ppohFEM_visualize_result_data_init

subroutine ppohFEM_visualize_set_node_item_val(idx_result_data, inode, jcomp, kdof, val)
  integer, intent(IN) :: idx_result_data
  integer, intent(IN) :: inode
  integer, intent(IN) :: jcomp
  integer, intent(IN) :: kdof
  real(8), intent(IN) :: val

  type(hecmwST_result_data), pointer :: result_data

  if (.not. ((idx_result_data .ge. 1) .and. (idx_result_data .le. NUM_RESULT))) then
    write(*,*) 'result index out of bounds'
    call ppohFEM_abort
  end if

  result_data => result_data_array(idx_result_data)

  result_data%node_val_item(((inode-1)*(sum(result_data%nn_dof(:))))+sum(result_data%nn_dof(1:jcomp-1))+kdof) = val

end subroutine ppohFEM_visualize_set_node_item_val


subroutine ppohFEM_visualize_set_elem_item_val(idx_result_data, ielem, jcomp, kdof, val)
  integer, intent(IN) :: idx_result_data
  integer, intent(IN) :: ielem
  integer, intent(IN) :: jcomp
  integer, intent(IN) :: kdof
  real(8), intent(IN) :: val

  type(hecmwST_result_data), pointer :: result_data

  if (.not. ((idx_result_data .ge. 1) .and. (idx_result_data .le. NUM_RESULT))) then
    write(*,*) 'result index out of bounds'
    call ppohFEM_abort
  end if

  result_data => result_data_array(idx_result_data)

  result_data%elem_val_item(((ielem-1)*(sum(result_data%nn_dof(:))))+sum(result_data%nn_dof(1:jcomp-1))+kdof) = val

end subroutine ppohFEM_visualize_set_elem_item_val

subroutine ppohFEM_visualize( idx_mesh, idx_result_data, step, max_step, interval)
! do visualize
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  integer(kind=kint), intent(IN) :: idx_result_data ! result data index
  integer(kind=kint), intent(IN) :: step, max_step, interval

  if (.not. ((idx_mesh .ge. 1) .and. (idx_mesh .le. NUM_MESH))) then
    write(*,*) 'mesh index out of bounds'
    call ppohFEM_abort
  end if

  if (.not. ((idx_result_data .ge. 1) .and. (idx_result_data .le. NUM_RESULT))) then
    write(*,*) 'result index out of bounds'
    call ppohFEM_abort
  end if

  call hecmw_visualize( mesh_array(idx_mesh), result_data_array(idx_result_data), step, max_step, interval )
end subroutine ppohFEM_visualize

subroutine ppohFEM_visualize_ext( idx_mesh, result_data, step, max_step, interval)
! do visualize using external result data structure
  integer(kind=kint), intent(IN) :: idx_mesh  ! mesh index
  type(hecmwST_result_data), intent(IN) :: result_data ! result data
  integer(kind=kint), intent(IN) :: step, max_step, interval

  call hecmw_visualize( mesh_array(idx_mesh), result_data, step, max_step, interval )
end subroutine ppohFEM_visualize_ext

subroutine ppohFEM_visualize_finalize()
! finalize visualizer
  call hecmw_visualize_finalize()
end subroutine ppohFEM_visualize_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! helper functions
!
logical function ppohFEM_streqr( s1, s2 )
        implicit none
        character(*) :: s1, s2
        integer :: i, n, a1,a2, da

        ppohFEM_streqr = .false.
        n = len_trim(s1)
        if( n /= len_trim(s2)) return

        da = iachar('a') - iachar('A')
        do i = 1, n
                a1 = iachar(s1(i:i))
                a2 = iachar(s2(i:i))
                if( a1 /= a2 ) then
                        if((a1 - da /= a2) .and. (a1 /= a2 - da)) return
                end if
        end do
        ppohFEM_streqr = .true.
end function ppohFEM_streqr

end module ppohFEM
