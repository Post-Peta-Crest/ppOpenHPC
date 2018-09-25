!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_grid_base
  use jcup_constant, only : MAX_GRID, MAX_MODEL, NO_GRID, NAME_LEN
  private

!--------------------------------   public  ----------------------------------!

  public :: init_grid_base
  public :: destruct_grid_base
  public :: set_my_area_info ! subroutine (grid_index, component_name, grid_name)
  public :: exchange_grid_info
  public :: global_index_to_local_index ! subroutine (comp_id, grid_num, global_index, local_index)
  public :: get_num_of_component ! integer function
  public :: get_num_of_grid ! integer function (component_id)
  public :: get_num_of_point ! integer function (component_id, grid_num)
  public :: get_comp_name_from_grid_name ! character function (grid_name)
  public :: get_grid_num ! integer function (component_name, grid_name)
  public :: get_num_of_pe ! integer function (component_id)
  public :: get_pe_num ! integer function (component_id, grid_num, grid_index)
  public :: get_grid_min_index ! integer function (component_id, grid_num)
  public :: get_grid_max_index ! integer function (component_id, grid_num)
  public :: get_my_local_area ! subroutine (component_id, grid_num, is, ie, js, je, ks, ke)
  public :: get_my_local_area_ptr ! type(local_area_type), pointer function (component_id, grid_num)
  public :: send_index2pe ! subroutine (my_comp_id, my_grid_num, dest_comp_id, dest_grid_num)
  public :: recv_index2pe ! subroutine (source_comp_id, source_grid_num)
  public :: get_grid_info ! subroutine (comp_name, grid_name, num_of_index, min_index, max_index)

  public :: local_area_type

!--------------------------------   private ----------------------------------!

  type local_area_type
    integer :: num_of_point
    integer, pointer :: grid_index(:)
    integer :: min_index, max_index
    character(len=NAME_LEN) :: grid_name 
    integer :: grid_num
  end type

  type global_area_type
    integer :: num_of_global_point
    integer :: num_of_index
    integer :: min_index, max_index
    integer, pointer :: index2pe(:) ! mapping table of grid index to pe number
    character(len=NAME_LEN) :: grid_name
    integer :: grid_num 
  end type

  type component_area_type
    integer :: num_of_grid
    type(local_area_type) :: local_area(MAX_GRID) ! number of grid of the component
    type(global_area_type) :: global_area(MAX_GRID) 
    character(len=NAME_LEN) :: comp_name
    integer :: comp_num ! component number
    integer :: num_of_pe
  end type

  integer :: num_of_component
  type(component_area_type), pointer :: comp_area(:) ! number of total component

  type(component_area_type), pointer :: current_area

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_grid_base()
  use jcup_comp, only : get_num_of_total_component
  use jcup_mpi_lib, only : jml_GetCommSizeLocal
  implicit none
  integer :: cmp, grd

  num_of_component = get_num_of_total_component()
  allocate(comp_area(num_of_component))

  do cmp = 1, num_of_component
    call init_area_info(comp_area(cmp))
    comp_area(cmp)%comp_num = cmp
    comp_area(cmp)%num_of_pe = jml_GetCommSizeLocal(cmp)
  end do

end subroutine init_grid_base

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_area_info(comp_area)
  implicit none
  type(component_area_type), intent(INOUT) :: comp_area
  integer :: grd

    comp_area%num_of_grid = 0
    comp_area%comp_name=""
    do grd = 1, MAX_GRID
      comp_area%local_area(grd)%num_of_point = 0
      comp_area%local_area(grd)%grid_index => NULL()
      comp_area%local_area(grd)%min_index = 9999999
      comp_area%local_area(grd)%max_index = -9999999
      comp_area%local_area(grd)%grid_name = ""
      comp_area%local_area(grd)%grid_num = 0
    end do
    do grd = 1, MAX_GRID
      comp_area%global_area(grd)%num_of_global_point = 0
      comp_area%global_area(grd)%num_of_index = 0
      comp_area%global_area(grd)%min_index = 9999999
      comp_area%global_area(grd)%max_index = -9999999
      comp_area%global_area(grd)%index2pe => NULL()
      comp_area%global_area(grd)%grid_name = ""
      comp_area%global_area(grd)%grid_num = 0
    end do

end subroutine init_area_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_grid_base() 
  implicit none
  integer :: cmp

  do cmp = 1, num_of_component
    call destruct_area_info(comp_area(cmp))
  end do

  deallocate(comp_area)

end subroutine destruct_grid_base

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_area_info(comp_area)
  implicit none
  type(component_area_type), intent(INOUT) :: comp_area
  integer :: grd

    do grd = 1, comp_area%num_of_grid
      if (associated(comp_area%local_area(grd)%grid_index)) then
        deallocate(comp_area%local_area(grd)%grid_index)
      end if
      if (associated(comp_area%global_area(grd)%index2pe)) then
        deallocate(comp_area%global_area(grd)%index2pe)
      end if
    end do

end subroutine destruct_area_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_my_area_info(grid_index, component_name, grid_name)
  use jcup_utils, only : error
  use jcup_mpi_lib, only : jml_GetCommSizeLocal
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  integer, intent(IN) :: grid_index(:)
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  integer :: cmp, grd

  cmp = get_comp_id_from_name(component_name)

  comp_area(cmp)%num_of_grid = comp_area(cmp)%num_of_grid + 1
  grd = comp_area(cmp)%num_of_grid

  comp_area(cmp)%comp_name = trim(component_name)
  comp_area(cmp)%comp_num = cmp 
  comp_area(cmp)%num_of_pe = jml_GetCommSizeLocal(cmp)

  comp_area(cmp)%local_area(grd)%grid_name = trim(grid_name)
  comp_area(cmp)%local_area(grd)%grid_num = grd
  comp_area(cmp)%global_area(grd)%grid_name = trim(grid_name)
  comp_area(cmp)%global_area(grd)%grid_num = grd

  comp_area(cmp)%local_area(grd)%num_of_point = size(grid_index)
  allocate(comp_area(cmp)%local_area(grd)%grid_index(size(grid_index)))

  comp_area(cmp)%local_area(grd)%grid_index(:) = grid_index(:)
  comp_area(cmp)%local_area(grd)%min_index = minval(grid_index)
  comp_area(cmp)%local_area(grd)%max_index = maxval(grid_index)

  call set_global_mapping_table(cmp, comp_area(cmp)%local_area(grd), &
                                     comp_area(cmp)%global_area(grd))

end subroutine set_my_area_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_global_mapping_table(comp_id, local_area, global_area)
  use jcup_mpi_lib, only : jml_ReduceMinLocal, jml_ReduceMaxLocal, jml_isLocalLeader, &
                           jml_GetCommSizeLocal, jml_SendLocal, jml_RecvLocal, jml_ReduceSumLocal, jml_BcastLocal
  implicit none
  integer, intent(IN) :: comp_id
  type(local_area_type), intent(IN) :: local_area
  type(global_area_type), intent(INOUT) :: global_area
  integer, allocatable :: index_buffer(:)
  integer :: int_buffer(4)
  integer :: res, pe, i

  res = 0

  int_buffer(1) = local_area%min_index
  call jml_ReduceMinLocal(comp_id, int_buffer(1), res)
  global_area%min_index = res

  int_buffer(1) = local_area%max_index
  call jml_ReduceMaxLocal(comp_id, int_buffer(1), res)

  global_area%max_index = res
  global_area%num_of_index = global_area%max_index - global_area%min_index + 1

  int_buffer(1) = local_area%num_of_point
  call jml_ReduceSumLocal(comp_id, int_buffer(1), res)
  global_area%num_of_global_point = res


  if (jml_isLocalLeader(comp_id)) then
    allocate(global_area%index2pe(global_area%min_index:global_area%max_index))
    global_area%index2pe(:) = 0
    !allocate(index_buffer(global_area%min_index:global_area%max_index))
    allocate(index_buffer(1:global_area%max_index-global_area%min_index+1)) ! 2014/02/14 [MOD]
  else 
    allocate(global_area%index2pe(1))
    allocate(index_buffer(1))
  end if
  
  if (jml_isLocalLeader(comp_id)) then
    do i = 1, local_area%num_of_point
      global_area%index2pe(local_area%grid_index(i)) = 1
    end do

    do pe = 2, jml_GetCommSizeLocal(comp_id)
      call jml_RecvLocal(comp_id, int_buffer, 1, 1, pe-1)
      call jml_RecvLocal(comp_id, index_buffer, 1, int_buffer(1), pe-1)
      do i = 1, int_buffer(1)
        global_area%index2pe(index_buffer(i)) = pe
      end do
    end do
  else
    int_buffer(1) = local_area%num_of_point
    call jml_SendLocal(comp_id, int_buffer, 1, 1, 0)
    call jml_SendLocal(comp_id, local_area%grid_index, 1, local_area%num_of_point, 0)
  end if    

  if (jml_isLocalLeader(comp_id)) then
    int_buffer(1) = global_area%num_of_global_point
    int_buffer(2) = global_area%num_of_index
    int_buffer(3) = global_area%min_index
    int_buffer(4) = global_area%max_index
  end if

  call jml_BcastLocal(comp_id, int_buffer, 1, 4, 0)

  global_area%num_of_global_point = int_buffer(1)
  global_area%num_of_index = int_buffer(2)
  global_area%min_index = int_buffer(3)
  global_area%max_index = int_buffer(4)

  deallocate(index_buffer)

end subroutine set_global_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine exchange_grid_info()
  use jcup_comp, only : is_my_component
  use jcup_mpi_lib, only : jml_GetLeaderRank, jml_isLocalLeader, jml_BcastGlobal
  implicit none
  integer :: local_leader_pe
  integer :: cmp, grd
  integer :: int_buffer(6)
  character(len=NAME_LEN) :: name_buffer

  do cmp = 1, get_num_of_component()

    local_leader_pe = jml_GetLeaderRank(cmp)

    if (jml_isLocalLeader(cmp)) then
      int_buffer(1) = comp_area(cmp)%num_of_grid
      int_buffer(2) = comp_area(cmp)%num_of_pe
      name_buffer = trim(comp_area(cmp)%comp_name)
    end if

    call jml_BcastGlobal(int_buffer,1,2,local_leader_pe)
    call jml_BcastGlobal(name_buffer, local_leader_pe)

    if (.not.is_my_component(cmp)) then
      comp_area(cmp)%num_of_grid = int_buffer(1)
      comp_area(cmp)%num_of_pe = int_buffer(2)
      comp_area(cmp)%comp_name = trim(name_buffer)
    end if

    do grd = 1, comp_area(cmp)%num_of_grid

      if (jml_isLocalLeader(cmp)) then
        int_buffer(1) = comp_area(cmp)%global_area(grd)%num_of_global_point
        int_buffer(2) = comp_area(cmp)%global_area(grd)%num_of_index
        int_buffer(3) = comp_area(cmp)%global_area(grd)%min_index
        int_buffer(4) = comp_area(cmp)%global_area(grd)%max_index
        int_buffer(5) = comp_area(cmp)%global_area(grd)%grid_num
        name_buffer = trim(comp_area(cmp)%global_area(grd)%grid_name)
      end if

      call jml_BcastGlobal(int_buffer,1,5,local_leader_pe)
      call jml_BcastGlobal(name_buffer, local_leader_pe)

      if (.not.is_my_component(cmp)) then
        comp_area(cmp)%global_area(grd)%num_of_global_point = int_buffer(1)
        comp_area(cmp)%global_area(grd)%num_of_index = int_buffer(2)
        comp_area(cmp)%global_area(grd)%min_index = int_buffer(3)
        comp_area(cmp)%global_area(grd)%max_index = int_buffer(4)
        comp_area(cmp)%global_area(grd)%grid_num = int_buffer(5)
        comp_area(cmp)%global_area(grd)%grid_name = trim(name_buffer)
      end if

    end do
  end do

end subroutine exchange_grid_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_write_area_info()
  use jcup_constant, only : DEBUG_FILE_ID
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  integer :: debug_file
  integer :: i, j, k

  ! debug write
  debug_file = DEBUG_FILE_ID + jml_GetMyrankGlobal()
  write(debug_file, *) "================================================================="
  write(debug_file, *) "set_local_area_info, number of local component: ", get_num_of_component()
  do i = 1, get_num_of_component()
    write(debug_file, *)  "local component : ", trim(comp_area(i)%comp_name)
    write(debug_file, *)  "    number of grid  : ", comp_area(i)%num_of_grid
    do j = 1, comp_area(i)%num_of_grid
      write(debug_file, *) "    grid name : ",  trim(comp_area(i)%global_area(j)%grid_name)
      !write(debug_file, '(6I6)')  area_info%comp_area(i)%global_area(j)%g_is, &
      !                      area_info%comp_area(i)%global_area(j)%g_ie, &
      !                      area_info%comp_area(i)%global_area(j)%g_js, &
      !                      area_info%comp_area(i)%global_area(j)%g_je, &
      !                      area_info%comp_area(i)%global_area(j)%g_ks, &
      !                      area_info%comp_area(i)%global_area(j)%g_ke
 
     !write(debug_file, *) "  local grid : ", area_info%num_of_pe
     !do k = 1, area_info%num_of_pe
     !    write(debug_file, '(A6,6I6)') "      ", &
     !                               area_info%comp_area(i)%global_area(j)%local_area(k)%is, &
     !                               area_info%comp_area(i)%global_area(j)%local_area(k)%ie, &
     !                               area_info%comp_area(i)%global_area(j)%local_area(k)%js, &
     !                               area_info%comp_area(i)%global_area(j)%local_area(k)%je, &
     !                               area_info%comp_area(i)%global_area(j)%local_area(k)%ks, &
     !                               area_info%comp_area(i)%global_area(j)%local_area(k)%ke
    ! 
    !  end do

    end do
  end do

end subroutine check_write_area_info

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_component()
  implicit none
  
  get_num_of_component = num_of_component

end function get_num_of_component

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_grid(comp_id)
  implicit none
  integer, intent(IN) :: comp_id

  get_num_of_grid = comp_area(comp_id)%num_of_grid

end function get_num_of_grid

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_point(comp_id, grid_num)
  implicit none
  integer, intent(IN) :: comp_id, grid_num

  get_num_of_point = comp_area(comp_id)%local_area(grid_num)%num_of_point

end function get_num_of_point

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_comp_name_from_grid_name(grid_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: grid_name
  integer :: cmp, grd

  do cmp = 1, get_num_of_component()
    do grd = 1, get_num_of_grid(cmp)
      if (trim(grid_name)==trim(comp_area(cmp)%local_area(grd)%grid_name)) then
        get_comp_name_from_grid_name = comp_area(cmp)%comp_name
        return
      end if
    end do
  end do

  call error("get_comp_name_from_grid_name", "no such grid name: "//trim(grid_name))

end function get_comp_name_from_grid_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_grid_num(comp_name, grid_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: comp_name, grid_name
  integer :: cmp, grd

  do cmp = 1, get_num_of_component()
    if (trim(comp_name)==trim(comp_area(cmp)%comp_name)) then
      do grd = 1, get_num_of_grid(cmp)
        if (trim(grid_name)==trim(comp_area(cmp)%global_area(grd)%grid_name)) then
          get_grid_num = comp_area(cmp)%global_area(grd)%grid_num
          return
        end if
      end do
    end if
  end do

  call error("get_grid_num", "no such component name: "//trim(comp_name)//", or grid name: "//trim(grid_name))
       
end function get_grid_num

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_pe(comp_id)
  implicit none
  integer, intent(IN) :: comp_id

  get_num_of_pe = comp_area(comp_id)%num_of_pe

end function get_num_of_pe

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_pe_num(comp_id, grid_num, grid_index)
  use jcup_constant, only : NO_GRID
  implicit none
  integer, intent(IN) :: comp_id, grid_num, grid_index

  !!write(0,*) "get_pe_num ", comp_id, grid_num, grid_index, &
  !!     size(comp_area(comp_id)%global_area(grid_num)%index2pe), &
  !!     maxval(comp_area(comp_id)%global_area(grid_num)%index2pe)
  get_pe_num = comp_area(comp_id)%global_area(grid_num)%index2pe(grid_index)

end function get_pe_num

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_grid_min_index(comp_id, grid_num)
  implicit none
  integer, intent(IN) :: comp_id, grid_num

  get_grid_min_index = comp_area(comp_id)%global_area(grid_num)%min_index

end function get_grid_min_index

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_grid_max_index(comp_id, grid_num)
  implicit none
  integer, intent(IN) :: comp_id, grid_num

  get_grid_max_index = comp_area(comp_id)%global_area(grid_num)%max_index

end function get_grid_max_index

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine global_index_to_local_index(comp_id, grid_num, global_area, local_index)
  use jcup_constant, only : NO_GRID
  implicit none
  integer, intent(IN) :: comp_id, grid_num
  integer, intent(IN) :: global_area
  integer, intent(OUT) :: local_index
  integer :: i

  local_index = NO_GRID

  do i = 1, comp_area(comp_id)%local_area(grid_num)%num_of_point
    if (global_area == comp_area(comp_id)%local_area(grid_num)%grid_index(i)) then
      local_index = i
      return
    end if
  end do

end subroutine global_index_to_local_index

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_component_area_ptr(comp_name) result(component_area_ptr)
  use jcup_utils, only  : error
  implicit none
  character(len=*), intent(IN) :: comp_name
  type(component_area_type), pointer :: component_area_ptr
  integer :: i

  do i = 1, get_num_of_component()
    if (trim(comp_name)==trim(comp_area(i)%comp_name)) then
      component_area_ptr => comp_area(i)
      return
    end if
  end do
  
  call error("get_component_area_ptr", "no such component name: "//trim(comp_name))

end function get_component_area_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_my_local_area_ptr(comp_id, grid_num) result(local_area_ptr)
  implicit none
  integer, intent(IN) :: comp_id, grid_num
  type(local_area_type), pointer :: local_area_ptr

  local_area_ptr => comp_area(comp_id)%local_area(grid_num)

end function get_my_local_area_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/19
subroutine send_index2pe(my_comp_id, my_grid_num, dest_comp_id, dest_grid_num)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLeader
  implicit none
  integer, intent(IN) :: my_comp_id, my_grid_num
  integer, intent(IN) :: dest_comp_id, dest_grid_num
  integer :: int_buffer(2)
  integer, allocatable :: index_buffer(:)
  integer :: counter, i

  if (.not.jml_isLocalLeader(my_comp_id)) return

  int_buffer(1) = comp_area(my_comp_id)%global_area(my_grid_num)%min_index
  int_buffer(2) = comp_area(my_comp_id)%global_area(my_grid_num)%max_index

  !!!write(0,*) "send_index2pe,",my_comp_id, dest_comp_id, int_buffer(1), &
  !!!       size(comp_area(my_comp_id)%global_area(my_grid_num)%index2pe), &
  !!!       maxval(comp_area(my_comp_id)%global_area(my_grid_nuM)%index2pe) 
  call jml_SendLeader(int_buffer, 1, 2, dest_comp_id-1)

  allocate(index_buffer(int_buffer(2)-int_buffer(1)+1))

  counter = 0
  do i = int_buffer(1), int_buffer(2)
    counter = counter + 1
    index_buffer(counter) = comp_area(my_comp_id)%global_area(my_grid_num)%index2pe(i)
  end do

  call jml_SendLeader(index_buffer, 1, int_buffer(2)-int_buffer(1)+1, dest_comp_id-1)
  !call jml_SendLeader(comp_area(my_comp_id)%global_area(my_grid_num)%index2pe, 1, int_buffer(1), dest_comp_id-1)

  deallocate(index_buffer)

end subroutine send_index2pe

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/19
subroutine recv_index2pe(my_comp_id, source_comp_id, source_grid_num)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_RecvLeader
  implicit none
  integer, intent(IN) :: my_comp_id
  integer, intent(IN) :: source_comp_id, source_grid_num
  integer :: int_buffer(2)
  integer :: cmp, grd
  integer, allocatable :: index_buffer(:)
  integer :: counter, i

  if (.not.jml_isLocalLeader(my_comp_id)) return

  cmp = source_comp_id
  grd = source_grid_num
  
  call jml_RecvLeader(int_buffer, 1, 2, source_comp_id-1)


  comp_area(cmp)%global_area(grd)%num_of_index = int_buffer(2)-int_buffer(1)+1
  comp_area(cmp)%global_area(grd)%min_index = int_buffer(1)
  comp_area(cmp)%global_area(grd)%max_index = int_buffer(2)

  allocate(comp_area(cmp)%global_area(grd)%index2pe(int_buffer(1):int_buffer(2)))

  allocate(index_buffer(int_buffer(2)-int_buffer(1)+1))

  !call jml_RecvLeader(comp_area(cmp)%global_area(grd)%index2pe, 1, int_buffer(1), source_comp_id-1)
  call jml_RecvLeader(index_buffer, 1, int_buffer(2)-int_buffer(1)+1, source_comp_id-1)

  counter = 0
  do i = int_buffer(1), int_buffer(2)
    counter = counter + 1
    comp_area(cmp)%global_area(grd)%index2pe(i) = index_buffer(counter)
  end do

  deallocate(index_buffer)

  !!!write(0,*) "recv_index2pe,",cmp, grd, int_buffer(1), &
  !!!       size(comp_area(cmp)%global_area(grd)%index2pe), &
  !!!       maxval(comp_area(cmp)%global_area(grd)%index2pe) 

end subroutine recv_index2pe

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_my_local_area(comp_id, grid_num, is, ie, js, je, ks, ke)
  implicit none
  integer, intent(IN) :: comp_id, grid_num
  integer, intent(INOUT) :: is, ie, js, je, ks, ke

  is = 1
  ie = comp_area(comp_id)%local_area(grid_num)%num_of_point
  js = 1
  je = 1
  ks = 1
  ke = 1

end subroutine get_my_local_area

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_grid_info(comp_name, grid_name, num_of_index, min_index, max_index)
  implicit none
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: grid_name
  integer, intent(OUT) :: num_of_index
  integer, intent(OUT) :: min_index
  integer, intent(OUT) :: max_index
  integer :: i, j

  do i = 1, num_of_component
    if (trim(comp_area(i)%comp_name) == trim(comp_name)) then
      do j = 1, comp_area(i)%num_of_grid
        if (trim(comp_area(i)%global_area(j)%grid_name) == trim(grid_name)) then
          num_of_index = comp_area(i)%global_area(j)%num_of_index
          min_index = comp_area(i)%global_area(j)%min_index
          max_index = comp_area(i)%global_area(j)%max_index
        end if
      end do 
    end if
  end do
  
end subroutine get_grid_info

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_grid_base

