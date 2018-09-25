!=======+=========+=========+=========+=========+=========+=========+=========+

!---------------------      module jcup_data_buffer     ----------------------!

!=======+=========+=========+=========+=========+=========+=========+=========+
!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_data_buffer
  use jcup_constant, only : NAME_LEN, REAL_DATA, DOUBLE_DATA, DATA_1D, DATA_2D, DATA_3D
  private

!--------------------------------   public  ----------------------------------!

  public :: data_buffer
  public :: init_data_buffer
  public :: destruct_data_buffer
  public :: put_data
  public :: get_data
  public :: get_name
  public :: is_using
  public :: get_data_type
  public :: get_next_ptr
  public :: reset_data_buffer
  !!public :: reset_all_data_buffer
  public :: reset_comp_data_buffer
  public :: get_num_of_using_data_buffer
  public :: search_data_buffer
  public :: write_data_buffer_info
  public :: write_data_buffer    ! subroutine (data_time, db, fid, comp_id, write_flag, data_name, data_ptr)
  public :: restore_data_buffer  ! subroutine (dt, component_id, data_id, name, data_type, data_dim, db_start) ! 2013.06.13 [ADD]

  integer, parameter, public :: NO_DATA = 9999999

  type data_buffer
    private
    integer :: component_id
    integer :: data_id
    real(kind=8), pointer :: double_d(:)
    character(len=NAME_LEN) :: name
    logical :: is_using
    integer :: data_type ! real, double 
    integer :: data_dim  ! 2D or 3D
    type(data_buffer), pointer :: before_ptr, next_ptr
  end type
  
!--------------------------------   private  ---------------------------------!


  interface put_data
    module procedure put_data_double_1d
    module procedure put_data_double_2d
    module procedure put_data_double_3d
  end interface

  interface get_data
    module procedure get_data_double_1d
    module procedure get_data_double_2d
    module procedure get_data_double_3d
  end interface

  private :: insert_data_buffer
  private :: delete_data_buffer
  private :: put_data_double_2d
  private :: put_data_double_3d
  private :: get_data_double_2d
  private :: get_data_double_3d

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_data_buffer(db)
  implicit none
  type(data_buffer), pointer :: db
  
  allocate(db)
  db%before_ptr => db
  db%next_ptr => db
  db%name = repeat(" ",NAME_LEN)
  db%is_using = .false.
  nullify(db%double_d)

end subroutine init_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_data_buffer(db)
  implicit none
  type(data_buffer), pointer :: db
  type(data_buffer), pointer :: start_ptr
  type(data_buffer), pointer :: tmp_ptr

  start_ptr => db
  do 
    tmp_ptr => db%next_ptr
    if (associated(tmp_ptr, start_ptr)) exit
    nullify(db%before_ptr)
    nullify(db%next_ptr)
    if (associated(db%double_d)) deallocate(db%double_d)
    deallocate(db)
    db => tmp_ptr
  end do

end subroutine destruct_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine insert_data_buffer(db)
  implicit none
  type(data_buffer), pointer :: db

  type(data_buffer), pointer :: new_data

  call init_data_buffer(new_data)
  new_data%before_ptr => db
  if (associated(db%next_ptr)) then 
    new_data%next_ptr => db%next_ptr
    db%next_ptr%before_ptr => new_data
  end if
  db%next_ptr => new_data 
  db => new_data

end subroutine insert_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine delete_data_buffer(db)
  implicit none
  type(data_buffer), pointer :: db

  db%before_ptr%next_ptr => db%next_ptr
  db%next_ptr%before_ptr => db%before_ptr
  
  if (associated(db%double_d)) deallocate(db%double_d)
  deallocate(db)

end subroutine delete_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_1d_to_1d(dtin, dt1d)
  implicit none
  real(kind=8), intent(IN) :: dtin(:)
  real(kind=8), intent(INOUT) :: dt1d(:)
  integer :: i

  do i = 1, size(dtin)
    dt1d(i) = dtin(i)
  end do

end subroutine set_1d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_2d_to_1d(dt2d, dt1d)
  implicit none
  real(kind=8), intent(IN) :: dt2d(:,:)
  real(kind=8), intent(INOUT) :: dt1d(:)
  integer :: i, j, ij

  ij = 0
  do j = 1, size(dt2d,2)
    do i = 1, size(dt2d,1)
      ij = ij+1
      dt1d(ij) = dt2d(i,j)
    end do
  end do

end subroutine set_2d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_3d_to_1d(dt3d, dt1d)
  implicit none
  real(kind=8), intent(IN) :: dt3d(:,:,:)
  real(kind=8), intent(INOUT) :: dt1d(:)
  integer :: i, j, k, ijk

  ijk = 0
  do k = 1, size(dt3d,3)
    do j = 1, size(dt3d,2)
      do i = 1, size(dt3d,1)
        ijk = ijk+1
        dt1d(ijk) = dt3d(i,j,k)
      end do
    end do
  end do

end subroutine set_3d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine add_1d_to_1d(dtin, weight, dt1d)
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  real(kind=8), intent(IN) :: dtin(:)
  real(kind=8), intent(IN) :: weight
  real(kind=8), intent(INOUT) :: dt1d(:)
  integer :: i, j, ij

  do i = 1, size(dtin)
    dt1d(i) = dt1d(i)+dtin(i)*weight
  end do

end subroutine add_1d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine add_2d_to_1d(dt2d, weight, dt1d)
  implicit none
  real(kind=8), intent(IN) :: dt2d(:,:)
  real(kind=8), intent(IN) :: weight
  real(kind=8), intent(INOUT) :: dt1d(:)
  integer :: i, j, ij

  ij = 0
  do j = 1, size(dt2d,2)
    do i = 1, size(dt2d,1)
      ij = ij+1
      dt1d(ij) = dt1d(ij)+dt2d(i,j)*weight
    end do
  end do

end subroutine add_2d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine add_3d_to_1d(dt3d, weight, dt1d)
  implicit none
  real(kind=8), intent(IN) :: dt3d(:,:,:)
  real(kind=8), intent(IN) :: weight
  real(kind=8), intent(INOUT) :: dt1d(:)
  integer :: i, j, k, ijk

  ijk = 0
  do k = 1, size(dt3d,3)
    do j = 1, size(dt3d,2)
      do i = 1, size(dt3d,1)
        ijk = ijk+1
        dt1d(ijk) = dt1d(ijk)+dt3d(i,j,k)*weight
      end do
    end do
  end do

end subroutine add_3d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_1d_to_1d(dt1d, dtout)
  implicit none
  real(kind=8), intent(IN) :: dt1d(:)
  real(kind=8), intent(INOUT) :: dtout(:)
  integer :: i

  do i = 1, size(dtout)
    dtout(i) = dt1d(i)
  end do

end subroutine get_1d_to_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_1d_to_2d(dt1d, dt2d)
  implicit none
  real(kind=8), intent(IN) :: dt1d(:)
  real(kind=8), intent(INOUT) :: dt2d(:,:)
  integer :: i, j, ij

  ij = 0
  do j = 1, size(dt2d,2)
    do i = 1, size(dt2d,1)
      ij = ij+1
      dt2d(i,j) = dt1d(ij)
    end do
  end do

end subroutine get_1d_to_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_1d_to_3d(dt1d, dt3d)
  implicit none
  real(kind=8), intent(IN) :: dt1d(:)
  real(kind=8), intent(INOUT) :: dt3d(:,:,:)
  integer :: i, j, k, ijk

  ijk = 0
  do k = 1, size(dt3d,3)
    do j = 1, size(dt3d,2)
      do i = 1, size(dt3d,1)
        ijk = ijk+1
        dt3d(i,j,k) = dt1d(ijk)
      end do
    end do
  end do

end subroutine get_1d_to_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_data_double_1d(dt, component_id, data_id, name, db_start, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  type(data_buffer), pointer :: db_start
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight

  type(data_buffer), pointer :: db_current

  db_current => db_start
  do 
    if ((.not.db_current%is_using).or.(data_id==db_current%data_id)) then
      if (associated(db_current%double_d)) then
        if (size(db_current%double_d) /= size(dt,1)) then
           if (associated(db_current%next_ptr, db_start)) exit
           db_current => db_current%next_ptr
           cycle 
        end if
      else
        allocate(db_current%double_d(size(dt,1)))
        db_current%double_d(:) = 0.0
        call put_log("allocate data buffer double 1d : name = "//trim(name)//&
                    ", Size:"//trim(IntToStr(size(dt,1))))
      end if
      if (is_mean) then
        call add_1d_to_1d(dt, weight, db_current%double_d)
      else
        call set_1d_to_1d(dt, db_current%double_d)
      end if
      db_current%component_id = component_id
      db_current%data_id = data_id
      db_current%name = name
      db_current%is_using = .true.
      db_current%data_type = DOUBLE_DATA
      db_current%data_dim  = DATA_1D
      call put_log("reuse data buffer double 1d : name = "//trim(name)//", comp id = "//trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))  
      return
    end if
    if (associated(db_current%next_ptr, db_start)) exit
    db_current => db_current%next_ptr
  end do

  ! new data
  call insert_data_buffer(db_current)
  allocate(db_current%double_d(size(dt,1)))
  if (is_mean) then
    db_current%double_d(:) = 0.d0
    call add_1d_to_1d(dt, weight, db_current%double_d)
  else
    call set_1d_to_1d(dt, db_current%double_d)
  end if
  db_current%component_id = component_id
  db_current%data_id = data_id
  db_current%name = name
  db_current%is_using = .true.
  db_current%data_type = DOUBLE_DATA
  db_current%data_dim  = DATA_1D

  call put_log("allocate new data buffer double 1d : name = "//trim(name)//", comp id = "//trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id))// &
                    ", Size:"//trim(IntToStr(size(dt,1))))

end subroutine put_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_data_double_2d(dt, component_id, data_id, name, db_start, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  type(data_buffer), pointer :: db_start
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight

  type(data_buffer), pointer :: db_current

  db_current => db_start
  do 
    if ((.not.db_current%is_using).or.(data_id==db_current%data_id)) then
      if (associated(db_current%double_d)) then
        if (size(db_current%double_d) /= size(dt,1)*size(dt,2)) then
           if (associated(db_current%next_ptr, db_start)) exit
           db_current => db_current%next_ptr
           cycle 
        end if
      else
        allocate(db_current%double_d(size(dt,1)*size(dt,2)))
        db_current%double_d(:) = 0.0
        call put_log("allocate data buffer double 2d : name = "//trim(name)//&
                    ", Size:"//trim(IntToStr(size(dt,1)))//"x"//trim(IntToStr(size(dt,2))))  
        end if
      if (is_mean) then
        call add_2d_to_1d(dt, weight, db_current%double_d)
      else
        call set_2d_to_1d(dt, db_current%double_d)
      end if
      db_current%component_id = component_id
      db_current%data_id = data_id
      db_current%name = name
      db_current%is_using = .true.
      db_current%data_type = DOUBLE_DATA
      db_current%data_dim  = DATA_2D
      call put_log("reuse data buffer double 2d : name = "//trim(name)//", comp id = "//trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))  
      return
    end if
    if (associated(db_current%next_ptr, db_start)) exit
    db_current => db_current%next_ptr
  end do

  ! new data
  call insert_data_buffer(db_current)
  allocate(db_current%double_d(size(dt,1)*size(dt,2)))
  if (is_mean) then
    db_current%double_d(:) = 0.d0
    call add_2d_to_1d(dt, weight, db_current%double_d)
  else
    call set_2d_to_1d(dt, db_current%double_d)
  end if
  db_current%component_id = component_id
  db_current%data_id = data_id
  db_current%name = name
  db_current%is_using = .true.
  db_current%data_type = DOUBLE_DATA
  db_current%data_dim  = DATA_2D

  call put_log("allocate new data buffer double 2d : name = "//trim(name)//", comp id = "//trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id))// &
                    ", Size:"//trim(IntToStr(size(dt,1)))//"x"//trim(IntToStr(size(dt,2))))  

end subroutine put_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_data_double_3d(dt, component_id, data_id, name, db_start, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  real(kind=8), intent(IN) :: dt(:,:,:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  type(data_buffer), pointer :: db_start
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight

  type(data_buffer), pointer :: db_current

  db_current => db_start
  do 
    if ((.not.db_current%is_using).or.(data_id==db_current%data_id)) then
      if (associated(db_current%double_d)) then
        if (size(db_current%double_d) /= size(dt,1)*size(dt,2)*size(dt,3)) then
           if (associated(db_current%next_ptr, db_start)) exit
           db_current => db_current%next_ptr
           cycle 
        end if
      else
        allocate(db_current%double_d(size(dt,1)*size(dt,2)*size(dt,3)))
        db_current%double_d(:) = 0.0
        call put_log("allocate data buffer double 3d : name = "//trim(name)//&
                    ", Size:"//trim(IntToStr(size(dt,1)))//"x"//trim(IntToStr(size(dt,2)))//"x"//&
                       trim(IntToStr(size(dt,3))))  
      end if
      if (is_mean) then
        call add_3d_to_1d(dt, weight, db_current%double_d)
      else
        call set_3d_to_1d(dt, db_current%double_d)
      end if
      db_current%component_id = component_id
      db_current%data_id = data_id
      db_current%name = name
      db_current%is_using = .true.
      db_current%data_type = DOUBLE_DATA
      db_current%data_dim  = DATA_3D
      call put_log("reuse data buffer double 3d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))  
      return
    end if
    if (associated(db_current%next_ptr, db_start)) exit
    db_current => db_current%next_ptr
  end do

  call insert_data_buffer(db_current)
  allocate(db_current%double_d(size(dt,1)*size(dt,2)*size(dt,3)))
  if (is_mean) then
    db_current%double_d(:) = 0.d0
    call add_3d_to_1d(dt, weight, db_current%double_d)
  else
    call set_3d_to_1d(dt, db_current%double_d)
  end if
  db_current%component_id = component_id
  db_current%data_id = data_id
  db_current%name = name
  db_current%is_using = .true.
  db_current%data_type = DOUBLE_DATA
  db_current%data_dim  = DATA_3D

  call put_log("allocate new data buffer double 3d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))// &
                    ", Size:"//trim(IntToStr(size(dt,1)))//"x"//trim(IntToStr(size(dt,2)))//"x"//&
                       trim(IntToStr(size(dt,3))))  

end subroutine put_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_data_double_1d(dt, component_id, data_id, name, db_start, status)
  use jcup_constant, only : NAME_LEN
  use jcup_utils, only : put_log, error, IntToStr
  implicit none
  real(kind=8), intent(INOUT) :: dt(:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  type(data_buffer), pointer :: db_start
  integer, intent(OUT) :: status

  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if ((db_current%is_using).and.(db_current%data_id == data_id)) then
      if (db_current%data_dim /= DATA_1D) then
         call error("get data double 1d","!!!! data dimension miss match, name = "//trim(name))
      end if

      call get_1d_to_1d(db_current%double_d, dt)

      status = 0 ; 
      call put_log("get data double 1d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
      return
    end if
    if (associated(db_current%next_ptr, db_start)) then
      status = 1
      call put_log("get data double 1d !!!! No such data : name = "//trim(name))
      call error("get data double 1d"," !!!! No such data : name = "//trim(name))
      return
    end if
    db_current => db_current%next_ptr
  end do
  
  status = 1

  call put_log("get data double 1d !!!! No such data : name = "//trim(name))
  call error("get data double 1d"," !!!! No such data : name = "//trim(name))

end subroutine get_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_data_double_2d(dt, component_id, data_id, name, db_start, status)
  use jcup_constant, only : NAME_LEN
  use jcup_utils, only : put_log, error, IntToStr
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  type(data_buffer), pointer :: db_start
  integer, intent(OUT) :: status

  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if ((db_current%is_using).and.(db_current%data_id == data_id)) then
      if (db_current%data_dim /= DATA_2D) then
         call error("get data double 2d","!!!! data dimension miss match, name = "//trim(name))
      end if
      call get_1d_to_2d(db_current%double_d, dt)
      status = 0 ; 
      call put_log("get data double 2d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
      return
    end if
    if (associated(db_current%next_ptr, db_start)) then
      status = 1
      call put_log("get data double 2d !!!! No such data : name = "//trim(name))
      call error("get data double 2d"," !!!! No such data : name = "//trim(name))
      return
    end if
    db_current => db_current%next_ptr
  end do
  
  status = 1

  call put_log("get data double 2d !!!! No such data : name = "//trim(name))
  call error("get data double 2d"," !!!! No such data : name = "//trim(name))

end subroutine get_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_data_double_3d(dt, component_id, data_id, name, db_start, status)
  use jcup_constant, only : NAME_LEN
  use jcup_utils, only : put_log, error, IntToStr
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:,:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  type(data_buffer), pointer :: db_start
  integer, intent(OUT) :: status

  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if ((db_current%is_using).and.(db_current%data_id == data_id)) then
      if (db_current%data_dim /= DATA_3D) then
         call error("get data double 3d","!!!! data dimension miss match, name = "//trim(name))
      end if
      call get_1d_to_3d(db_current%double_d, dt)

      status = 0 ; 
      call put_log("get data double 3d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
      return
    end if
    if (associated(db_current%next_ptr, db_start)) then
      status = 1
      call put_log("get data double 3d !!!! No such data : name = "//trim(name))
      call error("get data double 3d"," !!!! No such data : name = "//trim(name))
      return
    end if
    db_current => db_current%next_ptr
  end do
  
  status = 1

  call put_log("get data double 3d !!!! No such data : name = "//trim(name))
  call error("get data double 3d"," !!!! No such data : name = "//trim(name))

end subroutine get_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

function search_data_buffer(db_start, data_id) result(db_current)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  type(data_buffer), pointer :: db_start
  integer, intent(IN) :: data_id

  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if (db_current%data_id == data_id) then
      return
    end if
    if (associated(db_current%next_ptr, db_start)) return
    db_current => db_current%next_ptr
  end do
  
  call put_log("reset data buffer !!!! No such data : data_id = "//trim(IntToStr(data_id)))

end function search_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine reset_data_buffer(db_start, data_id, status)
  use jcup_utils, only : put_log, error, IntToStr
  implicit none
  type(data_buffer), pointer :: db_start
  integer, intent(IN) :: data_id
  integer, intent(OUT) :: status

  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if (db_current%data_id == data_id) then
      db_current%double_d(:) = 0.d0
      db_current%data_id = 0
      db_current%component_id = 0
      db_current%is_using = .false.
      db_current%name = repeat(" ",NAME_LEN)
      status = 0 ; 
      return
    end if
    if (associated(db_current%next_ptr, db_start)) return
    db_current => db_current%next_ptr
  end do
  
  call put_log("reset data buffer !!!! No such data : data_id = "//trim(IntToStr(data_id)))
  call error("reset data buffer","!!!! No such data : data_id = "//trim(IntToStr(data_id)))

  status = 1

end subroutine reset_data_buffer


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine reset_all_data_buffer(db_start)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  type(data_buffer), pointer :: db_start
  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if (db_current%is_using) then
      call put_log("reset data buffer : name = "//trim(db_current%name))
    end if
    db_current%double_d(:) = 0.d0
    db_current%data_id = 0
    db_current%component_id = 0
    db_current%is_using = .false.
    db_current%name = repeat(" ",NAME_LEN)
    if (associated(db_current%next_ptr, db_start)) return
    db_current => db_current%next_ptr
  end do
  
  call put_log("reset all data buffer !!!! reset error")

end subroutine reset_all_data_buffer


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine reset_comp_data_buffer(db_start, component_id)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  type(data_buffer), pointer :: db_start
  integer, intent(IN) :: component_id

  type(data_buffer), pointer :: db_current

  db_current => db_start

  do 
    if (db_current%component_id==component_id) then
      if (db_current%is_using) then
        call put_log("reset data buffer : name = "//trim(db_current%name))
      end if
      db_current%double_d(:) = 0.d0
      db_current%data_id = 0
      db_current%component_id = 0
      db_current%is_using = .false.
      db_current%name = repeat(" ",NAME_LEN)
    end if
    if (associated(db_current%next_ptr, db_start)) return
    db_current => db_current%next_ptr
  end do
  
  call put_log("reset comp data buffer !!!! reset error")

end subroutine reset_comp_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_using_data_buffer(db_start)
  use jcup_utils, only : put_log
  implicit none
  type(data_buffer), pointer :: db_start
  type(data_buffer), pointer :: db_current

  integer :: counter

  counter = 0

  db_current => db_start
  do
    if (db_current%is_using) counter = counter+1
    if (associated(db_current%next_ptr, db_start)) then
      get_num_of_using_data_buffer = counter
      return
    end if
    db_current => db_current%next_ptr
  end do

  call put_log("get_using_data_buffer !!!! count error")

end function get_num_of_using_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_name(db)
  implicit none
  type(data_buffer), pointer :: db

  get_name = db%name

end function get_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_data_type(db)
  implicit none
  type(data_buffer), pointer :: db

  get_data_type = db%data_type

end function get_data_type

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_using(db)
  implicit none
  type(data_buffer), pointer :: db

  is_using = db%is_using

end function is_using

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_next_ptr(db) result(next_ptr)
  implicit none
  type(data_buffer), pointer :: db
  type(data_buffer), pointer :: next_ptr

  next_ptr => db%next_ptr
 
end function get_next_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_data_buffer_info(db)
  use jcup_constant, only : STRING_LEN
  use jcup_utils, only : put_log
  implicit none
  type(data_buffer), pointer :: db
  character(len=STRING_LEN) :: log_str

  write(log_str,'("  Data buffer info : ",A,L2,I8)') trim(db%name)//", ", db%is_using, size(db%double_d,1)

  call put_log(trim(log_str),2)

end subroutine write_data_buffer_info

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/14 [MOD] data_time(6) -> data_time(8)
! 2014/11/04 [MOD] integer :: data_time -> integer(kind=8) :: data_time
subroutine write_data_buffer(data_time, db, fid, comp_id, write_flag, data_name, data_ptr)
  use jcup_mpi_lib, only : jml_isLocalLeader
  implicit none
  integer(kind=8), intent(IN) :: data_time(8)
  type(data_buffer), pointer :: db
  integer, intent(IN) :: fid
  integer, intent(IN) :: comp_id
  logical, intent(OUT) :: write_flag
  character(len=*), intent(OUT) :: data_name
  real(kind=8), pointer :: data_ptr(:)

  write_flag = .false.

  if (.not.db%is_using) return

  if (db%component_id /= comp_id) return

  ! write data info to mastar file
  if (jml_isLocalLeader(comp_id)) then
    write(fid, *) db%name
    write(fid, *) data_time
    write(fid, *) db%component_id
    write(fid, *) db%data_id
    write(fid, *) db%data_type
    write(fid, *) db%data_dim 
  end if

  write_flag = .true.
  data_name = db%name
  data_ptr => db%double_d

  !!write(0,*) " write_data_buffer ", db%component_id, db%data_id, size(db%double_d), db%name, db%data_type, db%data_dim


end subroutine write_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine restore_data_buffer(dt, component_id, data_id, name, data_type, data_dim, db_start)
  use jcup_utils, only : put_log, IntToStr
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer, intent(IN) :: data_type
  integer, intent(IN) :: data_dim
  type(data_buffer), pointer :: db_start


  type(data_buffer), pointer :: db_current

  db_current => db_start
  do 
    if ((.not.db_current%is_using).or.(data_id==db_current%data_id)) then
      if (associated(db_current%double_d)) then
        if (size(db_current%double_d) /= size(dt,1)) then
           if (associated(db_current%next_ptr, db_start)) exit
           db_current => db_current%next_ptr
           cycle 
        end if
      else
        allocate(db_current%double_d(size(dt,1)))
        db_current%double_d(:) = 0.0
        call put_log("allocate data buffer double 1d : name = "//trim(name)//&
                    ", Size:"//trim(IntToStr(size(dt,1))))
        end if
      call set_1d_to_1d(dt, db_current%double_d)
      db_current%component_id = component_id
      db_current%data_id = data_id
      db_current%name = name
      db_current%is_using = .true.
      db_current%data_type = data_type
      db_current%data_dim  = data_dim
      call put_log("reuse data buffer double 1d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))  
      return
    end if
    if (associated(db_current%next_ptr, db_start)) exit
    db_current => db_current%next_ptr
  end do

  ! new data
  call insert_data_buffer(db_current)
  allocate(db_current%double_d(size(dt,1)))
  call set_1d_to_1d(dt, db_current%double_d)
  db_current%component_id = component_id
  db_current%data_id = data_id
  db_current%name = name
  db_current%is_using = .true.
  db_current%data_type = data_type
  db_current%data_dim  = data_dim

  call put_log("allocate new data buffer double 1d : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))// &
                    ", Size:"//trim(IntToStr(size(dt,1))))

end subroutine restore_data_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_data_buffer


!=======+=========+=========+=========+=========+=========+=========+=========+

!---------------------      module jcup_time_buffer     ----------------------!

!=======+=========+=========+=========+=========+=========+=========+=========+

module jcup_time_buffer
  use jcup_data_buffer, only : data_buffer, NO_DATA
  use jcup_time, only : time_type

!--------------------------------   public  ----------------------------------!
 
  public :: time_buffer
  public :: init_time_buffer
  public :: destruct_time_buffer
  public :: search_time_buffer
  public :: insert_time_buffer
  public :: delete_time_buffer
  public :: reset_time_buffer
  public :: reset_past_time_buffer
  public :: get_start_data_ptr
  public :: get_current_data_ptr
  public :: get_before_ptr
  public :: get_next_ptr
  public :: get_time
  public :: is_using
  public :: inc_num_of_data
  public :: dec_num_of_data
  public :: get_num_of_data
  public :: write_time_buffer_info

  type time_buffer
    type(data_buffer), pointer :: dt_start, dt_current
    type(time_buffer), pointer :: before_ptr, next_ptr    
    integer :: num_of_data
    type(time_type) :: time
    logical :: is_using
  end type

!--------------------------------   private  ---------------------------------!
  
contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_time_buffer(tb, time)
  use jcup_data_buffer, only : init_data_buffer
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type), intent(IN) :: time

  allocate(tb)
  tb%before_ptr => tb
  tb%next_ptr => tb
  tb%num_of_data = 0
  tb%is_using = .false.
  tb%time = time
  call init_data_buffer(tb%dt_start)
  tb%dt_current => tb%dt_start

end subroutine init_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_time_buffer(tb)
  use jcup_data_buffer, only : destruct_data_buffer
  implicit none
  type(time_buffer), pointer :: tb
  type(time_buffer), pointer :: tmp_ptr
  type(time_buffer), pointer :: start_ptr

  start_ptr => tb
  do 
    tmp_ptr => tb%next_ptr
    if (associated(tmp_ptr, start_ptr)) exit
    call destruct_data_buffer(tb%dt_start)
    nullify(tb%before_ptr)
    nullify(tb%next_ptr)
    deallocate(tb)
    tb => tmp_ptr
  end do

end subroutine destruct_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine insert_time_buffer(tb, time)
  use jcup_time, only : DateToTimeStr
  use jcup_utils, only : put_log
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type) :: time

  type(time_buffer), pointer :: new_data
  character(len=20) :: time_str

  call init_time_buffer(new_data, time)
  new_data%before_ptr => tb
  new_data%next_ptr => tb%next_ptr
  if (associated(tb%next_ptr)) tb%next_ptr%before_ptr => new_data
  tb%next_ptr => new_data 
  tb => new_data
  tb%is_using = .true.
  call DateToTimeStr(time_str, time)
  call put_log("insert time buffer : "//trim(time_str))

end subroutine insert_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine delete_time_buffer(tb, time)
  use jcup_utils, only : put_log
  use jcup_time, only : DateToTimeStr
  use jcup_data_buffer, only : destruct_data_buffer
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type), intent(IN) :: time

  type(time_buffer), pointer :: tmp_ptr

  character(len=20) :: time_str

  call search_time_buffer(tb, time, .false.)
  tb%before_ptr%next_ptr => tb%next_ptr
  tb%next_ptr%before_ptr => tb%before_ptr
  call destruct_data_buffer(tb%dt_start)

  tmp_ptr => tb%before_ptr

  deallocate(tb)

  tb => tmp_ptr

  call DateToTimeStr(time_str, time)
  call put_log("delete time buffer : "//trim(time_str))

end subroutine delete_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine reset_time_buffer(tb, time, component_id)
  use jcup_utils, only : put_log
  use jcup_time, only : DateToTimeStr
  use jcup_data_buffer, only : reset_comp_data_buffer, get_num_of_using_data_buffer
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id

  character(len=20) :: time_str

  call search_time_buffer(tb, time, .false.)
  if (tb%num_of_data > 0) then
    call reset_comp_data_buffer(tb%dt_start, component_id)
    tb%num_of_data = get_num_of_using_data_buffer(tb%dt_start)
  end if
  if (tb%num_of_data == 0) tb%is_using = .false.
  call DateToTimeStr(time_str, time)
  call put_log("reset time buffer : "//trim(time_str))

end subroutine reset_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine reset_past_time_buffer(tb, current_time, component_id)
  use jcup_utils, only : put_log
  use jcup_time, only : operator(<), DateToTimeStr
  use jcup_data_buffer, only : reset_comp_data_buffer, get_num_of_using_data_buffer
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: component_id

  type(time_buffer), pointer :: start_time
  character(len=20) :: time_str

  start_time => tb
  do 
    if (tb%time < current_time)  then
      call DateToTimeStr(time_str, tb%time)
      call put_log("search and reset time buffer : "//trim(time_str))
      if (tb%num_of_data > 0) then
        call reset_comp_data_buffer(tb%dt_start,component_id)
        tb%num_of_data = get_num_of_using_data_buffer(tb%dt_start)
      end if
      if (tb%num_of_data == 0) tb%is_using = .false.
    end if
    tb => tb%next_ptr
    if (associated(tb, start_time)) exit
  end do
  
end subroutine reset_past_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine search_time_buffer(tb, time, insert_flag)
  use jcup_utils, only : put_log
  use jcup_time, only : operator(==), DateToTimeStr
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type), intent(IN) :: time
  logical, optional, intent(IN) :: insert_flag 

  type(time_buffer), pointer :: start_time
  character(len=20) :: time_str
  character(len=20) :: old_time_str
  
  logical :: is_insert

  if (present(insert_flag)) then
    is_insert = insert_flag
  else
    is_insert = .true.
  end if

  call DateToTimeStr(time_str, time)

  start_time => tb
  do 
    if (tb%time == time)  then
      tb%is_using = .true.
      call put_log("search time buffer : same time found : "//trim(time_str))
      return
    end if
    tb => tb%next_ptr
    if (associated(tb, start_time)) exit
  end do

  tb => start_time
  do 
    if (.not.tb%is_using) then
      call DateToTimeStr(old_time_str, tb%time)
      tb%time = time
      tb%is_using = .true.
      call put_log("search time buffer : usable time found : "//trim(old_time_str)//" : "//trim(time_str))
      return
    end if
    tb => tb%next_ptr
    if (associated(tb, start_time)) exit
  end do
    
  if (is_insert) then
    tb => start_time%before_ptr
    call insert_time_buffer(tb, time)
  else
    call put_log("search time buffer !!!! No such time "//trim(time_str))
  end if

end subroutine search_time_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_start_data_ptr(tb) result(data_ptr)
  implicit none
  type(time_buffer), pointer :: tb
  type(data_buffer), pointer :: data_ptr

  data_ptr => tb%dt_start

end function get_start_data_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_current_data_ptr(tb) result(data_ptr)
  implicit none
  type(time_buffer), pointer :: tb
  type(data_buffer), pointer :: data_ptr

  data_ptr => tb%dt_current

end function get_current_data_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_before_ptr(tb) result(time_ptr)
  implicit none
  type(time_buffer), pointer :: tb

  type(time_buffer), pointer :: time_ptr

  time_ptr => tb%before_ptr

end function get_before_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_next_ptr(tb) result(time_ptr)
  implicit none
  type(time_buffer), pointer :: tb

  type(time_buffer), pointer :: time_ptr

  time_ptr => tb%next_ptr

end function get_next_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_time(tb) result(time)
  implicit none
  type(time_buffer), pointer :: tb
  type(time_type) :: time

  time = tb%time

end function get_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine inc_num_of_data(tb)
  implicit none
  type(time_buffer), pointer :: tb

  tb%num_of_data = tb%num_of_data+1

end subroutine inc_num_of_data

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine dec_num_of_data(tb)
  implicit none
  type(time_buffer), pointer :: tb

  tb%num_of_data = tb%num_of_data-1

end subroutine dec_num_of_data

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_data(tb)
  implicit none
  type(time_buffer), pointer :: tb

  get_num_of_data = tb%num_of_data

end function get_num_of_data

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_using(tb)
  implicit none
  type(time_buffer), pointer :: tb

  is_using = tb%is_using

end function is_using

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine write_time_buffer_info(tb)
  use jcup_constant, only : STRING_LEN
  use jcup_time, only : DateToTimeStr
  use jcup_utils, only : put_log
  use jcup_data_buffer, only : write_data_buffer_info, get_next_ptr, get_num_of_using_data_buffer
  implicit none
  type(time_buffer), pointer :: tb
  character(len=20) :: time_str
  character(len=STRING_LEN) :: log_str

  call DateToTimeStr(time_str, tb%time)

  tb%num_of_data = get_num_of_using_data_buffer(tb%dt_start)

  write(log_str,'(" Time buffer info : ",A,L,I5)') trim(time_str)//" ", tb%is_using, tb%num_of_data
  call put_log(trim(log_str))

  !!!write(0,*) "time buffer ",tb%is_using, time_str 
  tb%dt_current => tb%dt_start
  do 
    if (associated(tb%dt_current)) call write_data_buffer_info(tb%dt_current)
    if (associated(get_next_ptr(tb%dt_current), tb%dt_start)) exit
    tb%dt_current => get_next_ptr(tb%dt_current)
  end do  

end subroutine write_time_buffer_info

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_time_buffer


!=======+=========+=========+=========+=========+=========+=========+=========+

!---------------------        module jcup_buffer        ----------------------!

!=======+=========+=========+=========+=========+=========+=========+=========+

module jcup_buffer
  use jcup_time_buffer, only : time_buffer
  use jcup_time, only : time_type

!--------------------------------   public  ----------------------------------!

  public :: init_buffer
  public :: destruct_buffer
  public :: get_send_data_type
  public :: put_send_data
  public :: get_send_data
  public :: remove_send_data
  public :: remove_send_time
  public :: remove_past_send_data
  public :: remove_past_recv_data
  public :: get_recv_data_type
  public :: put_recv_data
  public :: get_recv_data
  public :: remove_recv_data
  public :: remove_recv_time
  public :: buffer_check_write
  public :: get_num_of_time ! integer function (time_buffer_ptr) 2013.06.07 [ADD]
  public :: get_send_buffer_ptr ! function () result (send_buffer_ptr) 2013.06.07 [ADD]
  public :: write_buffer ! subroutine (file_id) 2013.05.29 [ADD]
  public :: read_buffer  ! subroutine (file_id) 2013.05.29 [ADD]
  public :: restore_buffer ! subroutine (dt, time, component_id, data_id, name, data_type, data_dim) 2013.06.13 [ADD]

!--------------------------------   private  ---------------------------------!

  private

  interface put_send_data
    module procedure put_send_data_double_1d
    module procedure put_send_data_double_2d
    module procedure put_send_data_double_3d
  end interface

  interface get_send_data
    module procedure get_send_data_double_1d
    module procedure get_send_data_double_2d
    module procedure get_send_data_double_3d
  end interface

  interface put_recv_data
    module procedure put_recv_data_double_1d
    module procedure put_recv_data_double_2d
    module procedure put_recv_data_double_3d
  end interface

  interface get_recv_data
    module procedure get_recv_data_double_1d
    module procedure get_recv_data_double_2d
    module procedure get_recv_data_double_3d
  end interface

  type(time_buffer), pointer :: send_buffer
  type(time_buffer), pointer :: recv_buffer

  private :: put_send_data_double_2d
  private :: put_send_data_double_3d
  private :: get_send_data_double_2d
  private :: get_send_data_double_3d
  private :: put_recv_data_double_2d
  private :: put_recv_data_double_3d
  private :: get_recv_data_double_2d
  private :: get_recv_data_double_3d
  
contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_buffer()
  use jcup_time_buffer, only : init_time_buffer
  implicit none

  type(time_type) :: time
  time%yyyy = 1000 ; time%mo = 1 ; time%dd = 1 ; time%hh = 1  ; time%mm = 0 ; time%ss = 0
  time%delta_t = 0.d0
  call init_time_buffer(send_buffer, time)
  call init_time_buffer(recv_buffer, time)

end subroutine init_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_buffer()
  use jcup_time_buffer, only : destruct_time_buffer
  implicit none

  call destruct_time_buffer(send_buffer)
  call destruct_time_buffer(recv_buffer)
  
end subroutine destruct_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_data_type(time, data_id)
  use jcup_data_buffer, only : get_data_type, search_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: data_id

  call search_time_buffer(send_buffer, time)
  get_send_data_type = get_data_type(search_data_buffer(get_start_data_ptr(send_buffer),data_id))

end function get_send_data_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_send_data_double_1d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : put_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("put_send_data_double_1d : put data : name = "//trim(name)//", comp id = "//trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))
  call search_time_buffer(send_buffer, time)
  call put_data(dt, component_id, data_id, name, get_start_data_ptr(send_buffer), is_mean, weight)
  call inc_num_of_data(send_buffer)

end subroutine put_send_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_send_data_double_2d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : put_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)

  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call put_log("put_send_data_double_2d : put data : name = "//trim(name)//", comp id = "//trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))
  call search_time_buffer(send_buffer, time)
  call put_data(dt, component_id, data_id, name, get_start_data_ptr(send_buffer), is_mean, weight)
  call inc_num_of_data(send_buffer)

end subroutine put_send_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_send_data_double_3d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : put_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)

  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call put_log("put_send_data_double_3d : put data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
  call search_time_buffer(send_buffer, time)
  call put_data(dt, component_id, data_id, name, get_start_data_ptr(send_buffer), is_mean, weight)
  call inc_num_of_data(send_buffer)

end subroutine put_send_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_data_double_1d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_data_buffer, only : get_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none
  real(kind=8), intent(INOUT) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer :: status
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
    & "get_send_data_double_1d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))//", time = "//trim(time_str))
  call search_time_buffer(send_buffer, time)
  call get_data(dt, component_id, data_id, name, get_start_data_ptr(send_buffer), status)

end subroutine get_send_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_data_double_2d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : get_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer :: status

  call put_log("get_send_data_double_2d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
  call search_time_buffer(send_buffer, time)
  call get_data(dt, component_id, data_id, name, get_start_data_ptr(send_buffer), status)

end subroutine get_send_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_data_double_3d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : get_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer :: status


  call put_log("get_send_data_double_3d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
  call search_time_buffer(send_buffer, time)
  call get_data(dt, component_id, data_id, name, get_start_data_ptr(send_buffer), status)

end subroutine get_send_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_send_data(time, data_id)
  use jcup_data_buffer, only : reset_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none 
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: data_id
  integer :: status

  call search_time_buffer(send_buffer, time, .false.)
  call reset_data_buffer(get_start_data_ptr(send_buffer), data_id, status)

end subroutine remove_send_data 
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_send_time(time, component_id)
  use jcup_time_buffer, only : reset_time_buffer
  implicit none
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id
 
  call reset_time_buffer(send_buffer, time, component_id)

end subroutine remove_send_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_past_send_data(current_time, component_id)
  use jcup_utils, only : put_log
  use jcup_time_buffer, only : reset_past_time_buffer
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: component_id

  call put_log("remove past send data")
  call reset_past_time_buffer(send_buffer, current_time, component_id)

end subroutine remove_past_send_data

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_past_recv_data(current_time, component_id)
  use jcup_utils, only : put_log
  use jcup_time_buffer, only : reset_past_time_buffer
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: component_id

  call put_log("remove past recv data")
  call reset_past_time_buffer(recv_buffer, current_time, component_id)

end subroutine remove_past_recv_data

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_recv_data_type(time, data_id)
  use jcup_data_buffer, only : get_data_type, search_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: data_id

  call search_time_buffer(recv_buffer, time)
  get_recv_data_type = get_data_type(search_data_buffer(get_start_data_ptr(recv_buffer),data_id))

end function get_recv_data_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_recv_data_double_1d(dt, time, component_id, data_id, name)
  use jcup_data_buffer, only : put_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call search_time_buffer(recv_buffer, time)
  call put_data(dt, component_id, data_id, name, get_start_data_ptr(recv_buffer), .false., 1.d0)
  call inc_num_of_data(recv_buffer)

end subroutine put_recv_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_recv_data_double_2d(dt, time, component_id, data_id, name)
  use jcup_data_buffer, only : put_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call search_time_buffer(recv_buffer, time)
  call put_data(dt, component_id, data_id, name, get_start_data_ptr(recv_buffer), .false., 1.d0)
  call inc_num_of_data(recv_buffer)

end subroutine put_recv_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_recv_data_double_3d(dt, time, component_id, data_id, name)
  use jcup_data_buffer, only : put_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call search_time_buffer(recv_buffer, time)
  call put_data(dt, component_id, data_id, name, get_start_data_ptr(recv_buffer), .false., 1.d0)
  call inc_num_of_data(recv_buffer)

end subroutine put_recv_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_data_double_1d(dt, time, component_id, data_id, name, is_reset_data)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : get_data, reset_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, dec_num_of_data, &
                               get_num_of_data, reset_time_buffer
  implicit none
  real(kind=8), intent(INOUT) :: dt(:)
  integer, intent(IN) :: component_id, data_id
  type(time_type), intent(IN) :: time  
  character(len=*), intent(IN) :: name
  logical, optional, intent(IN) :: is_reset_data

  logical :: is_reset
  integer :: status

  if (present(is_reset_data)) then
    is_reset = is_reset_data
  else
    is_reset = .true.
  end if

  call put_log("Get data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call search_time_buffer(recv_buffer, time)
  call get_data(dt, component_id, data_id, name, get_start_data_ptr(recv_buffer), status)
  call dec_num_of_data(recv_buffer)

  if (is_reset) then
    call reset_data_buffer(get_start_data_ptr(recv_buffer), data_id, status)
    if (get_num_of_data(recv_buffer) == 0) then
      call reset_time_buffer(recv_buffer, time, component_id)
    end if
  end if

end subroutine get_recv_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_data_double_2d(dt, time, component_id, data_id, name, is_reset_data)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : get_data, reset_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, dec_num_of_data, &
                               get_num_of_data, reset_time_buffer
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:)
  integer, intent(IN) :: component_id, data_id
  type(time_type), intent(IN) :: time  
  character(len=*), intent(IN) :: name
  logical, optional, intent(IN) :: is_reset_data

  logical :: is_reset
  integer :: status

  if (present(is_reset_data)) then
    is_reset = is_reset_data
  else
    is_reset = .true.
  end if

  call put_log("Get data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call search_time_buffer(recv_buffer, time)

  call get_data(dt, component_id, data_id, name, get_start_data_ptr(recv_buffer), status)
  call dec_num_of_data(recv_buffer)

  if (is_reset) then
    call reset_data_buffer(get_start_data_ptr(recv_buffer), data_id, status)
    if (get_num_of_data(recv_buffer) == 0) then
      call reset_time_buffer(recv_buffer, time, component_id)
    end if
  end if

end subroutine get_recv_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_data_double_3d(dt, time, component_id, data_id, name, is_reset_data)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : get_data, reset_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, dec_num_of_data, &
                               get_num_of_data, reset_time_buffer
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:,:)
  integer, intent(IN) :: component_id, data_id
  type(time_type), intent(IN) :: time  
  character(len=*), intent(IN) :: name
  logical, optional, intent(IN) :: is_reset_data

  logical :: is_reset
  integer :: status

  if (present(is_reset_data)) then
    is_reset = is_reset_data
  else
    is_reset = .true.
  end if

  call put_log("Get data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)

  call search_time_buffer(recv_buffer, time)
  call get_data(dt, component_id, data_id, name, get_start_data_ptr(recv_buffer), status)
  call dec_num_of_data(recv_buffer)

  if (is_reset) then
    call reset_data_buffer(get_start_data_ptr(recv_buffer), data_id, status)
    if (get_num_of_data(recv_buffer) == 0) then
      call reset_time_buffer(recv_buffer, time, component_id)
    end if
  end if

end subroutine get_recv_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_recv_data(time, component_id, data_id, name)
  use jcup_data_buffer, only : reset_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none 
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer :: status

  call search_time_buffer(recv_buffer, time, .false.)
  call reset_data_buffer(get_start_data_ptr(recv_buffer), data_id, status)

end subroutine remove_recv_data 
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_recv_time(time, component_id)
  use jcup_time_buffer, only : reset_time_buffer
  implicit none
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id

  call reset_time_buffer(recv_buffer, time, component_id)

end subroutine remove_recv_time

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_time(buffer_ptr)
  use jcup_time_buffer, only : get_next_ptr
  implicit none
  type(time_buffer), pointer :: buffer_ptr
  integer :: num_of_time
  type(time_buffer), pointer :: start_ptr
  type(time_buffer), pointer :: current_ptr

  start_ptr => buffer_ptr
  current_ptr => buffer_ptr

  num_of_time = 0
  do 
    if (.not.associated(current_ptr)) exit
    num_of_time = num_of_time + 1
    current_ptr => get_next_ptr(current_ptr)
    if (associated(current_ptr, start_ptr)) exit
  end do

  get_num_of_time = num_of_time
  
end function get_num_of_time

!=======+=========+=========+=========+=========+=========+=========+=========+


subroutine buffer_check_write()
  use jcup_utils, only : put_log
  use jcup_time_buffer, only : get_next_ptr, write_time_buffer_info
  implicit none

  type(time_buffer), pointer :: send_start

  call put_log("Check send buffer")
  send_start => send_buffer

  do 
    if (.not.associated(send_buffer)) exit
    call write_time_buffer_info(send_buffer)
    send_buffer => get_next_ptr(send_buffer)
    if (associated(send_buffer, send_start)) exit
  end do

  call put_log("Check recv buffer")
  send_start => recv_buffer

  do 
    if (.not.associated(recv_buffer)) exit
    call write_time_buffer_info(recv_buffer)
    recv_buffer => get_next_ptr(recv_buffer)
    if (associated(recv_buffer, send_start)) exit
  end do
  
end subroutine buffer_check_write

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_send_buffer_ptr() result (send_buffer_ptr)
  implicit none
  type(time_buffer), pointer :: send_buffer_ptr

  send_buffer_ptr => send_buffer

end function get_send_buffer_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine restore_buffer(dt, time, component_id, data_id, name, data_type, data_dim)
  use jcup_utils, only : put_log, IntToStr
  use jcup_data_buffer, only : restore_data_buffer
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr, inc_num_of_data
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer, intent(IN) :: data_type
  integer, intent(IN) :: data_dim

  call put_log("restore send data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
  call search_time_buffer(send_buffer, time)
  call restore_data_buffer(dt, component_id, data_id, name, data_type, data_dim, get_start_data_ptr(send_buffer))
  call inc_num_of_data(send_buffer)

end subroutine restore_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

end module

!program test
!  use jcup_data_container
!  implicit none

  
!  call init_data_buffer()
!  call set_step(1)
!  call set_step(2)

!end program
