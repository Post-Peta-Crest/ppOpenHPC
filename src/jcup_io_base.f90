module jcup_io_base
  use jcup_constant, only : NAME_LEN
  include "mpif.h"
  private

  public :: jcup_init_io ! subroutine ()
  public :: jcup_io_create_type ! subroutine (comp_id, grid_id, grid_index)
  public :: jcup_write_restart_base ! subroutine (file_id, component_id, end_time)
  public :: jcup_write_restart_gmean ! subroutine (file_id, component_id, end_time)
  public :: jcup_read_restart_base ! subroutine (file_id, component_id, end_time)
  public :: jcup_read_restart_gmean ! subroutine (file_id, component_id, end_time)

! --------------- AGCM variables ---------------

  type ft_type ! file type type
    integer :: comp_id = 0
    integer :: grid_id = 0
    integer :: num_of_vgrid = 1! the number of vertical grid or the number of data ! 2014/07/16 [ADD]
    integer :: file_type
    type(ft_type), pointer :: next_ptr
  end type

  type (ft_type), pointer :: ft_ptr


contains

!*=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_init_io()
  implicit none

  nullify(ft_ptr)

end subroutine jcup_init_io

!*=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/16 [MOD] 
subroutine jcup_io_create_type(comp_id, grid_id, num_of_vgrid, grid_index)
  implicit none
  integer, intent(IN) :: comp_id
  integer, intent(IN) :: grid_id
  integer, intent(IN) :: num_of_vgrid
  integer, intent(IN) :: grid_index(:)

  type(ft_type), pointer :: current_ft

  if (.not.associated(ft_ptr)) then
    allocate(ft_ptr)
    ft_ptr%comp_id = comp_id
    ft_ptr%grid_id = grid_id
    ft_ptr%num_of_vgrid = num_of_vgrid
    call jcup_io_create_file_type(ft_ptr, grid_index)
    nullify(ft_ptr%next_ptr)
    current_ft => ft_ptr
  else
    current_ft => ft_ptr
    do 
      if ((current_ft%comp_id == comp_id).and.(current_ft%grid_id == grid_id).and.(current_ft%num_of_vgrid == num_of_vgrid)) then
        exit
      end if
      if (.not.associated(current_ft%next_ptr)) then
        allocate(current_ft%next_ptr)
        current_ft%next_ptr%comp_id = comp_id
        current_ft%next_ptr%grid_id = grid_id
        current_ft%next_ptr%num_of_vgrid = num_of_vgrid
        current_ft => current_ft%next_ptr
        call jcup_io_create_file_type(current_ft, grid_index)
        nullify(current_ft%next_ptr)
        exit
      end if
      current_ft => current_ft%next_ptr
    end do
  end if

end subroutine jcup_io_create_type

!*=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/16 [MOD]
subroutine jcup_io_create_file_type(ftp, grid_index)
  use jcup_utils, only : error, put_log
  use jcup_mpi_lib, only : jml_AllReduceSumLocal
  implicit none
  type(ft_type), pointer :: ftp
  integer, intent(IN) :: grid_index(:)
  integer, allocatable :: index_buffer(:)
  integer :: num_of_hgrid ! the number of horizontal grid 
  integer :: ierror = 0
  integer :: offset
  integer :: i, j, counter

  if (ftp%num_of_vgrid > 1) then
    call jml_AllReduceSumLocal(ftp%comp_id, size(grid_index), num_of_hgrid)
    allocate(index_buffer(size(grid_index)*ftp%num_of_vgrid))
    counter = 0
    do j = 1, ftp%num_of_vgrid
      do i = 1, size(grid_index)
        counter = counter + 1
        index_buffer(counter) =  grid_index(i) + (j-1)*num_of_hgrid -1
      end do
    end do  
  else
    allocate(index_buffer(size(grid_index)))
    index_buffer(:) = grid_index(:) - 1
  end if

  offset = 1
  call MPI_type_create_indexed_block(size(index_buffer), offset, index_buffer, MPI_DOUBLE_PRECISION, ftp%file_type, ierror)

  if (ierror>0) then
     call error("jcup_io_create_file_type", "MPI_TYPE_create_indexed_block ")
     stop
  end if

  call MPI_Type_commit(ftp%file_type, ierror)

  if (ierror>0) then
     call error("jcup_io_create_file_type", "MPI_TYPE_commit error ")
     stop
  end if

  deallocate(index_buffer)
    
  !write(LOG_FILE_ID, '(" Msg : Sub[jcup_io_create_file_type]/Mod[jcup_io_base]")')
  !write(LOG_FILE_ID, '(" ----- File type : 2D")') 
  !write(LOG_FILE_ID, '(" ------- array size : x =",I7," y =",I7)') size_2d(1), size_2d(2)
  !write(LOG_FILE_ID, '(" ------- sub size   : x =",I7," y =",I7)') subsize_2d(1), subsize_2d(2)
  !write(LOG_FILE_ID, '(" ------- starts     : x =",I7," y =",I7)') starts_2d(1), starts_2d(2)
  !write(LOG_FILE_ID, '(" ------- filetype   :    ",I12,"    ",I3)') ftp%file_type, ierror
   
end subroutine jcup_io_create_file_type

!*=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/16 [MOD]
subroutine  search_file_type(comp_id, grid_id, num_of_vgrid, ftp)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id
  integer, intent(IN) :: grid_id
  integer, intent(IN) :: num_of_vgrid
  type(ft_type), pointer :: ftp

  ftp => ft_ptr
  do while(associated(ftp))
    if ((ftp%comp_id == comp_id).and.(ftp%grid_id == grid_id).and.(ftp%num_of_vgrid == num_of_vgrid)) then
      return
    end if
    ftp => ftp%next_ptr
  end do
  
  call error("search_file_type", "no such comp and grid")

end subroutine search_file_type
  
!*=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/15 [MOD] end_time(6) -> end_time(:)
! 2014/11/04 [MOD] integer :: time_array -> integer(kind=8) :: time_array
! 2015/04/06 [MOD] jml_isLocalLeader
! 2015/04/06 [MOD] call set_master_file_name
subroutine jcup_write_restart_base(fid, comp_id, end_time)
  use jcup_mpi_lib, only : jml_isLocalLeader
  use jcup_constant, only : STRING_LEN
  use jcup_buffer, only : get_num_of_time, get_send_buffer_ptr
  use jcup_time_buffer, only : time_buffer, get_start_data_ptr, get_next_time_ptr => get_next_ptr
  use jcup_data_buffer, only : data_buffer, get_next_data_ptr => get_next_ptr, write_data_buffer
  use jcup_comp, only : get_component_name
  use jcup_utils, only : error
  use jcup_time, only : write_time
  use jcup_config, only : get_send_data_conf_ptr, send_data_conf_type, set_current_conf
  implicit none
  integer, intent(IN) :: fid ! file id
  integer, intent(IN) :: comp_id ! component id
  integer, intent(IN) :: end_time(:) ! integration end time
  character(len=STRING_LEN) :: master_file_name
  logical :: is_opened
  type(time_buffer), pointer :: tb
  type(data_buffer), pointer :: db
  integer :: num_of_time
  integer :: i
  integer(kind=8) :: time_array(8) ! 2014/07/14 [MOD], 2014/11/04 [MOD] integer -> integer(kind=8)
  logical :: write_flag
  character(len=NAME_LEN) :: data_name
  real(kind=8), pointer :: data_ptr(:)
  type(send_data_conf_type), pointer :: send_data_conf_ptr
  integer :: file_handler
  integer :: str_index

  if (jml_isLocalLeader(comp_id)) then

    call set_master_file_name(comp_id, end_time, "mst", master_file_name)

    inquire(fid, opened = is_opened)
    if (is_opened) close(fid)

    open(fid, file=trim(master_file_name),form = 'formatted', &
           access = 'sequential', action = 'write', err = 200)

    call write_time(fid, comp_id)
  end if


  tb => get_send_buffer_ptr()

  num_of_time = get_num_of_time(tb)

  do i = 1, num_of_time

    db => get_start_data_ptr(tb)
    do
      if (.not.associated(db)) exit
      if (associated(db)) then
        time_array(1) = tb%time%yyyy ; time_array(2) = tb%time%mo ; time_array(3) = tb%time%dd
        time_array(4) = tb%time%hh   ; time_array(5) = tb%time%mm ; time_array(6) = tb%time%ss
        time_array(7) = tb%time%milli_sec ; time_array(8) = tb%time%micro_sec ! 2014/07/14 [ADD]
        !if (time_array(1) < 9999) then

          call write_data_buffer(time_array, db, fid, comp_id, write_flag, data_name, data_ptr)

          if (write_flag) then ! output buffer data
              call set_current_conf(comp_id)
              str_index = index(data_name, "__")
              if (str_index >= 1) then              
                send_data_conf_ptr => get_send_data_conf_ptr(data_name(1:str_index-1))
              else
                send_data_conf_ptr => get_send_data_conf_ptr(data_name)
              end if
              call jcup_io_open_file("./", comp_id, data_name, time_array, send_data_conf_ptr%grid_id, &
                                     send_data_conf_ptr%num_of_data, file_handler)            
              call jcup_io_write_data(file_handler, data_ptr)
              call jcup_io_close_file(file_handler)
          end if
        !end if
      end if
      db => get_next_data_ptr(db)
      if (associated(db, tb%dt_start)) exit
    end do

    tb => get_next_time_ptr(tb)

  end do


  if (jml_isLocalLeader(comp_id)) close(fid)


  return

200 call error('jcup_write_restart_base','cannot create restart master file: '//trim(master_file_name))
  
end subroutine jcup_write_restart_base

!*=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/14 [MOD] end_time(6) -> end_time(:), time_array(6) -> time_array(8)
! 2014/11/04 [MOD] integer :: time_array -> integer(kind=8) :: time_array
! 2014/11/05 [MOD] integer :: int_buffer(12) -> integer(kind=8) :: int_buffer(12)
! 2015/04/06 [MOD] call set_master_file_name
subroutine jcup_read_restart_base(fid, comp_id, end_time)
  use jcup_constant, only : NAME_LEN, STRING_LEN
  use jcup_buffer, only : get_num_of_time, get_send_buffer_ptr
  use jcup_buffer, only : restore_buffer
  use jcup_comp, only : get_component_name, is_my_component
  use jcup_utils, only : error, put_log
  use jcup_time, only : read_time, time_type, get_time_unit, TU_SEC, TU_MIL, TU_MCR
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BcastLocal
  implicit none
  integer, intent(IN) :: fid ! file id
  integer, intent(IN) :: comp_id ! component id
  integer, intent(IN) :: end_time(:) ! integration end time ! 2014/07/14 [MOD]
  character(len=STRING_LEN) :: master_file_name
  logical :: is_opened
  integer :: num_of_time
  integer(kind=8) :: time_array(8) ! 2014/07/14 [MOD] time_array(6) -> time_array(8)
  integer :: component_id, data_id, data_type, data_dim
  character(len=NAME_LEN) :: data_name
  integer :: file_handler
  integer(kind=8) :: int_buffer(8+1+1+1+1) ! 2014/07/14 [MOD] int_buffer(6+1+1+1+1) -> int_buffer(8+1+1+1+1), 
                                           ! 2014/11/05 [MOD] integer -> integer(kind=8)
  real(kind=8), pointer :: data_ptr(:)
  type(time_type) :: data_time

  if (jml_isLocalLeader(comp_id)) then

    call set_master_file_name(comp_id, end_time, "mst", master_file_name)

    inquire(fid, opened = is_opened)
    if (is_opened) close(fid)

    open(fid, file=trim(master_file_name),form = 'formatted', &
           access = 'sequential', action = 'read', err = 200)

    !write(0,*) "jcup_read_restart_base 1 ", master_file_name
    call read_time(fid, comp_id)
    !write(0,*) "jcup_read_restart_base 2 ", comp_id

    do

      read(fid, *, end = 100) data_name
      read(fid, *, end = 100) time_array
      read(fid, *, end = 100) component_id
      read(fid, *, end = 100) data_id
      read(fid, *, end = 100) data_type
      read(fid, *, end = 100) data_dim

      !write(0,*) "jcup_read_restart_base 3 ", trim(data_name), time_array

      int_buffer(1:8) = time_array
      int_buffer(9) = component_id
      int_buffer(10) = data_id
      int_buffer(11) = data_type
      int_buffer(12) = data_dim

      call jml_BcastLocal(comp_id, int_buffer, 1, 12)
      call jml_BcastLocal(comp_id, data_name)

      call jcup_read_restart_data("./", comp_id, data_name, time_array, data_ptr)
      
      data_time%yyyy = time_array(1) ; data_time%mo = time_array(2) ; data_time%dd = time_array(3)
      data_time%hh   = time_array(4) ; data_time%mm = time_array(5) ; data_time%ss = time_array(6)
      data_time%milli_sec = time_array(7) ; data_time%micro_sec = time_array(8)

      call restore_buffer(data_ptr, data_time, comp_id, data_id, data_name, data_type, data_dim)

      deallocate(data_ptr)
    end do

    100 continue

     int_buffer(1) = -100
     call jml_BcastLocal(comp_id, int_buffer, 1, 12)

  else

    call read_time(fid, comp_id)

    do
      call jml_BcastLocal(comp_id, int_buffer, 1, 12)

      if (int_buffer(1) <= -100) exit
      time_array(:) = int_buffer(1:8)
      component_id  = int_buffer(9)
      data_id       = int_buffer(10)
      data_type     = int_buffer(11)
      data_dim      = int_buffer(12)
      call jml_BcastLocal(comp_id, data_name)

      call jcup_read_restart_data("./", comp_id, data_name, time_array, data_ptr)

      data_time%yyyy = time_array(1) ; data_time%mo = time_array(2) ; data_time%dd = time_array(3)
      data_time%hh   = time_array(4) ; data_time%mm = time_array(5) ; data_time%ss = time_array(6)
      data_time%milli_sec = time_array(7) ; data_time%micro_sec = time_array(8)
      
      call restore_buffer(data_ptr, data_time, comp_id, data_id, data_name, data_type, data_dim)

      deallocate(data_ptr)
    end do

  end if

  close(fid)

  return

200 call error('jcup_read_restart_base','cannot open restart master file: '//trim(master_file_name))
  
end subroutine jcup_read_restart_base


!*=======+=========+=========+=========+=========+=========+=========+=========
! 2014/07/14 [MOD] data_time(6) -> data_time(8) 
! 2014/11/04 [MOD] integer :: data_time -> integer(kind=8) :: data_time
subroutine jcup_read_restart_data(out_dir, comp_id, data_name, data_time, data_ptr)
  use jcup_config, only : set_current_conf, get_send_data_conf_ptr, send_data_conf_type
  use jcup_grid_base, only : get_num_of_point
  implicit none
  character(len=*), intent(IN) :: out_dir
  integer, intent(IN) :: comp_id
  character(len=*), intent(IN) :: data_name
  integer(kind=8), intent(IN) :: data_time(8)
  real(kind=8), pointer :: data_ptr(:)
  type(send_data_conf_type), pointer :: send_data_conf_ptr
  integer :: grid_id
  integer :: num_of_vgrid
  integer :: num_of_point
  integer :: file_handler
  character(len=NAME_LEN) :: data_name_new

  call set_current_conf(comp_id)

  if (index(data_name, "__") > 0) then
    data_name_new = data_name(1:index(data_name, "__")-1)
  else
    data_name_new = data_name
  end if

  send_data_conf_ptr => get_send_data_conf_ptr(data_name_new)
  grid_id = send_data_conf_ptr%grid_id
  num_of_vgrid = send_data_conf_ptr%num_of_data

  num_of_point = get_num_of_point(comp_id, grid_id)*num_of_vgrid
   
  allocate(data_ptr(num_of_point))

  call jcup_io_open_file(out_dir, comp_id, data_name, data_time, grid_id, num_of_vgrid, file_handler)

  call jcup_io_read_data(file_handler, data_ptr)

  call jcup_io_close_file(file_handler)

end subroutine jcup_read_restart_data

!*=======+=========+=========+=========+=========+=========+=========+=========
! 2014/07/16 [MOD]
! 2014/11/04 [MOD] integer :: data_time -> integer(kind=8) :: data_time, 
subroutine jcup_io_open_file(out_dir, comp_id, data_name, data_time, grid_id, num_of_vgrid, file_handler)
  use jcup_comp, only : get_component_name
  use jcup_mpi_lib, only : jml_GetComm
  use jcup_constant, only : STRING_LEN
  use jcup_utils, only : error
  use jcup_time, only : get_time_unit, TU_SEC, TU_MIL, TU_MCR
  implicit none
  character(len=*), intent(IN) :: out_dir
  integer, intent(IN)          :: comp_id
  character(len=*), intent(IN) :: data_name
  integer(kind=8), intent(IN)          :: data_time(8)
  integer, intent(IN)          :: grid_id
  integer, intent(IN)          :: num_of_vgrid
  integer, intent(OUT) :: file_handler
  integer :: my_comm
  character(len=32) :: datarep
  integer :: ierror = 0
  integer(kind=MPI_OFFSET_KIND) :: offset
  character(len=STRING_LEN) :: file_name
  type(ft_type), pointer :: ftp

  my_comm = jml_GetComm(comp_id)

  select case (get_time_unit())
  case(TU_SEC)
    !write(file_name, '(A,A,".restart.",I4.4,5I2.2,".",A,".dat")') &
    !      trim(out_dir), trim(get_component_name(comp_id)), &
    !      data_time(1), data_time(2), data_time(3), data_time(4), data_time(5), data_time(6), trim(data_name) 
    write(file_name, '(A,A,".restart.",I14.14,".",A,".dat")') &
          trim(out_dir), trim(get_component_name(comp_id)), &
          data_time(6), trim(data_name) 
  case(TU_MIL)
    !write(file_name, '(A,A,".restart.",I4.4,5I2.2,I3.3,".",A,".dat")') &
    !      trim(out_dir), trim(get_component_name(comp_id)), &
    !      data_time(1), data_time(2), data_time(3), data_time(4), data_time(5), data_time(6), data_time(7), trim(data_name) 
    write(file_name, '(A,A,".restart.",I14.14,I3.3,".",A,".dat")') &
          trim(out_dir), trim(get_component_name(comp_id)), &
          data_time(6), data_time(7), trim(data_name) 
  case(TU_MCR)
    !write(file_name, '(A,A,".restart.",I4.4,5I2.2,2I3.3,".",A,".dat")') &
    !      trim(out_dir), trim(get_component_name(comp_id)), &
    !      data_time(1), data_time(2), data_time(3), data_time(4), data_time(5), data_time(6), data_time(7), data_time(8), trim(data_name) 
    write(file_name, '(A,A,".restart.",I14.14,2I3.3,".",A,".dat")') &
          trim(out_dir), trim(get_component_name(comp_id)), &
          data_time(6), data_time(7), data_time(8), trim(data_name) 
  case default
    call error("jcup_io_open_file", "time unit parameter error")
  end select

  call MPI_File_Open(my_comm, trim(file_name), &
                     MPI_MODE_CREATE + MPI_MODE_RDWR, MPI_INFO_NULL, file_handler, ierror)

    if (ierror>0) then
    !   write(LOG_FILE_ID, '(" Msg : Sub[jcup_io_open_file]/Mod[jcup_io_base]")')
    !   write(LOG_FILE_ID, '(" ----- MPI_File_open error",I5)') ierror 
    !   write(LOG_FILE_ID, '(" ----- file name : ",A)') trim(fhp%file_name)
    !   write(LOG_FILE_ID, '(" ------- file_handler id =",I3," file_handler =",I5,", file_type id =",I3," file_type =",I12)') &
    !                       fhp%fh_id, fhp%file_handler, fhp%file_type%ft_id, fhp%file_type%file_type
    !   close(LOG_FILE_ID)
       call error("jcup_io_open_file", "MPI_File_Open error, file name : "//trim(file_name))
       stop
    end if

  !write(LOG_FILE_ID, '(" Msg : Sub[jcup_io_open_file]/Mod[jcup_io_base]")')
  !write(LOG_FILE_ID, '(" ----- File open : ",A)') trim(fhp%file_name)
  !write(LOG_FILE_ID, '(" ------- file_handler id =",I3," file_handler =",I5,", file_type id =",I3," file_type =",I12)') &
  !                    fhp%fh_id, fhp%file_handler, fhp%file_type%ft_id, fhp%file_type%file_type


  call search_file_type(comp_id, grid_id, num_of_vgrid, ftp)

  offset = 0 
  datarep = "native"
  call MPI_File_set_view(file_handler, offset, MPI_REAL8, ftp%file_type, trim(datarep), MPI_INFO_NULL, ierror)

   ! if (ierror>0) then
   !    write(LOG_FILE_ID, '(" Msg : Sub[jcup_io_open_file]/Mod[jcup_io_base]")')
   !    write(LOG_FILE_ID, '(" ----- MPI_File_set_view error",I5)') ierror 
   !    close(LOG_FILE_ID)
   !    write(0,*) "jcup_io_open_file, MPI_File_set_view error ", ierror
   !    stop
   ! end if

  !write(LOG_FILE_ID, '(" ----- File set view")') 
  !write(LOG_FILE_ID, '(" ------- displacement =",I5,", data representation ="A)') &
  !                    offset, datarep

end subroutine jcup_io_open_file

!*=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_io_close_file(file_handler)
  !use jcup_interface, only : jcup_error
  implicit none
  integer, intent(IN) :: file_handler
  integer :: ierror = 0

  call MPI_file_close(file_handler, ierror)
      
      !if (ierror > 0) then
      !  write(LOG_FILE_ID, '(" Msg : Sub[jcup_io_close_file]/Mod[jcup_io_base]")')
      !  write(LOG_FILE_ID, '(" ----- File close error : ",A)') trim(current_fh%file_name)
      !  close(LOG_FILE_ID)
      !  write(0,*) "jcup_io_close file, file close error ", ierror
      !  stop
      !end if

      !write(LOG_FILE_ID, '(" Msg : Sub[jcup_io_close_file]/Mod[jcup_io_base]")')
      !write(LOG_FILE_ID, '(" ----- File close : ",A)') trim(current_fh%file_name)

end subroutine jcup_io_close_file

!*=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_io_write_data(file_handler, data)
  implicit none
  integer, intent(IN) :: file_handler
  real(kind=8), pointer :: data(:)
  integer :: ierror
  integer :: status(MPI_STATUS_SIZE)
  integer :: data_size

  data_size = size(data)

  call MPI_File_write_all(file_handler, data, data_size, MPI_REAL8, status, ierror)

  !write(LOG_FILE_ID, *)
  !write(LOG_FILE_ID, '(" ---------------------------------------------------------------------------------------")') 
  !write(LOG_FILE_ID, '(" Msg : Sub[jcup_write_data]/Mod[jcup_io_base]")')
  !write(LOG_FILE_ID, '(" --- Write data. data name =",A)') data_name
  !write(LOG_FILE_ID, '(" ----- file id = ",I3,", file type id =",I3,", data size =",I6)') &
  !                    current_ptr%fh_id, current_ptr%file_type%ft_id, data_size
  !write(LOG_FILE_ID, '(" ----- max val = ",F16.5,", min val =",F16.5)') &
  !                    maxval(data), minval(data)


end subroutine jcup_io_write_data

!*=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_io_read_data(file_handler, data)
  implicit none
  integer, intent(IN) :: file_handler
  real(kind=8), pointer :: data(:)
  integer :: ierror
  integer :: status(MPI_STATUS_SIZE)
  integer :: data_size

  data_size = size(data)

  call MPI_File_read_all(file_handler, data, data_size, MPI_REAL8, status, ierror)

  !write(LOG_FILE_ID, *)
  !write(LOG_FILE_ID, '(" ---------------------------------------------------------------------------------------")') 
  !write(LOG_FILE_ID, '(" Msg : Sub[jcup_write_data]/Mod[jcup_io_base]")')
  !write(LOG_FILE_ID, '(" --- Write data. data name =",A)') data_name
  !write(LOG_FILE_ID, '(" ----- file id = ",I3,", file type id =",I3,", data size =",I6)') &
  !                    current_ptr%fh_id, current_ptr%file_type%ft_id, data_size
  !write(LOG_FILE_ID, '(" ----- max val = ",F16.5,", min val =",F16.5)') &
  !                    maxval(data), minval(data)


end subroutine jcup_io_read_data

!*=======+=========+=========+=========+=========+=========+=========+=========
! 2015/04/06 [NEW]
! 2015/04/14 [MOD] add is_barrier_finish 
subroutine jcup_write_restart_gmean(fid, comp_id, end_time)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BarrierLeader
  use jcup_constant, only : STRING_LEN
  use jcup_comp, only : get_component_name, get_num_of_total_component
  use jcup_utils, only : error
  use jcup_time, only : get_time_unit, TU_SEC, TU_MIL, TU_MCR
  use jcup_exchange, only : write_all_scalar_data
  implicit none
  integer, intent(IN) :: fid ! file id
  integer, intent(IN) :: comp_id ! component id
  integer, intent(IN) :: end_time(:) ! integration end time
  character(len=STRING_LEN) :: gmean_file_name
  logical :: is_opened
  logical, save :: is_barrier_finish = .false. ! 2015/04/14 [ADD]

  integer :: i

  do i = 1, get_num_of_total_component() ! 2015/04/14 [ADD]
    if (jml_isLocalLeader(i)) then
      if (.not.is_barrier_finish) then
        call jml_BarrierLeader()
        is_barrier_finish = .true.
        exit
      end if
    end if
  end do

  if (.not.jml_isLocalLeader(comp_id)) return

  call set_master_file_name(comp_id, end_time, "gmean", gmean_file_name)

  inquire(fid, opened = is_opened)
  if (is_opened) close(fid)

  open(fid, file=trim(gmean_file_name),form = 'formatted', &
         access = 'sequential', action = 'write', err = 200)

  call write_all_scalar_data(fid, comp_id)

  close(fid)

  return

200 call error('jcup_write_restart_gmean','cannot create restart gmean file: '//trim(gmean_file_name))

end subroutine jcup_write_restart_gmean

!*=======+=========+=========+=========+=========+=========+=========+=========
! 2015/04/06 [NEW] 
subroutine jcup_read_restart_gmean(fid, comp_id, end_time)
  use jcup_utils, only  : error, put_log, IntToStr
  use jcup_constant, only : STRING_LEN, NAME_LEN
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLeader
  use jcup_comp, only : get_num_of_total_component, get_component_name
  implicit none
  integer, intent(IN) :: fid ! file id
  integer, intent(IN) :: comp_id ! component id
  integer, intent(IN) :: end_time(:) ! integration end time
  character(len=STRING_LEN) :: gmean_file_name
  character(len=NAME_LEN) :: source_comp_name
  character(len=NAME_LEN) :: my_comp_name
  integer :: tag
  real(kind=8) :: gmean(1)
  logical :: is_opened
  integer :: i

  if (.not.jml_isLocalLeader(comp_id)) return

  my_comp_name = get_component_name(comp_id)

  !write(0,*) "read_restart_gmean ", trim(my_comp_name)

  do i = 1, get_num_of_total_component() ! i is dest component id
    if (i == comp_id) cycle

    call set_master_file_name(i, end_time, "gmean", gmean_file_name)

    inquire(fid, opened = is_opened)
    if (is_opened) close(fid)

    open(fid, file=trim(gmean_file_name),form = 'formatted', &
           access = 'sequential', action = 'read', err = 200)

    do 

      read(fid, *, end = 100) source_comp_name
      read(fid, *, end = 100) tag
      read(fid, *, end = 100) gmean
    
      if (trim(source_comp_name) == trim(my_comp_name)) then ! if source component is my component
        call put_log("send gmean data to "//trim(get_component_name(i))//", data tag = "//trim(IntToStr(tag)))
        call jml_SendLeader(gmean, 1, 1, i-1, tag)  
      end if

    end do

    100 continue

    close(fid)
 
  end do


  return

200 call error('jcup_read_restart_gmean','cannot open restart gmean file: '//trim(gmean_file_name))
  
end subroutine jcup_read_restart_gmean

!*=======+=========+=========+=========+=========+=========+=========+=========+
! 2015/04/06 [NEW]
subroutine set_master_file_name(comp_id, end_time, file_code, file_name)
  use jcup_utils, only  : error
  use jcup_time, only : get_time_unit, TU_SEC, TU_MIL, TU_MCR
  use jcup_comp, only : get_component_name
  implicit none
  integer, intent(IN) :: comp_id
  integer, intent(IN) :: end_time(:)
  character(len=*), intent(IN) :: file_code
  character(len=*), intent(OUT) :: file_name

  select case(get_time_unit())
  case (TU_SEC)
    if (size(end_time) < 6) call error("set_master_file_name", "array size of end time < 6")
    write(file_name, '(A,".restart.",A,".",I4.4,5I2.2,".dat")') trim(get_component_name(comp_id)), trim(file_code), &
         end_time(1), end_time(2), end_time(3), end_time(4), end_time(5), end_time(6)
  case (TU_MIL)
    if (size(end_time) < 7) call error("set_master_file_name", "array size of end time < 7")
    write(file_name, '(A,".restart.",A,".",I4.4,5I2.2,I3.3,".dat")') trim(get_component_name(comp_id)), trim(file_code), &
         end_time(1), end_time(2), end_time(3), end_time(4), end_time(5), end_time(6), end_time(7)
  case (TU_MCR)
    if (size(end_time) < 8) call error("set_master_file_name", "array size of end time < 8")
    write(file_name, '(A,".restart.",A,".",I4.4,5I2.2,2I3.3,".dat")') trim(get_component_name(comp_id)), trim(file_code), &
         end_time(1), end_time(2), end_time(3), end_time(4), end_time(5), end_time(6), end_time(7), end_time(8)
  case default
    call error("set_master_file_name", "time unit parameter error")
  end select
end subroutine set_master_file_name

end module jcup_io_base

