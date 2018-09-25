module ppoh_MATHMP_base
  use mpi
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  integer, parameter, public :: NAME_LEN = 32
  integer, parameter, public :: STR_LEN  = 128
  integer,            public :: LOG_FID = 88

  
  public :: set_log_file_id ! subroutine (log_file_id)
  public :: open_log_file
  public :: put_log
  public :: close_log_file
  public :: error

!--------------------------------  private  ----------------------------------!


contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine open_log_file(file_name, my_rank)
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: my_rank
  character(len=STR_LEN) :: log_file_name
  integer :: istat

  write(log_file_name, "(A,I3.3)") trim(file_name), my_rank

  open(unit = LOG_FID, file = trim(log_file_name), form = "FORMATTED", action = "write", err = 1000)

  return

  1000 continue

  call error("Log file : "//trim(log_file_name)//" open error")

end subroutine open_log_file

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_log(log_str)
  implicit none
  character(len=*), intent(IN) :: log_str
  
  write(LOG_FID, *) trim(log_str)

end subroutine put_log

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine close_log_file()
  implicit none

  close(LOG_FID)

end subroutine close_log_file

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_log_file_id(log_file_id)
  implicit none
  integer, intent(IN) :: log_file_id

  LOG_FID = log_file_id

end subroutine set_log_file_id

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine error(error_str)
  implicit none
  character(len=*), intent(IN) :: error_str
  integer :: error_code
  integer :: ierror

  write(LOG_FID, *) "ERROR !!! : "//trim(error_str)
  write(0,       *) "ERROR !!! : "//trim(error_str)

  call mpi_abort(MPI_COMM_WORLD, error_code, ierror)

  stop 999

end subroutine error

end module ppoh_MATHMP_base
