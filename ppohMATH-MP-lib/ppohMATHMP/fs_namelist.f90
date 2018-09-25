!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! namelist handling module
!! 
module fs_namelist
  private

!--------------------------------   public  ----------------------------------!

  public :: init_configuration ! subroutine (conf_file_name)
  public :: write_configuration ! subroutine (file_id)
  public :: get_num_of_varp ! integer function (comp_name)
  public :: get_varp_conf ! subroutine (comp_name, var_num, var_name, grid_name)
  public :: get_num_of_varg ! integer function (comp_name)
  public :: get_varg_conf ! subroutine (comp_name, var_num, var_name, grid_name, put_comp_name, put_var_name, intvl, lag, flag)
  public :: get_mapping_table_file ! subroutine (put_comp_name, put_grid_name, get_comp_name, get_grid_name, file_name)

!--------------------------------  private  ----------------------------------!

  integer, parameter :: NAME_LEN = 32
  integer, parameter :: STR_LEN  = 128

  type var_conf_type ! type for variable configuration
    character(len=NAME_LEN) :: var_put ! name of put variable
    character(len=NAME_LEN) :: var_get ! name of get variable
    integer :: intvl ! data exchange interval 
    integer :: lag ! data exchange time lag 
    character(len=NAME_LEN) :: flag  ! 'AVR' os "SNP"
  end type

  type comp_conf_type ! type for component configuration
    character(len=NAME_LEN) :: comp_put ! name of put component
    character(len=NAME_LEN) :: comp_get ! name of get component
    character(len=NAME_LEN) :: grid_put ! name of put grid
    character(len=NAME_LEN) :: grid_get ! name of get grid
    integer :: remap_tag ! remapping tag
    character(len=STR_LEN)  :: fl_remap ! mapping table file name
    integer :: num_of_var
    type(var_conf_type), pointer :: var_conf(:)
  end type

  integer :: num_of_conf
  type(comp_conf_type), pointer :: fs_conf(:)

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read namelist file
subroutine init_configuration(conf_file_name)
  implicit none
  character(len=*), intent(IN) :: conf_file_name
  character(len=STR_LEN) :: file_str
  integer, parameter :: FID = 128
  character(len=NAME_LEN) :: comp_put
  character(len=NAME_LEN) :: comp_get
  character(len=NAME_LEN) :: grid_put
  character(len=NAME_LEN) :: grid_get
  integer :: remap_tag
  character(len=STR_LEN)  :: fl_remap
  namelist / model_config / comp_put, comp_get, grid_put, grid_get, remap_tag, fl_remap
  character(len=NAME_LEN) :: var_put
  character(len=NAME_LEN) :: var_get
  integer :: intvl
  integer :: lag
  character(LEN=NAME_LEN) :: flag
  namelist / var_config / var_put, var_get, intvl, lag, flag

  integer :: ierror
  integer :: i, j

  open(unit = FID, file = trim(conf_file_name), status="OLD", iostat=ierror)
  if (ierror /= 0) then
     write(0,*) "module fs_namelist, subroutine read_conf_file: Cannot open parameter file! file name = "//trim(conf_file_name)
     stop 999
  end if
  rewind(FID)

  call count_configuration(FID)

  rewind(FID)
  
  do i = 1, num_of_conf
    read(FID, nml=model_config)
    fs_conf(i)%comp_put = comp_put
    fs_conf(i)%comp_get = comp_get
    fs_conf(i)%grid_put = grid_put
    fs_conf(i)%grid_get = grid_get
    fs_conf(i)%remap_tag = remap_tag
    fs_conf(i)%fl_remap = fl_remap
    do j = 1, fs_conf(i)%num_of_var
      read(FID, nml=var_config)
      fs_conf(i)%var_conf(j)%var_put = var_put
      fs_conf(i)%var_conf(j)%var_get = var_get
      fs_conf(i)%var_conf(j)%intvl = intvl
      fs_conf(i)%var_conf(j)%lag   = lag
      fs_conf(i)%var_conf(j)%flag  = flag
    end do
  end do

  close(FID)

end subroutine init_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! count configuration
subroutine count_configuration(FID)
  implicit none
  integer, intent(IN) :: FID
  character(len=STR_LEN) :: file_str
  integer :: counter
  integer :: i

  ! count num_of_conf
  num_of_conf = 0
  do
    read(FID,*, end=100) file_str
    if (trim(file_str) == "&model_config") num_of_conf = num_of_conf + 1 
  end do
  100 continue

  allocate(fs_conf(num_of_conf))

  do i = 1, num_of_conf
    fs_conf(i)%comp_put = ""
    fs_conf(i)%comp_get = ""
    fs_conf(i)%grid_put = ""
    fs_conf(i)%grid_get = ""
    fs_conf(i)%remap_tag = 0
    fs_conf(i)%fl_remap = ""
    fs_conf(i)%num_of_var = 0
    fs_conf(i)%var_conf => null()
  end do
 
  rewind(FID) 

  ! count num_of_var
  counter = 0
  do 
    read(FID, *, end = 200) file_str
    if (trim(file_str) == "&model_config") counter = counter + 1
    if (trim(file_str) == "&var_config") fs_conf(counter)%num_of_var = fs_conf(counter)%num_of_var + 1
  end do

  200 continue 

  do i = 1, num_of_conf
    allocate(fs_conf(i)%var_conf(fs_conf(i)%num_of_var))
  end do

end subroutine count_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! write configuration
subroutine write_configuration(FID)
  implicit none
  integer, intent(IN) :: FID
  integer :: i, j

  do i = 1, num_of_conf
    write(FID, '(A)') "configuration"
    write(FID, '(" --- put component : ",A)') fs_conf(i)%comp_put 
    write(FID, '(" --- get component : ",A)') fs_conf(i)%comp_get 
    write(FID, '(" --- put grid      : ",A)') fs_conf(i)%grid_put 
    write(FID, '(" --- get grid      : ",A)') fs_conf(i)%grid_get
    write(FID, '(" --- remapping tag : ",I)') fs_conf(i)%remap_tag
    write(FID, '(" --- mapping table : ",A)') fs_conf(i)%fl_remap

    do j = 1, fs_conf(i)%num_of_var
      write(FID, '(" ----- put varriable : ",A)') fs_conf(i)%var_conf(j)%var_put
      write(FID, '(" ----- get varriable : ",A)') fs_conf(i)%var_conf(j)%var_get
      write(FID, '(" ----- interval      : ",I)') fs_conf(i)%var_conf(j)%intvl
      write(FID, '(" ----- time lag      : ",I)') fs_conf(i)%var_conf(j)%lag
      write(FID, '(" ----- flag          : ",A)') fs_conf(i)%var_conf(j)%flag
      write(FID, *)
    end do
    write(FID, *)
  end do

end subroutine write_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get the number of put variable
integer function get_num_of_varp(comp_name)
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer :: i, j
  integer :: counter

  counter = 0
  do i = 1, num_of_conf
    if (trim(comp_name) == trim(fs_conf(i)%comp_put)) then
      counter = counter + fs_conf(i)%num_of_var
    end if
  end do

  get_num_of_varp = counter

end function get_num_of_varp

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get put variable configuration
subroutine get_varp_conf(comp_name, var_num, var_name, grid_name)
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer, intent(IN) :: var_num
  character(len=*), intent(OUT) :: var_name
  character(len=*), intent(OUT) :: grid_name
  integer :: i, j
  integer :: counter

  counter = 0
  do i = 1, num_of_conf
    if (trim(comp_name) == trim(fs_conf(i)%comp_put)) then
      do j = 1, fs_conf(i)%num_of_var
        counter = counter + 1
        if (counter == var_num) then
          var_name = fs_conf(i)%var_conf(j)%var_put
          grid_name = fs_conf(i)%grid_put
          return
        end if
      end do
    end if
  end do

end subroutine get_varp_conf

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get the number of get variable
integer function get_num_of_varg(comp_name)
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer :: i, j
  integer :: counter

  counter = 0
  do i = 1, num_of_conf
    if (trim(comp_name) == trim(fs_conf(i)%comp_get)) then
      counter = counter + fs_conf(i)%num_of_var
    end if
  end do

  get_num_of_varg = counter

end function get_num_of_varg

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get get variable configuration
subroutine get_varg_conf(comp_name, var_num, var_name, grid_name, put_comp_name, put_var_name, map_tag, intvl, lag, flag)
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer, intent(IN) :: var_num
  character(len=*), intent(OUT) :: var_name
  character(len=*), intent(OUT) :: grid_name
  character(len=*), intent(OUT) :: put_comp_name
  character(len=*), intent(OUT) :: put_var_name
  integer, intent(OUT) :: map_tag
  integer, intent(OUT) :: intvl
  integer, intent(OUT) :: lag
  character(len=*), intent(OUT) :: flag
  integer :: i, j
  integer :: counter

  counter = 0
  do i = 1, num_of_conf
    if (trim(comp_name) == trim(fs_conf(i)%comp_get)) then
      do j = 1, fs_conf(i)%num_of_var
        counter = counter + 1
        if (counter == var_num) then
          var_name = fs_conf(i)%var_conf(j)%var_get
          grid_name = fs_conf(i)%grid_get
          put_comp_name = fs_conf(i)%comp_put
          put_var_name  = fs_conf(i)%var_conf(j)%var_put
          map_tag = fs_conf(i)%remap_tag
          intvl = fs_conf(i)%var_conf(j)%intvl
          lag   = fs_conf(i)%var_conf(j)%lag
          flag  = fs_conf(i)%var_conf(j)%flag
          return
        end if
      end do
    end if
  end do

end subroutine get_varg_conf

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get mapping table file name
subroutine get_mapping_table_file(put_comp_name, put_grid_name, get_comp_name, get_grid_name, mapping_tag, mapping_table_file)  
  implicit none
  character(len=*), intent(IN) :: put_comp_name, put_grid_name, get_comp_name, get_grid_name
  integer, intent(IN) :: mapping_tag
  character(len=*), intent(OUT) :: mapping_table_file
  integer :: i

  do i = 1, num_of_conf
    if (trim(fs_conf(i)%comp_put) == trim(put_comp_name)) then
    if (trim(fs_conf(i)%grid_put) == trim(put_grid_name)) then
    if (trim(fs_conf(i)%comp_get) == trim(get_comp_name)) then
    if (trim(fs_conf(i)%grid_get) == trim(get_grid_name)) then
    if (fs_conf(i)%remap_tag == mapping_tag) then
      mapping_table_file = fs_conf(i)%fl_remap
      return
    end if
    end if
    end if
    end if
    end if
  end do

  write(0,*) "Module fs_namelist, Subroutine get_mapping_table_file, no such comp, grid name !!"
  write(0,*) "put_comp_name = "//trim(put_comp_name)//", put_grid_name = "//trim(put_grid_name)
  write(0,*) "get_comp_name = "//trim(get_comp_name)//", get_grid_name = "//trim(get_grid_name)
  stop 999

end subroutine get_mapping_table_file

end module fs_namelist
