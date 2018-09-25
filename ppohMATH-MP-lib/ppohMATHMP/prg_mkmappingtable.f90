program main
  use fs_grid, only : write_grid_kml, cal_mapping_table
  use fem_grid, only : init_fem_grid, fi_grid, make_coupling_info_file
  use fdm_grid, only : grid_s, grid_u, grid_v, grid_w, init_fdm_grid 
  integer, parameter :: STR_LEN = 128
  integer, parameter :: NAME_LEN = 32
  integer, parameter :: FID = 221
  character(len=STR_LEN) :: namelist_file = "./coupling.nmlst" 

  character(len=NAME_LEN) :: model_name, grid_name
  integer :: total_rank
  integer :: xorg, yorg
  real(kind=8) :: angle
  character(len=STR_LEN) :: global_node_file
  character(len=STR_LEN) :: coupling_mesh_file
  character(len=STR_LEN) :: coupling_info_file 
  namelist / fem_model_config / model_name, grid_name, total_rank, xorg, yorg, angle, global_node_file, coupling_mesh_file, coupling_info_file

  integer :: delta_x, delta_y, delta_z
  integer :: kfs
  character(len=STR_LEN) :: cod_file, index_file
  namelist / fdm_model_config / model_name, grid_name, xorg, yorg, delta_x, delta_y, delta_z, kfs, cod_file, index_file


  character(len=STR_LEN) :: mapping_table_s
  character(len=STR_LEN) :: mapping_table_u
  character(len=STR_LEN) :: mapping_table_v
  character(len=STR_LEN) :: mapping_table_w
  character(len=STR_LEN) :: log_file_name
  integer :: kml_output
  character(len=STR_LEN) :: kml_file_name
  namelist / mapping_table / mapping_table_s, mapping_table_u, mapping_table_v, mapping_table_w, &
                             log_file_name, kml_output, kml_file_name 
  mapping_table_s = "-"
  mapping_table_u = "-"
  mapping_table_v = "-"
  mapping_table_w = "-"
  kml_output = 0
  kml_file_name = "-"

  open(unit=FID, file=trim(namelist_file), status="old")
  rewind(FID)

  read(FID, nml=fem_model_config)
  call init_fem_grid(0, total_rank, xorg, yorg, angle, global_node_file, coupling_mesh_file, coupling_info_file)
  call make_coupling_info_file()

  rewind(FID)
  read(FID,nml=fdm_model_config)
  call init_fdm_grid(xorg, yorg, delta_x, delta_y, delta_z, kfs, cod_file, index_file)



  rewind(FID)
  read(FID,nml=mapping_table)
  close(FID)


  write(0,*) "grid stag ", grid_u%stag_type
  write(0,*) "grid stag ", grid_v%stag_type
  write(0,*) "grid stag ", grid_w%stag_type

  if ((kml_output == 1).and.(trim(kml_file_name) /= "-")) then
    call write_grid_kml(trim(kml_file_name), fi_grid, grid_s)
  end if

  if (trim(mapping_table_s) /= "-") then
    open(unit=FID, file = trim(mapping_table_s))
    write(FID, *) "mapping_table S"
    call cal_mapping_table(FID, fi_grid, grid_s)
    close(FID)
  end if

  if (trim(mapping_table_u) /= "-") then
    open(unit=FID, file = trim(mapping_table_u))
    write(FID, *) "mapping_table U"
    call cal_mapping_table(FID, fi_grid, grid_u)
    close(FID)
  end if

  if (trim(mapping_table_v) /= "-") then
    open(unit=FID, file = trim(mapping_table_v))
    write(FID, *) "mapping_table V"
    call cal_mapping_table(FID, fi_grid, grid_v)
    close(FID)
  end if

  if (trim(mapping_table_w) /= "-") then
    open(unit=FID, file = trim(mapping_table_w))
    write(FID, *) "mapping_table W"
    call cal_mapping_table(FID,  fi_grid, grid_w)
    close(FID)
  end if

end program 
