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


module  hecmw_visualizer

  use hecmw_util
  use hecmw_result
  use hecmw_dist_copy_f2c_f

  implicit none

  public  :: hecmw_visualize
  public  :: hecmw_visualize_init
  public  :: hecmw_visualize_finalize

  private
  character(len=100) :: sname, vname

contains

subroutine  hecmw_visualize( mesh, result_data, step, max_step, interval )
  implicit none
  type(hecmwST_local_mesh),  intent(in) :: mesh
  type(hecmwST_result_data), intent(in) :: result_data
  integer(kind=kint),        intent(in) :: step, max_step, interval
  integer(kind=kint)                    :: ierr

  call  hecmw_visualize_init_if( mesh%n_node, mesh%n_elem, ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif

  call  hecmw_dist_copy_f2c( mesh, ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif

  call  hecmw_result_copy_f2c( result_data, ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif

  call  hecmw_visualize_if( step, max_step, interval, ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif

  call  hecmw_visualize_finalize_if( ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif
end subroutine  hecmw_visualize


subroutine  hecmw_visualize_init( )
  implicit none
  integer(kind=kint) :: ierr

  call  hecmw_init_for_visual_if( ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif
end subroutine  hecmw_visualize_init


subroutine hecmw_visualize_finalize( )
  implicit none
  integer(kind=kint) :: ierr

  call  hecmw_finalize_for_visual_if( ierr )
  if( ierr /= 0 )  then
    call  hecmw_abort( hecmw_comm_get_comm( ) )
  endif
end subroutine hecmw_visualize_finalize

end module  hecmw_visualizer
