!=====================================================================!!
!!                                                                    !!
!!   Software Name : ppOpen-APPL/AMR-FDM (ppohAMRFDM)                 !!
!!         Version : 0.3.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohAMRFDM.                               !!
!!     ppohAMRFDM is a free software, you can use it under the terms  !!
!!     of The MIT License (MIT). See LICENSE file and User's guide    !!
!!     for more details.                                              !!
!!                                                                    !!
!!   ppOpen-HPC project:                                              !!
!!     Open Source Infrastructure for Development and Execution of    !!
!!     Large-Scale Scientific Applications on Post-Peta-Scale         !!
!!     Supercomputers with Automatic Tuning (AT).                     !!
!!                                                                    !!
!!   Organizations:                                                   !!
!!     The University of Tokyo                                        !!
!!       - Information Technology Center                              !!
!!       - Atmosphere and Ocean Research Institute (AORI)             !!
!!       - Interfaculty Initiative in Information Studies/            !!
!!         Earthquake Research Institute (ERI)                        !!
!!       - Graduate School of Frontier Science                        !!
!!     Kyoto University                                               !!
!!       - Academic Center for Computing and Media Studies            !!
!!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC) !!
!!                                                                    !!
!!   Sponsorship:                                                     !!
!!     Japan Science and Technology Agency (JST), Basic Research      !!
!!     Programs: CREST, Development of System Software Technologies   !!
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Masaharu Matsumoto, The University of Tokyo  !!
!!                       matsumoto(at)cc.u-tokyo.ac.jp           >    !!
!!                                                                    !!
!!====================================================================!!

subroutine ppohAMRFDM_input_data(st_param,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  character(kind=1,len=100)::filename,tmp
  integer(kind=ppohAMRFDM_kint)::n_gp,n_pr
  filename="./input.dat"
  open(unit=10,file=filename,status='old')
  read(10,*) tmp
  read(10,*) tmp
  read(10,*) n_gp
  read(10,*) tmp
  read(10,*) tmp
  read(10,*) n_pr
  read(10,*) tmp
  read(10,*) st_param%iomp0
  read(10,*) tmp
  read(10,*) st_param%LvMax
  read(10,*) tmp
  read(10,*) st_param%initLv
  read(10,*) tmp
  read(10,*) tmp
  read(10,*) st_param%DDD
  read(10,*) tmp
  read(10,*) st_param%DDD_crtr
  read(10,*) tmp
  read(10,*) st_param%npy
  read(10,*) tmp
  read(10,*) st_param%nbf
  read(10,*) tmp
  read(10,*) st_param%crtr_r0
  read(10,*) tmp
  read(10,*) st_param%crtr_d0
  read(10,*) tmp
  read(10,*) st_param%dx(1),st_param%dx(2),st_param%dx(3)
  read(10,*) tmp
  read(10,*) st_param%dt
  read(10,*) tmp
  read(10,*) st_param%itmax
  close(10)
  ! -- check input data --
  if(st_comm_info%rank==0) then
     if(st_param%DDD==1.and.n_gp<4) then
        print *,'error input data'
        print *,'To enable DDD function, set the power index of grid points > 4'
        call ppohAMRFDM_abort(st_comm_info)
     endif
  endif
  ! -- set parameter from input data --
  st_param%ixmax=2**n_gp; st_param%iymax=2**n_gp; st_param%izmax=2**n_gp
  st_param%pxmax=2**n_pr; st_param%pymax=2**n_pr; st_param%pzmax=2**n_pr
  return
end subroutine ppohAMRFDM_input_data
