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

module m_ppohAMRFDM_util
  !use mpi
  implicit none
  public
  include 'mpif.h'
  include 'ppohAMRFDM_precision.inc'
  !$ include 'omp_lib.h'
  
  type st_ppohAMRFDM_param
     ! -- input parameters --
     integer(kind=ppohAMRFDM_kint)::ixmax,iymax,izmax
     integer(kind=ppohAMRFDM_kint)::pxmax,pymax,pzmax
     integer(kind=ppohAMRFDM_kint)::iomp0
     integer(kind=ppohAMRFDM_kint)::LvMax
     integer(kind=ppohAMRFDM_kint)::initLv
     integer(kind=ppohAMRFDM_kint)::DDD
     integer(kind=ppohAMRFDM_kint)::DDDflg
     integer(kind=ppohAMRFDM_kint)::npy
     integer(kind=ppohAMRFDM_kint)::nbf
     integer(kind=ppohAMRFDM_kint)::nlv
     integer(kind=ppohAMRFDM_kint)::it,itmax
     real(kind=ppohAMRFDM_kdbl)::dx(1:3),dt,crtr_r0,crtr_d0,DDD_crtr
     real(kind=ppohAMRFDM_kdbl),allocatable::crtr_r(:),crtr_d(:)
     ! -- parameters derived from input --
     integer(kind=ppohAMRFDM_kint)::nfg,nint2
     integer(kind=ppohAMRFDM_kint),allocatable::itorder(:),lvorder(:)
     integer(kind=ppohAMRFDM_kint),allocatable::nID(:)
     integer(kind=ppohAMRFDM_kint)::nloop
  end type st_ppohAMRFDM_param

  type st_ppohAMRFDM_octset
     integer(kind=ppohAMRFDM_kint)::octN
     integer(kind=ppohAMRFDM_kint)::octLv
     integer(kind=ppohAMRFDM_kint)::Csort
     integer(kind=ppohAMRFDM_kint)::iFLG(1:3)
     integer(kind=ppohAMRFDM_kint)::iPOS(1:3)
     integer(kind=ppohAMRFDM_kint)::MrtN
     integer(kind=ppohAMRFDM_kint)::bndry
     real(kind=ppohAMRFDM_kdbl),allocatable::F(:)
     real(kind=ppohAMRFDM_kdbl),allocatable::C(:)
     integer(kind=ppohAMRFDM_kint)::load
     type(st_ppohAMRFDM_octset),pointer::octPrt
     type(st_ppohAMRFDM_octset),pointer::octNb1
     type(st_ppohAMRFDM_octset),pointer::octNb2
     type(st_ppohAMRFDM_octset),pointer::octNb3
     type(st_ppohAMRFDM_octset),pointer::octNb4
     type(st_ppohAMRFDM_octset),pointer::octNb5
     type(st_ppohAMRFDM_octset),pointer::octNb6
     type(st_ppohAMRFDM_octset),pointer::octCh1
     type(st_ppohAMRFDM_octset),pointer::octCh2
     type(st_ppohAMRFDM_octset),pointer::octCh3
     type(st_ppohAMRFDM_octset),pointer::octCh4
     type(st_ppohAMRFDM_octset),pointer::octCh5
     type(st_ppohAMRFDM_octset),pointer::octCh6
     type(st_ppohAMRFDM_octset),pointer::octCh7
     type(st_ppohAMRFDM_octset),pointer::octCh8
     type(st_ppohAMRFDM_octset),pointer::Psort
  end type st_ppohAMRFDM_octset
  
  type st_ppohAMRFDM_comm_info
     integer(kind=ppohAMRFDM_kint)::comm,nprocs,rank
     integer(kind=ppohAMRFDM_kint),allocatable::Mn2CPU(:)
     integer(kind=ppohAMRFDM_kint),allocatable::nrproc(:),ngproc(:)
     integer(kind=ppohAMRFDM_kint),allocatable::iMesh(:)
     integer(kind=ppohAMRFDM_kint)::MnDisp,MinMn,MaxMn
     integer(kind=ppohAMRFDM_kint),allocatable::snum(:,:),rnum(:,:)
  end type st_ppohAMRFDM_comm_info
  
  type st_ppohAMRFDM_sendbuf
     integer(kind=ppohAMRFDM_kint),allocatable::bufi(:)
     real(kind=ppohAMRFDM_kdbl),allocatable::bufr(:)
  end type st_ppohAMRFDM_sendbuf
  
  type st_ppohAMRFDM_recvbuf
     integer(kind=ppohAMRFDM_kint),allocatable::bufi(:)
     real(kind=ppohAMRFDM_kdbl),allocatable::bufr(:)
  end type st_ppohAMRFDM_recvbuf
  
  type st_ppohAMRFDM_calc_sequence
     integer(kind=ppohAMRFDM_kint),allocatable::IDsq(:)
  end type st_ppohAMRFDM_calc_sequence

  type st_ppohAMRFDM_meshset
     type(st_ppohAMRFDM_octset),pointer::Mesh(:),GMesh(:)
     type(st_ppohAMRFDM_octset),pointer::Mesh2(:),GMesh2(:)
     type(st_ppohAMRFDM_sendbuf),allocatable::sBuf(:)
     type(st_ppohAMRFDM_recvbuf),allocatable::rBuf(:)
     type(st_ppohAMRFDM_calc_sequence),allocatable::getID(:)
     integer(kind=ppohAMRFDM_kint),allocatable::MinID(:,:),MaxID(:,:)
     integer(kind=ppohAMRFDM_kint),allocatable::rfnoct1(:),rfnoct2(:)
     integer(kind=ppohAMRFDM_kint),allocatable::rfnoct3(:),rfnoct4(:)
     integer(kind=ppohAMRFDM_kint)::rfncnt
  end type st_ppohAMRFDM_meshset
  
end module m_ppohAMRFDM_util
